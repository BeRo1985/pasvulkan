#version 460 core

//#define SHADERDEBUG

#ifndef VOXELIZATION
#extension GL_EXT_multiview : enable
#endif
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
//#extension GL_ARB_shader_draw_parameters : enable
#if defined(SHADERDEBUG) && !defined(VELOCITY)
#extension GL_EXT_debug_printf : enable
#endif
#extension GL_GOOGLE_include_directive : enable

#ifdef RAYTRACING
#extension GL_EXT_fragment_shader_barycentric : enable // for calculating the geometry normal in the fragment shader without dFdx/dFdy in a more direct way, per pervertexEXT
#endif

// No more layout(location = N) in vertex attributes — all data fetched via BDA vertex pulling.

layout(location = 0) out vec3 outWorldSpacePosition;
layout(location = 1) out vec3 outViewSpacePosition;
layout(location = 2) out vec3 outCameraRelativePosition;
layout(location = 3) out vec4 outTangentSign;
layout(location = 4) out vec3 outNormal;
layout(location = 5) out vec2 outTexCoord0;
layout(location = 6) out vec2 outTexCoord1;
layout(location = 7) out vec4 outColor0;
layout(location = 8) out vec3 outModelScale;
layout(location = 9) flat out uint outMaterialID;
layout(location = 10) flat out uint outInstanceDataIndex;
#ifndef VOXELIZATION
layout(location = 11) flat out int outViewIndex;
layout(location = 12) flat out uint outFrameIndex;
#ifdef VELOCITY
layout(location = 13) out vec4 outPreviousClipSpace;
layout(location = 14) out vec4 outCurrentClipSpace;
#endif // VELOCITY
#endif // VOXELIZATION

/* clang-format off */

#include "mesh_pushconstants.glsl"

#include "drawinfo.glsl"

// DrawInfo SSBO — replaces InstanceMatrices (binding 0) and InstanceDataIndexBuffer (binding 7)
layout(set = 0, binding = 0, std430) readonly buffer DrawInfoBuffer {
  DrawInfo drawInfoItems[];
};

// GlobalBDAPointers SSBO — global buffer device addresses for vertex pulling
layout(set = 0, binding = 7, std430) readonly buffer GlobalBDAPointersBuffer {
  GlobalBDAPointers globalBDAPointers;
};

// Global descriptor set

struct View {
  mat4 viewMatrix;
  mat4 projectionMatrix;
  mat4 inverseViewMatrix;
  mat4 inverseProjectionMatrix;
};

layout(set = 1, binding = 0, std140) uniform uboViews {
  View views[256]; // 65536 / (64 * 4) = 256
} uView;

out gl_PerVertex {
	vec4 gl_Position;
	float gl_PointSize;
};

/* clang-format on */

#include "adjugate.glsl"

void main() {

#ifdef VOXELIZATION
  uint viewIndex = pushConstants.viewBaseIndex;
#else
  uint viewIndex = pushConstants.viewBaseIndex + uint(gl_ViewIndex);
#endif

  // meshObjectID is passed via firstInstance field of the draw command
  const uint meshObjectID = uint(gl_InstanceIndex);
  DrawInfo drawInfo = drawInfoItems[meshObjectID];

  // Vertex index for vertex pulling, it matches the vertex index used in the CPU-side mesh compaction and 
  // GPU-side mesh cull compute shader, which is a global index across all meshes and instances, not a 
  // per-mesh local index. This allows us to use a single global vertex buffer (big-buffer approach) and 
  // fetch vertex data directly via BDA without needing to worry about per-mesh offsets or multiple buffers.
  const uint vertexIndex = uint(gl_VertexIndex);

  // Fetch vertex data via BDA vertex pulling (BDA pointers from global SSBO at binding 7)
  CachedVertexBuffer cachedVerts = CachedVertexBuffer(globalBDAPointers.cachedVerticesBDA);
  StaticVertexBuffer staticVerts = StaticVertexBuffer(globalBDAPointers.staticVerticesBDA);

  // Bounded, because the two buffers do not always grow in the same step: the cached one is per in-flight
  // frame and the static one is the long-lived one, and while a model is appearing there is a moment where
  // an index is already valid for one and not yet for the other. Reading past the end of an allocation
  // costs the device, so out of range this vertex is dropped instead (see the degenerate position below).
  bool vertexInRange = (vertexIndex < globalBDAPointers.cachedVerticesCount) &&
                       (vertexIndex < globalBDAPointers.staticVerticesCount);
  PackedCachedVertex cv = cachedVerts.vertices[vertexInRange ? vertexIndex : 0u];
  PackedStaticVertex sv = staticVerts.vertices[vertexInRange ? vertexIndex : 0u];

  // Unpack vertex attributes
  vec3 position = unpackPosition(cv);
  vec4 normalSign = unpackNormalSign(cv);
  vec3 tangent = unpackTangent(cv);
  vec3 modelScale = unpackModelScale(cv);
  vec2 texCoord0 = sv.texCoord0;
  vec2 texCoord1 = sv.texCoord1;
  vec4 color0 = unpackColor0(sv);
  uint materialID = sv.materialID;

#ifdef VELOCITY
  // Fetch previous frame vertex data and generation for motion vectors (BDA from global SSBO)
  CachedVertexBuffer prevCachedVerts = CachedVertexBuffer(globalBDAPointers.previousCachedVerticesBDA);
  GenerationBuffer genBuf = GenerationBuffer(globalBDAPointers.generationBDA);
  GenerationBuffer prevGenBuf = GenerationBuffer(globalBDAPointers.previousGenerationBDA);
  // The previous frame's buffers hold the scene as it was THEN, so a vertex that has appeared since has no
  // counterpart in them and reading one reads past the end of an allocation. The generation compare further
  // down is meant to catch exactly this vertex - but it happens long after the read, so the read itself has
  // to be bounded here. Out of range, the previous generation is made to differ, which sends that compare
  // down its "this vertex is new" branch and leaves the unread previous position unused.
  vec3 previousPosition = vec3(0.0);
  uint generation = 0u;
  if(vertexIndex < globalBDAPointers.generationCount){
    generation = genBuf.generations[vertexIndex];
  }
  uint previousGeneration = generation + 1u;
  if(vertexIndex < globalBDAPointers.previousCachedVerticesCount){
    previousPosition = unpackPosition(prevCachedVerts.vertices[vertexIndex]);
  }
  if(vertexIndex < globalBDAPointers.previousGenerationCount){
    previousGeneration = prevGenBuf.generations[vertexIndex];
  }
#endif

  // Build tangent space
  mat3 tangentSpace;
  {
    vec3 n = normalSign.xyz;
    tangentSpace = mat3(normalize(tangent), normalize(cross(n, tangent)) * normalSign.w, normalize(n));
  }

  View view = uView.views[viewIndex];

#if defined(SHADERDEBUG) && !(defined(VELOCITY) || defined(VOXELIZATION))
  if(gl_VertexIndex == 0){
    mat4 m = view.inverseProjectionMatrix;
    debugPrintfEXT("view-index %i matrix: %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f",
                   viewIndex,
                   m[0][0], m[0][1], m[0][2], m[0][3],
                   m[1][0], m[1][1], m[1][2], m[1][3],
                   m[2][0], m[2][1], m[2][2], m[2][3],
                   m[3][0], m[3][1], m[3][2], m[3][3]);
  }
#endif

#if 1
  // The actual standard approach
  vec3 cameraPosition = view.inverseViewMatrix[3].xyz;
#else
  // This approach assumes that the view matrix has no scaling or skewing, but only rotation and translation.
  vec3 cameraPosition = (-view.viewMatrix[3].xyz) * mat3(view.viewMatrix);
#endif

  // Unified transform path — no more if(instanceIndex > 0u) branching.
  // For pre-transformed meshes (mesh.comp output): matrixID=0 => Identity => no-op.
  // For instanced meshes: matrixID points to the world transform in MatrixPairBuffer.
  MatrixPairBuffer matrixPairBuffer = MatrixPairBuffer(globalBDAPointers.matrixPairBDA);
  // Bounded like the vertex buffers above: the draw can name a matrix that this frame's buffer does not
  // hold yet while an instance is appearing. Entry zero is the identity sentinel, so falling back to it
  // leaves the mesh where its own vertices put it rather than reading past the end of an allocation.
  MatrixPair matrixPair =
      matrixPairBuffer.pairs[(drawInfo.matrixID < globalBDAPointers.matrixPairCount) ? drawInfo.matrixID : 0u];
  mat4 modelMatrix = matrixPair.modelMatrix;

  modelScale *= vec3(length(modelMatrix[0].xyz), length(modelMatrix[1].xyz), length(modelMatrix[2].xyz));

  tangentSpace = adjugate(modelMatrix) * tangentSpace;

  vec4 viewSpacePosition;
  vec4 clipSpacePosition = view.projectionMatrix * (viewSpacePosition = ((view.viewMatrix * modelMatrix) * vec4(position, 1.0)));
  viewSpacePosition.xyz /= viewSpacePosition.w;

  vec3 worldSpacePosition = (modelMatrix * vec4(position, 1.0)).xyz;

  outInstanceDataIndex = drawInfo.instanceDataIndex;

#ifdef VELOCITY
  vec4 previousClipSpacePosition;
  if(generation != previousGeneration){
    previousClipSpacePosition = clipSpacePosition;
  }else{
    View previousView = uView.views[viewIndex + pushConstants.countAllViews];
    previousClipSpacePosition = previousView.projectionMatrix * ((previousView.viewMatrix * matrixPair.previousModelMatrix) * vec4(previousPosition, 1.0));
  }
#endif

  outWorldSpacePosition = worldSpacePosition;
  outViewSpacePosition = viewSpacePosition.xyz;
  outCameraRelativePosition = worldSpacePosition - cameraPosition;
  outTangentSign = vec4(tangentSpace[0], normalSign.w);
  outNormal = tangentSpace[2];
  outTexCoord0 = texCoord0;
  outTexCoord1 = texCoord1;
  outColor0 = color0;
  outModelScale = modelScale;
  outMaterialID = materialID;
#ifndef VOXELIZATION
  outViewIndex = int(viewIndex);
  outFrameIndex = pushConstants.frameIndex;
#endif

#ifdef VOXELIZATION

  gl_Position = vec4(0.0, 0.0, 0.0, 1.0); // Overrided by geometry shader anyway

#else

#if defined(VELOCITY)

  outCurrentClipSpace = clipSpacePosition;

  outPreviousClipSpace = previousClipSpacePosition;

#endif

  // A vertex whose data was out of range above carries whatever vertex zero holds, which is not this
  // vertex - so it is put behind the near plane rather than drawn somewhere wrong.
  gl_Position = vertexInRange ? clipSpacePosition : vec4(0.0, 0.0, -1.0, 0.0);

#endif

  gl_PointSize = 1.0;
}
