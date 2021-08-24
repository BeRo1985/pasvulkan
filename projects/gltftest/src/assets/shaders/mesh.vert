#version 450

#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout(location = 0) in vec3 inPosition;
layout(location = 1) in uint inNodeIndex;
layout(location = 2) in vec4 inQTangent;
layout(location = 3) in vec2 inTexCoord0;
layout(location = 4) in vec2 inTexCoord1;
layout(location = 5) in vec4 inColor0;
layout(location = 6) in uint inMorphTargetVertexBaseIndex;
layout(location = 7) in uint inCountMorphTargetVertices;
layout(location = 8) in uint inJointBlockBaseIndex;
layout(location = 9) in uint inCountJointBlocks;

layout(location = 0) out vec3 outWorldSpacePosition;
layout(location = 1) out vec3 outViewSpacePosition;
layout(location = 2) out vec3 outCameraRelativePosition;
layout(location = 3) out vec3 outTangent;
layout(location = 4) out vec3 outBitangent;
layout(location = 5) out vec3 outNormal;
layout(location = 6) out vec2 outTexCoord0;
layout(location = 7) out vec2 outTexCoord1;
layout(location = 8) out vec4 outColor0;

/* clang-format off */
layout (push_constant) uniform PushConstants {
	mat4 viewMatrix;
	mat4 projectionMatrix;
} pushConstants;

struct MorphTargetVertex {
   vec4 position;
   vec4 normal;
   vec4 tangent;
   uvec4 metaData; // x = index, y = next
};

layout(std430, set = 0, binding = 0) buffer MorphTargetVertices {
	MorphTargetVertex morphTargetVertices[];
};

struct JointBlock {
  uvec4 joints;
  vec4 weights;
};

layout(std430, set = 0, binding = 1) buffer JointBlocks {
	JointBlock jointBlocks[];
};

layout(std430, set = 0, binding = 2) buffer NodeMatrices {
	mat4 nodeMatrices[];
};

layout(std430, set = 0, binding = 3) buffer MorphTargetWeights {
	float morphTargetWeights[];
};

layout(set = 1, binding = 0) uniform Material {
	uint test; //mat4 items[];
} material;

out gl_PerVertex {
    vec4 gl_Position;   
};
/* clang-format on */

/* clang-format off */
mat3 QTangentToMatrix(vec4 q){  
  /*q = normalize(q);
  float qx2 = q.x + q.x,
        qy2 = q.y + q.y,
        qz2 = q.z + q.z,
        qxqx2 = q.x * qx2,
        qxqy2 = q.x * qy2,
        qxqz2 = q.x * qz2,
        qxqw2 = q.w * qx2,
        qyqy2 = q.y * qy2,
        qyqz2 = q.y * qz2,
        qyqw2 = q.w * qy2,
        qzqz2 = q.z * qz2,
        qzqw2 = q.w * qz2;
  mat3 m = mat3(1.0 - (qyqy2 + qzqz2), qxqy2 + qzqw2, qxqz2 - qyqw2,
                qxqy2 - qzqw2, 1.0 - (qxqx2 + qzqz2), qyqz2 + qxqw2,
                qxqz2 + qyqw2, qyqz2 - qxqw2, 1.0 - (qxqx2 + qyqy2));
  m[2] = normalize(cross(m[0], m[1])) * ((q.w < 0.0) ? -1.0 : 1.0);
  return m;*/
  float fTx  = 2.0 * q.x,
        fTy  = 2.0 * q.y,
        fTz  = 2.0 * q.z, 
        fTwx = fTx * q.w,
        fTwy = fTy * q.w, 
        fTwz = fTz * q.w, 
        fTxx = fTx * q.x, 
        fTxy = fTy * q.x, 
        fTxz = fTz * q.x, 
        fTyy = fTy * q.y, 
        fTyz = fTz * q.y, 
        fTzz = fTz * q.z;
  vec3 x = normalize(vec3(1.0 - (fTyy+fTzz), fTxy+fTwz, fTxz-fTwy ));  
  vec3 y = normalize(vec3( fTxy-fTwz, 1.0-(fTxx+fTzz), fTyz+fTwx ));    
  return mat3(y, normalize(cross(y, x) * sign(q.w)), x);
}

/* clang-format on */

void main() {
#if 0
  // The actual standard approach
  vec3 cameraPosition = inverse(pushConstants.viewMatrix)[3].xyz;
#else
  // This approach assumes that the view matrix has no scaling or skewing, but only rotation and translation.
  vec3 cameraPosition = (-pushConstants.viewMatrix[3].xyz) * mat3(pushConstants.viewMatrix);
#endif

  mat4 nodeMatrix = nodeMatrices[inNodeIndex];

  mat4 modelNodeMatrix = nodeMatrices[0] * nodeMatrix;

  vec3 position = inPosition;
  mat3 tangentSpace = QTangentToMatrix(inQTangent);

  if (inMorphTargetVertexBaseIndex != 0xffffffffu) {
    vec4 normal = vec4(tangentSpace[2], 0.0f);
    vec4 tangent = vec4(tangentSpace[0], sign(dot(cross(tangentSpace[2], tangentSpace[0]), tangentSpace[1])));
    uint morphTargetVertexIndex = inMorphTargetVertexBaseIndex;
    uint protectionCounter = 0x0ffffu;
    while ((morphTargetVertexIndex != 0xffffffffu) && (protectionCounter-- > 0u)) {
      MorphTargetVertex morphTargetVertex = morphTargetVertices[morphTargetVertexIndex];
      float weight = morphTargetWeights[morphTargetVertex.metaData.x];
      position += morphTargetVertex.position.xyz * weight;
      normal += vec4(morphTargetVertex.normal.xyz, 1.0) * weight;
      tangent.xyz += morphTargetVertex.tangent.xyz * weight;
      morphTargetVertexIndex = morphTargetVertex.metaData.y;
    }
    normal.xyz = normalize(normal.xyz);
    tangent.xyz = normalize(tangent.xyz);
    tangentSpace = mat3(tangent.xyz, normalize(cross(normal.xyz, tangent.xyz) * tangent.w), normal.xyz);
  }

  if (inCountJointBlocks > 0u) {
    mat4 inverseNodeMatrix = inverse(nodeMatrix);
    mat4 skinMatrix = mat4(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f);
    for (uint jointBlockBaseIndex = inJointBlockBaseIndex, endJointBlockBaseIndex = jointBlockBaseIndex + inCountJointBlocks;  //
         jointBlockBaseIndex < endJointBlockBaseIndex;                                                                         //
         jointBlockBaseIndex++) {
      JointBlock jointBlock = jointBlocks[jointBlockBaseIndex];
      skinMatrix += ((inverseNodeMatrix * nodeMatrices[jointBlock.joints.x]) * jointBlock.weights.x) +  //
                    ((inverseNodeMatrix * nodeMatrices[jointBlock.joints.y]) * jointBlock.weights.y) +  //
                    ((inverseNodeMatrix * nodeMatrices[jointBlock.joints.z]) * jointBlock.weights.z) +  //
                    ((inverseNodeMatrix * nodeMatrices[jointBlock.joints.w]) * jointBlock.weights.w);
    }
    modelNodeMatrix *= skinMatrix;
  }

  mat3 normalMatrix = transpose(inverse(mat3(modelNodeMatrix)));

  tangentSpace = normalMatrix * tangentSpace;

  tangentSpace[0] = normalize(tangentSpace[0]);
  tangentSpace[1] = normalize(tangentSpace[1]);
  tangentSpace[2] = normalize(tangentSpace[2]);

  mat4 modelViewMatrix = pushConstants.viewMatrix * modelNodeMatrix;

  vec4 worldSpacePosition = modelNodeMatrix * vec4(position, 1.0);
  worldSpacePosition.xyz /= worldSpacePosition.w;

  vec4 viewSpacePosition = modelViewMatrix * vec4(position, 1.0);
  viewSpacePosition.xyz /= viewSpacePosition.w;

  outWorldSpacePosition = worldSpacePosition.xyz;
  outViewSpacePosition = viewSpacePosition.xyz.xyz;
  outCameraRelativePosition = worldSpacePosition.xyz - cameraPosition;
  outTangent = tangentSpace[0];
  outBitangent = tangentSpace[1];
  outNormal = tangentSpace[2];
  outTexCoord0 = inTexCoord0;
  outTexCoord1 = inTexCoord1;
  outColor0 = inColor0;
  gl_Position = (pushConstants.projectionMatrix * modelViewMatrix) * vec4(position, 1.0);
}
