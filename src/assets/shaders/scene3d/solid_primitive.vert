#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

#define PRIMITIVE_TOPOLOGY_POINT 0u
#define PRIMITIVE_TOPOLOGY_LINE 1u
#define PRIMITIVE_TOPOLOGY_TRIANGLE 2u
#define PRIMITIVE_TOPOLOGY_TRIANGLE_WIREFRAME 3u

/*
  uvec4 position; // xy = Clip space position, zw = Offset0
  uvec4 position0; // xyz = Position, w = Primitive topology
  uvec4 position1; // xyz = Position, w = Line thickness or point size
  uvec4 position2; // xyz = Position, w = unused 
  uvec4 offset1Offset2; // xy = Offset1, zw = Offset2
  uvec4 color; // xyzw = Color
*/

layout(location = 0) in vec2 inPosition; // Position
layout(location = 1) in vec2 inOffset0; // Line start offset or center of point offset or triangle vertex 0 offset
layout(location = 2) in vec3 inPosition0; // Line start or center of point or triangle vertex 0
layout(location = 3) in uint inPrimitiveTopology; // Primitive topology
layout(location = 4) in vec3 inPosition1; // Line end or triangle vertex 1
layout(location = 5) in float inLineThicknessOrPointSize; // Line thickness or point size  
layout(location = 6) in vec3 inPosition2; // Triangle vertex 2 
layout(location = 7) in vec2 inOffset1; // Line end offset or triangle vertex 1 offset
layout(location = 8) in vec2 inOffset2; // Triangle vertex 2 offset
layout(location = 9) in vec4 inColor; // Color of the primitive

layout(location = 0) out vec4 outColor;
layout(location = 1) out vec2 outPosition;
layout(location = 2) out vec2 outPosition0;
layout(location = 3) out vec2 outPosition1;
layout(location = 4) out vec2 outPosition2;
layout(location = 5) out float outLineThicknessOrPointSize;
layout(location = 6) flat out uint outPrimitiveTopology;

/* clang-format off */
layout(push_constant) uniform PushConstants {
  uint viewBaseIndex;
  uint countViews;
  uint countAllViews;
  vec2 viewPortSize;
} pushConstants;

// Global descriptor set

struct View {
  mat4 viewMatrix;
  mat4 projectionMatrix;
  mat4 inverseViewMatrix;
  mat4 inverseProjectionMatrix;
};

layout(std140, set = 1, binding = 0) uniform uboViews {
  View views[256]; // 65536 / (64 * 4) = 256
} uView;

out gl_PerVertex {
	vec4 gl_Position;
	float gl_PointSize;
};

/* clang-format on */

vec2 clipSpaceToScreenSpace(const vec2 clipSpace) {
  return fma(clipSpace, vec2(0.5), vec2(0.5)) * pushConstants.viewPortSize;
}

void main() {

  uint viewIndex = pushConstants.viewBaseIndex + uint(gl_ViewIndex);
  outColor = inColor;

  mat4 viewProjectionMatrix = uView.views[viewIndex].projectionMatrix * uView.views[viewIndex].viewMatrix;

  bool threeDimensional = (inPrimitiveTopology & 0x4u) == 0u;

  switch(outPrimitiveTopology = inPrimitiveTopology & 0x3u){

    case PRIMITIVE_TOPOLOGY_POINT:{

      if(threeDimensional){

        // Center of the point
        vec4 position0 = viewProjectionMatrix * vec4(inPosition0, 1.0);
        outPosition0 = clipSpaceToScreenSpace((position0.xy / position0.w) + inOffset0);

      }else{

        // Center of the point
        outPosition0 = clipSpaceToScreenSpace(inPosition0.xy + inOffset0);

      }

      outPosition1 = vec2(0.0); // Not needed for points

      outLineThicknessOrPointSize = inLineThicknessOrPointSize;
      

      break;

    }

    case PRIMITIVE_TOPOLOGY_LINE:{

      if(threeDimensional){

        // Line start
        vec4 position0 = viewProjectionMatrix * vec4(inPosition0, 1.0);
        outPosition0 = clipSpaceToScreenSpace((position0.xy / position0.w) + inOffset0);

        // Line end
        vec4 position1 = viewProjectionMatrix * vec4(inPosition1, 1.0);
        outPosition1 = clipSpaceToScreenSpace((position1.xy / position1.w) + inOffset1);

      }else{ 

        // Line start
        outPosition0 = clipSpaceToScreenSpace(inPosition0.xy + inOffset0);

        // Line end
        outPosition1 = clipSpaceToScreenSpace(inPosition1.xy + inOffset1);

      }

      outLineThicknessOrPointSize = inLineThicknessOrPointSize;
      
      break;

    }

    case PRIMITIVE_TOPOLOGY_TRIANGLE:
    case PRIMITIVE_TOPOLOGY_TRIANGLE_WIREFRAME:{

      if(threeDimensional){

        // Triangle vertex 0
        vec4 position0 = viewProjectionMatrix * vec4(inPosition0, 1.0);
        outPosition0 = clipSpaceToScreenSpace((position0.xy / position0.w) + inOffset0);

        // Triangle vertex 1
        vec4 position1 = viewProjectionMatrix * vec4(inPosition1, 1.0);
        outPosition1 = clipSpaceToScreenSpace((position1.xy / position1.w) + inOffset1);

        // Triangle vertex 2      
        vec4 position2 = viewProjectionMatrix * vec4(inPosition2, 1.0);
        outPosition2 = clipSpaceToScreenSpace((position2.xy / position2.w) + inOffset2);

      }else{

        // Triangle vertex 0
        outPosition0 = clipSpaceToScreenSpace(inPosition0.xy + inOffset0);

        // Triangle vertex 1
        outPosition1 = clipSpaceToScreenSpace(inPosition1.xy + inOffset1);

        // Triangle vertex 2
        outPosition2 = clipSpaceToScreenSpace(inPosition2.xy + inOffset2);

      }

      outLineThicknessOrPointSize = (outPrimitiveTopology == PRIMITIVE_TOPOLOGY_TRIANGLE) ? 0.0 : inLineThicknessOrPointSize;

      break;

    }

    default:{

      outPosition0 = outPosition1 = outPosition2 = vec2(0.0);

      outLineThicknessOrPointSize = 0.0;

      break;

    }

  } 
    
  gl_Position = vec4(inPosition, 0.0, 1.0);

  gl_PointSize = 1.0; 

}