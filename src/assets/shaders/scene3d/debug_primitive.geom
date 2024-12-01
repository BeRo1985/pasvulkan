#version 430 core

layout(lines) in;
layout(triangle_strip, max_vertices = 4) out;

layout(location = 0) in vec4 inColor[];
layout(location = 1) in float inEdgeDistance[]; // unused

layout(location = 0) out vec4 outColor;
layout(location = 1) out float outEdgeDistance;

/* clang-format off */
layout (push_constant) uniform PushConstants {
  uint viewBaseIndex;
  uint countViews;
  uint countAllViews;
  uint dummy;
  vec2 viewPortSize;
} pushConstants;

in gl_PerVertex {
	vec4 gl_Position;
	float gl_PointSize;  
  float gl_ClipDistance[];
};

/* clang-format on */

void main() {

  float thickness = 3.0;
	vec2 lineStart = gl_in[0].gl_Position.xy / gl_in[0].gl_Position.w;
  vec2 lineEnd = gl_in[1].gl_Position.xy / gl_in[1].gl_Position.w;

  vec2 dir = lineStart - lineEnd;  
  vec2 normal = normalize(vec2(-dir.y, dir.x)) * thickness / pushConstants.viewPortSize;
	
  // line start
	gl_Position = vec4((lineStart - normal) * gl_in[0].gl_Position.w, gl_in[0].gl_Position.zw); 
	outEdgeDistance = -thickness;
  outColor = inColor[0];
	EmitVertex();
		
	gl_Position = vec4((lineStart + normal) * gl_in[0].gl_Position.w, gl_in[0].gl_Position.zw);
  outEdgeDistance = thickness;
  outColor = inColor[0];
	EmitVertex();
		
	 // line end
	gl_Position = vec4((lineEnd - normal) * gl_in[1].gl_Position.w, gl_in[1].gl_Position.zw);
  outEdgeDistance = -thickness;
  outColor = inColor[1];
	EmitVertex();
		
	gl_Position = vec4((lineEnd + normal) * gl_in[1].gl_Position.w, gl_in[1].gl_Position.zw);
	outEdgeDistance = thickness;
  outColor = inColor[1];
  EmitVertex();
    
  EndPrimitive();
  
}