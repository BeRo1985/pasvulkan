#ifndef REC2020_GLSL
#define REC2020_GLSL

const mat3 LinearRec2020ToLinearSRGBMatrix = mat3(
	vec3(1.6605, -0.1246, -0.0182),
	vec3(-0.5876, 1.1329, -0.1006),
	vec3(-0.0728, -0.0083, 1.1187)
);

const mat3 LinearSRGBToLinearRec2020Matrix = mat3(
	vec3(0.6274, 0.0691, 0.0164),
	vec3(0.3293, 0.9195, 0.0880),
	vec3(0.0433, 0.0113, 0.8956)
);

const vec3 LinearRec2020LuminanceWeights = vec3(0.2627002, 0.6779981, 0.0593017);

#endif