#ifndef PATHTRACING_GLSL
#define PATHTRACING_GLSL

struct Ray { 
	vec3 origin; 
	vec3 direction; 
};

struct BsdfSample {
	vec3 bsdfDir; 
	float pdf; 
};


struct RayPayload {
	Ray ray;
	BsdfSample bsdf;
	vec3 radiance;
	vec3 absorption;
	vec3 beta;
	vec3 worldPos;
	vec3 normal;
	vec3 ffnormal;
	uint depth;
	bool stop;
	float eta;
};

#endif