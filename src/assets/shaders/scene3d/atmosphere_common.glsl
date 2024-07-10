#ifndef ATMOSPHERE_COMMON_GLSL
#define ATMOSPHERE_COMMON_GLSL

#define ILLUMINANCE_IS_ONE

const float PI = 3.1415926535897932384626433832795;

struct DensityProfileLayer {
  float Width;
  float ExpTerm;
  float ExpScale;
  float LinearTerm;
  float ConstantTerm;
  float Unused0;
  float Unused1;
  float Unused2;
};

struct DensityProfile {
  DensityProfileLayer Layers[2];
};

struct AtmosphereParameters {
  DensityProfile RayleighDensity;
  DensityProfile MieDensity;
  DensityProfile AbsorptionDensity;
  vec4 Center;
  vec4 SolarIrradiance;
  vec4 RayleighScattering;
  vec4 MieScattering;
  vec4 MieExtinction;
  vec4 AbsorptionExtinction;
  vec4 GroundAlbedo;
  float MiePhaseFunctionG;
  float SunAngularRadius;
  float BottomRadius;
  float TopRadius;
  float MuSMin;
};

struct SingleScatteringResult {
	vec3 L;						// Scattered light (luminance)
	vec3 OpticalDepth;			// Optical depth (1/m)
	vec3 Transmittance;			// Transmittance in [0,1] (unitless)
	vec3 MultiScatAs1;
	vec3 NewMultiScatStep0Out;
	vec3 NewMultiScatStep1Out;
};

vec2 gResolution;

mat4 gSkyInvViewProjMat;

vec2 RayMarchMinMaxSPP;

#define PLANET_RADIUS_OFFSET 0.01f

struct Ray{
	vec3 o;
	vec3 d;
};

Ray createRay(in vec3 p, in vec3 d){
	Ray r;
	r.o = p;
	r.d = d;
	return r;
}

float fromUnitToSubUvs(float u, float resolution){ return ((u + 0.5) / resolution) * (resolution / (resolution + 1.0)); }
float fromSubUvsToUnit(float u, float resolution){ return ((u - 0.5) / resolution) * (resolution / (resolution - 1.0)); }

void UvToLutTransmittanceParams(AtmosphereParameters Atmosphere, out float viewHeight, out float viewZenithCosAngle, in vec2 uv){
	//uv = vec2(fromSubUvsToUnit(uv.x, TRANSMITTANCE_TEXTURE_WIDTH), fromSubUvsToUnit(uv.y, TRANSMITTANCE_TEXTURE_HEIGHT)); // No real impact so off
	float x_mu = uv.x;
	float x_r = uv.y;

	float H = sqrt((Atmosphere.TopRadius * Atmosphere.TopRadius) - (Atmosphere.BottomRadius * Atmosphere.BottomRadius));
	float rho = H * x_r;
	viewHeight = sqrt((rho * rho) + (Atmosphere.BottomRadius * Atmosphere.BottomRadius));

	float d_min = Atmosphere.TopRadius - viewHeight;
	float d_max = rho + H;
	float d = d_min + x_mu * (d_max - d_min);
	viewZenithCosAngle = (d == 0.0) ? 1.0 : (((H * H) - (rho * rho)) - (d * d)) / (2.0 * viewHeight * d);
	viewZenithCosAngle = clamp(viewZenithCosAngle, -1.0, 1.0);
}

#define NONLINEARSKYVIEWLUT 1
void UvToSkyViewLutParams(AtmosphereParameters Atmosphere, out float viewZenithCosAngle, out float lightViewCosAngle, in float viewHeight, in vec2 uv){
	// Constrain uvs to valid sub texel range (avoid zenith derivative issue making LUT usage visible)
	uv = vec2(fromSubUvsToUnit(uv.x, 192.0), fromSubUvsToUnit(uv.y, 108.0));

	float Vhorizon = sqrt((viewHeight * viewHeight) - (Atmosphere.BottomRadius * Atmosphere.BottomRadius));
	float CosBeta = Vhorizon / viewHeight;				// GroundToHorizonCos
	float Beta = acos(CosBeta);
	float ZenithHorizonAngle = PI - Beta;

	if(uv.y < 0.5){
		float coord = 2.0 * uv.y;
		coord = 1.0 - coord;
#if NONLINEARSKYVIEWLUT
		coord *= coord;
#endif
		coord = 1.0 - coord;
		viewZenithCosAngle = cos(ZenithHorizonAngle * coord);
	}else{
		float coord = fma(uv.y, 2.0, -1.0);
#if NONLINEARSKYVIEWLUT
		coord *= coord;
#endif
		viewZenithCosAngle = cos(ZenithHorizonAngle + Beta * coord);
	}

	float coord = uv.x;
	coord *= coord;
	lightViewCosAngle = -fma(coord, 2.0, -1.0);
}

void SkyViewLutParamsToUv(AtmosphereParameters Atmosphere, in bool IntersectGround, in float viewZenithCosAngle, in float lightViewCosAngle, in float viewHeight, out vec2 uv){
	float Vhorizon = sqrt((viewHeight * viewHeight) - (Atmosphere.BottomRadius * Atmosphere.BottomRadius));
	float CosBeta = Vhorizon / viewHeight;				// GroundToHorizonCos
	float Beta = acos(CosBeta);
	float ZenithHorizonAngle = PI - Beta;

	if(!IntersectGround){
		float coord = acos(viewZenithCosAngle) / ZenithHorizonAngle;
		coord = 1.0 - coord;
#if NONLINEARSKYVIEWLUT
		coord = sqrt(coord);
#endif
		coord = 1.0 - coord;
		uv.y = coord * 0.5;
	}else{
		float coord = (acos(viewZenithCosAngle) - ZenithHorizonAngle) / Beta;
#if NONLINEARSKYVIEWLUT
		coord = sqrt(coord);
#endif
		uv.y = fma(coord, 0.5, 0.5);
	}

	{
		float coord = fma(lightViewCosAngle, -0.5, 0.5);
		coord = sqrt(coord);
		uv.x = coord;
	}

	// Constrain uvs to valid sub texel range (avoid zenith derivative issue making LUT usage visible)
	uv = vec2(fromUnitToSubUvs(uv.x, 192.0), fromUnitToSubUvs(uv.y, 108.0));
}

float raySphereIntersectNearest(vec3 r0, vec3 rd, vec3 s0, float sR){
  float a = dot(rd, rd);
	vec3 s0_r0 = r0 - s0;
	float b = 2.0 * dot(rd, s0_r0);
	float c = dot(s0_r0, s0_r0) - (sR * sR);
	float delta = (b * b) - (4.0 * a * c);
	if((delta < 0.0) || (a == 0.0)){
		return -1.0;
	}else{
    vec2 sol01 = (vec2(-b) + (vec2(sqrt(delta)) * vec2(-1.0, 1.0))) / vec2(2.0 * a);
    if(all(lessThan(sol01, vec2(0.0)))){
      return -1.0;
    }else if(sol01.x < 0.0){
      return max(0.0, sol01.y);
    }else if(sol01.y < 0.0){
      return max(0.0, sol01.x);
    }else{
      return max(0.0, min(sol01.x, sol01.y));
    }
  }
}

void LutTransmittanceParamsToUv(const in AtmosphereParameters Atmosphere, in float viewHeight, in float viewZenithCosAngle, out vec2 uv){
	
  float H = sqrt(max(0.0, Atmosphere.TopRadius * Atmosphere.TopRadius - Atmosphere.BottomRadius * Atmosphere.BottomRadius));
	float rho = sqrt(max(0.0, viewHeight * viewHeight - Atmosphere.BottomRadius * Atmosphere.BottomRadius));

	float discriminant = viewHeight * viewHeight * (viewZenithCosAngle * viewZenithCosAngle - 1.0) + Atmosphere.TopRadius * Atmosphere.TopRadius;
	float d = max(0.0, (-viewHeight * viewZenithCosAngle + sqrt(discriminant))); // Distance to atmosphere boundary

	float d_min = Atmosphere.TopRadius - viewHeight;
	float d_max = rho + H;
	float x_mu = (d - d_min) / (d_max - d_min);
	float x_r = rho / H;

	uv = vec2(x_mu, x_r);
	//uv = vec2(fromUnitToSubUvs(uv.x, TRANSMITTANCE_TEXTURE_WIDTH), fromUnitToSubUvs(uv.y, TRANSMITTANCE_TEXTURE_HEIGHT)); // No real impact so off
}

float RayleighPhase(float cosTheta){
	float factor = 3.0 / (16.0 * PI);
	return factor * (1.0 + (cosTheta * cosTheta));
}

float CornetteShanksMiePhaseFunction(float g, float cosTheta){
	float k = ((3.0 / (8.0 * PI)) * (1.0 - (g * g))) / (2.0 + (g * g));
	return (k * (1.0 + (cosTheta * cosTheta))) / pow((1.0 + (g * g)) - (2.0 * g * -cosTheta), 1.5);
}

float hgPhase(float g, float cosTheta){
#ifdef USE_CornetteShanks
	return CornetteShanksMiePhaseFunction(g, cosTheta);
#else
	// Reference implementation (i.e. not schlick approximation). 
	// See http://www.pbr-book.org/3ed-2018/Volume_Scattering/Phase_Functions.html
	float numer = 1.0 - (g * g);
	float denom = 1.0 + (g * g) + (2.0 * g * cosTheta);
	return numer / (4.0 * PI * denom * sqrt(denom));
#endif
}

SingleScatteringResult IntegrateScatteredLuminance(in vec2 pixPos, 
                                                   in vec3 WorldPos, 
                                                   in vec3 WorldDir, 
                                                   in vec3 SunDir, 
                                                   const in AtmosphereParameters Atmosphere,
                                                   in bool ground, 
                                                   in float SampleCountIni, 
                                                   in float DepthBufferValue, 
                                                   in bool VariableSampleCount,
                                                   in bool MieRayPhase, 
                                                   in float tMaxMax){

  SingleScatteringResult result;

  vec3 ClipSpace = vec3(fma(vec2(pixPos) / vec2(gResolution), vec2(2.0, -2.0), vec2(1.0, -1.0)), 1.0);

  // Compute next intersection with atmosphere or ground 
  vec3 earthO = vec3(0.0);
  float tBottom = raySphereIntersectNearest(WorldPos, WorldDir, earthO, Atmosphere.BottomRadius);
	float tTop = raySphereIntersectNearest(WorldPos, WorldDir, earthO, Atmosphere.TopRadius);
	float tMax = 0.0;
	if(tBottom < 0.0){
		if (tTop < 0.0){
			tMax = 0.0; // No intersection with earth nor atmosphere: stop right away  
			return result;
		}else{
			tMax = tTop;
		}
	}else{
		if(tTop > 0.0){
			tMax = min(tTop, tBottom);
		}
	}  

	if(DepthBufferValue >= 0.0){
		ClipSpace.z = DepthBufferValue;
		if(ClipSpace.z < 1.0){
			vec4 DepthBufferWorldPos = gSkyInvViewProjMat * vec4(ClipSpace, 1.0);
			DepthBufferWorldPos /= DepthBufferWorldPos.w;

			float tDepth = length(DepthBufferWorldPos.xyz - (WorldPos + vec3(0.0, 0.0, -Atmosphere.BottomRadius))); // apply earth offset to go back to origin as top of earth mode. 
			if(tDepth < tMax){
				tMax = tDepth;
			}
		}
		/*
    if (VariableSampleCount && (ClipSpace.z == 1.0)){
		  return result;
    }*/
	}
	tMax = min(tMax, tMaxMax);

	// Sample count 
	float SampleCount = SampleCountIni;
	float SampleCountFloor = SampleCountIni;
	float tMaxFloor = tMax;
	if(VariableSampleCount){
		SampleCount = mix(RayMarchMinMaxSPP.x, RayMarchMinMaxSPP.y, clamp(tMax * 0.01, 0.0, 1.0));
		SampleCountFloor = floor(SampleCount);
		tMaxFloor = tMax * SampleCountFloor / SampleCount;	// rescale tMax to map to the last entire step segment.
	}
	float dt = tMax / SampleCount;

	// Phase functions
	const float uniformPhase = 1.0 / (4.0 * PI);
	const vec3 wi = SunDir;
	const vec3 wo = WorldDir;
	float cosTheta = dot(wi, wo);
	float MiePhaseValue = hgPhase(Atmosphere.MiePhaseFunctionG, -cosTheta);	// mnegate cosTheta because due to WorldDir being a "in" direction. 
	float RayleighPhaseValue = RayleighPhase(cosTheta);

#ifdef ILLUMINANCE_IS_ONE
	// When building the scattering factor, we assume light illuminance is 1 to compute a transfert function relative to identity illuminance of 1.
	// This make the scattering factor independent of the light. It is now only linked to the atmosphere properties.
	vec3 globalL = vec3(1.0);
#else
	vec3 globalL = gSunIlluminance;
#endif

	// Ray march the atmosphere to integrate optical depth
	vec3 L = vec3(0.0);
	vec3 throughput = vec3(1.0);
	vec3 OpticalDepth = vec3(0.0);
	float t = 0.0;
	float tPrev = 0.0;
	const float SampleSegmentT = 0.3;
	for (float s = 0.0; s < SampleCount; s += 1.0){
		if (VariableSampleCount){
			// More expenssive but artefact free
			float t0 = (s) / SampleCountFloor;
			float t1 = (s + 1.0) / SampleCountFloor;
			// Non linear distribution of sample within the range.
			t0 = t0 * t0;
			t1 = t1 * t1;
			// Make t0 and t1 world space distances.
			t0 = tMaxFloor * t0;
			if(t1 > 1.0){
				t1 = tMax;
		  //t1 = tMaxFloor;	// this reveal depth slices
			}else{
				t1 = tMaxFloor * t1;
			}
			//t = t0 + (t1 - t0) * (whangHashNoise(pixPos.x, pixPos.y, gFrameId * 1920 * 1080)); // With dithering required to hide some sampling artefact relying on TAA later? This may even allow volumetric shadow?
			t = t0 + ((t1 - t0) * SampleSegmentT);
			dt = t1 - t0;
		}else{
			//t = tMax * (s + SampleSegmentT) / SampleCount;
			// Exact difference, important for accuracy of multiple scattering
			float NewT = tMax * (s + SampleSegmentT) / SampleCount;
			dt = NewT - t;
			t = NewT;
		}
		vec3 P = WorldPos + t * WorldDir;

  }

  return result;

}

#endif