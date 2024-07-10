#ifndef ATMOSPHERE_COMMON_GLSL
#define ATMOSPHERE_COMMON_GLSL

struct DensityProfileLayer {
  float width;
  float expTerm;
  float expScale;
  float linearTerm;
  float constantTerm;
  float unused0;
  float unused1;
  float unused2;
};

struct DensityProfile {
  DensityProfileLayer layers[2];
};

struct AtmosphereParameters {
  DensityProfile rayleighDensity;
  DensityProfile mieDensity;
  DensityProfile absorptionDensity;
  vec4 center;
  vec4 solarIrradiance;
  vec4 rayleighScattering;
  vec4 mieScattering;
  vec4 mieExtinction;
  vec4 absorptionExtinction;
  vec4 groundAlbedo;
  float miePhaseFunctionG;
  float sunAngularRadius;
  float bottomRadius;
  float topRadius;
  float muSMin;
};

#endif