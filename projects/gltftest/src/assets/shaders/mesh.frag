#version 450 core

#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout(location = 0) in vec3 inWorldSpacePosition;
layout(location = 1) in vec3 inViewSpacePosition;
layout(location = 2) in vec3 inCameraRelativePosition;
layout(location = 3) in vec3 inTangent;
layout(location = 4) in vec3 inBitangent;
layout(location = 5) in vec3 inNormal;
layout(location = 6) in vec2 inTexCoord0;
layout(location = 7) in vec2 inTexCoord1;
layout(location = 8) in vec4 inColor0;

layout(set = 1, binding = 1) uniform sampler2D uTextures[];

layout(set = 2, binding = 0) uniform sampler2D uImageBasedLightingGGXBRDFTexture;

layout(set = 2, binding = 1) uniform samplerCube uImageBasedLightingGGXEnvMap;

layout(location = 0) out vec4 outFragColor;
#ifdef EXTRAEMISSIONOUTPUT
layout(location = 1) out vec4 outFragEmission;
#endif

/* clang-format off */
layout(std140, set = 1, binding = 0) uniform uboMaterial {
  vec4 baseColorFactor;
  vec4 specularFactor;
  vec4 emissiveFactor;
  vec4 metallicRoughnessNormalScaleOcclusionStrengthFactor;
  vec4 sheenColorFactorSheenIntensityFactor;
  vec4 clearcoatFactorClearcoatRoughnessFactor;
  uvec4 alphaCutOffFlagsTex0Tex1;
  mat4 textureTransforms[16];
} uMaterial;

#ifdef LIGHTS
struct Light {
  uvec4 metaData;
  vec4 colorIntensity;
  vec4 positionRange;
  vec4 directionZFar;
  mat4 shadowMapMatrix;
};

layout(std430, set=2, binding = 1) buffer LightData {
  uvec4 lightMetaData;
  Light lights[];
};
#endif

/* clang-format on */

float envMapMaxLevel;

vec3 convertLinearRGBToSRGB(vec3 c) {
  return mix((pow(c, vec3(1.0 / 2.4)) * vec3(1.055)) - vec3(5.5e-2), c * vec3(12.92), lessThan(c, vec3(3.1308e-3)));  //
}

vec3 convertSRGBToLinearRGB(vec3 c) {
  return mix(pow((c + vec3(5.5e-2)) / vec3(1.055), vec3(2.4)), c / vec3(12.92), lessThan(c, vec3(4.045e-2)));  //
}

vec4 convertSRGBToLinearRGB(vec4 c) {
  return vec4(convertSRGBToLinearRGB(c.xyz), c.w);  //
}

const float PI = 3.14159265358979323846, PI2 = 6.283185307179586476925286766559, OneOverPI = 1.0 / PI;

float sheenRoughness, cavity, transparency, refractiveAngle, ambientOcclusion, shadow, reflectance, clearcoatFactor, clearcoatRoughness;
vec4 sheenColorIntensityFactor;
vec3 clearcoatF0, clearcoatNormal, imageLightBasedLightDirection;
uint flags, shadingModel;

vec3 approximateAnalyticBRDF(vec3 specularColor, float roughness, float NoV) {
  const vec4 c0 = vec4(-1.0, -0.0275, -0.572, 0.022);
  const vec4 c1 = vec4(1.0, 0.0425, 1.04, -0.04);
  vec4 r = fma(c0, vec4(roughness), c1);
  vec2 AB = fma(vec2(-1.04, 1.04), vec2((min(r.x * r.x, exp2(-9.28 * NoV)) * r.x) + r.y), r.zw);
  return fma(specularColor, AB.xxx, AB.yyy);
}

vec3 diffuseLambert(vec3 diffuseColor) {
  return diffuseColor * OneOverPI;  //
}

vec3 diffuseFunction(vec3 diffuseColor, float roughness, float nDotV, float nDotL, float vDotH) {
  float FD90 = 0.5 + (2.0 * (vDotH * vDotH * roughness)), FdV = 1.0 + ((FD90 - 1.0) * pow(1.0 - nDotV, 5.0)), FdL = 1.0 + ((FD90 - 1.0) * pow(1.0 - nDotL, 5.0));
  return diffuseColor * (OneOverPI * FdV * FdL);
}

vec3 specularF(const in vec3 specularColor, const in float vDotH) {
  float fc = pow(1.0 - vDotH, 5.0);
  return vec3(clamp(max(max(specularColor.x, specularColor.y), specularColor.z) * 50.0, 0.0, 1.0) * fc) + ((1.0 - fc) * specularColor);
}

float specularD(const in float roughness, const in float nDotH) {
  float a = roughness * roughness;
  float a2 = a * a;
  float d = (((nDotH * a2) - nDotH) * nDotH) + 1.0;
  return a2 / (PI * (d * d));
}

float specularG(const in float roughness, const in float nDotV, const in float nDotL) {
  float k = (roughness * roughness) * 0.5;
  vec2 GVL = (vec2(nDotV, nDotL) * (1.0 - k)) + vec2(k);
  return 0.25 / (GVL.x * GVL.y);
}

float visibilityNeubelt(float NdotL, float NdotV) { return clamp(1.0 / (4.0 * ((NdotL + NdotV) - (NdotL * NdotV))), 0.0, 1.0); }

float sheenDistributionCarlie(float sheenRoughness, float NdotH) {
  float invR = 1.0 / (sheenRoughness * sheenRoughness);
  return (2.0 + invR) * pow(1.0 - (NdotH * NdotH), invR * 0.5) / PI2;
}

vec3 diffuseOutput, specularOutput, sheenOutput, clearcoatOutput, clearcoatBlendFactor;

void doSingleLight(const in vec3 lightColor, const in vec3 lightLit, const in vec3 lightDirection, const in vec3 normal, const in vec3 diffuseColor, const in vec3 specularColor, const in vec3 viewDirection, const in float refractiveAngle, const in float materialTransparency, const in float materialRoughness, const in float materialCavity) {
  vec3 halfVector = normalize(viewDirection + lightDirection);
  float nDotL = clamp(dot(normal, lightDirection), 1e-5, 1.0);
  float nDotV = clamp(abs(dot(normal, viewDirection)) + 1e-5, 0.0, 1.0);
  float nDotH = clamp(dot(normal, halfVector), 0.0, 1.0);
  float vDotH = clamp(dot(viewDirection, halfVector), 0.0, 1.0);
  vec3 lit = vec3((materialCavity * nDotL * lightColor) * lightLit);
  diffuseOutput += diffuseFunction(diffuseColor, materialRoughness, nDotV, nDotL, vDotH) * (1.0 - materialTransparency) * lit;
  specularOutput += specularF(specularColor, max(vDotH, refractiveAngle)) * specularD(materialRoughness, nDotH) * specularG(materialRoughness, nDotV, nDotL) * lit;
  if ((flags & (1u << 7u)) != 0u) {
    float sheenDistribution = sheenDistributionCarlie(sheenRoughness, nDotH);
    float sheenVisibility = visibilityNeubelt(nDotL, nDotV);
    sheenOutput += (sheenColorIntensityFactor.xyz * sheenColorIntensityFactor.w * sheenDistribution * sheenVisibility * PI) * lit;
  }
  if ((flags & (1u << 8u)) != 0u) {
    float nDotL = clamp(dot(clearcoatNormal, lightDirection), 1e-5, 1.0);
    float nDotV = clamp(abs(dot(clearcoatNormal, viewDirection)) + 1e-5, 0.0, 1.0);
    float nDotH = clamp(dot(clearcoatNormal, halfVector), 0.0, 1.0);
    vec3 lit = vec3((materialCavity * nDotL * lightColor) * lightLit);
    clearcoatOutput += specularF(clearcoatF0, max(vDotH, refractiveAngle)) * specularD(clearcoatRoughness, nDotH) * specularG(clearcoatRoughness, nDotV, nDotL) * lit;
  }
}

vec4 getEnvMap(sampler2D texEnvMap, vec3 rayDirection, float texLOD) {
  rayDirection = normalize(rayDirection);
  return textureLod(texEnvMap, (vec2((atan(rayDirection.z, rayDirection.x) / PI2) + 0.5, acos(rayDirection.y) / 3.1415926535897932384626433832795)), texLOD);
}

vec3 getDiffuseImageBasedLight(const in vec3 normal, const in vec3 diffuseColor) {
  float ao = cavity * ambientOcclusion;
  return (texture(uImageBasedLightingGGXEnvMap, normal.xyz, float(envMapMaxLevel)).xyz * diffuseColor * ao) * OneOverPI;
}

vec3 getSpecularImageBasedLight(const in vec3 normal, const in vec3 specularColor, const in float roughness, const in vec3 viewDirection, const in float litIntensity) {
  vec3 reflectionVector = normalize(reflect(viewDirection, normal.xyz));
  float NdotV = clamp(abs(dot(normal.xyz, viewDirection)) + 1e-5, 0.0, 1.0),                                                            //
      ao = cavity * ambientOcclusion,                                                                                                   //
      lit = mix(1.0, litIntensity, max(0.0, dot(reflectionVector, -imageLightBasedLightDirection) * (1.0 - (roughness * roughness)))),  //
      specularOcclusion = clamp((pow(NdotV + (ao * lit), roughness * roughness) - 1.0) + (ao * lit), 0.0, 1.0);
  vec2 brdf = textureLod(uImageBasedLightingGGXBRDFTexture, vec2(roughness, NdotV), 0.0).xy;
  return (texture(uImageBasedLightingGGXEnvMap,            //
                    reflectionVector,                      //
                    clamp((float(envMapMaxLevel) - 1.0) -  //
                              (1.0 - (1.2 * log2(roughness))),
                          0.0, float(envMapMaxLevel)))
              .xyz *                                                                                                                           //
          ((specularColor.xyz * brdf.x) + (brdf.yyy * clamp(max(max(specularColor.x, specularColor.y), specularColor.z) * 50.0, 0.0, 1.0))) *  //
          specularOcclusion) *
         OneOverPI;
}

float computeMSM(in vec4 moments, in float fragmentDepth, in float depthBias, in float momentBias) {
  vec4 b = mix(moments, vec4(0.5), momentBias);
  vec3 z;
  z[0] = fragmentDepth - depthBias;
  float L32D22 = fma(-b[0], b[1], b[2]);
  float D22 = fma(-b[0], b[0], b[1]);
  float squaredDepthVariance = fma(-b[1], b[1], b[3]);
  float D33D22 = dot(vec2(squaredDepthVariance, -L32D22), vec2(D22, L32D22));
  float InvD22 = 1.0 / D22;
  float L32 = L32D22 * InvD22;
  vec3 c = vec3(1.0, z[0], z[0] * z[0]);
  c[1] -= b.x;
  c[2] -= b.y + (L32 * c[1]);
  c[1] *= InvD22;
  c[2] *= D22 / D33D22;
  c[1] -= L32 * c[2];
  c[0] -= dot(c.yz, b.xy);
  float InvC2 = 1.0 / c[2];
  float p = c[1] * InvC2;
  float q = c[0] * InvC2;
  float D = (p * p * 0.25) - q;
  float r = sqrt(D);
  z[1] = (p * -0.5) - r;
  z[2] = (p * -0.5) + r;
  vec4 switchVal = (z[2] < z[0]) ? vec4(z[1], z[0], 1.0, 1.0) : ((z[1] < z[0]) ? vec4(z[0], z[1], 0.0, 1.0) : vec4(0.0));
  float quotient = (switchVal[0] * z[2] - b[0] * (switchVal[0] + z[2]) + b[1]) / ((z[2] - switchVal[1]) * (z[0] - z[1]));
  return clamp((switchVal[2] + (switchVal[3] * quotient)), 0.0, 1.0);
}

vec2 getShadowOffsets(const in vec3 p, const in vec3 N, const in vec3 L) {
  float cos_alpha = clamp(dot(N, L), 0.0, 1.0);
  float offset_scale_N = sqrt(1.0 - (cos_alpha * cos_alpha));  // sin(acos(L?N))
  float offset_scale_L = offset_scale_N / cos_alpha;           // tan(acos(L?N))
  return (vec2(offset_scale_N, min(2.0, offset_scale_L)) * vec2(0.0002, 0.00005)) + vec2(0.0, 0.0);
}

float linearStep(float a, float b, float v) {
  return clamp((v - a) / (b - a), 0.0, 1.0);  //
}

float reduceLightBleeding(float pMax, float amount) {
  return linearStep(amount, 1.0, pMax);  //
}

float getMSMShadowIntensity(vec4 moments, float depth, float depthBias, float momentBias) {
  vec4 b = mix(moments, vec4(0.5), momentBias);
  float d = depth - depthBias, l32d22 = fma(-b.x, b.y, b.z), d22 = fma(-b.x, b.x, b.y), squaredDepthVariance = fma(-b.y, b.y, b.w), d33d22 = dot(vec2(squaredDepthVariance, -l32d22), vec2(d22, l32d22)), invD22 = 1.0 / d22, l32 = l32d22 * invD22;
  vec3 c = vec3(1.0, d - b.x, d * d);
  c.z -= b.y + (l32 * c.y);
  c.yz *= vec2(invD22, d22 / d33d22);
  c.y -= l32 * c.z;
  c.x -= dot(c.yz, b.xy);
  vec2 pq = c.yx / c.z;
  vec3 z = vec3(d, vec2(-(pq.x * 0.5)) + (vec2(-1.0, 1.0) * sqrt(((pq.x * pq.x) * 0.25) - pq.y)));
  vec4 s = (z.z < z.x) ? vec3(z.y, z.x, 1.0).xyzz : ((z.y < z.x) ? vec4(z.x, z.y, 0.0, 1.0) : vec4(0.0));
  return clamp((s.z + (s.w * ((((s.x * z.z) - (b.x * (s.x + z.z))) + b.y) / ((z.z - s.y) * (z.x - z.y))))) * 1.03, 0.0, 1.0);
}

const uint smPBRMetallicRoughness = 0u,  //
    smPBRSpecularGlossiness = 1u,        //
    smUnlit = 2u;                        //

uvec2 texCoordIndices = uMaterial.alphaCutOffFlagsTex0Tex1.zw;
vec2 texCoords[2] = vec2[2](inTexCoord0, inTexCoord1);

vec4 textureFetch(const sampler2D tex, const in int textureIndex, const in vec4 defaultValue) {
  uint which = (texCoordIndices[textureIndex >> 3] >> ((uint(textureIndex) & 7u) << 2u)) & 0xfu;
  return (which < 0x2u) ? texture(tex, (uMaterial.textureTransforms[textureIndex] * vec3(texCoords[int(which)], 1.0).xyzz).xy) : defaultValue;
}

vec4 textureFetchSRGB(const sampler2D tex, const in int textureIndex, const in vec4 defaultValue) {
  uint which = (texCoordIndices[textureIndex >> 3] >> ((uint(textureIndex) & 7u) << 2u)) & 0xfu;
  vec4 texel;
  if (which < 0x2u) {
    texel = texture(tex, (uMaterial.textureTransforms[textureIndex] * vec3(texCoords[int(which)], 1.0).xyzz).xy);
    texel.xyz = convertSRGBToLinearRGB(texel.xyz);
  } else {
    texel = defaultValue;
  }
  return texel;
}

void main() {
  envMapMaxLevel = textureQueryLevels(uImageBasedLightingGGXEnvMap);
  flags = uMaterial.alphaCutOffFlagsTex0Tex1.y;
  shadingModel = (flags >> 0u) & 0xfu;
#ifdef SHADOWMAP
  vec4 t = uFrameGlobals.viewProjectionMatrix * vec4(vWorldSpacePosition, 1.0);
  float d = fma(t.z / t.w, 0.5, 0.5);
  float s = d * d;
  vec4 m = vec4(d, s, s * d, s * s);
  oOutput = m;
  float alpha = textureFetch(uTextures[0], 0, vec4(1.0)).w * uMaterial.baseColorFactor.w * vColor.w;
#else
  vec4 color = vec4(0.0);
#ifdef EXTRAEMISSIONOUTPUT
  vec4 emissionColor = vec4(0.0);
#endif
  float litIntensity = 1.0;
  switch (shadingModel) {
    case smPBRMetallicRoughness:
    case smPBRSpecularGlossiness: {
      vec4 diffuseColorAlpha, specularColorRoughness;
      switch (shadingModel) {
        case smPBRMetallicRoughness: {
          const vec3 f0 = vec3(0.04);  // dielectricSpecular
          vec4 baseColor = textureFetchSRGB(uTextures[0], 0, vec4(1.0)) * uMaterial.baseColorFactor;
          vec2 metallicRoughness = clamp(textureFetch(uTextures[1], 1, vec4(1.0)).zy * uMaterial.metallicRoughnessNormalScaleOcclusionStrengthFactor.xy, vec2(0.0, 1e-3), vec2(1.0));
          diffuseColorAlpha = vec4((baseColor.xyz * (vec3(1.0) - f0) * (1.0 - metallicRoughness.x)) * PI, baseColor.w);
          specularColorRoughness = vec4(mix(f0, baseColor.xyz, metallicRoughness.x) * PI, metallicRoughness.y);
          break;
        }
        case smPBRSpecularGlossiness: {
          vec4 specularGlossiness = textureFetchSRGB(uTextures[1], 1, vec4(1.0)) * vec4(uMaterial.specularFactor.xyz, uMaterial.metallicRoughnessNormalScaleOcclusionStrengthFactor.y);
          diffuseColorAlpha = textureFetchSRGB(uTextures[0], 0, vec4(1.0)) * uMaterial.baseColorFactor * vec2((1.0 - max(max(specularGlossiness.x, specularGlossiness.y), specularGlossiness.z)) * PI, 1.0).xxxy;
          specularColorRoughness = vec4(specularGlossiness.xyz * PI, clamp(1.0 - specularGlossiness.w, 1e-3, 1.0));
          break;
        }
      }
      vec3 normal;
      if ((texCoordIndices.x & 0x00000f00u) != 0x00000f00u) {
        vec4 normalTexture = textureFetch(uTextures[2], 2, vec2(0.0, 1.0).xxyx);
        normal = normalize(mat3(normalize(inTangent), normalize(inBitangent), normalize(inNormal)) * normalize((normalTexture.xyz - vec3(0.5)) * (vec2(uMaterial.metallicRoughnessNormalScaleOcclusionStrengthFactor.z, 1.0).xxy * 2.0)));
      } else {
        normal = normalize(inNormal);
      }
      normal *= (((flags & (1u << 6u)) != 0u) && !gl_FrontFacing) ? -1.0 : 1.0;
      vec4 occlusionTexture = textureFetch(uTextures[3], 3, vec4(1.0));
      vec4 emissiveTexture = textureFetchSRGB(uTextures[4], 4, vec4(1.0));
      cavity = clamp(mix(1.0, occlusionTexture.x, uMaterial.metallicRoughnessNormalScaleOcclusionStrengthFactor.w), 0.0, 1.0);
      transparency = 0.0;
      refractiveAngle = 0.0;
      ambientOcclusion = 1.0;
      shadow = 1.0;
      reflectance = max(max(specularColorRoughness.x, specularColorRoughness.y), specularColorRoughness.z);
      vec3 viewDirection = normalize(inCameraRelativePosition);
#ifdef LIGHTS
      imageLightBasedLightDirection = (lightMetaData.x != 0u) ? lights[0].directionZFar.xyz : vec3(0.0, 0.0, -1.0);
#else
      imageLightBasedLightDirection = vec3(0.0, 0.0, -1.0);
#endif
      diffuseOutput = specularOutput = sheenOutput = clearcoatOutput = vec3(0.0);
      if ((flags & (1u << 7u)) != 0u) {
        sheenColorIntensityFactor = uMaterial.sheenColorFactorSheenIntensityFactor;
        if ((texCoordIndices.x & 0x00f00000u) != 0x00f00000u) {
          sheenColorIntensityFactor *= textureFetchSRGB(uTextures[5], 5, vec4(1.0));
        }
        sheenRoughness = max(specularColorRoughness.w, 1e-7);
      }
      if ((flags & (1u << 8u)) != 0u) {
        clearcoatFactor = uMaterial.clearcoatFactorClearcoatRoughnessFactor.x;
        clearcoatRoughness = uMaterial.clearcoatFactorClearcoatRoughnessFactor.y;
        clearcoatF0 = vec3(0.04);
        if ((texCoordIndices.x & 0x0f000000u) != 0x0f000000u) {
          clearcoatFactor *= textureFetch(uTextures[6], 6, vec4(1.0)).x;
        }
        if ((texCoordIndices.x & 0xf0000000u) != 0xf0000000u) {
          clearcoatRoughness *= textureFetch(uTextures[7], 7, vec4(1.0)).y;
        }
        if ((texCoordIndices.y & 0x0000000fu) != 0x0000000fu) {
          vec4 normalTexture = textureFetch(uTextures[8], 8, vec2(0.0, 1.0).xxyx);
          clearcoatNormal = normalize(mat3(normalize(inTangent), normalize(inBitangent), normalize(inNormal)) * normalize((normalTexture.xyz - vec3(0.5)) * (vec2(uMaterial.metallicRoughnessNormalScaleOcclusionStrengthFactor.z, 1.0).xxy * 2.0)));
        } else {
          clearcoatNormal = normalize(inNormal);
        }
        clearcoatNormal *= (((flags & (1u << 6u)) != 0u) && !gl_FrontFacing) ? -1.0 : 1.0;
        clearcoatRoughness = clamp(clearcoatRoughness, 0.0, 1.0);
      }
#ifdef LIGHTS
      if (lightMetaData.x != 0u) {
        for (int lightIndex = 0, lightCount = int(lightMetaData.x); lightIndex < lightCount; lightIndex++) {
          Light light = lights[lightIndex];
          float lightAttenuation = 1.0;
          vec3 lightDirection;
          vec3 lightVector = light.positionRange.xyz - vWorldSpacePosition.xyz;
          vec3 normalizedLightVector = normalize(lightVector);
          if ((uShadows != 0) && ((light.metaData.y & 0x80000000u) == 0u)) {
            switch (light.metaData.x) {
              case 1u:    // Directional
              case 3u: {  // Spot
                vec4 shadowNDC = light.shadowMapMatrix * vec4(vWorldSpacePosition, 1.0);
                shadowNDC /= shadowNDC.w;
                if (all(greaterThanEqual(shadowNDC, vec4(-1.0))) && all(lessThanEqual(shadowNDC, vec4(1.0)))) {
                  shadowNDC.xyz = fma(shadowNDC.xyz, vec3(0.5), vec3(0.5));
                  vec4 moments = (textureLod(uNormalShadowMapArrayTexture, vec3(shadowNDC.xy, float(int(light.metaData.y))), 0.0) + vec2(-0.035955884801, 0.0).xyyy) * mat4(0.2227744146, 0.0771972861, 0.7926986636, 0.0319417555, 0.1549679261, 0.1394629426, 0.7963415838, -0.172282317, 0.1451988946, 0.2120202157, 0.7258694464, -0.2758014811, 0.163127443, 0.2591432266, 0.6539092497, -0.3376131734);
                  lightAttenuation *= 1.0 - reduceLightBleeding(getMSMShadowIntensity(moments, shadowNDC.z, 5e-3, 1e-2), 0.0);
                }
                break;
              }
              case 2u: {  // Point
                float znear = 1e-2, zfar = max(1.0, light.directionZFar.w);
                vec3 vector = light.positionRange.xyz - vWorldSpacePosition;
                vec4 moments = (textureLod(uCubeMapShadowMapArrayTexture, vec4(vec3(normalize(vector)), float(int(light.metaData.y))), 0.0) + vec2(-0.035955884801, 0.0).xyyy) * mat4(0.2227744146, 0.0771972861, 0.7926986636, 0.0319417555, 0.1549679261, 0.1394629426, 0.7963415838, -0.172282317, 0.1451988946, 0.2120202157, 0.7258694464, -0.2758014811, 0.163127443, 0.2591432266, 0.6539092497, -0.3376131734);
                lightAttenuation *= 1.0 - reduceLightBleeding(getMSMShadowIntensity(moments, clamp((length(vector) - znear) / (zfar - znear), 0.0, 1.0), 5e-3, 1e-2), 0.0);
                break;
              }
            }
            if (lightIndex == 0) {
              litIntensity = lightAttenuation;
            }
          }
          switch (light.metaData.x) {
            case 1u: {  // Directional
              lightDirection = -light.directionZFar.xyz;
              break;
            }
            case 2u: {  // Point
              lightDirection = normalizedLightVector;
              break;
            }
            case 3u: {  // Spot
#if 1
              float angularAttenuation = clamp(fma(dot(normalize(light.directionZFar.xyz), -normalizedLightVector), uintBitsToFloat(light.metaData.z), uintBitsToFloat(light.metaData.w)), 0.0, 1.0);
#else
              // Just for as reference
              float innerConeCosinus = uintBitsToFloat(light.metaData.z);
              float outerConeCosinus = uintBitsToFloat(light.metaData.w);
              float actualCosinus = dot(normalize(light.directionZFar.xyz), -normalizedLightVector);
              float angularAttenuation = mix(0.0, mix(smoothstep(outerConeCosinus, innerConeCosinus, actualCosinus), 1.0, step(innerConeCosinus, actualCosinus)), step(outerConeCosinus, actualCosinus));
#endif
              lightAttenuation *= angularAttenuation * angularAttenuation;
              lightDirection = normalizedLightVector;
              break;
            }
            default: {
              continue;
            }
          }
          switch (light.metaData.x) {
            case 2u:    // Point
            case 3u: {  // Spot
              if (light.positionRange.w >= 0.0) {
                float currentDistance = length(lightVector);
                if (currentDistance > 0.0) {
                  lightAttenuation *= 1.0 / (currentDistance * currentDistance);
                  if (light.positionRange.w > 0.0) {
                    float distanceByRange = currentDistance / light.positionRange.w;
                    lightAttenuation *= clamp(1.0 - (distanceByRange * distanceByRange * distanceByRange * distanceByRange), 0.0, 1.0);
                  }
                }
              }
              break;
            }
          }
          if (lightAttenuation > 0.0) {
            doSingleLight(light.colorIntensity.xyz * light.colorIntensity.w,  //
                          vec3(lightAttenuation),                             //
                          lightDirection,                                     //
                          normal.xyz,                                         //
                          diffuseColorAlpha.xyz,                              //
                          specularColorRoughness.xyz,                         //
                          -viewDirection,                                     //
                          refractiveAngle,                                    //
                          transparency,                                       //
                          specularColorRoughness.w,                           //
                          cavity);                                            //
          }
        }
      }
#endif
      diffuseOutput += getDiffuseImageBasedLight(normal.xyz, diffuseColorAlpha.xyz);
      specularOutput += getSpecularImageBasedLight(normal.xyz, specularColorRoughness.xyz, specularColorRoughness.w, viewDirection, litIntensity);
      if ((flags & (1u << 7u)) != 0u) {
        // TODO
      }
      if ((flags & (1u << 8u)) != 0u) {
        clearcoatOutput += getSpecularImageBasedLight(clearcoatNormal.xyz, clearcoatF0.xyz, clearcoatRoughness, viewDirection, litIntensity);
        clearcoatBlendFactor = vec3(clearcoatFactor * specularF(clearcoatF0, clamp(dot(clearcoatNormal, -viewDirection), 0.0, 1.0)));
      } else {
        clearcoatBlendFactor = vec3(0);
      }
      color = vec4(vec3(((diffuseOutput +
#ifndef EXTRAEMISSIONOUTPUT
                          (emissiveTexture.xyz * uMaterial.emissiveFactor.xyz) +
#endif
                          (sheenOutput * (1.0 - reflectance))) *
                         (vec3(1.0) - clearcoatBlendFactor)) +
                        mix(specularOutput, clearcoatOutput, clearcoatBlendFactor)),
                   diffuseColorAlpha.w);
      // color = vec4(sheenOutput, diffuseColorAlpha.w);
#ifdef EXTRAEMISSIONOUTPUT
      emissionColor.xyz = vec4((emissiveTexture.xyz * uMaterial.emissiveFactor.xyz) * (vec3(1.0) - clearcoatBlendFactor), 1.0);
#endif
      break;
    }
    case smUnlit: {
      color = textureFetchSRGB(uTextures[0], 0, vec4(1.0)) * uMaterial.baseColorFactor * vec2((litIntensity * 0.25) + 0.75, 1.0).xxxy;
      break;
    }
  }
  float alpha = color.w * inColor0.w, outputAlpha = mix(1.0, color.w * inColor0.w, float(int(uint((flags >> 5u) & 1u))));
  outFragColor = vec4(color.xyz * inColor0.xyz, outputAlpha);
#ifdef EXTRAEMISSIONOUTPUT
  outFragEmission = vec4(emissionColor.xyz * inColor0.xyz, outputAlpha);
#endif
#endif
#ifdef ALPHATEST
  if (alpha < uintBitsToFloat(uMaterial.alphaCutOffFlagsTex0Tex1.x)) {
    discard;
  }
#endif
}

/*oid main() {
  outFragColor = vec4(vec3(mix(0.25, 1.0, max(0.0, dot(inNormal, vec3(0.0, 0.0, 1.0))))), 1.0);
//outFragColor = vec4(texture(uTexture, inTexCoord)) * vec4(vec3(mix(0.25, 1.0, max(0.0, dot(inNormal, vec3(0.0, 0.0, 1.0))))), 1.0);
}*/