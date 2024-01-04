#ifndef COLOR_GRADING_GLSL
#define COLOR_GRADING_GLSL

#include "colortemperature.glsl"
#include "rec2020.glsl"

// Order of operations:
// - Exposure (other previous shader)
// - Night adaptation (not implemented yet)
// - White balance
// - Channel mixer
// - Shadows/mid-tones/highlights
// - Slope/offset/power (CDL)
// - Contrast
// - Hue
// - Vibrance
// - Saturation
// - Curves
// - Tone mapping (other next shader)

struct ColorGradingSettings {

  // Exposure, night adaptation, white balance
  vec4 exposureNightAndWhiteBalanceTemperatureTint; // x: exposure, y: night adaptation, z: white balance temperature, w: white balance tint
  
  // Channel mixer
  vec4 channelMixerRed; // x: red, y: green, z: blue, w: unused
  vec4 channelMixerGreen; // x: red, y: green, z: blue, w: unused
  vec4 channelMixerBlue; // x: red, y: green, z: blue, w: unused

  // Shadows/mid-tones/highlights 
  vec4 shadows;
  vec4 midtones;
  vec4 highlights;
  vec4 tonalRanges;

  // ASC CDL
  vec4 slope;
  vec4 offset;
  vec4 power;

  // Contrast, vibrance, saturation, hue
  vec4 contrastVibranceSaturationHue; // x: contrast, y: vibrance, z: saturation, w: hue

  // Curves
  vec4 curvesGamma; // x: red, y: green, z: blue, w: unused
  vec4 curvesMidPoint; // x: red, y: green, z: blue, w: unused
  vec4 curvesScale; // x: red, y: green, z: blue, w: unused

  // Luminance scaling, gamut mapping
  ivec4 luminanceScalingGamutMapping; // x: luminance scaling, y: gamut mapping, z: unused, w: unused
};

ColorGradingSettings defaultColorColorGradingSettings = ColorGradingSettings(
  vec4(0.0, 0.0, 0.0, 0.0), // exposureNightAndWhiteBalanceTemperatureTint
  vec4(1.0, 0.0, 0.0, 0.0), // channelMixerRed
  vec4(0.0, 1.0, 0.0, 0.0), // channelMixerGreen
  vec4(0.0, 0.0, 1.0, 0.0), // channelMixerBlue
  vec4(1.0, 1.0, 1.0, 0.0), // shadows
  vec4(1.0, 1.0, 1.0, 0.0), // midtones
  vec4(1.0, 1.0, 1.0, 0.0), // highlights
  vec4(0.0, 0.333, 0.55, 1.0), // tonalRanges, defaults from DaVinci Resolve 
  vec4(1.0, 1.0, 1.0, 0.0), // slope
  vec4(0.0, 0.0, 0.0, 0.0), // offset
  vec4(1.0, 1.0, 1.0, 0.0), // power
  vec4(1.0, 1.0, 1.0, 0.0), // contrastVibranceSaturationHue
  vec4(1.0, 1.0, 1.0, 0.0), // curvesGamma
  vec4(1.0, 1.0, 1.0, 0.0), // curvesMidPoint
  vec4(1.0, 1.0, 1.0, 0.0) // curvesScale
  ivec4(0, 0, 0, 0) // luminanceScalingGamutMapping
);

vec3 applyColorGrading(vec3 color, const in ColorGradingSettings colorGradingSettings){

  // const vec3 LinearRGBLuminanceWeighting = vec3(0.2126729, 0.7151522, 0.0721750); // Rec. 709 / Linear RGB
  
  // White balance in linear space
  {
    float x = 0.31271 - ((colorGradingSettings.exposureNightAndWhiteBalanceTemperatureTint.z * (1.0 / 6.0)) *  
                         ((colorGradingSettings.exposureNightAndWhiteBalanceTemperatureTint.z < 0.0) ? 0.1 : 0.05));
    float y = fma(colorGradingSettings.exposureNightAndWhiteBalanceTemperatureTint.w, 0.05 / 6.0, ((2.87 * x) - (3.0 * x * x)) - 0.27509507);
    color = max(vec3(0.0),
                mat3(2.85847, -1.62879, -0.0248910, -0.210182, 1.15820, 0.000324281, 0.0418120, -0.118169, 1.06867) *
              ((mat3(0.390405, 0.549941, 0.00892632, 0.0708416, 0.963172, 0.00135775, 0.0231082, 0.128021, 0.936245) * color) *
                (vec3(0.949237, 1.03542, 1.08728) / // D65 white point
                (mat3(0.7328, 0.4296, -0.1624, -0.7036, 1.6975, 0.0061, 0.0030, 0.0136, 0.9834) * 
                  ((vec3(x / y, 1, 1.0 - (x + y)) * 1.0) / vec2(1.0, y).yxy)
                )
                )
              ));    
  }

  // From linear sRGB to linear Rec. 2020 color space
  color = LinearSRGBToLinearRec2020Matrix * max(vec3(0.0), color);

  // Channel mixer
  color = vec3(
    dot(color, colorGradingSettings.channelMixerRed.xyz),
    dot(color, colorGradingSettings.channelMixerGreen.xyz),
    dot(color, colorGradingSettings.channelMixerBlue.xyz)
  );

  // Shadows/mid-tones/highlights
  {
    float y = dot(color, LinearRec2020LuminanceWeights),
          s = 1.0 - smoothstep(colorGradingSettings.tonalRanges.x, colorGradingSettings.tonalRanges.y, y),
          h = smoothstep(colorGradingSettings.tonalRanges.z, colorGradingSettings.tonalRanges.w, y),
          m = 1.0 - (s + h);
    color = (color * s * colorGradingSettings.shadows.xyz) + 
            (color * m * colorGradingSettings.midtones.xyz) +
            (color * h * colorGradingSettings.highlights.xyz);:
  }

  { 
    // Stuff which behaves better in log space

    // Linear to Log Space
    color = fma(log(fma(color, vec3(5.555556), vec3(0.047996))) * 0.43429448190325176, vec3(0.244161), vec3(0.386036));

    // ASC CDL
    color = fma(color, colorGradingSettings.slope.xyz, colorGradingSettings.offset.xyz);
    color = mix(pow(color, colorGradingSettings.power.xyz), color, vec3(lessThanEqual(color, vec3(0.0))));

    // Contrast
    color = mix(vec3(0.4135884), color, colorGradingSettings.contrastVibranceSaturationHue.x);

    // Log Space to Linear
    color = max(vec3(0.0), fma(exp(2.302585092994046 * fma(color, vec3(4.095658192749866), vec3(-1.5810715060963874))), vec3(0.17999998560000113), vec3(-0.008639279308857654)));

  }

  // Hue
  if(colorGradingSettings.contrastVibranceSaturationHue.w != 0.0){
    vec3 hueRotationValues = vec3(0.57735, sin(vec2(radians(colorGradingSettings.contrastVibranceSaturationHue.w)) + vec2(0.0, 1.57079632679)));
    color = mix(hueRotationValues.xxx * dot(hueRotationValues.xxx, color), color, hueRotationValues.z) + (cross(hueRotationValues.xxx, color) * hueRotationValues.y);
  }

  // Vibrance
  {
    float s = (colorGradingSettings.contrastVibranceSaturationHue.y - 1.0) / (1.0 + exp(-3.0 * (color.x - max(color.y, color.z)))) + 1.0;
    vec3 l = LinearRec2020LuminanceWeights * (1.0 - s);
    color = vec3(dot(color, l + vec2(s, 0.0).xyy), dot(color, l + vec2(s, 0.0).yxy), dot(color, l + vec2(s, 0.0).yyx));    
  }

  // Saturation
  color = max(vec3(0.0), mix(vec3(dot(color, LinearRec2020LuminanceWeights)), color, colorGradingSettings.contrastVibranceSaturationHue.z));
  
  // Curves - "Practical HDR and Wide Color Techniques in Gran Turismo SPORT", Uchimura 2018
  {
    vec3 d = vec3(1.0) / pow(colorGradingSettings.curvesMidPoint.xyz, colorGradingSettings.curvesGamma.xyz - vec3(1.0));
    vec3 dark = pow(color, colorGradingSettings.curvesGamma.xyz) * d;
    vec3 light = fma(color - colorGradingSettings.curvesMidPoint.xyz, colorGradingSettings.curvesScale.xyz, colorGradingSettings.curvesMidPoint.xyz);
    color = mix(light, dark, vec3(lessThanEqual(color, colorGradingSettings.curvesMidPoint.xyz)));
  }

  // From linear Rec. 2020 color space to linear sRGB
  color = LinearRec2020ToLinearSRGBMatrix * max(vec3(0.0), color);

  return color;

}

#endif 