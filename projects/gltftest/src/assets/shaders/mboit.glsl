#ifndef MBOIT_GLSL
#define MBOIT_GLSL

void MBOIT4_GenerateMoments(float depth, float transmittance, out float b_0, out vec4 b) {
  float                                      //
      absorbance = -log(transmittance),      //
      depth_pow2 = depth * depth,            //
      depth_pow4 = depth_pow2 * depth_pow2;  //
  b_0 = absorbance;
  b = vec4(                    //
          depth,               //
          depth_pow2,          //
          depth_pow2 * depth,  //
          depth_pow4) *        //
      absorbance;
}

float MBOIT4_ComputeTransmittanceAtDepthFrom4PowerMoments(  //
    float b_0,                                              //
    vec4 b,                                                 //
    float depth,                                            //
    float bias,                                             //
    float overestimation,                                   //
    vec4 bias_vector                                        //
) {
  // Bias input data to avoid artifacts
  b = mix(b, bias_vector, bias);
  vec3 z;
  z[0] = depth;

  // Compute a Cholesky factorization of the Hankel matrix B storing only non-
  // trivial entries or related products
  float L21D11 = fma(-b[0], b[1], b[2]);
  float D11 = fma(-b[0], b[0], b[1]);
  float InvD11 = 1.0 / D11;
  float L21 = L21D11 * InvD11;
  float SquaredDepthVariance = fma(-b[1], b[1], b[3]);
  float D22 = fma(-L21D11, L21, SquaredDepthVariance);

  // Obtain a scaled inverse image of bz=(1,z[0],z[0]*z[0])^T
  vec3 c = vec3(1.0, z[0], z[0] * z[0]);
  // Forward substitution to solve L*c1=bz
  c[1] -= b.x;
  c[2] -= b.y + L21 * c[1];
  // Scaling to solve D*c2=c1
  c[1] *= InvD11;
  c[2] /= D22;
  // Backward substitution to solve L^T*c3=c2
  c[1] -= L21 * c[2];
  c[0] -= dot(c.yz, b.xy);
  // Solve the quadratic equation c[0]+c[1]*z+c[2]*z^2 to obtain solutions
  // z[1] and z[2]
  float InvC2 = 1.0 / c[2];
  float p = c[1] * InvC2;
  float q = c[0] * InvC2;
  float D = (p * p * 0.25) - q;
  float r = sqrt(D);
  z[1] = -p * 0.5 - r;
  z[2] = -p * 0.5 + r;
  // Compute the absorbance by summing the appropriate weights
  vec3 polynomial;
  vec3 weight_factor = vec3(overestimation, (z[1] < z[0]) ? 1.0 : 0.0, (z[2] < z[0]) ? 1.0 : 0.0);
  float f0 = weight_factor[0];
  float f1 = weight_factor[1];
  float f2 = weight_factor[2];
  float f01 = (f1 - f0) / (z[1] - z[0]);
  float f12 = (f2 - f1) / (z[2] - z[1]);
  float f012 = (f12 - f01) / (z[2] - z[0]);
  polynomial[0] = f012;
  polynomial[1] = polynomial[0];
  polynomial[0] = f01 - polynomial[0] * z[1];
  polynomial[2] = polynomial[1];
  polynomial[1] = polynomial[0] - polynomial[1] * z[0];
  polynomial[0] = f0 - polynomial[0] * z[0];
  float absorbance = polynomial[0] + dot(b.xy, polynomial.yz);
  ;
  // Turn the normalized absorbance into transmittance
  return clamp(exp(-b_0 * absorbance), 0.0, 1.0);
}

void MBOIT4_ResolveMoments(            //
    float depth,                       //
    float b0,                          //
    vec4 b_1234,                       //
    out float transmittance_at_depth,  //
    out float total_transmittance      //
) {
  transmittance_at_depth = 1.0;
  total_transmittance = 1.0;

  if (b0 < 0.00100050033) {  //
    discard;                 //
  }
  total_transmittance = exp(-b0);

  b_1234 /= b0;

  transmittance_at_depth = MBOIT4_ComputeTransmittanceAtDepthFrom4PowerMoments  //
      (                                                                         //
          b0,                                                                   //
          b_1234,                                                               //
          depth,                                                                //
          0.0035                                                                // moment_bias
          0.1                                                                   // overestimation
          vec4(0.0, 0.375, 0.0, 0.375)                                          // bias_vector
      );
}

#endif