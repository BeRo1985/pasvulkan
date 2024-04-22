#ifndef MSAA_SAMPLE_POSITIONS_GLSL
#define MSAA_SAMPLE_POSITIONS_GLSL

// The current Vulkan/SPIR-V spec does not provide a way to retrieve non-standard sample positions. If the pipeline is 
// configured for standard default positions, we can return the positions for rasterizationSamples up to 16. However, for 
// rasterizationSamples of 32 and 64, there are no standard positions defined.

const vec2 msaa1SamplePositions[1] = vec2[1](
  vec2( 0.0/16.0,  0.0/16.0 )
);

const vec2 msaa2SamplePositions[2] = vec2[2](
  vec2( 4.0/16.0,  4.0/16.0 ),
  vec2(-4.0/16.0, -4.0/16.0 )
);

const vec2 msaa4SamplePositions[4] = vec2[4](
  vec2(-2.0/16.0, -6.0/16.0 ),
  vec2( 6.0/16.0, -2.0/16.0 ),
  vec2(-6.0/16.0,  2.0/16.0 ),
  vec2( 2.0/16.0,  6.0/16.0 )
);  

const vec2 msaa8SamplePositions[8] = vec2[8](
  vec2( 1.0/16.0, -3.0/16.0 ),
  vec2(-1.0/16.0,  3.0/16.0 ),
  vec2( 5.0/16.0,  1.0/16.0 ),
  vec2(-3.0/16.0, -5.0/16.0 ),
  vec2(-5.0/16.0,  5.0/16.0 ),
  vec2(-7.0/16.0, -1.0/16.0 ),
  vec2( 3.0/16.0,  7.0/16.0 ),
  vec2( 7.0/16.0, -7.0/16.0 )
);  

const vec2 msaa16SamplePositions[16] = vec2[16](
  vec2( 1.0/16.0,  1.0/16.0 ),
  vec2(-1.0/16.0, -3.0/16.0 ),
  vec2(-3.0/16.0,  2.0/16.0 ),
  vec2( 4.0/16.0, -1.0/16.0 ),
  vec2(-5.0/16.0, -2.0/16.0 ),
  vec2( 2.0/16.0,  5.0/16.0 ),
  vec2( 5.0/16.0,  3.0/16.0 ),
  vec2( 3.0/16.0, -5.0/16.0 ),
  vec2(-2.0/16.0,  6.0/16.0 ),
  vec2( 0.0/16.0, -7.0/16.0 ),
  vec2(-4.0/16.0, -6.0/16.0 ),
  vec2(-6.0/16.0,  4.0/16.0 ),
  vec2(-8.0/16.0,  0.0/16.0 ),
  vec2( 7.0/16.0, -4.0/16.0 ),
  vec2( 6.0/16.0,  7.0/16.0 ),
  vec2(-7.0/16.0, -8.0/16.0 )
);

vec2 getSamplePosition(const in int countSamples, const in int sampleIndex){
  switch(countSamples){
    case 1:{
      return msaa1SamplePositions[sampleIndex];
    }
    case 2:{
      return msaa2SamplePositions[sampleIndex];
    }
    case 4:{
      return msaa4SamplePositions[sampleIndex];
    }
    case 8:{
      return msaa8SamplePositions[sampleIndex];
    }
    case 16:{
      return msaa16SamplePositions[sampleIndex];
    }
    default:{
      // No standard positions defined for 32 and 64 and other sample counts
      return vec2(0.0);
    }
  }
}

#endif // MSAA_SAMPLE_POSITIONS_GLSL