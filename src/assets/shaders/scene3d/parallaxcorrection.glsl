#ifndef PARALLAXCORRECTION_GLSL
#define PARALLAXCORRECTION_GLSL

vec3 parallaxCorrectedReflection(vec3 reflectionDirection, vec3 fragmentWorldPosition, vec3 cameraWorldPosition){
    
#ifndef PARALLAX_CORRECTION_METHOD
#define PARALLAX_CORRECTION_METHOD 0 // 0 = None, 1 = Offset, 2 = Vector, 3 = Halfway (all without proxy geometry, at the moment) 
#endif

#if PARALLAX_CORRECTION_METHOD == 1

  // The most straightforward way to do parallax correction is to adjust the reflection vector based on the relative positions of the 
  // fragment and the camera. This adjustment will be based on how the view direction intersects with the virtual "bounding box" of the cubemap.
  // Given that a cubemap is, conceptually, a bounding box surrounding the scene, we can think of the parallax correction as finding the intersection 
  // of the view direction with this bounding box and using that point to adjust the reflection vector. Here's an approach to do this:

  // Calculate the normalized view direction, which is the direction from the camera to the fragment.
  vec3 viewDirection = normalize(fragmentWorldPosition - cameraWorldPosition);
  
  // Compute the offset between the view direction and the original reflection direction.
  // This offset represents how much the reflection direction should be adjusted to account for the viewer's position.
  vec3 offset = viewDirection - reflectionDirection;
  
  // Apply the offset to the original reflection direction to get the parallax-corrected reflection direction.
  vec3 parallaxCorrectedReflectionDirection = reflectionDirection + offset;

  return normalize(parallaxCorrectedReflectionDirection);

#elif PARALLAX_CORRECTION_METHOD == 2

  // Another approach to parallax correction is to compute the reflection direction as usual and then adjust it based on the relative positions of the
  // fragment and the camera. This adjustment will be based on how the reflection direction intersects with the virtual "bounding box" of the cubemap.
  // Given just the fragment position, camera position, and reflection direction, we can only apply a general parallax correction, assuming a virtual 
  // "bounding box" around the scene. Here's an approach to do this:

  // Normalize the input reflection direction
  vec3 normalizedReflectionDirection = normalize(reflectionDirection);

  // Compute the view direction, which is the direction from the camera to the fragment
  vec3 viewDirection = fragmentWorldPosition - cameraWorldPosition;
  
  // Create a vector perpendicular to the reflection direction and the view direction.
  vec3 perpendicularVector = cross(normalizedReflectionDirection, viewDirection);
  
  // Create another vector perpendicular to the reflection direction and the first perpendicular vector.
  vec3 correctionVector = cross(perpendicularVector, normalizedReflectionDirection);
  
  // Use the magnitude of the view direction to apply the parallax correction.
  float parallaxMagnitude = length(viewDirection) * 0.5;  // The scale factor (0.5) can be adjusted.
  
  // Apply the parallax correction to the reflection direction.
  // The reflection direction is shifted by a fraction of the parallax-reflected direction.
  vec3 parallaxCorrectedReflectionDirection = normalizedReflectionDirection + (correctionVector * parallaxMagnitude);

  return normalize(parallaxCorrectedReflectionDirection);

#elif PARALLAX_CORRECTION_METHOD == 3

  vec3 localSurfaceNormal = inNormal;

  // Normalize the input reflection direction
  vec3 normalizedReflectionDirection = normalize(reflectionDirection);
  
  // Compute the view direction, which is the direction from the camera to the fragment
  vec3 viewDirection = fragmentWorldPosition - cameraWorldPosition;
  
  // Calculate the halfway vector between the view direction and the reflection direction.
  // This is often used in shading models, especially for specular reflections.
  vec3 halfwayVector = normalize(viewDirection + normalizedReflectionDirection);
  
  // Compute the reflection of the view direction about the local surface normal.
  // This would be the reflection vector if the surface was a perfect mirror.
  vec3 parallaxReflectedDirection = reflect(viewDirection, localSurfaceNormal);
  
  // Compute a scale factor based on the angle between the halfway vector and the local surface normal.
  // The dot product here effectively measures the cosine of the angle between the two vectors.
  // This factor will be used to adjust the reflection direction based on the viewer's position.
  float parallaxScaleFactor = 0.5 * dot(halfwayVector, localSurfaceNormal);
  
  // Apply the parallax correction to the reflection direction.
  // The reflection direction is shifted by a fraction of the parallax-reflected direction.
  vec3 parallaxCorrectedReflectionDirection = normalizedReflectionDirection + (parallaxReflectedDirection * parallaxScaleFactor);
  
  // Return the normalized parallax-corrected reflection direction.
  return normalize(parallaxCorrectedReflectionDirection);

#else

  return reflectionDirection;

#endif

}

#endif