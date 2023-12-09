#ifndef CUBEMAP_GLSL
#define CUBEMAP_GLSL

vec3 getCubeMapDirection(in vec2 uv,
                         in int faceIndex){                        
  vec3 zDir = vec3(ivec3((faceIndex <= 1) ? 1 : 0,
                         (faceIndex & 2) >> 1,
                         (faceIndex & 4) >> 2)) *
             (((faceIndex & 1) == 1) ? -1.0 : 1.0),
       yDir = (faceIndex == 2)
                ? vec3(0.0, 0.0, 1.0)
                : ((faceIndex == 3)
                     ? vec3(0.0, 0.0, -1.0)
                     : vec3(0.0, -1.0, 0.0)),
       xDir = cross(zDir, yDir);
  return normalize((mix(-1.0, 1.0, uv.x) * xDir) +
                   (mix(-1.0, 1.0, uv.y) * yDir) +
                   zDir);
}

vec2 getCubeMapTexCoordAndFaceIndex(in vec3 direction, 
                                    out int faceIndex){
  vec3 absoluteDirection = abs(direction);
	float magnitude;
	vec2 uv;
	if(all(greaterThanEqual(absoluteDirection.zz, absoluteDirection.xy))){
		faceIndex = (direction.z < 0.0) ? 5 : 4;
		uv = vec2((direction.z < 0.0) ? -direction.x : direction.x, -direction.y);
		magnitude = absoluteDirection.z;
	}else if(absoluteDirection.y >= absoluteDirection.x){
		faceIndex = (direction.y < 0.0) ? 3 : 2;
		uv = vec2(direction.x, (direction.y < 0.0) ? -direction.z : direction.z);
		magnitude = absoluteDirection.y;
	}else{
		faceIndex = (direction.x < 0.0) ? 1 : 0;
		uv = vec2((direction.x < 0.0) ? direction.z : -direction.z, -direction.y);
		magnitude = absoluteDirection.x;
	}       
	return ((0.5 / magnitude) * uv) + vec2(0.5);
}

vec3 fixCubeMapLookup(in vec3 direction,
                      in float lod, 
                      in int cubeSize) {
  float magnitude = max(max(abs(direction.x), abs(direction.y)), abs(direction.z)),
        scale = 1.0 - (exp2(lod) / float(cubeSize)); // float(cubeSize - 1) / float(cubeSize);
  return direction *
         vec3((abs(direction.x) != magnitude) ? scale : 1.0, 
              (abs(direction.y) != magnitude) ? scale : 1.0, 
              (abs(direction.z) != magnitude) ? scale : 1.0);
}

vec4 textureManualCubeMap(in sampler2DArray tex,
                          in vec3 direction,
                          in float lod,
                          in int cubeSize) {
  int faceIndex;
  vec2 uv = getCubeMapTexCoordAndFaceIndex(fixCubeMapLookup(direction, lod, cubeSize), faceIndex);
  return textureLod(tex, vec3(uv, float(faceIndex)), lod);
}

#endif