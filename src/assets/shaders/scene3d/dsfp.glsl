#ifndef DSFP_GLSL
#define DSFP_GLSL

// Double-single floating point, emulating double precision with two floats.

#define dsfp vec2 // x = high, y = low 

dsfp floatToDSFP(float f){
  dsfp result;
  result.x = f;
  result.y = f - result.x;
  return result;
}

float dsfpToFloat(dsfp a){
  return a.x + a.y;
}

dsfp dsfpAdd(dsfp a, dsfp b){
  dsfp result;
  float s = a.x + b.x;
  float v = s - a.x;
  float t = (((b.x - v) + (a.x - (s - v))) + a.y) + b.y;
  result.x = s + t;
  result.y = t - (result.x - s);
  return result;
}

dsfp dsfpSub(dsfp a, dsfp b){
  dsfp result;
  float s = a.x - b.x;
  float v = s - a.x;
  float t = ((((-b.x) - v) + (a.x - (s - v))) + a.y) - b.y;
  result.x = s + t;
  result.y = t - (result.x - s);
  return result;
}

dsfp dsfpMul(dsfp a, dsfp b){
  dsfp result;
  float c11 = a.x * b.x;
  float c21 = a.x * b.y;
  float c2 = a.y * b.x;
  float c3 = a.y * b.y;
  float t1 = c11;
  float t2 = c21 + c2;
  float t3 = c3;
  float t4 = t2 + t3;
  float t5 = t1 + t4;
  result.x = t5;
  result.y = t4 - (t5 - t1);
  return result;
}

dsfp dsfpDiv(dsfp a, dsfp b) {
  dsfp result;
  float q1 = a.x / b.x;
  dsfp bq1 = dsfpMul(b, dsfp(q1, 0.0));
  dsfp r = dsfpSub(a, bq1);
  float q2 = r.x / b.x;
  result.x = q1 + q2;
  result.y = r.x - (result.x * b.x);
  return result;
}

bool dfspEqual(dsfp a, dsfp b){
  return all(equal(a, b));
}

bool dfspNotEqual(dsfp a, dsfp b){
  return any(notEqual(a, b));
}

bool dfspLessThan(dsfp a, dsfp b){
  if(a.x < b.x){
    return true;
  } else if(a.x > b.x){
    return false;
  } else {
    return a.y < b.y;
  }
}

bool dfspLessThanEqual(dsfp a, dsfp b){
  if(a.x < b.x){
    return true;
  } else if(a.x > b.x){
    return false;
  } else {
    return a.y <= b.y;
  }
}

bool dfspGreaterThan(dsfp a, dsfp b){
  if(a.x > b.x){
    return true;
  } else if(a.x < b.x){
    return false;
  } else {
    return a.y > b.y;
  }
}

bool dfspGreaterThanEqual(dsfp a, dsfp b){
  if(a.x > b.x){
    return true;
  } else if(a.x < b.x){
    return false;
  } else {
    return a.y >= b.y;
  }
}

vec3 dsfpVec3TwoSum(vec3 hi, vec3 lo, out vec3 o){
  vec3 s = hi + lo;
  vec3 v = s - hi;
  o = (hi - (s - v)) + (lo - v);
  return s;
}

vec3 dsfpVec3PreciseSum(vec3 ah, vec3 al, vec3 bh, vec3 bl){
  vec3 dh, dl;
  vec3 ch = dsfpVec3TwoSum(ah, bh, dh);
  vec3 cl = dsfpVec3TwoSum(al, bl, dl);
  vec3 chcldh = ch + (cl + dh);
  vec3 e = (cl + dh) - (chcldh - ch);
  return chcldh + (dl + e);
}

/* 

The packed matrix is a 4x4 matrix with the following layout:

affineXX, affineXY, affineXZ, fineX
affineYX, affineYY, affineYZ, fineY
affineZX, affineZY, affineZZ, fineZ
 coarseX,  coarseY,  coarseZ, 1.0
 
*/

vec3 dsfpTransformPosition(mat4 modelPacked, mat4 viewPacked, vec4 vertexPosition){
  vec3 modelCoarse = modelPacked[3].xyz, modelFine = vec3(modelPacked[0].w, modelPacked[1].w, modelPacked[2].w);
  vec3 viewCoarse = viewPacked[3].xyz, viewFine = vec3(viewPacked[0].w, viewPacked[1].w, viewPacked[2].w);
  vec3 displacement = dsfpVec3PreciseSum(modelCoarse, modelFine, -viewCoarse, -viewFine);
  mat4 modelView = mat4(mat3(viewPacked) * mat4x3(modelPacked[0].xyz, modelPacked[1].xyz, modelPacked[2].xyz, displacement)); 
  return (modelView * vertexPosition).xyz;
} 

// Clean the 4x4 matrix by removing the fine components, for direct usage after it.
mat4 dsfpMatrixClean(mat4 m){
  return mat4(vec4(m[0].xyz, 0.0), vec4(m[1].xyz, 0.0), vec4(m[2].xyz, 0.0), m[3].xyzw);
}

#endif