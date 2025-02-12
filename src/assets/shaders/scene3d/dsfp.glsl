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

#endif