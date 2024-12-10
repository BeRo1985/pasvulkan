#version 460 core

precision highp float;
precision highp int;

#extension GL_EXT_nonuniform_qualifier : require
#extension GL_GOOGLE_include_directive : require
#extension GL_EXT_ray_tracing : require
#extension GL_EXT_buffer_reference : require

#define RAYTRACING
#define USE_MATERIAL_BUFFER_REFERENCE
#define USE_BUFFER_REFERENCE
#define LIGHTS

#include "globaldescriptorset.glsl"

#include "pathtracing.glsl"

layout(location = 0) rayPayloadInEXT RayPayload payload;
layout(location = 1) rayPayloadEXT bool isShadowed;

hitAttributeEXT vec2 hit;

void main(){

}