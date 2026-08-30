#ifndef VOLUMETRIC_SCATTERING_COMPOSE_GLSL
#define VOLUMETRIC_SCATTERING_COMPOSE_GLSL

// Everything the two compose variants share: the half-resolution buffers they read, the switches they are
// steered by, and the depth-aware upsample itself. Two variants because the pass runs at two different
// points of the chain - as a compute pass over the resolved picture, and as a fragment pass over the
// still-multisampled one - and the arithmetic between those two must not be allowed to drift apart. What
// differs is only where the colour comes from and where it goes; the stage files hold that and nothing else.

layout(set = 0, binding = 1) uniform sampler2DArray uTextureInscattering; // the march, half size
layout(set = 0, binding = 2) uniform sampler2DArray uTextureExtinction;   // and what it takes away

// The depth the march measured, at HALF size, in the full-float image it wrote once. This is what the four
// coarse taps are weighed by; it is the same image the two blur steps read, and none of the three rewrites
// it. Not to be confused with the full-resolution opaque depth of the pixel being shaded, which each stage
// file binds for itself - the whole upsample is the comparison between those two.
layout(set = 0, binding = 5) uniform sampler2DArray uTextureScatteringDepth;

#ifdef VOLUMETRIC_SCATTERING_DUAL_OUTPUT
// What the air does over the SKY's distance at each texel - the same ray as the pair above, carried past
// the geometry instead of stopping at it. With this bound there is nothing left to borrow and nothing left
// to guess: a sample that looks at the sky reads the sky's air at its own pixel.
layout(set = 0, binding = 7) uniform sampler2DArray uTextureFarInscattering;
layout(set = 0, binding = 8) uniform sampler2DArray uTextureFarExtinction;
#endif

layout(push_constant) uniform PushConstants {
  // x = strength, y = how depth becomes a distance, z = depth weight, w = the largest depth the march may
  // store. That last one is a bound of the half-float buffer and not of the scene, which is the only kind
  // of bound there is under a reversed infinite projection - see the note at linearDepth below.
  vec4 strengthZNearDepthWeightSkyDepth;
  // Switches, in a word of their own rather than squeezed into a spare float lane.
  //
  // Flag bit 0 - VolumetricScatteringComposeFlagShowScatteringOnly - dims the picture almost to nothing
  // and leaves the scattering at full weight. A measurement rather than a look: anything odd in a finished
  // frame could belong to this effect or to something else entirely, and there is no telling the two apart
  // once both are added at equal weight.
  //
  // Flag bit 1 - VolumetricScatteringComposeFlagEnabled - whether the effect does anything at all this
  // frame. Clear hands the picture through untouched. This pass cannot simply be skipped the way the three
  // before it can: it owns the resource the rest of the chain reads, so it has to write something whatever
  // happens. And it must not merely multiply the scattering by nothing either - with the march skipped,
  // the buffers hold whatever was left in them, and a stray infinity times zero is still not zero.
  // x = flags, y = how many samples the raw depth has (multisampled variants only), zw unused
  uvec4 flagsSampleCountSpare;
} pushConstants;

const uint VolumetricScatteringComposeFlagShowScatteringOnly = 1u << 0;
const uint VolumetricScatteringComposeFlagEnabled = 1u << 1;

// Bit two: the extinction alone, as a colour, and nothing else in the frame. The one buffer of the three
// that could not be looked at until now, and the one that multiplies the whole picture - so when the
// world comes out dark there is no telling from a finished frame whether this is the cause or the
// exposure behind it reacting to something else. White here means the air takes nothing away; grey means
// it takes that much; black means the world is being multiplied by nothing and the darkness is here.
const uint VolumetricScatteringComposeFlagShowExtinctionOnly = 1u << 2;

// Bit three: the upsample the old compose did - all four taps weighted and normalised every time, never a
// single one, and with a depth weight so soft that where the depths agree it comes out as a plain 2x2
// average. That averaging is a second, gentle blur over the whole picture, and losing it is not a subtle
// change: it is the difference between the old soft glow and something visibly crisper. The newer path is
// sharper where the depths agree and almost a hard choice where they do not, which is better at holding an
// edge and is not what the old picture looked like.
const uint VolumetricScatteringComposeFlagLegacyLook = 1u << 3;

// Bit four: where none of the four taps is of the same KIND as the pixel being shaded - sky against
// geometry - look further out for one that is, instead of settling for the four that are not.
//
// The case that makes this necessary is a silhouette against the sky. The march computes one answer per
// texel, from the resolved depth, and the resolve keeps the NEAREST sample - so at a silhouette the march
// walked to the geometry, and there is no answer anywhere in the footprint for the sky standing behind it.
// The weighted branch below then hands that sky its four geometry taps, normalised to a quarter each, and
// the sky receives the air of the mountain in front of it. That is the rim along every outline.
//
// Per-sample compositing does not fix it and cannot: it gives every sample its own colour and its own
// depth, but all samples of a pixel still read the same marched air. The missing quantity - the air over
// the SKY's distance at this pixel - was never computed by anybody, at any resolution.
//
// Borrowing it from a neighbour is sound here in a way that borrowing a COLOUR is not. These buffers are
// half resolution and bilaterally blurred, so they are low-frequency by construction and a tap two coarse
// texels away is a fair estimate of what this one would have held. The sky's COLOUR is the opposite of
// that - sun, stars, clouds - and estimating it from neighbours was tried and gave a black outline.
const uint VolumetricScatteringComposeFlagSkyTapSearch = 1u << 4;

// Bit five: weigh the geometry's air against the sky's by how much of the pixel each of them actually
// covers, instead of giving the whole pixel the geometry's.
//
// Only the resolved MSAA path has anything to do with this, and it is the reason that path aliases. It
// walks the samples, skips the sky ones, and applies the average of the rest to the WHOLE pixel - so a
// pixel that is nine tenths sky wears the tower's air over its full area. No rim, because the value stays
// continuous towards the geometry; but the coverage gradient that MSAA went to the trouble of producing is
// flattened, and a hard scattering edge sits over a smooth colour edge.
//
// The pixel's coverage is already known - it is how many samples were geometry - and the sky's answer is
// already computed, in the branch for a pixel that is nothing but sky. The two were simply never weighed
// against each other. Blending the TERMS by coverage is the same thing as compositing twice and blending
// the results, because the composite is affine in them for a fixed colour:
//
//   c*(C*Eg + Ig) + (1-c)*(C*Es + Is)  =  C*(c*Eg + (1-c)*Es) + (c*Ig + (1-c)*Is)
//
// so one composite over blended terms is exact here, not an approximation of two.
//
// This wants the sky-tap search above to be on as well: without it the sky's answer at a silhouette is read
// off geometry taps, and weighing a wrong value in by its coverage only spreads it more politely.
const uint VolumetricScatteringComposeFlagCoverageWeighting = 1u << 5;

// How much of the picture is left standing in that first mode - enough to see where one is, not enough to
// confuse with what is being looked at.
const float ShowScatteringOnlySceneWeight = 0.03;

// How far apart two linear depths may be before the cheap single-sample path is given up. In the units the
// march wrote, so metres here.
const float UpsampleDepthAgreement = 1.0;

// How far that search may reach, in coarse texels beyond the 2x2 footprint. A silhouette pixel lies ON the
// boundary, so the nearest texel of the other kind is almost always in the very first ring; the bound is
// there so that a pixel which genuinely has no neighbour of its kind gives up quickly instead of scanning
// its surroundings for nothing.
const int SkyTapSearchMaximumRadius = 3;

// A pixel's depth as a distance that grows away from the camera, the same way the march computed the one
// it stored - the two have to be the same quantity or comparing them means nothing.
//
// The bound is part of "the same quantity", and it has to be the right one. Under a reversed infinite
// projection a sky pixel's raw depth is exactly zero, so this division is unbounded - and there is no far
// plane to bound it with, because there is no far plane. The bound therefore comes from what the march can
// store in a half-float alpha, and it sits far beyond any geometry so that the sky keeps a depth of its
// own. Bounding at the march's REACH instead - 4096 metres - was the earlier mistake: every mountain past
// four kilometres then read as being exactly as far as the sky, and the upsample could no longer tell the
// silhouette from what was behind it.
float linearDepth(const in float aDepth){

  // Sky first, and by an explicit test rather than by letting the division run into a guard. Under the
  // reversed infinite projection this renderer uses, a sky pixel's raw depth is EXACTLY zero, and the
  // distance it stands for is unbounded - so there is no number to compute, only one to agree on. The
  // march writes the store cap for such a pixel, and this returns the same cap, so the two agree by
  // construction instead of by arithmetic accident.
  //
  // It used to arrive here as zNear/max(rawDepth,1e-7), and that was wrong twice over. The value then
  // depended on the guard constant rather than on anything real; and zNear is NEGATIVE in this
  // convention - the engine marks a reversed infinite projection by a negative near and an infinite
  // negative far - so what came out was around minus a hundred thousand while the march had written plus
  // sixty thousand for the same pixel. Not merely a different number: a different sign.
  //
  // Inside the sky that stayed invisible, because all four taps were equally wrong and the weights
  // normalise. At a silhouette it did not: the sky pixel's own tap sat 160000 away while the geometry tap
  // in front of it sat 99800 away, so the GEOMETRY tap won, and the sky drew its scattering from the
  // mountain it was standing behind. A rim along every outline, on the sky side, at any resolution and
  // under any antialiasing.
  if(aDepth <= 0.0){
    return pushConstants.strengthZNearDepthWeightSkyDepth.w;
  }

  // And the near distance as a magnitude, for the same reason - its sign is a flag about the projection,
  // not part of the distance. Held to half the sky's value, exactly as the march holds it, so that no
  // geometry can arrive at the number the sky is recognised by.
  return min(abs(pushConstants.strengthZNearDepthWeightSkyDepth.y) / aDepth,
             pushConstants.strengthZNearDepthWeightSkyDepth.w * 0.5);

}

// Whether a depth - a stored one or a shaded pixel's own - stands for the sky rather than for something
// solid. Not a threshold with a tolerance around it: the march writes the sky its exact sentinel and
// linearDepth returns that same number, while everything solid is held to half of it, so the two classes
// are separated by a factor of two and the test cannot be close to its own boundary.
bool isSkyDepth(const in float aDepth){
  return aDepth > (pushConstants.strengthZNearDepthWeightSkyDepth.w * 0.75);
}

// The nearest taps that are of the same kind as the pixel being shaded, looked for ring by ring outwards
// from the 2x2 footprint, and averaged over the first ring that holds any. Averaged rather than picked, so
// that the borrowed value does not jump from one neighbour to another along an outline and turn a rim into
// a dotted one.
//
// Rings, and not one box of the full radius, so that the common case costs one ring: a silhouette pixel
// lies on the boundary, so its neighbour of the other kind is right there.
bool findKindredTaps(const in ivec3 at,
                     const in ivec3 scatteringSize,
                     const in ivec2 scatteringBase,
                     const in bool wantSky,
                     out vec3 inscattering,
                     out vec3 extinction){

  inscattering = vec3(0.0);
  extinction = vec3(0.0);

  for(int radius = 1; radius <= SkyTapSearchMaximumRadius; radius++){

    vec3 sumInscattering = vec3(0.0);
    vec3 sumExtinction = vec3(0.0);
    int count = 0;

    for(int y = -radius; y <= (1 + radius); y++){
      for(int x = -radius; x <= (1 + radius); x++){

        // How far outside the 2x2 footprint this offset lies, per axis. Zero inside it, so the shell this
        // offset belongs to is the larger of the two - and only the current shell is looked at, everything
        // nearer having been tried in an earlier turn of the loop.
        int outsideX = (x < 0) ? (-x) : ((x > 1) ? (x - 1) : 0);
        int outsideY = (y < 0) ? (-y) : ((y > 1) ? (y - 1) : 0);
        if(max(outsideX, outsideY) != radius){
          continue;
        }

        ivec2 tapXY = scatteringBase + ivec2(x, y);

        // Off the edge of the buffer, and NOT clamped back in: clamping would fold the border onto itself
        // and let a pixel borrow from the very texel it is standing on, which is the one that has nothing
        // to offer.
        if(any(lessThan(tapXY, ivec2(0))) || any(greaterThanEqual(tapXY, scatteringSize.xy))){
          continue;
        }

        if(isSkyDepth(texelFetch(uTextureScatteringDepth, ivec3(tapXY, at.z), 0).x) != wantSky){
          continue;
        }

        sumInscattering += texelFetch(uTextureInscattering, ivec3(tapXY, at.z), 0).xyz;
        sumExtinction += texelFetch(uTextureExtinction, ivec3(tapXY, at.z), 0).xyz;
        count++;

      }
    }

    if(count > 0){
      float weight = 1.0 / float(count);
      inscattering = sumInscattering * weight;
      extinction = sumExtinction * weight;
      return true;
    }

  }

  return false;

}

// What the air does at one depth: the four coarse taps around this pixel, weighed by how well each agrees
// with the depth given. Split out of main because under MSAA it has to be answered once per SAMPLE rather
// than once per pixel - see the note there.
void resolveScattering(const in ivec3 at,
                       const in ivec3 scatteringSize,
                       const in ivec2 scatteringBase,
                       const in vec2 scatteringFraction,
                       const in float depthHere,
                       const in bool legacyLook,
                       out vec3 inscattering,
                       out vec3 extinction){

  vec4 taps[4];
  vec3 tapExtinctions[4];
  float depthDifferences[4];

  // Whether this pixel is looking at the sky, and whether any of its four taps is - kept while the taps are
  // fetched, because the search below only has to run when the two disagree entirely.
  bool wantSky = isSkyDepth(depthHere);
  bool anyKindred = false;

#ifdef VOLUMETRIC_SCATTERING_DUAL_OUTPUT

  // Looking at the sky, and the march has computed what the air does over the sky's distance at every
  // texel. So there is nothing to search for and nothing to weigh: EVERY tap holds the right kind of
  // answer, and a plain bilinear read is not a compromise here but the exact thing wanted.
  //
  // This is why the depth weighting is skipped rather than merely satisfied. The depth image says where
  // each ray met geometry, which is what the near pair is indexed by; the far pair is that same ray carried
  // on to the sky, so all four of its texels describe the same distance - the sky's - however different
  // their geometry depths are. Weighing them against a depth they are not indexed by would throw away three
  // of the four taps at exactly the silhouettes this exists for.
  if(wantSky){

    vec4 bilinear = vec4((1.0 - scatteringFraction.x) * (1.0 - scatteringFraction.y),
                         scatteringFraction.x * (1.0 - scatteringFraction.y),
                         (1.0 - scatteringFraction.x) * scatteringFraction.y,
                         scatteringFraction.x * scatteringFraction.y);

    inscattering = vec3(0.0);
    extinction = vec3(0.0);

    for(int y = 0; y < 2; y++){
      for(int x = 0; x < 2; x++){
        ivec2 tapXY = clamp(scatteringBase + ivec2(x, y), ivec2(0), scatteringSize.xy - ivec2(1));
        float weight = bilinear[(y * 2) + x];
        inscattering += texelFetch(uTextureFarInscattering, ivec3(tapXY, at.z), 0).xyz * weight;
        extinction += texelFetch(uTextureFarExtinction, ivec3(tapXY, at.z), 0).xyz * weight;
      }
    }

    return;

  }

#endif

  for(int y = 0; y < 2; y++){
    for(int x = 0; x < 2; x++){
      int index = (y * 2) + x;
      ivec2 tapXY = clamp(scatteringBase + ivec2(x, y), ivec2(0), scatteringSize.xy - ivec2(1));
      taps[index] = texelFetch(uTextureInscattering, ivec3(tapXY, at.z), 0);
      tapExtinctions[index] = texelFetch(uTextureExtinction, ivec3(tapXY, at.z), 0).xyz;
      float tapDepth = texelFetch(uTextureScatteringDepth, ivec3(tapXY, at.z), 0).x;
      depthDifferences[index] = abs(tapDepth - depthHere);
      anyKindred = anyKindred || (isSkyDepth(tapDepth) == wantSky);
    }
  }

  // Not one of the four is of this pixel's kind, so weighing them against each other can only choose among
  // four wrong answers. Look outwards for a right one instead. Left to the flag because it is a borrowed
  // value and not a computed one, and because without it the effect behaves exactly as it did before.
  if((!anyKindred) &&
     ((pushConstants.flagsSampleCountSpare.x & VolumetricScatteringComposeFlagSkyTapSearch) != 0u) &&
     findKindredTaps(at, scatteringSize, scatteringBase, wantSky, inscattering, extinction)){
    return;
  }

  if((!legacyLook) &&
     ((depthDifferences[0] < UpsampleDepthAgreement) && (depthDifferences[1] < UpsampleDepthAgreement)) &&
     ((depthDifferences[2] < UpsampleDepthAgreement) && (depthDifferences[3] < UpsampleDepthAgreement))){

    // All four belong to the same distance, so there is nothing to weigh against depth and the four are
    // simply blended by where this pixel falls between them - a plain bilinear read, done by hand because
    // the other branch needs the four taps separately and one fetch has to serve both. This is the path
    // almost every pixel takes.
    //
    // Blended and not just picked: taking one of the four would shift the whole effect by up to a coarse
    // texel against the picture it is laid over, which reads as the light shafts sitting beside what casts
    // them.
    vec4 bilinear = vec4((1.0 - scatteringFraction.x) * (1.0 - scatteringFraction.y),
                         scatteringFraction.x * (1.0 - scatteringFraction.y),
                         (1.0 - scatteringFraction.x) * scatteringFraction.y,
                         scatteringFraction.x * scatteringFraction.y);
    inscattering = ((taps[0].xyz * bilinear.x) + (taps[1].xyz * bilinear.y)) +
                   ((taps[2].xyz * bilinear.z) + (taps[3].xyz * bilinear.w));
    extinction = ((tapExtinctions[0] * bilinear.x) + (tapExtinctions[1] * bilinear.y)) +
                 ((tapExtinctions[2] * bilinear.z) + (tapExtinctions[3] * bilinear.w));

  }else{

    // Each tap counts by how well its own depth agrees with this pixel's, so that what belongs to the far
    // side of a silhouette contributes almost nothing. Reached only at an edge normally; under the legacy
    // look it is the only path, and with the soft weight that goes with it the four come out as a plain
    // average wherever the depths agree - which is most of the picture, and is the second blur the old
    // compose gave the whole frame.
    float weights[4];
    float sum = 0.0;
    for(int index = 0; index < 4; index++){
      weights[index] = 1.0 / (1.0 + (depthDifferences[index] * pushConstants.strengthZNearDepthWeightSkyDepth.z));
      sum += weights[index];
    }

    inscattering = vec3(0.0);
    extinction = vec3(0.0);
    for(int index = 0; index < 4; index++){
      float weight = weights[index] / max(sum, 1e-8);
      inscattering += taps[index].xyz * weight;
      extinction += tapExtinctions[index] * weight;
    }

  }

}

// The footprint of the four half-resolution texels around a full-resolution pixel. Through the ratio of the
// two sizes rather than a fixed half: half of an odd width is not half, and a coordinate built in one
// resolution and used in another is the mistake this effect has already been caught by once. The same
// footprint serves every sample of the pixel, so it is worked out once.
void scatteringFootprint(const in ivec2 at,
                         const in ivec2 destinationSize,
                         out ivec3 scatteringSize,
                         out ivec2 scatteringBase,
                         out vec2 scatteringFraction){
  scatteringSize = textureSize(uTextureInscattering, 0);
  vec2 scatteringCoord = ((vec2(at) + vec2(0.5)) * (vec2(scatteringSize.xy) / vec2(destinationSize))) - vec2(0.5);
  scatteringBase = ivec2(floor(scatteringCoord));
  scatteringFraction = scatteringCoord - vec2(scatteringBase);
}

// What the pass finally writes, once the air at this pixel is known - shared so that the two stages cannot
// combine the same three quantities in two different ways. The show-only modes live here too, because they
// are answers to "what is in the buffer", and a diagnostic that only tells the truth in one of the two
// variants is worse than none.
vec4 composeScattering(const in vec4 here,
                       const in vec3 inscattering,
                       const in vec3 extinction){

  // Straight out, before anything is combined, so that what is shown is the buffer itself and not the
  // buffer after this pass has had its way with it.
  //
  // Overdriven on purpose. Everything this pass writes still goes through tone mapping and an auto
  // exposure that adapts to whatever it is given, so a flat buffer comes out mid grey whatever its value
  // was, and "is the extinction one or a twentieth" - the whole question - could not be read off it. At
  // eight times, anything near one clips to white and stays white however the exposure moves, while
  // anything genuinely dark has nothing to be lifted out of. The wavelength tint survives it too: a wash
  // that comes out orange is blue being taken away faster than red, which is the Rayleigh model working.
  if((pushConstants.flagsSampleCountSpare.x & VolumetricScatteringComposeFlagShowExtinctionOnly) != 0u){
    return vec4(extinction * 8.0, here.w);
  }

  float sceneWeight = ((pushConstants.flagsSampleCountSpare.x & VolumetricScatteringComposeFlagShowScatteringOnly) != 0u) ?
                        ShowScatteringOnlySceneWeight :
                        1.0;

  // The strength dials the effect as a whole: it scales what was gathered, and pulls the extinction back
  // toward one by the same amount, so that turning it down thins the air rather than leaving it thick and
  // unlit.
  float strength = pushConstants.strengthZNearDepthWeightSkyDepth.x;
  vec3 appliedExtinction = mix(vec3(1.0), extinction, clamp(strength, 0.0, 1.0));

  return vec4(((here.xyz * sceneWeight) * appliedExtinction) + (inscattering * strength), here.w);

}

#endif
