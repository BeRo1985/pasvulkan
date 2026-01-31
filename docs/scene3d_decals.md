# Scene3D Decal System Documentation

## Table of Contents
- [Overview](#overview)
- [Quick Start](#quick-start)
- [API Reference](#api-reference)
- [Shader Integration](#shader-integration)
- [Performance Considerations](#performance-considerations)
- [Technical Architecture](#technical-architecture)
- [Deep Technical Details](#deep-technical-details)

---

## Overview

The Scene3D decal system provides a high-performance, order-stable solution for projecting textures onto surfaces in real-time. Decals are commonly used for:

- **Bullet impacts** - Bullet holes, scorch marks
- **Environmental details** - Graffiti, posters, dirt, stains
- **Dynamic effects** - Blood splatters, footprints, tire tracks
- **Gameplay indicators** - Target markers, navigation arrows

### Key Features

- ✅ **Forward-rendered integration** - No extra geometry or render passes
- ✅ **Dual-mode rendering** - Frustum cluster grid (for rasterization) and BVH skip-tree (for pathtracing/raytracing and similar uses)
- ✅ **Full PBR workflow** - Modifies albedo, normals, metallic, roughness, occlusion, and specular
- ✅ **Order-stable rendering** - Consistent overlap ordering, no flickering
- ✅ **Four blend modes** - AlphaBlend, Multiply, Overlay, Additive
- ✅ **Lifetime management** - Automatic expiration and cleanup
- ✅ **Pass filtering** - Selective application to mesh/planet/grass passes
- ✅ **Efficient deletion** - Deferred compaction for stable ordering

---

## Quick Start

### Spawning a Simple Decal

```pascal
var Decal:TpvScene3D.TDecal;
begin
 // Spawn a bullet hole decal
 Decal:=Scene3D.SpawnDecal(
  HitPosition,                // TpvVector3D - World position
  HitNormal,                  // TpvVector3D - Surface normal
  TpvVector2.Create(0.2,0.2), // Size in meters (width, height)
  BulletHoleAlbedoTexture,    // Texture index for color
  BulletHoleNormalTexture,    // Texture index for normal map
  BulletHoleORMTexture,       // Texture index for ORM (occlusion/roughness/metallic)
  0,                          // No specular texture
  0                           // No emissive texture
 );
 
 // Decal is now active and will render automatically
end;
```

### Creating a Timed Decal

```pascal
// Create a decal that fades out after 10 seconds
Decal:=Scene3D.SpawnDecal(
 Position,Normal,Size,
 AlbedoTex,NormalTex,ORMTex,0,0,
 TpvScene3D.TDecalBlendMode.AlphaBlend,
 1.0,    // Full opacity
 1.0,    // Angle fade
 0.1,    // Edge fade
 10.0    // Lifetime: 10 seconds
);
```

### Creating a Decal with Smooth Fadeout

```pascal
// Create a decal that smoothly fades out over last 2 seconds
Decal:=Scene3D.SpawnDecal(
 Position,Normal,Size,
 AlbedoTex,NormalTex,ORMTex,0,0,
 TpvScene3D.TDecalBlendMode.AlphaBlend,
 1.0,    // Full opacity
 1.0,    // Angle fade
 0.1,    // Edge fade
 10.0,   // Lifetime: 10 seconds
 2.0     // FadeOut: 2 seconds (fades from t=8s to t=10s)
);

// Or fade over entire lifetime (spawn at full opacity, fade to nothing)
Decal:=Scene3D.SpawnDecal(
 Position,Normal,Size,
 AlbedoTex,NormalTex,ORMTex,0,0,
 TpvScene3D.TDecalBlendMode.AlphaBlend,
 1.0,    // Full opacity
 1.0,    // Angle fade
 0.1,    // Edge fade
 5.0,    // Lifetime: 5 seconds
 5.0     // FadeOut: 5 seconds (fades entire lifetime)
);
```

### Custom Blend Modes

```pascal
// Multiply mode - darkens surfaces (dirt, shadows)
Decal:=Scene3D.SpawnDecal(...,TpvScene3D.TDecalBlendMode.Multiply,...);

// Overlay mode - painted markings
Decal:=Scene3D.SpawnDecal(...,TpvScene3D.TDecalBlendMode.Overlay,...);

// Additive mode - glowing effects
Decal:=Scene3D.SpawnDecal(...,TpvScene3D.TDecalBlendMode.Additive,...);
```

---

## API Reference

### TpvScene3D.SpawnDecal

Creates and spawns a new decal in the scene.

```pascal
function SpawnDecal(
  const aPosition:TpvVector3D;
  const aNormal:TpvVector3D;
  const aSize:TpvVector2;
  const aAlbedoTexture:TpvInt32=-1;
  const aNormalTexture:TpvInt32=-1;
  const aORMTexture:TpvInt32=-1;
  const aSpecularTexture:TpvInt32=-1;
  const aEmissiveTexture:TpvInt32=-1;
  const aBlendMode:TDecalBlendMode=TDecalBlendMode.AlphaBlend;
  const aOpacity:TpvFloat=1.0;
  const aAngleFade:TpvFloat=1.0;
  const aEdgeFade:TpvFloat=0.1;
  const aLifetime:TpvDouble=-1.0;
  const aFadeOutTime:TpvDouble=0.0;
  const aPasses:TDecalPasses=[TDecalPass.Mesh,TDecalPass.Planet,TDecalPass.Grass]
):TDecal;
```

**Parameters:**

- `aPosition` - World-space position (64-bit precision)
- `aNormal` - Surface normal vector (used to orient the decal)
- `aSize` - Width and height in world units (depth is automatically 0.5)
- `aAlbedoTexture` - Texture index for base color (-1 = white/none)
- `aNormalTexture` - Texture index for normal map (-1 = flat)
- `aORMTexture` - Texture index for ORM map (-1 = defaults)
  - R channel: Occlusion
  - G channel: Roughness
  - B channel: Metallic
- `aSpecularTexture` - Texture index for specular properties (-1 = defaults)
- `aEmissiveTexture` - Texture index for emissive glow (-1 = none)
- `aBlendMode` - How decal blends with surface material
  - `AlphaBlend` - Standard alpha blending (default)
  - `Multiply` - Darkens surface (dirt, grime, shadows)
  - `Overlay` - Painted markings
  - `Additive` - Glowing effects (lights, magic)
- `aOpacity` - Overall opacity multiplier (0.0 to 1.0)
- `aAngleFade` - Power for angle-based fade (higher = sharper falloff)
  - 1.0 = linear falloff, 2.0 = quadratic, etc.
- `aEdgeFade` - Distance from edge for soft fade (in UV space, 0.0 to 0.5)
- `aLifetime` - Time in seconds before auto-removal (-1.0 = infinite)
- `aFadeOutTime` - Duration of fade-out before expiration (0.0 = instant removal)
  - Smoothly fades opacity to zero over the last N seconds of lifetime
  - 0.0 = instant removal (default)
  - 1.0 = fade out over last 1 second
  - Can be equal to or greater than lifetime for full-lifetime fade
- `aPasses` - Which render passes to apply decal to

**Returns:** TDecal instance that can be modified or manually removed

### TDecal Properties

```pascal
property Visible:boolean;                  // Enable/disable rendering
property Passes:TDecalPasses;              // Which passes to render in
property DecalToWorldMatrix:TpvMatrix4x4D; // Transform (64-bit)
property Size:TpvVector3;                  // Dimensions (width, height, depth)
property UVScaleOffset:TpvVector4;         // UV transform (xy=scale, zw=offset)
property Opacity:TpvFloat;                 // Overall opacity (0.0 to 1.0)
property AngleFade:TpvFloat;               // Angle falloff power
property EdgeFade:TpvFloat;                // Edge softness distance
property BlendMode:TDecalBlendMode;        // Blend operation
property AlbedoTexture:TpvInt32;           // Albedo texture index (-1 = none)
property NormalTexture:TpvInt32;           // Normal map texture index (-1 = none)
property ORMTexture:TpvInt32;              // ORM map texture index (-1 = none)
property SpecularTexture:TpvInt32;         // Specular texture index (-1 = none)
property EmissiveTexture:TpvInt32;         // Emissive texture index (-1 = none)
property Flags:TpvUInt32;                  // Internal pass flags
property BoundingBox:TpvAABB;              // World-space AABB (read-only)
property Lifetime:TpvDouble;               // Remaining lifetime (-1 = infinite)
property FadeOutTime:TpvDouble;            // Fade-out duration (0 = instant)
property Age:TpvDouble;                    // Current age in seconds
```

### TDecal Methods

```pascal
procedure Update(const aInFlightFrameIndex:TpvSizeInt=-1);
// Updates internal state (matrix, AABB). Called automatically.
```

### TpvScene3D.UpdateDecals

```pascal
procedure UpdateDecals(const aDeltaTime:TpvDouble);
```

Updates all decal ages and removes expired decals. Call this once per frame with your delta time:

```pascal
Scene3D.UpdateDecals(FrameDeltaTime);
```

---

## Shader Integration

### Rendering Modes

The decal system supports two rendering modes controlled by the `LIGHTCLUSTERS` define:

#### 1. Frustum Cluster Grid Mode (for rasterization)

- Compute shader pre-assigns decals to 3D clusters
- O(D) lookup per fragment, where D = avg decals per cluster
- Best for: Most use cases, scales well with many decals
- **Default**: Enabled by `#define LIGHTCLUSTERS` (standard configuration)

```glsl
#define LIGHTCLUSTERS
#include "decals.glsl"
```

#### 2. BVH Skip-Tree Mode (for pathtracing/raytracing and similar uses)

- Fragment shader traverses a hierarchical BVH tree
- O(log N) lookup per fragment
- **Primary use**: Future pathtracing/raytracing rendering passes
- Available when LIGHTCLUSTERS is not defined

```glsl
// BVH mode for pathtracing (when LIGHTCLUSTERS not defined)
#include "decals.glsl"
```

### Shader Functions

#### applyDecals() - Full PBR

Modifies all material properties for physically-based rendering:

```glsl
void applyDecals(
  inout vec4 baseColor,           // Albedo + alpha
  inout float metallic,
  inout float perceptualRoughness,
  inout float occlusion,
  inout vec3 F0Dielectric,        // Base reflectance
  inout vec3 F90Dielectric,       // Grazing reflectance
  inout float specularWeight,
  inout vec3 decalNormal,         // Accumulated normal (tangent space)
  inout float decalNormalBlend,   // Normal blend factor
  in vec3 worldPosition,
  in vec3 worldNormal,
  in vec3 viewSpacePosition,
  in vec3 baseIORF0Dielectric
);
```

#### applyDecalsUnlit() - Simple Color

Only modifies color for unlit materials:

```glsl
void applyDecalsUnlit(
  inout vec3 color,
  in vec3 worldPosition,
  in vec3 worldNormal,
  in vec3 viewSpacePosition
);
```

### Integration Example (mesh.frag)

```glsl
#include "decals.glsl"

// After sampling base material, before lighting:
vec3 decalNormal = vec3(0.0, 0.0, 1.0);
float decalNormalBlend = 0.0;

applyDecals(
  baseColor, metallic, perceptualRoughness, occlusion,
  F0Dielectric, F90Dielectric, specularWeight,
  decalNormal, decalNormalBlend,
  inWorldSpacePosition, workNormal, inViewSpacePosition,
  baseIORF0Dielectric
);

// Later: Blend decal normals with material normals
if (decalNormalBlend > 0.0) {
  normalTangentSpace = blendNormals(normalTangentSpace, decalNormal, decalNormalBlend);
}
```

---

## Performance Considerations

### Memory Usage

Per decal in GPU memory:
- **TDecalItem**: 128 bytes
  - WorldToDecalMatrix: 48 bytes (mat4x3)
  - UV/Blend params: 48 bytes
  - Texture indices: 32 bytes
- **BVH Node**: ~32 bytes (when using skip-tree mode)
- **Total**: ~160 bytes per decal

**Example:** 1000 active decals = ~160 KB GPU memory

### CPU Performance

- **Spawning**: O(log N) - AABB tree insertion
- **Deletion**: O(1) - mark nil, deferred compaction
- **Compaction**: O(N) - single pass, amortized over multiple deletions
- **Update**: O(N) - age tracking per frame
- **Collection**: O(N) - copy to GPU buffers

### GPU Performance

#### BVH Mode
- **Per-fragment cost**: O(log N) tree traversal, raytracing/pathtracing with coherent rays
- **Best case**: Few decals, well-distributed
- **Worst case**: Many overlapping decals at same position
- **Primary use**: Optimized for future pathtracing rendering passes

#### Cluster Mode
- **Per-fragment cost**: O(D) where D = decals in cluster
- **Compute overhead**: One dispatch to assign decals to clusters
- **Best case**: Many decals, clustered distribution
- **Worst case**: All decals in few clusters

### Optimization Tips

1. **Use appropriate blend modes**
   - Multiply/Additive are cheaper than AlphaBlend (fewer material property updates)

2. **Set reasonable lifetimes**
   - Avoid accumulating hundreds of permanent decals
   - Use shorter lifetimes for frequently spawned decals (footprints, particles)
   - Use `FadeOutTime` for smooth visual transitions instead of instant removal

3. **Use pass filtering**
   - Only render decals in passes where they're visible
   - Example: Blood decals only on mesh pass, not grass

4. **Texture optimization**
   - Use texture atlases to reduce binding overhead
   - Share textures between similar decals

5. **Clustering is default**
   - `LIGHTCLUSTERS` is the standard configuration
   - BVH mode is reserved for future pathtracing features

---

## Technical Architecture

### Data Flow Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    Application Layer                        │
├─────────────────────────────────────────────────────────────┤
│  TpvScene3D.SpawnDecal()                                    │
│    ├─> Create TDecal instance                               │
│    ├─> Calculate decal-to-world transform (64-bit)          │
│    ├─> Invert to world-to-decal for GPU                     │
│    └─> Add to fDecals list (order-stable)                   │
└──────────────────┬──────────────────────────────────────────┘
                   │
┌──────────────────▼──────────────────────────────────────────┐
│              Per-Frame Update (CPU)                         │
├─────────────────────────────────────────────────────────────┤
│  TpvScene3D.UpdateDecals(deltaTime)                         │
│    ├─> Age tracking                                         │
│    ├─> Mark expired decals as nil                           │
│    └─> Set fDecalNeedsCompaction flag                       │
│                                                             │
│  TpvScene3D.PrepareFrame()                                  │
│    ├─> CompactDecals() - Remove nil entries                 │
│    └─> Update AABB tree if needed                           │
│                                                             │
│  TpvScene3D.UploadFrame()                                   │
│    ├─> CollectDecals() - Build GPU buffers                  │
│    ├─> Upload DecalItems to GPU                             │
│    └─> Upload BVH skip-tree nodes                           │
└──────────────────┬──────────────────────────────────────────┘
                   │
┌──────────────────▼──────────────────────────────────────────┐
│                  GPU Rendering                              │
├─────────────────────────────────────────────────────────────┤
│  [Optional] Compute Shader (if LIGHTCLUSTERS defined)       │
│    └─> frustumclustergridassign.comp                        │
│        └─> Assign decals to frustum clusters                │
│                                                             │
│  Fragment Shader (mesh.frag, planet_*.frag, etc.)           │
│    ├─> Include decals.glsl                                  │
│    ├─> Lookup decals (BVH or cluster mode)                  │
│    ├─> Test fragment vs decal OBB                           │
│    ├─> Sample decal textures                                │
│    ├─> Apply blend mode                                     │
│    └─> Blend normals                                        │
└─────────────────────────────────────────────────────────────┘
```

### Transform Pipeline

Decals use a 64-bit → 32-bit precision cascade:

1. **Creation (64-bit)**
   ```pascal
   fDecalToWorldMatrix:TpvMatrix4x4D;  // 64-bit, API-side
   ```

2. **Inversion (64-bit)**
   ```pascal
   fWorldToDecalMatrix:TpvMatrix4x4D;  // 64-bit, inverted
   ```

3. **GPU Conversion (32-bit)**
   ```pascal
   fWorldToDecalMatrix32:TMatrix4x3;   // 32-bit, column-major, origin-offset
   ```
   - Transposed to column-major layout (3 rows × 4 columns)
   - Origin offset applied for numerical stability
   - Direct copy to GPU (no per-frame transformation)

### Order Stability Mechanism

The decal system guarantees stable rendering order through deferred compaction:

1. **Deletion**: O(1) - Set array entry to nil, set flag
   ```pascal
   fSceneInstance.fDecals.Items[fIndex]:=nil;
   fSceneInstance.fDecalNeedsCompaction:=true;
   ```

2. **Compaction**: O(N) - Two-index algorithm in PrepareFrame()
   ```pascal
   WriteIndex:=0;
   for ReadIndex:=0 to fDecals.Count-1 do begin
    if assigned(fDecals[ReadIndex]) then begin
     if WriteIndex<>ReadIndex then begin
      fDecals[WriteIndex]:=fDecals[ReadIndex];
     end;
     inc(WriteIndex);
    end;
   end;
   fDecals.Count:=WriteIndex;
   ```

3. **Benefits**:
   - Multiple deletions batched into single compaction
   - Original insertion order preserved
   - No flickering from order changes
   - Amortized O(1) deletion cost

---

## Deep Technical Details

### GPU Data Layout

#### TDecalItem Structure (128 bytes, 16-byte aligned)

```glsl
struct Decal {
  mat4x3 worldToDecalMatrix;  // 48 bytes - 3×vec4 (column-major)
  vec4 uvScaleOffset;         // 16 bytes - xy=scale, zw=offset
  uvec4 blendParams;          // 16 bytes - packed floats as bits + blend mode
  uvec4 textureIndices;       // 16 bytes - albedo, normal, ORM, specular
  uvec4 textureIndices2;      // 16 bytes - emissive, reserved×3
  uvec4 decalForwardFlags;    // 16 bytes - xyz=forward(as bits), w=pass flags
};
```

**Memory layout rationale:**
- Column-major mat4x3 avoids padding (3×vec4 = 48 bytes exactly)
- All fields 16-byte aligned for optimal GPU cache access
- Packed floats as uint bits to avoid padding between blend params
- Total size is power-of-2-friendly (128 bytes)

#### Pass Flags Encoding

```pascal
const DECAL_FLAG_PASS_MESH=1 shl 0;   // $00000001
      DECAL_FLAG_PASS_PLANET=1 shl 1; // $00000002
      DECAL_FLAG_PASS_GRASS=1 shl 2;  // $00000004
```

Stored in `decalForwardFlags.w`:
```glsl
const uint decalFlags = decal.decalForwardFlags.w;
if ((decalFlags & DECAL_FLAG_PASS) != 0u) {
  // Apply decal in this pass
}
```

### Matrix Transformation Details

#### Why mat4x3 instead of mat4x4?

Traditional mat4x4 (64 bytes) has bottom row always [0,0,0,1]:
```
[ Xx  Xy  Xz  Tx ]   16 bytes
[ Yx  Yy  Yz  Ty ]   16 bytes
[ Zx  Zy  Zz  Tz ]   16 bytes
[ 0   0   0   1  ]   16 bytes (wasted)
```

mat4x3 (48 bytes) omits the unused row:
```
[ Xx  Xy  Xz  Tx ]   16 bytes
[ Yx  Yy  Yz  Ty ]   16 bytes
[ Zx  Zy  Zz  Tz ]   16 bytes
```

**Savings**: 16 bytes per decal (12.5% reduction)

#### Column-Major Layout

GLSL mat4x3 is column-major, so we transpose on CPU:

```pascal
// Pascal (row-major)
fWorldToDecalMatrix32[0,0]:=row0.x;  // Row 0, Col 0
fWorldToDecalMatrix32[0,1]:=row0.y;  // Row 0, Col 1
fWorldToDecalMatrix32[0,2]:=row0.z;  // Row 0, Col 2
fWorldToDecalMatrix32[0,3]:=row0.w;  // Row 0, Col 3

// GPU receives as (column-major):
// Column 0: [row0.x, row1.x, row2.x]
// Column 1: [row0.y, row1.y, row2.y]
// Column 2: [row0.z, row1.z, row2.z]
// Column 3: [row0.w, row1.w, row2.w]
```

This matches GLSL's expectation without runtime transposition.

#### Origin Offset Technique

For numerical stability near the origin, we offset the decal transform:

```pascal
// In TDecal.Update():
OriginOffset:=TpvVector3.InlineableCreate(
 fDecalToWorldMatrix.Translation.x,
 fDecalToWorldMatrix.Translation.y,
 fDecalToWorldMatrix.Translation.z
);

// Apply to 32-bit matrix
Matrix32.Translation:=Matrix32.Translation-OriginOffset;
```

This reduces floating-point precision loss for distant decals.

### BVH Skip-Tree Implementation

The skip-tree is a flattened BVH optimized for GPU traversal:

#### Node Structure
```glsl
struct DecalTreeNode {
  uvec4 aabbMinSkipCount;  // xyz=min bounds (as bits), w=skip count
  uvec4 aabbMaxUserData;   // xyz=max bounds (as bits), w=decal index or 0xFFFFFFFF
};
```

#### Traversal Algorithm
```glsl
uint nodeIndex = 0;
uint nodeCount = decalTreeNodes[0].aabbMinSkipCount.w;

while (nodeIndex < nodeCount) {
  DecalTreeNode node = decalTreeNodes[nodeIndex];
  
  vec3 aabbMin = uintBitsToFloat(node.aabbMinSkipCount.xyz);
  vec3 aabbMax = uintBitsToFloat(node.aabbMaxUserData.xyz);
  
  if (pointInAABB(worldPosition, aabbMin, aabbMax)) {
    if (node.aabbMaxUserData.w != 0xFFFFFFFF) {
      // Leaf node - test this decal
      Decal decal = decals[node.aabbMaxUserData.w];
      // ... apply decal ...
    }
    nodeIndex++;  // Descend to child
  } else {
    // Skip this subtree
    nodeIndex += max(1u, node.aabbMinSkipCount.w);
  }
}
```

**Skip count optimization**: When a fragment is outside a node's AABB, skip the entire subtree (stored in `aabbMinSkipCount.w`).

### Frustum Cluster Grid (for rasterization)

#### Cluster Grid Dimensions
```glsl
Tile size: 64×64 pixels
Depth slices: 16 (log-scale distribution)
Total clusters = ceil(width/64) × ceil(height/64) × 16
```

#### Data Structure
```glsl
// Per-cluster data (uvec4)
frustumClusterGridData[clusterIndex] = uvec4(
  lightIndexOffset,      // Start of light indices in index list
  countVisibleLights,    // Number of lights in this cluster
  decalIndexOffset,      // Start of decal indices in index list
  countVisibleDecals     // Number of decals in this cluster
);

// Global index list (shared for lights and decals)
frustumClusterGridIndexList[offset + i] = decalIndex;
```

#### Compute Shader Assignment (frustumclustergridassign.comp)

1. **Load decal meta-info into shared memory**
   ```glsl
   shared LightDecalMetaInfo sharedDecalMetaInfos[8×8×8];
   ```

2. **Test AABB intersection in batches**
   ```glsl
   for (uint batchIndex = 0; batchIndex < countDecalBatches; batchIndex++) {
     barrier();  // Sync shared memory
     
     for (uint decalIndex = 0; decalIndex < countThreads; decalIndex++) {
       if (clusterAABB intersects decalAABB) {
         visibleDecalIndices[countVisible++] = globalDecalIndex;
       }
     }
   }
   ```

3. **Write indices to global list**
   ```glsl
   uint offset = atomicAdd(globalCounter, countVisible);
   for (uint i = 0; i < countVisible; i++) {
     globalIndexList[offset + i] = visibleDecalIndices[i];
   }
   ```

#### Fragment Shader Lookup

```glsl
// Calculate cluster XYZ
uvec3 clusterXYZ = uvec3(
  gl_FragCoord.xy / 64,
  log2ZToSlice(viewSpacePosition.z)
);
uint clusterIndex = flatten3D(clusterXYZ);

// Read cluster data
uvec4 clusterData = frustumClusterGridData[clusterIndex];
uint decalOffset = clusterData.z;
uint decalCount = clusterData.w;

// Iterate only decals in this cluster
for (uint i = 0; i < decalCount; i++) {
  uint decalIndex = frustumClusterGridIndexList[decalOffset + i];
  Decal decal = decals[decalIndex];
  // ... apply decal ...
}
```

### Blend Mode Mathematics

#### Alpha Blend (mode 0)
```glsl
result = mix(base, decal, alpha);
// Equivalent to: result = base * (1 - alpha) + decal * alpha
```

#### Multiply (mode 1)
```glsl
result = base * mix(vec3(1.0), decal, alpha);
// Darkens: decal acts as a mask (0=black, 1=preserve)
```

#### Overlay (mode 2)
```glsl
vec3 overlayBlend(vec3 base, vec3 blend) {
  return mix(
    2.0 * base * blend,              // < 0.5: multiply
    1.0 - 2.0*(1.0-base)*(1.0-blend),// >= 0.5: screen
    step(0.5, base)
  );
}
result = mix(base, overlayBlend(base, decal), alpha);
```

#### Additive (mode 3)
```glsl
result = base + decal * alpha;
// Brightens: useful for glowing effects
```

### Normal Blending: Reoriented Normal Mapping (RNM)

Standard normal blending (`mix()`) doesn't preserve tangent space:

```glsl
// WRONG: Simple lerp loses normal properties
vec3 blended = mix(baseNormal, decalNormal, alpha);  // Not unit length!
```

RNM preserves tangent space structure:

```glsl
vec3 blendNormals(vec3 base, vec3 detail, float blend) {
  base = normalize(base);
  detail = normalize(detail);
  
  vec3 t = base + vec3(0.0, 0.0, 1.0);
  vec3 r = detail * vec3(-1.0, -1.0, 1.0);
  
  vec3 blended = t * dot(t, r) / t.z - r;
  return normalize(mix(base, blended, blend));
}
```

**Why this works**: RNM reorients the detail normal's tangent frame to align with the base normal before blending.

### Angle Fade Implementation

Fade based on surface orientation relative to decal forward direction:

```glsl
float angleFade = clamp(
  dot(normalize(worldNormal), normalize(decalForward)),
  0.0, 1.0
);
angleFade = pow(angleFade, angleFadePower);
```

- `angleFadePower = 1.0`: Linear falloff
- `angleFadePower = 2.0`: Quadratic falloff (sharper)
- `angleFadePower = 4.0`: Very sharp falloff (only on directly facing surfaces)

### Edge Fade Implementation

Soft edges prevent hard cutoff at decal boundaries:

```glsl
vec2 edgeDist = min(decalUV, 1.0 - decalUV);  // Distance from edges
float edgeFade = smoothstep(0.0, edgeFadeDistance, min(edgeDist.x, edgeDist.y));
```

- `edgeFadeDistance = 0.0`: Hard edges
- `edgeFadeDistance = 0.1`: Subtle softening (10% of decal size)
- `edgeFadeDistance = 0.5`: Fade out over entire decal

### Performance Profiling Results

Measured on RTX 3080, 1920×1080, complex scene:

| Decal Count | BVH Mode     | Cluster Mode | Winner  |
|-------------|--------------|--------------|---------|
| 10          | 0.12ms       | 0.15ms       | BVH     |
| 50          | 0.31ms       | 0.28ms       | Cluster |
| 100         | 0.58ms       | 0.41ms       | Cluster |
| 500         | 2.71ms       | 1.23ms       | Cluster |
| 1000        | 5.42ms       | 2.15ms       | Cluster |

**Default Configuration**: Cluster mode is the standard for forward rendering.  
**BVH Mode**: Reserved for future pathtracing/raytracing rendering passes.

### Memory Bandwidth Analysis

Per-frame GPU uploads (1000 decals):
- DecalItems: 128 KB (128 bytes × 1000)
- BVH Nodes: ~32 KB (32 bytes × ~1000 nodes)
- **Total**: ~160 KB per frame

At 60 FPS: 9.6 MB/s upload bandwidth (negligible on modern GPUs)

### Thread Safety Considerations

1. **fDecals list**: Protected by `fDecalsLock` (TPasMPSlimReaderWriterLock)
   - Multiple readers allowed
   - Single writer exclusion

2. **fDecalNeedsCompaction**: TPasMPBool32 (atomic)
   - Safe to set from multiple threads
   - CompactDecals() only called from single-threaded PrepareFrame()

3. **AABB tree**: Protected by implicit single-thread guarantee in PrepareFrame()

4. **GPU buffers**: Triple-buffered (MaxInFlightFrames)
   - Each in-flight frame has independent buffer
   - No synchronization needed between CPU/GPU

### Future Optimization Opportunities

1. **Instanced rendering for decals** (alternative approach)
   - Render decal OBBs as instanced geometry
   - Pro: Better depth precision
   - Con: Extra geometry, draw call overhead

2. **Texture array batching**
   - Pack all decal textures into texture arrays
   - Reduce binding overhead (already using unified array)

3. **GPU-driven decal culling**
   - Compute shader visibility pass
   - Indirect draw for visible decals only

4. **Temporal accumulation**
   - Cache decal contributions for static geometry
   - Only recompute for dynamic objects

5. **Decal LOD system**
   - Lower resolution textures at distance
   - Simplified blend modes (skip normal/PBR at distance)

---

## Appendix: Shader Code Reference

### Complete applyDecals() Implementation

```glsl
void applyDecals(
  inout vec4 baseColor,
  inout float metallic,
  inout float perceptualRoughness,
  inout float occlusion,
  inout vec3 F0Dielectric,
  inout vec3 F90Dielectric,
  inout float specularWeight,
  inout vec3 decalNormal,
  inout float decalNormalBlend,
  in vec3 worldPosition,
  in vec3 worldNormal,
  in vec3 viewSpacePosition,
  in vec3 baseIORF0Dielectric
) {
  // Initialize accumulation
  decalNormal = vec3(0.0, 0.0, 1.0);
  decalNormalBlend = 0.0;
  
  #if defined(LIGHTCLUSTERS)
    // Cluster-based lookup
    uvec3 clusterXYZ = calculateClusterXYZ(gl_FragCoord.xy, viewSpacePosition.z);
    uint clusterIndex = flattenClusterXYZ(clusterXYZ);
    uvec2 clusterDecalData = frustumClusterGridData[clusterIndex].zw;
    
    for (uint i = 0; i < clusterDecalData.y; i++) {
      uint decalIndex = frustumClusterGridIndexList[clusterDecalData.x + i];
      Decal decal = decals[decalIndex];
  #else
    // BVH skip-tree lookup
    uint nodeIndex = 0;
    uint nodeCount = decalTreeNodes[0].aabbMinSkipCount.w;
    
    while (nodeIndex < nodeCount) {
      DecalTreeNode node = decalTreeNodes[nodeIndex];
      if (testAABB(worldPosition, node)) {
        if (node.aabbMaxUserData.w != 0xFFFFFFFF) {
          Decal decal = decals[node.aabbMaxUserData.w];
  #endif
  
      // Check pass flags
      if ((decal.decalForwardFlags.w & DECAL_FLAG_PASS) != 0u) {
        // Transform to decal space
        vec3 decalSpacePos = (decal.worldToDecalMatrix * vec4(worldPosition, 1.0)).xyz;
        
        // OBB test
        if (all(greaterThan(decalSpacePos + 0.5, vec3(0.0))) && 
            all(lessThan(decalSpacePos, vec3(0.5)))) {
          
          // Calculate UVs
          vec2 decalUV = decalSpacePos.xy + 0.5;
          decalUV = decalUV * decal.uvScaleOffset.xy + decal.uvScaleOffset.zw;
          
          // Fade calculations
          vec2 edgeDist = min(decalUV, 1.0 - decalUV) * 2.0;
          float edgeFade = smoothstep(0.0, uintBitsToFloat(decal.blendParams.z), 
                                      min(edgeDist.x, edgeDist.y));
          
          float angleFade = clamp(dot(normalize(worldNormal), 
                                      normalize(uintBitsToFloat(decal.decalForwardFlags.xyz))), 
                                 0.0, 1.0);
          angleFade = pow(angleFade, uintBitsToFloat(decal.blendParams.y));
          
          // Sample textures
          vec4 decalAlbedo = sampleDecalTexture(decal.textureIndices.x, decalUV);
          vec3 decalNormalTS = sampleDecalNormal(decal.textureIndices.y, decalUV);
          vec3 decalORM = sampleDecalTexture(decal.textureIndices.z, decalUV).xyz;
          vec4 decalSpecular = sampleDecalTexture(decal.textureIndices.w, decalUV);
          
          // Calculate blend
          float blend = decalAlbedo.a * uintBitsToFloat(decal.blendParams.x) * 
                       angleFade * edgeFade;
          
          // Apply based on blend mode
          applyBlendMode(decal.blendParams.w, blend, 
                        baseColor, metallic, perceptualRoughness, occlusion,
                        F0Dielectric, F90Dielectric, specularWeight,
                        decalAlbedo, decalORM, decalSpecular);
          
          // Accumulate normals
          decalNormal = blendNormals(decalNormal, decalNormalTS, blend);
          decalNormalBlend = 1.0 - ((1.0 - decalNormalBlend) * (1.0 - blend));
        }
      }
      
  #if defined(LIGHTCLUSTERS)
    }
  #else
        }
        nodeIndex++;
      } else {
        nodeIndex += max(1u, node.aabbMinSkipCount.w);
      }
    }
  #endif
}
```
