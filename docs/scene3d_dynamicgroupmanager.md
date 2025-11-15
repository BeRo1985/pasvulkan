# PasVulkan Scene3D Dynamic Group Manager

## Overview

The `PasVulkan.Scene3D.DynamicGroupManager` provides efficient management of Scene3D rendering resources with a focus on:
- **Memory efficiency**: Sharing heavy GPU resources across multiple render instances
- **Performance**: Minimizing GPU memory allocations during gameplay
- **Flexibility**: Generic, application-agnostic design

## Architecture

### Three-Level Resource Hierarchy

```
Level 1: TGroup (Model Definition)
  ├─ One per unique model/mesh type
  ├─ Contains: Vertices, indices, materials, skeleton
  └─ Heavy GPU memory

Level 2: TInstance (Animation State)
  ├─ One per unique animation configuration
  ├─ Contains: Animated vertex data, joint matrices
  ├─ Shared by all render instances in same state
  ├─ Heavy GPU memory
  └─ NEVER freed during gameplay (only recycled)

Level 3: TRenderInstance (Individual Object)
  ├─ One per visible object
  ├─ Contains: Transform matrix, instance-specific data
  └─ Lightweight (~few hundred bytes per instance)
```

### Key Principles

1. **Group Sharing**: All objects of the same type share one `TGroup`
2. **Instance Reuse**: `TInstance` objects are pooled and reused, never freed during gameplay
3. **State-Based Batching**: Objects with identical animation states share the same `TInstance`
4. **Lightweight Rendering**: Each object only needs a tiny `TRenderInstance`

## Core Types

### TpvScene3DDynamicGroupManager.TGroupKey
```pascal
TGroupKey = TpvUInt64;
```
Generic identifier for model types. Applications decide what this represents:
- Could be a hash of model filename
- Could be an enum cast to UInt64
- Could be a unique type ID

### TpvScene3DDynamicGroupManager.TStateKey
```pascal
TStateKey = record
  Hash: TpvUInt64;              // Primary comparison hash
  Data: array[0..7] of TpvUInt64; // Custom state data
end;
```
Represents an animation/rendering state. Applications encode their state data here:
- Animation clip IDs
- Blend weights
- Material overrides
- Any other state that affects rendering

## Basic Usage Pattern

### 1. Initialize Manager

```pascal
var
  Manager: TpvScene3DDynamicGroupManager;
  Scene3D: TpvScene3D;
begin
  Scene3D := TpvScene3D.Create(...);
  Manager := TpvScene3DDynamicGroupManager.Create(Scene3D);
end;
```

### 2. Register Groups (Model Types)

```pascal
const
  GROUP_KEY_COW = 1;
  GROUP_KEY_SHEEP = 2;
  
var
  CowGroup: TpvScene3D.TGroup;
begin
  // Load cow model
  CowGroup := LoadModelFromFile('models/cow.gltf');
  
  // Register it
  Manager.RegisterGroup(GROUP_KEY_COW, CowGroup);
end;
```

### 3. Create Animation State Keys

```pascal
function CreateAnimationStateKey(const aAnimationID:TpvInt32;
                                  const aTime:TpvDouble;
                                  const aBlendWeight:TpvFloat):TpvScene3DDynamicGroupManager.TStateKey;
var StateData:array[0..2] of TpvUInt64;
begin
 // Encode your animation state into the key
 StateData[0]:=TpvUInt64(aAnimationID);
 StateData[1]:=TpvUInt64(Round(aTime*1000.0));
 StateData[2]:=TpvUInt64(Round(aBlendWeight*65536.0));
 result:=TpvScene3DDynamicGroupManager.TStateKey.CreateFromData(StateData);
end;
```

### 4. Acquire Instances for Rendering

```pascal
var StateKey:TpvScene3DDynamicGroupManager.TStateKey;
    Instance:TpvScene3D.TGroup.TInstance;
    RenderInstance:TpvScene3D.TGroup.TInstance.TRenderInstance;
begin
 // Get or create instance for this state
 StateKey:=CreateAnimationStateKey(ANIM_WALK,1.5,1.0);
 Instance:=Manager.AcquireInstance(GROUP_KEY_COW,StateKey);
 // Create render instance for this specific cow
 RenderInstance:=Manager.AcquireRenderInstance(Instance);
 RenderInstance.ModelMatrix:=TransformMatrix;
 RenderInstance.Active:=true;
end;
```

### 5. Clean Up When Done

```pascal
begin
 // Release render instance (goes back to pool)
 Manager.ReleaseRenderInstance(RenderInstance);
 // Release instance reference (may deactivate if no more refs)
 Manager.ReleaseInstance(Instance);
end;
```

## Game Integration Example (Seeds/PlanetGame1)

### Game-Specific Helper Functions

```pascal
// In UnitGameCharacters.pas or similar

// Convert TCharacterType to GroupKey
function CharacterTypeToGroupKey(const aType:TCharacterType):TpvScene3DDynamicGroupManager.TGroupKey;
begin
 result:=TpvUInt64(Ord(aType))+1000; // Offset to avoid collisions
end;

// Convert game animation state to StateKey
function AnimationStateToStateKey(const aAnimState:TAnimationState;
                                   const aAnimationIDs:array of TpvInt32):TpvScene3DDynamicGroupManager.TStateKey;
var StateData:array[0..7] of TpvUInt64;
begin
 // Encode animation state
 StateData[0]:=TpvUInt64(aAnimationIDs[0]); // Primary anim
 StateData[1]:=TpvUInt64(Round(aAnimState.WalkingRunningState*65536.0));
 StateData[2]:=TpvUInt64(Round(aAnimState.WalkingRunningTime*1000.0));
 StateData[3]:=TpvUInt64(Round(aAnimState.Floating*65536.0));
 // ... encode more state as needed
 result:=TpvScene3DDynamicGroupManager.TStateKey.CreateFromData(StateData);
end;
```

### Modified TGameCharacter

```pascal
type TGameCharacter=class
      private
       fDynamicGroupManager:TpvScene3DDynamicGroupManager;
       fCurrentInstance:TpvScene3D.TGroup.TInstance;
       fRenderInstance:TpvScene3D.TGroup.TInstance.TRenderInstance;
       fCurrentStateKey:TpvScene3DDynamicGroupManager.TStateKey;
      protected
       procedure UpdateAnimationState;
      public
       procedure InitializeRendering;
       procedure CleanupRendering;
     end;

procedure TGameCharacter.InitializeRendering;
var GroupKey:TpvScene3DDynamicGroupManager.TGroupKey;
    StateKey:TpvScene3DDynamicGroupManager.TStateKey;
begin
 // Get manager from game
 fDynamicGroupManager:=TGame(fGame).DynamicGroupManager;
 
 // Convert character type to group key
 GroupKey:=CharacterTypeToGroupKey(fCharacterType);
 
 // Generate initial state key
 StateKey:=AnimationStateToStateKey(fAnimationState,[...]);
 fCurrentStateKey:=StateKey;
 
 // Acquire instance for this state
 fCurrentInstance:=fDynamicGroupManager.AcquireInstance(GroupKey,StateKey);
 
 // Acquire render instance
 fRenderInstance:=fDynamicGroupManager.AcquireRenderInstance(fCurrentInstance);
end;

procedure TGameCharacter.UpdateAnimationState;
var NewStateKey:TpvScene3DDynamicGroupManager.TStateKey;
    NewInstance:TpvScene3D.TGroup.TInstance;
    GroupKey:TpvScene3DDynamicGroupManager.TGroupKey;
begin
 // Generate new state key from current animation
 NewStateKey:=AnimationStateToStateKey(fAnimationState,[...]);
 
 // Check if state changed
 if not NewStateKey.Equals(fCurrentStateKey) then begin
  // State changed, need new instance
  GroupKey:=CharacterTypeToGroupKey(fCharacterType);
  
  // Release old render instance
  fDynamicGroupManager.ReleaseRenderInstance(fRenderInstance);
  
  // Release old instance reference
  fDynamicGroupManager.ReleaseInstance(fCurrentInstance);
  
  // Acquire new instance
  NewInstance:=fDynamicGroupManager.AcquireInstance(GroupKey,NewStateKey);
  fCurrentInstance:=NewInstance;
  fCurrentStateKey:=NewStateKey;
  
  // Acquire new render instance
  fRenderInstance:=fDynamicGroupManager.AcquireRenderInstance(fCurrentInstance);
 end;
 
 // Update transform
 fRenderInstance.ModelMatrix:=GetWorldTransform;
end;

procedure TGameCharacter.CleanupRendering;
begin
 if assigned(fRenderInstance) then begin
  fDynamicGroupManager.ReleaseRenderInstance(fRenderInstance);
  fRenderInstance:=nil;
 end;
 
 if assigned(fCurrentInstance) then begin
  fDynamicGroupManager.ReleaseInstance(fCurrentInstance);
  fCurrentInstance:=nil;
 end;
end;
```

## Advanced Features

### Custom State Comparison

If the default hash+data comparison isn't sufficient, provide custom logic:

```pascal
function MyCustomStateCompare(const aKeyA,aKeyB:TpvScene3DDynamicGroupManager.TStateKey):Boolean;
begin
 // Custom comparison logic
 result:=(aKeyA.Data[0]=aKeyB.Data[0]) and 
         (Abs(TpvInt64(aKeyA.Data[1])-TpvInt64(aKeyB.Data[1]))<100); // Time tolerance
end;

Manager.CustomStateCompare:=MyCustomStateCompare;
```

### Per-Frame Updates

```pascal
procedure TGame.Update;
begin
 // Call at start of each frame
 Manager.BeginFrame;
 
 // ... rest of update logic
end;
```

### Statistics

```pascal
writeln('Instance pool size: ',Manager.GetInstancePoolSize);
writeln('Active instances: ',Manager.GetActiveInstanceCount);
writeln('RenderInstance pool size: ',Manager.GetRenderInstancePoolSize);
```

## Memory Management Notes

1. **Groups**: Registered groups remain until explicitly unregistered
2. **Instances**: In fixed pool mode (aMaxInstances >= 0), instances never freed during gameplay
3. **Instances**: In dynamic mode (aMaxInstances < 0), excess instances freed when free list > 25% of pool
3. **RenderInstances**: Pooled and reused, freed only at shutdown
4. **Thread Safety**: All public methods are thread-safe

## Performance Considerations

- **State Key Design**: Keep state keys as simple as possible
- **Instance Sharing**: More sharing = better performance
- **State Granularity**: Balance between sharing (coarse states) and correctness (fine states)
- **Pool Growth**: Instance pool grows to accommodate peak usage, never shrinks

## Migration Guide

### Before (Direct Creation)
```pascal
fMainGroup:=LoadModel('cow.gltf');
fMainGroupInstance:=fMainGroup.CreateInstance;
```

### After (Using Manager)
```pascal
// One-time setup
Manager.RegisterGroup(GROUP_KEY_COW,
                      LoadModel('cow.gltf'));

// Per-character
fCurrentInstance:=Manager.AcquireInstance(GROUP_KEY_COW,
                                          CreateStateKey(...));

fRenderInstance:=Manager.AcquireRenderInstance(fCurrentInstance);
```

## Reasons

GPU-driven rendering depends on linear memory layouts and large contiguous buffers for random access on the GPU side. Resizing GPU buffers is costly, slow and can lead to out-of-memory errors because:

1. The old buffer must be kept alive until the new buffer is fully populated and in use
2. The old buffer contents must be copied over to the new buffer
3. Only then can the old buffer finally be freed

Frequent allocations and deallocations fragment memory and degrade performance. This manager minimizes such operations by pooling and reusing resources, leading to smoother gameplay and better resource utilization.