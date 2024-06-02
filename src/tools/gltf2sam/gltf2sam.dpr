program gltf2sam;
{$ifdef fpc}
 {$mode delphi}
{$endif}

uses Classes,
     SysUtils,
     Math,
     PUCU in '../../../externals/pucu/src/PUCU.pas',
     PasMP in '../../../externals/pasmp/src/PasMP.pas',
     PasDblStrUtils in '../../../externals/pasdblstrutils/src/PasDblStrUtils.pas',
     PasJSON in '../../../externals/pasjson/src/PasJSON.pas',
     PasGLTF in '../../../externals/pasgltf/src/PasGLTF.pas',
     Vulkan in '../../Vulkan.pas',
     PasVulkan.Types in '../../PasVulkan.Types.pas',
     PasVulkan.Utils in '../../PasVulkan.Utils.pas',
     PasVulkan.Math in '../../PasVulkan.Math.pas',
     PasVulkan.Collections in '../../PasVulkan.Collections.pas',
     PasVulkan.FileFormats.GLTF in '../../PasVulkan.FileFormats.GLTF.pas',
     PasVulkan.FileFormats.SAM in '../../PasVulkan.FileFormats.SAM.pas';

var Animations:TpvSAM.TAnimations;

    TexCoords:TpvSAM.TTexCoords;

    Indices:TpvSAM.TIndices;

    CountUsedVertices:TpvSizeInt;
    CountUsedIndices:TpvSizeInt;

    GLTF:TpvGLTF;
    GLTFInstance:TpvGLTF.TInstance;

    FileHeader:TpvSAM.TFileHeader;

    BoundingBox:TpvAABB;

    BoundingSphere:TpvSphere;

    BaseColorTextureData:TBytes;
    NormalTextureData:TBytes;
    MetallicRoughnessTextureData:TBytes;
    OcclusionTextureData:TBytes;
    EmissiveTextureData:TBytes;
    
function CompareFramesByTime(const a,b:TpvSAM.TFrame):TpvInt32;
begin
 result:=Sign(a.Time-b.Time);
end;

function OptimizeFrames(var aTimeFrames:TpvSAM.TFrames;out aFrames:TpvSAM.TFrames;const aCountTargetFrames:TpvSizeInt):boolean;
var FrameIndex,CurrentFrameIndex,NextFrameIndex,OutFrameIndex,VertexIndex:TpvSizeInt;
    StartTime,EndTime,Time,TimeStep,InterpolationFactor:TpvDouble;
    CurrentFrame,NextFrame:TpvSAM.PFrame;
    CurrentVertex,NextVertex,InterpolatedVertex:TpvSAM.PFullVertex;
    CurrentTangent,CurrentBitangent,CurrentNormal,
    NextTangent,NextBitangent,NextNormal,
    InterpolatedTangent,InterpolatedBitangent,InterpolatedNormal:TpvVector3;
    CurrentTangentSpace,NextTangentSpace,InterpolatedTangentSpace:TpvMatrix3x3;
begin

 if length(aTimeFrames)=0 then begin
  result:=false;
  exit;
 end;

 // Sort time frames
 TpvTypedSort<TpvSAM.TFrame>.IntroSort(@aTimeFrames[0],0,length(aTimeFrames)-1,CompareFramesByTime);

 // Find start and end time
 StartTime:=aTimeFrames[0].Time;
 EndTime:=aTimeFrames[length(aTimeFrames)-1].Time;

 // Calculate time step
 TimeStep:=(EndTime-StartTime)/aCountTargetFrames;

 // Normalize time frames
 aFrames:=nil;
 SetLength(aFrames,aCountTargetFrames);
 Time:=StartTime;
 FrameIndex:=0;
 OutFrameIndex:=0;
 while (Time<(EndTime+TimeStep)) and (OutFrameIndex<aCountTargetFrames) do begin

  aFrames[OutFrameIndex].Time:=Time;
  aFrames[OutFrameIndex].Vertices:=nil;
  aFrames[OutFrameIndex].FullVertices:=nil;
  SetLength(aFrames[OutFrameIndex].Vertices,CountUsedVertices);
  SetLength(aFrames[OutFrameIndex].FullVertices,CountUsedVertices);

  // Advance to next frame
  while (FrameIndex<length(aTimeFrames)) and (aTimeFrames[FrameIndex].Time<Time) do begin
   inc(FrameIndex);
  end;

  // Get current and next frame indices
  CurrentFrameIndex:=FrameIndex; 
  NextFrameIndex:=Min(CurrentFrameIndex+1,length(aTimeFrames)-1);

  // Get current and next frame pointers  
  CurrentFrame:=@aTimeFrames[CurrentFrameIndex];
  NextFrame:=@aTimeFrames[NextFrameIndex];

  // Interpolate between current and next frame
  InterpolationFactor:=(Time-aTimeFrames[CurrentFrameIndex].Time)/(aTimeFrames[NextFrameIndex].Time-aTimeFrames[CurrentFrameIndex].Time);
  for VertexIndex:=0 to CountUsedVertices-1 do begin
   
   CurrentVertex:=@CurrentFrame^.FullVertices[VertexIndex];
   NextVertex:=@NextFrame^.FullVertices[VertexIndex];

   InterpolatedVertex:=@aFrames[OutFrameIndex].FullVertices[VertexIndex];
   
   InterpolatedVertex^.Position:=CurrentVertex^.Position.Lerp(NextVertex^.Position,InterpolationFactor);

{  InterpolatedVertex^.TexCoordU:=Min(Max(round((CurrentVertex^.TexCoordU*(1.0-InterpolationFactor))+(NextVertex^.TexCoordU*InterpolationFactor)),0),65535);
   InterpolatedVertex^.TexCoordV:=Min(Max(round((CurrentVertex^.TexCoordV*(1.0-InterpolationFactor))+(NextVertex^.TexCoordV*InterpolationFactor)),0),65535);}

   CurrentTangentSpace:=TpvMatrix3x3.Create(CurrentVertex^.Tangent,CurrentVertex^.Bitangent,CurrentVertex^.Normal);
   
   NextTangentSpace:=TpvMatrix3x3.Create(NextVertex^.Tangent,NextVertex^.Bitangent,NextVertex^.Normal);

   InterpolatedTangentSpace:=CurrentTangentSpace.Slerp(NextTangentSpace,InterpolationFactor);

   InterpolatedVertex^.Tangent:=InterpolatedTangentSpace.Tangent;
   InterpolatedVertex^.Bitangent:=InterpolatedTangentSpace.Bitangent;
   InterpolatedVertex^.Normal:=InterpolatedTangentSpace.Normal;

  end; 

  Time:=Time+TimeStep; 

  inc(OutFrameIndex);

 end;

end;

function GetAnimation(const aAnimationIndex:TpvSizeInt):TpvSAM.TAnimation;
var FrameIndex,OtherIndex,TriangleIndex,VertexIndex,CountFrames:TpvSizeInt;
    KeyTimes:TPasGLTFDoubleDynamicArray;
    GLTFBakedVertexIndexedMesh:TpvGLTF.TBakedVertexIndexedMesh;
    GLTFBakedVertexIndexedMeshVertex:TpvGLTF.PVertex;
    Vertex:TpvSAM.PFullVertex;
begin

 result.Name:='';
 result.StartTime:=0.0;
 result.EndTime:=0.0;
 result.Frames:=nil;

 if aAnimationIndex<0 then begin

 end else if (aAnimationIndex>=0) and (aAnimationIndex<length(GLTF.Animations)) then begin

  result.Name:=GLTF.Animations[aAnimationIndex].Name;

  KeyTimes:=GLTF.GetAnimationTimes(aAnimationIndex);
  if length(KeyTimes)>0 then begin

   try

    CountFrames:=length(KeyTimes);

    result.StartTime:=KeyTimes[0];
    result.EndTime:=KeyTimes[length(KeyTimes)-1];

    SetLength(result.Frames,CountFrames);

    for FrameIndex:=0 to CountFrames-1 do begin

     GLTFInstance.Animation:=aAnimationIndex;

     GLTFInstance.AnimationTime:=KeyTimes[FrameIndex];

     GLTFInstance.Update;

     result.Frames[FrameIndex].Time:=GLTFInstance.AnimationTime;

     GLTFBakedVertexIndexedMesh:=GLTFInstance.GetBakedVertexIndexedMesh(false,true,-1,[TPasGLTF.TMaterial.TAlphaMode.Opaque,TPasGLTF.TMaterial.TAlphaMode.Blend,TPasGLTF.TMaterial.TAlphaMode.Mask]);
     if assigned(GLTFBakedVertexIndexedMesh) then begin

      try

       if length(result.Frames[FrameIndex].Vertices)<>GLTFBakedVertexIndexedMesh.Vertices.Count then begin
        SetLength(result.Frames[FrameIndex].Vertices,GLTFBakedVertexIndexedMesh.Vertices.Count);
       end;

       if length(result.Frames[FrameIndex].FullVertices)<>GLTFBakedVertexIndexedMesh.Vertices.Count then begin
        SetLength(result.Frames[FrameIndex].FullVertices,GLTFBakedVertexIndexedMesh.Vertices.Count);
       end;

       for VertexIndex:=0 to GLTFBakedVertexIndexedMesh.Vertices.Count-1 do begin

        GLTFBakedVertexIndexedMeshVertex:=@GLTFBakedVertexIndexedMesh.Vertices.ItemArray[VertexIndex];

        Vertex:=@result.Frames[FrameIndex].FullVertices[VertexIndex];

        Vertex^.Position:=TpvVector3.Create(GLTFBakedVertexIndexedMeshVertex^.Position[0],GLTFBakedVertexIndexedMeshVertex^.Position[1],GLTFBakedVertexIndexedMeshVertex^.Position[2]);

{       Vertex^.TexCoordU:=Min(Max(round(GLTFBakedVertexIndexedMeshVertex^.TexCoord0[0]*16384.0),0),65535);
        Vertex^.TexCoordV:=Min(Max(round(GLTFBakedVertexIndexedMeshVertex^.TexCoord0[1]*16384.0),0),65535);}

        Vertex^.Tangent:=TpvVector3.Create(GLTFBakedVertexIndexedMeshVertex^.Tangent[0],GLTFBakedVertexIndexedMeshVertex^.Tangent[1],GLTFBakedVertexIndexedMeshVertex^.Tangent[2]);
        Vertex^.Bitangent:=(TpvVector3.Create(GLTFBakedVertexIndexedMeshVertex^.Normal[0],GLTFBakedVertexIndexedMeshVertex^.Normal[1],GLTFBakedVertexIndexedMeshVertex^.Normal[2]).Cross(TpvVector3.Create(GLTFBakedVertexIndexedMeshVertex^.Tangent[0],GLTFBakedVertexIndexedMeshVertex^.Tangent[1],GLTFBakedVertexIndexedMeshVertex^.Tangent[2])))*GLTFBakedVertexIndexedMeshVertex^.Tangent[3];
        Vertex^.Normal:=TpvVector3.Create(GLTFBakedVertexIndexedMeshVertex^.Normal[0],GLTFBakedVertexIndexedMeshVertex^.Normal[1],GLTFBakedVertexIndexedMeshVertex^.Normal[2]);

       end;

      finally
       FreeAndNil(GLTFBakedVertexIndexedMesh);
      end;
     end;

    end;

   finally
    KeyTimes:=nil;
   end;

  end;

 end;    

end;

function ConvertModel(const aInputFileName,aOutputFileName:String):boolean;
var Index,FrameIndex,OtherIndex,FoundPresetAnimation,VertexIndex,
    BaseColorTextureIndex,NormalTextureIndex,MetallicRoughnessTextureIndex,
    OcclusionTextureIndex,EmissiveTextureIndex,
    ImageIndex:TpvSizeInt;
    CountFrames,ui32:TpvUInt32;
    GLTFBakedVertexIndexedMesh:TpvGLTF.TBakedVertexIndexedMesh;
    ta,tb,t:TpvDouble;
    AnimationName:TpvUTF8String;
    Frames:TpvSAM.TFrames;
    Material:TpvGLTF.PMaterial;
    Stream:TMemoryStream;
    BaseColorFactor:TpvVector4;
    MetallicRoughnessFactor:TpvVector2;
    NormalScale,OcclusionStrength:TpvFloat;
    EmissiveFactor:TpvVector3;
    First:boolean;
    GLTFBakedVertexIndexedMeshVertex:TpvGLTF.PVertex;
begin

 result:=true;

{$if declared(SetExceptionMask)}
 SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
{$ifend}

 FillChar(Animations,SizeOf(TpvSAM.TAnimations),#0);

 GLTF:=TpvGLTF.Create;
 try

  GLTF.LoadFromFile(aInputFileName);

  GLTF.Upload;

  GLTFInstance:=GLTF.AcquireInstance;
  try

   GLTFInstance.Upload;

   if length(GLTF.Animations)>0 then begin

    // Get indices for all animations once for all in advance
    begin

     GLTFInstance.Animation:=-1;
     GLTFInstance.AnimationTime:=0;
     GLTFInstance.Update;

     GLTFBakedVertexIndexedMesh:=GLTFInstance.GetBakedVertexIndexedMesh(false,true,-1,[TPasGLTF.TMaterial.TAlphaMode.Opaque,TPasGLTF.TMaterial.TAlphaMode.Blend,TPasGLTF.TMaterial.TAlphaMode.Mask]);
     if assigned(GLTFBakedVertexIndexedMesh) then begin

      try

       CountUsedVertices:=GLTFBakedVertexIndexedMesh.Vertices.Count;
       SetLength(TexCoords,CountUsedVertices);
       for VertexIndex:=0 to GLTFBakedVertexIndexedMesh.Vertices.Count-1 do begin
        GLTFBakedVertexIndexedMeshVertex:=@GLTFBakedVertexIndexedMesh.Vertices.ItemArray[VertexIndex];
        TexCoords[VertexIndex].x:=Min(Max(round(GLTFBakedVertexIndexedMeshVertex^.TexCoord0[0]*16384.0),0),65535);
        TexCoords[VertexIndex].y:=Min(Max(round(GLTFBakedVertexIndexedMeshVertex^.TexCoord0[1]*16384.0),0),65535);
       end;

       CountUsedIndices:=GLTFBakedVertexIndexedMesh.Indices.Count;
       SetLength(Indices,GLTFBakedVertexIndexedMesh.Indices.Count);
       for Index:=0 to GLTFBakedVertexIndexedMesh.Indices.Count-1 do begin
        Indices[Index]:=GLTFBakedVertexIndexedMesh.Indices.ItemArray[Index];
       end;

      finally
       FreeAndNil(GLTFBakedVertexIndexedMesh);
      end;

     end else begin
      WriteLn('Error: No vertex indexed mesh found!');
      result:=false;
     end;

    end;

    if result then begin

     // Get PBR textures
     BaseColorTextureIndex:=-1;
     NormalTextureIndex:=-1;
     MetallicRoughnessTextureIndex:=-1;
     OcclusionTextureIndex:=-1;
     EmissiveTextureIndex:=-1;
     BaseColorFactor:=TpvVector4.Create(1.0,1.0,1.0,1.0);
     MetallicRoughnessFactor:=TpvVector2.Create(1.0,1.0);
     NormalScale:=1.0;
     OcclusionStrength:=1.0;
     EmissiveFactor:=TpvVector3.Create(1.0,1.0,1.0);
     if length(GLTF.Materials)>0 then begin
      Material:=@GLTF.Materials[0];
      if Material^.ShadingModel=TpvGLTF.TMaterial.TShadingModel.PBRMetallicRoughness then begin
       BaseColorFactor:=TpvVector4(Pointer(@Material^.PBRMetallicRoughness.BaseColorFactor)^);
       BaseColorTextureIndex:=Material^.PBRMetallicRoughness.BaseColorTexture.Index;
       NormalTextureIndex:=Material^.NormalTexture.Index;
       MetallicRoughnessFactor:=TpvVector2.Create(Material^.PBRMetallicRoughness.MetallicFactor,Material^.PBRMetallicRoughness.RoughnessFactor);
       MetallicRoughnessTextureIndex:=Material^.PBRMetallicRoughness.MetallicRoughnessTexture.Index;
       OcclusionTextureIndex:=Material^.OcclusionTexture.Index;
       NormalScale:=Material^.NormalTextureScale;
       OcclusionStrength:=Material^.OcclusionTextureStrength;
       EmissiveTextureIndex:=Material^.EmissiveTexture.Index;
       EmissiveFactor:=TpvVector3(Pointer(@Material^.EmissiveFactor)^);
       if BaseColorTextureIndex>=0 then begin
        ImageIndex:=GLTF.Textures[BaseColorTextureIndex].Image;
        if ImageIndex>=0 then begin
         BaseColorTextureData:=GLTF.Images[ImageIndex].Data;
        end;
       end;
       if NormalTextureIndex>=0 then begin
        ImageIndex:=GLTF.Textures[NormalTextureIndex].Image;
        if ImageIndex>=0 then begin
         NormalTextureData:=GLTF.Images[ImageIndex].Data;
        end;
       end;
       if MetallicRoughnessTextureIndex>=0 then begin
        ImageIndex:=GLTF.Textures[MetallicRoughnessTextureIndex].Image;
        if ImageIndex>=0 then begin
         MetallicRoughnessTextureData:=GLTF.Images[ImageIndex].Data;
        end;
       end;
       if OcclusionTextureIndex>=0 then begin
        ImageIndex:=GLTF.Textures[OcclusionTextureIndex].Image;
        if ImageIndex>=0 then begin
         OcclusionTextureData:=GLTF.Images[ImageIndex].Data;
        end;
       end;
       if EmissiveTextureIndex>=0 then begin
        ImageIndex:=GLTF.Textures[EmissiveTextureIndex].Image;
        if ImageIndex>=0 then begin
         EmissiveTextureData:=GLTF.Images[ImageIndex].Data;
        end;
       end;
      end else begin
       WriteLn('Error: No PBR metallic roughness material found!');
       result:=false;
      end;
     end else begin
      WriteLn('Error: No material found!');
      result:=false;
     end;

    end;

    if result then begin

     // Get all animations
     SetLength(Animations,length(GLTF.Animations)+1);
     Animations[0]:=GetAnimation(-1);
     for Index:=0 to length(Animations)-1 do begin
      Animations[Index+1]:=GetAnimation(Index);
      if length(Animations[Index+1].Frames)>40 then begin
       Frames:=Animations[Index+1].Frames;
       Animations[Index+1].Frames:=nil;
       OptimizeFrames(Frames,Animations[Index+1].Frames,40);
      end;
     end;

     // Get bounding box
     First:=true;
     for Index:=0 to length(Animations)-1 do begin
      for FrameIndex:=0 to length(Animations[Index].Frames)-1 do begin
       for VertexIndex:=0 to CountUsedVertices-1 do begin
        if First then begin
         First:=false;
         BoundingBox.Min:=Animations[Index].Frames[FrameIndex].FullVertices[VertexIndex].Position;
         BoundingBox.Max:=Animations[Index].Frames[FrameIndex].FullVertices[VertexIndex].Position;
        end else begin
         BoundingBox.DirectCombineVector3(Animations[Index].Frames[FrameIndex].FullVertices[VertexIndex].Position);
        end;
       end;
      end;
     end;

     // Get bounding sphere
     BoundingSphere:=TpvSphere.CreateFromAABB(BoundingBox);

     // Save
     Stream:=TMemoryStream.Create;
     try

      FileHeader.Signature:=TpvSAM.Signature;
      FileHeader.Version:=TpvSAM.Version;
      FileHeader.CountVertices:=CountUsedVertices;
      FileHeader.CountIndices:=CountUsedIndices;
      FileHeader.CountAnimations:=length(Animations);

      FileHeader.BoundingBoxMin:=BoundingBox.Min;
      FileHeader.BoundingBoxMax:=BoundingBox.Max;
      FileHeader.BoundingSphere:=TpvVector4.InlineableCreate(BoundingSphere.Center,BoundingSphere.Radius);

      FileHeader.MaterialHeader.BaseColorFactor:=BaseColorFactor;
      FileHeader.MaterialHeader.EmissiveFactorOcclusionStrength:=TpvVector4.Create(EmissiveFactor.x,EmissiveFactor.y,EmissiveFactor.z,OcclusionStrength);
      FileHeader.MaterialHeader.MetallicRoughnessFactorNormalScale:=TpvVector4.Create(MetallicRoughnessFactor.x,MetallicRoughnessFactor.y,NormalScale,0.0);
      FileHeader.MaterialHeader.BaseColorTextureSize:=length(BaseColorTextureData);
      FileHeader.MaterialHeader.NormalTextureSize:=length(NormalTextureData);
      FileHeader.MaterialHeader.MetallicRoughnessTextureSize:=length(MetallicRoughnessTextureData);
      FileHeader.MaterialHeader.OcclusionTextureSize:=length(OcclusionTextureData);
      FileHeader.MaterialHeader.EmissiveTextureSize:=length(EmissiveTextureData);

      // Write file header
      Stream.WriteBuffer(FileHeader,SizeOf(TpvSAM.TFileHeader));

      // Write indices globally
      Stream.WriteBuffer(Indices[0],SizeOf(TpvSAM.TIndex)*CountUsedIndices);

      // Write texcoords globally
      Stream.WriteBuffer(TexCoords[0],SizeOf(TpvUInt16Vector2)*CountUsedVertices);

      // Write vertices from all animations
      for Index:=0 to length(Animations)-1 do begin
       ui32:=length(Animations[Index].Name);
       Stream.WriteBuffer(ui32,SizeOf(TpvUInt32));
       if ui32>0 then begin
        Stream.WriteBuffer(Animations[Index].Name[1],ui32);
       end;
       Stream.WriteBuffer(Animations[Index].StartTime,SizeOf(TpvDouble));
       Stream.WriteBuffer(Animations[Index].EndTime,SizeOf(TpvDouble));
       CountFrames:=length(Animations[Index].Frames);
       Stream.WriteBuffer(CountFrames,SizeOf(TpvUInt32));
       for FrameIndex:=0 to length(Animations[Index].Frames)-1 do begin
        Animations[Index].Frames[FrameIndex].Pack;
        Stream.WriteBuffer(Animations[Index].Frames[FrameIndex].Time,SizeOf(TpvDouble));
        if CountUsedVertices>0 then begin
         Stream.WriteBuffer(Animations[Index].Frames[FrameIndex].Vertices[0],SizeOf(TpvSAM.TVertex)*CountUsedVertices);
        end;
       end;
      end;
      
      // Write textures
      if length(BaseColorTextureData)>0 then begin
       Stream.WriteBuffer(BaseColorTextureData[0],length(BaseColorTextureData));
      end;
      if length(NormalTextureData)>0 then begin
       Stream.WriteBuffer(NormalTextureData[0],length(NormalTextureData));
      end;
      if length(MetallicRoughnessTextureData)>0 then begin
       Stream.WriteBuffer(MetallicRoughnessTextureData[0],length(MetallicRoughnessTextureData));
      end;
      if length(OcclusionTextureData)>0 then begin
       Stream.WriteBuffer(OcclusionTextureData[0],length(OcclusionTextureData));
      end;
      if length(EmissiveTextureData)>0 then begin
       Stream.WriteBuffer(EmissiveTextureData[0],length(EmissiveTextureData));
      end;

      // Save to file
      Stream.SaveToFile(aOutputFileName);

      // Done!

     finally
      FreeAndNil(Stream);
     end;

    end;
   
   end;

  finally
   FreeAndNil(GLTFInstance);
  end;

 finally
  FreeAndNil(GLTF);
 end;

end;

var Index:TpvSizeInt;
    Parameter,InputFileName,OutputFileName:String;
{   m0,m1:TpvMatrix3x3;
    u0:TpvUInt32;//}
begin

{m0:=TpvMatrix3x3.Create(TpvVector3.InlineableCreate(0.707,0.707,0.0).Normalize,
                         TpvVector3.InlineableCreate(0.707,-0.707,0.0).Normalize,
                         TpvVector3.InlineableCreate(0.0,0.0,1.0).Normalize);

 u0:=EncodeTangentSpaceAsRGB10A2SNorm(m0);
 
 DecodeTangentSpaceFromRGB10A2SNorm(u0,m1);

 WriteLn('m0 Tangent: ',m0.Tangent.x:7:4,' ',m0.Tangent.y:7:4,' ',m0.Tangent.z:7:4);
 WriteLn('m0 Bitangent: ',m0.Bitangent.x:7:4,' ',m0.Bitangent.y:7:4,' ',m0.Bitangent.z:7:4);
 WriteLn('m0 Normal: ',m0.Normal.x:7:4,' ',m0.Normal.y:7:4,' ',m0.Normal.z:7:4);
 WriteLn;
 WriteLn('u0: ',u0);
 WriteLn;
 WriteLn('m1 Tangent: ',m1.Tangent.x:7:4,' ',m1.Tangent.y:7:4,' ',m1.Tangent.z:7:4);
 WriteLn('m1 Bitangent: ',m1.Bitangent.x:7:4,' ',m1.Bitangent.y:7:4,' ',m1.Bitangent.z:7:4);
 WriteLn('m1 Normal: ',m1.Normal.x:7:4,' ',m1.Normal.y:7:4,' ',m1.Normal.z:7:4);

 exit;//}

 if ParamCount=0 then begin
  writeln('Usage: ',ExtractFileName(ParamStr(0)),' <input file name> <output file name>');
  halt(1);
 end;

 InputFileName:='';
 OutputFileName:='';

 for Index:=1 to ParamCount do begin
  Parameter:=ParamStr(Index);
  if length(Parameter)>0 then begin
   case Parameter[1] of
    '-':begin
    end;
    '/':begin
    end;
    else begin
     if length(InputFileName)=0 then begin
      InputFileName:=Parameter;
     end else if length(OutputFileName)=0 then begin
      OutputFileName:=Parameter;
     end;
    end;
   end;
  end;
 end;

 if (length(InputFileName)=0) or not FileExists(InputFileName) then begin
  writeln('Error: No input file name specified or input file name not found!');
  halt(1);
 end;

 if length(OutputFileName)=0 then begin
  writeln('Error: No output file name specified!');
  halt(1);
 end;

 BaseColorTextureData:=nil;
 NormalTextureData:=nil;
 MetallicRoughnessTextureData:=nil;
 OcclusionTextureData:=nil;
 EmissiveTextureData:=nil;
 
 ConvertModel(InputFileName,OutputFileName);

 WriteLn('Done!');

 BaseColorTextureData:=nil;
 NormalTextureData:=nil;
 MetallicRoughnessTextureData:=nil;
 OcclusionTextureData:=nil;
 EmissiveTextureData:=nil;

end.


