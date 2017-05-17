unit UnitModel;
{$ifdef fpc}
 {$mode delphi}
{$endif}

interface

uses SysUtils,Classes,Vulkan,Kraft,UnitMath3D;

type PModelQTangent=^TModelQTangent;
     TModelQTangent=packed record
      x,y,z,w:TVkUInt16;
     end;

     PModelVertex=^TModelVertex;
     TModelVertex=packed record
      Position:TVector3;
      QTangent:TModelQTangent;
      TexCoord:TVector2;
      Material:TVkInt32;
{     // In future maybe also:
      BlendIndices:array[0..7] of TVkUInt16;
      BlendWeight:array[0..7] of TVkUInt16;
}
     end;

     TModelVertices=array of TModelVertex;

     PModelIndex=^TModelIndex;
     TModelIndex=TVkUInt32;

     TModelIndices=array of TModelIndex;

     PModelMaterial=^TModelMaterial;
     TModelMaterial=record
      Name:ansistring;
      Texture:ansistring;
      Ambient:TVector3;
      Diffuse:TVector3;
      Emission:TVector3;
      Specular:TVector3;
      Shininess:single;
     end;

     TModelMaterials=array of TModelMaterial;

     PModelPart=^TModelPart;
     TModelPart=record
      Material:TVkInt32;
      StartIndex:TVkInt32;
      CountIndices:TVkInt32;
     end;

     TModelParts=array of TModelPart;

     PModelObject=^TModelObject;
     TModelObject=record
      Name:ansistring;
      Sphere:TSphere;
      AABB:TAABB;
     end;

     TModelObjects=array of TModelObject;

     EModelLoad=class(Exception);

     TModel=class
      private
       fUploaded:boolean;
       fSphere:TSphere;
       fAABB:TAABB;
       fMaterials:TModelMaterials;
       fCountMaterials:TVkInt32;
       fVertices:TModelVertices;
       fCountVertices:TVkInt32;
       fIndices:TModelIndices;
       fCountIndices:TVkInt32;
       fParts:TModelParts;
       fCountParts:TVkInt32;
       fObjects:TModelObjects;
       fCountObjects:TVkInt32;
       fKraftMesh:TKraftMesh;
       fKraftConvexHull:TKraftConvexHull;
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
       procedure Clear;
       procedure LoadFromStream(const pStream:TStream;const pDoFree:boolean=false);
       procedure Upload;
       procedure Unload;
       property Uploaded:boolean read fUploaded;
       property Sphere:TSphere read fSphere;
       property AABB:TAABB read fAABB;
       property Materials:TModelMaterials read fMaterials;
       property CountMaterials:TVkInt32 read fCountMaterials;
       property Vertices:TModelVertices read fVertices;
       property CountVertices:TVkInt32 read fCountVertices;
       property Indices:TModelIndices read fIndices;
       property CountIndices:TVkInt32 read fCountIndices;
       property Parts:TModelParts read fParts;
       property CountParts:TVkInt32 read fCountParts;
       property Objects:TModelObjects read fObjects;
       property CountObjects:TVkInt32 read fCountObjects;
       property KraftMesh:TKraftMesh read fKraftMesh write fKraftMesh;
       property KraftConvexHull:TKraftConvexHull read fKraftConvexHull write fKraftConvexHull;
     end;

implementation

uses UnitBufferedStream,UnitChunkStream;

function Matrix3x3FromQTangent(pQTangent:TQuaternion):TMatrix3x3;
var qx2,qy2,qz2,qxqx2,qxqy2,qxqz2,qxqw2,qyqy2,qyqz2,qyqw2,qzqz2,qzqw2:single;
begin
 QuaternionNormalize(pQTangent);
 qx2:=pQTangent.x+pQTangent.x;
 qy2:=pQTangent.y+pQTangent.y;
 qz2:=pQTangent.z+pQTangent.z;
 qxqx2:=pQTangent.x*qx2;
 qxqy2:=pQTangent.x*qy2;
 qxqz2:=pQTangent.x*qz2;
 qxqw2:=pQTangent.w*qx2;
 qyqy2:=pQTangent.y*qy2;
 qyqz2:=pQTangent.y*qz2;
 qyqw2:=pQTangent.w*qy2;
 qzqz2:=pQTangent.z*qz2;
 qzqw2:=pQTangent.w*qz2;
 result[0,0]:=1.0-(qyqy2+qzqz2);
 result[0,1]:=qxqy2+qzqw2;
 result[0,2]:=qxqz2-qyqw2;
 result[1,0]:=qxqy2-qzqw2;
 result[1,1]:=1.0-(qxqx2+qzqz2);
 result[1,2]:=qyqz2+qxqw2;
 result[2,0]:=(result[0,1]*result[1,2])-(result[0,2]*result[1,1]);
 result[2,1]:=(result[0,2]*result[1,0])-(result[0,0]*result[1,2]);
 result[2,2]:=(result[0,0]*result[1,1])-(result[0,1]*result[1,0]);
{result[2,0]:=qxqz2+qyqw2;
 result[2,1]:=qyqz2-qxqw2;
 result[2,2]:=1.0-(qxqx2+qyqy2);}
 if pQTangent.w<0.0 then begin
  result[2,0]:=-result[2,0];
  result[2,1]:=-result[2,1];
  result[2,2]:=-result[2,2];
 end;
end;

constructor TModel.Create;
begin
 inherited Create;
 fUploaded:=false;
 fKraftMesh:=nil;
 fKraftConvexHull:=nil;
 Clear;
end;

destructor TModel.Destroy;
begin
 Unload;
 Clear;
 inherited Destroy;
end;

procedure TModel.Clear;
begin
 fMaterials:=nil;
 fCountMaterials:=0;
 fVertices:=nil;
 fCountVertices:=0;
 fIndices:=nil;
 fCountIndices:=0;
 fParts:=nil;
 fCountParts:=0;
 fObjects:=nil;
 fCountObjects:=0;
end;

procedure TModel.LoadFromStream(const pStream:TStream;const pDoFree:boolean=false);
const ModelSignature:TChunkSignature=('m','d','l',#0);
      ModelVersion:TVkUInt32=0;
var Signature:TChunkSignature;
    Version:TVkUInt32;
    ChunkOffset,CountChunks:TVkInt32;
    Chunks:TChunks;
 function GetChunkStream(const ChunkSignature:TChunkSignature;const WithMemoryCopy:boolean=true):TChunkStream;
 var Index:TVkInt32;
     Chunk:PChunk;
 begin
  result:=nil;
  for Index:=0 to CountChunks-1 do begin
   Chunk:=@Chunks[Index];
   if Chunk^.Signature=ChunkSignature then begin
    result:=TChunkStream.Create(pStream,Chunk^.Offset,Chunk^.Size,WithMemoryCopy);
    exit;
   end;
  end;
 end;
 procedure ReadBOVO;
 const ChunkSignature:TChunkSignature=('B','O','V','O');
 var ChunkStream:TChunkStream;
 begin
  ChunkStream:=GetChunkStream(ChunkSignature,false);
  try
   if assigned(ChunkStream) and (ChunkStream.Size<>0) then begin
    fSphere.Center:=ChunkStream.ReadVector3;
    fSphere.Radius:=ChunkStream.ReadFloat;
    fAABB.Min:=ChunkStream.ReadVector3;
    fAABB.Max:=ChunkStream.ReadVector3;
   end else begin
    raise EModelLoad.Create('Missing "'+ChunkSignature[0]+ChunkSignature[1]+ChunkSignature[2]+ChunkSignature[3]+'" chunk');
   end;
  finally
   ChunkStream.Free;
  end;
 end;
 procedure ReadMATE;
 const ChunkSignature:TChunkSignature=('M','A','T','E');
 var ChunkStream:TChunkStream;
     Index:TVkInt32;
     Material:PModelMaterial;
 begin
  ChunkStream:=GetChunkStream(ChunkSignature,true);
  try
   if assigned(ChunkStream) and (ChunkStream.Size<>0) then begin
    fCountMaterials:=ChunkStream.ReadInteger;
    SetLength(fMaterials,fCountMaterials);
    for Index:=0 to fCountMaterials-1 do begin
     Material:=@fMaterials[Index];
     Material^.Name:=ChunkStream.ReadString;
     Material^.Texture:=ChunkStream.ReadString;
     Material^.Ambient:=ChunkStream.ReadVector3;
     Material^.Diffuse:=ChunkStream.ReadVector3;
     Material^.Emission:=ChunkStream.ReadVector3;
     Material^.Specular:=ChunkStream.ReadVector3;
     Material^.Shininess:=ChunkStream.ReadFloat;
    end;
   end else begin
    raise EModelLoad.Create('Missing "'+ChunkSignature[0]+ChunkSignature[1]+ChunkSignature[2]+ChunkSignature[3]+'" chunk');
   end;
  finally
   ChunkStream.Free;
  end;
 end;
 procedure ReadVBOS;
 const ChunkSignature:TChunkSignature=('V','B','O','S');
 var ChunkStream:TChunkStream;
     VertexIndex:TVkInt32;
     q:TQuaternion;
     m:TMatrix3x3;
 begin
  ChunkStream:=GetChunkStream(ChunkSignature,true);
  try
   if assigned(ChunkStream) and (ChunkStream.Size<>0) then begin
    fCountVertices:=ChunkStream.ReadInteger;
    SetLength(fVertices,fCountVertices);
    if fCountVertices>0 then begin
     ChunkStream.ReadWithCheck(fVertices[0],fCountVertices*SizeOf(TModelVertex));
     if assigned(fKraftMesh) then begin
      for VertexIndex:=0 to fCountVertices-1 do begin
       fKraftMesh.AddVertex(Kraft.Vector3(fVertices[VertexIndex].Position.x,fVertices[VertexIndex].Position.y,fVertices[VertexIndex].Position.z));
       q.x:=(Vertices[VertexIndex].QTangent.x-32767)/32768.0;
       q.y:=(Vertices[VertexIndex].QTangent.y-32767)/32768.0;
       q.z:=(Vertices[VertexIndex].QTangent.z-32767)/32768.0;
       q.w:=(Vertices[VertexIndex].QTangent.w-32767)/32768.0;
       m:=Matrix3x3FromQTangent(q);
       fKraftMesh.AddNormal(Kraft.Vector3(m[2,0],m[2,1],m[2,2]));
      end;
     end;
    end;
   end else begin
    raise EModelLoad.Create('Missing "'+ChunkSignature[0]+ChunkSignature[1]+ChunkSignature[2]+ChunkSignature[3]+'" chunk');
   end;
  finally
   ChunkStream.Free;
  end;
 end;
 procedure ReadIBOS;
 const ChunkSignature:TChunkSignature=('I','B','O','S');
 var ChunkStream:TChunkStream;
     Index:TVkInt32;
 begin
  ChunkStream:=GetChunkStream(ChunkSignature,true);
  try
   if assigned(ChunkStream) and (ChunkStream.Size<>0) then begin
    fCountIndices:=ChunkStream.ReadInteger;
    SetLength(fIndices,fCountIndices);
    if fCountIndices>0 then begin
     ChunkStream.ReadWithCheck(fIndices[0],fCountIndices*SizeOf(TModelIndex));
     if assigned(fKraftMesh) then begin
      Index:=0;
      while (Index+2)<fCountIndices-1 do begin
       fKraftMesh.AddTriangle(fIndices[Index],fIndices[Index+1],fIndices[Index+2],
                              fIndices[Index],fIndices[Index+1],fIndices[Index+2]);
       inc(Index,3);
      end;
      fKraftMesh.Finish;
     end;
    end;
   end else begin
    raise EModelLoad.Create('Missing "'+ChunkSignature[0]+ChunkSignature[1]+ChunkSignature[2]+ChunkSignature[3]+'" chunk');
   end;
  finally
   ChunkStream.Free;
  end;
 end;
 procedure ReadPART;
 const ChunkSignature:TChunkSignature=('P','A','R','T');
 var ChunkStream:TChunkStream;
     Index:TVkInt32;
     Part:PModelPart;
 begin
  ChunkStream:=GetChunkStream(ChunkSignature,true);
  try
   if assigned(ChunkStream) and (ChunkStream.Size<>0) then begin
    fCountParts:=ChunkStream.ReadInteger;
    SetLength(fParts,fCountParts);
    for Index:=0 to fCountParts-1 do begin
     Part:=@fParts[Index];
     Part^.Material:=ChunkStream.ReadInteger;
     Part^.StartIndex:=ChunkStream.ReadInteger;
     Part^.CountIndices:=ChunkStream.ReadInteger;
    end;
   end else begin
    raise EModelLoad.Create('Missing "'+ChunkSignature[0]+ChunkSignature[1]+ChunkSignature[2]+ChunkSignature[3]+'" chunk');
   end;
  finally
   ChunkStream.Free;
  end;
 end;
 procedure ReadOBJS;
 const ChunkSignature:TChunkSignature=('O','B','J','S');
 var ChunkStream:TChunkStream;
     ObjectIndex:TVkInt32;
     AObject:PModelObject;
 begin
  ChunkStream:=GetChunkStream(ChunkSignature,true);
  try
   if assigned(ChunkStream) and (ChunkStream.Size<>0) then begin
    fCountObjects:=ChunkStream.ReadInteger;
    SetLength(fObjects,fCountObjects);
    for ObjectIndex:=0 to fCountObjects-1 do begin
     AObject:=@fObjects[ObjectIndex];
     AObject^.Name:=ChunkStream.ReadString;
     AObject^.Sphere.Center:=ChunkStream.ReadVector3;
     AObject^.Sphere.Radius:=ChunkStream.ReadFloat;
     AObject^.AABB.Min:=ChunkStream.ReadVector3;
     AObject^.AABB.Max:=ChunkStream.ReadVector3;
    end;
   end else begin
    raise EModelLoad.Create('Missing "'+ChunkSignature[0]+ChunkSignature[1]+ChunkSignature[2]+ChunkSignature[3]+'" chunk');
   end;
  finally
   ChunkStream.Free;
  end;
 end;
 procedure ReadCOHU;
 const ChunkSignature:TChunkSignature=('C','O','H','U');
 var ChunkStream:TChunkStream;
 begin
  ChunkStream:=GetChunkStream(ChunkSignature,true);
  try
   if assigned(ChunkStream) and (ChunkStream.Size<>0) then begin
    if assigned(fKraftConvexHull) then begin
     fKraftConvexHull.LoadFromStream(ChunkStream);
    end;
   end else begin
    raise EModelLoad.Create('Missing "'+ChunkSignature[0]+ChunkSignature[1]+ChunkSignature[2]+ChunkSignature[3]+'" chunk');
   end;
  finally
   ChunkStream.Free;
  end;
 end;
begin
 if assigned(pStream) then begin
  try
   Chunks:=nil;
   try
    begin
     if pStream.Seek(0,soBeginning)<>0 then begin
      raise EModelLoad.Create('Stream seek error');
     end;
    end;
    begin
     if pStream.Read(Signature,SizeOf(TChunkSignature))<>SizeOf(TChunkSignature) then begin
      raise EModelLoad.Create('Stream read error');
     end;
     if Signature<>ModelSignature then begin
      raise EModelLoad.Create('Invalid model file signature');
     end;
    end;
    begin
     if pStream.Read(Version,SizeOf(TVkUInt32))<>SizeOf(TVkUInt32) then begin
      raise EModelLoad.Create('Stream read error');
     end;
     if Version<>ModelVersion then begin
      raise EModelLoad.Create('Invalid model file version');
     end;
    end;
    begin
     if pStream.Read(CountChunks,SizeOf(TVkInt32))<>SizeOf(TVkInt32) then begin
      raise EModelLoad.Create('Stream read error');
     end;
     if pStream.Read(ChunkOffset,SizeOf(TVkInt32))<>SizeOf(TVkInt32) then begin
      raise EModelLoad.Create('Stream read error');
     end;
     if pStream.Seek(ChunkOffset,soBeginning)<>ChunkOffset then begin
      raise EModelLoad.Create('Stream seek error');
     end;
     if CountChunks=0 then begin
      raise EModelLoad.Create('Model file without chunks');
     end;
     SetLength(Chunks,CountChunks);
     if pStream.Read(Chunks[0],CountChunks*SizeOf(TChunk))<>(CountChunks*SizeOf(TChunk)) then begin
      raise EModelLoad.Create('Stream read error');
     end;
    end;
    begin
     ReadBOVO;
     ReadMATE;
     ReadVBOS;
     ReadIBOS;
     ReadPART;
     ReadOBJS;
     ReadCOHU;
    end;
   finally
    SetLength(Chunks,0);
   end;
  finally
   if pDoFree then begin
    pStream.Free;
   end;
  end;
 end;
end;

procedure TModel.Upload;
begin
 if not fUploaded then begin
  fUploaded:=true;
 end;
end;

procedure TModel.Unload;
begin
 if fUploaded then begin
  fUploaded:=false;
 end;
end;

end.
