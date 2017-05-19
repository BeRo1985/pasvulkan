unit UnitModel;
{$ifdef fpc}
 {$mode delphi}
{$endif}

interface

uses SysUtils,Classes,Math,Vulkan,Kraft,UnitMath3D,PasVulkan;

const VULKAN_MODEL_VERTEX_BUFFER_BIND_ID=0;

type PModelQTangent=^TModelQTangent;
     TModelQTangent=packed record
      x,y,z,w:TVkInt16;
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

     TModelBuffers=array of TVulkanBuffer;

     PModelBufferSize=^TModelBufferSize;
     TModelBufferSize=TVkUInt32;

     TModelBufferSizes=array of TModelBufferSize;

     TModel=class
      private
       fVulkanDevice:TVulkanDevice;
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
       fVertexBuffers:TModelBuffers;
       fIndexBuffers:TModelBuffers;
       fBufferSizes:TModelBufferSizes;
       fCountBuffers:TVkInt32;
      public
       constructor Create(const pVulkanDevice:TVulkanDevice); reintroduce;
       destructor Destroy; override;
       procedure Clear;
       procedure MakeCube(const pSizeX,pSizeY,pSizeZ:single);
       procedure LoadFromStream(const pStream:TStream;const pDoFree:boolean=false);
       procedure Upload(const pQueue:TVulkanQueue;
                        const pCommandBuffer:TVulkanCommandBuffer;
                        const pFence:TVulkanFence);
       procedure Unload;
       procedure Draw(const pCommandBuffer:TVulkanCommandBuffer;const pInstanceCount:TVkUInt32=1;const pFirstInstance:TVkUInt32=0);
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

procedure RobustOrthoNormalize(var Tangent,Bitangent,Normal:TVector3;const Tolerance:single=1e-3);
var Bisector,Axis:TVector3;
begin
 begin
  if Vector3Length(Normal)<Tolerance then begin
   // Degenerate case, compute new normal
   Normal:=Vector3Cross(Tangent,Bitangent);
   if Vector3Length(Normal)<Tolerance then begin
    Tangent:=Vector3XAxis;
    Bitangent:=Vector3YAxis;
    Normal:=Vector3ZAxis;
    exit;
   end;
  end;
  Normal:=Vector3Norm(Normal);
 end;
 begin
  // Project tangent and bitangent onto the normal orthogonal plane
  Tangent:=Vector3Sub(Tangent,Vector3ScalarMul(Normal,Vector3Dot(Tangent,Normal)));
  Bitangent:=Vector3Sub(Bitangent,Vector3ScalarMul(Normal,Vector3Dot(Bitangent,Normal)));
 end;
 begin
  // Check for several degenerate cases
  if Vector3Length(Tangent)<Tolerance then begin
   if Vector3Length(Bitangent)<Tolerance then begin
    Tangent:=Vector3Norm(Normal);
    if (Tangent.x<=Tangent.y) and (Tangent.x<=Tangent.z) then begin
     Tangent:=Vector3XAxis;
    end else if (Tangent.y<=Tangent.x) and (Tangent.y<=Tangent.z) then begin
     Tangent:=Vector3YAxis;
    end else begin
     Tangent:=Vector3ZAxis;
    end;
    Tangent:=Vector3Sub(Tangent,Vector3ScalarMul(Normal,Vector3Dot(Tangent,Normal)));
    Bitangent:=Vector3Norm(Vector3Cross(Normal,Tangent));
   end else begin
    Tangent:=Vector3Norm(Vector3Cross(Bitangent,Normal));
   end;
  end else begin
   Tangent:=Vector3Norm(Tangent);
   if Vector3Length(Bitangent)<Tolerance then begin
    Bitangent:=Vector3Norm(Vector3Cross(Normal,Tangent));
   end else begin
    Bitangent:=Vector3Norm(Bitangent);
    Bisector:=Vector3Add(Tangent,Bitangent);
    if Vector3Length(Bisector)<Tolerance then begin
     Bisector:=Tangent;
    end else begin
     Bisector:=Vector3Norm(Bisector);
    end;
    Axis:=Vector3Norm(Vector3Cross(Bisector,Normal));
    if Vector3Dot(Axis,Tangent)>0.0 then begin
     Tangent:=Vector3Norm(Vector3Add(Bisector,Axis));
     Bitangent:=Vector3Norm(Vector3Sub(Bisector,Axis));
    end else begin
     Tangent:=Vector3Norm(Vector3Sub(Bisector,Axis));
     Bitangent:=Vector3Norm(Vector3Add(Bisector,Axis));
    end;
   end;
  end;
 end;
 Bitangent:=Vector3Norm(Vector3Cross(Normal,Tangent));
 Tangent:=Vector3Norm(Vector3Cross(Bitangent,Normal));
 Normal:=Vector3Norm(Vector3Cross(Tangent,Bitangent));
end;

function Matrix3x3ToQTangent(RawComponents:TMatrix3x3):TQuaternion;
const Threshold=1.0/32767.0;
var Scale,t,s,Renormalization:single;
begin
 RobustOrthoNormalize(PVector3(@RawComponents[0,0])^,
                      PVector3(@RawComponents[1,0])^,
                      PVector3(@RawComponents[2,0])^);
 if ((((((RawComponents[0,0]*RawComponents[1,1]*RawComponents[2,2])+
         (RawComponents[0,1]*RawComponents[1,2]*RawComponents[2,0])
        )+
        (RawComponents[0,2]*RawComponents[1,0]*RawComponents[2,1])
       )-
       (RawComponents[0,2]*RawComponents[1,1]*RawComponents[2,0])
      )-
      (RawComponents[0,1]*RawComponents[1,0]*RawComponents[2,2])
     )-
     (RawComponents[0,0]*RawComponents[1,2]*RawComponents[2,1])
    )<0.0 then begin
  // Reflection matrix, so flip y axis in case the tangent frame encodes a reflection
  Scale:=-1.0;
  RawComponents[2,0]:=-RawComponents[2,0];
  RawComponents[2,1]:=-RawComponents[2,1];
  RawComponents[2,2]:=-RawComponents[2,2];
 end else begin
  // Rotation matrix, so nothing is doing to do
  Scale:=1.0;
 end;
 begin
  // Convert to quaternion
  t:=RawComponents[0,0]+(RawComponents[1,1]+RawComponents[2,2]);
  if t>2.9999999 then begin
   result.x:=0.0;
   result.y:=0.0;
   result.z:=0.0;
   result.w:=1.0;
  end else if t>0.0000001 then begin
   s:=sqrt(1.0+t)*2.0;
   result.x:=(RawComponents[1,2]-RawComponents[2,1])/s;
   result.y:=(RawComponents[2,0]-RawComponents[0,2])/s;
   result.z:=(RawComponents[0,1]-RawComponents[1,0])/s;
   result.w:=s*0.25;
  end else if (RawComponents[0,0]>RawComponents[1,1]) and (RawComponents[0,0]>RawComponents[2,2]) then begin
   s:=sqrt(1.0+(RawComponents[0,0]-(RawComponents[1,1]+RawComponents[2,2])))*2.0;
   result.x:=s*0.25;
   result.y:=(RawComponents[1,0]+RawComponents[0,1])/s;
   result.z:=(RawComponents[2,0]+RawComponents[0,2])/s;
   result.w:=(RawComponents[1,2]-RawComponents[2,1])/s;
  end else if RawComponents[1,1]>RawComponents[2,2] then begin
   s:=sqrt(1.0+(RawComponents[1,1]-(RawComponents[0,0]+RawComponents[2,2])))*2.0;
   result.x:=(RawComponents[1,0]+RawComponents[0,1])/s;
   result.y:=s*0.25;
   result.z:=(RawComponents[2,1]+RawComponents[1,2])/s;
   result.w:=(RawComponents[2,0]-RawComponents[0,2])/s;
  end else begin
   s:=sqrt(1.0+(RawComponents[2,2]-(RawComponents[0,0]+RawComponents[1,1])))*2.0;
   result.x:=(RawComponents[2,0]+RawComponents[0,2])/s;
   result.y:=(RawComponents[2,1]+RawComponents[1,2])/s;
   result.z:=s*0.25;
   result.w:=(RawComponents[0,1]-RawComponents[1,0])/s;
  end;
  QuaternionNormalize(result);
 end;
 begin
  // Make sure, that we don't end up with 0 as w component
  if abs(result.w)<=Threshold then begin
   Renormalization:=sqrt(1.0-sqr(Threshold));
   result.x:=result.x*Renormalization;
   result.y:=result.y*Renormalization;
   result.z:=result.z*Renormalization;
   if result.w>0.0 then begin
    result.w:=Threshold;
   end else begin
    result.w:=-Threshold;
   end;
  end;
 end;
 if ((Scale<0.0) and (result.w>=0.0)) or ((Scale>=0.0) and (result.w<0.0)) then begin
  // Encode reflection into quaternion's w element by making sign of w negative,
  // if y axis needs to be flipped, otherwise it stays positive
  result.x:=-result.x;
  result.y:=-result.y;
  result.z:=-result.z;
  result.w:=-result.w;
 end;
end;

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

constructor TModel.Create(const pVulkanDevice:TVulkanDevice);
begin
 inherited Create;
 fVulkanDevice:=pVulkanDevice;
 fUploaded:=false;
 fKraftMesh:=nil;
 fKraftConvexHull:=nil;
 fVertexBuffers:=nil;
 fIndexBuffers:=nil;
 fBufferSizes:=nil;
 fCountBuffers:=0;
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

procedure TModel.MakeCube(const pSizeX,pSizeY,pSizeZ:single);
type PCubeVertex=^TCubeVertex;
     TCubeVertex=record
      Position:TVector3;
      Tangent:TVector3;
      Bitangent:TVector3;
      Normal:TVector3;
      TexCoord:TVector2;
     end;
const CubeVertices:array[0..23] of TCubeVertex=
       (// Left
        (Position:(x:-1;y:-1;z:-1;);Tangent:(x:0;y:0;z:1;);Bitangent:(x:0;y:1;z:0;);Normal:(x:-1;y:0;z:0;);TexCoord:(u:0;v:0)),
        (Position:(x:-1;y: 1;z:-1;);Tangent:(x:0;y:0;z:1;);Bitangent:(x:0;y:1;z:0;);Normal:(x:-1;y:0;z:0;);TexCoord:(u:0;v:1)),
        (Position:(x:-1;y: 1;z: 1;);Tangent:(x:0;y:0;z:1;);Bitangent:(x:0;y:1;z:0;);Normal:(x:-1;y:0;z:0;);TexCoord:(u:1;v:1)),
        (Position:(x:-1;y:-1;z: 1;);Tangent:(x:0;y:0;z:1;);Bitangent:(x:0;y:1;z:0;);Normal:(x:-1;y:0;z:0;);TexCoord:(u:1;v:0)),

        // Right
        (Position:(x: 1;y:-1;z: 1;);Tangent:(x:0;y:0;z:-1;);Bitangent:(x:0;y:1;z:0;);Normal:(x:1;y:0;z:0;);TexCoord:(u:0;v:0)),
        (Position:(x: 1;y: 1;z: 1;);Tangent:(x:0;y:0;z:-1;);Bitangent:(x:0;y:1;z:0;);Normal:(x:1;y:0;z:0;);TexCoord:(u:0;v:1)),
        (Position:(x: 1;y: 1;z:-1;);Tangent:(x:0;y:0;z:-1;);Bitangent:(x:0;y:1;z:0;);Normal:(x:1;y:0;z:0;);TexCoord:(u:1;v:1)),
        (Position:(x: 1;y:-1;z:-1;);Tangent:(x:0;y:0;z:-1;);Bitangent:(x:0;y:1;z:0;);Normal:(x:1;y:0;z:0;);TexCoord:(u:1;v:0)),

        // Bottom
        (Position:(x:-1;y:-1;z:-1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:0;z:1;);Normal:(x:0;y:-1;z:0;);TexCoord:(u:0;v:0)),
        (Position:(x:-1;y:-1;z: 1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:0;z:1;);Normal:(x:0;y:-1;z:0;);TexCoord:(u:0;v:1)),
        (Position:(x: 1;y:-1;z: 1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:0;z:1;);Normal:(x:0;y:-1;z:0;);TexCoord:(u:1;v:1)),
        (Position:(x: 1;y:-1;z:-1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:0;z:1;);Normal:(x:0;y:-1;z:0;);TexCoord:(u:1;v:0)),

        // Top
        (Position:(x:-1;y: 1;z:-1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:0;z:-1;);Normal:(x:0;y:1;z:0;);TexCoord:(u:0;v:0)),
        (Position:(x: 1;y: 1;z:-1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:0;z:-1;);Normal:(x:0;y:1;z:0;);TexCoord:(u:0;v:1)),
        (Position:(x: 1;y: 1;z: 1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:0;z:-1;);Normal:(x:0;y:1;z:0;);TexCoord:(u:1;v:1)),
        (Position:(x:-1;y: 1;z: 1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:0;z:-1;);Normal:(x:0;y:1;z:0;);TexCoord:(u:1;v:0)),

        // Back
        (Position:(x: 1;y:-1;z:-1;);Tangent:(x:-1;y:0;z:0;);Bitangent:(x:0;y:1;z:0;);Normal:(x:0;y:0;z:-1;);TexCoord:(u:0;v:0)),
        (Position:(x: 1;y: 1;z:-1;);Tangent:(x:-1;y:0;z:0;);Bitangent:(x:0;y:1;z:0;);Normal:(x:0;y:0;z:-1;);TexCoord:(u:0;v:1)),
        (Position:(x:-1;y: 1;z:-1;);Tangent:(x:-1;y:0;z:0;);Bitangent:(x:0;y:1;z:0;);Normal:(x:0;y:0;z:-1;);TexCoord:(u:1;v:1)),
        (Position:(x:-1;y:-1;z:-1;);Tangent:(x:-1;y:0;z:0;);Bitangent:(x:0;y:1;z:0;);Normal:(x:0;y:0;z:-1;);TexCoord:(u:1;v:0)),

        // Front
        (Position:(x:-1;y:-1;z:1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:1;z:0;);Normal:(x:0;y:0;z:1;);TexCoord:(u:0;v:0)),
        (Position:(x:-1;y: 1;z:1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:1;z:0;);Normal:(x:0;y:0;z:1;);TexCoord:(u:0;v:1)),
        (Position:(x: 1;y: 1;z:1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:1;z:0;);Normal:(x:0;y:0;z:1;);TexCoord:(u:1;v:1)),
        (Position:(x: 1;y:-1;z:1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:1;z:0;);Normal:(x:0;y:0;z:1;);TexCoord:(u:1;v:0))

       );
      CubeIndices:array[0..35] of TVkInt32=
       ( 0, 1, 2,
         0, 2, 3,

         // Right
         4, 5, 6,
         4, 6, 7,

         // Bottom
         8, 9, 10,
         8, 10, 11,

         // Top
         12, 13, 14,
         12, 14, 15,

         // Back
         16, 17, 18,
         16, 18, 19,

         // Front
         20, 21, 22,
         20, 22, 23);
var Index:TVkInt32;
    Material:PModelMaterial;
    ModelVertex:PModelVertex;
    CubeVertex:PCubeVertex;
    m:TMatrix3x3;
    q:TQuaternion;
    Part:PModelPart;
    AObject:PModelObject;
begin

 fCountMaterials:=1;
 fCountVertices:=length(CubeVertices);
 fCountIndices:=length(CubeIndices);
 fCountParts:=1;
 fCountObjects:=1;

 SetLength(fMaterials,fCountMaterials);
 SetLength(fVertices,fCountVertices);
 SetLength(fIndices,fCountIndices);
 SetLength(fParts,fCountParts);
 SetLength(fObjects,fCountObjects);

 Material:=@fMaterials[0];
 Material^.Name:='cube';
 Material^.Texture:='cube';
 Material^.Ambient:=UnitMath3D.Vector3(0.1,0.1,0.1);
 Material^.Diffuse:=UnitMath3D.Vector3(0.8,0.8,0.8);
 Material^.Emission:=UnitMath3D.Vector3(0.0,0.0,0.0);
 Material^.Specular:=UnitMath3D.Vector3(0.1,0.1,0.1);
 Material^.Shininess:=1.0;

 for Index:=0 to fCountVertices-1 do begin
  ModelVertex:=@fVertices[Index];
  CubeVertex:=@CubeVertices[Index];
  ModelVertex^.Position:=Vector3Mul(CubeVertex^.Position,UnitMath3D.Vector3(pSizeX*0.5,pSizeY*0.5,pSizeZ*0.5));
  m[0,0]:=CubeVertex^.Tangent.x;
  m[0,1]:=CubeVertex^.Tangent.y;
  m[0,2]:=CubeVertex^.Tangent.z;
  m[1,0]:=CubeVertex^.Bitangent.x;
  m[1,1]:=CubeVertex^.Bitangent.y;
  m[1,2]:=CubeVertex^.Bitangent.z;
  m[2,0]:=CubeVertex^.Normal.x;
  m[2,1]:=CubeVertex^.Normal.y;
  m[2,2]:=CubeVertex^.Normal.z;
  q:=Matrix3x3ToQTangent(m);
  ModelVertex^.QTangent.x:=Min(Max(round(Min(Max(q.x,-1.0),1.0)*32767),-32767),32767);
  ModelVertex^.QTangent.y:=Min(Max(round(Min(Max(q.y,-1.0),1.0)*32767),-32767),32767);
  ModelVertex^.QTangent.z:=Min(Max(round(Min(Max(q.z,-1.0),1.0)*32767),-32767),32767);
  ModelVertex^.QTangent.w:=Min(Max(round(Min(Max(q.w,-1.0),1.0)*32767),-32767),32767);
  ModelVertex^.TexCoord:=CubeVertex^.TexCoord;
  ModelVertex^.Material:=0;
 end;

 for Index:=0 to fCountIndices-1 do begin
  fIndices[Index]:=CubeIndices[Index];
 end;

 Part:=@fParts[0];
 Part^.Material:=0;
 Part^.StartIndex:=0;
 Part^.CountIndices:=fCountIndices;

 AObject:=@fObjects[0];
 AObject^.Name:='cube';
 AObject^.AABB.Min.x:=-(pSizeX*0.5);
 AObject^.AABB.Min.y:=-(pSizeY*0.5);
 AObject^.AABB.Min.z:=-(pSizeZ*0.5);
 AObject^.AABB.Max.x:=pSizeX*0.5;
 AObject^.AABB.Max.y:=pSizeY*0.5;
 AObject^.AABB.Max.z:=pSizeZ*0.5;
 AObject^.Sphere:=SphereFromAABB(AObject^.AABB);

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
       q.x:=Vertices[VertexIndex].QTangent.x/32767.0;
       q.y:=Vertices[VertexIndex].QTangent.y/32767.0;
       q.z:=Vertices[VertexIndex].QTangent.z/32767.0;
       q.w:=Vertices[VertexIndex].QTangent.w/32767.0;
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

procedure TModel.Upload(const pQueue:TVulkanQueue;
                        const pCommandBuffer:TVulkanCommandBuffer;
                        const pFence:TVulkanFence);
var MaxIndexedIndex:TVkUInt32;
    MaxCount:TVkUInt64;
begin
 if not fUploaded then begin

  if fVulkanDevice.PhysicalDevice.Features.fullDrawIndexUint32<>0 then begin
   MaxIndexedIndex:=high(TVkUInt32)-1;
  end else begin
   MaxIndexedIndex:=fVulkanDevice.PhysicalDevice.Properties.limits.maxDrawIndexedIndexValue;
  end;
  MaxCount:=MaxIndexedIndex+1;

  if fCountIndices<MaxCount then begin

   // Good, the model fits into one whole single vertex buffer

   fCountBuffers:=1;

   SetLength(fVertexBuffers,fCountBuffers);
   SetLength(fIndexBuffers,fCountBuffers);
   SetLength(fBufferSizes,fCountBuffers);

   fBufferSizes[0]:=fCountIndices;

   fVertexBuffers[0]:=TVulkanBuffer.Create(fVulkanDevice,
                                           fCountVertices*SizeOf(TModelVertex),
                                           TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT),
                                           TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                           nil,
                                           TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
                                          );
   fVertexBuffers[0].UploadData(pQueue,
                                pCommandBuffer,
                                pFence,
                                fVertices[0],
                                0,
                                fCountVertices*SizeOf(TModelVertex),
                                vbutsbmYes);

   fIndexBuffers[0]:=TVulkanBuffer.Create(fVulkanDevice,
                                          fCountIndices*SizeOf(TModelIndex),
                                          TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_INDEX_BUFFER_BIT),
                                          TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                          nil,
                                          TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
                                         );
   fIndexBuffers[0].UploadData(pQueue,
                               pCommandBuffer,
                               pFence,
                               fIndices[0],
                               0,
                               fCountIndices*SizeOf(TModelIndex),
                               vbutsbmYes);

  end else begin

   // We do to need split the model into multipe vertex buffers

   Assert(false,'TODO');

  end;

  fUploaded:=true;

 end;
end;

procedure TModel.Unload;
var Index:TVkInt32;
begin
 if fUploaded then begin

  fUploaded:=false;

  for Index:=0 to fCountBuffers-1 do begin
   FreeAndNil(fVertexBuffers[Index]);
   FreeAndNil(fIndexBuffers[Index]);
  end;

  fVertexBuffers:=nil;
  fIndexBuffers:=nil;
  fBufferSizes:=nil;
  fCountBuffers:=0;

 end;
end;

procedure TModel.Draw(const pCommandBuffer:TVulkanCommandBuffer;const pInstanceCount:TVkUInt32=1;const pFirstInstance:TVkUInt32=0);
const Offsets:array[0..0] of TVkDeviceSize=(0);
var Index:TVkInt32;
begin
 for Index:=0 to fCountBuffers-1 do begin
  pCommandBuffer.CmdBindVertexBuffers(VULKAN_MODEL_VERTEX_BUFFER_BIND_ID,1,@fVertexBuffers[Index].Handle,@Offsets);
  pCommandBuffer.CmdBindIndexBuffer(fIndexBuffers[Index].Handle,0,VK_INDEX_TYPE_UINT32);
  pCommandBuffer.CmdDrawIndexed(fBufferSizes[Index],pInstanceCount,0,0,pFirstInstance);
 end;
end;

end.
