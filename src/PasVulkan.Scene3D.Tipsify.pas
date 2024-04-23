(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2024, Benjamin Rosseaux (benjamin@rosseaux.de)          *
 *                                                                            *
 * This software is provided 'as-is', without any express or implied          *
 * warranty. In no event will the authors be held liable for any damages      *
 * arising from the use of this software.                                     *
 *                                                                            *
 * Permission is granted to anyone to use this software for any purpose,      *
 * including commercial applications, and to alter it and redistribute it     *
 * freely, subject to the following restrictions:                             *
 *                                                                            *
 * 1. The origin of this software must not be misrepresented; you must not    *
 *    claim that you wrote the original software. If you use this software    *
 *    in a product, an acknowledgement in the product documentation would be  *
 *    appreciated but is not required.                                        *
 * 2. Altered source versions must be plainly marked as such, and must not be *
 *    misrepresented as being the original software.                          *
 * 3. This notice may not be removed or altered from any source distribution. *
 *                                                                            *
 ******************************************************************************
 *                  General guidelines for code contributors                  *
 *============================================================================*
 *                                                                            *
 * 1. Make sure you are legally allowed to make a contribution under the zlib *
 *    license.                                                                *
 * 2. The zlib license header goes at the top of each source file, with       *
 *    appropriate copyright notice.                                           *
 * 3. This PasVulkan wrapper may be used only with the PasVulkan-own Vulkan   *
 *    Pascal header.                                                          *
 * 4. After a pull request, check the status of your pull request on          *
      http://github.com/BeRo1985/pasvulkan                                    *
 * 5. Write code which's compatible with Delphi >= 2009 and FreePascal >=     *
 *    3.1.1                                                                   *
 * 6. Don't use Delphi-only, FreePascal-only or Lazarus-only libraries/units, *
 *    but if needed, make it out-ifdef-able.                                  *
 * 7. No use of third-party libraries/units as possible, but if needed, make  *
 *    it out-ifdef-able.                                                      *
 * 8. Try to use const when possible.                                         *
 * 9. Make sure to comment out writeln, used while debugging.                 *
 * 10. Make sure the code compiles on 32-bit and 64-bit platforms (x86-32,    *
 *     x86-64, ARM, ARM64, etc.).                                             *
 * 11. Make sure the code runs on all platforms with Vulkan support           *
 *                                                                            *
 ******************************************************************************)
unit PasVulkan.Scene3D.Tipsify;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Collections,
     PasVulkan.Scene3D;

type { TpvScene3DTipsify }
     TpvScene3DTipsify=class
      public
       type TIndicesDynamicArray=TpvScene3D.TIndicesDynamicArray;
            TUInt32DynamicArray=TpvDynamicArray<TpvUInt32>;
            TUInt32Queue=TpvDynamicQueue<TpvUInt32>;
            TBooleanDynamicArray=TpvDynamicArray<Boolean>;
            { TAdjacencyInfo }
            TAdjacencyInfo=class
             private
              fTrianglesPerVertex:TUInt32DynamicArray;
              fIndexBufferOffset:TUInt32DynamicArray;
              fTriangleData:TUInt32DynamicArray;
             public
              constructor Create;
              destructor Destroy; override;
              procedure BuildAdjaceny(const aCountVertices,aCountIndices:TpvSizeInt;const aIndices:TIndicesDynamicArray);
            end;
      private
       class function SkipDeadEnd(const aIndices:TIndicesDynamicArray;const aLiveTriCount:TUInt32DynamicArray;var aDeadEndStack:TUInt32Queue;const aCurrentVertex:TpvUInt32;const aCountVertices:TpvUInt32;var aCursor:TpvSizeInt):TpvSizeInt; static;
       class function GetNextVertex(const aIndices:TIndicesDynamicArray;const aCurrentVertex:TpvUInt32;const aCacheSize:TpvSizeInt;const aOneRing:TUInt32DynamicArray;const aCacheTimeStamps:TUInt32DynamicArray;const aTimeStamp:TpvUInt32;const aLiveTriCount:TUInt32DynamicArray;var aDeadEndStack:TUInt32Queue;const aCountVertices:TpvUInt32;var aCursor:TpvSizeInt):TpvSizeInt; static; 
      public 
       class procedure OptimizeIndexBuffer(const aIndices:TIndicesDynamicArray;const aCountIndices,aCountVertices,aCacheSize:TpvSizeInt;var aOptimizedIndices:TIndicesDynamicArray); static;
     end;     

implementation

{ TpvScene3DTipsify.TAdjacencyInfo }

constructor TpvScene3DTipsify.TAdjacencyInfo.Create;
begin
 inherited Create;
 fTrianglesPerVertex.Initialize;
 fIndexBufferOffset.Initialize;
 fTriangleData.Initialize;
end;

destructor TpvScene3DTipsify.TAdjacencyInfo.Destroy;
begin
 fTriangleData.Finalize;
 fIndexBufferOffset.Finalize;
 fTrianglesPerVertex.Finalize;
 inherited Destroy;
end;

procedure TpvScene3DTipsify.TAdjacencyInfo.BuildAdjaceny(const aCountVertices,aCountIndices:TpvSizeInt;const aIndices:TIndicesDynamicArray);
var Index,CountTriangles:TpvSizeInt;
    TriangleOffset,a,b,c:TpvUInt32;
begin

 // Count how often a vertex is used in the index buffer
 fTrianglesPerVertex.Resize(aCountVertices);
 FillChar(fTrianglesPerVertex.Items[0],fTrianglesPerVertex.Count*SizeOf(TpvUInt32),#0);
 for Index:=0 to aCountIndices-1 do begin
  inc(fTrianglesPerVertex.Items[aIndices[Index]]);
 end;

	// Calculate the offsets for to need to look up into the index buffer for a given triangle 
 TriangleOffset:=0;
 fIndexBufferOffset.Resize(aCountVertices);
 FillChar(fIndexBufferOffset.Items[0],fIndexBufferOffset.Count*SizeOf(TpvUInt32),#0);
 for Index:=0 to aCountVertices-1 do begin
  fIndexBufferOffset.Items[Index]:=TriangleOffset;
  inc(TriangleOffset,fTrianglesPerVertex.Items[Index]);
 end;

 // Build the triangle data
 CountTriangles:=aCountIndices div 3;
 fTriangleData.Resize(TriangleOffset);
 for Index:=0 to CountTriangles-1 do begin
  
  a:=aIndices.Items[(Index*3)+0];
  b:=aIndices.Items[(Index*3)+1];
  c:=aIndices.Items[(Index*3)+2];
  
  fTriangleData.Items[fIndexBufferOffset.Items[a]]:=Index;
  inc(fIndexBufferOffset.Items[a]);
  
  fTriangleData.Items[fIndexBufferOffset.Items[b]]:=Index;
  inc(fIndexBufferOffset.Items[b]);
  
  fTriangleData.Items[fIndexBufferOffset.Items[c]]:=Index;
  inc(fIndexBufferOffset.Items[c]);

 end;

end;

{ TpvScene3DTipsify }

class function TpvScene3DTipsify.SkipDeadEnd(const aIndices:TIndicesDynamicArray;const aLiveTriCount:TUInt32DynamicArray;var aDeadEndStack:TUInt32Queue;const aCurrentVertex:TpvUInt32;const aCountVertices:TpvUInt32;var aCursor:TpvSizeInt):TpvSizeInt;
var VertexIndex:TpvUInt32;
begin
 while aDeadEndStack.Dequeue(VertexIndex) do begin
  if aLiveTriCount.Items[VertexIndex]>0 then begin
   result:=VertexIndex;
   exit;
  end;   
 end; 
 while aCursor<aLiveTriCount.Count do begin
  if aLiveTriCount.Items[aCursor]>0 then begin
   result:=aCursor;
   exit;
  end else begin
   inc(aCursor);
  end; 
 end;
 result:=-1;
end;

class function TpvScene3DTipsify.GetNextVertex(const aIndices:TIndicesDynamicArray;const aCurrentVertex:TpvUInt32;const aCacheSize:TpvSizeInt;const aOneRing:TUInt32DynamicArray;const aCacheTimeStamps:TUInt32DynamicArray;const aTimeStamp:TpvUInt32;const aLiveTriCount:TUInt32DynamicArray;var aDeadEndStack:TUInt32Queue;const aCountVertices:TpvUInt32;var aCursor:TpvSizeInt):TpvSizeInt; static; 
var Index,HighestPriority,Priority:TpvSizeInt;
    VertexIndex:TpvUInt32;
begin
 result:=-1;
 HighestPriority:=-1;
 for Index:=0 to aOneRing.Count-1 do begin
  VertexIndex:=aOneRing.Items[Index];
  if aLiveTriCount.Items[VertexIndex]>0 then begin
   Priority:=0;
   if (TpvSizeInt(aTimeStamp+(aLiveTriCount.Items[VertexIndex]*2))-TpvSizeInt(aCacheTimeStamps.Items[VertexIndex]))<=aCacheSize then begin
    Priority:=TpvSizeInt(aTimeStamp)-TpvSizeInt(aCacheTimeStamps.Items[VertexIndex]);
   end;
   if HighestPriority<Priority then begin
    HighestPriority:=Priority;
    result:=VertexIndex;
   end;   
  end; 
 end;
 if result<0 then begin
  result:=SkipDeadEnd(aIndices,aLiveTriCount,aDeadEndStack,aCurrentVertex,aCountVertices,aCursor);
 end;
end;

class procedure TpvScene3DTipsify.OptimizeIndexBuffer(const aIndices:TIndicesDynamicArray;const aCountIndices,aCountVertices,aCacheSize:TpvSizeInt;var aOptimizedIndices:TIndicesDynamicArray); 
var Index,Triangle,CurrentVertex,TimeStamp,Cursor:TpvSizeInt;
    AdjacencyInfo:TAdjacencyInfo;
    LiveTriCount:TUInt32DynamicArray;
    CacheTimeStamps:TUInt32DynamicArray;
    DeadEndStack:TUInt32Queue;
    OneRing:TUInt32DynamicArray;
    EmittedTriangles:TBooleanDynamicArray;
    a,b,c:TpvUInt32;
begin

 aOptimizedIndices.ClearNoFree;

 AdjacencyInfo:=TAdjacencyInfo.Create;
 try

  AdjacencyInfo.BuildAdjaceny(aCountVertices,aCountIndices,aIndices);

  LiveTriCount.Initialize;
  try

   LiveTriCount.Assign(AdjacencyInfo.fTrianglesPerVertex);

   CacheTimeStamps.Initialize;
   try
     
    CacheTimeStamps.Resize(aCountVertices);
    FillChar(CacheTimeStamps.Items[0],CacheTimeStamps.Count*SizeOf(TpvUInt32),#0);

    DeadEndStack.Initialize;
    try

     EmittedTriangles.Initialize;
     try

      EmittedTriangles.Resize(aCountIndices div 3);
      FillChar(EmittedTriangles.Items[0],EmittedTriangles.Count*SizeOf(Boolean),#0);

      CurrentVertex:=0;
      TimeStamp:=aCacheSize+1;
      Cursor:=1;

      OneRing.Initialize;
      try
      
       while CurrentVertex>=0 do begin

        OneRing.Clear;

        for Index:=TpvSizeInt(AdjacencyInfo.fIndexBufferOffset.Items[CurrentVertex]) to TpvSizeInt(AdjacencyInfo.fIndexBufferOffset.Items[CurrentVertex]+AdjacencyInfo.fTrianglesPerVertex.Items[CurrentVertex])-1 do begin

         Triangle:=AdjacencyInfo.fTriangleData.Items[Index];

         if not EmittedTriangles.Items[Triangle] then begin

          a:=aIndices.Items[(Triangle*3)+0];
          b:=aIndices.Items[(Triangle*3)+1];
          c:=aIndices.Items[(Triangle*3)+2];

          aOptimizedIndices.Add(a);
          aOptimizedIndices.Add(b);
          aOptimizedIndices.Add(c);

          DeadEndStack.Enqueue(a);
          DeadEndStack.Enqueue(b);
          DeadEndStack.Enqueue(c);

          OneRing.Add(a);
          OneRing.Add(b);
          OneRing.Add(c);

          dec(LiveTriCount.Items[a]);
          dec(LiveTriCount.Items[b]);
          dec(LiveTriCount.Items[c]);

          if (TpvSizeInt(TimeStamp)-TpvSizeInt(CacheTimeStamps.Items[a]))>TpvSizeInt(aCacheSize) then begin
           CacheTimeStamps.Items[a]:=TimeStamp;
           inc(TimeStamp);
          end;

          if (TpvSizeInt(TimeStamp)-TpvSizeInt(CacheTimeStamps.Items[b]))>TpvSizeInt(aCacheSize) then begin
           CacheTimeStamps.Items[b]:=TimeStamp;
           inc(TimeStamp);
          end;

          if (TpvSizeInt(TimeStamp)-TpvSizeInt(CacheTimeStamps.Items[c]))>TpvSizeInt(aCacheSize) then begin
           CacheTimeStamps.Items[c]:=TimeStamp;
           inc(TimeStamp);
          end;

          EmittedTriangles.Items[Triangle]:=true;

         end;         

        end;

        CurrentVertex:=GetNextVertex(aIndices,CurrentVertex,aCacheSize,OneRing,CacheTimeStamps,TimeStamp,LiveTriCount,DeadEndStack,aCountVertices,Cursor);

       end;

      finally
       OneRing.Finalize;
      end; 

     finally
      EmittedTriangles.Finalize;
     end;

    finally
     DeadEndStack.Finalize;
    end;

   finally
    CacheTimeStamps.Finalize;
   end;

  finally
   LiveTriCount.Finalize;
  end;
   
 finally
  FreeAndNil(AdjacencyInfo);
 end;
   
end;

end.
