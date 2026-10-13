(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2026, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.VectorPath;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$m+}

{$define CANINLINE}

interface

uses SysUtils,
     Classes,
     Math,
     Generics.Collections,
     Vulkan,
     PasDblStrUtils,
     PasMP,
     PasVulkan.Types,
     PasVulkan.Framework,
     PasVulkan.Utils,
     PasVulkan.Collections,
     PasVulkan.BufferRangeAllocator,
     PasVulkan.Math,
     PasVulkan.Math.Double,
     PasVulkan.Streams;

type PpvVectorPathCommandType=^TpvVectorPathCommandType;
     TpvVectorPathCommandType=
      (
       MoveTo,
       LineTo,
       QuadraticCurveTo,
       CubicCurveTo,
       Close
      );

     { TpvVectorPathVector }

     TpvVectorPathVector=record
      public
       constructor Create(const aValue:TpvDouble); overload;
       constructor Create(const aX,aY:TpvDouble); overload;
       function Length:TpvDouble; {$ifdef CANINLINE}inline;{$endif}
       function LengthSquared:TpvDouble; {$ifdef CANINLINE}inline;{$endif}
       function Distance(const b:TpvVectorPathVector):TpvDouble; {$ifdef CANINLINE}inline;{$endif}
       function DistanceSquared(const b:TpvVectorPathVector):TpvDouble; {$ifdef CANINLINE}inline;{$endif}
       function Direction:TpvDouble; {$ifdef CANINLINE}inline;{$endif}
       function Normalize:TpvVectorPathVector; {$ifdef CANINLINE}inline;{$endif}
       function Minimum(const aRight:TpvVectorPathVector):TpvVectorPathVector;
       function Maximum(const aRight:TpvVectorPathVector):TpvVectorPathVector;
       function Dot(const aRight:TpvVectorPathVector):TpvDouble; {$ifdef CANINLINE}inline;{$endif}
       function Cross(const aRight:TpvVectorPathVector):TpvDouble; {$ifdef CANINLINE}inline;{$endif}
       function OrthoNormal:TpvVectorPathVector; {$ifdef CANINLINE}inline;{$endif}
       function Lerp(const b:TpvVectorPathVector;const t:TpvDouble):TpvVectorPathVector;
       function ClampedLerp(const b:TpvVectorPathVector;const t:TpvDouble):TpvVectorPathVector;
       class function IsLeft(const a,b,c:TpvVectorPathVector):TpvDouble; static;
       class operator Implicit(const a:TpvVector2):TpvVectorPathVector; {$ifdef CANINLINE}inline;{$endif}
       class operator Implicit(const a:TpvVectorPathVector):TpvVector2; {$ifdef CANINLINE}inline;{$endif}
       class operator Implicit(const a:TpvVector2D):TpvVectorPathVector; {$ifdef CANINLINE}inline;{$endif}
       class operator Implicit(const a:TpvVectorPathVector):TpvVector2D; {$ifdef CANINLINE}inline;{$endif}
       class operator Explicit(const a:TpvVectorPathVector):TpvVector2; {$ifdef CANINLINE}inline;{$endif}
       class operator Explicit(const a:TpvVector2):TpvVectorPathVector; {$ifdef CANINLINE}inline;{$endif}
       class operator Explicit(const a:TpvVectorPathVector):TpvVector2D; {$ifdef CANINLINE}inline;{$endif}
       class operator Explicit(const a:TpvVector2D):TpvVectorPathVector; {$ifdef CANINLINE}inline;{$endif}
       class operator Equal(const a,b:TpvVectorPathVector):boolean; {$ifdef CANINLINE}inline;{$endif}
       class operator NotEqual(const a,b:TpvVectorPathVector):boolean; {$ifdef CANINLINE}inline;{$endif}
       class operator Add(const a,b:TpvVectorPathVector):TpvVectorPathVector; {$ifdef CANINLINE}inline;{$endif}
       class operator Subtract(const a,b:TpvVectorPathVector):TpvVectorPathVector; {$ifdef CANINLINE}inline;{$endif}
       class operator Multiply(const a,b:TpvVectorPathVector):TpvVectorPathVector; overload; {$ifdef CANINLINE}inline;{$endif}
       class operator Multiply(const a:TpvVectorPathVector;const b:TpvDouble):TpvVectorPathVector; overload; {$ifdef CANINLINE}inline;{$endif}
       class operator Multiply(const a:TpvDouble;const b:TpvVectorPathVector):TpvVectorPathVector; overload; {$ifdef CANINLINE}inline;{$endif}
       class operator Divide(const a,b:TpvVectorPathVector):TpvVectorPathVector; overload; {$ifdef CANINLINE}inline;{$endif}
       class operator Divide(const a:TpvVectorPathVector;const b:TpvDouble):TpvVectorPathVector; overload; {$ifdef CANINLINE}inline;{$endif}
       class operator Negative(const a:TpvVectorPathVector):TpvVectorPathVector; {$ifdef CANINLINE}inline;{$endif}
       class operator Positive(const a:TpvVectorPathVector):TpvVectorPathVector; {$ifdef CANINLINE}inline;{$endif}
      public
       case boolean of
        false:(
         x:TpvDouble;
         y:TpvDouble;
        );
        true:(
         xy:array[0..1] of TpvDouble;
        );
     end;

     PpvVectorPathVector=^TpvVectorPathVector;

     TpvVectorPathVectors=array of TpvVectorPathVector;

     { TpvVectorPathVectorList }

     TpvVectorPathVectorList=class(TpvGenericList<TpvVectorPathVector>)
      public
       procedure Sort; reintroduce;
       procedure RemoveDuplicates;
     end;

     TpvVectorPathRawVectors=array[0..65535] of TpvVectorPathVector;

     PpvVectorPathRawVectors=^TpvVectorPathRawVectors;

     { TpvVectorPathBoundingBox }

     TpvVectorPathBoundingBox=record
      public
       const EPSILON=1e-14;
      public
       constructor Create(const aMin,aMax:TpvVectorPathVector);
       procedure Extend(const aVector:TpvVectorPathVector);
       function Combine(const aWith:TpvVectorPathBoundingBox):TpvVectorPathBoundingBox;
       function Cost:TpvDouble;
       function Volume:TpvDouble;
       function Area:TpvDouble;
       function Center:TpvVectorPathVector;
       function Contains(const aVector:TpvVectorPathVector):Boolean; overload;
       class function Contains(const aMin,aMax:TpvVectorPathVector;const aWith:TpvVectorPathBoundingBox):Boolean; overload; static;
       class function Contains(const aMin,aMax:TpvVector2;const aWith:TpvVectorPathBoundingBox):Boolean; overload; static;
       function Contains(const aBoundingBox:TpvVectorPathBoundingBox):Boolean; overload;
       class function Contains(const aMin,aMax,aVector:TpvVectorPathVector):boolean; overload; static;
       class function Contains(const aMin,aMax:TpvVector2;const aVector:TpvVectorPathVector):Boolean; overload; static;
       function Intersect(const aWith:TpvVectorPathBoundingBox;const aThreshold:TpvDouble=EPSILON):Boolean; overload;
       class function Intersect(const aMin,aMax:TpvVectorPathVector;const aWith:TpvVectorPathBoundingBox;const aThreshold:TpvDouble=EPSILON):Boolean; overload; static;
       class function Intersect(const aMin,aMax:TpvVector2;const aWith:TpvVectorPathBoundingBox;const aThreshold:TpvDouble=EPSILON):Boolean; overload; static;
       function FastRayIntersection(const aOrigin,aDirection:TpvVectorPathVector):Boolean; overload;
       class function FastRayIntersection(const aMin,aMax:TpvVectorPathVector;const aOrigin,aDirection:TpvVectorPathVector):Boolean; overload; static;
       class function FastRayIntersection(const aMin,aMax:TpvVector2;const aOrigin,aDirection:TpvVectorPathVector):Boolean; overload; static;
       function LineIntersection(const aStartPoint,aEndPoint:TpvVectorPathVector):Boolean; overload;
       class function LineIntersection(const aMin,aMax:TpvVectorPathVector;const aStartPoint,aEndPoint:TpvVectorPathVector):Boolean; overload; static;
       class function LineIntersection(const aMin,aMax:TpvVector2;const aStartPoint,aEndPoint:TpvVectorPathVector):Boolean; overload; static;
      public
       case boolean of
        false:(
         Min:TpvVectorPathVector;
         Max:TpvVectorPathVector;
        );
        true:(
         MinMax:array[0..1] of TpvVectorPathVector;
        );
     end;

     PpvVectorPathBoundingBox=^TpvVectorPathBoundingBox;

     { TpvVectorPathMatrix }

     // Affine 2D transformation matrix in the row-major 3x2 layout that SVG uses for its
     // transform attribute, so that the six values map one-to-one onto matrix(a,b,c,d,e,f).
     TpvVectorPathMatrix=record
      public
       constructor Create(const aValue:TpvDouble); overload;
       constructor Create(const aM00,aM01,aM10,aM11,aM20,aM21:TpvDouble); overload;
       class function CreateIdentity:TpvVectorPathMatrix; static;
       class function CreateTranslate(const aTX,aTY:TpvDouble):TpvVectorPathMatrix; overload; static;
       class function CreateTranslate(const aTranslate:TpvVectorPathVector):TpvVectorPathMatrix; overload; static;
       class function CreateScale(const aSX,aSY:TpvDouble):TpvVectorPathMatrix; overload; static;
       class function CreateScale(const aScale:TpvVectorPathVector):TpvVectorPathMatrix; overload; static;
       class function CreateRotate(const aAngle:TpvDouble):TpvVectorPathMatrix; overload; static;
       class function CreateRotate(const aAngle,aCenterX,aCenterY:TpvDouble):TpvVectorPathMatrix; overload; static;
       class function CreateSkewX(const aAngle:TpvDouble):TpvVectorPathMatrix; static;
       class function CreateSkewY(const aAngle:TpvDouble):TpvVectorPathMatrix; static;
       function Determinant:TpvDouble;
       function Inverse:TpvVectorPathMatrix;
       // Average of the two axis scale factors, used to pick a curve flattening tolerance that
       // matches the on-screen size instead of the untransformed one.
       function ExpansionFactor:TpvDouble;
       function Transform(const aVector:TpvVectorPathVector):TpvVectorPathVector; overload; {$ifdef CANINLINE}inline;{$endif}
       function Transform(const aX,aY:TpvDouble):TpvVectorPathVector; overload; {$ifdef CANINLINE}inline;{$endif}
       // Transforms a direction, so without the translation part.
       function TransformDirection(const aVector:TpvVectorPathVector):TpvVectorPathVector; {$ifdef CANINLINE}inline;{$endif}
       class operator Equal(const a,b:TpvVectorPathMatrix):boolean;
       class operator NotEqual(const a,b:TpvVectorPathMatrix):boolean;
       class operator Multiply(const a,b:TpvVectorPathMatrix):TpvVectorPathMatrix;
       class operator Multiply(const a:TpvVectorPathMatrix;const b:TpvVectorPathVector):TpvVectorPathVector;
       class operator Multiply(const a:TpvVectorPathVector;const b:TpvVectorPathMatrix):TpvVectorPathVector;
      public
       case boolean of
        false:(
         m00:TpvDouble;
         m01:TpvDouble;
         m10:TpvDouble;
         m11:TpvDouble;
         m20:TpvDouble;
         m21:TpvDouble;
        );
        true:(
         Values:array[0..5] of TpvDouble;
        );
     end;

     PpvVectorPathMatrix=^TpvVectorPathMatrix;

     { TpvVectorPathBVHDynamicAABBTree }
     TpvVectorPathBVHDynamicAABBTree=class
      public
       const NULLNODE=-1;
             AABBMULTIPLIER=2.0;
             AABBEPSILON=1e-14;
             ThresholdAABBVector:TpvVectorPathVector=(x:AABBEPSILON;y:AABBEPSILON);
       type TTreeNode=record
             public
              AABB:TpvVectorPathBoundingBox;
              UserData:TpvPtrInt;
              Children:array[0..1] of TpvSizeInt;
              Height:TpvSizeInt;
              case boolean of
               false:(
                Parent:TpvSizeInt;
               );
               true:(
                Next:TpvSizeInt;
               );
            end;
            PTreeNode=^TTreeNode;
            TTreeNodes=array of TTreeNode;
            TState=record
             TreeNodes:TTreeNodes;
             Root:TpvSizeInt;
            end;
            PState=^TState;
            TUserDataArray=array of TpvPtrInt;
            TSizeIntArray=array[0..65535] of TpvSizeInt;
            PSizeIntArray=^TSizeIntArray;
            TSkipListNode=packed record // <= GPU-compatible with 32 bytes per node
             public
              // (u)vec4 aabbMinSkipCount
              AABBMin:TpvVector2;
              SkipCount:TpvUInt32;
              Dummy0:TpvUInt32;
              // (u)vec4 aabbMaxUserData
              AABBMax:TpvVector2;
              UserData:TpvUInt32;
              Dummy1:TpvUInt32;
            end;
            PSkipListNode=^TSkipListNode;
            TSkipListNodes=array of TSkipListNode;
            TSkipListNodeArray=TpvDynamicArray<TSkipListNode>;
            TSkipListNodeMap=array of TpvSizeUInt;
            TSkipListNodeStackItem=record
             Pass:TpvSizeInt;
             Node:TpvSizeInt;
            end;
            PSkipListNodeStackItem=^TSkipListNodeStackItem;
            TSkipListNodeStack=TpvDynamicStack<TSkipListNodeStackItem>;
            TGetUserDataIndex=function(const aUserData:TpvPtrInt):TpvUInt32 of object;
            TRayCastUserData=function(const aUserData:TpvPtrInt;const aRayOrigin,aRayDirection:TpvVectorPathVector;out aTime:TpvDouble;out aStop:boolean):boolean of object;
            { TSkipList }
            TSkipList=class
             private
              fNodeArray:TSkipListNodeArray;
             public
              constructor Create(const aFrom:TpvVectorPathBVHDynamicAABBTree;const aGetUserDataIndex:TpvVectorPathBVHDynamicAABBTree.TGetUserDataIndex); reintroduce;
              destructor Destroy; override;
              function IntersectionQuery(const aAABB:TpvVectorPathBoundingBox):TpvVectorPathBVHDynamicAABBTree.TUserDataArray;
              function ContainQuery(const aAABB:TpvVectorPathBoundingBox):TpvVectorPathBVHDynamicAABBTree.TUserDataArray; overload;
              function ContainQuery(const aPoint:TpvVectorPathVector):TpvVectorPathBVHDynamicAABBTree.TUserDataArray; overload;
              function RayCast(const aRayOrigin,aRayDirection:TpvVectorPathVector;out aTime:TpvDouble;out aUserData:TpvUInt32;const aStopAtFirstHit:boolean;const aRayCastUserData:TpvVectorPathBVHDynamicAABBTree.TRayCastUserData):boolean;
              function RayCastLine(const aFrom,aTo:TpvVectorPathVector;out aTime:TpvDouble;out aUserData:TpvUInt32;const aStopAtFirstHit:boolean;const aRayCastUserData:TpvVectorPathBVHDynamicAABBTree.TRayCastUserData):boolean;
             public
              property NodeArray:TSkipListNodeArray read fNodeArray;
            end;
      private
       fSkipListNodeLock:TPasMPSpinLock;
       fSkipListNodeMap:TSkipListNodeMap;
       fSkipListNodeStack:TSkipListNodeStack;
      public
       Root:TpvSizeInt;
       Nodes:TTreeNodes;
       NodeCount:TpvSizeInt;
       NodeCapacity:TpvSizeInt;
       FreeList:TpvSizeInt;
       Path:TpvSizeUInt;
       InsertionCount:TpvSizeInt;
       constructor Create;
       destructor Destroy; override;
       function AllocateNode:TpvSizeInt;
       procedure FreeNode(const aNodeID:TpvSizeInt);
       function Balance(const aNodeID:TpvSizeInt):TpvSizeInt;
       procedure InsertLeaf(const aLeaf:TpvSizeInt);
       procedure RemoveLeaf(const aLeaf:TpvSizeInt);
       function CreateProxy(const aAABB:TpvVectorPathBoundingBox;const aUserData:TpvPtrInt):TpvSizeInt;
       procedure DestroyProxy(const aNodeID:TpvSizeInt);
       function MoveProxy(const aNodeID:TpvSizeInt;const aAABB:TpvVectorPathBoundingBox;const aDisplacement:TpvVectorPathVector):boolean;
       procedure Rebalance(const aIterations:TpvSizeInt);
       procedure RebuildBottomUp;
       procedure RebuildTopDown;
       procedure Rebuild;
       function ComputeHeight:TpvSizeInt;
       function GetHeight:TpvSizeInt;
       function GetAreaRatio:TpvDouble;
       function GetMaxBalance:TpvSizeInt;
       function ValidateStructure:boolean;
       function ValidateMetrics:boolean;
       function Validate:boolean;
       function IntersectionQuery(const aAABB:TpvVectorPathBoundingBox):TpvVectorPathBVHDynamicAABBTree.TUserDataArray;
       function ContainQuery(const aAABB:TpvVectorPathBoundingBox):TpvVectorPathBVHDynamicAABBTree.TUserDataArray; overload;
       function ContainQuery(const aPoint:TpvVectorPathVector):TpvVectorPathBVHDynamicAABBTree.TUserDataArray; overload;
       function RayCast(const aRayOrigin,aRayDirection:TpvVectorPathVector;out aTime:TpvDouble;out aUserData:TpvUInt32;const aStopAtFirstHit:boolean;const aRayCastUserData:TpvVectorPathBVHDynamicAABBTree.TRayCastUserData):boolean;
       function RayCastLine(const aFrom,aTo:TpvVectorPathVector;out aTime:TpvDouble;out aUserData:TpvUInt32;const aStopAtFirstHit:boolean;const aRayCastUserData:TpvVectorPathBVHDynamicAABBTree.TRayCastUserData):boolean;
       procedure GetSkipListNodes(var aSkipListNodeArray:TSkipListNodeArray;const aGetUserDataIndex:TpvVectorPathBVHDynamicAABBTree.TGetUserDataIndex);
     end;

     { TpvVectorPathVectorCommand }

     TpvVectorPathCommand=class
      private
       fCommandType:TpvVectorPathCommandType;
       fX0:TpvDouble;
       fY0:TpvDouble;
       fX1:TpvDouble;
       fY1:TpvDouble;
       fX2:TpvDouble;
       fY2:TpvDouble;
      public
       constructor Create(const aCommandType:TpvVectorPathCommandType;
                          const aX0:TpvDouble=0.0;
                          const aY0:TpvDouble=0.0;
                          const aX1:TpvDouble=0.0;
                          const aY1:TpvDouble=0.0;
                          const aX2:TpvDouble=0.0;
                          const aY2:TpvDouble=0.0); reintroduce;
      published
       property CommandType:TpvVectorPathCommandType read fCommandType write fCommandType;
       property x0:TpvDouble read fX0 write fX0;
       property y0:TpvDouble read fY0 write fY0;
       property x1:TpvDouble read fX1 write fX1;
       property y1:TpvDouble read fY1 write fY1;
       property x2:TpvDouble read fX2 write fX2;
       property y2:TpvDouble read fY2 write fY2;
     end;

     TpvVectorPathCommandList=class(TObjectList<TpvVectorPathCommand>);

     TpvVectorPathFillRule=
      (
       NonZero=0,
       EvenOdd
      );

     PpvVectorPathFillRule=^TpvVectorPathFillRule;

     // Which structure the shader is given to find the curves near a pixel with.
     TpvVectorPathAccelerationStructure=
      (
       // A grid cut in both axes. A cell carries the segments that reach into it, and since it is
       // cut in x as well, it also needs meta winding setting lines standing for everything to its
       // left. Few curves to look at per pixel, but those meta lines are the bulk of the data by
       // far: at the tiger they are some ninety nine of every hundred segments kept.
       Grid=0,
       // Two sets of bands after Eric Lengyel's Slug algorithm, one cut along y and one along x,
       // each spanning the whole of the other axis. A band therefore carries every curve at its
       // height and needs no meta lines at all, which makes it some fifty times smaller; in return
       // a pixel walks more curves.
       SlugBands=1
      );

     PpvVectorPathAccelerationStructure=^TpvVectorPathAccelerationStructure;

     // How far repetition in the acceleration structure is folded away. Each step costs building
     // time and gives back space, and the last of them gives back very little.
     TpvVectorPathDuplicateSharing=
      (
       // Nothing, which is the fastest to build and what the others are measured against.
       None=0,
       // Meta winding setting lines that say the same thing become one, and cells holding the same
       // list of segments share one range of the indirect buffer. At the tiger this takes the grid
       // from 38,74 to 10,36 megabytes and its drawing from 1,08 to 0,34 milliseconds.
       EqualLists=1,
       // And on top of that, a cell whose list is a contiguous run inside another cell's list
       // points into the middle of that one, which is what Lengyel's notes suggest for the bands
       // of Slug. At the tiger it saves another 74 kilobytes and costs a second of building, so it
       // is there to be asked for rather than had by default.
       ContiguousRuns=2
      );

     PpvVectorPathDuplicateSharing=^TpvVectorPathDuplicateSharing;

     TpvVectorPathSegmentType=
      (
       Line=0,
       QuadraticCurve,
       CubicCurve,
       MetaWindingSettingLine
      );

     PpvVectorPathSegmentType=^TpvVectorPathSegmentType;

     { TpvVectorPathSegment }

     TpvVectorPathSegment=class
      private
       fType:TpvVectorPathSegmentType;
       fCachedBoundingBox:TpvVectorPathBoundingBox;
       fHasCachedBoundingBox:TPasMPBool32;
       fCachedBoundingBoxLock:TPasMPInt32;
       fIndex:TPasMPInt32;
      protected
       procedure GetIntersectionPointsWithLineSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList); virtual;
       procedure GetIntersectionPointsWithQuadraticCurveSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList); virtual;
       procedure GetIntersectionPointsWithCubicCurveSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList); virtual;
      public
       constructor Create; reintroduce; overload; virtual;
       destructor Destroy; override;
       procedure Assign(const aSegment:TpvVectorPathSegment); virtual;
       function Clone:TpvVectorPathSegment; virtual;
       function GetBoundingBox:TpvVectorPathBoundingBox; virtual;
       procedure GetIntersectionPointsWithSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList); virtual;
       function GetCompareHorizontalLowestXCoordinate:TpvDouble;
       procedure Transform(const aMatrix:TpvVectorPathMatrix); virtual;
      public
       property BoundingBox:TpvVectorPathBoundingBox read GetBoundingBox;
      published
       property Type_:TpvVectorPathSegmentType read fType;
     end;

     { TpvVectorPathSegmentLine }

     TpvVectorPathSegmentLine=class(TpvVectorPathSegment)
      public
       Points:array[0..1] of TpvVectorPathVector;
      protected
       procedure GetIntersectionPointsWithLineSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList); override;
       procedure GetIntersectionPointsWithQuadraticCurveSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList); override;
       procedure GetIntersectionPointsWithCubicCurveSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList); override;
      public
       constructor Create; overload; override;
       constructor Create(const aP0,aP1:TpvVectorPathVector); overload;
       procedure Assign(const aSegment:TpvVectorPathSegment); override;
       function Clone:TpvVectorPathSegment; override;
       function GetBoundingBox:TpvVectorPathBoundingBox; override;
       procedure GetIntersectionPointsWithSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList); override;
       procedure Transform(const aMatrix:TpvVectorPathMatrix); override;
     end;

     { TpvVectorPathSegmentMetaWindingSettingLine }
     TpvVectorPathSegmentMetaWindingSettingLine=class(TpvVectorPathSegmentLine)
      private
       fWinding:TpvInt32;
      public
       constructor Create; overload; override;
       constructor Create(const aP0,aP1:TpvVectorPathVector;const aWinding:TpvInt32); reintroduce; overload;
      published
       property Winding:TpvInt32 read fWinding write fWinding;
     end;

     { TpvVectorPathSegmentQuadraticCurve }

     TpvVectorPathSegmentQuadraticCurve=class(TpvVectorPathSegment)
      public
       Points:array[0..2] of TpvVectorPathVector;
      protected
       procedure GetIntersectionPointsWithLineSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList); override;
       procedure GetIntersectionPointsWithQuadraticCurveSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList); override;
       procedure GetIntersectionPointsWithCubicCurveSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList); override;
      public
       constructor Create; overload; override;
       constructor Create(const aP0,aP1,aP2:TpvVectorPathVector); overload;
       procedure Assign(const aSegment:TpvVectorPathSegment); override;
       function Clone:TpvVectorPathSegment; override;
       function GetBoundingBox:TpvVectorPathBoundingBox; override;
       procedure GetIntersectionPointsWithSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList); override;
       procedure Transform(const aMatrix:TpvVectorPathMatrix); override;
     end;

     { TpvVectorPathSegmentCubicCurve }

     TpvVectorPathSegmentCubicCurve=class(TpvVectorPathSegment)
      public
       Points:array[0..3] of TpvVectorPathVector;
      protected
       procedure GetIntersectionPointsWithLineSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList); override;
       procedure GetIntersectionPointsWithQuadraticCurveSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList); override;
       procedure GetIntersectionPointsWithCubicCurveSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList); override;
      public
       constructor Create; overload; override;
       constructor Create(const aP0,aP1,aP2,aP3:TpvVectorPathVector); overload;
       procedure Assign(const aSegment:TpvVectorPathSegment); override;
       function Clone:TpvVectorPathSegment; override;
       function GetBoundingBox:TpvVectorPathBoundingBox; override;
       procedure GetIntersectionPointsWithSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList); override;
       procedure Transform(const aMatrix:TpvVectorPathMatrix); override;
     end;

     { TpvVectorPathSegments }

     TpvVectorPathSegments=class(TpvObjectGenericList<TpvVectorPathSegment>)
      public
       procedure SortHorizontal;
     end;

     // Which segment stands in for another one, which is what folding the repeated meta winding
     // setting lines onto one of each kind is kept in.
     TpvVectorPathSegmentHashMap=TpvHashMap<TpvVectorPathSegment,TpvVectorPathSegment>;

     // Which cell already wrote out a given list of indirect segments, so that the next one with
     // the same list can point at it instead of writing its own copy.
     // Where a given list of indirect segments already lies: the cell that wrote it in the upper
     // half and where in that cell's list the run starts in the lower half.
     TpvVectorPathIndirectListHashMap=TpvHashMap<TpvUInt64,TpvUInt64>;

     { TpvVectorPathContour }

     TpvVectorPathContour=class
      private
       fSegments:TpvVectorPathSegments;
       fClosed:boolean;
      public
       constructor Create; reintroduce; overload;
       constructor Create(const aContour:TpvVectorPathContour); overload;
       destructor Destroy; override;
       procedure Assign(const aContour:TpvVectorPathContour);
       function Clone:TpvVectorPathContour;
       function GetBoundingBox:TpvVectorPathBoundingBox;
       procedure Transform(const aMatrix:TpvVectorPathMatrix);
      published
       property Segments:TpvVectorPathSegments read fSegments;
       property Closed:boolean read fClosed write fClosed;
     end;

     TpvVectorPathContours=TpvObjectGenericList<TpvVectorPathContour>;

     TpvVectorPath=class;

     { TpvVectorPathShape }

     TpvVectorPathShape=class
      private
       fFillRule:TpvVectorPathFillRule;
       fContours:TpvVectorPathContours;
       fResolution:TpvInt32;
       fBoundingBoxExtent:TpvDouble;
       fCubicToQuadraticTolerance:TpvDouble;
       fAccelerationStructure:TpvVectorPathAccelerationStructure;
       fCountBands:TpvInt32;
       fDuplicateSharing:TpvVectorPathDuplicateSharing;
      public
       constructor Create; reintroduce; overload;
       constructor Create(const aVectorPathShape:TpvVectorPathShape); reintroduce; overload;
       constructor Create(const aVectorPath:TpvVectorPath); reintroduce; overload;
       destructor Destroy; override;
       procedure Assign(const aVectorPathShape:TpvVectorPathShape); overload;
       procedure Assign(const aVectorPath:TpvVectorPath); overload;
       function Clone:TpvVectorPathShape;
       function GetBoundingBox:TpvVectorPathBoundingBox;
       procedure Transform(const aMatrix:TpvVectorPathMatrix);
       procedure ConvertCubicCurvesToQuadraticCurves(const aPixelRatio:TpvDouble=1.0);
       procedure ConvertCurvesToLines(const aPixelRatio:TpvDouble=1.0);
       function GetSignedDistance(const aX,aY,aScale:TpvDouble;out aInsideOutsideSign:TpvInt32):TpvDouble;
       function GetBeginEndPoints:TpvVectorPathVectors;
       procedure GetSegmentIntersectionPoints(const aIntersectionPoints:TpvVectorPathVectorList);
      public
       type { TCachedSegmentHead }
            // What a segment is besides its points: which kind it is, and the winding a meta
            // winding setting line carries.
            TCachedSegmentHead=packed record
             Type_:TpvUInt32;
             Winding:TpvInt32;
            end;
            { TCachedContourHead }
            TCachedContourHead=packed record
             Closed:TpvUInt32;
             CountSegments:TpvUInt32;
            end;
      public
       class function CountSegmentPoints(const aType:TpvVectorPathSegmentType):TpvSizeInt; static;
       // One segment at a time, which nothing writes with any more since the blocks below are what
       // keeps loading down to milliseconds, but which is the readable form of the same thing.
       class procedure SaveSegmentToStream(const aStreamIO:TpvStreamIO;const aSegment:TpvVectorPathSegment); static;
       class function LoadSegmentFromStream(const aStreamIO:TpvStreamIO):TpvVectorPathSegment; static;
       // A run of segments onto a stream and back, as two blocks rather than a number at a time,
       // which both this and the GPU shape built from it are written with.
       class procedure SaveSegmentsToStream(const aStreamIO:TpvStreamIO;const aSegments:TpvVectorPathSegments); static;
       class procedure LoadSegmentsFromStream(const aStreamIO:TpvStreamIO;const aSegments:TpvVectorPathSegments); static;
       // The whole shape: its fill rule, the settings it carries to the GPU, and its contours.
       procedure SaveToStream(const aStreamIO:TpvStreamIO);
       procedure LoadFromStream(const aStreamIO:TpvStreamIO);
      published
       property FillRule:TpvVectorPathFillRule read fFillRule write fFillRule;
       property Contours:TpvVectorPathContours read fContours;
       property Resolution:TpvInt32 read fResolution write fResolution;
       property BoundingBoxExtent:TpvDouble read fBoundingBoxExtent write fBoundingBoxExtent;
       // How far a quadratic curve may stray from the cubic one it replaces when this shape goes
       // to the GPU, in the units of the shape itself. One unit is right whenever one unit is
       // about one pixel on screen; a shape whose units are much smaller than a pixel, which is
       // what a viewBox of a few units gives, needs a correspondingly smaller value.
       property CubicToQuadraticTolerance:TpvDouble read fCubicToQuadraticTolerance write fCubicToQuadraticTolerance;
       // Which structure is built for this shape when it goes to the GPU.
       property AccelerationStructure:TpvVectorPathAccelerationStructure read fAccelerationStructure write fAccelerationStructure;
       // How many Slug bands per axis, or zero to let it pick from the curve count the way Slug
       // does. Means nothing for the grid, which goes by Resolution.
       property CountBands:TpvInt32 read fCountBands write fCountBands;
       // How far repetition in the acceleration structure is folded away.
       property DuplicateSharing:TpvVectorPathDuplicateSharing read fDuplicateSharing write fDuplicateSharing;
     end;

     { TpvVectorPath }

     TpvVectorPath=class
      private
       fShape:TpvVectorPathShape;
       fCommands:TpvVectorPathCommandList;
       fFillRule:TpvVectorPathFillRule;
       fStartPointSeen:Boolean;
      public
       constructor Create; reintroduce;
       constructor CreateFromSVGPath(const aCommands:TpvRawByteString);
       constructor CreateFromShape(const aShape:TpvVectorPathShape);
       destructor Destroy; override;
       procedure Assign(const aFrom:TpvVectorPath); overload;
       procedure Assign(const aShape:TpvVectorPathShape); overload;
       // Blends between two paths, which only works when both are made of the same sequence of
       // commands. Where they are not, the result jumps from one to the other halfway through,
       // since there is no sensible way to grow or drop a command. Returns whether it could
       // really interpolate, so that a caller can tell the two cases apart.
       function InterpolateFrom(const aFrom,aTo:TpvVectorPath;const aFactor:TpvDouble):boolean;
       function MoveTo(const aX,aY:TpvDouble):TpvVectorPath; overload;
       function MoveTo(const aPosition:TpvVectorPathVector):TpvVectorPath; overload;
       function LineTo(const aX,aY:TpvDouble):TpvVectorPath; overload;
       function LineTo(const aPosition:TpvVectorPathVector):TpvVectorPath; overload;
       function QuadraticCurveTo(const aCX,aCY,aAX,aAY:TpvDouble):TpvVectorPath; overload;
       function QuadraticCurveTo(const aControlPoint,aAnchorPoint:TpvVectorPathVector):TpvVectorPath; overload;
       function CubicCurveTo(const aC0X,aC0Y,aC1X,aC1Y,aAX,aAY:TpvDouble):TpvVectorPath; overload;
       function CubicCurveTo(const aControlPoint0,aControlPoint1,aAnchorPoint:TpvVectorPathVector):TpvVectorPath; overload;
       function ArcTo(const aOrigin,aRadius:TpvVectorPathVector;const aStartAngle,aEndAngle:TpvDouble;const aCounterClockwise:boolean;const aRotation:TpvDouble):TpvVectorPath; overload;
       function ArcTo(const aOriginX,aOriginY,aRadiusX,aRadiusY:TpvDouble;const aStartAngle,aEndAngle:TpvDouble;const aCounterClockwise:boolean;const aRotation:TpvDouble):TpvVectorPath; overload;
       function Arc(const aCenterX,aCenterY,aRadius,aStartAngle,aEndAngle:TpvDouble;const aCounterClockwise:boolean):TpvVectorPath; overload;
       function Arc(const aCenter:TpvVectorPathVector;const aRadius,aStartAngle,aEndAngle:TpvDouble;const aCounterClockwise:boolean):TpvVectorPath; overload;
       function Ellipse(const aCenterX,aCenterY,aRadiusX,aRadiusY:TpvDouble):TpvVectorPath; overload;
       function Ellipse(const aCenter,aRadius:TpvVectorPathVector):TpvVectorPath; overload;
       function Circle(const aCenterX,aCenterY,aRadius:TpvDouble):TpvVectorPath; overload;
       function Circle(const aCenter:TpvVectorPathVector;const aRadius:TpvDouble):TpvVectorPath; overload;
       function Rectangle(const aCenterX,aCenterY,aBoundsX,aBoundsY:TpvDouble):TpvVectorPath; overload;
       function Rectangle(const aCenter,aBounds:TpvVectorPathVector):TpvVectorPath; overload;
       function RoundedRectangle(const aCenterX,aCenterY,aBoundsX,aBoundsY,aRadiusTopLeft,aRadiusTopRight,aRadiusBottomLeft,aRadiusBottomRight:TpvDouble):TpvVectorPath; overload;
       function RoundedRectangle(const aCenter,aBounds:TpvVectorPathVector;const aRadiusTopLeft,aRadiusTopRight,aRadiusBottomLeft,aRadiusBottomRight:TpvDouble):TpvVectorPath; overload;
       function RoundedRectangle(const aCenterX,aCenterY,aBoundsX,aBoundsY,aRadius:TpvDouble):TpvVectorPath; overload;
       function RoundedRectangle(const aCenter,aBounds:TpvVectorPathVector;const aRadius:TpvDouble):TpvVectorPath; overload;
       function Close:TpvVectorPath;
       function GetShape:TpvVectorPathShape;
       procedure Transform(const aMatrix:TpvVectorPathMatrix);
      published
       property FillRule:TpvVectorPathFillRule read fFillRule write fFillRule;
       property Commands:TpvVectorPathCommandList read fCommands;
     end;

     PpvVectorPathStrokeCapMode=^TpvVectorPathStrokeCapMode;
     TpvVectorPathStrokeCapMode=
      (
       Butt=0,
       Round,
       Square
      );

     PpvVectorPathStrokeJoinMode=^TpvVectorPathStrokeJoinMode;
     TpvVectorPathStrokeJoinMode=
      (
       Bevel=0,
       Miter,
       MiterRevert,
       MiterRound,
       Round
      );

     PpvVectorPathStrokeInnerJoinMode=^TpvVectorPathStrokeInnerJoinMode;
     TpvVectorPathStrokeInnerJoinMode=
      (
       Bevel=0,
       Jag,
       Miter,
       Round
      );

     TpvVectorPathStrokeDashArray=array of TpvDouble;

     { TpvVectorPathStrokeSettings }

     TpvVectorPathStrokeSettings=record
      public
       Width:TpvDouble;
       CapMode:TpvVectorPathStrokeCapMode;
       JoinMode:TpvVectorPathStrokeJoinMode;
       InnerJoinMode:TpvVectorPathStrokeInnerJoinMode;
       MiterLimit:TpvDouble;
       InnerMiterLimit:TpvDouble;
       DashArray:TpvVectorPathStrokeDashArray;
       DashOffset:TpvDouble;
      public
       class function CreateDefault:TpvVectorPathStrokeSettings; static;
       procedure Assign(const aFrom:TpvVectorPathStrokeSettings);
       function HasDashes:boolean;
     end;

     PpvVectorPathStrokeSettings=^TpvVectorPathStrokeSettings;

     { TpvVectorPathStroker }

     // Converts the outline of a stroked path into a fillable path, after the manner of the
     // Anti-Grain Geometry stroke converter: every flattened polyline is walked forwards and
     // backwards, with a join emitted at every interior vertex and a cap at every end. The
     // result is always meant to be filled with the non-zero rule.
     TpvVectorPathStroker=class
      public
       type TLinePoint=record
             Position:TpvVectorPathVector;
             Distance:TpvDouble;
            end;
            PLinePoint=^TLinePoint;
            TLinePoints=array of TLinePoint;
      private
       fSettings:TpvVectorPathStrokeSettings;
       fApproximationScale:TpvDouble;
       fLinePoints:TLinePoints;
       fCountLinePoints:TpvSizeInt;
       fDashedLinePoints:TLinePoints;
       fCountDashedLinePoints:TpvSizeInt;
       fDashes:TpvVectorPathStrokeDashArray;
       fDashFlags:array of boolean;
       fOutputShape:TpvVectorPathShape;
       fOutputContour:TpvVectorPathContour;
       fFirstPosition:TpvVectorPathVector;
       fLastPosition:TpvVectorPathVector;
       fHasLastPosition:boolean;
       fFirst:boolean;
       fWidth:TpvDouble;
       fWidthAbsolute:TpvDouble;
       fWidthEpsilon:TpvDouble;
       fWidthSign:TpvInt32;
       procedure AddLinePoint(const aPosition:TpvVectorPathVector);
       procedure AddDashedLinePoint(const aPosition:TpvVectorPathVector);
       procedure Point(const aX,aY:TpvDouble);
       procedure CloseOutputContour;
       procedure DoArc(const aX,aY,aDX1,aDY1,aDX2,aDY2:TpvDouble);
       procedure DoMiter(const aP1X,aP1Y,aP2X,aP2Y,aP3X,aP3Y,aDX1,aDY1,aDX2,aDY2:TpvDouble;
                         const aJoinMode:TpvVectorPathStrokeJoinMode;
                         const aMiterLimit,aBevel:TpvDouble);
       procedure Join(const aIndex1,aIndex2,aIndex3,aDistanceIndex1,aDistanceIndex2:TpvSizeInt;
                      const aLinePoints:TLinePoints);
       procedure Cap(const aIndex1,aIndex2,aDistanceIndex:TpvSizeInt;
                     const aLinePoints:TLinePoints);
       procedure MakeDashes;
       procedure ConvertStrokeToPolygon(const aLinePoints:TLinePoints;const aCountLinePoints:TpvSizeInt;const aClosed:boolean);
       procedure ProcessDashes(const aLinePoints:TLinePoints;const aCountLinePoints:TpvSizeInt;const aClosed:boolean);
       procedure FlushLine(const aClosed:boolean);
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
       // Strokes the already flattened contours of aShape and returns a freshly created shape
       // holding the outline, which the caller owns. aApproximationScale is the scale at which
       // the result will be rasterized, so that arcs in round joins and caps get enough
       // subdivisions, and nothing beyond that.
       function Stroke(const aShape:TpvVectorPathShape;
                       const aSettings:TpvVectorPathStrokeSettings;
                       const aApproximationScale:TpvDouble=1.0):TpvVectorPathShape;
      published
       property ApproximationScale:TpvDouble read fApproximationScale write fApproximationScale;
     end;

     TpvVectorPathGPUSegmentData=packed record // 32 bytes per segment
      public
       const TypeUnknown=0;
             TypeLine=1;
             TypeQuadraticCurve=2;
             TypeMetaWindingSettingLine=3;
      public
       // uvec4 typeWindingPoint0 - Begin
       Type_:TpvUInt32;
       Winding:TpvInt32;
       Point0:TpvVector2;
       // uvec4 typeWindingPoint0 - End
       // vec4 point1Point2 - Begin
       Point1:TpvVector2;
       Point2:TpvVector2;
       // vec4 point1Point2 - End
     end;

     PpvVectorPathGPUSegmentData=^TpvVectorPathGPUSegmentData;

     TpvVectorPathGPUSegmentDataArray=array of TpvVectorPathGPUSegmentData;

     TpvVectorPathGPUIndirectSegmentData=TpvUInt32; // 4 bytes per indirect segment

     PpvVectorPathGPUIndirectSegmentData=^TpvVectorPathGPUIndirectSegmentData;

     TpvVectorPathGPUIndirectSegmentDataArray=array of TpvVectorPathGPUIndirectSegmentData;

     TpvVectorPathGPUGridCellData=packed record // 8 bytes per grid cell
      // uvec2 Begin
      StartIndirectSegmentIndex:TpvUInt32;
      CountIndirectSegments:TpvUInt32;
      // uvec2 End
     end;

     PpvVectorPathGPUGridCellData=^TpvVectorPathGPUGridCellData;

     TpvVectorPathGPUGridCellDataArray=array of TpvVectorPathGPUGridCellData;

     TpvVectorPathGPUShapeData=packed record // 32 bytes per segment
      // vec4 minMax - Begin
      Min:TpvVector2;
      Max:TpvVector2;
      // vec4 minMax - End
      // uvec4 flagsStartGridCellIndexGridSize - Begin
      Flags:TpvUInt32; // Bit 0 = even-odd / non-zero fill rule selector (1 => even-odd, 0 => non-zero)
      StartGridCellIndex:TpvUInt32;
      GridSizeX:TpvUInt32;
      GridSizeY:TpvUInt32;
      // uvec4 flagsStartGridCellIndexGridSize - End
     end;

     PpvVectorPathGPUShapeData=^TpvVectorPathGPUShapeData;

     TpvVectorPathGPUShapeDataArray=array of TpvVectorPathGPUShapeData;

     TpvVectorPathGPUBufferPool=class;

     { TpvVectorPathGPUShape }

     TpvVectorPathGPUShape=class
      public
       const CoordinateExtents=1.41421356237; // sqrt(2.0)
       type { TGridCell }
            TGridCell=class
             public
              type TKind=
                    (
                     Empty,
                     HasContent
                    );
             private
              fKind:TKind;
              fVectorPathGPUShape:TpvVectorPathGPUShape;
              fBoundingBox:TpvVectorPathBoundingBox;
              fExtendedBoundingBox:TpvVectorPathBoundingBox;
              fSegments:TpvVectorPathSegments;
              // The meta winding setting lines this cell made, which it owns until the shape has
              // taken them over. They used to go straight into the segments of the shape, which is
              // the one thing that kept two cells from being built at the same time.
              fMetaWindingSettingLines:TpvVectorPathSegments;
             public
              // aOutlineSegments are the segments of the outline itself, without the meta winding
              // setting lines the cells add as they are built. Handed in rather than taken from
              // the shape, because the shape collects those meta lines of every cell built so far
              // and walking them again for every further cell is what made a document of a few
              // hundred outlines take twenty seconds to build.
              constructor Create(const aVectorPathGPUShape:TpvVectorPathGPUShape;const aOutlineSegments:TpvVectorPathSegments;const aBoundingBox:TpvVectorPathBoundingBox); reintroduce; overload;
              // An empty one, for the Slug bands, which fill it themselves and need none of the
              // work the constructor above does for a grid cell.
              constructor Create(const aVectorPathGPUShape:TpvVectorPathGPUShape;const aBoundingBox:TpvVectorPathBoundingBox); reintroduce; overload;
              destructor Destroy; override;
             public
              // The segments a point inside this cell has to be tested against, which is what the
              // shader walks, including the meta winding setting lines that carry the winding
              // everything to the left of the cell contributes.
              property Segments:TpvVectorPathSegments read fSegments;
              property BoundingBox:TpvVectorPathBoundingBox read fBoundingBox;
              // The bounding box widened by the coordinate extents, which is the area a segment
              // has to touch to be taken into this cell.
              property ExtendedBoundingBox:TpvVectorPathBoundingBox read fExtendedBoundingBox;
            end;
            TGridCells=TpvObjectGenericList<TGridCell>;
      private
       fBufferPool:TpvVectorPathGPUBufferPool;
       fSourceVectorPathShape:TpvVectorPathShape;
       fVectorPathShape:TpvVectorPathShape;
       fBoundingBox:TpvVectorPathBoundingBox;
       fResolution:TpvInt32;
       fAccelerationStructure:TpvVectorPathAccelerationStructure;
       // How many bands the Slug structure is cut into per axis. Zero lets it pick for itself, the
       // way Slug does, from how many curves the outline has.
       fCountBands:TpvInt32;
       fCountHorizontalBands:TpvInt32;
       fCountVerticalBands:TpvInt32;
       fDuplicateSharing:TpvVectorPathDuplicateSharing;
       fSegments:TpvVectorPathSegments;
       fSegmentDynamicAABBTree:TpvVectorPathBVHDynamicAABBTree;
       fGridCells:TGridCells;
       fGeneration:TPasMPUInt64; // gets the lastest generation from the buffer pool when created or updated, since simple self-increment is not sufficient, when a new TpvVectorPathGPUShape gets the same pointer as a previously removed shape
       fSegmentBufferRange:TpvBufferRangeAllocator.TBufferRange;
       fIndirectSegmentBufferRange:TpvBufferRangeAllocator.TBufferRange;
       fGridCellBufferRange:TpvBufferRangeAllocator.TBufferRange;
       fShapeIndex:TpvInt32;
       fBoundingBoxExtent:TpvDouble;
       // What the rows of grid cells are built over, or nil to build them one after the other on
       // the calling thread. Not owned here.
       fPasMPInstance:TPasMP;
      private
       procedure ClearAndInvalidateBufferRanges;
       procedure UpdateBufferPool;
       // Builds the rows of grid cells from aFromIndex to aToIndex. One row is independent of
       // every other, so this is what the rows are spread over the cores with.
       procedure BuildGridCellRowsJob(const aJob:PPasMPJob;const aThreadIndex:TPasMPInt32;const aData:pointer;const aFromIndex,aToIndex:TPasMPNativeInt);
       // Builds the two sets of bands of the Slug structure out of the outline segments, in place
       // of the grid. They go into the very same list of cells, the horizontal ones first and the
       // vertical ones after them, so that everything downstream stays as it is.
       procedure BuildSlugBands(const aOutlineSegments:TpvVectorPathSegments);
       // Folds the meta winding setting lines of all cells onto one of each kind and hands those
       // to the shape, with every cell left pointing at the one that was kept.
       procedure ShareMetaWindingSettingLines;
      public
       // One height at which the winding to the right of a segment changes, and by how much. A run
       // of a segment that only ever moves one way in the height counts for every height from its
       // lower end up to but not including its upper one, so it opens at the one and closes at the
       // other.
       type TWindingChangeEvent=record
             Y:TpvDouble;
             Delta:TpvInt32;
            end;
            PWindingChangeEvent=^TWindingChangeEvent;
            // Three runs at most, which is what a cubic curve can be cut into, and two events each.
            TWindingChangeEvents=array[0..5] of TWindingChangeEvent;
            TWindingChangeEventDynamicArray=TpvDynamicArray<TWindingChangeEvent>;
      public
       // The winding at a point, by casting a ray to the left and counting the crossings of the
       // given segments. Public because it is the one definition of what the grid has to add up
       // to, which is what makes it checkable from the outside.
       class function ComputeWindingAtPosition(const aSegments:TpvVectorPathSegments;const aX,aY:TpvDouble):TpvInt32; static;
       // Where the given segment changes how often a scan line crosses it, and by how much, which
       // is where the starting winding of a cell to the right of it changes with it: where the
       // segment begins, where it ends and where it turns around. The bounding box carries only
       // the lowest and the highest of them, so a segment that turns around hides whichever of its
       // two ends lies between the two, and a cell that goes by the box alone takes one winding
       // for a span the winding really changes inside of. Adding these up in order of height gives
       // the winding of a whole span at once, rather than asking every segment again for every
       // span, which is what the cells used to do and what made them cost the square of how many
       // segments feed them.
       class function GetWindingChangeEvents(const aSegment:TpvVectorPathSegment;out aEvents:TWindingChangeEvents):TpvSizeInt; static;
      public
       // aPasMPInstance spreads the rows of grid cells over the cores, which is where nearly all of
       // the time of building a shape goes. Left out, they are built one after the other.
       constructor Create(const aBufferPool:TpvVectorPathGPUBufferPool;const aVectorPathShape:TpvVectorPathShape;const aResolution:TpvInt32=32;const aBoundingBoxExtent:TpvDouble=4.0;const aPasMPInstance:TPasMP=nil;const aAccelerationStructure:TpvVectorPathAccelerationStructure=TpvVectorPathAccelerationStructure.Grid;const aCountBands:TpvInt32=0;const aDuplicateSharing:TpvVectorPathDuplicateSharing=TpvVectorPathDuplicateSharing.EqualLists); reintroduce; overload;
       // Takes everything off a stream instead of working it out, which is what a cache is for.
       constructor Create(const aBufferPool:TpvVectorPathGPUBufferPool;const aVectorPathShape:TpvVectorPathShape;const aStreamIO:TpvStreamIO;const aPasMPInstance:TPasMP=nil); reintroduce; overload;
       destructor Destroy; override;
       procedure Update;
       // The whole acceleration structure as it stands: the settings, the segments, and which of
       // them each cell or band holds. Nothing here is worked out again on the way back in.
       procedure SaveToStream(const aStreamIO:TpvStreamIO);
       procedure LoadFromStream(const aStreamIO:TpvStreamIO);
      public
       // The bounding box already widened by the extent, so a caller that draws a quad over it
       // covers the whole antialiased edge and not just the geometry.
       property BoundingBox:TpvVectorPathBoundingBox read fBoundingBox;
       // Every segment the grid cells point into: the outline, with each of its contours closed
       // where it was left open, followed by the meta winding setting lines the cells added. This
       // and not the outline it was built from is what the winding of this shape is defined by,
       // which is what makes that checkable from the outside.
       property Segments:TpvVectorPathSegments read fSegments;
       // The grid the shader looks a point up in, row by row, and how many cells it has per axis.
       property GridCells:TGridCells read fGridCells;
       property Resolution:TpvInt32 read fResolution;
       // Which structure the cells above actually hold: a grid, or the two sets of Slug bands with
       // the horizontal ones first and the vertical ones after them.
       property AccelerationStructure:TpvVectorPathAccelerationStructure read fAccelerationStructure;
       property CountHorizontalBands:TpvInt32 read fCountHorizontalBands;
       property CountVerticalBands:TpvInt32 read fCountVerticalBands;
       // The outline this shape was built from, after the cubic curves were turned into quadratic
       // ones, which is what the cells hold their segments of.
       property VectorPathShape:TpvVectorPathShape read fVectorPathShape;
       // The index into the shapes buffer, which is what a shader needs in order to find this
       // shape, and -1 as long as the shape has none.
       property ShapeIndex:TpvInt32 read fShapeIndex;
     end;

     TpvVectorPathGPUShapes=array of TpvVectorPathGPUShape;

     TpvVectorPathIndexHashMap=TpvHashMap<TpvVectorPathShape,TpvInt32>;

     TpvVectorPathGPUBufferPoolShapeFreeList=TpvDynamicStack<TpvInt32>;

     { TpvVectorPathGPUBufferPool }

     TpvVectorPathGPUBufferPool=class     
      public
       type { TState }
            TState=class
             private

              // The buffer pool this state belongs to
              fBufferPool:TpvVectorPathGPUBufferPool;

              // The device this state is created for
              fDevice:TpvVulkanDevice; 

              // Generation of this state for tracking updates
              fGeneration:TPasMPUInt64;

              // Buffers (have size properties, no extra size variables needed)
              fSegmentsBuffer:TpvVulkanBuffer;
              fIndirectSegmentsBuffer:TpvVulkanBuffer;
              fGridCellsBuffer:TpvVulkanBuffer;
              fShapesBuffer:TpvVulkanBuffer;

              // Shape generation buffer
              fShapes:TpvVectorPathGPUShapes;
              fShapeGenerations:TpvUInt64DynamicArray;

              // Descriptor set for the 4 shared buffers
              fDescriptorPool:TpvVulkanDescriptorPool;
              fDescriptorSet:TpvVulkanDescriptorSet;

             public
              constructor Create(const aBufferPool:TpvVectorPathGPUBufferPool); reintroduce;
              destructor Destroy; override;
              procedure Update;
             published
              property SegmentsBuffer:TpvVulkanBuffer read fSegmentsBuffer;
              property IndirectSegmentsBuffer:TpvVulkanBuffer read fIndirectSegmentsBuffer;
              property GridCellsBuffer:TpvVulkanBuffer read fGridCellsBuffer;
              property ShapesBuffer:TpvVulkanBuffer read fShapesBuffer;
              property DescriptorSet:TpvVulkanDescriptorSet read fDescriptorSet;
            end;
            TStates=array[0..1] of TState; // Double-buffered upload-wise, but single-buffered in terms of actual data,
                                           // so that only one state is active at a time and so that no synchronization is 
                                           // needed in order to read from the buffers while uploading new data to the other buffers  
      private

       // The device this buffer pool is created for
       fDevice:TpvVulkanDevice;

       // What the shapes made here build their grids over, or nil to build them one row after the
       // other. Not owned here, and set from outside, because only the application knows which one
       // its own work runs on.
       fPasMPInstance:TPasMP;

       // Array of GPU shapes, shape index / ID wise
       fGPUShapes:TpvVectorPathGPUShapes;

       // Hash map for mapping TpvVectorPathShape to shape index / shape ID
       fShapeIndexHashMap:TpvVectorPathIndexHashMap;

       // Generation for tracking updates 
       fGeneration:TPasMPUInt64;

       // Descriptor set layout for the 4 shared buffers
       fDescriptorSetLayout:TpvVulkanDescriptorSetLayout;

       // States with double-buffering for uploads and rendering for synchronization avoidance at cost of double memory usage
       fStates:TStates;
       fActiveStateIndex:TPasMPInt32;

       // The segment buffer holds all segments for all shapes
       fSegments:TpvVectorPathGPUSegmentDataArray;
       fSegmentsAllocator:TpvBufferRangeAllocator;

       // The indirect segments buffer holds all indirect segment indices for all shapes
       fIndirectSegments:TpvVectorPathGPUIndirectSegmentDataArray;
       fIndirectSegmentsAllocator:TpvBufferRangeAllocator;

       // The grid cells buffer holds all grid cells for all shapes
       fGridCells:TpvVectorPathGPUGridCellDataArray;
       fGridCellsAllocator:TpvBufferRangeAllocator;
       
       // The shapes buffer holds all shape metadata
       fShapes:TpvVectorPathGPUShapeDataArray;
       fFreeShapeIndices:TpvVectorPathGPUBufferPoolShapeFreeList;
       fShapeIndexCounter:TpvInt32;
       // Note: Shape metadata uses fGPUShapes array (direct indexing, no allocator needed), instead 
       // a free list is used for reusing shape indices

       fTransferQueue:TpvVulkanQueue;
       fTransferCommandPool:TpvVulkanCommandPool;
       fTransferCommandBuffer:TpvVulkanCommandBuffer;
       fTransferFence:TpvVulkanFence;

       fFragmentationFactorThreshold:TpvDouble;

       function GetActiveState:TpvVectorPathGPUBufferPool.TState;

       function GetDescriptorSet:TpvVulkanDescriptorSet;

       function CalculateFragmentationFactor:TpvDouble;
       
      public

       constructor Create(const aDevice:TpvVulkanDevice); reintroduce;
       destructor Destroy; override;

       procedure Update;

       function GetOrCreateShape(const aShape:TpvVectorPathShape):TpvVectorPathGPUShape; overload;
       // The same, but with the acceleration structure taken off a stream rather than worked out.
       function GetOrCreateShape(const aShape:TpvVectorPathShape;const aStreamIO:TpvStreamIO):TpvVectorPathGPUShape; overload;
       // The one already there for this shape, or nil, which is what writing a cache needs.
       function GetShapeOf(const aShape:TpvVectorPathShape):TpvVectorPathGPUShape;
       procedure RemoveShape(const aShape:TpvVectorPathShape);
       procedure UpdateShape(const aShape:TpvVectorPathShape);

       procedure Defragment(const aForce:boolean=false);

       // Reads the four buffers back off the device and holds them against the arrays they were
       // filled from, which is the one way to tell a shape that was built wrong from one that was
       // merely handed over wrong. Returns how many entries disagree and puts the first few of
       // each kind into aReport. Meant for looking into a problem, not for a frame: it waits for
       // the device on every one of the four.
       function VerifyUploadedBuffers(out aReport:TpvUTF8String;const aMaximalReportsPerBuffer:TpvSizeInt=8):TpvSizeInt;

      published

       property DescriptorSet:TpvVulkanDescriptorSet read GetDescriptorSet;
       
       property DescriptorSetLayout:TpvVulkanDescriptorSetLayout read fDescriptorSetLayout;

       // Set this and the shapes made here spread the rows of their grids over the cores, which is
       // where nearly all of the time of registering a shape goes.
       property PasMPInstance:TPasMP read fPasMPInstance write fPasMPInstance;

       property FragmentationFactorThreshold:TpvDouble read fFragmentationFactorThreshold write fFragmentationFactorThreshold;

     end;

// Turns a pictorial dash pattern into a dash array. A run of non-space characters is a dash and
// a run of spaces is a gap, each character worth aStepSize, so that '--  -  ' draws a long dash,
// a gap, a short dash and a gap. This is the shorthand the old vector editor used; the SVG
// stroke-dasharray form goes straight into TpvVectorPathStrokeSettings.DashArray instead.
function pvVectorPathStrokePatternToDashArray(const aPattern:TpvRawByteString;
                                              const aStepSize:TpvDouble=1.0):TpvVectorPathStrokeDashArray;

implementation

function Clamp(const Value,MinValue,MaxValue:TpvDouble):TpvDouble;
begin
 if Value<=MinValue then begin
  result:=MinValue;
 end else if Value>=MaxValue then begin
  result:=MaxValue;
 end else begin
  result:=Value;
 end;
end;

{ TpvVectorPathVector }

constructor TpvVectorPathVector.Create(const aValue:TpvDouble);
begin
 x:=aValue;
 y:=aValue;
end;

constructor TpvVectorPathVector.Create(const aX,aY:TpvDouble);
begin
 x:=aX;
 y:=aY;
end;

function TpvVectorPathVector.Length:TpvDouble;
begin
 result:=sqrt(sqr(x)+sqr(y));
end;

function TpvVectorPathVector.LengthSquared:TpvDouble;
begin
 result:=sqr(x)+sqr(y);
end;

function TpvVectorPathVector.Distance(const b:TpvVectorPathVector):TpvDouble;
begin
 result:=(b-self).Length;
end;

function TpvVectorPathVector.DistanceSquared(const b:TpvVectorPathVector):TpvDouble;
begin
 result:=(b-self).LengthSquared;
end;

function TpvVectorPathVector.Direction:TpvDouble;
begin
 result:=ArcTan2(y,x);
end;

function TpvVectorPathVector.Normalize:TpvVectorPathVector;
var Len:TpvDouble;
begin
 Len:=Length;
 if IsZero(Len) then begin
  result.x:=0.0;
  result.y:=0.0;
 end else begin
  result.x:=x/Len;
  result.y:=y/Len;
 end;
end;

function TpvVectorPathVector.Minimum(const aRight:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=Min(x,aRight.x);
 result.y:=Min(y,aRight.y);
end;

function TpvVectorPathVector.Maximum(const aRight:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=Max(x,aRight.x);
 result.y:=Max(y,aRight.y);
end;

function TpvVectorPathVector.Dot(const aRight:TpvVectorPathVector):TpvDouble;
begin
 result:=(x*aRight.x)+(y*aRight.y);
end;

function TpvVectorPathVector.Cross(const aRight:TpvVectorPathVector):TpvDouble;
begin
 result:=(x*aRight.y)-(y*aRight.x);
end;

function TpvVectorPathVector.OrthoNormal:TpvVectorPathVector;
var Len:TpvDouble;
begin
 Len:=Length;
 if IsZero(Len) then begin
  result.x:=0.0;
  result.y:=0.0;
 end else begin
  result.x:=y/Len;
  result.y:=(-x)/Len;
 end;
end;

function TpvVectorPathVector.Lerp(const b:TpvVectorPathVector;const t:TpvDouble):TpvVectorPathVector;
begin
 result.x:=(x*(1.0-t))+(b.x*t);
 result.y:=(y*(1.0-t))+(b.y*t);
end;

function TpvVectorPathVector.ClampedLerp(const b:TpvVectorPathVector;const t:TpvDouble):TpvVectorPathVector;
begin
 if t<=0.0 then begin
  result:=self;
 end else if t>=1.0 then begin
  result:=b;
 end else begin
  result.x:=(x*(1.0-t))+(b.x*t);
  result.y:=(y*(1.0-t))+(b.y*t);
 end;
end;

class function TpvVectorPathVector.IsLeft(const a,b,c:TpvVectorPathVector):TpvDouble;
begin
 result:=((b.x*a.x)*(c.y*a.y))-((c.x*a.x)*(b.y*a.y));
end;

class operator TpvVectorPathVector.Implicit(const a:TpvVector2):TpvVectorPathVector;
begin
 result.x:=a.x;
 result.y:=a.y;
end;

class operator TpvVectorPathVector.Implicit(const a:TpvVectorPathVector):TpvVector2;
begin
 result.x:=a.x;
 result.y:=a.y;
end;

class operator TpvVectorPathVector.Implicit(const a:TpvVector2D):TpvVectorPathVector;
begin
 result.x:=a.x;
 result.y:=a.y;
end;

class operator TpvVectorPathVector.Implicit(const a:TpvVectorPathVector):TpvVector2D;
begin
 result.x:=a.x;
 result.y:=a.y;
end;

class operator TpvVectorPathVector.Explicit(const a:TpvVectorPathVector):TpvVector2;
begin
 result.x:=a.x;
 result.y:=a.y;
end;

class operator TpvVectorPathVector.Explicit(const a:TpvVector2):TpvVectorPathVector;
begin
 result.x:=a.x;
 result.y:=a.y;
end;

class operator TpvVectorPathVector.Explicit(const a:TpvVectorPathVector):TpvVector2D;
begin
 result.x:=a.x;
 result.y:=a.y;
end;

class operator TpvVectorPathVector.Explicit(const a:TpvVector2D):TpvVectorPathVector;
begin
 result.x:=a.x;
 result.y:=a.y;
end;

class operator TpvVectorPathVector.Equal(const a,b:TpvVectorPathVector):boolean;
begin
 result:=SameValue(a.x,b.x) and SameValue(a.y,b.y);
end;

class operator TpvVectorPathVector.NotEqual(const a,b:TpvVectorPathVector):boolean;
begin
 result:=(not SameValue(a.x,b.x)) or (not SameValue(a.y,b.y));
end;

class operator TpvVectorPathVector.Add(const a,b:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=a.x+b.x;
 result.y:=a.y+b.y;
end;

class operator TpvVectorPathVector.Subtract(const a,b:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=a.x-b.x;
 result.y:=a.y-b.y;
end;

class operator TpvVectorPathVector.Multiply(const a,b:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=a.x*b.x;
 result.y:=a.y*b.y;
end;

class operator TpvVectorPathVector.Multiply(const a:TpvVectorPathVector;const b:TpvDouble):TpvVectorPathVector;
begin
 result.x:=a.x*b;
 result.y:=a.y*b;
end;

class operator TpvVectorPathVector.Multiply(const a:TpvDouble;const b:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=a*b.x;
 result.y:=a*b.y;
end;

class operator TpvVectorPathVector.Divide(const a,b:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=a.x/b.x;
 result.y:=a.y/b.y;
end;

class operator TpvVectorPathVector.Divide(const a:TpvVectorPathVector;const b:TpvDouble):TpvVectorPathVector;
begin
 result.x:=a.x/b;
 result.y:=a.y/b;
end;

class operator TpvVectorPathVector.Negative(const a:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=-a.x;
 result.y:=-a.y;
end;

class operator TpvVectorPathVector.Positive(const a:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=a.x;
 result.y:=a.y;
end;

{ TpvVectorPathVectorList }

procedure TpvVectorPathVectorList.Sort;
type PByteArray=^TByteArray;
     TByteArray=array[0..$3fffffff] of TpvUInt8;
     PStackItem=^TStackItem;
     TStackItem=record
      Left,Right,Depth:TpvInt32;
     end;
var Left,Right,Depth,i,j,Middle,Size,Parent,Child,Pivot,iA,iB,iC:TpvSizeInt;
    StackItem:PStackItem;
    Stack:array[0..31] of TStackItem;
 function CompareItem(const a,b:TpvSizeInt):TpvSizeInt;
 var va,vb:TpvVectorPathVector;
 begin
  va:=Items[a];
  vb:=Items[b];
  result:=Sign(va.y-vb.y);
  if result=0 then begin
   result:=Sign(va.x-vb.x);
   if result=0 then begin
    result:=Sign(a-b);
   end;
  end;
 end;
begin
 begin
  if Count>1 then begin
   StackItem:=@Stack[0];
   StackItem^.Left:=0;
   StackItem^.Right:=Count-1;
   StackItem^.Depth:=IntLog2(Count) shl 1;
   inc(StackItem);
   while TpvPtrUInt(TpvPointer(StackItem))>TpvPtrUInt(TpvPointer(@Stack[0])) do begin
    dec(StackItem);
    Left:=StackItem^.Left;
    Right:=StackItem^.Right;
    Depth:=StackItem^.Depth;
    Size:=(Right-Left)+1;
    if Size<16 then begin
     // Insertion sort
     iA:=Left;
     iB:=iA+1;
     while iB<=Right do begin
      iC:=iB;
      while (iA>=Left) and
            (iC>=Left) and
            (CompareItem(iA,iC)>0) do begin
       Exchange(iA,iC);
       dec(iA);
       dec(iC);
      end;
      iA:=iB;
      inc(iB);
     end;
    end else begin
     if (Depth=0) or (TpvPtrUInt(TpvPointer(StackItem))>=TpvPtrUInt(TpvPointer(@Stack[high(Stack)-1]))) then begin
      // Heap sort
      i:=Size div 2;
      repeat
       if i>0 then begin
        dec(i);
       end else begin
        dec(Size);
        if Size>0 then begin
         Exchange(Left+Size,Left);
        end else begin
         break;
        end;
       end;
       Parent:=i;
       repeat
        Child:=(Parent*2)+1;
        if Child<Size then begin
         if (Child<(Size-1)) and (CompareItem(Left+Child,Left+Child+1)<0) then begin
          inc(Child);
         end;
         if CompareItem(Left+Parent,Left+Child)<0 then begin
          Exchange(Left+Parent,Left+Child);
          Parent:=Child;
          continue;
         end;
        end;
        break;
       until false;
      until false;
     end else begin
      // Quick sort width median-of-three optimization
      Middle:=Left+((Right-Left) shr 1);
      if (Right-Left)>3 then begin
       if CompareItem(Left,Middle)>0 then begin
        Exchange(Left,Middle);
       end;
       if CompareItem(Left,Right)>0 then begin
        Exchange(Left,Right);
       end;
       if CompareItem(Middle,Right)>0 then begin
        Exchange(Middle,Right);
       end;
      end;
      Pivot:=Middle;
      i:=Left;
      j:=Right;
      repeat
       while (i<Right) and (CompareItem(i,Pivot)<0) do begin
        inc(i);
       end;
       while (j>=i) and (CompareItem(j,Pivot)>0) do begin
        dec(j);
       end;
       if i>j then begin
        break;
       end else begin
        if i<>j then begin
         Exchange(i,j);
         if Pivot=i then begin
          Pivot:=j;
         end else if Pivot=j then begin
          Pivot:=i;
         end;
        end;
        inc(i);
        dec(j);
       end;
      until false;
      if i<Right then begin
       StackItem^.Left:=i;
       StackItem^.Right:=Right;
       StackItem^.Depth:=Depth-1;
       inc(StackItem);
      end;
      if Left<j then begin
       StackItem^.Left:=Left;
       StackItem^.Right:=j;
       StackItem^.Depth:=Depth-1;
       inc(StackItem);
      end;
     end;
    end;
   end;
  end;
 end;
end;

procedure TpvVectorPathVectorList.RemoveDuplicates;
var Index:TpvSizeInt;
begin
 if Count>=2 then begin
  Sort;
  for Index:=Count-1 downto 1 do begin
   if Items[Index-1]=Items[Index] then begin
    Delete(Index-1);
   end;
  end;
 end;
end;

{ TpvVectorPathBoundingBox }

constructor TpvVectorPathBoundingBox.Create(const aMin,aMax:TpvVectorPathVector);
begin
 MinMax[0]:=aMin;
 MinMax[1]:=aMax;
end;

procedure TpvVectorPathBoundingBox.Extend(const aVector:TpvVectorPathVector);
begin
 MinMax[0]:=MinMax[0].Minimum(aVector);
 MinMax[1]:=MinMax[1].Maximum(aVector);
end;

function TpvVectorPathBoundingBox.Combine(const aWith:TpvVectorPathBoundingBox):TpvVectorPathBoundingBox;
begin
 result:=TpvVectorPathBoundingBox.Create(MinMax[0].Minimum(aWith.MinMax[0]),MinMax[1].Maximum(aWith.MinMax[1]));
end;

function TpvVectorPathBoundingBox.Cost:TpvDouble;
begin
 result:=(Max.x-Min.x)+(Max.y-Min.y); // Manhattan distance
end;

function TpvVectorPathBoundingBox.Volume:TpvDouble;
begin
 result:=(Max.x-Min.x)*(Max.y-Min.y); // Volume
end;

function TpvVectorPathBoundingBox.Area:TpvDouble;
begin
 result:=2.0*(abs(Max.x-Min.x)*abs(Max.y-Min.y));
end;

function TpvVectorPathBoundingBox.Center:TpvVectorPathVector;
begin
 result:=Min.Lerp(Max,0.5);
end;

function TpvVectorPathBoundingBox.Contains(const aVector:TpvVectorPathVector):Boolean;
begin
 result:=((aVector.x>=MinMax[0].x) and (aVector.y>=MinMax[0].y)) and
         ((aVector.x<=MinMax[1].x) and (aVector.y<=MinMax[1].y));
end;

function TpvVectorPathBoundingBox.Contains(const aBoundingBox:TpvVectorPathBoundingBox):Boolean;
begin
 result:=((Min.x-EPSILON)<=(aBoundingBox.Min.x+EPSILON)) and ((Min.y-EPSILON)<=(aBoundingBox.Min.y+EPSILON)) and
         ((Max.x+EPSILON)>=(aBoundingBox.Min.x+EPSILON)) and ((Max.y+EPSILON)>=(aBoundingBox.Min.y+EPSILON)) and
         ((Min.x-EPSILON)<=(aBoundingBox.Max.x-EPSILON)) and ((Min.y-EPSILON)<=(aBoundingBox.Max.y-EPSILON)) and
         ((Max.x+EPSILON)>=(aBoundingBox.Max.x-EPSILON)) and ((Max.y+EPSILON)>=(aBoundingBox.Max.y-EPSILON));
end;

class function TpvVectorPathBoundingBox.Contains(const aMin,aMax:TpvVectorPathVector;const aWith:TpvVectorPathBoundingBox):Boolean;
begin
 result:=((aMin.x-EPSILON)<=(aWith.Min.x+EPSILON)) and ((aMin.y-EPSILON)<=(aWith.Min.y+EPSILON)) and
         ((aMax.x+EPSILON)>=(aWith.Min.x+EPSILON)) and ((aMax.y+EPSILON)>=(aWith.Min.y+EPSILON)) and
         ((aMin.x-EPSILON)<=(aWith.Max.x-EPSILON)) and ((aMin.y-EPSILON)<=(aWith.Max.y-EPSILON)) and
         ((aMax.x+EPSILON)>=(aWith.Max.x-EPSILON)) and ((aMax.y+EPSILON)>=(aWith.Max.y-EPSILON));
end;

class function TpvVectorPathBoundingBox.Contains(const aMin,aMax:TpvVector2;const aWith:TpvVectorPathBoundingBox):Boolean;
begin
 result:=((aMin.x-EPSILON)<=(aWith.Min.x+EPSILON)) and ((aMin.y-EPSILON)<=(aWith.Min.y+EPSILON)) and
         ((aMax.x+EPSILON)>=(aWith.Min.x+EPSILON)) and ((aMax.y+EPSILON)>=(aWith.Min.y+EPSILON)) and
         ((aMin.x-EPSILON)<=(aWith.Max.x-EPSILON)) and ((aMin.y-EPSILON)<=(aWith.Max.y-EPSILON)) and
         ((aMax.x+EPSILON)>=(aWith.Max.x-EPSILON)) and ((aMax.y+EPSILON)>=(aWith.Max.y-EPSILON));
end;

class function TpvVectorPathBoundingBox.Contains(const aMin,aMax,aVector:TpvVectorPathVector):boolean;
begin
 result:=((aVector.x>=(aMin.x-EPSILON)) and (aVector.x<=(aMax.x+EPSILON))) and
         ((aVector.y>=(aMin.y-EPSILON)) and (aVector.y<=(aMax.y+EPSILON)));
end;

class function TpvVectorPathBoundingBox.Contains(const aMin,aMax:TpvVector2;const aVector:TpvVectorPathVector):Boolean;
begin
 result:=((aVector.x>=(aMin.x-EPSILON)) and (aVector.x<=(aMax.x+EPSILON))) and
         ((aVector.y>=(aMin.y-EPSILON)) and (aVector.y<=(aMax.y+EPSILON)));
end;

function TpvVectorPathBoundingBox.Intersect(const aWith:TpvVectorPathBoundingBox;const aThreshold:TpvDouble):Boolean;
begin
 result:=(((Max.x+aThreshold)>=(aWith.Min.x-aThreshold)) and ((Min.x-aThreshold)<=(aWith.Max.x+aThreshold))) and
         (((Max.y+aThreshold)>=(aWith.Min.y-aThreshold)) and ((Min.y-aThreshold)<=(aWith.Max.y+aThreshold)));
end;

class function TpvVectorPathBoundingBox.Intersect(const aMin,aMax:TpvVectorPathVector;const aWith:TpvVectorPathBoundingBox;const aThreshold:TpvDouble):Boolean;
begin
 result:=(((aMax.x+aThreshold)>=(aWith.Min.x-aThreshold)) and ((aMin.x-aThreshold)<=(aWith.Max.x+aThreshold))) and
         (((aMax.y+aThreshold)>=(aWith.Min.y-aThreshold)) and ((aMin.y-aThreshold)<=(aWith.Max.y+aThreshold)));
end;

class function TpvVectorPathBoundingBox.Intersect(const aMin,aMax:TpvVector2;const aWith:TpvVectorPathBoundingBox;const aThreshold:TpvDouble):Boolean;
begin
 result:=(((aMax.x+aThreshold)>=(aWith.Min.x-aThreshold)) and ((aMin.x-aThreshold)<=(aWith.Max.x+aThreshold))) and
         (((aMax.y+aThreshold)>=(aWith.Min.y-aThreshold)) and ((aMin.y-aThreshold)<=(aWith.Max.y+aThreshold)));
end;

function TpvVectorPathBoundingBox.FastRayIntersection(const aOrigin,aDirection:TpvVectorPathVector):Boolean;
var Center,BoxExtents,Diff:TpvVectorPathVector;
begin
 Center:=(Min+Max)*0.5;
 BoxExtents:=Center-Min;
 Diff:=aOrigin-Center;
 result:=not ((((abs(Diff.x)>BoxExtents.x) and ((Diff.x*aDirection.x)>=0)) or
               ((abs(Diff.y)>BoxExtents.y) and ((Diff.y*aDirection.y)>=0))) or
              ((abs((aDirection.x*Diff.y)-(aDirection.y*Diff.x))>((BoxExtents.x*abs(aDirection.y))+(BoxExtents.y*abs(aDirection.x))))));
end;

class function TpvVectorPathBoundingBox.FastRayIntersection(const aMin,aMax:TpvVectorPathVector;const aOrigin,aDirection:TpvVectorPathVector):Boolean;
var Center,BoxExtents,Diff:TpvVectorPathVector;
begin
 Center:=(aMin+aMax)*0.5;
 BoxExtents:=Center-aMin;
 Diff:=aOrigin-Center;
 result:=not ((((abs(Diff.x)>BoxExtents.x) and ((Diff.x*aDirection.x)>=0)) or
               ((abs(Diff.y)>BoxExtents.y) and ((Diff.y*aDirection.y)>=0))) or
              ((abs((aDirection.x*Diff.y)-(aDirection.y*Diff.x))>((BoxExtents.x*abs(aDirection.y))+(BoxExtents.y*abs(aDirection.x))))));
end;

class function TpvVectorPathBoundingBox.FastRayIntersection(const aMin,aMax:TpvVector2;const aOrigin,aDirection:TpvVectorPathVector):Boolean;
var Center,BoxExtents,Diff:TpvVectorPathVector;
begin
 Center.x:=(aMin.x+aMax.x)*0.5;
 Center.y:=(aMin.y+aMax.y)*0.5;
 BoxExtents.x:=Center.x-aMin.x;
 BoxExtents.y:=Center.y-aMin.y;
 Diff:=aOrigin-Center;
 result:=not ((((abs(Diff.x)>BoxExtents.x) and ((Diff.x*aDirection.x)>=0)) or
               ((abs(Diff.y)>BoxExtents.y) and ((Diff.y*aDirection.y)>=0))) or
              ((abs((aDirection.x*Diff.y)-(aDirection.y*Diff.x))>((BoxExtents.x*abs(aDirection.y))+(BoxExtents.y*abs(aDirection.x))))));
end;

function TpvVectorPathBoundingBox.LineIntersection(const aStartPoint,aEndPoint:TpvVectorPathVector):Boolean;
var Direction,InvDirection,a,b:TpvVectorPathVector;
    Len,TimeMin,TimeMax:TpvDouble;
begin
 if Contains(aStartPoint) or Contains(aEndPoint) then begin
  result:=true;
 end else begin
  Direction:=aEndPoint-aStartPoint;
  Len:=Direction.Length;
  if Len<>0.0 then begin
   Direction:=Direction/Len;
  end;
  if Direction.x<>0.0 then begin
   InvDirection.x:=1.0/Direction.x;
  end else begin
   InvDirection.x:=Infinity;
  end;
  if Direction.y<>0.0 then begin
   InvDirection.y:=1.0/Direction.y;
  end else begin
   InvDirection.y:=Infinity;
  end;
  a.x:=((Min.x-EPSILON)-aStartPoint.x)*InvDirection.x;
  a.y:=((Min.y-EPSILON)-aStartPoint.y)*InvDirection.y;
  b.x:=((Max.x+EPSILON)-aStartPoint.x)*InvDirection.x;
  b.y:=((Max.y+EPSILON)-aStartPoint.y)*InvDirection.y;
  TimeMin:=Math.Max(Math.Min(a.x,a.y),Math.Min(b.x,b.y));
  TimeMax:=Math.Min(Math.Max(a.x,a.y),Math.Max(b.x,b.y));
  result:=((TimeMin<=TimeMax) and (TimeMax>=0.0)) and (TimeMin<=(Len+EPSILON));
 end;
end;

class function TpvVectorPathBoundingBox.LineIntersection(const aMin,aMax:TpvVectorPathVector;const aStartPoint,aEndPoint:TpvVectorPathVector):Boolean;
var Direction,InvDirection,a,b:TpvVectorPathVector;
    Len,TimeMin,TimeMax:TpvDouble;
begin
 if TpvVectorPathBoundingBox.Contains(aMin,aMax,aStartPoint) or TpvVectorPathBoundingBox.Contains(aMin,aMax,aEndPoint) then begin
  result:=true;
 end else begin
  Direction:=aEndPoint-aStartPoint;
  Len:=Direction.Length;
  if Len<>0.0 then begin
   Direction:=Direction/Len;
  end;
  if Direction.x<>0.0 then begin
   InvDirection.x:=1.0/Direction.x;
  end else begin
   InvDirection.x:=Infinity;
  end;
  if Direction.y<>0.0 then begin
   InvDirection.y:=1.0/Direction.y;
  end else begin
   InvDirection.y:=Infinity;
  end;
  a.x:=((aMin.x-EPSILON)-aStartPoint.x)*InvDirection.x;
  a.y:=((aMin.y-EPSILON)-aStartPoint.y)*InvDirection.y;
  b.x:=((aMax.x+EPSILON)-aStartPoint.x)*InvDirection.x;
  b.y:=((aMax.y+EPSILON)-aStartPoint.y)*InvDirection.y;
  TimeMin:=Math.Max(Math.Min(a.x,a.y),Math.Min(b.x,b.y));
  TimeMax:=Math.Min(Math.Max(a.x,a.y),Math.Max(b.x,b.y));
  result:=((TimeMin<=TimeMax) and (TimeMax>=0.0)) and (TimeMin<=(Len+EPSILON));
 end;
end;

class function TpvVectorPathBoundingBox.LineIntersection(const aMin,aMax:TpvVector2;const aStartPoint,aEndPoint:TpvVectorPathVector):Boolean;
var Direction,InvDirection,a,b:TpvVectorPathVector;
    Len,TimeMin,TimeMax:TpvDouble;
begin
 if TpvVectorPathBoundingBox.Contains(aMin,aMax,aStartPoint) or TpvVectorPathBoundingBox.Contains(aMin,aMax,aEndPoint) then begin
  result:=true;
 end else begin
  Direction:=aEndPoint-aStartPoint;
  Len:=Direction.Length;
  if Len<>0.0 then begin
   Direction:=Direction/Len;
  end;
  if Direction.x<>0.0 then begin
   InvDirection.x:=1.0/Direction.x;
  end else begin
   InvDirection.x:=Infinity;
  end;
  if Direction.y<>0.0 then begin
   InvDirection.y:=1.0/Direction.y;
  end else begin
   InvDirection.y:=Infinity;
  end;
  a.x:=((aMin.x-EPSILON)-aStartPoint.x)*InvDirection.x;
  a.y:=((aMin.y-EPSILON)-aStartPoint.y)*InvDirection.y;
  b.x:=((aMax.x+EPSILON)-aStartPoint.x)*InvDirection.x;
  b.y:=((aMax.y+EPSILON)-aStartPoint.y)*InvDirection.y;
  TimeMin:=Math.Max(Math.Min(a.x,a.y),Math.Min(b.x,b.y));
  TimeMax:=Math.Min(Math.Max(a.x,a.y),Math.Max(b.x,b.y));
  result:=((TimeMin<=TimeMax) and (TimeMax>=0.0)) and (TimeMin<=(Len+EPSILON));
 end;
end;

{ TpvVectorPathBVHDynamicAABBTree.TSkipList }

constructor TpvVectorPathBVHDynamicAABBTree.TSkipList.Create(const aFrom:TpvVectorPathBVHDynamicAABBTree;const aGetUserDataIndex:TpvVectorPathBVHDynamicAABBTree.TGetUserDataIndex);
begin
 fNodeArray.Initialize;
 aFrom.GetSkipListNodes(fNodeArray,aGetUserDataIndex);
end;

destructor TpvVectorPathBVHDynamicAABBTree.TSkipList.Destroy;
begin
 fNodeArray.Finalize;
 inherited Destroy;
end;

function TpvVectorPathBVHDynamicAABBTree.TSkipList.IntersectionQuery(const aAABB:TpvVectorPathBoundingBox):TpvVectorPathBVHDynamicAABBTree.TUserDataArray;
var Index,Count:TpvSizeInt;
    Node:TpvVectorPathBVHDynamicAABBTree.PSkipListNode;
begin
 result:=nil;
 Count:=fNodeArray.Count;
 if Count>0 then begin
  Index:=0;
  while Index<Count do begin
   Node:=@fNodeArray.Items[Index];
   if TpvVectorPathBoundingBox.Intersect(Node^.AABBMin,Node^.AABBMax,aAABB) then begin
    if Node^.UserData<>0 then begin
     result:=result+[Node^.UserData];
    end;
    inc(Index);
   end else begin
    if Node^.SkipCount>0 then begin
     inc(Index,Node^.SkipCount);
    end else begin
     break;
    end;
   end;
  end;
 end;
end;

function TpvVectorPathBVHDynamicAABBTree.TSkipList.ContainQuery(const aAABB:TpvVectorPathBoundingBox):TpvVectorPathBVHDynamicAABBTree.TUserDataArray;
var Index,Count:TpvSizeInt;
    Node:TpvVectorPathBVHDynamicAABBTree.PSkipListNode;
begin
 result:=nil;
 Count:=fNodeArray.Count;
 if Count>0 then begin
  Index:=0;
  while Index<Count do begin
   Node:=@fNodeArray.Items[Index];
   if TpvVectorPathBoundingBox.Contains(Node^.AABBMin,Node^.AABBMax,aAABB) then begin
    if Node^.UserData<>0 then begin
     result:=result+[Node^.UserData];
    end;
    inc(Index);
   end else begin
    if Node^.SkipCount>0 then begin
     inc(Index,Node^.SkipCount);
    end else begin
     break;
    end;
   end;
  end;
 end;
end;

function TpvVectorPathBVHDynamicAABBTree.TSkipList.ContainQuery(const aPoint:TpvVectorPathVector):TpvVectorPathBVHDynamicAABBTree.TUserDataArray;
var Index,Count:TpvSizeInt;
    Node:TpvVectorPathBVHDynamicAABBTree.PSkipListNode;
begin
 result:=nil;
 Count:=fNodeArray.Count;
 if Count>0 then begin
  Index:=0;
  while Index<Count do begin
   Node:=@fNodeArray.Items[Index];
   if TpvVectorPathBoundingBox.Contains(Node^.AABBMin,Node^.AABBMax,aPoint) then begin
    if Node^.UserData<>0 then begin
     result:=result+[Node^.UserData];
    end;
    inc(Index);
   end else begin
    if Node^.SkipCount>0 then begin
     inc(Index,Node^.SkipCount);
    end else begin
     break;
    end;
   end;
  end;
 end;
end;

function TpvVectorPathBVHDynamicAABBTree.TSkipList.RayCast(const aRayOrigin,aRayDirection:TpvVectorPathVector;out aTime:TpvDouble;out aUserData:TpvUInt32;const aStopAtFirstHit:boolean;const aRayCastUserData:TpvVectorPathBVHDynamicAABBTree.TRayCastUserData):boolean;
var Index,Count:TpvSizeInt;
    Node:TpvVectorPathBVHDynamicAABBTree.PSkipListNode;
    RayEnd:TpvVectorPathVector;
    Time:TpvDouble;
    Stop:boolean;
begin
 result:=false;
 Count:=fNodeArray.Count;
 if assigned(aRayCastUserData) and (Count>0) then begin
  aTime:=Infinity;
  RayEnd:=aRayOrigin;
  Index:=0;
  while Index<Count do begin
   Node:=@fNodeArray.Items[Index];
   if ((not result) and
       (TpvVectorPathBoundingBox.Contains(Node^.AABBMin,Node^.AABBMax,aRayOrigin) or
        TpvVectorPathBoundingBox.FastRayIntersection(Node^.AABBMin,Node^.AABBMax,aRayOrigin,aRayDirection))) or
      (result and TpvVectorPathBoundingBox.LineIntersection(Node^.AABBMin,Node^.AABBMax,aRayOrigin,RayEnd)) then begin
    if (Node^.UserData<>High(TpvUInt32)) and aRayCastUserData(Node^.UserData,aRayOrigin,aRayDirection,Time,Stop) then begin
     if (not result) or (Time<aTime) then begin
      aTime:=Time;
      aUserData:=Node^.UserData;
      result:=true;
      if aStopAtFirstHit or Stop then begin
       break;
      end else begin
       RayEnd:=aRayOrigin+(aRayDirection*Time);
      end;
     end;
    end;
    inc(Index);
   end else begin
    if Node^.SkipCount>0 then begin
     inc(Index,Node^.SkipCount);
    end else begin
     break;
    end;
   end;
  end;
 end;
end;

function TpvVectorPathBVHDynamicAABBTree.TSkipList.RayCastLine(const aFrom,aTo:TpvVectorPathVector;out aTime:TpvDouble;out aUserData:TpvUInt32;const aStopAtFirstHit:boolean;const aRayCastUserData:TpvVectorPathBVHDynamicAABBTree.TRayCastUserData):boolean;
var Index,Count:TpvSizeInt;
    Node:TpvVectorPathBVHDynamicAABBTree.PSkipListNode;
    Time,RayLength:TpvDouble;
    RayOrigin,RayDirection,RayEnd:TpvVectorPathVector;
    Stop:boolean;
begin
 result:=false;
 Count:=fNodeArray.Count;
 if assigned(aRayCastUserData) and (Count>0) then begin
  aTime:=Infinity;
  RayOrigin:=aFrom;
  RayEnd:=aTo;
  RayDirection:=(RayEnd-RayOrigin).Normalize;
  RayLength:=(RayEnd-RayOrigin).Length;
  Index:=0;
  while Index<Count do begin
   Node:=@fNodeArray.Items[Index];
   if TpvVectorPathBoundingBox.LineIntersection(Node^.AABBMin,Node^.AABBMax,RayOrigin,RayEnd) then begin
    if (Node^.UserData<>High(TpvUInt32)) and aRayCastUserData(Node^.UserData,RayOrigin,RayDirection,Time,Stop) then begin
     if ((Time>=0.0) and (Time<=RayLength)) and ((not result) or (Time<aTime)) then begin
      aTime:=Time;
      aUserData:=Node^.UserData;
      result:=true;
      if aStopAtFirstHit or Stop then begin
       break;
      end else begin
       RayEnd:=RayOrigin+(RayDirection*Time);
       RayLength:=Time;
      end;
     end;
    end;
    inc(Index);
   end else begin
    if Node^.SkipCount>0 then begin
     inc(Index,Node^.SkipCount);
    end else begin
     break;
    end;
   end;
  end;
 end;
end;

{ TpvVectorPathMatrix }

constructor TpvVectorPathMatrix.Create(const aValue:TpvDouble);
begin
 m00:=aValue;
 m01:=aValue;
 m10:=aValue;
 m11:=aValue;
 m20:=aValue;
 m21:=aValue;
end;

constructor TpvVectorPathMatrix.Create(const aM00,aM01,aM10,aM11,aM20,aM21:TpvDouble);
begin
 m00:=aM00;
 m01:=aM01;
 m10:=aM10;
 m11:=aM11;
 m20:=aM20;
 m21:=aM21;
end;

class function TpvVectorPathMatrix.CreateIdentity:TpvVectorPathMatrix;
begin
 result.m00:=1.0;
 result.m01:=0.0;
 result.m10:=0.0;
 result.m11:=1.0;
 result.m20:=0.0;
 result.m21:=0.0;
end;

class function TpvVectorPathMatrix.CreateTranslate(const aTX,aTY:TpvDouble):TpvVectorPathMatrix;
begin
 result.m00:=1.0;
 result.m01:=0.0;
 result.m10:=0.0;
 result.m11:=1.0;
 result.m20:=aTX;
 result.m21:=aTY;
end;

class function TpvVectorPathMatrix.CreateTranslate(const aTranslate:TpvVectorPathVector):TpvVectorPathMatrix;
begin
 result:=TpvVectorPathMatrix.CreateTranslate(aTranslate.x,aTranslate.y);
end;

class function TpvVectorPathMatrix.CreateScale(const aSX,aSY:TpvDouble):TpvVectorPathMatrix;
begin
 result.m00:=aSX;
 result.m01:=0.0;
 result.m10:=0.0;
 result.m11:=aSY;
 result.m20:=0.0;
 result.m21:=0.0;
end;

class function TpvVectorPathMatrix.CreateScale(const aScale:TpvVectorPathVector):TpvVectorPathMatrix;
begin
 result:=TpvVectorPathMatrix.CreateScale(aScale.x,aScale.y);
end;

class function TpvVectorPathMatrix.CreateRotate(const aAngle:TpvDouble):TpvVectorPathMatrix;
var Sinus,Cosinus:TpvDouble;
begin
 Sinus:=sin(aAngle);
 Cosinus:=cos(aAngle);
 result.m00:=Cosinus;
 result.m01:=Sinus;
 result.m10:=-Sinus;
 result.m11:=Cosinus;
 result.m20:=0.0;
 result.m21:=0.0;
end;

class function TpvVectorPathMatrix.CreateRotate(const aAngle,aCenterX,aCenterY:TpvDouble):TpvVectorPathMatrix;
begin
 result:=TpvVectorPathMatrix.CreateTranslate(-aCenterX,-aCenterY)*
         TpvVectorPathMatrix.CreateRotate(aAngle)*
         TpvVectorPathMatrix.CreateTranslate(aCenterX,aCenterY);
end;

class function TpvVectorPathMatrix.CreateSkewX(const aAngle:TpvDouble):TpvVectorPathMatrix;
begin
 result.m00:=1.0;
 result.m01:=0.0;
 result.m10:=tan(aAngle);
 result.m11:=1.0;
 result.m20:=0.0;
 result.m21:=0.0;
end;

class function TpvVectorPathMatrix.CreateSkewY(const aAngle:TpvDouble):TpvVectorPathMatrix;
begin
 result.m00:=1.0;
 result.m01:=tan(aAngle);
 result.m10:=0.0;
 result.m11:=1.0;
 result.m20:=0.0;
 result.m21:=0.0;
end;

function TpvVectorPathMatrix.Determinant:TpvDouble;
begin
 result:=(m00*m11)-(m01*m10);
end;

function TpvVectorPathMatrix.Inverse:TpvVectorPathMatrix;
var CurrentDeterminant,InverseDeterminant:TpvDouble;
begin
 CurrentDeterminant:=Determinant;
 if abs(CurrentDeterminant)<1e-30 then begin
  result:=TpvVectorPathMatrix.CreateIdentity;
 end else begin
  InverseDeterminant:=1.0/CurrentDeterminant;
  result.m00:=m11*InverseDeterminant;
  result.m01:=-(m01*InverseDeterminant);
  result.m10:=-(m10*InverseDeterminant);
  result.m11:=m00*InverseDeterminant;
  result.m20:=((m10*m21)-(m11*m20))*InverseDeterminant;
  result.m21:=((m01*m20)-(m00*m21))*InverseDeterminant;
 end;
end;

function TpvVectorPathMatrix.ExpansionFactor:TpvDouble;
begin
 result:=(sqrt(sqr(m00)+sqr(m01))+sqrt(sqr(m10)+sqr(m11)))*0.5;
end;

function TpvVectorPathMatrix.Transform(const aVector:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=((aVector.x*m00)+(aVector.y*m10))+m20;
 result.y:=((aVector.x*m01)+(aVector.y*m11))+m21;
end;

function TpvVectorPathMatrix.Transform(const aX,aY:TpvDouble):TpvVectorPathVector;
begin
 result.x:=((aX*m00)+(aY*m10))+m20;
 result.y:=((aX*m01)+(aY*m11))+m21;
end;

function TpvVectorPathMatrix.TransformDirection(const aVector:TpvVectorPathVector):TpvVectorPathVector;
begin
 result.x:=(aVector.x*m00)+(aVector.y*m10);
 result.y:=(aVector.x*m01)+(aVector.y*m11);
end;

class operator TpvVectorPathMatrix.Equal(const a,b:TpvVectorPathMatrix):boolean;
begin
 result:=SameValue(a.m00,b.m00) and
         SameValue(a.m01,b.m01) and
         SameValue(a.m10,b.m10) and
         SameValue(a.m11,b.m11) and
         SameValue(a.m20,b.m20) and
         SameValue(a.m21,b.m21);
end;

class operator TpvVectorPathMatrix.NotEqual(const a,b:TpvVectorPathMatrix):boolean;
begin
 result:=not (a=b);
end;

class operator TpvVectorPathMatrix.Multiply(const a,b:TpvVectorPathMatrix):TpvVectorPathMatrix;
begin
 result.m00:=(a.m00*b.m00)+(a.m01*b.m10);
 result.m01:=(a.m00*b.m01)+(a.m01*b.m11);
 result.m10:=(a.m10*b.m00)+(a.m11*b.m10);
 result.m11:=(a.m10*b.m01)+(a.m11*b.m11);
 result.m20:=((a.m20*b.m00)+(a.m21*b.m10))+b.m20;
 result.m21:=((a.m20*b.m01)+(a.m21*b.m11))+b.m21;
end;

class operator TpvVectorPathMatrix.Multiply(const a:TpvVectorPathMatrix;const b:TpvVectorPathVector):TpvVectorPathVector;
begin
 result:=a.Transform(b);
end;

class operator TpvVectorPathMatrix.Multiply(const a:TpvVectorPathVector;const b:TpvVectorPathMatrix):TpvVectorPathVector;
begin
 result:=b.Transform(a);
end;

{ TpvVectorPathBVHDynamicAABBTree }

constructor TpvVectorPathBVHDynamicAABBTree.Create;
var i:TpvSizeInt;
begin
 inherited Create;
 Root:=NULLNODE;
 Nodes:=nil;
 NodeCount:=0;
 NodeCapacity:=16;
 SetLength(Nodes,NodeCapacity);
 FillChar(Nodes[0],NodeCapacity*SizeOf(TTreeNode),#0);
 for i:=0 to NodeCapacity-2 do begin
  Nodes[i].Next:=i+1;
  Nodes[i].Height:=-1;
 end;
 Nodes[NodeCapacity-1].Next:=NULLNODE;
 Nodes[NodeCapacity-1].Height:=-1;
 FreeList:=0;
 Path:=0;
 InsertionCount:=0;
 fSkipListNodeLock:=TPasMPSpinLock.Create;
 fSkipListNodeMap:=nil;
 fSkipListNodeStack.Initialize;
end;

destructor TpvVectorPathBVHDynamicAABBTree.Destroy;
begin
 fSkipListNodeStack.Finalize;
 fSkipListNodeMap:=nil;
 FreeAndNil(fSkipListNodeLock);
 Nodes:=nil;
 inherited Destroy;
end;

function TpvVectorPathBVHDynamicAABBTree.AllocateNode:TpvSizeInt;
var Node:PTreeNode;
    i:TpvSizeInt;
begin
 if FreeList=NULLNODE then begin
  inc(NodeCapacity,(NodeCapacity+1) shr 1); // *1.5
  SetLength(Nodes,NodeCapacity);
  FillChar(Nodes[NodeCount],(NodeCapacity-NodeCount)*SizeOf(TTreeNode),#0);
  for i:=NodeCount to NodeCapacity-2 do begin
   Nodes[i].Next:=i+1;
   Nodes[i].Height:=-1;
  end;
  Nodes[NodeCapacity-1].Next:=NULLNODE;
  Nodes[NodeCapacity-1].Height:=-1;
  FreeList:=NodeCount;
 end;
 result:=FreeList;
 FreeList:=Nodes[result].Next;
 Node:=@Nodes[result];
 Node^.Parent:=NULLNODE;
 Node^.Children[0]:=NULLNODE;
 Node^.Children[1]:=NULLNODE;
 Node^.Height:=0;
 Node^.UserData:=0;
 inc(NodeCount);
end;

procedure TpvVectorPathBVHDynamicAABBTree.FreeNode(const aNodeID:TpvSizeInt);
var Node:PTreeNode;
begin
 Node:=@Nodes[aNodeID];
 Node^.Next:=FreeList;
 Node^.Height:=-1;
 FreeList:=aNodeID;
 dec(NodeCount);
end;

function TpvVectorPathBVHDynamicAABBTree.Balance(const aNodeID:TpvSizeInt):TpvSizeInt;
var NodeA,NodeB,NodeC,NodeD,NodeE,NodeF,NodeG:PTreeNode;
    NodeBID,NodeCID,NodeDID,NodeEID,NodeFID,NodeGID,NodeBalance:TpvSizeInt;
begin
 NodeA:=@Nodes[aNodeID];
 if (NodeA.Children[0]<0) or (NodeA^.Height<2) then begin
  result:=aNodeID;
 end else begin
  NodeBID:=NodeA.Children[0];
  NodeCID:=NodeA.Children[1];
  NodeB:=@Nodes[NodeBID];
  NodeC:=@Nodes[NodeCID];
  NodeBalance:=NodeC^.Height-NodeB^.Height;
  if NodeBalance>1 then begin
   NodeFID:=NodeC.Children[0];
   NodeGID:=NodeC.Children[1];
   NodeF:=@Nodes[NodeFID];
   NodeG:=@Nodes[NodeGID];
   NodeC^.Children[0]:=aNodeID;
   NodeC^.Parent:=NodeA^.Parent;
   NodeA^.Parent:=NodeCID;
   if NodeC.Parent>=0 then begin
    if Nodes[NodeC^.Parent].Children[0]=aNodeID then begin
     Nodes[NodeC^.Parent].Children[0]:=NodeCID;
    end else begin
     Nodes[NodeC^.Parent].Children[1]:=NodeCID;
    end;
   end else begin
    Root:=NodeCID;
   end;
   if NodeF^.Height>NodeG^.Height then begin
    NodeC^.Children[1]:=NodeFID;
    NodeA^.Children[1]:=NodeGID;
    NodeG^.Parent:=aNodeID;
    NodeA^.AABB:=NodeB^.AABB.Combine(NodeG^.AABB);
    NodeC^.AABB:=NodeA^.AABB.Combine(NodeF^.AABB);
    NodeA^.Height:=1+Max(NodeB^.Height,NodeG^.Height);
    NodeC^.Height:=1+Max(NodeA^.Height,NodeF^.Height);
   end else begin
    NodeC^.Children[1]:=NodeGID;
    NodeA^.Children[1]:=NodeFID;
    NodeF^.Parent:=aNodeID;
    NodeA^.AABB:=NodeB^.AABB.Combine(NodeF^.AABB);
    NodeC^.AABB:=NodeA^.AABB.Combine(NodeG^.AABB);
    NodeA^.Height:=1+Max(NodeB^.Height,NodeF^.Height);
    NodeC^.Height:=1+Max(NodeA^.Height,NodeG^.Height);
   end;
   result:=NodeCID;
  end else if NodeBalance<-1 then begin
   NodeDID:=NodeB^.Children[0];
   NodeEID:=NodeB^.Children[1];
   NodeD:=@Nodes[NodeDID];
   NodeE:=@Nodes[NodeEID];
   NodeB^.Children[0]:=aNodeID;
   NodeB^.Parent:=NodeA^.Parent;
   NodeA^.Parent:=NodeBID;
   if NodeB^.Parent>=0 then begin
    if Nodes[NodeB^.Parent].Children[0]=aNodeID then begin
     Nodes[NodeB^.Parent].Children[0]:=NodeBID;
    end else begin
     Nodes[NodeB^.Parent].Children[1]:=NodeBID;
    end;
   end else begin
    Root:=NodeBID;
   end;
   if NodeD^.Height>NodeE^.Height then begin
    NodeB^.Children[1]:=NodeDID;
    NodeA^.Children[0]:=NodeEID;
    NodeE^.Parent:=aNodeID;
    NodeA^.AABB:=NodeC^.AABB.Combine(NodeE^.AABB);
    NodeB^.AABB:=NodeA^.AABB.Combine(NodeD^.AABB);
    NodeA^.Height:=1+Max(NodeC^.Height,NodeE^.Height);
    NodeB^.Height:=1+Max(NodeA^.Height,NodeD^.Height);
   end else begin
    NodeB^.Children[1]:=NodeEID;
    NodeA^.Children[0]:=NodeDID;
    NodeD^.Parent:=aNodeID;
    NodeA^.AABB:=NodeC^.AABB.Combine(NodeD^.AABB);
    NodeB^.AABB:=NodeA^.AABB.Combine(NodeE^.AABB);
    NodeA^.Height:=1+Max(NodeC^.Height,NodeD^.Height);
    NodeB^.Height:=1+Max(NodeA^.Height,NodeE^.Height);
   end;
   result:=NodeBID;
  end else begin
   result:=aNodeID;
  end;
 end;
end;

procedure TpvVectorPathBVHDynamicAABBTree.InsertLeaf(const aLeaf:TpvSizeInt);
var Node:PTreeNode;
    LeafAABB,CombinedAABB,AABB:TpvVectorPathBoundingBox;
    Index,Sibling,OldParent,NewParent:TpvSizeInt;
    Children:array[0..1] of TpvSizeInt;
    CombinedCost,Cost,InheritanceCost:TpvDouble;
    Costs:array[0..1] of TpvDouble;
begin
 inc(InsertionCount);
 if Root<0 then begin
  Root:=aLeaf;
  Nodes[aLeaf].Parent:=NULLNODE;
 end else begin
  LeafAABB:=Nodes[aLeaf].AABB;
  Index:=Root;
  while Nodes[Index].Children[0]>=0 do begin

   Children[0]:=Nodes[Index].Children[0];
   Children[1]:=Nodes[Index].Children[1];

   CombinedAABB:=Nodes[Index].AABB.Combine(LeafAABB);
   CombinedCost:=CombinedAABB.Cost;
   Cost:=CombinedCost*2.0;
   InheritanceCost:=2.0*(CombinedCost-Nodes[Index].AABB.Cost);

   AABB:=LeafAABB.Combine(Nodes[Children[0]].AABB);
   if Nodes[Children[0]].Children[0]<0 then begin
    Costs[0]:=AABB.Cost+InheritanceCost;
   end else begin
    Costs[0]:=(AABB.Cost-Nodes[Children[0]].AABB.Cost)+InheritanceCost;
   end;

   AABB:=LeafAABB.Combine(Nodes[Children[1]].AABB);
   if Nodes[Children[1]].Children[1]<0 then begin
    Costs[1]:=AABB.Cost+InheritanceCost;
   end else begin
    Costs[1]:=(AABB.Cost-Nodes[Children[1]].AABB.Cost)+InheritanceCost;
   end;

   if (Cost<Costs[0]) and (Cost<Costs[1]) then begin
    break;
   end else begin
    if Costs[0]<Costs[1] then begin
     Index:=Children[0];
    end else begin
     Index:=Children[1];
    end;
   end;

  end;

  Sibling:=Index;

  OldParent:=Nodes[Sibling].Parent;
  NewParent:=AllocateNode;
  Nodes[NewParent].Parent:=OldParent;
  Nodes[NewParent].UserData:=0;
  Nodes[NewParent].AABB:=LeafAABB.Combine(Nodes[Sibling].AABB);
  Nodes[NewParent].Height:=Nodes[Sibling].Height+1;

  if OldParent>=0 then begin
   if Nodes[OldParent].Children[0]=Sibling then begin
    Nodes[OldParent].Children[0]:=NewParent;
   end else begin
    Nodes[OldParent].Children[1]:=NewParent;
   end;
   Nodes[NewParent].Children[0]:=Sibling;
   Nodes[NewParent].Children[1]:=aLeaf;
   Nodes[Sibling].Parent:=NewParent;
   Nodes[aLeaf].Parent:=NewParent;
  end else begin
   Nodes[NewParent].Children[0]:=Sibling;
   Nodes[NewParent].Children[1]:=aLeaf;
   Nodes[Sibling].Parent:=NewParent;
   Nodes[aLeaf].Parent:=NewParent;
   Root:=NewParent;
  end;

  Index:=Nodes[aLeaf].Parent;
  while Index>=0 do begin
   Index:=Balance(Index);
   Node:=@Nodes[Index];
   Node^.AABB:=Nodes[Node^.Children[0]].AABB.Combine(Nodes[Node^.Children[1]].AABB);
   Node^.Height:=1+Max(Nodes[Node^.Children[0]].Height,Nodes[Node^.Children[1]].Height);
   Index:=Node^.Parent;
  end;

 end;
end;

procedure TpvVectorPathBVHDynamicAABBTree.RemoveLeaf(const aLeaf:TpvSizeInt);
var Node:PTreeNode;
    Parent,GrandParent,Sibling,Index:TpvSizeInt;
begin
 if Root=aLeaf then begin
  Root:=NULLNODE;
 end else begin
  Parent:=Nodes[aLeaf].Parent;
  GrandParent:=Nodes[Parent].Parent;
  if Nodes[Parent].Children[0]=aLeaf then begin
   Sibling:=Nodes[Parent].Children[1];
  end else begin
   Sibling:=Nodes[Parent].Children[0];
  end;
  if GrandParent>=0 then begin
   if Nodes[GrandParent].Children[0]=Parent then begin
    Nodes[GrandParent].Children[0]:=Sibling;
   end else begin
    Nodes[GrandParent].Children[1]:=Sibling;
   end;
   Nodes[Sibling].Parent:=GrandParent;
   FreeNode(Parent);
   Index:=GrandParent;
   while Index>=0 do begin
    Index:=Balance(Index);
    Node:=@Nodes[Index];
    Node^.AABB:=Nodes[Node^.Children[0]].AABB.Combine(Nodes[Node^.Children[1]].AABB);
    Node^.Height:=1+Max(Nodes[Node^.Children[0]].Height,Nodes[Node^.Children[1]].Height);
    Index:=Node^.Parent;
   end;
  end else begin
   Root:=Sibling;
   Nodes[Sibling].Parent:=NULLNODE;
   FreeNode(Parent);
  end;
 end;
end;

function TpvVectorPathBVHDynamicAABBTree.CreateProxy(const aAABB:TpvVectorPathBoundingBox;const aUserData:TpvPtrInt):TpvSizeInt;
var Node:PTreeNode;
begin
 result:=AllocateNode;
 Node:=@Nodes[result];
 Node^.AABB.Min:=aAABB.Min-ThresholdAABBVector;
 Node^.AABB.Max:=aAABB.Max+ThresholdAABBVector;
 Node^.UserData:=aUserData;
 Node^.Height:=0;
 InsertLeaf(result);
end;

procedure TpvVectorPathBVHDynamicAABBTree.DestroyProxy(const aNodeID:TpvSizeInt);
begin
 RemoveLeaf(aNodeID);
 FreeNode(aNodeID);
end;

function TpvVectorPathBVHDynamicAABBTree.MoveProxy(const aNodeID:TpvSizeInt;const aAABB:TpvVectorPathBoundingBox;const aDisplacement:TpvVectorPathVector):boolean;
var Node:PTreeNode;
    b:TpvVectorPathBoundingBox;
    d:TpvVectorPathVector;
begin
 Node:=@Nodes[aNodeID];
 result:=not Node^.AABB.Contains(aAABB);
 if result then begin
  RemoveLeaf(aNodeID);
  b.Min:=aAABB.Min-ThresholdAABBVector;
  b.Max:=aAABB.Max+ThresholdAABBVector;
  d:=aDisplacement*AABBMULTIPLIER;
  if d.x<0.0 then begin
   b.Min.x:=b.Min.x+d.x;
  end else if d.x>0.0 then begin
   b.Max.x:=b.Max.x+d.x;
  end;
  if d.y<0.0 then begin
   b.Min.y:=b.Min.y+d.y;
  end else if d.y>0.0 then begin
   b.Max.y:=b.Max.y+d.y;
  end;
  Node^.AABB:=b;
  InsertLeaf(aNodeID);
 end;
end;

procedure TpvVectorPathBVHDynamicAABBTree.Rebalance(const aIterations:TpvSizeInt);
var Counter,Node:TpvSizeInt;
    Bit:TpvSizeUInt;
//  Children:PSizeIntArray;
begin
 if (Root>=0) and (Root<NodeCount) then begin
  for Counter:=1 to aIterations do begin
   Bit:=0;
   Node:=Root;
   while Nodes[Node].Children[0]>=0 do begin
    Node:=Nodes[Node].Children[(Path shr Bit) and 1];
    Bit:=(Bit+1) and 31;
   end;
   inc(Path);
   if ((Node>=0) and (Node<NodeCount)) and (Nodes[Node].Children[0]<0) then begin
    RemoveLeaf(Node);
    InsertLeaf(Node);
   end else begin
    break;
   end;
  end;
 end;
end;

procedure TpvVectorPathBVHDynamicAABBTree.RebuildBottomUp;
var Count,IndexA,IndexB,IndexAMin,IndexBMin,Index1,Index2,ParentIndex:TpvSizeint;
    NewNodes:array of TpvSizeInt;
    Children:array[0..1] of TpvVectorPathBVHDynamicAABBTree.PTreeNode;
    Parent:TpvVectorPathBVHDynamicAABBTree.PTreeNode;
    MinCost,Cost:TpvDouble;
    AABBa,AABBb:PpvVectorPathBoundingBox;
    AABB:TpvVectorPathBoundingBox;
    First:boolean;
begin
 if NodeCount>0 then begin
  NewNodes:=nil;
  try
   SetLength(NewNodes,NodeCount);
   FillChar(NewNodes[0],NodeCount*SizeOf(TpvSizeint),#0);
   Count:=0;
   for IndexA:=0 to NodeCapacity-1 do begin
    if Nodes[IndexA].Height>=0 then begin
     if Nodes[IndexA].Children[0]<0 then begin
      Nodes[IndexA].Parent:=TpvVectorPathBVHDynamicAABBTree.NULLNODE;
      NewNodes[Count]:=IndexA;
      inc(Count);
     end else begin
      FreeNode(IndexA);
     end;
    end;
   end;
   while Count>1 do begin
    First:=true;
    MinCost:=MAX_SCALAR;
    IndexAMin:=-1;
    IndexBMin:=-1;
  {}/////////////////TOOPTIMIZE///////////////////
  {}for IndexA:=0 to Count-1 do begin           //
  {} AABBa:=@Nodes[NewNodes[IndexA]].AABB;      //
  {} for IndexB:=IndexA+1 to Count-1 do begin   //
  {}  AABBb:=@Nodes[NewNodes[IndexB]].AABB;     //
  {}  AABB:=AABBa^.Combine(AABBb^);             //
  {}  Cost:=AABB.Cost;                          //
  {}  if First or (Cost<MinCost) then begin     //
  {}   First:=false;                            //
  {}   MinCost:=Cost;                           //
  {}   IndexAMin:=IndexA;                       //
  {}   IndexBMin:=IndexB;                       //
  {}  end;                                      //
  {} end;                                       //
  {}end;                                        //
  {}/////////////////TOOPTIMIZE///////////////////
    Index1:=NewNodes[IndexAMin];
    Index2:=NewNodes[IndexBMin];
    Children[0]:=@Nodes[Index1];
    Children[1]:=@Nodes[Index2];
    ParentIndex:=AllocateNode;
    Parent:=@Nodes[ParentIndex];
    Parent^.Children[0]:=Index1;
    Parent^.Children[1]:=Index2;
    Parent^.Height:=1+Max(Children[0]^.Height,Children[1]^.Height);
    Parent^.AABB:=Children[0]^.AABB.Combine(Children[1]^.AABB);
    Parent^.Parent:=TpvVectorPathBVHDynamicAABBTree.NULLNODE;
    Children[0]^.Parent:=ParentIndex;
    Children[1]^.Parent:=ParentIndex;
    NewNodes[IndexBMin]:=NewNodes[Count-1];
    NewNodes[IndexAMin]:=ParentIndex;
    dec(Count);
   end;
   Root:=NewNodes[0];
  finally
   NewNodes:=nil;
  end;
 end;
end;

procedure TpvVectorPathBVHDynamicAABBTree.RebuildTopDown;
type TLeafNodes=array of TpvSizeInt;
     TFillStackItem=record
      Parent:TpvSizeInt;
      Which:TpvSizeInt;
      LeafNodes:TLeafNodes;
     end;
     TFillStack=TpvDynamicStack<TFillStackItem>;
     THeightStackItem=record
      Node:TpvSizeInt;
      Pass:TpvSizeInt;
     end;
     THeightStack=TpvDynamicStack<THeightStackItem>;
var Count,Index,MinPerSubTree,ParentIndex,NodeIndex,SplitAxis,TempIndex,
    LeftIndex,RightIndex,LeftCount,RightCount:TpvSizeint;
    LeafNodes:TLeafNodes;
    SplitValue:TpvDouble;
    AABB:TpvVectorPathBoundingBox;
    Center:TpvVectorPathVector;
    VarianceX,VarianceY,MeanX,MeanY:TpvDouble;
    FillStack:TFillStack;
    FillStackItem,NewFillStackItem:TFillStackItem;
    HeightStack:THeightStack;
    HeightStackItem,NewHeightStackItem:THeightStackItem;
begin

 if NodeCount>0 then begin

  LeafNodes:=nil;
  try

   SetLength(LeafNodes,NodeCount);
   FillChar(LeafNodes[0],NodeCount*SizeOf(TpvSizeint),#0);

   Count:=0;
   for Index:=0 to NodeCapacity-1 do begin
    if Nodes[Index].Height>=0 then begin
     if Nodes[Index].Children[0]<0 then begin
      Nodes[Index].Parent:=TpvVectorPathBVHDynamicAABBTree.NULLNODE;
      LeafNodes[Count]:=Index;
      inc(Count);
     end else begin
      FreeNode(Index);
     end;
    end;
   end;

   Root:=TpvVectorPathBVHDynamicAABBTree.NULLNODE;

   if Count>0 then begin

    FillStack.Initialize;
    try

     NewFillStackItem.Parent:=TpvVectorPathBVHDynamicAABBTree.NULLNODE;
     NewFillStackItem.Which:=-1;
     NewFillStackItem.LeafNodes:=copy(LeafNodes,0,Count);
     FillStack.Push(NewFillStackItem);

     while FillStack.Pop(FillStackItem) do begin

      case length(FillStackItem.LeafNodes) of

       0:begin
       end;

       1:begin
        NodeIndex:=FillStackItem.LeafNodes[0];
        ParentIndex:=FillStackItem.Parent;
        Nodes[NodeIndex].Parent:=ParentIndex;
        if (FillStackItem.Which>=0) and (ParentIndex>=0) then begin
         Nodes[ParentIndex].Children[FillStackItem.Which]:=NodeIndex;
        end else begin
         Root:=NodeIndex;
        end;
       end;
       else begin

        NodeIndex:=AllocateNode;

        ParentIndex:=FillStackItem.Parent;

        Nodes[NodeIndex].Parent:=ParentIndex;

        if (FillStackItem.Which>=0) and (ParentIndex>=0) then begin
         Nodes[ParentIndex].Children[FillStackItem.Which]:=NodeIndex;
        end else begin
         Root:=NodeIndex;
        end;

        AABB:=Nodes[FillStackItem.LeafNodes[0]].AABB;
        for Index:=1 to length(FillStackItem.LeafNodes)-1 do begin
         AABB:=AABB.Combine(Nodes[FillStackItem.LeafNodes[Index]].AABB);
        end;

        Nodes[NodeIndex].AABB:=AABB;

        MeanX:=0.0;
        MeanY:=0.0;
        for Index:=0 to length(FillStackItem.LeafNodes)-1 do begin
         Center:=Nodes[FillStackItem.LeafNodes[Index]].AABB.Center;
         MeanX:=MeanX+Center.x;
         MeanY:=MeanY+Center.y;
        end;
        MeanX:=MeanX/length(FillStackItem.LeafNodes);
        MeanY:=MeanY/length(FillStackItem.LeafNodes);

        VarianceX:=0.0;
        VarianceY:=0.0;
        for Index:=0 to length(FillStackItem.LeafNodes)-1 do begin
         Center:=Nodes[FillStackItem.LeafNodes[Index]].AABB.Center;
         VarianceX:=VarianceX+sqr(Center.x-MeanX);
         VarianceY:=VarianceY+sqr(Center.y-MeanY);
        end;
        VarianceX:=VarianceX/length(FillStackItem.LeafNodes);
        VarianceY:=VarianceY/length(FillStackItem.LeafNodes);

        if VarianceX<VarianceY then begin
         SplitAxis:=1;
         SplitValue:=MeanY;
        end else begin
         SplitAxis:=0;
         SplitValue:=MeanX;
        end;

        LeftIndex:=0;
        RightIndex:=length(FillStackItem.LeafNodes);
        LeftCount:=0;
        RightCount:=0;
        while LeftIndex<RightIndex do begin
         Center:=Nodes[FillStackItem.LeafNodes[LeftIndex]].AABB.Center;
         if Center.xy[SplitAxis]<=SplitValue then begin
          inc(LeftIndex);
          inc(LeftCount);
         end else begin
          dec(RightIndex);
          inc(RightCount);
          TempIndex:=FillStackItem.LeafNodes[LeftIndex];
          FillStackItem.LeafNodes[LeftIndex]:=FillStackItem.LeafNodes[RightIndex];
          FillStackItem.LeafNodes[RightIndex]:=TempIndex;
         end;
        end;

        MinPerSubTree:=(TpvInt64(length(FillStackItem.LeafNodes)+1)*341) shr 10;
        if (LeftCount=0) or
           (RightCount=0) or
           (LeftCount<=MinPerSubTree) or
           (RightCount<=MinPerSubTree) then begin
         RightIndex:=(length(FillStackItem.LeafNodes)+1) shr 1;
        end;

        begin
         NewFillStackItem.Parent:=NodeIndex;
         NewFillStackItem.Which:=1;
         NewFillStackItem.LeafNodes:=copy(FillStackItem.LeafNodes,RightIndex,length(FillStackItem.LeafNodes)-RightIndex);
         FillStack.Push(NewFillStackItem);
        end;

        begin
         NewFillStackItem.Parent:=NodeIndex;
         NewFillStackItem.Which:=0;
         NewFillStackItem.LeafNodes:=copy(FillStackItem.LeafNodes,0,RightIndex);
         FillStack.Push(NewFillStackItem);
        end;

        FillStackItem.LeafNodes:=nil;

       end;
      end;
     end;

    finally
     FillStack.Finalize;
    end;

    HeightStack.Initialize;
    try

     NewHeightStackItem.Node:=Root;
     NewHeightStackItem.Pass:=0;
     HeightStack.Push(NewHeightStackItem);

     while HeightStack.Pop(HeightStackItem) do begin
      case HeightStackItem.Pass of
       0:begin
        NewHeightStackItem.Node:=HeightStackItem.Node;
        NewHeightStackItem.Pass:=1;
        HeightStack.Push(NewHeightStackItem);
        if Nodes[HeightStackItem.Node].Children[1]>=0 then begin
         NewHeightStackItem.Node:=Nodes[HeightStackItem.Node].Children[1];
         NewHeightStackItem.Pass:=0;
         HeightStack.Push(NewHeightStackItem);
        end;
        if Nodes[HeightStackItem.Node].Children[0]>=0 then begin
         NewHeightStackItem.Node:=Nodes[HeightStackItem.Node].Children[0];
         NewHeightStackItem.Pass:=0;
         HeightStack.Push(NewHeightStackItem);
        end;
       end;
       1:begin
        if (Nodes[HeightStackItem.Node].Children[0]<0) and (Nodes[HeightStackItem.Node].Children[1]<0) then begin
         Nodes[HeightStackItem.Node].Height:=1;
        end else begin
         Nodes[HeightStackItem.Node].Height:=1+Max(Nodes[Nodes[HeightStackItem.Node].Children[0]].Height,Nodes[Nodes[HeightStackItem.Node].Children[1]].Height);
        end;
       end;
      end;
     end;

    finally
     HeightStack.Finalize;
    end;

   end;

  finally

   LeafNodes:=nil;

  end;

 end;

end;

procedure TpvVectorPathBVHDynamicAABBTree.Rebuild;
begin
 if NodeCount<128 then begin
  RebuildBottomUp;
 end else begin
  RebuildTopDown;
 end;
end;

function TpvVectorPathBVHDynamicAABBTree.ComputeHeight:TpvSizeInt;
type TStackItem=record
      NodeID:TpvSizeInt;
      Height:TpvSizeInt;
     end;
     TStack=TpvDynamicStack<TStackItem>;
var Stack:TStack;
    StackItem,NewStackItem:TStackItem;
    Node:TpvVectorPathBVHDynamicAABBTree.PTreeNode;
begin
 result:=0;
 if (NodeCount>0) and (Root>=0) then begin
  Stack.Initialize;
  try
   NewStackItem.NodeID:=Root;
   NewStackItem.Height:=1;
   Stack.Push(NewStackItem);
   while Stack.Pop(StackItem) do begin
    Node:=@Nodes[StackItem.NodeID];
    if result<StackItem.Height then begin
     result:=StackItem.Height;
    end;
    if Node^.Children[1]>=0 then begin
     NewStackItem.NodeID:=Node^.Children[1];
     NewStackItem.Height:=StackItem.Height+1;
     Stack.Push(NewStackItem);
    end;
    if Node^.Children[0]>=0 then begin
     NewStackItem.NodeID:=Node^.Children[0];
     NewStackItem.Height:=StackItem.Height+1;
     Stack.Push(NewStackItem);
    end;
   end;
  finally
   Stack.Finalize;
  end;
 end;
end;

function TpvVectorPathBVHDynamicAABBTree.GetHeight:TpvSizeInt;
begin
 if Root>=0 then begin
  result:=Nodes[Root].Height;
 end else begin
  result:=0;
 end;
end;

function TpvVectorPathBVHDynamicAABBTree.GetAreaRatio:TpvDouble;
var NodeID:TpvSizeInt;
    Node:TpvVectorPathBVHDynamicAABBTree.PTreeNode;
begin
 result:=0.0;
 if Root>=0 then begin
  for NodeID:=0 to NodeCount-1 do begin
   Node:=@Nodes[NodeID];
   if Node^.Height>=0 then begin
    result:=result+Node^.AABB.Cost;
   end;
  end;
  result:=result/Nodes[Root].AABB.Cost;
 end;
end;

function TpvVectorPathBVHDynamicAABBTree.GetMaxBalance:TpvSizeInt;
var NodeID,Balance:TpvSizeInt;
    Node:TpvVectorPathBVHDynamicAABBTree.PTreeNode;
begin
 result:=0;
 if Root>=0 then begin
  for NodeID:=0 to NodeCount-1 do begin
   Node:=@Nodes[NodeID];
   if (Node^.Height>1) and (Node^.Children[0]>=0) and (Node^.Children[1]>=0) then begin
    Balance:=abs(Nodes[Node^.Children[0]].Height-Nodes[Node^.Children[1]].Height);
    if result<Balance then begin
     result:=Balance;
    end;
   end;
  end;
 end;
end;

function TpvVectorPathBVHDynamicAABBTree.ValidateStructure:boolean;
type TStackItem=record
      NodeID:TpvSizeInt;
      Parent:TpvSizeInt;
     end;
     TStack=TpvDynamicStack<TStackItem>;
var Stack:TStack;
    StackItem,NewStackItem:TStackItem;
    Node:TpvVectorPathBVHDynamicAABBTree.PTreeNode;
begin
 result:=true;
 if (NodeCount>0) and (Root>=0) and (Root<NodeCount) then begin
  Stack.Initialize;
  try
   NewStackItem.NodeID:=Root;
   NewStackItem.Parent:=TpvVectorPathBVHDynamicAABBTree.NULLNODE;
   Stack.Push(NewStackItem);
   while Stack.Pop(StackItem) do begin
    Node:=@Nodes[StackItem.NodeID];
    if Node^.Parent<>StackItem.Parent then begin
     result:=false;
     break;
    end else begin
     if (Node^.Children[1]>=0) and (Node^.Children[1]<NodeCount) then begin
      NewStackItem.NodeID:=Node^.Children[1];
      NewStackItem.Parent:=StackItem.NodeID;
      Stack.Push(NewStackItem);
     end;
     if (Node^.Children[0]>=0) and (Node^.Children[0]<NodeCount) then begin
      NewStackItem.NodeID:=Node^.Children[0];
      NewStackItem.Parent:=StackItem.NodeID;
      Stack.Push(NewStackItem);
     end;
    end;
   end;
  finally
   Stack.Finalize;
  end;
 end;
end;

function TpvVectorPathBVHDynamicAABBTree.ValidateMetrics:boolean;
type TStackItem=record
      NodeID:TpvSizeInt;
     end;
     TStack=TpvDynamicStack<TStackItem>;
var Stack:TStack;
    StackItem,NewStackItem:TStackItem;
    Node:TpvVectorPathBVHDynamicAABBTree.PTreeNode;
begin
 result:=true;
 if (NodeCount>0) and (Root>=0) and (Root<NodeCount) then begin
  Stack.Initialize;
  try
   NewStackItem.NodeID:=Root;
   Stack.Push(NewStackItem);
   while Stack.Pop(StackItem) do begin
    Node:=@Nodes[StackItem.NodeID];
    if (((Node^.Children[0]<0) or (Node^.Children[0]>=NodeCount)) or
        ((Node^.Children[1]<0) or (Node^.Children[1]>=NodeCount))) or
       (Node^.Height<>(1+Max(Nodes[Node^.Children[0]].Height,Nodes[Node^.Children[1]].Height))) then begin
     result:=false;
     break;
    end else begin
     if (Node^.Children[1]>=0) and (Node^.Children[1]<NodeCount) then begin
      NewStackItem.NodeID:=Node^.Children[1];
      Stack.Push(NewStackItem);
     end;
     if (Node^.Children[0]>=0) and (Node^.Children[0]<NodeCount) then begin
      NewStackItem.NodeID:=Node^.Children[0];
      Stack.Push(NewStackItem);
     end;
    end;
   end;
  finally
   Stack.Finalize;
  end;
 end;
end;

function TpvVectorPathBVHDynamicAABBTree.Validate:boolean;
var NodeID,FreeCount:TpvSizeInt;
begin
 result:=ValidateStructure;
 if result then begin
  result:=ValidateMetrics;
  if result then begin
   result:=ComputeHeight=GetHeight;
   if result then begin
    NodeID:=FreeList;
    FreeCount:=0;
    while NodeID>=0 do begin
     NodeID:=Nodes[NodeID].Next;
     inc(FreeCount);
    end;
    result:=(NodeCount+FreeCount)=NodeCapacity;
   end;
  end;
 end;
end;

function TpvVectorPathBVHDynamicAABBTree.IntersectionQuery(const aAABB:TpvVectorPathBoundingBox):TpvVectorPathBVHDynamicAABBTree.TUserDataArray;
type TStackItem=record
      NodeID:TpvSizeInt;
     end;
     TStack=TpvDynamicStack<TStackItem>;
var Stack:TStack;
    StackItem,NewStackItem:TStackItem;
    Node:TpvVectorPathBVHDynamicAABBTree.PTreeNode;
begin
 result:=nil;
 if (NodeCount>0) and (Root>=0) then begin
  Stack.Initialize;
  try
   NewStackItem.NodeID:=Root;
   Stack.Push(NewStackItem);
   while Stack.Pop(StackItem) do begin
    Node:=@Nodes[StackItem.NodeID];
    if Node^.AABB.Intersect(aAABB) then begin
     if Node^.UserData<>0 then begin
      result:=result+[Node^.UserData];
     end;
     if (Node^.Children[1]>=0) then begin
      NewStackItem.NodeID:=Node^.Children[1];
      Stack.Push(NewStackItem);
     end;
     if (Node^.Children[0]>=0) then begin
      NewStackItem.NodeID:=Node^.Children[0];
      Stack.Push(NewStackItem);
     end;
    end;
   end;
  finally
   Stack.Finalize;
  end;
 end;
end;

function TpvVectorPathBVHDynamicAABBTree.ContainQuery(const aAABB:TpvVectorPathBoundingBox):TpvVectorPathBVHDynamicAABBTree.TUserDataArray;
type TStackItem=record
      NodeID:TpvSizeInt;
     end;
     TStack=TpvDynamicStack<TStackItem>;
var Stack:TStack;
    StackItem,NewStackItem:TStackItem;
    Node:TpvVectorPathBVHDynamicAABBTree.PTreeNode;
begin
 result:=nil;
 if (NodeCount>0) and (Root>=0) then begin
  Stack.Initialize;
  try
   NewStackItem.NodeID:=Root;
   Stack.Push(NewStackItem);
   while Stack.Pop(StackItem) do begin
    Node:=@Nodes[StackItem.NodeID];
    if Node^.AABB.Contains(aAABB) then begin
     if Node^.UserData<>0 then begin
      result:=result+[Node^.UserData];
     end;
     if (Node^.Children[1]>=0) then begin
      NewStackItem.NodeID:=Node^.Children[1];
      Stack.Push(NewStackItem);
     end;
     if (Node^.Children[0]>=0) then begin
      NewStackItem.NodeID:=Node^.Children[0];
      Stack.Push(NewStackItem);
     end;
    end;
   end;
  finally
   Stack.Finalize;
  end;
 end;
end;

function TpvVectorPathBVHDynamicAABBTree.ContainQuery(const aPoint:TpvVectorPathVector):TpvVectorPathBVHDynamicAABBTree.TUserDataArray;
type TStackItem=record
      NodeID:TpvSizeInt;
     end;
     TStack=TpvDynamicStack<TStackItem>;
var Stack:TStack;
    StackItem,NewStackItem:TStackItem;
    Node:TpvVectorPathBVHDynamicAABBTree.PTreeNode;
begin
 result:=nil;
 if (NodeCount>0) and (Root>=0) then begin
  Stack.Initialize;
  try
   NewStackItem.NodeID:=Root;
   Stack.Push(NewStackItem);
   while Stack.Pop(StackItem) do begin
    Node:=@Nodes[StackItem.NodeID];
    if Node^.AABB.Contains(aPoint) then begin
     if Node^.UserData<>0 then begin
      result:=result+[Node^.UserData];
     end;
     if (Node^.Children[1]>=0) then begin
      NewStackItem.NodeID:=Node^.Children[1];
      Stack.Push(NewStackItem);
     end;
     if (Node^.Children[0]>=0) then begin
      NewStackItem.NodeID:=Node^.Children[0];
      Stack.Push(NewStackItem);
     end;
    end;
   end;
  finally
   Stack.Finalize;
  end;
 end;
end;

function TpvVectorPathBVHDynamicAABBTree.RayCast(const aRayOrigin,aRayDirection:TpvVectorPathVector;out aTime:TpvDouble;out aUserData:TpvUInt32;const aStopAtFirstHit:boolean;const aRayCastUserData:TpvVectorPathBVHDynamicAABBTree.TRayCastUserData):boolean;
type TStackItem=record
      NodeID:TpvSizeInt;
     end;
     TStack=TpvDynamicStack<TStackItem>;
var Stack:TStack;
    StackItem,NewStackItem:TStackItem;
    Node:TpvVectorPathBVHDynamicAABBTree.PTreeNode;
    RayEnd:TpvVectorPathVector;
    Time:TpvDouble;
    Stop:boolean;
begin
 result:=false;
 if assigned(aRayCastUserData) and (NodeCount>0) and (Root>=0) then begin
  aTime:=Infinity;
  RayEnd:=aRayOrigin;
  Stack.Initialize;
  try
   NewStackItem.NodeID:=Root;
   Stack.Push(NewStackItem);
   while Stack.Pop(StackItem) do begin
    Node:=@Nodes[StackItem.NodeID];
    if ((not result) and
        (Node^.AABB.Contains(aRayOrigin) or Node^.AABB.FastRayIntersection(aRayOrigin,aRayDirection))) or
       (result and Node^.AABB.LineIntersection(aRayOrigin,RayEnd)) then begin
     if (Node^.UserData<>0) and aRayCastUserData(Node^.UserData,aRayOrigin,aRayDirection,Time,Stop) then begin
      if (not result) or (Time<aTime) then begin
       aTime:=Time;
       aUserData:=Node^.UserData;
       result:=true;
       if aStopAtFirstHit or Stop then begin
        break;
       end else begin
        RayEnd:=aRayOrigin+(aRayDirection*Time);
       end;
      end;
     end;
     if (Node^.Children[1]>=0) then begin
      NewStackItem.NodeID:=Node^.Children[1];
      Stack.Push(NewStackItem);
     end;
     if (Node^.Children[0]>=0) then begin
      NewStackItem.NodeID:=Node^.Children[0];
      Stack.Push(NewStackItem);
     end;
    end;
   end;
  finally
   Stack.Finalize;
  end;
 end;
end;

function TpvVectorPathBVHDynamicAABBTree.RayCastLine(const aFrom,aTo:TpvVectorPathVector;out aTime:TpvDouble;out aUserData:TpvUInt32;const aStopAtFirstHit:boolean;const aRayCastUserData:TpvVectorPathBVHDynamicAABBTree.TRayCastUserData):boolean;
type TStackItem=record
      NodeID:TpvSizeInt;
     end;
     TStack=TpvDynamicStack<TStackItem>;
var Stack:TStack;
    StackItem,NewStackItem:TStackItem;
    Node:TpvVectorPathBVHDynamicAABBTree.PTreeNode;
    Time,RayLength:TpvDouble;
    RayOrigin,RayDirection,RayEnd:TpvVectorPathVector;
    Stop:boolean;
begin
 result:=false;
 if assigned(aRayCastUserData) and (NodeCount>0) and (Root>=0) then begin
  aTime:=Infinity;
  RayOrigin:=aFrom;
  RayEnd:=aTo;
  RayDirection:=(RayEnd-RayOrigin).Normalize;
  RayLength:=(RayEnd-RayOrigin).Length;
  Stack.Initialize;
  try
   NewStackItem.NodeID:=Root;
   Stack.Push(NewStackItem);
   while Stack.Pop(StackItem) do begin
    Node:=@Nodes[StackItem.NodeID];
    if Node^.AABB.LineIntersection(RayOrigin,RayEnd) then begin
     if (Node^.UserData<>0) and aRayCastUserData(Node^.UserData,RayOrigin,RayDirection,Time,Stop) then begin
      if ((Time>=0.0) and (Time<=RayLength)) and ((not result) or (Time<aTime)) then begin
       aTime:=Time;
       aUserData:=Node^.UserData;
       result:=true;
       if aStopAtFirstHit or Stop then begin
        break;
       end else begin
        RayEnd:=RayOrigin+(RayDirection*Time);
        RayLength:=Time;
       end;
      end;
     end;
     if (Node^.Children[1]>=0) then begin
      NewStackItem.NodeID:=Node^.Children[1];
      Stack.Push(NewStackItem);
     end;
     if (Node^.Children[0]>=0) then begin
      NewStackItem.NodeID:=Node^.Children[0];
      Stack.Push(NewStackItem);
     end;
    end;
   end;
  finally
   Stack.Finalize;
  end;
 end;
end;

procedure TpvVectorPathBVHDynamicAABBTree.GetSkipListNodes(var aSkipListNodeArray:TSkipListNodeArray;const aGetUserDataIndex:TpvVectorPathBVHDynamicAABBTree.TGetUserDataIndex);
//const ThresholdVector:TpvVectorPathVector=(x:1e-7;y:1e-7);
var StackItem,NewStackItem:TSkipListNodeStackItem;
    Node:PTreeNode;
    SkipListNode:TSkipListNode;
    SkipListNodeIndex:TpvSizeInt;
begin
 fSkipListNodeLock.Acquire;
 try
  if Root>=0 then begin
   if length(fSkipListNodeMap)<length(Nodes) then begin
    SetLength(fSkipListNodeMap,(length(Nodes)*3) shr 1);
   end;
   aSkipListNodeArray.Count:=0;
   NewStackItem.Pass:=0;
   NewStackItem.Node:=Root;
   fSkipListNodeStack.Push(NewStackItem);
   while fSkipListNodeStack.Pop(StackItem) do begin
    case StackItem.Pass of
     0:begin
      if StackItem.Node>=0 then begin
       Node:=@Nodes[StackItem.Node];
       SkipListNode.AABBMin.x:=Node^.AABB.Min.x;
       SkipListNode.AABBMin.y:=Node^.AABB.Min.y;
       SkipListNode.AABBMax.x:=Node^.AABB.Max.x;
       SkipListNode.AABBMax.y:=Node^.AABB.Max.y;
       SkipListNode.SkipCount:=0;
       if Node^.UserData<>0 then begin
        if assigned(aGetUserDataIndex) then begin
         SkipListNode.UserData:=aGetUserDataIndex(Node^.UserData);
        end else begin
         SkipListNode.UserData:=Node^.UserData;
        end;
       end else begin
        SkipListNode.UserData:=High(TpvUInt32);
       end;
       SkipListNodeIndex:=aSkipListNodeArray.Add(SkipListNode);
       fSkipListNodeMap[StackItem.Node]:=SkipListNodeIndex;
       NewStackItem.Pass:=1;
       NewStackItem.Node:=StackItem.Node;
       fSkipListNodeStack.Push(NewStackItem);
       if Node^.Children[1]>=0 then begin
        NewStackItem.Pass:=0;
        NewStackItem.Node:=Node^.Children[1];
        fSkipListNodeStack.Push(NewStackItem);
       end;
       if Node^.Children[0]>=0 then begin
        NewStackItem.Pass:=0;
        NewStackItem.Node:=Node^.Children[0];
        fSkipListNodeStack.Push(NewStackItem);
       end;
      end;
     end;
     1:begin
      if StackItem.Node>=0 then begin
       SkipListNodeIndex:=fSkipListNodeMap[StackItem.Node];
       aSkipListNodeArray.Items[SkipListNodeIndex].SkipCount:=aSkipListNodeArray.Count-SkipListNodeIndex;
      end;
     end;
    end;
   end;
  end;
 finally
  fSkipListNodeLock.Release;
 end;
end;

{ TpvVectorPathCommand }

constructor TpvVectorPathCommand.Create(const aCommandType:TpvVectorPathCommandType;
                                        const aX0:TpvDouble=0.0;
                                        const aY0:TpvDouble=0.0;
                                        const aX1:TpvDouble=0.0;
                                        const aY1:TpvDouble=0.0;
                                        const aX2:TpvDouble=0.0;
                                        const aY2:TpvDouble=0.0);
begin
 inherited Create;
 fCommandType:=aCommandType;
 fX0:=aX0;
 fY0:=aY0;
 fX1:=aX1;
 fY1:=aY1;
 fX2:=aX2;
 fY2:=aY2;
end;

procedure GetIntersectionPointsForLineLine(const aSegment0,aSegment1:TpvVectorPathSegmentLine;const aIntersectionPoints:TpvVectorPathVectorList);
var a,b,Determinant:TpvDouble;
begin
 Determinant:=((aSegment1.Points[1].y-aSegment1.Points[0].y)*(aSegment0.Points[1].x-aSegment0.Points[0].x))-((aSegment1.Points[1].x-aSegment1.Points[0].x)*(aSegment0.Points[1].y-aSegment0.Points[0].y));
 if not IsZero(Determinant) then begin
  a:=(((aSegment1.Points[1].x-aSegment1.Points[0].x)*(aSegment0.Points[0].y-aSegment1.Points[0].y))-((aSegment1.Points[1].y-aSegment1.Points[0].y)*(aSegment0.Points[0].x-aSegment1.Points[0].x)))/Determinant;
  b:=(((aSegment0.Points[1].x-aSegment0.Points[0].x)*(aSegment0.Points[0].y-aSegment1.Points[0].y))-((aSegment0.Points[1].y-aSegment0.Points[0].y)*(aSegment0.Points[0].x-aSegment1.Points[0].x)))/Determinant;
  if ((a>=0.0) and (a<=1.0)) and ((b>=0.0) and (b<=1.0)) then begin
   aIntersectionPoints.Add(aSegment0.Points[0].Lerp(aSegment0.Points[1],a));
  end;
 end;
end;

procedure GetIntersectionPointsForLineQuadraticCurve(const aSegment0:TpvVectorPathSegmentLine;const aSegment1:TpvVectorPathSegmentQuadraticCurve;const aIntersectionPoints:TpvVectorPathVectorList);
var Min_,Max_,c0,c1,c2,n,p:TpvVectorPathVector;
    {a,}cl,t:TpvDouble;
    Roots:TpvDoubleDynamicArray;
//  Roots:array[0..1] of TpvDouble;
    RootIndex,CountRoots:TpvSizeInt;
begin
 Min_:=aSegment0.Points[0].Minimum(aSegment0.Points[1]);
 Max_:=aSegment0.Points[0].Maximum(aSegment0.Points[1]);
 c2:=aSegment1.Points[0]+((aSegment1.Points[1]*(-2.0))+aSegment1.Points[2]);
 c1:=(aSegment1.Points[0]*(-2.0))+(aSegment1.Points[1]*2.0);
 c0:=TpvVectorPathVector.Create(aSegment1.Points[0].x,aSegment1.Points[0].y);
 n:=TpvVectorPathVector.Create(aSegment0.Points[0].y-aSegment0.Points[1].y,aSegment0.Points[1].x-aSegment0.Points[0].x);
 cl:=(aSegment0.Points[0].x*aSegment0.Points[1].y)-(aSegment0.Points[1].x*aSegment0.Points[0].y);
 // TpvPolynomial keeps its coefficients with the constant term first and the leading one last, so
 // the powers go in ascending order.
 Roots:=(TpvPolynomial.Create([n.Dot(c0)+cl,n.Dot(c1),n.Dot(c2)])).GetRoots;
 try
  CountRoots:=length(Roots);
 {a:=n.Dot(c0)+cl;
  if IsZero(a) then begin
   CountRoots:=0;
  end else begin
   CountRoots:=SolveQuadratic(a,n.Dot(c1)/a,n.Dot(c2)/a,Roots[0],Roots[1]);
  end;}
  for RootIndex:=0 to CountRoots-1 do begin
   t:=Roots[RootIndex];
   if (t>=0.0) and (t<=1.0) then begin
    p:=(aSegment1.Points[0].Lerp(aSegment1.Points[1],t)).Lerp(aSegment1.Points[1].Lerp(aSegment1.Points[2],t),t);
    if SameValue(aSegment0.Points[0].x,aSegment0.Points[1].x) then begin
     if (p.y>=Min_.y) and (p.y<=Max_.y) then begin
      aIntersectionPoints.Add(p);
     end;
    end else if SameValue(aSegment0.Points[0].y,aSegment0.Points[1].y) then begin
     if (p.x>=Min_.x) and (p.x<=Max_.x) then begin
      aIntersectionPoints.Add(p);
     end;
    end else if ((p.x>=Min_.x) and (p.x<=Max_.x)) and ((p.y>=Min_.y) and (p.y<=Max_.y)) then begin
     aIntersectionPoints.Add(p);
    end;
   end;
  end;
 finally
  Roots:=nil;
 end;
end;

procedure GetIntersectionPointsForLineCubicCurve(const aSegment0:TpvVectorPathSegmentLine;const aSegment1:TpvVectorPathSegmentCubicCurve;const aIntersectionPoints:TpvVectorPathVectorList);
var Min_,Max_,c0,c1,c2,c3,n,p,p1,p2,p3,p4,p5,p6,p7,p8,p9:TpvVectorPathVector;
    {a,}cl,t:TpvDouble;
    Roots:TpvDoubleDynamicArray;
//  Roots:array[0..2] of TpvDouble;
    RootIndex,CountRoots:TpvSizeInt;
begin
 Min_:=aSegment0.Points[0].Minimum(aSegment0.Points[1]);
 Max_:=aSegment0.Points[0].Maximum(aSegment0.Points[1]);
 p1:=aSegment1.Points[0];
 p2:=aSegment1.Points[1];
 p3:=aSegment1.Points[2];
 p4:=aSegment1.Points[3];
 c0:=p1;
 c1:=(p1*(-3.0))+(p2*3.0);
 c2:=(p1*3.0)+((p2*(-6.0))+(p3*3.0));
 c3:=(p1*(-1.0))+((p2*3.0)+((p3*(-3.0))+p4));
 n:=TpvVectorPathVector.Create(aSegment0.Points[0].y-aSegment0.Points[1].y,aSegment0.Points[1].x-aSegment0.Points[0].x);
 cl:=(aSegment0.Points[0].x*aSegment0.Points[1].y)-(aSegment0.Points[1].x*aSegment0.Points[0].y);
 Roots:=(TpvPolynomial.Create([n.Dot(c0)+cl,n.Dot(c1),n.Dot(c2),n.Dot(c3)])).GetRoots;
 try
  CountRoots:=length(Roots);
 {a:=n.Dot(c0)+cl;
  if IsZero(a) then begin
   CountRoots:=0;
  end else begin
   CountRoots:=SolveCubic(a,n.Dot(c1),n.Dot(c2),n.Dot(c3),Roots[0],Roots[1],Roots[2]);
  end;}
  for RootIndex:=0 to CountRoots-1 do begin
   t:=Roots[RootIndex];
   if (t>=0.0) and (t<=1.0) then begin
    p5:=p1.Lerp(p2,t);
    p6:=p2.Lerp(p3,t);
    p7:=p3.Lerp(p4,t);
    p8:=p5.Lerp(p6,t);
    p9:=p6.Lerp(p7,t);
    p:=p8.Lerp(p9,t);
    if SameValue(aSegment0.Points[0].x,aSegment0.Points[1].x) then begin
     if (p.y>=Min_.y) and (p.y<=Max_.y) then begin
      aIntersectionPoints.Add(p);
     end;
    end else if SameValue(aSegment0.Points[0].y,aSegment0.Points[1].y) then begin
     if (p.x>=Min_.x) and (p.x<=Max_.x) then begin
      aIntersectionPoints.Add(p);
     end;
    end else if ((p.x>=Min_.x) and (p.x<=Max_.x)) and ((p.y>=Min_.y) and (p.y<=Max_.y)) then begin
     aIntersectionPoints.Add(p);
    end;
   end;
  end;
 finally
  Roots:=nil;
 end;
end;

procedure GetIntersectionPointsForQuadraticCurveQuadraticCurve(const aSegment0,aSegment1:TpvVectorPathSegmentQuadraticCurve;const aIntersectionPoints:TpvVectorPathVectorList);
var a1,a2,a3,b1,b2,b3,c10,c11,c12,c20,c21,c22:TpvVectorPathVector;
    v0,v1,v2,v3,v4,v5,v6,s,XRoot:TpvDouble;
    Roots,XRoots,YRoots:TpvDoubleDynamicArray;
//  Roots:array[0..3] of TpvDouble;
//  XRoots,YRoots:array[0..1] of TpvDouble;
    CountRoots,CountXRoots,CountYRoots,Index,XIndex,YIndex:TpvSizeInt;
    OK:boolean;
begin
 a1:=aSegment0.Points[0];
 a2:=aSegment0.Points[1];
 a3:=aSegment0.Points[2];
 b1:=aSegment1.Points[0];
 b2:=aSegment1.Points[1];
 b3:=aSegment1.Points[2];
 c10:=a1;
 c11:=(a1*(-2.0))+(a2*2.0);
 c12:=a1+((a2*(-2.0))+a3);
 c20:=b1;
 c21:=(b1*(-2.0))+(b2*2.0);
 c22:=b1+((b2*(-2.0))+b3);
 if IsZero(c12.y) then begin
  v0:=c12.x*(c10.y-c20.y);
  v1:=v0-(c11.x*c11.y);
  v2:=v0+v1;
  v3:=c11.y*c11.y;
  Roots:=(TpvPolynomial.Create([((c10.x-c20.x)*v3)+((c10.y-c20.y)*v1),
                               (((-c21.x)*v3)-(c21.y*v0))-(c21.y*v1),
                               (((c12.x*c21.y*c21.y)-(c22.x*v3))-(c22.y*v0))-(c22.y*v1),
                               2.0*c12.x*c21.y*c22.y,
                               c12.x*c22.y*c22.y])).GetRoots;
{ CountRoots:=SolveQuartic(c12.x*c22.y*c22.y,
                           2.0*c12.x*c21.y*c22.y,
                           (((c12.x*c21.y*c21.y)-(c22.x*v3))-(c22.y*v0))-(c22.y*v1),
                           (((-c21.x)*v3)-(c21.y*v0))-(c21.y*v1),
                           ((c10.x-c20.x)*v3)+((c10.y-c20.y)*v1),
                           Roots[0],
                           Roots[1],
                           Roots[2],
                           Roots[3]);}
 end else begin
  v0:=(c12.x*c22.y)-(c12.y*c22.x);
  v1:=(c12.x*c21.y)-(c21.x*c12.y);
  v2:=(c11.x*c12.y)-(c11.y*c12.x);
  v3:=c10.y-c20.y;
  v4:=(c12.y*(c10.x-c20.x))-(c12.x*v3);
  v5:=((-c11.y)*v2)+(c12.y*v4);
  v6:=v2*v2;
  Roots:=(TpvPolynomial.Create([((v3*v6)+(v4*v5))/c12.y,
                                ((((-c21.y)*v6)+(c12.y*v1*v4))+(v1*v5))/c12.y,
                                ((((-c22.y)*v6)+(c12.y*v1*v1))+(c12.y*v0*v4)+(v0*v5))/c12.y,
                                2.0*v0*v1,
                                sqr(v0)])).GetRoots;
{ CountRoots:=SolveQuartic(sqr(v0),
                           2.0*v0*v1,
                           ((((-c22.y)*v6)+(c12.y*v1*v1))+(c12.y*v0*v4)+(v0*v5))/c12.y,
                           ((((-c21.y)*v6)+(c12.y*v1*v4))+(v1*v5))/c12.y,
                           ((v3*v6)+(v4*v5))/c12.y,
                           Roots[0],
                           Roots[1],
                           Roots[2],
                           Roots[3]);}
 end;
 XRoots:=nil;
 YRoots:=nil;
 try
  CountRoots:=length(Roots);
  for Index:=0 to CountRoots-1 do begin
   s:=Roots[Index];
   if (s>=0.0) and (s<=1.0) then begin
    XRoots:=(TpvPolynomial.Create([((c10.x-c20.x)-(s*c21.x))-(sqr(s)*c22.x),
                                   c11.x,
                                   c12.x])).GetRoots;
    YRoots:=(TpvPolynomial.Create([((c10.y-c20.y)-(s*c21.y))-(sqr(s)*c22.y),
                                   c11.y,
                                   c12.y])).GetRoots;
    CountXRoots:=length(XRoots);
    CountYRoots:=length(YRoots);
{   CountXRoots:=SolveQuadratic(c12.x,
                                c11.x,
                                ((c10.x-c20.x)-(s*c21.x))-(sqr(s)*c22.x),
                                XRoots[0],
                                XRoots[1]
                               );
    CountYRoots:=SolveQuadratic(c12.y,
                                c11.y,
                                ((c10.y-c20.y)-(s*c21.y))-(sqr(s)*c22.y),
                                YRoots[0],
                                YRoots[1]
                               );}
    if (CountXRoots>0) and (CountYRoots>0) then begin
     OK:=false;
     for XIndex:=0 to CountXRoots-1 do begin
      XRoot:=XRoots[XIndex];
      if (XRoot>=0.0) and (XRoot<=1.0) then begin
       for YIndex:=0 to CountYRoots-1 do begin
        if SameValue(XRoot,YRoots[XIndex],1e-4) then begin
         aIntersectionPoints.Add((c22*sqr(s))+(c21*s)+c20);
         OK:=true;
         break;
        end;
       end;
      end;
      if OK then begin
       break;
      end;
     end;
    end;
   end;
  end;
 finally
  Roots:=nil;
  XRoots:=nil;
  YRoots:=nil;
 end;
end;

procedure GetIntersectionPointsForQuadraticCurveCubicCurve(const aSegment0:TpvVectorPathSegmentQuadraticCurve;const aSegment1:TpvVectorPathSegmentCubicCurve;const aIntersectionPoints:TpvVectorPathVectorList);
var a1,a2,a3,b1,b2,b3,b4,
    c10,c11,c12,c20,c21,c22,c23,
    c10s,c11s,c12s,c20s,c21s,c22s,c23s:TpvVectorPathVector;
    PolyCoefs:array[0..6] of TpvDouble;
    Roots,XRoots,YRoots:TpvDoubleDynamicArray;
    //XRoots,YRoots:array[0..1] of TpvDouble;
    CountXRoots,CountYRoots,Index,XIndex,YIndex:TpvSizeInt;
    OK:boolean;
    s,XRoot:TpvDouble;
begin
 a1:=aSegment0.Points[0];
 a2:=aSegment0.Points[1];
 a3:=aSegment0.Points[2];
 b1:=aSegment1.Points[0];
 b2:=aSegment1.Points[1];
 b3:=aSegment1.Points[2];
 b4:=aSegment1.Points[3];
 c10:=a1;
 c11:=(a1*(-2.0))+(a2*2.0);
 c12:=(a1+(a2*(-2.0)))+a3;
 c20:=b1;
 c21:=(b1*(-3.0))+(b2*3.0);
 c22:=((b1*3.0)+(b2*(-6.0)))+(b3*3.0);
 c23:=(((b1*(-1.0))+(b2*3.0))+(b3*(-3.0)))+b4;
 c10s:=c10*c10;
 c11s:=c11*c11;
 c12s:=c12*c12;
 c20s:=c20*c20;
 c21s:=c21*c21;
 c22s:=c22*c22;
 c23s:=c23*c23;
 PolyCoefs[0]:=(((-2.0)*c12.x*c12.y*c23.x*c23.y)+(c12s.x*c23s.y))+(c12s.y*c23s.x);
 PolyCoefs[1]:=((((-2.0)*c12.x*c12.y*c22.x*c23.y)-(2.0*c12.x*c12.y*c22.y*c23.x))+(2.0*c12s.y*c22.x*c23.x))+(2.0*c12s.x*c22.y*c23.y);
 PolyCoefs[2]:=(((((((-2.0)*c12.x*c21.x*c12.y*c23.y)-(2.0*c12.x*c12.y*c21.y*c23.x))-(2.0*c12.x*c12.y*c22.x*c22.y))+(2.0*c21.x*c12s.y*c23.x))+(c12s.y*c22s.x))+(c12s.x*((2.0*c21.y*c23.y)+c22s.y)));
 PolyCoefs[3]:=(((((((((((((((2.0*c10.x*c12.x*c12.y*c23.y)+(2.0*c10.y*c12.x*c12.y*c23.x))+(c11.x*c11.y*c12.x*c23.y))+(c11.x*c11.y*c12.y*c23.x))-(2.0*c20.x*c12.x*c12.y*c23.y))-(2.0*c12.x*c20.y*c12.y*c23.x))-(2.0*c12.x*c21.x*c12.y*c22.y))-(2.0*c12.x*c12.y*c21.y*c22.x))-(2.0*c10.x*c12s.y*c23.x))-(2.0*c10.y*c12s.x*c23.y))+(2.0*c20.x*c12s.y*c23.x))+(2.0*c21.x*c12s.y*c22.x))-(c11s.y*c12.x*c23.x))-(c11s.x*c12.y*c23.y))+(c12s.x*((2.0*c20.y*c23.y)+(2.0*c21.y*c22.y))));
 PolyCoefs[4]:=((((((((((((((2.0*c10.x*c12.x*c12.y*c22.y)+(2.0*c10.y*c12.x*c12.y*c22.x))+(c11.x*c11.y*c12.x*c22.y))+(c11.x*c11.y*c12.y*c22.x))-(2.0*c20.x*c12.x*c12.y*c22.y))-(2.0*c12.x*c20.y*c12.y*c22.x))-(2.0*c12.x*c21.x*c12.y*c21.y))-(2.0*c10.x*c12s.y*c22.x))-(2.0*c10.y*c12s.x*c22.y))+(2.0*c20.x*c12s.y*c22.x))-(c11s.y*c12.x*c22.x))-(c11s.x*c12.y*c22.y))+(c21s.x*c12s.y))+(c12s.x*((2.0*c20.y*c22.y)+c21s.y)));
 PolyCoefs[5]:=(((((((((((2.0*c10.x*c12.x*c12.y*c21.y)+(2.0*c10.y*c12.x*c21.x*c12.y))+(c11.x*c11.y*c12.x*c21.y))+(c11.x*c11.y*c21.x*c12.y)-(2.0*c20.x*c12.x*c12.y*c21.y))-(2.0*c12.x*c20.y*c21.x*c12.y))-(2.0*c10.x*c21.x*c12s.y))-(2.0*c10.y*c12s.x*c21.y))+(2.0*c20.x*c21.x*c12s.y))-(c11s.y*c12.x*c21.x))-(c11s.x*c12.y*c21.y))+(2.0*c12s.x*c20.y*c21.y));
 PolyCoefs[6]:=(((((((((((((((((((-2.0)*c10.x*c10.y*c12.x*c12.y)-(c10.x*c11.x*c11.y*c12.y))-(c10.y*c11.x*c11.y*c12.x))+(2.0*c10.x*c12.x*c20.y*c12.y))+(2.0*c10.y*c20.x*c12.x*c12.y))+(c11.x*c20.x*c11.y*c12.y))+(c11.x*c11.y*c12.x*c20.y))-(2.0*c20.x*c12.x*c20.y*c12.y))-(2.0*c10.x*c20.x*c12s.y))+(c10.x*c11s.y*c12.x))+(c10.y*c11s.x*c12.y))-(2.0*c10.y*c12s.x*c20.y))-(c20.x*c11s.y*c12.x))-(c11s.x*c20.y*c12.y))+(c10s.x*c12s.y))+(c10s.y*c12s.x))+(c20s.x*c12s.y))+(c12s.x*c20s.y));
 // PolyCoefs runs from the highest power down, the polynomial wants it the other way round.
 Roots:=(TpvPolynomial.Create([PolyCoefs[6],PolyCoefs[5],PolyCoefs[4],PolyCoefs[3],PolyCoefs[2],PolyCoefs[1],PolyCoefs[0]])).GetRootsInInterval(0.0,1.0);
//Roots:=SolveRootsInInterval(PolyCoefs,0.0,1.0);
 XRoots:=nil;
 YRoots:=nil;
 try
  for Index:=0 to length(Roots)-1 do begin
   s:=Roots[Index];
   if (s>=0.0) and (s<=1.0) then begin
    XRoots:=(TpvPolynomial.Create([(((c10.x-c20.x)-(s*c21.x))-(sqr(s)*c22.x))-((sqr(s)*s)*c23.x),
                                   c11.x,
                                   c12.x])).GetRoots;
    YRoots:=(TpvPolynomial.Create([(((c10.y-c20.y)-(s*c21.y))-(sqr(s)*c22.y))-((sqr(s)*s)*c23.y),
                                   c11.y,
                                   c12.y])).GetRoots;
    CountXRoots:=length(XRoots);
    CountYRoots:=length(YRoots);
 {  CountXRoots:=SolveQuadratic(c12.x,
                                c11.x,
                                (((c10.x-c20.x)-(s*c21.x))-(sqr(s)*c22.x))-((sqr(s)*s)*c23.x),
                                XRoots[0],
                                XRoots[1]
                               );
    CountYRoots:=SolveQuadratic(c12.y,
                                c11.y,
                                (((c10.x-c20.y)-(s*c21.y))-(sqr(s)*c22.y))-((sqr(s)*s)*c23.y),
                                YRoots[0],
                                YRoots[1]
                               );}
    if (CountXRoots>0) and (CountYRoots>0) then begin
     OK:=false;
     for XIndex:=0 to CountXRoots-1 do begin
      XRoot:=XRoots[XIndex];
      if (XRoot>=0.0) and (XRoot<=1.0) then begin
       for YIndex:=0 to CountYRoots-1 do begin
        if SameValue(XRoot,YRoots[XIndex],1e-4) then begin
         aIntersectionPoints.Add((c23*(sqr(s)*s))+(c22*sqr(s))+(c21*s)+c20);
         OK:=true;
         break;
        end;
       end;
      end;
      if OK then begin
       break;
      end;
     end;
    end;
   end;
  end;
 finally
  Roots:=nil;
  XRoots:=nil;
  YRoots:=nil;
 end;
end;

procedure GetIntersectionPointsForCubicCurveCubicCurve(const aSegment0,aSegment1:TpvVectorPathSegmentCubicCurve;const aIntersectionPoints:TpvVectorPathVectorList);
var a1,a2,a3,a4,b1,b2,b3,b4,
    c10,c11,c12,c13,c20,c21,c22,c23,
    c10s,c11s,c12s,c13s,c20s,c21s,c22s,c23s,
    c10c,c11c,c12c,c13c,c20c,c21c,c22c,c23c:TpvVectorPathVector;
    PolyCoefs:array[0..9] of TpvDouble;
    Roots,XRoots,YRoots:TpvDoubleDynamicArray;
//  XRoots,YRoots:array[0..1] of TpvDouble;
    CountXRoots,CountYRoots,Index,XIndex,YIndex:TpvSizeInt;
    OK:boolean;
    s,XRoot:TpvDouble;
begin
 a1:=aSegment0.Points[0];
 a2:=aSegment0.Points[1];
 a3:=aSegment0.Points[2];
 a4:=aSegment0.Points[3];
 b1:=aSegment1.Points[0];
 b2:=aSegment1.Points[1];
 b3:=aSegment1.Points[2];
 b4:=aSegment1.Points[3];
 c10:=a1;
 c11:=(a1*(-3.0))+(a2*3.0);
 c12:=((a1*3.0)+(a2*(-6.0)))+(a3*3.0);
 c13:=(((a1*(-1.0))+(a2*3.0))+(a3*(-3.0)))+a4;
 c20:=b1;
 c21:=(b1*(-3.0))+(b2*3.0);
 c22:=((b1*3.0)+(b2*(-6.0)))+(b3*3.0);
 c23:=(((b1*(-1.0))+(b2*3.0))+(b3*(-3.0)))+b4;
 c10s:=c10*c10;
 c11s:=c11*c11;
 c12s:=c12*c12;
 c13s:=c13*c13;
 c20s:=c20*c20;
 c21s:=c21*c21;
 c22s:=c22*c22;
 c23s:=c23*c23;
 c10c:=c10s*c10;
 c11c:=c11s*c11;
 c12c:=c12s*c12;
 c13c:=c13s*c13;
 c20c:=c20s*c20;
 c21c:=c21s*c21;
 c22c:=c22s*c22;
 c23c:=c23s*c23;
 PolyCoefs[0]:=-c13c.x*c23c.y+c13c.y*c23c.x-3*c13.x*c13s.y*c23s.x*c23.y+3*c13s.x*c13.y*c23.x*c23s.y;
 PolyCoefs[1]:=-6*c13.x*c22.x*c13s.y*c23.x*c23.y+6*c13s.x*c13.y*c22.y*c23.x*c23.y+3*c22.x*c13c.y*c23s.x-3*c13c.x*c22.y*c23s.y-3*c13.x*c13s.y*c22.y*c23s.x+3*c13s.x*c22.x*c13.y*c23s.y;
 PolyCoefs[2]:=-6*c21.x*c13.x*c13s.y*c23.x*c23.y-6*c13.x*c22.x*c13s.y*c22.y*c23.x+6*c13s.x*c22.x*c13.y*c22.y*c23.y+3*c21.x*c13c.y*c23s.x+3*c22s.x*c13c.y*c23.x+3*c21.x*c13s.x*c13.y*c23s.y-3*c13.x*c21.y*c13s.y*c23s.x-3*c13.x*c22s.x*c13s.y*c23.y+c13s.x*c13.y*c23.x*(6*c21.y*c23.y+3*c22s.y)+c13c.x*(-c21.y*c23s.y-2*c22s.y*c23.y-c23.y*(2*c21.y*c23.y+c22s.y));
 PolyCoefs[3]:=c11.x*c12.y*c13.x*c13.y*c23.x*c23.y-c11.y*c12.x*c13.x*c13.y*c23.x*c23.y+6*c21.x*c22.x*c13c.y*c23.x+3*c11.x*c12.x*c13.x*c13.y*c23s.y+6*c10.x*c13.x*c13s.y*c23.x*c23.y-3*c11.x*c12.x*c13s.y*c23.x*c23.y-3*c11.y*c12.y*c13.x*c13.y*c23s.x-6*c10.y*c13s.x*c13.y*c23.x*c23.y-6*c20.x*c13.x*c13s.y*c23.x*c23.y+3*c11.y*c12.y*c13s.x*c23.x*c23.y-2*c12.x*c12s.y*c13.x*c23.x*c23.y-6*c21.x*c13.x*c22.x*c13s.y*c23.y-6*c21.x*c13.x*c13s.y*c22.y*c23.x-6*c13.x*c21.y*c22.x*c13s.y*c23.x+6*c21.x*c13s.x*c13.y*c22.y*c23.y+2*c12s.x*c12.y*c13.y*c23.x*c23.y+c22c.x*c13c.y-3*c10.x*c13c.y*c23s.x+3*c10.y*c13c.x*c23s.y+3*c20.x*c13c.y*c23s.x+c12c.y*c13.x*c23s.x-c12c.x*c13.y*c23s.y-3*c10.x*c13s.x*c13.y*c23s.y+3*c10.y*c13.x*c13s.y*c23s.x-2*c11.x*c12.y*c13s.x*c23s.y+c11.x*c12.y*c13s.y*c23s.x-c11.y*c12.x*c13s.x*c23s.y+2*c11.y*c12.x*c13s.y*c23s.x+3*c20.x*c13s.x*c13.y*c23s.y-c12.x*c12s.y*c13.y*c23s.x-3*c20.y*c13.x*c13s.y*c23s.x+c12s.x*c12.y*c13.x*c23s.y-
               3*c13.x*c22s.x*c13s.y*c22.y+c13s.x*c13.y*c23.x*(6*c20.y*c23.y+6*c21.y*c22.y)+c13s.x*c22.x*c13.y*(6*c21.y*c23.y+3*c22s.y)+c13c.x*(-2*c21.y*c22.y*c23.y-c20.y*c23s.y-c22.y*(2*c21.y*c23.y+c22s.y)-c23.y*(2*c20.y*c23.y+2*c21.y*c22.y));
 PolyCoefs[4]:=6*c11.x*c12.x*c13.x*c13.y*c22.y*c23.y+c11.x*c12.y*c13.x*c22.x*c13.y*c23.y+c11.x*c12.y*c13.x*c13.y*c22.y*c23.x-c11.y*c12.x*c13.x*c22.x*c13.y*c23.y-c11.y*c12.x*c13.x*c13.y*c22.y*c23.x-6*c11.y*c12.y*c13.x*c22.x*c13.y*c23.x-6*c10.x*c22.x*c13c.y*c23.x+6*c20.x*c22.x*c13c.y*c23.x+6*c10.y*c13c.x*c22.y*c23.y+2*c12c.y*c13.x*c22.x*c23.x-2*c12c.x*c13.y*c22.y*c23.y+6*c10.x*c13.x*c22.x*c13s.y*c23.y+6*c10.x*c13.x*c13s.y*c22.y*c23.x+6*c10.y*c13.x*c22.x*c13s.y*c23.x-3*c11.x*c12.x*c22.x*c13s.y*c23.y-3*c11.x*c12.x*c13s.y*c22.y*c23.x+2*c11.x*c12.y*c22.x*c13s.y*c23.x+4*c11.y*c12.x*c22.x*c13s.y*c23.x-6*c10.x*c13s.x*c13.y*c22.y*c23.y-6*c10.y*c13s.x*c22.x*c13.y*c23.y-6*c10.y*c13s.x*c13.y*c22.y*c23.x-4*c11.x*c12.y*c13s.x*c22.y*c23.y-6*c20.x*c13.x*c22.x*c13s.y*c23.y-6*c20.x*c13.x*c13s.y*c22.y*c23.x-2*c11.y*c12.x*c13s.x*c22.y*c23.y+3*c11.y*c12.y*c13s.x*c22.x*c23.y+3*c11.y*c12.y*c13s.x*c22.y*c23.x-2*c12.x*c12s.y*c13.x*c22.x*c23.y-
               2*c12.x*c12s.y*c13.x*c22.y*c23.x-2*c12.x*c12s.y*c22.x*c13.y*c23.x-6*c20.y*c13.x*c22.x*c13s.y*c23.x-6*c21.x*c13.x*c21.y*c13s.y*c23.x-6*c21.x*c13.x*c22.x*c13s.y*c22.y+6*c20.x*c13s.x*c13.y*c22.y*c23.y+2*c12s.x*c12.y*c13.x*c22.y*c23.y+2*c12s.x*c12.y*c22.x*c13.y*c23.y+2*c12s.x*c12.y*c13.y*c22.y*c23.x+3*c21.x*c22s.x*c13c.y+3*c21s.x*c13c.y*c23.x-3*c13.x*c21.y*c22s.x*c13s.y-3*c21s.x*c13.x*c13s.y*c23.y+c13s.x*c22.x*c13.y*(6*c20.y*c23.y+6*c21.y*c22.y)+c13s.x*c13.y*c23.x*(6*c20.y*c22.y+3*c21s.y)+c21.x*c13s.x*c13.y*(6*c21.y*c23.y+3*c22s.y)+c13c.x*(-2*c20.y*c22.y*c23.y-c23.y*(2*c20.y*c22.y+c21s.y)-c21.y*(2*c21.y*c23.y+c22s.y)-c22.y*(2*c20.y*c23.y+2*c21.y*c22.y));
 PolyCoefs[5]:=c11.x*c21.x*c12.y*c13.x*c13.y*c23.y+c11.x*c12.y*c13.x*c21.y*c13.y*c23.x+c11.x*c12.y*c13.x*c22.x*c13.y*c22.y-c11.y*c12.x*c21.x*c13.x*c13.y*c23.y-c11.y*c12.x*c13.x*c21.y*c13.y*c23.x-c11.y*c12.x*c13.x*c22.x*c13.y*c22.y-6*c11.y*c21.x*c12.y*c13.x*c13.y*c23.x-6*c10.x*c21.x*c13c.y*c23.x+6*c20.x*c21.x*c13c.y*c23.x+2*c21.x*c12c.y*c13.x*c23.x+6*c10.x*c21.x*c13.x*c13s.y*c23.y+6*c10.x*c13.x*c21.y*c13s.y*c23.x+6*c10.x*c13.x*c22.x*c13s.y*c22.y+6*c10.y*c21.x*c13.x*c13s.y*c23.x-3*c11.x*c12.x*c21.x*c13s.y*c23.y-3*c11.x*c12.x*c21.y*c13s.y*c23.x-3*c11.x*c12.x*c22.x*c13s.y*c22.y+2*c11.x*c21.x*c12.y*c13s.y*c23.x+4*c11.y*c12.x*c21.x*c13s.y*c23.x-6*c10.y*c21.x*c13s.x*c13.y*c23.y-6*c10.y*c13s.x*c21.y*c13.y*c23.x-6*c10.y*c13s.x*c22.x*c13.y*c22.y-6*c20.x*c21.x*c13.x*c13s.y*c23.y-6*c20.x*c13.x*c21.y*c13s.y*c23.x-6*c20.x*c13.x*c22.x*c13s.y*c22.y+3*c11.y*c21.x*c12.y*c13s.x*c23.y-3*c11.y*c12.y*c13.x*c22s.x*c13.y+3*c11.y*c12.y*c13s.x*c21.y*c23.x+
               3*c11.y*c12.y*c13s.x*c22.x*c22.y-2*c12.x*c21.x*c12s.y*c13.x*c23.y-2*c12.x*c21.x*c12s.y*c13.y*c23.x-2*c12.x*c12s.y*c13.x*c21.y*c23.x-2*c12.x*c12s.y*c13.x*c22.x*c22.y-6*c20.y*c21.x*c13.x*c13s.y*c23.x-6*c21.x*c13.x*c21.y*c22.x*c13s.y+6*c20.y*c13s.x*c21.y*c13.y*c23.x+2*c12s.x*c21.x*c12.y*c13.y*c23.y+2*c12s.x*c12.y*c21.y*c13.y*c23.x+2*c12s.x*c12.y*c22.x*c13.y*c22.y-3*c10.x*c22s.x*c13c.y+3*c20.x*c22s.x*c13c.y+3*c21s.x*c22.x*c13c.y+c12c.y*c13.x*c22s.x+3*c10.y*c13.x*c22s.x*c13s.y+c11.x*c12.y*c22s.x*c13s.y+2*c11.y*c12.x*c22s.x*c13s.y-c12.x*c12s.y*c22s.x*c13.y-3*c20.y*c13.x*c22s.x*c13s.y-3*c21s.x*c13.x*c13s.y*c22.y+c12s.x*c12.y*c13.x*(2*c21.y*c23.y+c22s.y)+c11.x*c12.x*c13.x*c13.y*(6*c21.y*c23.y+3*c22s.y)+c21.x*c13s.x*c13.y*(6*c20.y*c23.y+6*c21.y*c22.y)+c12c.x*c13.y*(-2*c21.y*c23.y-c22s.y)+c10.y*c13c.x*(6*c21.y*c23.y+3*c22s.y)+c11.y*c12.x*c13s.x*(-2*c21.y*c23.y-c22s.y)+
               c11.x*c12.y*c13s.x*(-4*c21.y*c23.y-2*c22s.y)+c10.x*c13s.x*c13.y*(-6*c21.y*c23.y-3*c22s.y)+c13s.x*c22.x*c13.y*(6*c20.y*c22.y+3*c21s.y)+c20.x*c13s.x*c13.y*(6*c21.y*c23.y+3*c22s.y)+c13c.x*(-2*c20.y*c21.y*c23.y-c22.y*(2*c20.y*c22.y+c21s.y)-c20.y*(2*c21.y*c23.y+c22s.y)-c21.y*(2*c20.y*c23.y+2*c21.y*c22.y));
 PolyCoefs[6]:=-c10.x*c11.x*c12.y*c13.x*c13.y*c23.y+c10.x*c11.y*c12.x*c13.x*c13.y*c23.y+6*c10.x*c11.y*c12.y*c13.x*c13.y*c23.x-6*c10.y*c11.x*c12.x*c13.x*c13.y*c23.y-c10.y*c11.x*c12.y*c13.x*c13.y*c23.x+c10.y*c11.y*c12.x*c13.x*c13.y*c23.x+c11.x*c11.y*c12.x*c12.y*c13.x*c23.y-c11.x*c11.y*c12.x*c12.y*c13.y*c23.x+c11.x*c20.x*c12.y*c13.x*c13.y*c23.y+c11.x*c20.y*c12.y*c13.x*c13.y*c23.x+c11.x*c21.x*c12.y*c13.x*c13.y*c22.y+c11.x*c12.y*c13.x*c21.y*c22.x*c13.y-c20.x*c11.y*c12.x*c13.x*c13.y*c23.y-6*c20.x*c11.y*c12.y*c13.x*c13.y*c23.x-c11.y*c12.x*c20.y*c13.x*c13.y*c23.x-c11.y*c12.x*c21.x*c13.x*c13.y*c22.y-c11.y*c12.x*c13.x*c21.y*c22.x*c13.y-6*c11.y*c21.x*c12.y*c13.x*c22.x*c13.y-6*c10.x*c20.x*c13c.y*c23.x-6*c10.x*c21.x*c22.x*c13c.y-2*c10.x*c12c.y*c13.x*c23.x+6*c20.x*c21.x*c22.x*c13c.y+2*c20.x*c12c.y*c13.x*c23.x+2*c21.x*c12c.y*c13.x*c22.x+2*c10.y*c12c.x*c13.y*c23.y-6*c10.x*c10.y*c13.x*c13s.y*c23.x+3*c10.x*c11.x*c12.x*c13s.y*c23.y-
               2*c10.x*c11.x*c12.y*c13s.y*c23.x-4*c10.x*c11.y*c12.x*c13s.y*c23.x+3*c10.y*c11.x*c12.x*c13s.y*c23.x+6*c10.x*c10.y*c13s.x*c13.y*c23.y+6*c10.x*c20.x*c13.x*c13s.y*c23.y-3*c10.x*c11.y*c12.y*c13s.x*c23.y+2*c10.x*c12.x*c12s.y*c13.x*c23.y+2*c10.x*c12.x*c12s.y*c13.y*c23.x+6*c10.x*c20.y*c13.x*c13s.y*c23.x+6*c10.x*c21.x*c13.x*c13s.y*c22.y+6*c10.x*c13.x*c21.y*c22.x*c13s.y+4*c10.y*c11.x*c12.y*c13s.x*c23.y+6*c10.y*c20.x*c13.x*c13s.y*c23.x+2*c10.y*c11.y*c12.x*c13s.x*c23.y-3*c10.y*c11.y*c12.y*c13s.x*c23.x+2*c10.y*c12.x*c12s.y*c13.x*c23.x+6*c10.y*c21.x*c13.x*c22.x*c13s.y-3*c11.x*c20.x*c12.x*c13s.y*c23.y+2*c11.x*c20.x*c12.y*c13s.y*c23.x+c11.x*c11.y*c12s.y*c13.x*c23.x-3*c11.x*c12.x*c20.y*c13s.y*c23.x-3*c11.x*c12.x*c21.x*c13s.y*c22.y-3*c11.x*c12.x*c21.y*c22.x*c13s.y+2*c11.x*c21.x*c12.y*c22.x*c13s.y+4*c20.x*c11.y*c12.x*c13s.y*c23.x+4*c11.y*c12.x*c21.x*c22.x*c13s.y-2*c10.x*c12s.x*c12.y*c13.y*c23.y-6*c10.y*c20.x*c13s.x*c13.y*c23.y-
               6*c10.y*c20.y*c13s.x*c13.y*c23.x-6*c10.y*c21.x*c13s.x*c13.y*c22.y-2*c10.y*c12s.x*c12.y*c13.x*c23.y-2*c10.y*c12s.x*c12.y*c13.y*c23.x-6*c10.y*c13s.x*c21.y*c22.x*c13.y-c11.x*c11.y*c12s.x*c13.y*c23.y-2*c11.x*c11s.y*c13.x*c13.y*c23.x+3*c20.x*c11.y*c12.y*c13s.x*c23.y-2*c20.x*c12.x*c12s.y*c13.x*c23.y-2*c20.x*c12.x*c12s.y*c13.y*c23.x-6*c20.x*c20.y*c13.x*c13s.y*c23.x-6*c20.x*c21.x*c13.x*c13s.y*c22.y-6*c20.x*c13.x*c21.y*c22.x*c13s.y+3*c11.y*c20.y*c12.y*c13s.x*c23.x+3*c11.y*c21.x*c12.y*c13s.x*c22.y+3*c11.y*c12.y*c13s.x*c21.y*c22.x-2*c12.x*c20.y*c12s.y*c13.x*c23.x-2*c12.x*c21.x*c12s.y*c13.x*c22.y-2*c12.x*c21.x*c12s.y*c22.x*c13.y-2*c12.x*c12s.y*c13.x*c21.y*c22.x-6*c20.y*c21.x*c13.x*c22.x*c13s.y-c11s.y*c12.x*c12.y*c13.x*c23.x+2*c20.x*c12s.x*c12.y*c13.y*c23.y+6*c20.y*c13s.x*c21.y*c22.x*c13.y+2*c11s.x*c11.y*c13.x*c13.y*c23.y+c11s.x*c12.x*c12.y*c13.y*c23.y+2*c12s.x*c20.y*c12.y*c13.y*c23.x+2*c12s.x*c21.x*c12.y*c13.y*c22.y+
               2*c12s.x*c12.y*c21.y*c22.x*c13.y+c21c.x*c13c.y+3*c10s.x*c13c.y*c23.x-3*c10s.y*c13c.x*c23.y+3*c20s.x*c13c.y*c23.x+c11c.y*c13s.x*c23.x-c11c.x*c13s.y*c23.y-c11.x*c11s.y*c13s.x*c23.y+c11s.x*c11.y*c13s.y*c23.x-3*c10s.x*c13.x*c13s.y*c23.y+3*c10s.y*c13s.x*c13.y*c23.x-c11s.x*c12s.y*c13.x*c23.y+c11s.y*c12s.x*c13.y*c23.x-3*c21s.x*c13.x*c21.y*c13s.y-3*c20s.x*c13.x*c13s.y*c23.y+3*c20s.y*c13s.x*c13.y*c23.x+c11.x*c12.x*c13.x*c13.y*(6*c20.y*c23.y+6*c21.y*c22.y)+c12c.x*c13.y*(-2*c20.y*c23.y-2*c21.y*c22.y)+c10.y*c13c.x*(6*c20.y*c23.y+6*c21.y*c22.y)+c11.y*c12.x*c13s.x*(-2*c20.y*c23.y-2*c21.y*c22.y)+c12s.x*c12.y*c13.x*(2*c20.y*c23.y+2*c21.y*c22.y)+c11.x*c12.y*c13s.x*(-4*c20.y*c23.y-4*c21.y*c22.y)+c10.x*c13s.x*c13.y*(-6*c20.y*c23.y-6*c21.y*c22.y)+c20.x*c13s.x*c13.y*(6*c20.y*c23.y+6*c21.y*c22.y)+c21.x*c13s.x*c13.y*(6*c20.y*c22.y+3*c21s.y)+c13c.x*(-2*c20.y*c21.y*c22.y-c20s.y*c23.y-c21.y*(2*c20.y*c22.y+c21s.y)-c20.y*(2*c20.y*c23.y+2*c21.y*c22.y));
 PolyCoefs[7]:=-c10.x*c11.x*c12.y*c13.x*c13.y*c22.y+c10.x*c11.y*c12.x*c13.x*c13.y*c22.y+6*c10.x*c11.y*c12.y*c13.x*c22.x*c13.y-6*c10.y*c11.x*c12.x*c13.x*c13.y*c22.y-c10.y*c11.x*c12.y*c13.x*c22.x*c13.y+c10.y*c11.y*c12.x*c13.x*c22.x*c13.y+c11.x*c11.y*c12.x*c12.y*c13.x*c22.y-c11.x*c11.y*c12.x*c12.y*c22.x*c13.y+c11.x*c20.x*c12.y*c13.x*c13.y*c22.y+c11.x*c20.y*c12.y*c13.x*c22.x*c13.y+c11.x*c21.x*c12.y*c13.x*c21.y*c13.y-c20.x*c11.y*c12.x*c13.x*c13.y*c22.y-6*c20.x*c11.y*c12.y*c13.x*c22.x*c13.y-c11.y*c12.x*c20.y*c13.x*c22.x*c13.y-c11.y*c12.x*c21.x*c13.x*c21.y*c13.y-6*c10.x*c20.x*c22.x*c13c.y-2*c10.x*c12c.y*c13.x*c22.x+2*c20.x*c12c.y*c13.x*c22.x+2*c10.y*c12c.x*c13.y*c22.y-6*c10.x*c10.y*c13.x*c22.x*c13s.y+3*c10.x*c11.x*c12.x*c13s.y*c22.y-2*c10.x*c11.x*c12.y*c22.x*c13s.y-4*c10.x*c11.y*c12.x*c22.x*c13s.y+3*c10.y*c11.x*c12.x*c22.x*c13s.y+6*c10.x*c10.y*c13s.x*c13.y*c22.y+6*c10.x*c20.x*c13.x*c13s.y*c22.y-3*c10.x*c11.y*c12.y*c13s.x*c22.y+
               2*c10.x*c12.x*c12s.y*c13.x*c22.y+2*c10.x*c12.x*c12s.y*c22.x*c13.y+6*c10.x*c20.y*c13.x*c22.x*c13s.y+6*c10.x*c21.x*c13.x*c21.y*c13s.y+4*c10.y*c11.x*c12.y*c13s.x*c22.y+6*c10.y*c20.x*c13.x*c22.x*c13s.y+2*c10.y*c11.y*c12.x*c13s.x*c22.y-3*c10.y*c11.y*c12.y*c13s.x*c22.x+2*c10.y*c12.x*c12s.y*c13.x*c22.x-3*c11.x*c20.x*c12.x*c13s.y*c22.y+2*c11.x*c20.x*c12.y*c22.x*c13s.y+c11.x*c11.y*c12s.y*c13.x*c22.x-3*c11.x*c12.x*c20.y*c22.x*c13s.y-3*c11.x*c12.x*c21.x*c21.y*c13s.y+4*c20.x*c11.y*c12.x*c22.x*c13s.y-2*c10.x*c12s.x*c12.y*c13.y*c22.y-6*c10.y*c20.x*c13s.x*c13.y*c22.y-6*c10.y*c20.y*c13s.x*c22.x*c13.y-6*c10.y*c21.x*c13s.x*c21.y*c13.y-2*c10.y*c12s.x*c12.y*c13.x*c22.y-2*c10.y*c12s.x*c12.y*c22.x*c13.y-c11.x*c11.y*c12s.x*c13.y*c22.y-2*c11.x*c11s.y*c13.x*c22.x*c13.y+3*c20.x*c11.y*c12.y*c13s.x*c22.y-2*c20.x*c12.x*c12s.y*c13.x*c22.y-2*c20.x*c12.x*c12s.y*c22.x*c13.y-6*c20.x*c20.y*c13.x*c22.x*c13s.y-6*c20.x*c21.x*c13.x*c21.y*c13s.y+
               3*c11.y*c20.y*c12.y*c13s.x*c22.x+3*c11.y*c21.x*c12.y*c13s.x*c21.y-2*c12.x*c20.y*c12s.y*c13.x*c22.x-2*c12.x*c21.x*c12s.y*c13.x*c21.y-c11s.y*c12.x*c12.y*c13.x*c22.x+2*c20.x*c12s.x*c12.y*c13.y*c22.y-3*c11.y*c21s.x*c12.y*c13.x*c13.y+6*c20.y*c21.x*c13s.x*c21.y*c13.y+2*c11s.x*c11.y*c13.x*c13.y*c22.y+c11s.x*c12.x*c12.y*c13.y*c22.y+2*c12s.x*c20.y*c12.y*c22.x*c13.y+2*c12s.x*c21.x*c12.y*c21.y*c13.y-3*c10.x*c21s.x*c13c.y+3*c20.x*c21s.x*c13c.y+3*c10s.x*c22.x*c13c.y-3*c10s.y*c13c.x*c22.y+3*c20s.x*c22.x*c13c.y+c21s.x*c12c.y*c13.x+c11c.y*c13s.x*c22.x-c11c.x*c13s.y*c22.y+3*c10.y*c21s.x*c13.x*c13s.y-c11.x*c11s.y*c13s.x*c22.y+c11.x*c21s.x*c12.y*c13s.y+2*c11.y*c12.x*c21s.x*c13s.y+c11s.x*c11.y*c22.x*c13s.y-c12.x*c21s.x*c12s.y*c13.y-3*c20.y*c21s.x*c13.x*c13s.y-3*c10s.x*c13.x*c13s.y*c22.y+3*c10s.y*c13s.x*c22.x*c13.y-c11s.x*c12s.y*c13.x*c22.y+c11s.y*c12s.x*c22.x*c13.y-3*c20s.x*c13.x*c13s.y*c22.y+3*c20s.y*c13s.x*c22.x*c13.y+
               c12s.x*c12.y*c13.x*(2*c20.y*c22.y+c21s.y)+c11.x*c12.x*c13.x*c13.y*(6*c20.y*c22.y+3*c21s.y)+c12c.x*c13.y*(-2*c20.y*c22.y-c21s.y)+c10.y*c13c.x*(6*c20.y*c22.y+3*c21s.y)+c11.y*c12.x*c13s.x*(-2*c20.y*c22.y-c21s.y)+c11.x*c12.y*c13s.x*(-4*c20.y*c22.y-2*c21s.y)+c10.x*c13s.x*c13.y*(-6*c20.y*c22.y-3*c21s.y)+c20.x*c13s.x*c13.y*(6*c20.y*c22.y+3*c21s.y)+c13c.x*(-2*c20.y*c21s.y-c20s.y*c22.y-c20.y*(2*c20.y*c22.y+c21s.y));
 PolyCoefs[8]:=-c10.x*c11.x*c12.y*c13.x*c21.y*c13.y+c10.x*c11.y*c12.x*c13.x*c21.y*c13.y+6*c10.x*c11.y*c21.x*c12.y*c13.x*c13.y-6*c10.y*c11.x*c12.x*c13.x*c21.y*c13.y-c10.y*c11.x*c21.x*c12.y*c13.x*c13.y+c10.y*c11.y*c12.x*c21.x*c13.x*c13.y-c11.x*c11.y*c12.x*c21.x*c12.y*c13.y+c11.x*c11.y*c12.x*c12.y*c13.x*c21.y+c11.x*c20.x*c12.y*c13.x*c21.y*c13.y+6*c11.x*c12.x*c20.y*c13.x*c21.y*c13.y+c11.x*c20.y*c21.x*c12.y*c13.x*c13.y-c20.x*c11.y*c12.x*c13.x*c21.y*c13.y-6*c20.x*c11.y*c21.x*c12.y*c13.x*c13.y-c11.y*c12.x*c20.y*c21.x*c13.x*c13.y-6*c10.x*c20.x*c21.x*c13c.y-2*c10.x*c21.x*c12c.y*c13.x+6*c10.y*c20.y*c13c.x*c21.y+2*c20.x*c21.x*c12c.y*c13.x+2*c10.y*c12c.x*c21.y*c13.y-2*c12c.x*c20.y*c21.y*c13.y-6*c10.x*c10.y*c21.x*c13.x*c13s.y+3*c10.x*c11.x*c12.x*c21.y*c13s.y-2*c10.x*c11.x*c21.x*c12.y*c13s.y-4*c10.x*c11.y*c12.x*c21.x*c13s.y+3*c10.y*c11.x*c12.x*c21.x*c13s.y+6*c10.x*c10.y*c13s.x*c21.y*c13.y+6*c10.x*c20.x*c13.x*c21.y*c13s.y-
               3*c10.x*c11.y*c12.y*c13s.x*c21.y+2*c10.x*c12.x*c21.x*c12s.y*c13.y+2*c10.x*c12.x*c12s.y*c13.x*c21.y+6*c10.x*c20.y*c21.x*c13.x*c13s.y+4*c10.y*c11.x*c12.y*c13s.x*c21.y+6*c10.y*c20.x*c21.x*c13.x*c13s.y+2*c10.y*c11.y*c12.x*c13s.x*c21.y-3*c10.y*c11.y*c21.x*c12.y*c13s.x+2*c10.y*c12.x*c21.x*c12s.y*c13.x-3*c11.x*c20.x*c12.x*c21.y*c13s.y+2*c11.x*c20.x*c21.x*c12.y*c13s.y+c11.x*c11.y*c21.x*c12s.y*c13.x-3*c11.x*c12.x*c20.y*c21.x*c13s.y+4*c20.x*c11.y*c12.x*c21.x*c13s.y-6*c10.x*c20.y*c13s.x*c21.y*c13.y-2*c10.x*c12s.x*c12.y*c21.y*c13.y-6*c10.y*c20.x*c13s.x*c21.y*c13.y-6*c10.y*c20.y*c21.x*c13s.x*c13.y-2*c10.y*c12s.x*c21.x*c12.y*c13.y-2*c10.y*c12s.x*c12.y*c13.x*c21.y-c11.x*c11.y*c12s.x*c21.y*c13.y-4*c11.x*c20.y*c12.y*c13s.x*c21.y-2*c11.x*c11s.y*c21.x*c13.x*c13.y+3*c20.x*c11.y*c12.y*c13s.x*c21.y-2*c20.x*c12.x*c21.x*c12s.y*c13.y-2*c20.x*c12.x*c12s.y*c13.x*c21.y-6*c20.x*c20.y*c21.x*c13.x*c13s.y-2*c11.y*c12.x*c20.y*c13s.x*c21.y+
               3*c11.y*c20.y*c21.x*c12.y*c13s.x-2*c12.x*c20.y*c21.x*c12s.y*c13.x-c11s.y*c12.x*c21.x*c12.y*c13.x+6*c20.x*c20.y*c13s.x*c21.y*c13.y+2*c20.x*c12s.x*c12.y*c21.y*c13.y+2*c11s.x*c11.y*c13.x*c21.y*c13.y+c11s.x*c12.x*c12.y*c21.y*c13.y+2*c12s.x*c20.y*c21.x*c12.y*c13.y+2*c12s.x*c20.y*c12.y*c13.x*c21.y+3*c10s.x*c21.x*c13c.y-3*c10s.y*c13c.x*c21.y+3*c20s.x*c21.x*c13c.y+c11c.y*c21.x*c13s.x-c11c.x*c21.y*c13s.y-3*c20s.y*c13c.x*c21.y-c11.x*c11s.y*c13s.x*c21.y+c11s.x*c11.y*c21.x*c13s.y-3*c10s.x*c13.x*c21.y*c13s.y+3*c10s.y*c21.x*c13s.x*c13.y-c11s.x*c12s.y*c13.x*c21.y+c11s.y*c12s.x*c21.x*c13.y-3*c20s.x*c13.x*c21.y*c13s.y+3*c20s.y*c21.x*c13s.x*c13.y;
 PolyCoefs[9]:=c10.x*c10.y*c11.x*c12.y*c13.x*c13.y-c10.x*c10.y*c11.y*c12.x*c13.x*c13.y+c10.x*c11.x*c11.y*c12.x*c12.y*c13.y-c10.y*c11.x*c11.y*c12.x*c12.y*c13.x-c10.x*c11.x*c20.y*c12.y*c13.x*c13.y+6*c10.x*c20.x*c11.y*c12.y*c13.x*c13.y+c10.x*c11.y*c12.x*c20.y*c13.x*c13.y-c10.y*c11.x*c20.x*c12.y*c13.x*c13.y-6*c10.y*c11.x*c12.x*c20.y*c13.x*c13.y+c10.y*c20.x*c11.y*c12.x*c13.x*c13.y-c11.x*c20.x*c11.y*c12.x*c12.y*c13.y+c11.x*c11.y*c12.x*c20.y*c12.y*c13.x+c11.x*c20.x*c20.y*c12.y*c13.x*c13.y-c20.x*c11.y*c12.x*c20.y*c13.x*c13.y-2*c10.x*c20.x*c12c.y*c13.x+2*c10.y*c12c.x*c20.y*c13.y-3*c10.x*c10.y*c11.x*c12.x*c13s.y-6*c10.x*c10.y*c20.x*c13.x*c13s.y+3*c10.x*c10.y*c11.y*c12.y*c13s.x-2*c10.x*c10.y*c12.x*c12s.y*c13.x-2*c10.x*c11.x*c20.x*c12.y*c13s.y-c10.x*c11.x*c11.y*c12s.y*c13.x+3*c10.x*c11.x*c12.x*c20.y*c13s.y-4*c10.x*c20.x*c11.y*c12.x*c13s.y+3*c10.y*c11.x*c20.x*c12.x*c13s.y+6*c10.x*c10.y*c20.y*c13s.x*c13.y+2*c10.x*c10.y*c12s.x*c12.y*c13.y+
               2*c10.x*c11.x*c11s.y*c13.x*c13.y+2*c10.x*c20.x*c12.x*c12s.y*c13.y+6*c10.x*c20.x*c20.y*c13.x*c13s.y-3*c10.x*c11.y*c20.y*c12.y*c13s.x+2*c10.x*c12.x*c20.y*c12s.y*c13.x+c10.x*c11s.y*c12.x*c12.y*c13.x+c10.y*c11.x*c11.y*c12s.x*c13.y+4*c10.y*c11.x*c20.y*c12.y*c13s.x-3*c10.y*c20.x*c11.y*c12.y*c13s.x+2*c10.y*c20.x*c12.x*c12s.y*c13.x+2*c10.y*c11.y*c12.x*c20.y*c13s.x+c11.x*c20.x*c11.y*c12s.y*c13.x-3*c11.x*c20.x*c12.x*c20.y*c13s.y-2*c10.x*c12s.x*c20.y*c12.y*c13.y-6*c10.y*c20.x*c20.y*c13s.x*c13.y-2*c10.y*c20.x*c12s.x*c12.y*c13.y-2*c10.y*c11s.x*c11.y*c13.x*c13.y-c10.y*c11s.x*c12.x*c12.y*c13.y-2*c10.y*c12s.x*c20.y*c12.y*c13.x-2*c11.x*c20.x*c11s.y*c13.x*c13.y-c11.x*c11.y*c12s.x*c20.y*c13.y+3*c20.x*c11.y*c20.y*c12.y*c13s.x-2*c20.x*c12.x*c20.y*c12s.y*c13.x-c20.x*c11s.y*c12.x*c12.y*c13.x+3*c10s.y*c11.x*c12.x*c13.x*c13.y+3*c11.x*c12.x*c20s.y*c13.x*c13.y+2*c20.x*c12s.x*c20.y*c12.y*c13.y-3*c10s.x*c11.y*c12.y*c13.x*c13.y+
               2*c11s.x*c11.y*c20.y*c13.x*c13.y+c11s.x*c12.x*c20.y*c12.y*c13.y-3*c20s.x*c11.y*c12.y*c13.x*c13.y-c10c.x*c13c.y+c10c.y*c13c.x+c20c.x*c13c.y-c20c.y*c13c.x-3*c10.x*c20s.x*c13c.y-c10.x*c11c.y*c13s.x+3*c10s.x*c20.x*c13c.y+c10.y*c11c.x*c13s.y+3*c10.y*c20s.y*c13c.x+c20.x*c11c.y*c13s.x+c10s.x*c12c.y*c13.x-3*c10s.y*c20.y*c13c.x-c10s.y*c12c.x*c13.y+c20s.x*c12c.y*c13.x-c11c.x*c20.y*c13s.y-c12c.x*c20s.y*c13.y-c10.x*c11s.x*c11.y*c13s.y+c10.y*c11.x*c11s.y*c13s.x-3*c10.x*c10s.y*c13s.x*c13.y-c10.x*c11s.y*c12s.x*c13.y+c10.y*c11s.x*c12s.y*c13.x-c11.x*c11s.y*c20.y*c13s.x+3*c10s.x*c10.y*c13.x*c13s.y+c10s.x*c11.x*c12.y*c13s.y+2*c10s.x*c11.y*c12.x*c13s.y-2*c10s.y*c11.x*c12.y*c13s.x-c10s.y*c11.y*c12.x*c13s.x+c11s.x*c20.x*c11.y*c13s.y-3*c10.x*c20s.y*c13s.x*c13.y+3*c10.y*c20s.x*c13.x*c13s.y+c11.x*c20s.x*c12.y*c13s.y-2*c11.x*c20s.y*c12.y*c13s.x+c20.x*c11s.y*c12s.x*c13.y-c11.y*c12.x*c20s.y*c13s.x-c10s.x*c12.x*c12s.y*c13.y-3*c10s.x*c20.y*c13.x*c13s.y+
               3*c10s.y*c20.x*c13s.x*c13.y+c10s.y*c12s.x*c12.y*c13.x-c11s.x*c20.y*c12s.y*c13.x+2*c20s.x*c11.y*c12.x*c13s.y+3*c20.x*c20s.y*c13s.x*c13.y-c20s.x*c12.x*c12s.y*c13.y-3*c20s.x*c20.y*c13.x*c13s.y+c12s.x*c20s.y*c12.y*c13.x;
 // PolyCoefs runs from the highest power down, the polynomial wants it the other way round.
 Roots:=(TpvPolynomial.Create([PolyCoefs[9],PolyCoefs[8],PolyCoefs[7],PolyCoefs[6],PolyCoefs[5],PolyCoefs[4],PolyCoefs[3],PolyCoefs[2],PolyCoefs[1],PolyCoefs[0]])).GetRootsInInterval(0.0,1.0);
//Roots:=SolveRootsInInterval(PolyCoefs,0.0,1.0);
 XRoots:=nil;
 YRoots:=nil;
 try
  for Index:=0 to length(Roots)-1 do begin
   s:=Roots[Index];
   if (s>=0.0) and (s<=1.0) then begin
    XRoots:=(TpvPolynomial.Create([(((c10.x-c20.x)-(s*c21.x))-(sqr(s)*c22.x))-((sqr(s)*s)*c23.x),
                                   c11.x,
                                   c12.x])).GetRoots;
    YRoots:=(TpvPolynomial.Create([(((c10.y-c20.y)-(s*c21.y))-(sqr(s)*c22.y))-((sqr(s)*s)*c23.y),
                                   c11.y,
                                   c12.y])).GetRoots;
    CountXRoots:=length(XRoots);
    CountYRoots:=length(YRoots);
{   CountXRoots:=SolveQuadratic(c12.x,
                                c11.x,
                                (((c10.x-c20.x)-(s*c21.x))-(sqr(s)*c22.x))-((sqr(s)*s)*c23.x),
                                XRoots[0],
                                XRoots[1]
                               );
    CountYRoots:=SolveQuadratic(c12.y,
                                c11.y,
                                (((c10.x-c20.y)-(s*c21.y))-(sqr(s)*c22.y))-((sqr(s)*s)*c23.y),
                                YRoots[0],
                                YRoots[1]
                               );}
    if (CountXRoots>0) and (CountYRoots>0) then begin
     OK:=false;
     for XIndex:=0 to CountXRoots-1 do begin
      XRoot:=XRoots[XIndex];
      if (XRoot>=0.0) and (XRoot<=1.0) then begin
       for YIndex:=0 to CountYRoots-1 do begin
        if SameValue(XRoot,YRoots[XIndex],1e-4) then begin
         aIntersectionPoints.Add((c23*(sqr(s)*s))+(c22*sqr(s))+(c21*s)+c20);
         OK:=true;
         break;
        end;
       end;
      end;
      if OK then begin
       break;
      end;
     end;
    end;
   end;
  end;
 finally
  Roots:=nil;
  XRoots:=nil;
  YRoots:=nil;
 end;
end;

{ TpvVectorPathSegment }

constructor TpvVectorPathSegment.Create;
begin
 inherited Create;
 fHasCachedBoundingBox:=false;
 fCachedBoundingBoxLock:=0;
 fIndex:=0;
end;

destructor TpvVectorPathSegment.Destroy;
begin
 inherited Destroy;
end;

procedure TpvVectorPathSegment.Assign(const aSegment:TpvVectorPathSegment);
begin
end;

function TpvVectorPathSegment.Clone:TpvVectorPathSegment;
begin
 result:=TpvVectorPathSegment(ClassType).Create;
 result.Assign(self);
end;

function TpvVectorPathSegment.GetBoundingBox:TpvVectorPathBoundingBox;
begin
 result:=TpvVectorPathBoundingBox.Create(TpvVectorPathVector.Create(MaxDouble,MaxDouble),
                                         TpvVectorPathVector.Create(-MaxDouble,-MaxDouble));
end;

procedure TpvVectorPathSegment.GetIntersectionPointsWithLineSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList);
begin
end;

procedure TpvVectorPathSegment.GetIntersectionPointsWithQuadraticCurveSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList);
begin
end;

procedure TpvVectorPathSegment.GetIntersectionPointsWithCubicCurveSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList);
begin
end;

procedure TpvVectorPathSegment.GetIntersectionPointsWithSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList);
begin

end;

procedure TpvVectorPathSegment.Transform(const aMatrix:TpvVectorPathMatrix);
begin
 // The subclasses transform their own points and then call here, so that the cached bounding
 // box is dropped in exactly one place.
 TPasMPMultipleReaderSingleWriterSpinLock.AcquireWrite(fCachedBoundingBoxLock);
 try
  fHasCachedBoundingBox:=false;
 finally
  TPasMPMultipleReaderSingleWriterSpinLock.ReleaseWrite(fCachedBoundingBoxLock);
 end;
end;

function TpvVectorPathSegment.GetCompareHorizontalLowestXCoordinate:TpvDouble;
var OK:boolean;
begin
 OK:=false;
 TPasMPMultipleReaderSingleWriterSpinLock.AcquireRead(fCachedBoundingBoxLock);
 try
  if fHasCachedBoundingBox then begin
   result:=fCachedBoundingBox.MinMax[0].x;
   OK:=true;
  end;
 finally
  TPasMPMultipleReaderSingleWriterSpinLock.ReleaseWrite(fCachedBoundingBoxLock);
 end;
 if OK then begin
  exit;
 end;
 result:=GetBoundingBox.MinMax[0].x;
end;

{ TpvVectorPathSegmentLine }

constructor TpvVectorPathSegmentLine.Create;
begin
 inherited Create;
 fType:=TpvVectorPathSegmentType.Line;
end;

constructor TpvVectorPathSegmentLine.Create(const aP0,aP1:TpvVectorPathVector);
begin
 Create;
 Points[0]:=aP0;
 Points[1]:=aP1;
end;

procedure TpvVectorPathSegmentLine.Assign(const aSegment:TpvVectorPathSegment);
begin
 if assigned(aSegment) and (aSegment is TpvVectorPathSegmentLine) then begin
  Points:=TpvVectorPathSegmentLine(aSegment).Points;
 end;
end;

function TpvVectorPathSegmentLine.Clone:TpvVectorPathSegment;
begin
 result:=TpvVectorPathSegmentLine.Create(Points[0],Points[1]);
end;

function TpvVectorPathSegmentLine.GetBoundingBox:TpvVectorPathBoundingBox;
begin
 TPasMPMultipleReaderSingleWriterSpinLock.AcquireRead(fCachedBoundingBoxLock);
 try
  if fHasCachedBoundingBox then begin
   result:=fCachedBoundingBox;
  end else begin
   TPasMPMultipleReaderSingleWriterSpinLock.ReadToWrite(fCachedBoundingBoxLock);
   try
    if fHasCachedBoundingBox then begin
     result:=fCachedBoundingBox;
    end else begin
     result:=TpvVectorPathBoundingBox.Create(Points[0].Minimum(Points[1]),
                                             Points[0].Maximum(Points[1]));
     fCachedBoundingBox:=result;
     fHasCachedBoundingBox:=true;
    end;
   finally
    TPasMPMultipleReaderSingleWriterSpinLock.WriteToRead(fCachedBoundingBoxLock);
   end;
  end;
 finally
  TPasMPMultipleReaderSingleWriterSpinLock.ReleaseRead(fCachedBoundingBoxLock);
 end;
end;

procedure TpvVectorPathSegmentLine.GetIntersectionPointsWithLineSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList);
begin
 GetIntersectionPointsForLineLine(TpvVectorPathSegmentLine(self),TpvVectorPathSegmentLine(aWith),aIntersectionPoints);
end;

procedure TpvVectorPathSegmentLine.GetIntersectionPointsWithQuadraticCurveSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList);
begin
 GetIntersectionPointsForLineQuadraticCurve(TpvVectorPathSegmentLine(self),TpvVectorPathSegmentQuadraticCurve(aWith),aIntersectionPoints);
end;

procedure TpvVectorPathSegmentLine.GetIntersectionPointsWithCubicCurveSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList);
begin
 GetIntersectionPointsForLineCubicCurve(TpvVectorPathSegmentLine(self),TpvVectorPathSegmentCubicCurve(aWith),aIntersectionPoints);
end;

procedure TpvVectorPathSegmentLine.GetIntersectionPointsWithSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList);
begin
 if assigned(self) and assigned(aWith) then begin
  aWith.GetIntersectionPointsWithLineSegment(self,aIntersectionPoints);
 end;
end;

procedure TpvVectorPathSegmentLine.Transform(const aMatrix:TpvVectorPathMatrix);
begin
 Points[0]:=aMatrix.Transform(Points[0]);
 Points[1]:=aMatrix.Transform(Points[1]);
 inherited Transform(aMatrix);
end;

{ TpvVectorPathSegmentMetaWindingSettingLine }

constructor TpvVectorPathSegmentMetaWindingSettingLine.Create;
begin
 inherited Create;
 fType:=TpvVectorPathSegmentType.MetaWindingSettingLine;
 fWinding:=0;
end;

constructor TpvVectorPathSegmentMetaWindingSettingLine.Create(const aP0,aP1:TpvVectorPathVector;const aWinding:TpvInt32);
begin
 Create;
 Points[0]:=aP0;
 Points[1]:=aP1;
 fWinding:=aWinding;
end;

{ TpvVectorPathSegmentQuadraticCurve }

constructor TpvVectorPathSegmentQuadraticCurve.Create;
begin
 inherited Create;
 fType:=TpvVectorPathSegmentType.QuadraticCurve;
end;

constructor TpvVectorPathSegmentQuadraticCurve.Create(const aP0,aP1,aP2:TpvVectorPathVector);
begin
 Create;
 Points[0]:=aP0;
 Points[1]:=aP1;
 Points[2]:=aP2;
end;

procedure TpvVectorPathSegmentQuadraticCurve.Assign(const aSegment:TpvVectorPathSegment);
begin
 if assigned(aSegment) and (aSegment is TpvVectorPathSegmentQuadraticCurve) then begin
  Points:=TpvVectorPathSegmentQuadraticCurve(aSegment).Points;
 end;
end;

function TpvVectorPathSegmentQuadraticCurve.Clone:TpvVectorPathSegment;
begin
 result:=TpvVectorPathSegmentQuadraticCurve.Create(Points[0],Points[1],Points[2]);
end;

function TpvVectorPathSegmentQuadraticCurve.GetBoundingBox:TpvVectorPathBoundingBox;
var t,s,Numerator,Denominator:TpvVectorPathVector;
begin
 TPasMPMultipleReaderSingleWriterSpinLock.AcquireRead(fCachedBoundingBoxLock);
 try
  if fHasCachedBoundingBox then begin
   result:=fCachedBoundingBox;
  end else begin
   TPasMPMultipleReaderSingleWriterSpinLock.ReadToWrite(fCachedBoundingBoxLock);
   try
    if fHasCachedBoundingBox then begin
     result:=fCachedBoundingBox;
    end else begin
     // This code calculates the bounding box for a quadratic bezier curve. It starts by initializing the
     // bounding box to the minimum and maximum values of the start and end points of the curve. It then
     // checks if the control point is already contained within the bounding box, and if it is not, it
     // calculates the value of t at which the derivative of the curve (which is a linear equation) is
     // equal to 0. If t is within the range of 0 to 1, the code calculates the corresponding point on the
     // curve and extends the bounding box to include that point if necessary.
     // Overall, this code appears to be well written and effective at calculating the bounding box for a
     // quadratic bezier curve. It is concise and uses a clever method for finding the extrema of the curve.
     result:=TpvVectorPathBoundingBox.Create(Points[0].Minimum(Points[2]),Points[0].Maximum(Points[2]));
     if not result.Contains(Points[1]) then begin
      // Since the bezier is quadratic, the bounding box can be compute here with a linear equation.
      // p = (1-t)^2*p0 + 2(1-t)t*p1 + t^2*p2
      // dp/dt = 2(t-1)*p0 + 2(1-2t)*p1 + 2t*p2 = t*(2*p0-4*p1+2*p2) + 2*(p1-p0)
      // dp/dt = 0 -> t*(p0-2*p1+p2) = (p0-p1);
      // Credits for the idea: Inigo Quilez
      Numerator:=Points[0]-Points[1];
      Denominator:=(Points[0]-(Points[1]*2.0))+Points[2];
      // A second difference of zero on an axis means the curve runs straight along it, so its
      // extremum there is one of the end points and there is no inner t to solve for. Dividing
      // anyway would give an infinity, or a NaN where the numerator vanishes as well, and a NaN
      // would survive the clamping below and poison the whole bounding box.
      if IsZero(Denominator.x) then begin
       t.x:=0.0;
      end else begin
       t.x:=Numerator.x/Denominator.x;
      end;
      if IsZero(Denominator.y) then begin
       t.y:=0.0;
      end else begin
       t.y:=Numerator.y/Denominator.y;
      end;
      if t.x<=0.0 then begin
       t.x:=0.0;
      end else if t.x>=1.0 then begin
       t.x:=1.0;
      end;
      if t.y<=0.0 then begin
       t.y:=0.0;
      end else if t.y>=1.0 then begin
       t.y:=1.0;
      end;
      s:=TpvVectorPathVector.Create(1.0,1.0)-t;
      result.Extend((Points[0]*(s*s))+(Points[1]*((t*s)*2.0))+(Points[2]*(t*t)));
     end;
     fCachedBoundingBox:=result;
     fHasCachedBoundingBox:=true;
    end;
   finally
    TPasMPMultipleReaderSingleWriterSpinLock.WriteToRead(fCachedBoundingBoxLock);
   end;
  end;
 finally
  TPasMPMultipleReaderSingleWriterSpinLock.ReleaseRead(fCachedBoundingBoxLock);
 end;
end;

procedure TpvVectorPathSegmentQuadraticCurve.GetIntersectionPointsWithLineSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList);
begin
 GetIntersectionPointsForLineQuadraticCurve(TpvVectorPathSegmentLine(aWith),TpvVectorPathSegmentQuadraticCurve(self),aIntersectionPoints);
end;

procedure TpvVectorPathSegmentQuadraticCurve.GetIntersectionPointsWithQuadraticCurveSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList);
begin
 GetIntersectionPointsForQuadraticCurveQuadraticCurve(TpvVectorPathSegmentQuadraticCurve(self),TpvVectorPathSegmentQuadraticCurve(aWith),aIntersectionPoints);
end;

procedure TpvVectorPathSegmentQuadraticCurve.GetIntersectionPointsWithCubicCurveSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList);
begin
 GetIntersectionPointsForQuadraticCurveCubicCurve(TpvVectorPathSegmentQuadraticCurve(self),TpvVectorPathSegmentCubicCurve(aWith),aIntersectionPoints);
end;

procedure TpvVectorPathSegmentQuadraticCurve.GetIntersectionPointsWithSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList);
begin
 if assigned(self) and assigned(aWith) then begin
  aWith.GetIntersectionPointsWithQuadraticCurveSegment(self,aIntersectionPoints);
 end;
end;

procedure TpvVectorPathSegmentQuadraticCurve.Transform(const aMatrix:TpvVectorPathMatrix);
begin
 Points[0]:=aMatrix.Transform(Points[0]);
 Points[1]:=aMatrix.Transform(Points[1]);
 Points[2]:=aMatrix.Transform(Points[2]);
 inherited Transform(aMatrix);
end;

{ TpvVectorPathSegmentCubicCurve }

constructor TpvVectorPathSegmentCubicCurve.Create;
begin
 inherited Create;
 fType:=TpvVectorPathSegmentType.CubicCurve;
end;

constructor TpvVectorPathSegmentCubicCurve.Create(const aP0,aP1,aP2,aP3:TpvVectorPathVector);
begin
 Create;
 Points[0]:=aP0;
 Points[1]:=aP1;
 Points[2]:=aP2;
 Points[3]:=aP3;
end;

procedure TpvVectorPathSegmentCubicCurve.Assign(const aSegment:TpvVectorPathSegment);
begin
 if assigned(aSegment) and (aSegment is TpvVectorPathSegmentCubicCurve) then begin
  Points:=TpvVectorPathSegmentCubicCurve(aSegment).Points;
 end;
end;

function TpvVectorPathSegmentCubicCurve.Clone:TpvVectorPathSegment;
begin
 result:=TpvVectorPathSegmentCubicCurve.Create(Points[0],Points[1],Points[2],Points[3]);
end;

function TpvVectorPathSegmentCubicCurve.GetBoundingBox:TpvVectorPathBoundingBox;
var c,b,a,h:TpvVectorPathVector;
    t,s,q:TpvDouble;
 function GetCurveParameter(const aNumerator,aDenominator:TpvDouble):TpvDouble;
 begin
  if IsZero(aDenominator) then begin
   // There is no root in this case, so just return a curve parameter value outside of the 0.0 .. 1.0
   // range, so that the caller skips it, instead of dividing by zero here.
   result:=-1.0;
  end else begin
   result:=aNumerator/aDenominator;
  end;
 end;
begin
 TPasMPMultipleReaderSingleWriterSpinLock.AcquireRead(fCachedBoundingBoxLock);
 try
  if fHasCachedBoundingBox then begin
   result:=fCachedBoundingBox;
  end else begin
   TPasMPMultipleReaderSingleWriterSpinLock.ReadToWrite(fCachedBoundingBoxLock);
   try
    if fHasCachedBoundingBox then begin
     result:=fCachedBoundingBox;
    end else begin
     // This code appears to be a correct implementation for computing the bounding box of a cubic bezier curve.
     // It uses the fact that the bounding box of a cubic Bezier curve can be computed by finding the roots of
     // quadratic equation formed from the bezier curve's coefficients. The roots of this equation correspond
     // to the parameter values at which the curve reaches an extreme point (i.e., a minimum or maximum). The
     // bounding box is then constructed by evaluating the curve at these parameter values and using the
     // resulting points to extend the initial bounding box.
     // One thing to note is that the code only handles the case where the roots of the quadratic equation are real.
     // If the roots are complex, the bounding box is not extended. This is acceptable since complex roots do not
     // correspond to physical points on the curve.
     // Overall, I would rate this code as good and efficient for computing the bounding box of a cubic bezier curve.
     result:=TpvVectorPathBoundingBox.Create(Points[0].Minimum(Points[3]),Points[0].Maximum(Points[3]));
     // Since the bezier is cubic, the bounding box can be compute here with a quadratic equation with
     // pascal triangle coefficients. Credits for the idea: Inigo Quilez
     a:=(((-Points[0])+(Points[1]*3.0))-(Points[2]*3.0))+Points[3];
     b:=(Points[0]-(Points[1]*2.0))+Points[2];
     c:=Points[1]-Points[0];
     h:=(b*b)-(c*a);
     if h.x>0.0 then begin
      h.x:=sqrt(h.x);
      t:=GetCurveParameter(c.x,(-b.x)-h.x);
      if (t>0.0) and (t<1.0) then begin
       s:=1.0-t;
       q:=(Points[0].x*(sqr(s)*s))+(Points[1].x*(3.0*sqr(s)*t))+(Points[2].x*(3.0*s*sqr(t)))+(Points[3].x*sqr(t)*t);
       if result.Min.x>q then begin
        result.Min.x:=q;
       end;
       if result.Max.x<q then begin
        result.Max.x:=q;
       end;
      end;
      t:=GetCurveParameter(c.x,(-b.x)+h.x);
      if (t>0.0) and (t<1.0) then begin
       s:=1.0-t;
       q:=(Points[0].x*(sqr(s)*s))+(Points[1].x*(3.0*sqr(s)*t))+(Points[2].x*(3.0*s*sqr(t)))+(Points[3].x*sqr(t)*t);
       if result.Min.x>q then begin
        result.Min.x:=q;
       end;
       if result.Max.x<q then begin
        result.Max.x:=q;
       end;
      end;
     end;
     if h.y>0.0 then begin
      h.y:=sqrt(h.y);
      t:=GetCurveParameter(c.y,(-b.y)-h.y);
      if (t>0.0) and (t<1.0) then begin
       s:=1.0-t;
       q:=(Points[0].y*(sqr(s)*s))+(Points[1].y*(3.0*sqr(s)*t))+(Points[2].y*(3.0*s*sqr(t)))+(Points[3].y*sqr(t)*t);
       if result.Min.y>q then begin
        result.Min.y:=q;
       end;
       if result.Max.y<q then begin
        result.Max.y:=q;
       end;
      end;
      t:=GetCurveParameter(c.y,(-b.y)+h.y);
      if (t>0.0) and (t<1.0) then begin
       s:=1.0-t;
       q:=(Points[0].y*(sqr(s)*s))+(Points[1].y*(3.0*sqr(s)*t))+(Points[2].y*(3.0*s*sqr(t)))+(Points[3].y*sqr(t)*t);
       if result.Min.y>q then begin
        result.Min.y:=q;
       end;
       if result.Max.y<q then begin
        result.Max.y:=q;
       end;
      end;
     end;
     fCachedBoundingBox:=result;
     fHasCachedBoundingBox:=true;
    end;
   finally
    TPasMPMultipleReaderSingleWriterSpinLock.WriteToRead(fCachedBoundingBoxLock);
   end;
  end;
 finally
  TPasMPMultipleReaderSingleWriterSpinLock.ReleaseRead(fCachedBoundingBoxLock);
 end;
end;

procedure TpvVectorPathSegmentCubicCurve.GetIntersectionPointsWithLineSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList);
begin
 GetIntersectionPointsForLineCubicCurve(TpvVectorPathSegmentLine(aWith),TpvVectorPathSegmentCubicCurve(self),aIntersectionPoints);
end;

procedure TpvVectorPathSegmentCubicCurve.GetIntersectionPointsWithQuadraticCurveSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList);
begin
 GetIntersectionPointsForQuadraticCurveCubicCurve(TpvVectorPathSegmentQuadraticCurve(aWith),TpvVectorPathSegmentCubicCurve(self),aIntersectionPoints);
end;

procedure TpvVectorPathSegmentCubicCurve.GetIntersectionPointsWithCubicCurveSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList);
begin
 GetIntersectionPointsForCubicCurveCubicCurve(TpvVectorPathSegmentCubicCurve(self),TpvVectorPathSegmentCubicCurve(aWith),aIntersectionPoints);
end;

procedure TpvVectorPathSegmentCubicCurve.GetIntersectionPointsWithSegment(const aWith:TpvVectorPathSegment;const aIntersectionPoints:TpvVectorPathVectorList);
begin
 if assigned(self) and assigned(aWith) then begin
  aWith.GetIntersectionPointsWithCubicCurveSegment(self,aIntersectionPoints);
 end;
end;

procedure TpvVectorPathSegmentCubicCurve.Transform(const aMatrix:TpvVectorPathMatrix);
begin
 Points[0]:=aMatrix.Transform(Points[0]);
 Points[1]:=aMatrix.Transform(Points[1]);
 Points[2]:=aMatrix.Transform(Points[2]);
 Points[3]:=aMatrix.Transform(Points[3]);
 inherited Transform(aMatrix);
end;

{ TpvVectorPathSegments }

procedure TpvVectorPathSegments.SortHorizontal;
type PByteArray=^TByteArray;
     TByteArray=array[0..$3fffffff] of TpvUInt8;
     PStackItem=^TStackItem;
     TStackItem=record
      Left,Right,Depth:TpvInt32;
     end;
var Left,Right,Depth,i,j,Middle,Size,Parent,Child,Pivot,iA,iB,iC:TpvSizeInt;
    StackItem:PStackItem;
    Stack:array[0..31] of TStackItem;
 function CompareItem(const a,b:TpvSizeInt):TpvSizeInt;
 var SegmentA,SegmentB:TpvVectorPathSegment;
 begin
  SegmentA:=Items[a];
  SegmentB:=Items[b];
  result:=Sign(SegmentA.GetCompareHorizontalLowestXCoordinate-SegmentB.GetCompareHorizontalLowestXCoordinate);
  if result=0 then begin
   result:=Sign(a-b);
  end;
 end;
begin
 begin
  if Count>1 then begin
   StackItem:=@Stack[0];
   StackItem^.Left:=0;
   StackItem^.Right:=Count-1;
   StackItem^.Depth:=IntLog2(Count) shl 1;
   inc(StackItem);
   while TpvPtrUInt(TpvPointer(StackItem))>TpvPtrUInt(TpvPointer(@Stack[0])) do begin
    dec(StackItem);
    Left:=StackItem^.Left;
    Right:=StackItem^.Right;
    Depth:=StackItem^.Depth;
    Size:=(Right-Left)+1;
    if Size<16 then begin
     // Insertion sort
     iA:=Left;
     iB:=iA+1;
     while iB<=Right do begin
      iC:=iB;
      while (iA>=Left) and
            (iC>=Left) and
            (CompareItem(iA,iC)>0) do begin
       Exchange(iA,iC);
       dec(iA);
       dec(iC);
      end;
      iA:=iB;
      inc(iB);
     end;
    end else begin
     if (Depth=0) or (TpvPtrUInt(TpvPointer(StackItem))>=TpvPtrUInt(TpvPointer(@Stack[high(Stack)-1]))) then begin
      // Heap sort
      i:=Size div 2;
      repeat
       if i>0 then begin
        dec(i);
       end else begin
        dec(Size);
        if Size>0 then begin
         Exchange(Left+Size,Left);
        end else begin
         break;
        end;
       end;
       Parent:=i;
       repeat
        Child:=(Parent*2)+1;
        if Child<Size then begin
         if (Child<(Size-1)) and (CompareItem(Left+Child,Left+Child+1)<0) then begin
          inc(Child);
         end;
         if CompareItem(Left+Parent,Left+Child)<0 then begin
          Exchange(Left+Parent,Left+Child);
          Parent:=Child;
          continue;
         end;
        end;
        break;
       until false;
      until false;
     end else begin
      // Quick sort width median-of-three optimization
      Middle:=Left+((Right-Left) shr 1);
      if (Right-Left)>3 then begin
       if CompareItem(Left,Middle)>0 then begin
        Exchange(Left,Middle);
       end;
       if CompareItem(Left,Right)>0 then begin
        Exchange(Left,Right);
       end;
       if CompareItem(Middle,Right)>0 then begin
        Exchange(Middle,Right);
       end;
      end;
      Pivot:=Middle;
      i:=Left;
      j:=Right;
      repeat
       while (i<Right) and (CompareItem(i,Pivot)<0) do begin
        inc(i);
       end;
       while (j>=i) and (CompareItem(j,Pivot)>0) do begin
        dec(j);
       end;
       if i>j then begin
        break;
       end else begin
        if i<>j then begin
         Exchange(i,j);
         if Pivot=i then begin
          Pivot:=j;
         end else if Pivot=j then begin
          Pivot:=i;
         end;
        end;
        inc(i);
        dec(j);
       end;
      until false;
      if i<Right then begin
       StackItem^.Left:=i;
       StackItem^.Right:=Right;
       StackItem^.Depth:=Depth-1;
       inc(StackItem);
      end;
      if Left<j then begin
       StackItem^.Left:=Left;
       StackItem^.Right:=j;
       StackItem^.Depth:=Depth-1;
       inc(StackItem);
      end;
     end;
    end;
   end;
  end;
 end;
end;

{ TpvVectorPathContour }

constructor TpvVectorPathContour.Create;
begin
 inherited Create;
 fSegments:=TpvVectorPathSegments.Create;
 fSegments.OwnsObjects:=true;
 fClosed:=false;
end;

constructor TpvVectorPathContour.Create(const aContour:TpvVectorPathContour);
begin
 Create;
 Assign(aContour);
end;

destructor TpvVectorPathContour.Destroy;
begin
 FreeAndNil(fSegments);
 inherited Destroy;
end;

procedure TpvVectorPathContour.Assign(const aContour:TpvVectorPathContour);
var Segment:TpvVectorPathSegment;
begin
 fSegments.Clear;
 if assigned(aContour) and assigned(aContour.fSegments) then begin
  for Segment in aContour.fSegments do begin
   if assigned(Segment) then begin
    fSegments.Add(Segment.Clone);
   end;
  end;
 end;
end;

function TpvVectorPathContour.Clone:TpvVectorPathContour;
begin
 result:=TpvVectorPathContour.Create(self);
end;

function TpvVectorPathContour.GetBoundingBox:TpvVectorPathBoundingBox;
var Segment:TpvVectorPathSegment;
begin
 result:=TpvVectorPathBoundingBox.Create(TpvVectorPathVector.Create(MaxDouble,MaxDouble),
                                         TpvVectorPathVector.Create(-MaxDouble,-MaxDouble));
 for Segment in fSegments do begin
  if assigned(Segment) then begin
   result:=result.Combine(Segment.GetBoundingBox);
  end;
 end;
end;

procedure TpvVectorPathContour.Transform(const aMatrix:TpvVectorPathMatrix);
var Segment:TpvVectorPathSegment;
begin
 for Segment in fSegments do begin
  if assigned(Segment) then begin
   Segment.Transform(aMatrix);
  end;
 end;
end;

{ TpvVectorPathShape }

constructor TpvVectorPathShape.Create;
begin
 inherited Create;
 fContours:=TpvVectorPathContours.Create;
 fContours.OwnsObjects:=true;
 fResolution:=32;
 fBoundingBoxExtent:=4.0;
 fCubicToQuadraticTolerance:=1.0;
 fAccelerationStructure:=TpvVectorPathAccelerationStructure.Grid;
 fCountBands:=0;
 fDuplicateSharing:=TpvVectorPathDuplicateSharing.EqualLists;
end;

constructor TpvVectorPathShape.Create(const aVectorPathShape:TpvVectorPathShape);
begin
 Create;
 if assigned(aVectorPathShape) then begin
  Assign(aVectorPathShape);
 end;
end;

constructor TpvVectorPathShape.Create(const aVectorPath:TpvVectorPath);
begin
 Create;
 if assigned(aVectorPath) then begin
  Assign(aVectorPath);
 end;
end;

destructor TpvVectorPathShape.Destroy;
begin
 FreeAndNil(fContours);
 inherited Destroy;
end;

procedure TpvVectorPathShape.Assign(const aVectorPathShape:TpvVectorPathShape);
var Contour:TpvVectorPathContour;
begin
 fContours.Clear;
 if assigned(aVectorPathShape) then begin
  fFillRule:=aVectorPathShape.fFillRule;
  if assigned(aVectorPathShape.fContours) then begin
   for Contour in aVectorPathShape.fContours do begin
    if assigned(Contour) then begin
     fContours.Add(Contour.Clone);
    end;
   end;
  end;
 end;
 fResolution:=aVectorPathShape.fResolution;
 fBoundingBoxExtent:=aVectorPathShape.fBoundingBoxExtent;
 fCubicToQuadraticTolerance:=aVectorPathShape.fCubicToQuadraticTolerance;
 fAccelerationStructure:=aVectorPathShape.fAccelerationStructure;
 fCountBands:=aVectorPathShape.fCountBands;
 fDuplicateSharing:=aVectorPathShape.fDuplicateSharing;
end;

procedure TpvVectorPathShape.Assign(const aVectorPath:TpvVectorPath);
var CommandIndex:TpvSizeInt;
    Command:TpvVectorPathCommand;
    Contour:TpvVectorPathContour;
    StartPoint,LastPoint,ControlPoint,OtherControlPoint,Point:TpvVectorPathVector;
begin
 fContours.Clear;
 if assigned(aVectorPath) then begin
  fFillRule:=aVectorPath.fFillRule;
  Contour:=nil;
  StartPoint:=TpvVectorPathVector.Create(0.0,0.0);
  LastPoint:=TpvVectorPathVector.Create(0.0,0.0);
  for CommandIndex:=0 to aVectorPath.fCommands.Count-1 do begin
   Command:=aVectorPath.fCommands[CommandIndex];
   case Command.CommandType of
    TpvVectorPathCommandType.MoveTo:begin
     if assigned(Contour) then begin
      if LastPoint<>StartPoint then begin
       Contour.fSegments.Add(TpvVectorPathSegmentLine.Create(LastPoint,StartPoint));
      end;
     end;
     Contour:=TpvVectorPathContour.Create;
     fContours.Add(Contour);
     LastPoint.x:=Command.x0;
     LastPoint.y:=Command.y0;
     StartPoint:=LastPoint;
    end;
    TpvVectorPathCommandType.LineTo:begin
     if not assigned(Contour) then begin
      Contour:=TpvVectorPathContour.Create;
      fContours.Add(Contour);
     end;
     Point.x:=Command.x0;
     Point.y:=Command.y0;
     if assigned(Contour) and (LastPoint<>Point) then begin
      Contour.fSegments.Add(TpvVectorPathSegmentLine.Create(LastPoint,Point));
     end;
     LastPoint:=Point;
    end;
    TpvVectorPathCommandType.QuadraticCurveTo:begin
     if not assigned(Contour) then begin
      Contour:=TpvVectorPathContour.Create;
      fContours.Add(Contour);
     end;
     ControlPoint.x:=Command.x0;
     ControlPoint.y:=Command.y0;
     Point.x:=Command.x1;
     Point.y:=Command.y1;
     if assigned(Contour) and not ((LastPoint=ControlPoint) and (LastPoint=Point)) then begin
      Contour.fSegments.Add(TpvVectorPathSegmentQuadraticCurve.Create(LastPoint,ControlPoint,Point));
     end;
     LastPoint:=Point;
    end;
    TpvVectorPathCommandType.CubicCurveTo:begin
     if not assigned(Contour) then begin
      Contour:=TpvVectorPathContour.Create;
      fContours.Add(Contour);
     end;
     ControlPoint.x:=Command.x0;
     ControlPoint.y:=Command.y0;
     OtherControlPoint.x:=Command.x1;
     OtherControlPoint.y:=Command.y1;
     Point.x:=Command.x2;
     Point.y:=Command.y2;
     if assigned(Contour) and not ((LastPoint=ControlPoint) and (LastPoint=OtherControlPoint) and (LastPoint=Point)) then begin
      Contour.fSegments.Add(TpvVectorPathSegmentCubicCurve.Create(LastPoint,ControlPoint,OtherControlPoint,Point));
     end;
     LastPoint:=Point;
    end;
    TpvVectorPathCommandType.Close:begin
     if assigned(Contour) then begin
      Contour.fClosed:=true;
      if LastPoint<>StartPoint then begin
       Contour.fSegments.Add(TpvVectorPathSegmentLine.Create(LastPoint,StartPoint));
      end;
     end;
     Contour:=nil;
    end;
   end;
  end;
 end;
end;

function TpvVectorPathShape.Clone:TpvVectorPathShape;
begin
 result:=TpvVectorPathShape.Create(self);
end;

function TpvVectorPathShape.GetBoundingBox:TpvVectorPathBoundingBox;
var Contour:TpvVectorPathContour;
begin
 result:=TpvVectorPathBoundingBox.Create(TpvVectorPathVector.Create(MaxDouble,MaxDouble),
                                         TpvVectorPathVector.Create(-MaxDouble,-MaxDouble));
 for Contour in fContours do begin
  if assigned(Contour) then begin
   result:=result.Combine(Contour.GetBoundingBox);
  end;
 end;
end;

procedure TpvVectorPathShape.Transform(const aMatrix:TpvVectorPathMatrix);
var Contour:TpvVectorPathContour;
begin
 for Contour in fContours do begin
  if assigned(Contour) then begin
   Contour.Transform(aMatrix);
  end;
 end;
end;

function TpvVectorPathShape.GetBeginEndPoints:TpvVectorPathVectors;
var Count:TpvSizeInt;
    Contour:TpvVectorPathContour;
    Segment:TpvVectorPathSegment;
begin
 result:=nil;
 Count:=0;
 try
  for Contour in fContours do begin
   for Segment in Contour.fSegments do begin
    case Segment.Type_ of
     TpvVectorPathSegmentType.Line:begin
      if (Count+1)>=length(result) then begin
       SetLength(result,(Count+2)*2);
      end;
      result[Count]:=TpvVectorPathSegmentLine(Segment).Points[0];
      result[Count+1]:=TpvVectorPathSegmentLine(Segment).Points[1];
      inc(Count,2);
     end;
     TpvVectorPathSegmentType.QuadraticCurve:begin
      if (Count+1)>=length(result) then begin
       SetLength(result,(Count+2)*2);
      end;
      result[Count]:=TpvVectorPathSegmentQuadraticCurve(Segment).Points[0];
      result[Count+1]:=TpvVectorPathSegmentQuadraticCurve(Segment).Points[2];
      inc(Count,2);
     end;
     TpvVectorPathSegmentType.CubicCurve:begin
      if (Count+1)>=length(result) then begin
       SetLength(result,(Count+2)*2);
      end;
      result[Count]:=TpvVectorPathSegmentCubicCurve(Segment).Points[0];
      result[Count+1]:=TpvVectorPathSegmentCubicCurve(Segment).Points[3];
      inc(Count,2);
     end;
     else begin
     end;
    end;
   end;
  end;
 finally
  SetLength(result,Count);
 end;
end;

// How many control points a segment of this type carries, which is what lets the points of a whole
// shape go onto the stream as one block with the types as another.
class function TpvVectorPathShape.CountSegmentPoints(const aType:TpvVectorPathSegmentType):TpvSizeInt;
begin
 case aType of
  TpvVectorPathSegmentType.Line,TpvVectorPathSegmentType.MetaWindingSettingLine:begin
   result:=2;
  end;
  TpvVectorPathSegmentType.QuadraticCurve:begin
   result:=3;
  end;
  TpvVectorPathSegmentType.CubicCurve:begin
   result:=4;
  end;
  else begin
   result:=0;
  end;
 end;
end;

// The heads and the points of a run of segments, each as one block. Writing them a number at a
// time is what made loading take a fifth of a second rather than the few milliseconds it should,
// so everything that comes in counts goes out in blocks, the way TpvScene3D handles its meshes.
class procedure TpvVectorPathShape.SaveSegmentsToStream(const aStreamIO:TpvStreamIO;const aSegments:TpvVectorPathSegments);
var Index,PointIndex,CountPoints,CountAllPoints:TpvSizeInt;
    Segment:TpvVectorPathSegment;
    Heads:array of TCachedSegmentHead;
    Points:array of TpvVectorPathVector;
begin
 aStreamIO.WriteInt32(aSegments.Count);
 if aSegments.Count=0 then begin
  aStreamIO.WriteInt32(0);
  exit;
 end;
 SetLength(Heads,aSegments.Count);
 CountAllPoints:=0;
 for Index:=0 to aSegments.Count-1 do begin
  Segment:=aSegments[Index];
  Heads[Index].Type_:=TpvUInt32(TpvVectorPathSegmentType(Segment.Type_));
  if Segment.Type_=TpvVectorPathSegmentType.MetaWindingSettingLine then begin
   Heads[Index].Winding:=TpvVectorPathSegmentMetaWindingSettingLine(Segment).Winding;
  end else begin
   Heads[Index].Winding:=0;
  end;
  inc(CountAllPoints,CountSegmentPoints(Segment.Type_));
 end;
 aStreamIO.WriteWithCheck(Heads[0],aSegments.Count*SizeOf(TCachedSegmentHead));
 aStreamIO.WriteInt32(CountAllPoints);
 if CountAllPoints>0 then begin
  SetLength(Points,CountAllPoints);
  CountAllPoints:=0;
  for Index:=0 to aSegments.Count-1 do begin
   Segment:=aSegments[Index];
   CountPoints:=CountSegmentPoints(Segment.Type_);
   for PointIndex:=0 to CountPoints-1 do begin
    case Segment.Type_ of
     TpvVectorPathSegmentType.Line,TpvVectorPathSegmentType.MetaWindingSettingLine:begin
      Points[CountAllPoints]:=TpvVectorPathSegmentLine(Segment).Points[PointIndex];
     end;
     TpvVectorPathSegmentType.QuadraticCurve:begin
      Points[CountAllPoints]:=TpvVectorPathSegmentQuadraticCurve(Segment).Points[PointIndex];
     end;
     TpvVectorPathSegmentType.CubicCurve:begin
      Points[CountAllPoints]:=TpvVectorPathSegmentCubicCurve(Segment).Points[PointIndex];
     end;
     else begin
      Points[CountAllPoints]:=TpvVectorPathVector.Create(0.0,0.0);
     end;
    end;
    inc(CountAllPoints);
   end;
  end;
  aStreamIO.WriteWithCheck(Points[0],CountAllPoints*SizeOf(TpvVectorPathVector));
 end;
end;

class procedure TpvVectorPathShape.LoadSegmentsFromStream(const aStreamIO:TpvStreamIO;const aSegments:TpvVectorPathSegments);
var Index,PointIndex,CountSegments,CountPoints,CountAllPoints:TpvSizeInt;
    Heads:array of TCachedSegmentHead;
    Points:array of TpvVectorPathVector;
    SegmentType:TpvVectorPathSegmentType;
    Segment:TpvVectorPathSegment;
begin
 CountSegments:=aStreamIO.ReadInt32;
 if CountSegments<=0 then begin
  aStreamIO.ReadInt32;
  exit;
 end;
 SetLength(Heads,CountSegments);
 aStreamIO.ReadWithCheck(Heads[0],CountSegments*SizeOf(TCachedSegmentHead));
 CountAllPoints:=aStreamIO.ReadInt32;
 if CountAllPoints>0 then begin
  SetLength(Points,CountAllPoints);
  aStreamIO.ReadWithCheck(Points[0],CountAllPoints*SizeOf(TpvVectorPathVector));
 end;
 PointIndex:=0;
 for Index:=0 to CountSegments-1 do begin
  SegmentType:=TpvVectorPathSegmentType(Heads[Index].Type_);
  CountPoints:=CountSegmentPoints(SegmentType);
  case SegmentType of
   TpvVectorPathSegmentType.Line:begin
    Segment:=TpvVectorPathSegmentLine.Create(Points[PointIndex],Points[PointIndex+1]);
   end;
   TpvVectorPathSegmentType.MetaWindingSettingLine:begin
    Segment:=TpvVectorPathSegmentMetaWindingSettingLine.Create(Points[PointIndex],Points[PointIndex+1],Heads[Index].Winding);
   end;
   TpvVectorPathSegmentType.QuadraticCurve:begin
    Segment:=TpvVectorPathSegmentQuadraticCurve.Create(Points[PointIndex],Points[PointIndex+1],Points[PointIndex+2]);
   end;
   TpvVectorPathSegmentType.CubicCurve:begin
    Segment:=TpvVectorPathSegmentCubicCurve.Create(Points[PointIndex],Points[PointIndex+1],Points[PointIndex+2],Points[PointIndex+3]);
   end;
   else begin
    Segment:=nil;
   end;
  end;
  if assigned(Segment) then begin
   Segment.fIndex:=aSegments.Count;
   aSegments.Add(Segment);
  end;
  inc(PointIndex,CountPoints);
 end;
end;

class procedure TpvVectorPathShape.SaveSegmentToStream(const aStreamIO:TpvStreamIO;const aSegment:TpvVectorPathSegment);
var PointIndex,CountPoints:TpvSizeInt;
begin
 aStreamIO.WriteUInt32(TpvUInt32(TpvVectorPathSegmentType(aSegment.Type_)));
 case aSegment.Type_ of
  TpvVectorPathSegmentType.Line:begin
   CountPoints:=2;
  end;
  TpvVectorPathSegmentType.MetaWindingSettingLine:begin
   CountPoints:=2;
  end;
  TpvVectorPathSegmentType.QuadraticCurve:begin
   CountPoints:=3;
  end;
  TpvVectorPathSegmentType.CubicCurve:begin
   CountPoints:=4;
  end;
  else begin
   CountPoints:=0;
  end;
 end;
 for PointIndex:=0 to CountPoints-1 do begin
  case aSegment.Type_ of
   TpvVectorPathSegmentType.Line,TpvVectorPathSegmentType.MetaWindingSettingLine:begin
    aStreamIO.WriteDouble(TpvVectorPathSegmentLine(aSegment).Points[PointIndex].x);
    aStreamIO.WriteDouble(TpvVectorPathSegmentLine(aSegment).Points[PointIndex].y);
   end;
   TpvVectorPathSegmentType.QuadraticCurve:begin
    aStreamIO.WriteDouble(TpvVectorPathSegmentQuadraticCurve(aSegment).Points[PointIndex].x);
    aStreamIO.WriteDouble(TpvVectorPathSegmentQuadraticCurve(aSegment).Points[PointIndex].y);
   end;
   TpvVectorPathSegmentType.CubicCurve:begin
    aStreamIO.WriteDouble(TpvVectorPathSegmentCubicCurve(aSegment).Points[PointIndex].x);
    aStreamIO.WriteDouble(TpvVectorPathSegmentCubicCurve(aSegment).Points[PointIndex].y);
   end;
   else begin
   end;
  end;
 end;
 // A meta winding setting line stands for everything to the left of a cell, and what it stands for
 // is the number it carries, so that goes with it.
 if aSegment.Type_=TpvVectorPathSegmentType.MetaWindingSettingLine then begin
  aStreamIO.WriteInt32(TpvVectorPathSegmentMetaWindingSettingLine(aSegment).Winding);
 end;
end;

class function TpvVectorPathShape.LoadSegmentFromStream(const aStreamIO:TpvStreamIO):TpvVectorPathSegment;
var SegmentType:TpvVectorPathSegmentType;
    PointIndex,CountPoints:TpvSizeInt;
    Points:array[0..3] of TpvVectorPathVector;
begin
 SegmentType:=TpvVectorPathSegmentType(aStreamIO.ReadUInt32);
 case SegmentType of
  TpvVectorPathSegmentType.Line:begin
   CountPoints:=2;
  end;
  TpvVectorPathSegmentType.MetaWindingSettingLine:begin
   CountPoints:=2;
  end;
  TpvVectorPathSegmentType.QuadraticCurve:begin
   CountPoints:=3;
  end;
  TpvVectorPathSegmentType.CubicCurve:begin
   CountPoints:=4;
  end;
  else begin
   CountPoints:=0;
  end;
 end;
 for PointIndex:=0 to CountPoints-1 do begin
  Points[PointIndex].x:=aStreamIO.ReadDouble;
  Points[PointIndex].y:=aStreamIO.ReadDouble;
 end;
 case SegmentType of
  TpvVectorPathSegmentType.Line:begin
   result:=TpvVectorPathSegmentLine.Create(Points[0],Points[1]);
  end;
  TpvVectorPathSegmentType.MetaWindingSettingLine:begin
   result:=TpvVectorPathSegmentMetaWindingSettingLine.Create(Points[0],Points[1],aStreamIO.ReadInt32);
  end;
  TpvVectorPathSegmentType.QuadraticCurve:begin
   result:=TpvVectorPathSegmentQuadraticCurve.Create(Points[0],Points[1],Points[2]);
  end;
  TpvVectorPathSegmentType.CubicCurve:begin
   result:=TpvVectorPathSegmentCubicCurve.Create(Points[0],Points[1],Points[2],Points[3]);
  end;
  else begin
   result:=nil;
  end;
 end;
end;

procedure TpvVectorPathShape.SaveToStream(const aStreamIO:TpvStreamIO);
var ContourIndex,SegmentIndex:TpvSizeInt;
    Contour:TpvVectorPathContour;
    ContourHeads:array of TCachedContourHead;
    All:TpvVectorPathSegments;
begin
 aStreamIO.WriteUInt32(TpvUInt32(TpvVectorPathFillRule(fFillRule)));
 aStreamIO.WriteInt32(fResolution);
 aStreamIO.WriteDouble(fBoundingBoxExtent);
 aStreamIO.WriteDouble(fCubicToQuadraticTolerance);
 aStreamIO.WriteUInt32(TpvUInt32(TpvVectorPathAccelerationStructure(fAccelerationStructure)));
 aStreamIO.WriteInt32(fCountBands);
 aStreamIO.WriteUInt32(TpvUInt32(TpvVectorPathDuplicateSharing(fDuplicateSharing)));
 aStreamIO.WriteInt32(fContours.Count);
 if fContours.Count>0 then begin
  // How the segments that follow are shared out over the contours, as one block.
  SetLength(ContourHeads,fContours.Count);
  for ContourIndex:=0 to fContours.Count-1 do begin
   Contour:=fContours[ContourIndex];
   if Contour.fClosed then begin
    ContourHeads[ContourIndex].Closed:=1;
   end else begin
    ContourHeads[ContourIndex].Closed:=0;
   end;
   ContourHeads[ContourIndex].CountSegments:=Contour.fSegments.Count;
  end;
  aStreamIO.WriteWithCheck(ContourHeads[0],fContours.Count*SizeOf(TCachedContourHead));
 end;
 All:=TpvVectorPathSegments.Create;
 try
  All.OwnsObjects:=false;
  for ContourIndex:=0 to fContours.Count-1 do begin
   Contour:=fContours[ContourIndex];
   for SegmentIndex:=0 to Contour.fSegments.Count-1 do begin
    All.Add(Contour.fSegments[SegmentIndex]);
   end;
  end;
  SaveSegmentsToStream(aStreamIO,All);
 finally
  FreeAndNil(All);
 end;
end;

procedure TpvVectorPathShape.LoadFromStream(const aStreamIO:TpvStreamIO);
var ContourIndex,SegmentIndex,CountContours,RunningIndex:TpvSizeInt;
    Contour:TpvVectorPathContour;
    ContourHeads:array of TCachedContourHead;
    All:TpvVectorPathSegments;
begin
 fFillRule:=TpvVectorPathFillRule(aStreamIO.ReadUInt32);
 fResolution:=aStreamIO.ReadInt32;
 fBoundingBoxExtent:=aStreamIO.ReadDouble;
 fCubicToQuadraticTolerance:=aStreamIO.ReadDouble;
 fAccelerationStructure:=TpvVectorPathAccelerationStructure(aStreamIO.ReadUInt32);
 fCountBands:=aStreamIO.ReadInt32;
 fDuplicateSharing:=TpvVectorPathDuplicateSharing(aStreamIO.ReadUInt32);
 fContours.Clear;
 CountContours:=aStreamIO.ReadInt32;
 if CountContours>0 then begin
  SetLength(ContourHeads,CountContours);
  aStreamIO.ReadWithCheck(ContourHeads[0],CountContours*SizeOf(TCachedContourHead));
 end;
 All:=TpvVectorPathSegments.Create;
 try
  All.OwnsObjects:=false;
  LoadSegmentsFromStream(aStreamIO,All);
  RunningIndex:=0;
  for ContourIndex:=0 to CountContours-1 do begin
   Contour:=TpvVectorPathContour.Create;
   try
    Contour.fClosed:=ContourHeads[ContourIndex].Closed<>0;
    for SegmentIndex:=0 to TpvSizeInt(ContourHeads[ContourIndex].CountSegments)-1 do begin
     if RunningIndex<All.Count then begin
      Contour.fSegments.Add(All[RunningIndex]);
      inc(RunningIndex);
     end;
    end;
   finally
    fContours.Add(Contour);
   end;
  end;
 finally
  FreeAndNil(All);
 end;
end;

procedure TpvVectorPathShape.GetSegmentIntersectionPoints(const aIntersectionPoints:TpvVectorPathVectorList);
var SegmentIndex,OtherSegmentIndex:TpvSizeInt;
    Segment,OtherSegment:TpvVectorPathSegment;
    Segments:TpvVectorPathSegments;
    Contour:TpvVectorPathContour;
begin
 aIntersectionPoints.Clear;
 Segments:=TpvVectorPathSegments.Create;
 try
  Segments.OwnsObjects:=false;
  for Contour in fContours do begin
   for Segment in Contour.fSegments do begin
    Segments.Add(Segment);
   end;
  end;
  for SegmentIndex:=0 to Segments.Count-1 do begin
   Segment:=Segments[SegmentIndex];
   for OtherSegmentIndex:=SegmentIndex+1 to Segments.Count-1 do begin
    OtherSegment:=Segments[OtherSegmentIndex];
    Segment.GetIntersectionPointsWithSegment(OtherSegment,aIntersectionPoints);
   end;
  end;
 finally
  FreeAndNil(Segments);
 end;
 aIntersectionPoints.RemoveDuplicates;
end;

procedure TpvVectorPathShape.ConvertCubicCurvesToQuadraticCurves(const aPixelRatio:TpvDouble);
var ValueOne,NearlyZeroValue,LengthScale:TpvDouble;
    Contour:TpvVectorPathContour;
 procedure ConvertCubicCurveToQuadraticCurve(const aCP0,aCP1,aCP2,aCP3:TpvVectorPathVector);
 const MaxChoppedPoints=10;
 type TChoppedPoints=array[0..MaxChoppedPoints-1] of TpvVectorPathVector;
 var ChoppedPoints:TChoppedPoints;
  procedure OutputLine(const aP0,aP1:TpvVectorPathVector);
  begin
   Contour.fSegments.Add(TpvVectorPathSegmentLine.Create(aP0,aP1));
  end;
  procedure OutputQuad(const aP0,aP1,aP2:TpvVectorPathVector);
  begin
   Contour.fSegments.Add(TpvVectorPathSegmentQuadraticCurve.Create(aP0,aP1,aP2));
  end;
  procedure ChopCubicAt(Src,Dst:PpvVectorPathRawVectors;const t:TpvDouble); overload;
  var p0,p1,p2,p3,ab,bc,cd,abc,bcd,abcd:TpvVectorPathVector;
  begin
   if SameValue(t,1.0) then begin
    Dst^[0]:=Src^[0];
    Dst^[1]:=Src^[1];
    Dst^[2]:=Src^[2];
    Dst^[3]:=Src^[3];
    Dst^[4]:=Src^[3];
    Dst^[5]:=Src^[3];
    Dst^[6]:=Src^[3];
   end else begin
    p0:=Src^[0];
    p1:=Src^[1];
    p2:=Src^[2];
    p3:=Src^[3];
    ab:=p0.Lerp(p1,t);
    bc:=p1.Lerp(p2,t);
    cd:=p2.Lerp(p3,t);
    abc:=ab.Lerp(bc,t);
    bcd:=bc.Lerp(cd,t);
    abcd:=abc.Lerp(bcd,t);
    Dst^[0]:=p0;
    Dst^[1]:=ab;
    Dst^[2]:=abc;
    Dst^[3]:=abcd;
    Dst^[4]:=bcd;
    Dst^[5]:=cd;
    Dst^[6]:=p3;
   end;
  end;
  procedure ChopCubicAt(Src,Dst:PpvVectorPathRawVectors;const t0,t1:TpvDouble); overload;
  var p0,p1,p2,p3,
      ab0,bc0,cd0,abc0,bcd0,abcd0,
      ab1,bc1,cd1,abc1,bcd1,abcd1,
      Middle0,Middle1:TpvVectorPathVector;
  begin
   if SameValue(t1,1.0) then begin
    ChopCubicAt(Src,Dst,t0);
    Dst^[7]:=Src^[3];
    Dst^[8]:=Src^[3];
    Dst^[9]:=Src^[3];
   end else begin
    p0:=Src^[0];
    p1:=Src^[1];
    p2:=Src^[2];
    p3:=Src^[3];
    ab0:=p0.Lerp(p1,t0);
    bc0:=p1.Lerp(p2,t0);
    cd0:=p2.Lerp(p3,t0);
    abc0:=ab0.Lerp(bc0,t0);
    bcd0:=bc0.Lerp(cd0,t0);
    abcd0:=abc0.Lerp(bcd0,t0);
    ab1:=p0.Lerp(p1,t1);
    bc1:=p1.Lerp(p2,t1);
    cd1:=p2.Lerp(p3,t1);
    abc1:=ab1.Lerp(bc1,t1);
    bcd1:=bc1.Lerp(cd1,t1);
    abcd1:=abc1.Lerp(bcd1,t1);
    Middle0:=abc0.Lerp(bcd0,t1);
    Middle1:=abc1.Lerp(bcd1,t0);
    Dst^[0]:=p0;
    Dst^[1]:=ab0;
    Dst^[2]:=abc0;
    Dst^[3]:=abcd0;
    Dst^[4]:=Middle0;
    Dst^[5]:=Middle1;
    Dst^[6]:=abcd1;
    Dst^[7]:=bcd1;
    Dst^[8]:=cd1;
    Dst^[9]:=p3;
   end;
  end;
  function ChopCubicAtInflections(const aSrc:array of TpvVectorPathVector;out aDst:TChoppedPoints):TpvSizeInt;
   function ValidUnitDivide(aNumerator,aDenominator:TpvDouble;out aRatio:TpvDouble):boolean;
   begin
    if aNumerator<0.0 then begin
     aNumerator:=-aNumerator;
     aDenominator:=-aDenominator;
    end;
    if IsZero(aNumerator) or IsZero(aDenominator) or (aNumerator>=aDenominator) then begin
     result:=false;
    end else begin
     aRatio:=aNumerator/aDenominator;
     if IsNaN(aRatio) or IsZero(aRatio) then begin
      result:=false;
     end else begin
      result:=true;
     end;
    end;
   end;
   function FindUnitQuadRoots(const A,B,C:TpvDouble;out aRoot0,aRoot1:TpvDouble):TpvSizeInt;
   var dr,Q:TpvDouble;
   begin
    if IsZero(A) then begin
     if ValidUnitDivide(-C,B,aRoot0) then begin
      result:=1;
     end else begin
      result:=0;
     end;
    end else begin
     dr:=sqr(B)-(4.0*A*C);
     if dr<0.0 then begin
      result:=0;
     end else begin
      dr:=sqrt(dr);
      if IsInfinite(dr) or IsNaN(dr) then begin
       result:=0;
      end else begin
       if B<0.0 then begin
        Q:=-(B-dr)*0.5;
       end else begin
        Q:=-(B+dr)*0.5;
       end;
       if ValidUnitDivide(Q,A,aRoot0) then begin
        if ValidUnitDivide(C,Q,aRoot1) then begin
         result:=2;
         if aRoot0>aRoot1 then begin
          Q:=aRoot0;
          aRoot0:=aRoot1;
          aRoot1:=Q;
         end else if SameValue(aRoot0,aRoot1) then begin
          dec(result);
         end;
        end else begin
         result:=1;
        end;
       end else begin
        if ValidUnitDivide(C,Q,aRoot0) then begin
         result:=1;
        end else begin
         result:=0;
        end;
       end;
      end;
     end;
    end;
   end;
  var Index,Count:TpvSizeInt;
      Times:array[0..1] of TpvDouble;
      Ax,Ay,Bx,By,Cx,Cy,t0,t1,LastTime:TpvDouble;
      Src:PpvVectorPathVector;
      Dst:PpvVectorPathVector;
  begin
   Ax:=aSrc[1].x-aSrc[0].x;
   Ay:=aSrc[1].y-aSrc[0].y;
   Bx:=aSrc[2].x-(2.0*aSrc[1].x)+aSrc[0].x;
   By:=aSrc[2].y-(2.0*aSrc[1].y)+aSrc[0].y;
   Cx:=aSrc[3].x+(3.0*(aSrc[1].x-aSrc[2].x))-aSrc[0].x;
   Cy:=aSrc[3].y+(3.0*(aSrc[1].y-aSrc[2].y))-aSrc[0].y;
   Count:=FindUnitQuadRoots((Bx*Cy)-(By*Cx),(Ax*Cy)-(Ay*Cx),(Ax*By)-(Ay*Bx),Times[0],Times[1]);
   if Count=0 then begin
    aDst[0]:=aSrc[0];
    aDst[1]:=aSrc[1];
    aDst[2]:=aSrc[2];
    aDst[3]:=aSrc[3];
   end else begin
    Src:=@aSrc[0];
    Dst:=@aDst[0];
    Index:=0;
    while Index<(Count-1) do begin
     t0:=Times[Index+0];
     t1:=Times[Index+1];
     if Index<>0 then begin
      LastTime:=Times[Index-1];
      t0:=Clamp((t0-LastTime)/(1.0-LastTime),0.0,1.0);
      t1:=Clamp((t1-LastTime)/(1.0-LastTime),0.0,1.0);
     end;
     ChopCubicAt(TpvPointer(Src),TpvPointer(Dst),t0,t1);
     inc(Src,4);
     inc(Dst,6);
     inc(Index,2);
    end;
    if Index<Count then begin
     t0:=Times[Index];
     if Index<>0 then begin
      LastTime:=Times[Index-1];
      t0:=Clamp((t0-LastTime)/(1.0-LastTime),0.0,1.0);
     end;
     ChopCubicAt(TpvPointer(Src),TpvPointer(Dst),t0);
    end;
   end;
   result:=Count+1;
  end;
  function IsNearlyZeroValue(const aValue:TpvDouble):Boolean;
  begin
   result:=(aValue<NearlyZeroValue) or IsZero(aValue);
  end;
  procedure ConvertNonInflectCubicToQuads(const aPoints:PpvVectorPathRawVectors;const aSquaredTolerance:TpvDouble;const aSubLevel:TpvSizeInt=0;const aPreserveFirstTangent:boolean=true;const aPreserveLastTangent:boolean=true);
  const MaxSubdivisions=10;
  var ab,dc,c0,c1,c:TpvVectorPathVector;
      p:array[0..7] of TpvVectorPathVector;
  begin
   ab:=aPoints^[1]-aPoints^[0];
   dc:=aPoints^[2]-aPoints^[3];
   if IsNearlyZeroValue(ab.LengthSquared) then begin
    if IsNearlyZeroValue(dc.LengthSquared) then begin
     OutputLine(aPoints^[0],aPoints^[3]);
     exit;
    end else begin
     ab:=aPoints^[2]-aPoints^[0];
    end;
   end;
   if IsNearlyZeroValue(dc.LengthSquared) then begin
    dc:=aPoints^[1]-aPoints^[3];
   end;
   ab.x:=ab.x*LengthScale;
   ab.y:=ab.y*LengthScale;
   dc.x:=dc.x*LengthScale;
   dc.y:=dc.y*LengthScale;
   c0:=aPoints^[0]+ab;
   c1:=aPoints^[3]+dc;
   if (aSubLevel>MaxSubdivisions) or ((c0-c1).LengthSquared<aSquaredTolerance) then begin
    if aPreserveFirstTangent=aPreserveLastTangent then begin
     c:=c0.Lerp(c1,0.5);
    end else if aPreserveFirstTangent then begin
     c:=c0;
    end else begin
     c:=c1;
    end;
    OutputQuad(aPoints^[0],c,aPoints^[3]);
   end else begin
    ChopCubicAt(aPoints,TpvPointer(@p[0]),0.5);
    ConvertNonInflectCubicToQuads(TpvPointer(@p[0]),aSquaredTolerance,aSubLevel+1,aPreserveFirstTangent,false);
    ConvertNonInflectCubicToQuads(TpvPointer(@p[3]),aSquaredTolerance,aSubLevel+1,false,aPreserveLastTangent);
   end;
  end;
 var Count,Index:TpvSizeInt;
     Points:array[0..3] of TpvVectorPathVector;
 begin
  Points[0]:=aCP0;
  Points[1]:=aCP1;
  Points[2]:=aCP2;
  Points[3]:=aCP3;
  if (IsNaN(Points[0].x) or IsInfinite(Points[0].x)) or
     (IsNaN(Points[0].y) or IsInfinite(Points[0].y)) or
     (IsNaN(Points[1].x) or IsInfinite(Points[1].x)) or
     (IsNaN(Points[1].y) or IsInfinite(Points[1].y)) or
     (IsNaN(Points[2].x) or IsInfinite(Points[2].x)) or
     (IsNaN(Points[2].y) or IsInfinite(Points[2].y)) or
     (IsNaN(Points[3].x) or IsInfinite(Points[3].x)) or
     (IsNaN(Points[3].y) or IsInfinite(Points[3].y)) then begin
   OutputLine(Points[0],Points[2]);
  end else begin
   Count:=ChopCubicAtInflections(Points,ChoppedPoints);
   if Count>0 then begin
    for Index:=0 to Count-1 do begin
     ConvertNonInflectCubicToQuads(TpvPointer(@ChoppedPoints[Index*3]),ValueOne,0,true,true);
    end;
   end;
  end;
 end;
var Segments:TpvVectorPathSegments;
    Segment:TpvVectorPathSegment;
begin
 ValueOne:=aPixelRatio;
 NearlyZeroValue:=ValueOne/TpvInt64(1 shl 18);
 LengthScale:=ValueOne*1.5;
 for Contour in fContours do begin
  Segments:=Contour.fSegments;
  try
   Contour.fSegments:=TpvVectorPathSegments.Create;
   Contour.fSegments.OwnsObjects:=true;
   for Segment in Segments do begin
    case Segment.Type_ of
     TpvVectorPathSegmentType.CubicCurve:begin
      ConvertCubicCurveToQuadraticCurve(TpvVectorPathSegmentCubicCurve(Segment).Points[0],
                                        TpvVectorPathSegmentCubicCurve(Segment).Points[1],
                                        TpvVectorPathSegmentCubicCurve(Segment).Points[2],
                                        TpvVectorPathSegmentCubicCurve(Segment).Points[3]);
     end;
     else begin
      Contour.fSegments.Add(Segment.Clone);
     end;
    end;
   end;
  finally
   FreeAndNil(Segments);
  end;
 end;
end;

procedure TpvVectorPathShape.ConvertCurvesToLines(const aPixelRatio:TpvDouble);
const CurveRecursionLimit=16;
var Contour:TpvVectorPathContour;
    CurveTessellationTolerance,CurveTessellationToleranceSquared,
    LastX,LastY:TpvDouble;
 procedure DoLineTo(const aToX,aToY:TpvDouble);
 begin
  if (not SameValue(LastX,aToX)) or (not SameValue(LastY,aToY)) then begin
   Contour.fSegments.Add(TpvVectorPathSegmentLine.Create(TpvVectorPathVector.Create(LastX,LastY),TpvVectorPathVector.Create(aToX,aToY)));
  end;
  LastX:=aToX;
  LastY:=aToY;
 end;
 procedure DoQuadraticCurveTo(const aLX,aLY,aC0X,aC0Y,aA0X,aA0Y:TpvDouble);
  procedure Recursive(const x1,y1,x2,y2,x3,y3:TpvDouble;const Level:TpvInt32);
  var x12,y12,x23,y23,x123,y123,dx,dy:TpvDouble;
  begin
   x12:=(x1+x2)*0.5;
   y12:=(y1+y2)*0.5;
   x23:=(x2+x3)*0.5;
   y23:=(y2+y3)*0.5;
   x123:=(x12+x23)*0.5;
   y123:=(y12+y23)*0.5;
   dx:=x3-x1;
   dy:=y3-y1;
   if (Level>CurveRecursionLimit) or
      ((Level>0) and
       (sqr(((x2-x3)*dy)-((y2-y3)*dx))<((sqr(dx)+sqr(dy))*CurveTessellationToleranceSquared))) then begin
    DoLineTo(x3,y3);
   end else begin
    Recursive(x1,y1,x12,y12,x123,y123,Level+1);
    Recursive(x123,y123,x23,y23,x3,y3,Level+1);
   end;
  end;
 begin
  LastX:=aLX;
  LastY:=aLY;
  Recursive(aLX,aLY,aC0X,aC0Y,aA0X,aA0Y,0);
  DoLineTo(aA0X,aA0Y);
 end;
 procedure DoCubicCurveTo(const aLX,aLY,aC0X,aC0Y,aC1X,aC1Y,aA0X,aA0Y:TpvDouble);
  procedure Recursive(const x1,y1,x2,y2,x3,y3,x4,y4:TpvDouble;const Level:TpvInt32);
  var x12,y12,x23,y23,x34,y34,x123,y123,x234,y234,x1234,y1234,dx,dy:TpvDouble;
  begin
   x12:=(x1+x2)*0.5;
   y12:=(y1+y2)*0.5;
   x23:=(x2+x3)*0.5;
   y23:=(y2+y3)*0.5;
   x34:=(x3+x4)*0.5;
   y34:=(y3+y4)*0.5;
   x123:=(x12+x23)*0.5;
   y123:=(y12+y23)*0.5;
   x234:=(x23+x34)*0.5;
   y234:=(y23+y34)*0.5;
   x1234:=(x123+x234)*0.5;
   y1234:=(y123+y234)*0.5;
   dx:=x4-x1;
   dy:=y4-y1;
   if (Level>CurveRecursionLimit) or
      ((Level>0) and
       (sqr(abs(((x2-x4)*dy)-((y2-y4)*dx))+
            abs(((x3-x4)*dy)-((y3-y4)*dx)))<((sqr(dx)+sqr(dy))*CurveTessellationToleranceSquared))) then begin
    DoLineTo(x4,y4);
   end else begin
    Recursive(x1,y1,x12,y12,x123,y123,x1234,y1234,Level+1);
    Recursive(x1234,y1234,x234,y234,x34,y34,x4,y4,Level+1);
   end;
  end;
 begin
  LastX:=aLX;
  LastY:=aLY;
  Recursive(aLX,aLY,aC0X,aC0Y,aC1X,aC1Y,aA0X,aA0Y,0);
  DoLineTo(aA0X,aA0Y);
 end;
var Segments:TpvVectorPathSegments;
    Segment:TpvVectorPathSegment;
begin
 CurveTessellationTolerance:=aPixelRatio*0.125;
 CurveTessellationToleranceSquared:=CurveTessellationTolerance*CurveTessellationTolerance;
 LastX:=0.0;
 LastY:=0.0;
 for Contour in fContours do begin
  Segments:=Contour.fSegments;
  try
   Contour.fSegments:=TpvVectorPathSegments.Create;
   Contour.fSegments.OwnsObjects:=true;
   for Segment in Segments do begin
    case Segment.Type_ of
     TpvVectorPathSegmentType.QuadraticCurve:begin
      DoQuadraticCurveTo(TpvVectorPathSegmentQuadraticCurve(Segment).Points[0].x,TpvVectorPathSegmentQuadraticCurve(Segment).Points[0].y,
                         TpvVectorPathSegmentQuadraticCurve(Segment).Points[1].x,TpvVectorPathSegmentQuadraticCurve(Segment).Points[1].y,
                         TpvVectorPathSegmentQuadraticCurve(Segment).Points[2].x,TpvVectorPathSegmentQuadraticCurve(Segment).Points[2].y);
     end;
     TpvVectorPathSegmentType.CubicCurve:begin
      DoCubicCurveTo(TpvVectorPathSegmentCubicCurve(Segment).Points[0].x,TpvVectorPathSegmentCubicCurve(Segment).Points[0].y,
                     TpvVectorPathSegmentCubicCurve(Segment).Points[1].x,TpvVectorPathSegmentCubicCurve(Segment).Points[1].y,
                     TpvVectorPathSegmentCubicCurve(Segment).Points[2].x,TpvVectorPathSegmentCubicCurve(Segment).Points[2].y,
                     TpvVectorPathSegmentCubicCurve(Segment).Points[3].x,TpvVectorPathSegmentCubicCurve(Segment).Points[3].y);
     end;
     else begin
      Contour.fSegments.Add(Segment.Clone);
     end;
    end;
   end;
  finally
   FreeAndNil(Segments);
  end;
 end;
end;

function TpvVectorPathShape.GetSignedDistance(const aX,aY,aScale:TpvDouble;out aInsideOutsideSign:TpvInt32):TpvDouble;
const CurveTessellationTolerance=0.25;
      CurveTessellationToleranceSquared=CurveTessellationTolerance*CurveTessellationTolerance;
      CurveRecursionLimit=16;
var ResultDistance,LastX,LastY:TpvDouble;
 procedure LineDistance(const aPX,aPY,aAX,aAY,aBX,aBY:TpvDouble);
 var pax,pay,bax,bay,t:TpvDouble;
 begin
  pax:=aPX-aAX;
  pay:=aPY-aAY;
  bax:=aBX-aAX;
  bay:=aBY-aAY;
  if ((aAY>aPY)<>(aBY>aPY)) and (pax<(bax*(pay/bay))) then begin
   aInsideOutsideSign:=-aInsideOutsideSign;
  end;
  t:=sqr(bax)+sqr(bay);
  if t>0.0 then begin
   t:=Min(Max(((pax*bax)+(pay*bay))/t,0.0),1.0);
  end else begin
   t:=0.0;
  end;
  ResultDistance:=Min(ResultDistance,sqr(pax-(bax*t))+sqr(pay-(bay*t)));
 end;
 procedure DoLineTo(const aLX,aLY,aToX,aToY:TpvDouble);
 begin
  LineDistance(aX,aY,aLX,aLY,aToX,aToY);
  LastX:=aToX;
  LastY:=aToY;
 end;
 procedure DoQuadraticCurveTo(const aLX,aLY,aC0X,aC0Y,aA0X,aA0Y:TpvDouble);
  procedure Recursive(const x1,y1,x2,y2,x3,y3:TpvDouble;const Level:TpvInt32);
  var x12,y12,x23,y23,x123,y123,dx,dy:TpvDouble;
  begin
   x12:=(x1+x2)*0.5;
   y12:=(y1+y2)*0.5;
   x23:=(x2+x3)*0.5;
   y23:=(y2+y3)*0.5;
   x123:=(x12+x23)*0.5;
   y123:=(y12+y23)*0.5;
   dx:=x3-x1;
   dy:=y3-y1;
   if (Level>CurveRecursionLimit) or
      ((Level>0) and
       (sqr(((x2-x3)*dy)-((y2-y3)*dx))<((sqr(dx)+sqr(dy))*CurveTessellationToleranceSquared))) then begin
    DoLineTo(LastX,LastY,x3,y3);
   end else begin
    Recursive(x1,y1,x12,y12,x123,y123,level+1);
    Recursive(x123,y123,x23,y23,x3,y3,level+1);
   end;
  end;
 begin
  LastX:=aLX;
  LastY:=aLY;
  Recursive(aLX,aLY,aC0X,aC0Y,aA0X,aA0Y,0);
  DoLineTo(LastX,LastY,aA0X,aA0Y);
 end;
 procedure DoCubicCurveTo(const aLX,aLY,aC0X,aC0Y,aC1X,aC1Y,aA0X,aA0Y:TpvDouble);
  procedure Recursive(const x1,y1,x2,y2,x3,y3,x4,y4:TpvDouble;const Level:TpvInt32);
  var x12,y12,x23,y23,x34,y34,x123,y123,x234,y234,x1234,y1234,dx,dy:TpvDouble;
  begin
   x12:=(x1+x2)*0.5;
   y12:=(y1+y2)*0.5;
   x23:=(x2+x3)*0.5;
   y23:=(y2+y3)*0.5;
   x34:=(x3+x4)*0.5;
   y34:=(y3+y4)*0.5;
   x123:=(x12+x23)*0.5;
   y123:=(y12+y23)*0.5;
   x234:=(x23+x34)*0.5;
   y234:=(y23+y34)*0.5;
   x1234:=(x123+x234)*0.5;
   y1234:=(y123+y234)*0.5;
   dx:=x4-x1;
   dy:=y4-y1;
   if (Level>CurveRecursionLimit) or
      ((Level>0) and
       (sqr(abs(((x2-x4)*dy)-((y2-y4)*dx))+
            abs(((x3-x4)*dy)-((y3-y4)*dx)))<((sqr(dx)+sqr(dy))*CurveTessellationToleranceSquared))) then begin
    DoLineTo(LastX,LastY,x4,y4);
   end else begin
    Recursive(x1,y1,x12,y12,x123,y123,x1234,y1234,Level+1);
    Recursive(x1234,y1234,x234,y234,x34,y34,x4,y4,Level+1);
   end;
  end;
 begin
  LastX:=aLX;
  LastY:=aLY;
  Recursive(aLX,aLY,aC0X,aC0Y,aC1X,aC1Y,aA0X,aA0Y,0);
  DoLineTo(LastX,LastY,aA0X,aA0Y);
 end;
var Contour:TpvVectorPathContour;
    Segments:TpvVectorPathSegments;
    Segment:TpvVectorPathSegment;
begin
 ResultDistance:=Infinity;
 aInsideOutsideSign:=1;
 LastX:=0.0;
 LastY:=0.0;
 for Contour in fContours do begin
  for Segment in Contour.fSegments do begin
   case Segment.Type_ of
    TpvVectorPathSegmentType.Line:begin
     DoLineTo(TpvVectorPathSegmentLine(Segment).Points[0].x,TpvVectorPathSegmentLine(Segment).Points[0].y,
              TpvVectorPathSegmentLine(Segment).Points[1].x,TpvVectorPathSegmentLine(Segment).Points[1].y);
    end;
    TpvVectorPathSegmentType.QuadraticCurve:begin
     DoQuadraticCurveTo(TpvVectorPathSegmentQuadraticCurve(Segment).Points[0].x,TpvVectorPathSegmentQuadraticCurve(Segment).Points[0].y,
                        TpvVectorPathSegmentQuadraticCurve(Segment).Points[1].x,TpvVectorPathSegmentQuadraticCurve(Segment).Points[1].y,
                        TpvVectorPathSegmentQuadraticCurve(Segment).Points[2].x,TpvVectorPathSegmentQuadraticCurve(Segment).Points[2].y);
    end;
    TpvVectorPathSegmentType.CubicCurve:begin
     DoCubicCurveTo(TpvVectorPathSegmentCubicCurve(Segment).Points[0].x,TpvVectorPathSegmentCubicCurve(Segment).Points[0].y,
                    TpvVectorPathSegmentCubicCurve(Segment).Points[1].x,TpvVectorPathSegmentCubicCurve(Segment).Points[1].y,
                    TpvVectorPathSegmentCubicCurve(Segment).Points[2].x,TpvVectorPathSegmentCubicCurve(Segment).Points[2].y,
                    TpvVectorPathSegmentCubicCurve(Segment).Points[3].x,TpvVectorPathSegmentCubicCurve(Segment).Points[3].y);
    end;
    else begin
    end;
   end;
  end;
 end;
 result:=sqrt(ResultDistance);
end;

{ TpvVectorPath }

constructor TpvVectorPath.Create;
begin
 inherited Create;
 fShape:=nil;
 fCommands:=TpvVectorPathCommandList.Create(true);
 fFillRule:=TpvVectorPathFillRule.EvenOdd;
 fStartPointSeen:=false;
end;

constructor TpvVectorPath.CreateFromSVGPath(const aCommands:TpvRawByteString);
var i,SrcPos,StartSrcPos,SrcLen,large_arc_flag,sweep_flag:TpvInt32;
    lx,ly,lcx,lcy,x0,y0,x1,y1,x2,y2,lmx,lmy,rx,ry,x_axis_rotation,x,y:TpvDouble;
    Src:TpvRawByteString;
    Command,LastCommand:AnsiChar;
    StartCommandCount:TpvSizeInt;
    MissingNumber:boolean;
 procedure SkipBlank;
 begin
  while (SrcPos<=SrcLen) and (Src[SrcPos] in [#0..#32]) do begin
   inc(SrcPos);
  end;
 end;
 function GetFloat:TpvDouble;
 var StartPos,CountDigits:TpvInt32;
 begin
  SkipBlank;
  StartPos:=SrcPos;
  CountDigits:=0;
  if (SrcPos<=SrcLen) and (Src[SrcPos] in ['-','+']) then begin
   inc(SrcPos);
  end;
  while (SrcPos<=SrcLen) and (Src[SrcPos] in ['0'..'9']) do begin
   inc(SrcPos);
   inc(CountDigits);
  end;
  if (SrcPos<=SrcLen) and (Src[SrcPos] in ['.']) then begin
   inc(SrcPos);
   while (SrcPos<=SrcLen) and (Src[SrcPos] in ['0'..'9']) do begin
    inc(SrcPos);
    inc(CountDigits);
   end;
  end;
  if (SrcPos<=SrcLen) and (Src[SrcPos] in ['e','E']) then begin
   inc(SrcPos);
   if (SrcPos<=SrcLen) and (Src[SrcPos] in ['-','+']) then begin
    inc(SrcPos);
   end;
   while (SrcPos<=SrcLen) and (Src[SrcPos] in ['0'..'9']) do begin
    inc(SrcPos);
   end;
  end;
  if CountDigits>0 then begin
   result:=ConvertStringToDouble(TpvRawByteString(copy(String(Src),StartPos,SrcPos-StartPos)),rmNearest,nil,-1);
  end else begin
   // There is no number at all at this position, not even a single digit, so the whole current command
   // is incomplete and must be dropped again by the caller.
   result:=0.0;
   MissingNumber:=true;
  end;
  SkipBlank;
 end;
 function GetInt:TpvInt32;
 var CountDigits:TpvInt32;
     s:TpvRawByteString;
 begin
  SkipBlank;
  s:='';
  CountDigits:=0;
  if (SrcPos<=SrcLen) and (Src[SrcPos] in ['-','+']) then begin
   s:=s+Src[SrcPos];
   inc(SrcPos);
  end;
  while (SrcPos<=SrcLen) and (Src[SrcPos] in ['0'..'9']) do begin
   s:=s+Src[SrcPos];
   inc(SrcPos);
   inc(CountDigits);
  end;
  if (SrcPos<=SrcLen) and (Src[SrcPos] in ['.']) then begin
   inc(SrcPos);
   while (SrcPos<=SrcLen) and (Src[SrcPos] in ['0'..'9']) do begin
    inc(SrcPos);
   end;
  end;
  if (SrcPos<=SrcLen) and (Src[SrcPos] in ['e','E']) then begin
   inc(SrcPos);
   if (SrcPos<=SrcLen) and (Src[SrcPos] in ['-','+']) then begin
    inc(SrcPos);
   end;
   while (SrcPos<=SrcLen) and (Src[SrcPos] in ['0'..'9']) do begin
    inc(SrcPos);
   end;
  end;
  SkipBlank;
  if CountDigits=0 then begin
   // There is no number at all at this position, so the arc flag is missing and the whole current
   // command is incomplete and must be dropped again by the caller.
   MissingNumber:=true;
  end else begin
   // Nothing to do here in this case
  end;
  result:=Trunc(ConvertStringToDouble(s,rmNearest,nil,-1));
 end;
 procedure ConvertArcToCubicCurves(rx,ry,x_axis_rotation:TpvDouble;large_arc_flag,sweep_flag:TpvInt32;x,y:TpvDouble);
 var sin_th,cos_th,a00,a01,a10,a11,x0,y0,x1,y1,xc,yc,d,sfactor,sfactor_sq,
     th0,th1,th_arc,dx,dy,dx1,dy1,Pr1,Pr2,Px,Py,check:TpvDouble;
     i,n_segs:TpvInt32;
  procedure ProcessSegment(xc,yc,th0,th1,rx,ry,x_axis_rotation:TpvDouble);
  var sin_th,cos_th,a00,a01,a10,a11,x1,y1,x2,y2,x3,y3,t,th_half:TpvDouble;
      i:TpvInt32;
  begin
   sin_th:=sin(x_axis_rotation*deg2rad);
   cos_th:=cos(x_axis_rotation*deg2rad);
   a00:=cos_th*rx;
   a01:=(-sin_th)*ry;
   a10:=sin_th*rx;
   a11:=cos_th*ry;
   th_half:=0.5*(th1-th0);
   t:=(8.0/3.0)*sin(th_half*0.5)*sin(th_half*0.5)/sin(th_half);
   x1:=(xc+cos(th0))-(t*sin(th0));
   y1:=(yc+sin(th0))+(t*cos(th0));
   x3:=xc+cos(th1);
   y3:=yc+sin(th1);
   x2:=x3+(t*sin(th1));
   y2:=y3-(t*cos(th1));
   CubicCurveTo((a00*x1)+(a01*y1),(a10*x1)+(a11*y1),
                (a00*x2)+(a01*y2),(a10*x2)+(a11*y2),
                (a00*x3)+(a01*y3),(a10*x3)+(a11*y3));
  end;
 begin
  if (abs(rx)<1e-14) or (abs(ry)<1e-14) then begin
   LineTo(x,y);
   exit;
  end;
  sin_th:=sin(x_axis_rotation*deg2rad);
  cos_th:=cos(x_axis_rotation*deg2rad);
  dx:=(lx-x)*0.5;
  dy:=(ly-y)*0.5;
  dx1:=(cos_th*dx)+(sin_th*dy);
  dy1:=(cos_th*dy)-(sin_th*dx);
  Pr1:=sqr(rx);
  Pr2:=sqr(ry);
  Px:=sqr(dx1);
  Py:=sqr(dy1);
  check:=(Px/Pr1)+(Py/Pr2);
  if check>1.0 then begin
   rx:=rx*sqrt(check);
   ry:=ry*sqrt(check);
  end;
  a00:=cos_th/rx;
  a01:=sin_th/rx;
  a10:=(-sin_th)/ry;
  a11:=cos_th/ry;
  x0:=(a00*lx)+(a01*ly);
  y0:=(a10*lx)+(a11*ly);
  x1:=(a00*x)+(a01*y);
  y1:=(a10*x)+(a11*y);
  d:=sqr(x1-x0)+sqr(y1-y0);
  sfactor_sq:=(1.0/d)-0.25;
  if sfactor_sq<0.0 then begin
   sfactor_sq:=0.0;
  end;
  sfactor:=sqrt(sfactor_sq);
  if sweep_flag=large_arc_flag then begin
   sfactor:=-sfactor;
  end;
  xc:=(0.5*(x0+x1))-(sfactor*(y1-y0));
  yc:=(0.5*(y0+y1))+(sfactor*(x1-x0));
  th0:=arctan2(y0-yc,x0-xc);
  th1:=arctan2(y1-yc,x1-xc);
  th_arc:=th1-th0;
  if (th_arc<0.0) and (sweep_flag<>0) then begin
   th_arc:=th_arc+TwoPI;
  end else if (th_arc>0.0) and (sweep_flag=0) then begin
   th_arc:=th_arc-TwoPI;
  end;
  n_segs:=ceil(abs(th_arc/((pi*0.5)+0.001)));
  for i:=0 to n_segs-1 do begin
   ProcessSegment(xc,yc,
                  th0+((i*th_arc)/n_segs),
                  th0+(((i+1)*th_arc)/n_segs),
                  rx,ry,
                  x_axis_rotation);
  end;
  if n_segs=0 then begin
   LineTo(x,y);
  end;
 end;
begin
 Create;
 SrcLen:=length(aCommands);
 Src:=aCommands;
 for i:=1 to SrcLen do begin
  if Src[i] in [#0..#32,','] then begin
   Src[i]:=' ';
  end;
 end;
 lx:=0;
 ly:=0;
 lcx:=0;
 lcy:=0;
 lmx:=0;
 lmy:=0;
 SrcPos:=1;
 Command:=#0;
 LastCommand:=#0;
 while SrcPos<=SrcLen do begin
  SkipBlank;
  if SrcPos<=SrcLen then begin
   StartSrcPos:=SrcPos;
   StartCommandCount:=fCommands.Count;
   MissingNumber:=false;
   if Src[SrcPos] in ['A'..'Z','a'..'z'] then begin
    Command:=Src[SrcPos];
    inc(SrcPos);
    SkipBlank;
   end;
   case Command of
    'Z','z':begin
     Close;
     lx:=lmx;
     ly:=lmy;
     // A close path command has no parameters at all and therefore cannot be repeated implicitly, so
     // forget it here, so that any directly following coordinates do end the whole parsing instead of
     // emitting further close path commands.
     Command:=#0;
    end;
    'H':begin
     lx:=GetFloat;
     LineTo(lx,ly);
    end;
    'h':begin
     lx:=lx+GetFloat;
     LineTo(lx,ly);
    end;
    'V':begin
     ly:=GetFloat;
     LineTo(lx,ly);
    end;
    'v':begin
     ly:=ly+GetFloat;
     LineTo(lx,ly);
    end;
    'M':begin
     lx:=GetFloat;
     ly:=GetFloat;
     lmx:=lx;
     lmy:=ly;
     MoveTo(lx,ly);
     Command:='L';
    end;
    'm':begin
     lx:=lx+GetFloat;
     ly:=ly+GetFloat;
     lmx:=lx;
     lmy:=ly;
     MoveTo(lx,ly);
     Command:='l';
    end;
    'L':begin
     lx:=GetFloat;
     ly:=GetFloat;
     LineTo(lx,ly);
    end;
    'l':begin
     lx:=lx+GetFloat;
     ly:=ly+GetFloat;
     LineTo(lx,ly);
    end;
    'A':begin
     rx:=GetFloat;
     ry:=GetFloat;
     x_axis_rotation:=GetFloat;
     large_arc_flag:=GetInt;
     sweep_flag:=GetInt;
     x:=GetFloat;
     y:=GetFloat;
     ConvertArcToCubicCurves(rx,ry,x_axis_rotation,large_arc_flag,sweep_flag,x,y);
     lx:=x;
     ly:=y;
    end;
    'a':begin
     rx:=GetFloat;
     ry:=GetFloat;
     x_axis_rotation:=GetFloat;
     large_arc_flag:=GetInt;
     sweep_flag:=GetInt;
     x:=lx+GetFloat;
     y:=ly+GetFloat;
     ConvertArcToCubicCurves(rx,ry,x_axis_rotation,large_arc_flag,sweep_flag,x,y);
     lx:=x;
     ly:=y;
    end;
    'T':begin
     lcx:=lx+(lx-lcx);
     lcy:=ly+(ly-lcy);
     if not (LastCommand in ['T','t','Q','q']) then begin
      lcx:=lx;
      lcy:=ly;
     end;
     lx:=GetFloat;
     ly:=GetFloat;
     QuadraticCurveTo(lcx,lcy,lx,ly);
    end;
    't':begin
     lcx:=lx+(lx-lcx);
     lcy:=ly+(ly-lcy);
     if not (LastCommand in ['T','t','Q','q']) then begin
      lcx:=lx;
      lcy:=ly;
     end;
     lx:=lx+GetFloat;
     ly:=ly+GetFloat;
     QuadraticCurveTo(lcx,lcy,lx,ly);
    end;
    'S':begin
     x0:=lx+(lx-lcx);
     y0:=ly+(ly-lcy);
     if not (LastCommand in ['S','s','C','c']) then begin
      x0:=lx;
      y0:=ly;
     end;
     x1:=GetFloat;
     y1:=GetFloat;
     x2:=GetFloat;
     y2:=GetFloat;
     lcx:=x1;
     lcy:=y1;
     lx:=x2;
     ly:=y2;
     CubicCurveTo(x0,y0,x1,y1,x2,y2);
    end;
    's':begin
     x0:=lx+(lx-lcx);
     y0:=ly+(ly-lcy);
     if not (LastCommand in ['S','s','C','c']) then begin
      x0:=lx;
      y0:=ly;
     end;
     x1:=lx+GetFloat;
     y1:=ly+GetFloat;
     x2:=lx+GetFloat;
     y2:=ly+GetFloat;
     lcx:=x1;
     lcy:=y1;
     lx:=x2;
     ly:=y2;
     CubicCurveTo(x0,y0,x1,y1,x2,y2);
    end;
    'Q':begin
     x0:=GetFloat;
     y0:=GetFloat;
     x1:=GetFloat;
     y1:=GetFloat;
     lcx:=x0;
     lcy:=y0;
     lx:=x1;
     ly:=y1;
     QuadraticCurveTo(x0,y0,x1,y1);
    end;
    'q':begin
     x0:=lx+GetFloat;
     y0:=ly+GetFloat;
     x1:=lx+GetFloat;
     y1:=ly+GetFloat;
     lcx:=x0;
     lcy:=y0;
     lx:=x1;
     ly:=y1;
     QuadraticCurveTo(x0,y0,x1,y1);
    end;
    'C':begin
     x0:=GetFloat;
     y0:=GetFloat;
     x1:=GetFloat;
     y1:=GetFloat;
     x2:=GetFloat;
     y2:=GetFloat;
     lcx:=x1;
     lcy:=y1;
     lx:=x2;
     ly:=y2;
     CubicCurveTo(x0,y0,x1,y1,x2,y2);
    end;
    'c':begin
     x0:=lx+GetFloat;
     y0:=ly+GetFloat;
     x1:=lx+GetFloat;
     y1:=ly+GetFloat;
     x2:=lx+GetFloat;
     y2:=ly+GetFloat;
     lcx:=x1;
     lcy:=y1;
     lx:=x2;
     ly:=y2;
     CubicCurveTo(x0,y0,x1,y1,x2,y2);
    end;
    else begin
     break;
    end;
   end;
   if MissingNumber then begin
    // At least one coordinate of the current command was missing, so the path data is malformed from
    // here on. Drop the incompletely parsed command again and stop, so that everything up to the error
    // stays, like the SVG specification demands it for erroneous path data.
    while fCommands.Count>StartCommandCount do begin
     fCommands.Delete(fCommands.Count-1);
    end;
    fStartPointSeen:=fCommands.Count>0;
    break;
   end;
   LastCommand:=Command;
   if SrcPos=StartSrcPos then begin
    // Nothing was consumed in this iteration at all, which happens only when the remaining input is
    // malformed, for example at a character which is neither a command nor a part of a number, so the
    // loop must stop here, because it would never terminate otherwise.
    break;
   end;
  end else begin
   break;
  end;
 end;
end;

constructor TpvVectorPath.CreateFromShape(const aShape:TpvVectorPathShape);
begin
 Create;
 Assign(aShape);
end;

destructor TpvVectorPath.Destroy;
begin
 FreeAndNil(fCommands);
 FreeAndNil(fShape);
 inherited Destroy;
end;

procedure TpvVectorPath.Assign(const aFrom:TpvVectorPath);
var Index:TpvSizeInt;
    SrcCmd:TpvVectorPathCommand;
begin
 fCommands.Clear;
 for Index:=0 to aFrom.fCommands.Count-1 do begin
  SrcCmd:=aFrom.fCommands[Index];
  fCommands.Add(TpvVectorPathCommand.Create(SrcCmd.fCommandType,SrcCmd.fX0,SrcCmd.fY0,SrcCmd.fX1,SrcCmd.fY1,SrcCmd.fX2,SrcCmd.fY2));
 end;
 fFillRule:=aFrom.fFillRule;
 fStartPointSeen:=Index>0;
end;

function TpvVectorPath.InterpolateFrom(const aFrom,aTo:TpvVectorPath;const aFactor:TpvDouble):boolean;
var Index:TpvSizeInt;
    FromCommand,ToCommand:TpvVectorPathCommand;
    Factor:TpvDouble;
begin

 if not (assigned(aFrom) and assigned(aTo)) then begin
  result:=false;
  exit;
 end;

 // The two have to agree on how many commands there are and on what each of them is, otherwise
 // there is nothing to pair up.
 result:=aFrom.fCommands.Count=aTo.fCommands.Count;
 if result then begin
  for Index:=0 to aFrom.fCommands.Count-1 do begin
   if aFrom.fCommands[Index].fCommandType<>aTo.fCommands[Index].fCommandType then begin
    result:=false;
    break;
   end;
  end;
 end;

 // Mismatched paths simply swap over at the halfway point.
 if not result then begin
  if aFactor<0.5 then begin
   Assign(aFrom);
  end else begin
   Assign(aTo);
  end;
  exit;
 end;

 Factor:=aFactor;
 if Factor<0.0 then begin
  Factor:=0.0;
 end else if Factor>1.0 then begin
  Factor:=1.0;
 end;

 // All six coordinates are blended whatever the command is, since the ones a command does not
 // use are zero on both sides anyway.
 fCommands.Clear;
 for Index:=0 to aFrom.fCommands.Count-1 do begin
  FromCommand:=aFrom.fCommands[Index];
  ToCommand:=aTo.fCommands[Index];
  fCommands.Add(TpvVectorPathCommand.Create(FromCommand.fCommandType,
                                            FromCommand.fX0+((ToCommand.fX0-FromCommand.fX0)*Factor),
                                            FromCommand.fY0+((ToCommand.fY0-FromCommand.fY0)*Factor),
                                            FromCommand.fX1+((ToCommand.fX1-FromCommand.fX1)*Factor),
                                            FromCommand.fY1+((ToCommand.fY1-FromCommand.fY1)*Factor),
                                            FromCommand.fX2+((ToCommand.fX2-FromCommand.fX2)*Factor),
                                            FromCommand.fY2+((ToCommand.fY2-FromCommand.fY2)*Factor)));
 end;

 fFillRule:=aFrom.fFillRule;
 fStartPointSeen:=fCommands.Count>0;

end;

procedure TpvVectorPath.Assign(const aShape:TpvVectorPathShape);
var Contour:TpvVectorPathContour;
    Segment:TpvVectorPathSegment;
    First:boolean;
    Last,Start:TpvVectorPathVector;
begin
 fCommands.Clear;
 fStartPointSeen:=false;
 if assigned(aShape) then begin
  fFillRule:=aShape.fFillRule;
  First:=true;
  Last:=TpvVectorPathVector.Create(0.0,0.0);
  Start:=TpvVectorPathVector.Create(0.0,0.0);
  for Contour in aShape.fContours do begin
   for Segment in Contour.fSegments do begin
    case Segment.Type_ of
     TpvVectorPathSegmentType.Line:begin
      if First then begin
       First:=false;
       Start:=TpvVectorPathSegmentLine(Segment).Points[0];
       Last:=Start;
       fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.MoveTo,
                                                 TpvVectorPathSegmentLine(Segment).Points[0].x,TpvVectorPathSegmentLine(Segment).Points[0].y));
      end else if Last<>TpvVectorPathSegmentLine(Segment).Points[0] then begin
       fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.MoveTo,
                                                 TpvVectorPathSegmentLine(Segment).Points[0].x,TpvVectorPathSegmentLine(Segment).Points[0].y));
      end;
      fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.LineTo,
                                                TpvVectorPathSegmentLine(Segment).Points[1].x,TpvVectorPathSegmentLine(Segment).Points[1].y));
      Last:=TpvVectorPathSegmentLine(Segment).Points[1];
     end;
     TpvVectorPathSegmentType.QuadraticCurve:begin
      if First then begin
       First:=false;
       Start:=TpvVectorPathSegmentQuadraticCurve(Segment).Points[0];
       Last:=Start;
       fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.MoveTo,
                                                 TpvVectorPathSegmentQuadraticCurve(Segment).Points[0].x,TpvVectorPathSegmentQuadraticCurve(Segment).Points[0].y));
      end else if Last<>TpvVectorPathSegmentQuadraticCurve(Segment).Points[0] then begin
       fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.MoveTo,
                                                 TpvVectorPathSegmentQuadraticCurve(Segment).Points[0].x,TpvVectorPathSegmentQuadraticCurve(Segment).Points[0].y));
      end;
      fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.QuadraticCurveTo,
                                                TpvVectorPathSegmentQuadraticCurve(Segment).Points[1].x,TpvVectorPathSegmentQuadraticCurve(Segment).Points[1].y,
                                                TpvVectorPathSegmentQuadraticCurve(Segment).Points[2].x,TpvVectorPathSegmentQuadraticCurve(Segment).Points[2].y));
      Last:=TpvVectorPathSegmentQuadraticCurve(Segment).Points[2];
     end;
     TpvVectorPathSegmentType.CubicCurve:begin
      if First then begin
       First:=false;
       Start:=TpvVectorPathSegmentCubicCurve(Segment).Points[0];
       Last:=Start;
       fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.MoveTo,
                                                 TpvVectorPathSegmentCubicCurve(Segment).Points[0].x,TpvVectorPathSegmentCubicCurve(Segment).Points[0].y));
      end else if Last<>TpvVectorPathSegmentCubicCurve(Segment).Points[0] then begin
       fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.MoveTo,
                                                 TpvVectorPathSegmentCubicCurve(Segment).Points[0].x,TpvVectorPathSegmentCubicCurve(Segment).Points[0].y));
      end;
      fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.CubicCurveTo,
                                                TpvVectorPathSegmentCubicCurve(Segment).Points[1].x,TpvVectorPathSegmentCubicCurve(Segment).Points[1].y,
                                                TpvVectorPathSegmentCubicCurve(Segment).Points[2].x,TpvVectorPathSegmentCubicCurve(Segment).Points[2].y,
                                                TpvVectorPathSegmentCubicCurve(Segment).Points[3].x,TpvVectorPathSegmentCubicCurve(Segment).Points[3].y));
      Last:=TpvVectorPathSegmentCubicCurve(Segment).Points[3];
     end;
     else begin
     end;
    end;
    fStartPointSeen:=true;
   end;
   if (not First) and Contour.fClosed then begin
    fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.Close));
   end;
  end;
 end;
end;

function TpvVectorPath.MoveTo(const aX,aY:TpvDouble):TpvVectorPath;
begin
 fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.MoveTo,aX,aY));
 fStartPointSeen:=true;
 result:=self;
end;

function TpvVectorPath.MoveTo(const aPosition:TpvVectorPathVector):TpvVectorPath;
begin
 fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.MoveTo,aPosition.x,aPosition.y));
 fStartPointSeen:=true;
 result:=self;
end;

function TpvVectorPath.LineTo(const aX,aY:TpvDouble):TpvVectorPath;
begin
 fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.LineTo,aX,aY));
 fStartPointSeen:=true;
 result:=self;
end;

function TpvVectorPath.LineTo(const aPosition:TpvVectorPathVector):TpvVectorPath;
begin
 fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.LineTo,aPosition.x,aPosition.y));
 fStartPointSeen:=true;
 result:=self;
end;

function TpvVectorPath.QuadraticCurveTo(const aCX,aCY,aAX,aAY:TpvDouble):TpvVectorPath;
begin
 fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.QuadraticCurveTo,aCX,aCY,aAX,aAY));
 fStartPointSeen:=true;
 result:=self;
end;

function TpvVectorPath.QuadraticCurveTo(const aControlPoint,aAnchorPoint:TpvVectorPathVector):TpvVectorPath;
begin
 fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.QuadraticCurveTo,aControlPoint.x,aControlPoint.y,aAnchorPoint.x,aAnchorPoint.y));
 fStartPointSeen:=true;
 result:=self;
end;

function TpvVectorPath.CubicCurveTo(const aC0X,aC0Y,aC1X,aC1Y,aAX,aAY:TpvDouble):TpvVectorPath;
begin
 fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.CubicCurveTo,aC0X,aC0Y,aC1X,aC1Y,aAX,aAY));
 fStartPointSeen:=true;
 result:=self;
end;

function TpvVectorPath.CubicCurveTo(const aControlPoint0,aControlPoint1,aAnchorPoint:TpvVectorPathVector):TpvVectorPath;
begin
 fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.CubicCurveTo,aControlPoint0.x,aControlPoint0.y,aControlPoint1.x,aControlPoint1.y,aAnchorPoint.x,aAnchorPoint.y));
 fStartPointSeen:=true;
 result:=self;
end;

function TpvVectorPath.ArcTo(const aOriginX,aOriginY,aRadiusX,aRadiusY:TpvDouble;const aStartAngle,aEndAngle:TpvDouble;const aCounterClockwise:boolean;const aRotation:TpvDouble):TpvVectorPath;
type TMatrix=array[0..5] of TpvFloat;
var SweepDirection:TpvInt32;    
    ArcSweepLeft,StartAngle,CurrentStartAngle,CurrentEndAngle:TpvFloat;
    CurrentStartOffset,CurrentEndOffset,cp0,cp1,RotationSinCos:TpvVectorPathVector;
    KappaFactor:TpvFloat;
    Matrix:TMatrix;
begin
 
 // Calculate the sweep direction
 if aCounterClockwise then begin
  SweepDirection:=-1;
 end else begin
  SweepDirection:=1;
 end;

 // Calculate the total arc we're going to sweep
 ArcSweepLeft:=(aEndAngle-aStartAngle)*SweepDirection;

 // Ensure the sweep is positive, and normalize it
 if ArcSweepLeft<0.0 then begin
  ArcSweepLeft:=TwoPI+(ArcSweepLeft-(Floor(ArcSweepLeft/TwoPI)*TwoPI));
  StartAngle:=aEndAngle-(ArcSweepLeft*SweepDirection);
 end else if ArcSweepLeft>TwoPI then begin
  ArcSweepLeft:=TwoPI;
  StartAngle:=aStartAngle;
 end else begin
  StartAngle:=aStartAngle;
 end;

 // Create transformation matrix from scratch at once
 SinCos(aRotation,RotationSinCos.x,RotationSinCos.y);

 Matrix[0]:=aRadiusX*RotationSinCos.y;
 Matrix[1]:=aRadiusY*RotationSinCos.x;
 Matrix[2]:=-aRadiusX*RotationSinCos.x;
 Matrix[3]:=aRadiusY*RotationSinCos.y;
 Matrix[4]:=aOriginX;
 Matrix[5]:=aOriginY;

 // Current start angle and offset (unit circle)
 CurrentStartAngle:=StartAngle;
 SinCos(StartAngle,CurrentStartOffset.y,CurrentStartOffset.x);

 // Move to the start point (transformed) 
 if fStartPointSeen then begin
  LineTo((CurrentStartOffset.x*Matrix[0])+(CurrentStartOffset.y*Matrix[2])+Matrix[4],
         (CurrentStartOffset.x*Matrix[1])+(CurrentStartOffset.y*Matrix[3])+Matrix[5]);
 end else begin
  MoveTo((CurrentStartOffset.x*Matrix[0])+(CurrentStartOffset.y*Matrix[2])+Matrix[4],
         (CurrentStartOffset.x*Matrix[1])+(CurrentStartOffset.y*Matrix[3])+Matrix[5]);
 end;

 while ArcSweepLeft>0.0 do begin

  // Calculate the end angle and offset (unit circle)
  CurrentEndAngle:=CurrentStartAngle+(Min(ArcSweepLeft,HalfPI)*SweepDirection);
  SinCos(CurrentEndAngle,CurrentEndOffset.y,CurrentEndOffset.x);

  // Calculate the kappa factor
  KappaFactor:=(4.0/3.0)*tan((CurrentEndAngle-CurrentStartAngle)*0.25);

  // Calculate the control points
  cp0:=TpvVectorPathVector.Create(CurrentStartOffset.x-(CurrentStartOffset.y*KappaFactor),
                                  CurrentStartOffset.y+(CurrentStartOffset.x*KappaFactor));
  cp1:=TpvVectorPathVector.Create(CurrentEndOffset.x+(CurrentEndOffset.y*KappaFactor),
                                  CurrentEndOffset.y-(CurrentEndOffset.x*KappaFactor));

  // Draw the current arc segment as a Bezier curve (using baked coordinates)
  CubicCurveTo((cp0.x*Matrix[0])+(cp0.y*Matrix[2])+Matrix[4],
               (cp0.x*Matrix[1])+(cp0.y*Matrix[3])+Matrix[5],
               (cp1.x*Matrix[0])+(cp1.y*Matrix[2])+Matrix[4],
               (cp1.x*Matrix[1])+(cp1.y*Matrix[3])+Matrix[5],
               (CurrentEndOffset.x*Matrix[0])+(CurrentEndOffset.y*Matrix[2])+Matrix[4],
               (CurrentEndOffset.x*Matrix[1])+(CurrentEndOffset.y*Matrix[3])+Matrix[5]);

  // Move to the next segment 
  ArcSweepLeft:=ArcSweepLeft-HalfPI;
  CurrentStartAngle:=CurrentEndAngle;
  CurrentStartOffset:=CurrentEndOffset;

 end;

 result:=self;

end;

function TpvVectorPath.ArcTo(const aOrigin,aRadius:TpvVectorPathVector;const aStartAngle,aEndAngle:TpvDouble;const aCounterClockwise:boolean;const aRotation:TpvDouble):TpvVectorPath;
type TMatrix=array[0..5] of TpvFloat;
var SweepDirection:TpvInt32;    
    ArcSweepLeft,StartAngle,CurrentStartAngle,CurrentEndAngle:TpvFloat;
    CurrentStartOffset,CurrentEndOffset,cp0,cp1,RotationSinCos:TpvVectorPathVector;
    KappaFactor:TpvFloat;
    Matrix:TMatrix;
begin
 
 // Calculate the sweep direction
 if aCounterClockwise then begin
  SweepDirection:=-1;
 end else begin
  SweepDirection:=1;
 end;

 // Calculate the total arc we're going to sweep
 ArcSweepLeft:=(aEndAngle-aStartAngle)*SweepDirection;

 // Ensure the sweep is positive, and normalize it
 if ArcSweepLeft<0.0 then begin
  ArcSweepLeft:=TwoPI+(ArcSweepLeft-(Floor(ArcSweepLeft/TwoPI)*TwoPI));
  StartAngle:=aEndAngle-(ArcSweepLeft*SweepDirection);
 end else if ArcSweepLeft>TwoPI then begin
  ArcSweepLeft:=TwoPI;
  StartAngle:=aStartAngle;
 end else begin
  StartAngle:=aStartAngle;
 end;

 // Create transformation matrix from scratch at once
 SinCos(aRotation,RotationSinCos.x,RotationSinCos.y);

 Matrix[0]:=aRadius.x*RotationSinCos.y;
 Matrix[1]:=aRadius.y*RotationSinCos.x;
 Matrix[2]:=-aRadius.x*RotationSinCos.x;
 Matrix[3]:=aRadius.y*RotationSinCos.y;
 Matrix[4]:=aOrigin.x;
 Matrix[5]:=aOrigin.y;

 // Current start angle and offset (unit circle)
 CurrentStartAngle:=StartAngle;
 SinCos(StartAngle,CurrentStartOffset.y,CurrentStartOffset.x);

 // Move to the start point (transformed) 
 if fStartPointSeen then begin
  LineTo((CurrentStartOffset.x*Matrix[0])+(CurrentStartOffset.y*Matrix[2])+Matrix[4],
         (CurrentStartOffset.x*Matrix[1])+(CurrentStartOffset.y*Matrix[3])+Matrix[5]);
 end else begin
  MoveTo((CurrentStartOffset.x*Matrix[0])+(CurrentStartOffset.y*Matrix[2])+Matrix[4],
         (CurrentStartOffset.x*Matrix[1])+(CurrentStartOffset.y*Matrix[3])+Matrix[5]);
 end;

 while ArcSweepLeft>0.0 do begin

  // Calculate the end angle and offset (unit circle)
  CurrentEndAngle:=CurrentStartAngle+(Min(ArcSweepLeft,HalfPI)*SweepDirection);
  SinCos(CurrentEndAngle,CurrentEndOffset.y,CurrentEndOffset.x);

  // Calculate the kappa factor
  KappaFactor:=(4.0/3.0)*tan((CurrentEndAngle-CurrentStartAngle)*0.25);

  // Calculate the control points
  cp0:=TpvVectorPathVector.Create(CurrentStartOffset.x-(CurrentStartOffset.y*KappaFactor),
                                  CurrentStartOffset.y+(CurrentStartOffset.x*KappaFactor));
  cp1:=TpvVectorPathVector.Create(CurrentEndOffset.x+(CurrentEndOffset.y*KappaFactor),
                                  CurrentEndOffset.y-(CurrentEndOffset.x*KappaFactor));

  // Draw the current arc segment as a Bezier curve (using baked coordinates)
  CubicCurveTo((cp0.x*Matrix[0])+(cp0.y*Matrix[2])+Matrix[4],
               (cp0.x*Matrix[1])+(cp0.y*Matrix[3])+Matrix[5],
               (cp1.x*Matrix[0])+(cp1.y*Matrix[2])+Matrix[4],
               (cp1.x*Matrix[1])+(cp1.y*Matrix[3])+Matrix[5],
               (CurrentEndOffset.x*Matrix[0])+(CurrentEndOffset.y*Matrix[2])+Matrix[4],
               (CurrentEndOffset.x*Matrix[1])+(CurrentEndOffset.y*Matrix[3])+Matrix[5]);

  // Move to the next segment 
  ArcSweepLeft:=ArcSweepLeft-HalfPI;
  CurrentStartAngle:=CurrentEndAngle;
  CurrentStartOffset:=CurrentEndOffset;

 end;

 result:=self;

end;

function TpvVectorPath.Arc(const aCenterX,aCenterY,aRadius,aStartAngle,aEndAngle:TpvDouble;const aCounterClockwise:boolean):TpvVectorPath;
begin
 result:=ArcTo(aCenterX,aCenterY,aRadius,aRadius,aStartAngle,aEndAngle,aCounterClockwise,0.0);
end;

function TpvVectorPath.Arc(const aCenter:TpvVectorPathVector;const aRadius,aStartAngle,aEndAngle:TpvDouble;const aCounterClockwise:boolean):TpvVectorPath;
begin
 result:=ArcTo(aCenter.X,aCenter.Y,aRadius,aRadius,aStartAngle,aEndAngle,aCounterClockwise,0.0);
end;

function TpvVectorPath.Ellipse(const aCenterX,aCenterY,aRadiusX,aRadiusY:TpvDouble):TpvVectorPath;
const ARC_MAGIC=0.5522847498; // 4/3 * (1-cos 45°)/sin 45° = 4/3 * (sqrt(2) - 1)
begin
 MoveTo(aCenterX+aRadiusX,aCenterY);
 CubicCurveTo(aCenterX+aRadiusX,aCenterY-(aRadiusY*ARC_MAGIC),
              aCenterX+(aRadiusX*ARC_MAGIC),aCenterY-aRadiusY,
              aCenterX,aCenterY-aRadiusY);
 CubicCurveTo(aCenterX-(aRadiusX*ARC_MAGIC),aCenterY-aRadiusY,
              aCenterX-aRadiusX,aCenterY-(aRadiusY*ARC_MAGIC),
              aCenterX-aRadiusX,aCenterY);
 CubicCurveTo(aCenterX-aRadiusX,aCenterY+(aRadiusY*ARC_MAGIC),
              aCenterX-(aRadiusX*ARC_MAGIC),aCenterY+aRadiusY,
              aCenterX,aCenterY+aRadiusY);
 CubicCurveTo(aCenterX+(aRadiusX*ARC_MAGIC),aCenterY+aRadiusY,
              aCenterX+aRadiusX,aCenterY+(aRadiusY*ARC_MAGIC),
              aCenterX+aRadiusX,aCenterY);
 Close;
 result:=self;
end;

function TpvVectorPath.Ellipse(const aCenter,aRadius:TpvVectorPathVector):TpvVectorPath;
const ARC_MAGIC=0.5522847498; // 4/3 * (1-cos 45°)/sin 45° = 4/3 * (sqrt(2) - 1)
begin
 MoveTo(aCenter.x+aRadius.x,aCenter.y);
 CubicCurveTo(aCenter.x+aRadius.x,aCenter.y-(aRadius.y*ARC_MAGIC),
              aCenter.x+(aRadius.x*ARC_MAGIC),aCenter.y-aRadius.y,
              aCenter.x,aCenter.y-aRadius.y);
 CubicCurveTo(aCenter.x-(aRadius.x*ARC_MAGIC),aCenter.y-aRadius.y,
              aCenter.x-aRadius.x,aCenter.y-(aRadius.y*ARC_MAGIC),
              aCenter.x-aRadius.x,aCenter.y);
 CubicCurveTo(aCenter.x-aRadius.x,aCenter.y+(aRadius.y*ARC_MAGIC),
              aCenter.x-(aRadius.x*ARC_MAGIC),aCenter.y+aRadius.y,
              aCenter.x,aCenter.y+aRadius.y);
 CubicCurveTo(aCenter.x+(aRadius.x*ARC_MAGIC),aCenter.y+aRadius.y,
              aCenter.x+aRadius.x,aCenter.y+(aRadius.y*ARC_MAGIC),
              aCenter.x+aRadius.x,aCenter.y);
 Close;
 result:=self;
end;

function TpvVectorPath.Circle(const aCenterX,aCenterY,aRadius:TpvDouble):TpvVectorPath;
begin
 result:=Ellipse(aCenterX,aCenterY,aRadius,aRadius);
end;

function TpvVectorPath.Circle(const aCenter:TpvVectorPathVector;const aRadius:TpvDouble):TpvVectorPath;
begin
 result:=Ellipse(aCenter.x,aCenter.y,aRadius,aRadius);
end;

function TpvVectorPath.Rectangle(const aCenterX,aCenterY,aBoundsX,aBoundsY:TpvDouble):TpvVectorPath;
begin
 MoveTo(aCenterX-aBoundsX,aCenterY-aBoundsY);
 LineTo(aCenterX+aBoundsX,aCenterY-aBoundsY);
 LineTo(aCenterX+aBoundsX,aCenterY+aBoundsY);
 LineTo(aCenterX-aBoundsX,aCenterY+aBoundsY);
 Close;
 result:=self;
end;

function TpvVectorPath.Rectangle(const aCenter,aBounds:TpvVectorPathVector):TpvVectorPath;
begin
 MoveTo(aCenter.x-aBounds.x,aCenter.y-aBounds.y);
 LineTo(aCenter.x+aBounds.x,aCenter.y-aBounds.y);
 LineTo(aCenter.x+aBounds.x,aCenter.y+aBounds.y);
 LineTo(aCenter.x-aBounds.x,aCenter.y+aBounds.y);
 Close;
 result:=self;
end;

function TpvVectorPath.RoundedRectangle(const aCenterX,aCenterY,aBoundsX,aBoundsY,aRadiusTopLeft,aRadiusTopRight,aRadiusBottomLeft,aRadiusBottomRight:TpvDouble):TpvVectorPath;
const ARC_MAGIC=0.5522847498; // 4/3 * (1-cos 45°)/sin 45° = 4/3 * (sqrt(2) - 1)
var OffsetX,OffsetY,SizeX,SizeY,TopLeftX,TopLeftY,TopRightX,TopRightY,BottomLeftX,BottomLeftY,BottomRightX,BottomRightY:TpvDouble;
begin
 if IsZero(aRadiusTopLeft) and
    IsZero(aRadiusTopRight) and
    IsZero(aRadiusBottomLeft) and
    IsZero(aRadiusBottomRight) then begin
  Rectangle(aCenterX,aCenterY,aBoundsX,aBoundsY);
 end else begin
  OffsetX:=aCenterX-aBoundsX;
  OffsetY:=aCenterY-aBoundsY;
  SizeX:=aBoundsX*2.0;
  SizeY:=aBoundsY*2.0;
  TopLeftX:=Min(aBoundsX,aRadiusTopLeft)*Sign(SizeX);
  TopLeftY:=Min(aBoundsY,aRadiusTopLeft)*Sign(SizeY);
  TopRightX:=Min(aBoundsX,aRadiusTopRight)*Sign(SizeX);
  TopRightY:=Min(aBoundsY,aRadiusTopRight)*Sign(SizeY);
  BottomLeftX:=Min(aBoundsX,aRadiusBottomLeft)*Sign(SizeX);
  BottomLeftY:=Min(aBoundsY,aRadiusBottomLeft)*Sign(SizeY);
  BottomRightX:=Min(aBoundsX,aRadiusBottomRight)*Sign(SizeX);
  BottomRightY:=Min(aBoundsY,aRadiusBottomRight)*Sign(SizeY);
  MoveTo(OffsetX,OffsetY+TopLeftY);
  LineTo(OffsetX,OffsetY+SizeY-BottomLeftY);
  CubicCurveTo(OffsetX,OffsetY+SizeY-(BottomLeftY*(1.0-ARC_MAGIC)),
               OffsetX+BottomLeftX*(1.0-ARC_MAGIC),OffsetY+SizeY,
               OffsetX+BottomLeftX,OffsetY+SizeY);
  LineTo(OffsetX+SizeX-BottomRightX,OffsetY+SizeY);
  CubicCurveTo(OffsetX+SizeX-(BottomRightX*(1.0-ARC_MAGIC)),OffsetY+SizeY,
               OffsetX+SizeX,OffsetY+SizeY-(BottomRightY*(1.0-ARC_MAGIC)),
               OffsetX+SizeX,OffsetY+SizeY-BottomRightY);
  LineTo(OffsetX+SizeX,OffsetY+TopRightY);
  CubicCurveTo(OffsetX+SizeX,OffsetY+TopRightY*(1.0-ARC_MAGIC),
               OffsetX+SizeX-(TopRightX*(1.0-ARC_MAGIC)),OffsetY,
               OffsetX+SizeX-TopRightX,OffsetY);
  LineTo(OffsetX+TopLeftX,OffsetY);
  CubicCurveTo(OffsetX+TopLeftX*(1.0-ARC_MAGIC),OffsetY,
               OffsetX,OffsetY+TopLeftY*(1.0-ARC_MAGIC),
               OffsetX,OffsetY+TopLeftY);
  Close;
 end;
 result:=self;
end;

function TpvVectorPath.RoundedRectangle(const aCenter,aBounds:TpvVectorPathVector;const aRadiusTopLeft,aRadiusTopRight,aRadiusBottomLeft,aRadiusBottomRight:TpvDouble):TpvVectorPath;
const ARC_MAGIC=0.5522847498; // 4/3 * (1-cos 45°)/sin 45° = 4/3 * (sqrt(2) - 1)
var Offset,Size,TopLeft,TopRight,BottomLeft,BottomRight:TpvVectorPathVector;
begin
 if IsZero(aRadiusTopLeft) and
    IsZero(aRadiusTopRight) and
    IsZero(aRadiusBottomLeft) and
    IsZero(aRadiusBottomRight) then begin
  Rectangle(aCenter,aBounds);
 end else begin
  Offset:=aCenter-aBounds;
  Size:=aBounds*2.0;
  TopLeft:=TpvVectorPathVector.Create(Min(aBounds.x,aRadiusTopLeft)*Sign(Size.x),
                                      Min(aBounds.y,aRadiusTopLeft)*Sign(Size.y));
  TopRight:=TpvVectorPathVector.Create(Min(aBounds.x,aRadiusTopRight)*Sign(Size.x),
                                       Min(aBounds.y,aRadiusTopRight)*Sign(Size.y));
  BottomLeft:=TpvVectorPathVector.Create(Min(aBounds.x,aRadiusBottomLeft)*Sign(Size.x),
                                         Min(aBounds.y,aRadiusBottomLeft)*Sign(Size.y));
  BottomRight:=TpvVectorPathVector.Create(Min(aBounds.x,aRadiusBottomRight)*Sign(Size.x),
                                          Min(aBounds.y,aRadiusBottomRight)*Sign(Size.y));
  MoveTo(Offset.x,Offset.y+TopLeft.y);
  LineTo(Offset.x,Offset.y+Size.y-BottomLeft.y);
  CubicCurveTo(Offset.x,Offset.y+Size.y-(BottomLeft.y*(1.0-ARC_MAGIC)),
               Offset.x+BottomLeft.x*(1.0-ARC_MAGIC),Offset.y+Size.y,
               Offset.x+BottomLeft.x,Offset.y+Size.y);
  LineTo(Offset.x+Size.x-BottomRight.x,Offset.y+Size.y);
  CubicCurveTo(Offset.x+Size.x-(BottomRight.x*(1.0-ARC_MAGIC)),Offset.y+Size.y,
               Offset.x+Size.x,Offset.y+Size.y-(BottomRight.y*(1.0-ARC_MAGIC)),
               Offset.x+Size.x,Offset.y+Size.y-BottomRight.y);
  LineTo(Offset.x+Size.x,Offset.y+TopRight.y);
  CubicCurveTo(Offset.x+Size.x,Offset.y+TopRight.y*(1.0-ARC_MAGIC),
               Offset.x+Size.x-(TopRight.x*(1.0-ARC_MAGIC)),Offset.y,
               Offset.x+Size.x-TopRight.x,Offset.y);
  LineTo(Offset.x+TopLeft.x,Offset.y);
  CubicCurveTo(Offset.x+TopLeft.x*(1.0-ARC_MAGIC),Offset.y,
               Offset.x,Offset.y+TopLeft.y*(1.0-ARC_MAGIC),
               Offset.x,Offset.y+TopLeft.y);
  Close;
 end;
 result:=self;
end;

function TpvVectorPath.RoundedRectangle(const aCenterX,aCenterY,aBoundsX,aBoundsY,aRadius:TpvDouble):TpvVectorPath;
begin
 result:=RoundedRectangle(aCenterX,aCenterY,aBoundsX,aBoundsY,aRadius,aRadius,aRadius,aRadius);
end;

function TpvVectorPath.RoundedRectangle(const aCenter,aBounds:TpvVectorPathVector;const aRadius:TpvDouble):TpvVectorPath;
begin
 result:=RoundedRectangle(aCenter,aBounds,aRadius,aRadius,aRadius,aRadius);
end;

function TpvVectorPath.Close:TpvVectorPath;
begin
 fCommands.Add(TpvVectorPathCommand.Create(TpvVectorPathCommandType.Close));
 result:=self;
end;

function TpvVectorPath.GetShape:TpvVectorPathShape;
begin
 if assigned(fShape) then begin
  fShape.Assign(self);
 end else begin
  fShape:=TpvVectorPathShape.Create(self);
 end;
 result:=fShape;
end;

procedure TpvVectorPath.Transform(const aMatrix:TpvVectorPathMatrix);
var Command:TpvVectorPathCommand;
    Position:TpvVectorPathVector;
begin
 for Command in fCommands do begin
  if assigned(Command) then begin
   case Command.fCommandType of
    TpvVectorPathCommandType.MoveTo,
    TpvVectorPathCommandType.LineTo:begin
     Position:=aMatrix.Transform(Command.fX0,Command.fY0);
     Command.fX0:=Position.x;
     Command.fY0:=Position.y;
    end;
    TpvVectorPathCommandType.QuadraticCurveTo:begin
     Position:=aMatrix.Transform(Command.fX0,Command.fY0);
     Command.fX0:=Position.x;
     Command.fY0:=Position.y;
     Position:=aMatrix.Transform(Command.fX1,Command.fY1);
     Command.fX1:=Position.x;
     Command.fY1:=Position.y;
    end;
    TpvVectorPathCommandType.CubicCurveTo:begin
     Position:=aMatrix.Transform(Command.fX0,Command.fY0);
     Command.fX0:=Position.x;
     Command.fY0:=Position.y;
     Position:=aMatrix.Transform(Command.fX1,Command.fY1);
     Command.fX1:=Position.x;
     Command.fY1:=Position.y;
     Position:=aMatrix.Transform(Command.fX2,Command.fY2);
     Command.fX2:=Position.x;
     Command.fY2:=Position.y;
    end;
    else {TpvVectorPathCommandType.Close:}begin
    end;
   end;
  end;
 end;
 if assigned(fShape) then begin
  fShape.Transform(aMatrix);
 end;
end;

{ TpvVectorPathStrokeSettings }

class function TpvVectorPathStrokeSettings.CreateDefault:TpvVectorPathStrokeSettings;
begin
 result.Width:=1.0;
 result.CapMode:=TpvVectorPathStrokeCapMode.Butt;
 result.JoinMode:=TpvVectorPathStrokeJoinMode.Miter;
 result.InnerJoinMode:=TpvVectorPathStrokeInnerJoinMode.Miter;
 result.MiterLimit:=4.0;
 result.InnerMiterLimit:=1.01;
 result.DashArray:=nil;
 result.DashOffset:=0.0;
end;

procedure TpvVectorPathStrokeSettings.Assign(const aFrom:TpvVectorPathStrokeSettings);
begin
 Width:=aFrom.Width;
 CapMode:=aFrom.CapMode;
 JoinMode:=aFrom.JoinMode;
 InnerJoinMode:=aFrom.InnerJoinMode;
 MiterLimit:=aFrom.MiterLimit;
 InnerMiterLimit:=aFrom.InnerMiterLimit;
 DashArray:=copy(aFrom.DashArray);
 DashOffset:=aFrom.DashOffset;
end;

function TpvVectorPathStrokeSettings.HasDashes:boolean;
var Index:TpvSizeInt;
begin
 result:=false;
 for Index:=0 to length(DashArray)-1 do begin
  if DashArray[Index]>1e-12 then begin
   result:=true;
   break;
  end;
 end;
end;

function pvVectorPathStrokePatternToDashArray(const aPattern:TpvRawByteString;
                                              const aStepSize:TpvDouble):TpvVectorPathStrokeDashArray;
var Index,RunStart,Count,OldCount:TpvSizeInt;
    Character:AnsiChar;
    IsDash,StartsWithGap:boolean;
begin
 result:=nil;
 Count:=0;
 if Length(aPattern)=0 then begin
  exit;
 end;
 // A dash array always starts with a dash, so a pattern that opens with a gap gets a zero
 // length dash in front of it.
 StartsWithGap:=aPattern[1]=' ';
 if StartsWithGap then begin
  SetLength(result,1);
  result[0]:=0.0;
  Count:=1;
 end;
 Index:=1;
 while Index<=Length(aPattern) do begin
  Character:=aPattern[Index];
  IsDash:=Character<>' ';
  RunStart:=Index;
  while (Index<=Length(aPattern)) and
        ((aPattern[Index]=Character) or (IsDash and (aPattern[Index]<>' '))) do begin
   inc(Index);
  end;
  if Count>=Length(result) then begin
   SetLength(result,(Count+1)*2);
  end;
  result[Count]:=(Index-RunStart)*aStepSize;
  inc(Count);
 end;
 SetLength(result,Count);
 if (Count>0) and ((Count and 1)=1) then begin
  // An odd number of entries repeats with dashes and gaps swapped, so doubling it keeps the
  // stroker from having to track that parity.
  OldCount:=Count;
  SetLength(result,OldCount*2);
  for Index:=0 to OldCount-1 do begin
   result[OldCount+Index]:=result[Index];
  end;
 end;
end;

{ TpvVectorPathStroker }

// Intersection of the infinite lines through a-b and c-d, false when they are parallel within
// the epsilon.
function TpvVectorPathStrokerCalculateIntersection(const aAX,aAY,aBX,aBY,aCX,aCY,aDX,aDY:TpvDouble;out aX,aY:TpvDouble):boolean;
var Numerator,Denominator,Ratio:TpvDouble;
begin
 Numerator:=((aAY-aCY)*(aDX-aCX))-((aAX-aCX)*(aDY-aCY));
 Denominator:=((aBX-aAX)*(aDY-aCY))-((aBY-aAY)*(aDX-aCX));
 if abs(Denominator)<1e-14 then begin
  aX:=0.0;
  aY:=0.0;
  result:=false;
 end else begin
  Ratio:=Numerator/Denominator;
  aX:=aAX+(Ratio*(aBX-aAX));
  aY:=aAY+(Ratio*(aBY-aAY));
  result:=true;
 end;
end;

// Which side of the line through 1-2 the point lies on. Only the sign is used.
function TpvVectorPathStrokerCrossProduct(const aX1,aY1,aX2,aY2,aX,aY:TpvDouble):TpvDouble;
begin
 result:=((aX-aX2)*(aY2-aY1))-((aY-aY2)*(aX2-aX1));
end;

// Which way the polyline turns at point 2. Positive and negative tell the two sides of the turn
// apart, which is what picks the outer join over the inner one.
function TpvVectorPathStrokerCalculatePointLocation(const aX1,aY1,aX2,aY2,aX3,aY3:TpvDouble):TpvDouble;
begin
 result:=((aX3-aX2)*(aY2-aY1))-((aY3-aY2)*(aX2-aX1));
end;

constructor TpvVectorPathStroker.Create;
begin
 inherited Create;
 fSettings:=TpvVectorPathStrokeSettings.CreateDefault;
 fApproximationScale:=1.0;
 fLinePoints:=nil;
 fCountLinePoints:=0;
 fDashedLinePoints:=nil;
 fCountDashedLinePoints:=0;
 fDashes:=nil;
 fDashFlags:=nil;
 fOutputShape:=nil;
 fOutputContour:=nil;
 fHasLastPosition:=false;
 fFirst:=true;
 fWidth:=0.5;
 fWidthAbsolute:=0.5;
 fWidthEpsilon:=0.5/1024.0;
 fWidthSign:=1;
end;

destructor TpvVectorPathStroker.Destroy;
begin
 fLinePoints:=nil;
 fDashedLinePoints:=nil;
 fDashes:=nil;
 fDashFlags:=nil;
 inherited Destroy;
end;

procedure TpvVectorPathStroker.AddLinePoint(const aPosition:TpvVectorPathVector);
begin
 if fCountLinePoints>=length(fLinePoints) then begin
  SetLength(fLinePoints,(fCountLinePoints+1)*2);
 end;
 fLinePoints[fCountLinePoints].Position:=aPosition;
 fLinePoints[fCountLinePoints].Distance:=0.0;
 inc(fCountLinePoints);
end;

procedure TpvVectorPathStroker.AddDashedLinePoint(const aPosition:TpvVectorPathVector);
begin
 if fCountDashedLinePoints>=length(fDashedLinePoints) then begin
  SetLength(fDashedLinePoints,(fCountDashedLinePoints+1)*2);
 end;
 fDashedLinePoints[fCountDashedLinePoints].Position:=aPosition;
 fDashedLinePoints[fCountDashedLinePoints].Distance:=0.0;
 inc(fCountDashedLinePoints);
end;

procedure TpvVectorPathStroker.Point(const aX,aY:TpvDouble);
var Position:TpvVectorPathVector;
begin
 Position:=TpvVectorPathVector.Create(aX,aY);
 if fFirst then begin
  fFirstPosition:=Position;
  fFirst:=false;
  fOutputContour:=TpvVectorPathContour.Create;
  fOutputContour.fClosed:=true;
  fOutputShape.fContours.Add(fOutputContour);
 end else begin
  fOutputContour.fSegments.Add(TpvVectorPathSegmentLine.Create(fLastPosition,Position));
 end;
 fLastPosition:=Position;
 fHasLastPosition:=true;
end;

procedure TpvVectorPathStroker.CloseOutputContour;
begin
 if (not fFirst) and assigned(fOutputContour) then begin
  if (not SameValue(fLastPosition.x,fFirstPosition.x)) or
     (not SameValue(fLastPosition.y,fFirstPosition.y)) then begin
   fOutputContour.fSegments.Add(TpvVectorPathSegmentLine.Create(fLastPosition,fFirstPosition));
   fLastPosition:=fFirstPosition;
  end;
 end;
end;

procedure TpvVectorPathStroker.DoArc(const aX,aY,aDX1,aDY1,aDX2,aDY2:TpvDouble);
var Angle,AngleFrom,AngleTo,AngleDelta:TpvDouble;
    Index,Count:TpvSizeInt;
begin
 AngleFrom:=ArcTan2(aDY1*fWidthSign,aDX1*fWidthSign);
 AngleTo:=ArcTan2(aDY2*fWidthSign,aDX2*fWidthSign);
 AngleDelta:=ArcCos(fWidthAbsolute/(fWidthAbsolute+(0.125/fApproximationScale)))*2.0;
 Point(aX+aDX1,aY+aDY1);
 if fWidthSign>0 then begin
  if AngleFrom>AngleTo then begin
   AngleTo:=AngleTo+(2.0*PI);
  end;
  Count:=trunc((AngleTo-AngleFrom)/AngleDelta);
  AngleDelta:=(AngleTo-AngleFrom)/(Count+1);
  Angle:=AngleFrom+AngleDelta;
  for Index:=0 to Count-1 do begin
   Point(aX+(cos(Angle)*fWidth),aY+(sin(Angle)*fWidth));
   Angle:=Angle+AngleDelta;
  end;
 end else begin
  if AngleFrom<AngleTo then begin
   AngleTo:=AngleTo-(2.0*PI);
  end;
  Count:=trunc((AngleFrom-AngleTo)/AngleDelta);
  AngleDelta:=(AngleFrom-AngleTo)/(Count+1);
  Angle:=AngleFrom-AngleDelta;
  for Index:=0 to Count-1 do begin
   Point(aX+(cos(Angle)*fWidth),aY+(sin(Angle)*fWidth));
   Angle:=Angle-AngleDelta;
  end;
 end;
 Point(aX+aDX2,aY+aDY2);
end;

procedure TpvVectorPathStroker.DoMiter(const aP1X,aP1Y,aP2X,aP2Y,aP3X,aP3Y,aDX1,aDY1,aDX2,aDY2:TpvDouble;
                                       const aJoinMode:TpvVectorPathStrokeJoinMode;
                                       const aMiterLimit,aBevel:TpvDouble);
var IntersectionX,IntersectionY,Distance,Limit,MiterLimit,X1,Y1,X2,Y2:TpvDouble;
    MiterLimitExceeded,IntersectionFailed:boolean;
begin
 IntersectionX:=aP2X;
 IntersectionY:=aP2Y;
 Distance:=1.0;
 Limit:=fWidthAbsolute*aMiterLimit;
 MiterLimitExceeded:=true;
 IntersectionFailed:=true;
 if TpvVectorPathStrokerCalculateIntersection(aP1X+aDX1,aP1Y-aDY1,aP2X+aDX1,aP2Y-aDY1,
                                              aP2X+aDX2,aP2Y-aDY2,aP3X+aDX2,aP3Y-aDY2,
                                              IntersectionX,IntersectionY) then begin
  Distance:=sqrt(sqr(aP2X-IntersectionX)+sqr(aP2Y-IntersectionY));
  if Distance<=Limit then begin
   Point(IntersectionX,IntersectionY);
   MiterLimitExceeded:=false;
  end;
  IntersectionFailed:=false;
 end else begin
  X2:=aP2X+aDX1;
  Y2:=aP2Y-aDY1;
  if (TpvVectorPathStrokerCrossProduct(aP1X,aP1Y,aP2X,aP2Y,X2,Y2)<0.0)=
     (TpvVectorPathStrokerCrossProduct(aP2X,aP2Y,aP3X,aP3Y,X2,Y2)<0.0) then begin
   Point(aP2X+aDX1,aP2Y-aDY1);
   MiterLimitExceeded:=false;
  end;
 end;
 if MiterLimitExceeded then begin
  case aJoinMode of
   TpvVectorPathStrokeJoinMode.MiterRevert:begin
    Point(aP2X+aDX1,aP2Y-aDY1);
    Point(aP2X+aDX2,aP2Y-aDY2);
   end;
   TpvVectorPathStrokeJoinMode.MiterRound:begin
    DoArc(aP2X,aP2Y,aDX1,-aDY1,aDX2,-aDY2);
   end;
   else begin
    if IntersectionFailed then begin
     MiterLimit:=aMiterLimit*fWidthSign;
     Point(aP2X+aDX1+(aDY1*MiterLimit),aP2Y-aDY1+(aDX1*MiterLimit));
     Point(aP2X+aDX2-(aDY2*MiterLimit),aP2Y-aDY2-(aDX2*MiterLimit));
    end else begin
     X1:=aP2X+aDX1;
     Y1:=aP2Y-aDY1;
     X2:=aP2X+aDX2;
     Y2:=aP2Y-aDY2;
     Distance:=(Limit-aBevel)/(Distance-aBevel);
     Point(X1+((IntersectionX-X1)*Distance),Y1+((IntersectionY-Y1)*Distance));
     Point(X2+((IntersectionX-X2)*Distance),Y2+((IntersectionY-Y2)*Distance));
    end;
   end;
  end;
 end;
end;

procedure TpvVectorPathStroker.Join(const aIndex1,aIndex2,aIndex3,aDistanceIndex1,aDistanceIndex2:TpvSizeInt;
                                    const aLinePoints:TLinePoints);
var X1,Y1,X2,Y2,X3,Y3,DX1,DY1,DX2,DY2,Distance1,Distance2,Distance,
    InnerLimit,CrossProduct,DX,DY,Bevel:TpvDouble;
begin
 X1:=aLinePoints[aIndex1].Position.x;
 Y1:=aLinePoints[aIndex1].Position.y;
 X2:=aLinePoints[aIndex2].Position.x;
 Y2:=aLinePoints[aIndex2].Position.y;
 X3:=aLinePoints[aIndex3].Position.x;
 Y3:=aLinePoints[aIndex3].Position.y;
 Distance1:=aLinePoints[aDistanceIndex1].Distance;
 Distance2:=aLinePoints[aDistanceIndex2].Distance;
 DX1:=(fWidth*(Y2-Y1))/Distance1;
 DY1:=(fWidth*(X2-X1))/Distance1;
 DX2:=(fWidth*(Y3-Y2))/Distance2;
 DY2:=(fWidth*(X3-X2))/Distance2;
 CrossProduct:=TpvVectorPathStrokerCalculatePointLocation(X1,Y1,X2,Y2,X3,Y3);
 if (CrossProduct<>0.0) and ((CrossProduct>0.0)=(fWidth>0.0)) then begin
  // Inner join, so the side of the turn where the two offset edges overlap each other.
  InnerLimit:=Max(Min(Distance1,Distance2)/fWidthAbsolute,fSettings.InnerMiterLimit);
  case fSettings.InnerJoinMode of
   TpvVectorPathStrokeInnerJoinMode.Miter:begin
    DoMiter(X1,Y1,X2,Y2,X3,Y3,DX1,DY1,DX2,DY2,TpvVectorPathStrokeJoinMode.MiterRevert,InnerLimit,0.0);
   end;
   TpvVectorPathStrokeInnerJoinMode.Jag:begin
    Distance:=sqr(DX1-DX2)+sqr(DY1-DY2);
    if (Distance<sqr(Distance1)) and (Distance<sqr(Distance2)) then begin
     DoMiter(X1,Y1,X2,Y2,X3,Y3,DX1,DY1,DX2,DY2,TpvVectorPathStrokeJoinMode.MiterRevert,InnerLimit,0.0);
    end else begin
     Point(X2+DX1,Y2-DY1);
     Point(X2,Y2);
     Point(X2+DX2,Y2-DY2);
    end;
   end;
   TpvVectorPathStrokeInnerJoinMode.Round:begin
    Distance:=sqr(DX1-DX2)+sqr(DY1-DY2);
    if (Distance<sqr(Distance1)) and (Distance<sqr(Distance2)) then begin
     DoMiter(X1,Y1,X2,Y2,X3,Y3,DX1,DY1,DX2,DY2,TpvVectorPathStrokeJoinMode.MiterRevert,InnerLimit,0.0);
    end else begin
     Point(X2+DX1,Y2-DY1);
     Point(X2,Y2);
     DoArc(X2,Y2,DX2,-DY2,DX1,-DY1);
     Point(X2,Y2);
     Point(X2+DX2,Y2-DY2);
    end;
   end;
   else {TpvVectorPathStrokeInnerJoinMode.Bevel:}begin
    Point(X2+DX1,Y2-DY1);
    Point(X2+DX2,Y2-DY2);
   end;
  end;
 end else begin
  DX:=(DX1+DX2)*0.5;
  DY:=(DY1+DY2)*0.5;
  Bevel:=sqrt(sqr(DX)+sqr(DY));
  if fSettings.JoinMode in [TpvVectorPathStrokeJoinMode.Round,TpvVectorPathStrokeJoinMode.Bevel] then begin
   // The turn is so shallow that at the target scale a single point is indistinguishable from
   // the proper join, so one point saves the whole arc or bevel.
   if (fApproximationScale*(fWidthAbsolute-Bevel))<fWidthEpsilon then begin
    if TpvVectorPathStrokerCalculateIntersection(X1+DX1,Y1-DY1,X2+DX1,Y2-DY1,
                                                 X2+DX2,Y2-DY2,X3+DX2,Y3-DY2,
                                                 DX,DY) then begin
     Point(DX,DY);
    end else begin
     Point(X2+DX1,Y2-DY1);
    end;
    exit;
   end;
  end;
  case fSettings.JoinMode of
   TpvVectorPathStrokeJoinMode.Round:begin
    DoArc(X2,Y2,DX1,-DY1,DX2,-DY2);
   end;
   TpvVectorPathStrokeJoinMode.Miter,
   TpvVectorPathStrokeJoinMode.MiterRevert,
   TpvVectorPathStrokeJoinMode.MiterRound:begin
    DoMiter(X1,Y1,X2,Y2,X3,Y3,DX1,DY1,DX2,DY2,fSettings.JoinMode,fSettings.MiterLimit,Bevel);
   end;
   else {TpvVectorPathStrokeJoinMode.Bevel:}begin
    Point(X2+DX1,Y2-DY1);
    Point(X2+DX2,Y2-DY2);
   end;
  end;
 end;
end;

procedure TpvVectorPathStroker.Cap(const aIndex1,aIndex2,aDistanceIndex:TpvSizeInt;
                                   const aLinePoints:TLinePoints);
var X1,Y1,X2,Y2,DX1,DY1,DX2,DY2,Distance,Angle,AngleDelta:TpvDouble;
    Index,Count:TpvSizeInt;
begin
 X1:=aLinePoints[aIndex1].Position.x;
 Y1:=aLinePoints[aIndex1].Position.y;
 X2:=aLinePoints[aIndex2].Position.x;
 Y2:=aLinePoints[aIndex2].Position.y;
 Distance:=aLinePoints[aDistanceIndex].Distance;
 DX1:=((Y2-Y1)/Distance)*fWidth;
 DY1:=((X2-X1)/Distance)*fWidth;
 case fSettings.CapMode of
  TpvVectorPathStrokeCapMode.Round:begin
   AngleDelta:=ArcCos(fWidthAbsolute/(fWidthAbsolute+(0.125/fApproximationScale)))*2.0;
   Count:=trunc(PI/AngleDelta);
   AngleDelta:=PI/(Count+1);
   Point(X1-DX1,Y1+DY1);
   if fWidthSign>0 then begin
    Angle:=ArcTan2(DY1,-DX1)+AngleDelta;
    for Index:=0 to Count-1 do begin
     Point(X1+(cos(Angle)*fWidth),Y1+(sin(Angle)*fWidth));
     Angle:=Angle+AngleDelta;
    end;
   end else begin
    Angle:=ArcTan2(-DY1,DX1)-AngleDelta;
    for Index:=0 to Count-1 do begin
     Point(X1+(cos(Angle)*fWidth),Y1+(sin(Angle)*fWidth));
     Angle:=Angle-AngleDelta;
    end;
   end;
   Point(X1+DX1,Y1-DY1);
  end;
  TpvVectorPathStrokeCapMode.Square:begin
   DX2:=DY1*fWidthSign;
   DY2:=DX1*fWidthSign;
   Point((X1-DX1)-DX2,(Y1+DY1)-DY2);
   Point((X1+DX1)-DX2,(Y1-DY1)-DY2);
  end;
  else {TpvVectorPathStrokeCapMode.Butt:}begin
   Point(X1-DX1,Y1+DY1);
   Point(X1+DX1,Y1-DY1);
  end;
 end;
end;

procedure TpvVectorPathStroker.MakeDashes;
var Index,Count,OldCount:TpvSizeInt;
begin
 fDashes:=nil;
 fDashFlags:=nil;
 Count:=0;
 for Index:=0 to length(fSettings.DashArray)-1 do begin
  if not SameValue(fSettings.DashArray[Index],0.0) then begin
   if Count>=length(fDashes) then begin
    SetLength(fDashes,(Count+1)*2);
    SetLength(fDashFlags,(Count+1)*2);
   end;
   fDashes[Count]:=fSettings.DashArray[Index];
   fDashFlags[Count]:=(Count and 1)=0;
   inc(Count);
  end;
 end;
 SetLength(fDashes,Count);
 SetLength(fDashFlags,Count);
 if (Count>0) and ((Count and 1)=1) then begin
  // An odd dash array repeats with the on and off roles swapped, exactly as SVG demands, so the
  // array gets doubled instead of carrying the parity around through the whole walk.
  OldCount:=Count;
  SetLength(fDashes,OldCount*2);
  SetLength(fDashFlags,OldCount*2);
  for Index:=0 to OldCount-1 do begin
   fDashes[OldCount+Index]:=fDashes[Index];
   fDashFlags[OldCount+Index]:=not fDashFlags[Index];
  end;
 end;
end;

procedure TpvVectorPathStroker.ConvertStrokeToPolygon(const aLinePoints:TLinePoints;const aCountLinePoints:TpvSizeInt;const aClosed:boolean);
var Index,Count:TpvSizeInt;
    Delta:TpvVectorPathVector;
    Distance:TpvDouble;
    Closed:boolean;
begin
 if aCountLinePoints<2 then begin
  exit;
 end;
 Count:=aCountLinePoints;
 Closed:=false;
 if aClosed then begin
  if SameValue(aLinePoints[0].Position.x,aLinePoints[Count-1].Position.x) and
     SameValue(aLinePoints[0].Position.y,aLinePoints[Count-1].Position.y) then begin
   // The explicit closing point coincides with the start point, so it would make a zero length
   // segment and a degenerate join.
   dec(Count);
   if Count<2 then begin
    exit;
   end;
  end;
  Closed:=true;
 end;
 for Index:=0 to Count-1 do begin
  Delta:=aLinePoints[(Index+1) mod Count].Position-aLinePoints[Index].Position;
  Distance:=Delta.Length;
  if Distance=0.0 then begin
   Distance:=1.0;
  end;
  aLinePoints[Index].Distance:=Distance;
 end;
 fWidth:=fSettings.Width*0.5;
 if abs(fWidth)<1e-15 then begin
  if fSettings.Width<0.0 then begin
   fWidth:=-1e-15;
  end else begin
   fWidth:=1e-15;
  end;
 end;
 if fWidth<0.0 then begin
  fWidthAbsolute:=-fWidth;
  fWidthSign:=-1;
 end else begin
  fWidthAbsolute:=fWidth;
  fWidthSign:=1;
 end;
 fWidthEpsilon:=fWidth/1024.0;
 if Closed then begin
  // A closed polyline yields two contours, the outer one walked forwards and the inner one
  // walked backwards, which together fill the ring under the non-zero rule.
  fFirst:=true;
  for Index:=0 to Count-1 do begin
   Join(((Index+Count)-1) mod Count,
        Index,
        (Index+1) mod Count,
        ((Index+Count)-1) mod Count,
        Index,
        aLinePoints);
  end;
  CloseOutputContour;
  fFirst:=true;
  for Index:=Count-1 downto 0 do begin
   Join((Index+1) mod Count,
        Index,
        ((Index+Count)-1) mod Count,
        Index,
        ((Index+Count)-1) mod Count,
        aLinePoints);
  end;
  CloseOutputContour;
 end else begin
  // An open polyline yields a single contour: cap, forward joins, cap, backward joins.
  fFirst:=true;
  Cap(0,1,0,aLinePoints);
  for Index:=1 to Count-2 do begin
   Join(((Index+Count)-1) mod Count,
        Index,
        (Index+1) mod Count,
        ((Index+Count)-1) mod Count,
        Index,
        aLinePoints);
  end;
  Cap(Count-1,Count-2,Count-2,aLinePoints);
  for Index:=Count-2 downto 1 do begin
   Join((Index+1) mod Count,
        Index,
        ((Index+Count)-1) mod Count,
        Index,
        ((Index+Count)-1) mod Count,
        aLinePoints);
  end;
  CloseOutputContour;
 end;
end;

procedure TpvVectorPathStroker.ProcessDashes(const aLinePoints:TLinePoints;const aCountLinePoints:TpvSizeInt;const aClosed:boolean);
var Index,LastIndex,DashIndex,PointCount:TpvSizeInt;
    Position,Start,Delta:TpvVectorPathVector;
    DashRemain,LineLength,LineRemain,StartRemain,StepLength,Factor:TpvDouble;
    IsInLine:boolean;
begin
 if (aCountLinePoints<2) or (length(fDashes)=0) then begin
  exit;
 end;
 IsInLine:=false;
 fCountDashedLinePoints:=0;
 LineRemain:=0.0;
 LineLength:=0.0;
 Start:=aLinePoints[0].Position;
 Delta:=TpvVectorPathVector.Create(0.0,0.0);
 DashIndex:=0;
 DashRemain:=fDashes[DashIndex];
 StartRemain:=fSettings.DashOffset;
 while StartRemain>0.0 do begin
  StepLength:=Min(DashRemain,StartRemain);
  if StepLength=0.0 then begin
   break;
  end;
  StartRemain:=StartRemain-StepLength;
  DashRemain:=DashRemain-StepLength;
  if DashRemain<=0.0 then begin
   inc(DashIndex);
   if DashIndex>=length(fDashes) then begin
    DashIndex:=0;
   end;
   DashRemain:=fDashes[DashIndex];
  end;
 end;
 if aClosed then begin
  PointCount:=aCountLinePoints;
  LastIndex:=aCountLinePoints-1;
 end else begin
  PointCount:=aCountLinePoints;
  LastIndex:=aCountLinePoints-2;
 end;
 for Index:=0 to LastIndex do begin
  Start:=aLinePoints[Index mod PointCount].Position;
  Delta:=aLinePoints[(Index+1) mod PointCount].Position-Start;
  LineLength:=Delta.Length;
  LineRemain:=LineLength;
  while LineRemain>0.0 do begin
   StepLength:=Min(DashRemain,LineRemain);
   if StepLength=0.0 then begin
    break;
   end;
   Factor:=(LineLength-LineRemain)/LineLength;
   Position:=Start+(Delta*Factor);
   if fDashFlags[DashIndex] then begin
    IsInLine:=true;
    AddDashedLinePoint(Position);
   end else begin
    if IsInLine then begin
     IsInLine:=false;
     AddDashedLinePoint(Position);
     ConvertStrokeToPolygon(fDashedLinePoints,fCountDashedLinePoints,false);
     fCountDashedLinePoints:=0;
    end;
   end;
   LineRemain:=LineRemain-StepLength;
   DashRemain:=DashRemain-StepLength;
   if DashRemain<=0.0 then begin
    inc(DashIndex);
    if DashIndex>=length(fDashes) then begin
     DashIndex:=0;
    end;
    DashRemain:=fDashes[DashIndex];
   end;
  end;
 end;
 if IsInLine and (LineLength>1e-18) then begin
  Factor:=(LineLength-LineRemain)/LineLength;
  AddDashedLinePoint(Start+(Delta*Factor));
  ConvertStrokeToPolygon(fDashedLinePoints,fCountDashedLinePoints,false);
  fCountDashedLinePoints:=0;
 end;
end;

procedure TpvVectorPathStroker.FlushLine(const aClosed:boolean);
begin
 if fCountLinePoints>0 then begin
  if length(fDashes)>0 then begin
   ProcessDashes(fLinePoints,fCountLinePoints,aClosed);
  end else begin
   ConvertStrokeToPolygon(fLinePoints,fCountLinePoints,aClosed);
  end;
  fCountLinePoints:=0;
 end;
end;

function TpvVectorPathStroker.Stroke(const aShape:TpvVectorPathShape;
                                     const aSettings:TpvVectorPathStrokeSettings;
                                     const aApproximationScale:TpvDouble):TpvVectorPathShape;
var Contour:TpvVectorPathContour;
    Segment:TpvVectorPathSegment;
    SegmentLine:TpvVectorPathSegmentLine;
    First:boolean;
begin
 fSettings.Assign(aSettings);
 if aApproximationScale>0.125 then begin
  fApproximationScale:=aApproximationScale;
 end else begin
  fApproximationScale:=0.125;
 end;
 MakeDashes;
 result:=TpvVectorPathShape.Create;
 try
  result.fFillRule:=TpvVectorPathFillRule.NonZero;
  fOutputShape:=result;
  fOutputContour:=nil;
  fCountLinePoints:=0;
  fCountDashedLinePoints:=0;
  fFirst:=true;
  fHasLastPosition:=false;
  if assigned(aShape) then begin
   for Contour in aShape.fContours do begin
    if assigned(Contour) then begin
     First:=true;
     for Segment in Contour.fSegments do begin
      // Only line segments are walked, so the caller has to flatten the curves beforehand, which
      // keeps the flattening tolerance in the caller's hands where the target scale is known.
      if Segment is TpvVectorPathSegmentLine then begin
       SegmentLine:=TpvVectorPathSegmentLine(Segment);
       if First then begin
        First:=false;
        AddLinePoint(SegmentLine.Points[0]);
       end;
       AddLinePoint(SegmentLine.Points[1]);
      end;
     end;
     FlushLine(Contour.fClosed);
    end;
   end;
  end;
 finally
  fOutputShape:=nil;
  fOutputContour:=nil;
 end;
end;

{ TpvVectorPathGPUShape.TGridCell }

function TpvVectorPathGPUShape_TGridCell_WindingEventsSortFunc(const a,b:TpvVectorPathGPUShape.TWindingChangeEvent):TpvInt32;
begin
 result:=Sign(a.Y-b.Y);
end;

constructor TpvVectorPathGPUShape.TGridCell.Create(const aVectorPathGPUShape:TpvVectorPathGPUShape;const aBoundingBox:TpvVectorPathBoundingBox);
begin

 inherited Create;

 fVectorPathGPUShape:=aVectorPathGPUShape;

 fBoundingBox:=aBoundingBox;

 fExtendedBoundingBox.MinMax[0]:=fBoundingBox.MinMax[0]-TpvVectorPathVector.Create(TpvVectorPathGPUShape.CoordinateExtents,TpvVectorPathGPUShape.CoordinateExtents);
 fExtendedBoundingBox.MinMax[1]:=fBoundingBox.MinMax[1]+TpvVectorPathVector.Create(TpvVectorPathGPUShape.CoordinateExtents,TpvVectorPathGPUShape.CoordinateExtents);

 fSegments:=TpvVectorPathSegments.Create;
 fSegments.OwnsObjects:=false;

 // No list of its own meta winding setting lines, because a cell built this way never makes any:
 // the bands have none at all, and a cell read back from a cache gets the ones the shape already
 // owns. At a grid of three hundred thousand cells that is three hundred thousand allocations
 // saved, which is most of what loading one used to cost.
 fMetaWindingSettingLines:=nil;

 fKind:=TKind.Empty;

end;

constructor TpvVectorPathGPUShape.TGridCell.Create(const aVectorPathGPUShape:TpvVectorPathGPUShape;const aOutlineSegments:TpvVectorPathSegments;const aBoundingBox:TpvVectorPathBoundingBox);
const EPSILON=1e-8;
type TYCoordinateHashMap=TpvHashMap<TpvDouble,Boolean>;
var Segment:TpvVectorPathSegment;
    IntersectionSegments:TpvVectorPathSegments;
    BoundingBox:TpvVectorPathBoundingBox;
    EventIndex,SegmentIndex,OtherSegmentIndex,PointIndex:TpvSizeInt;
    CountWindingChangeEvents:TpvSizeInt;
    CurrentWinding:TpvInt32;
    WindingChangeEvents:TpvVectorPathGPUShape.TWindingChangeEvents;
    WindingEvents:TpvVectorPathGPUShape.TWindingChangeEventDynamicArray;
    WindingEvent:TpvVectorPathGPUShape.PWindingChangeEvent;
    // YCoordinateHashMap:TYCoordinateHashMap;
    Vector:TpvVectorPathVector;
    LastY,CurrentY:TpvDouble;
    // IntersectionPoints:TpvVectorPathVectorList;
    DummyGridCellLeftSplitSegmentLine:TpvVectorPathSegmentLine;
    SegmentMetaWindingSettingLine:TpvVectorPathSegmentMetaWindingSettingLine;
begin
 inherited Create;

 fVectorPathGPUShape:=aVectorPathGPUShape;

 fBoundingBox:=aBoundingBox;

 fExtendedBoundingBox.MinMax[0]:=fBoundingBox.MinMax[0]-TpvVectorPathVector.Create(TpvVectorPathGPUShape.CoordinateExtents,TpvVectorPathGPUShape.CoordinateExtents);
 fExtendedBoundingBox.MinMax[1]:=fBoundingBox.MinMax[1]+TpvVectorPathVector.Create(TpvVectorPathGPUShape.CoordinateExtents,TpvVectorPathGPUShape.CoordinateExtents);

 fSegments:=TpvVectorPathSegments.Create;
 fSegments.OwnsObjects:=false;

 fMetaWindingSettingLines:=TpvVectorPathSegments.Create;
 fMetaWindingSettingLines.OwnsObjects:=true;

 // IntersectionPoints:=TpvVectorPathVectorList.Create; // <= nothing collects into it any more
 try

  IntersectionSegments:=TpvVectorPathSegments.Create;
  try

   IntersectionSegments.OwnsObjects:=false;

   for Segment in aOutlineSegments do begin
    BoundingBox:=Segment.GetBoundingBox;
    if Segment.Type_<>TpvVectorPathSegmentType.MetaWindingSettingLine then begin
     if fExtendedBoundingBox.Intersect(BoundingBox) then begin
      // A segment that reaches into this cell is evaluated inside it, point by point, so it must
      // not also go into the starting winding below, which would count it a second time.
      fSegments.Add(Segment);
     end else if ((BoundingBox.MinMax[0].x-TpvVectorPathBoundingBox.EPSILON)<=(fExtendedBoundingBox.MinMax[0].x+TpvVectorPathBoundingBox.EPSILON)) and
                 ((BoundingBox.MinMax[0].y-TpvVectorPathBoundingBox.EPSILON)<=(fExtendedBoundingBox.MinMax[1].y+TpvVectorPathBoundingBox.EPSILON)) and
                 ((fExtendedBoundingBox.MinMax[0].y-TpvVectorPathBoundingBox.EPSILON)<=(BoundingBox.MinMax[1].y+TpvVectorPathBoundingBox.EPSILON)) then begin
      // Everything else that lies to the left of the cell and spans its rows contributes a fixed
      // amount to every point in it, which is what the meta winding setting lines carry.
      IntersectionSegments.Add(Segment);
     end;
    end;
   end;

   // The segments of the cell are sorted because that is the order they go to the buffers in, and
   // one the shader walks from left to right reads them in the order it meets them.
   fSegments.SortHorizontal;

   // The feeding ones are only ever added up, and a sum does not care in which order it is taken,
   // so sorting them was work with nothing to show for it.
   // IntersectionSegments.SortHorizontal;

   { Collected the heights at which the feeding segments cross each other and the left edge of the
     cell, and took those as places where the starting winding changes as well. Neither of them is
     one. Where two segments cross, both of them go on crossing the scan line, so the number of
     crossings and with it the winding stays what it was. And a feeding segment lies wholly to the
     left of the cell by the way it was picked, so it never reaches that left edge at all. The
     heights that really matter are the ones GetWindingChangeYCoordinates gives below, and these
     cost a pass over every pair of segments in every cell, which was most of the time the whole
     grid took to build.
   DummyGridCellLeftSplitSegmentLine:=TpvVectorPathSegmentLine.Create(fExtendedBoundingBox.MinMax[0],TpvVectorPathVector.Create(fExtendedBoundingBox.MinMax[0].x,fExtendedBoundingBox.MinMax[1].y));
   try

    for SegmentIndex:=0 to IntersectionSegments.Count-1 do begin
     Segment:=IntersectionSegments[SegmentIndex];
     Segment.GetIntersectionPointsWithSegment(DummyGridCellLeftSplitSegmentLine,IntersectionPoints);
     for OtherSegmentIndex:=SegmentIndex+1 to IntersectionSegments.Count-1 do begin
      Segment.GetIntersectionPointsWithSegment(IntersectionSegments[OtherSegmentIndex],IntersectionPoints);
     end;
    end;

   finally
    FreeAndNil(DummyGridCellLeftSplitSegmentLine);
   end;

   IntersectionPoints.RemoveDuplicates;

   for PointIndex:=IntersectionPoints.Count-1 downto 0 do begin
    Vector:=IntersectionPoints[PointIndex];
    if Vector.x>=fExtendedBoundingBox.MinMax[0].x then begin
     IntersectionPoints.Delete(PointIndex);
    end;
   end;
   }

   WindingEvents.Initialize;
   try

    { A hash map kept the heights apart that two segments share, which is one allocation per cell
      for something the sorted walk below does by itself: it skips a height that is the same as the
      one before it, so a height collected twice costs one comparison and nothing else.
    YCoordinateHashMap:=TYCoordinateHashMap.Create(false);
    try
    }

     // Where a segment to the left starts, ends or turns around in y, the number of times it
     // crosses a scan line changes, so the starting winding has to change there too. Every one of
     // those is collected with the amount it changes it by, so that one walk up through them adds
     // up the winding of every span at once.
     for Segment in IntersectionSegments do begin
      CountWindingChangeEvents:=TpvVectorPathGPUShape.GetWindingChangeEvents(Segment,WindingChangeEvents);
      for PointIndex:=0 to CountWindingChangeEvents-1 do begin
       WindingEvents.Add(WindingChangeEvents[PointIndex]);
      end;
     end;

     { Went with the crossing points collected above, which are no longer collected.
     for Vector in IntersectionPoints do begin
      //if (Vector.x-TpvVectorPathBoundingBox.EPSILON)<=(fExtendedBoundingBox.MinMax[1].x+TpvVectorPathBoundingBox.EPSILON) then begin
      if Vector.x<fExtendedBoundingBox.MinMax[0].x then begin
       CurrentY:=Vector.y;
       if not YCoordinateHashMap.ExistKey(CurrentY) then begin
        YCoordinateHashMap.Add(CurrentY,true);
        YCoordinates.Add(CurrentY);
       end;
      end;
     end;
     }
    {finally
     FreeAndNil(YCoordinateHashMap);
    end;}

    if WindingEvents.Count>=2 then begin
     TpvTypedSort<TpvVectorPathGPUShape.TWindingChangeEvent>.IntroSort(@WindingEvents.Items[0],0,WindingEvents.Count-1,TpvVectorPathGPUShape_TGridCell_WindingEventsSortFunc);
    end;

    // One walk from the bottom of the cell up through the heights. Whatever happens below the cell
    // has already happened by the time it begins, so those events only move the winding along, and
    // from there every stretch between two heights is one span of constant winding.
    CurrentWinding:=0;
    LastY:=fExtendedBoundingBox.MinMax[0].y;
    for EventIndex:=0 to WindingEvents.Count do begin
     if EventIndex<WindingEvents.Count then begin
      WindingEvent:=@WindingEvents.Items[EventIndex];
      CurrentY:=WindingEvent^.Y;
      if CurrentY<=fExtendedBoundingBox.MinMax[0].y then begin
       inc(CurrentWinding,WindingEvent^.Delta);
       continue;
      end;
      if CurrentY>=fExtendedBoundingBox.MinMax[1].y then begin
       CurrentY:=fExtendedBoundingBox.MinMax[1].y;
       WindingEvent:=nil;
      end;
     end else begin
      CurrentY:=fExtendedBoundingBox.MinMax[1].y;
      WindingEvent:=nil;
     end;
     if not SameValue(CurrentY,LastY) then begin
      // The winding is the one of everything to the left of the cell, which is where the feeding
      // segments were picked from, and it holds for every height from LastY up to but not
      // including CurrentY, which is the very span this line carries.
      SegmentMetaWindingSettingLine:=TpvVectorPathSegmentMetaWindingSettingLine.Create(TpvVectorPathVector.Create(-Infinity,LastY),
                                                                                       TpvVectorPathVector.Create(-Infinity,CurrentY),
                                                                                       CurrentWinding);
      try
       fSegments.Add(SegmentMetaWindingSettingLine);
      finally
       fMetaWindingSettingLines.Add(SegmentMetaWindingSettingLine);
      end;
      LastY:=CurrentY;
     end;
     if not assigned(WindingEvent) then begin
      break;
     end;
     inc(CurrentWinding,WindingEvent^.Delta);
    end;

   finally
    WindingEvents.Finalize;
   end;

  finally
   FreeAndNil(IntersectionSegments);
  end;

 finally
  // FreeAndNil(IntersectionPoints);
 end;

end;

destructor TpvVectorPathGPUShape.TGridCell.Destroy;
begin
 FreeAndNil(fMetaWindingSettingLines);
 FreeAndNil(fSegments);
 inherited Destroy;
end;

{ TpvVectorPathGPUShape }

constructor TpvVectorPathGPUShape.Create(const aBufferPool:TpvVectorPathGPUBufferPool;const aVectorPathShape:TpvVectorPathShape;const aResolution:TpvInt32;const aBoundingBoxExtent:TpvDouble;const aPasMPInstance:TPasMP;const aAccelerationStructure:TpvVectorPathAccelerationStructure;const aCountBands:TpvInt32;const aDuplicateSharing:TpvVectorPathDuplicateSharing);
begin

 inherited Create;

 fBufferPool:=aBufferPool;

 fPasMPInstance:=aPasMPInstance;

 fResolution:=aResolution;

 fAccelerationStructure:=aAccelerationStructure;
 fCountBands:=aCountBands;
 fDuplicateSharing:=aDuplicateSharing;
 fCountHorizontalBands:=0;
 fCountVerticalBands:=0;

 fGeneration:=1;

 fSegmentBufferRange.Clear;
 fIndirectSegmentBufferRange.Clear;
 fGridCellBufferRange.Clear;
 fShapeIndex:=-1; 

 fGridCells:=nil;
 fSegmentDynamicAABBTree:=nil;
 fSegments:=nil;
 fVectorPathShape:=nil;       

 fSourceVectorPathShape:=aVectorPathShape;

 fBoundingBoxExtent:=aBoundingBoxExtent;

 Update;

end;

constructor TpvVectorPathGPUShape.Create(const aBufferPool:TpvVectorPathGPUBufferPool;const aVectorPathShape:TpvVectorPathShape;const aStreamIO:TpvStreamIO;const aPasMPInstance:TPasMP);
begin

 inherited Create;

 fBufferPool:=aBufferPool;

 fPasMPInstance:=aPasMPInstance;

 fGeneration:=1;

 fSegmentBufferRange.Clear;
 fIndirectSegmentBufferRange.Clear;
 fGridCellBufferRange.Clear;
 fShapeIndex:=-1;

 fGridCells:=nil;
 fSegmentDynamicAABBTree:=nil;
 fSegments:=nil;
 fVectorPathShape:=nil;

 fSourceVectorPathShape:=aVectorPathShape;

 LoadFromStream(aStreamIO);

end;

procedure TpvVectorPathGPUShape.SaveToStream(const aStreamIO:TpvStreamIO);
var Index,CellIndex,CountAllIndices:TpvSizeInt;
    GridCell:TGridCell;
    CellBoxes:array of TpvVectorPathBoundingBox;
    CellCounts,CellIndices:array of TpvUInt32;
begin

 aStreamIO.WriteInt32(fResolution);
 aStreamIO.WriteUInt32(TpvUInt32(TpvVectorPathAccelerationStructure(fAccelerationStructure)));
 aStreamIO.WriteInt32(fCountBands);
 aStreamIO.WriteInt32(fCountHorizontalBands);
 aStreamIO.WriteInt32(fCountVerticalBands);
 aStreamIO.WriteUInt32(TpvUInt32(TpvVectorPathDuplicateSharing(fDuplicateSharing)));
 aStreamIO.WriteDouble(fBoundingBoxExtent);
 aStreamIO.WriteDouble(fBoundingBox.MinMax[0].x);
 aStreamIO.WriteDouble(fBoundingBox.MinMax[0].y);
 aStreamIO.WriteDouble(fBoundingBox.MinMax[1].x);
 aStreamIO.WriteDouble(fBoundingBox.MinMax[1].y);

 // The outline after the cubic curves were turned into quadratic ones, which is what the cells
 // hold their segments of and what the fill rule is read from.
 fVectorPathShape.SaveToStream(aStreamIO);

 // Every segment of the shape, the closing ones and the meta winding setting lines among them.
 for Index:=0 to fSegments.Count-1 do begin
  fSegments[Index].fIndex:=Index;
 end;
 TpvVectorPathShape.SaveSegmentsToStream(aStreamIO,fSegments);

 // And which of them each cell or band holds, as indices into the list above rather than as
 // segments of their own, since they are the very same objects. Boxes, counts and indices each go
 // as one block.
 aStreamIO.WriteInt32(fGridCells.Count);
 if fGridCells.Count>0 then begin
  SetLength(CellBoxes,fGridCells.Count);
  SetLength(CellCounts,fGridCells.Count);
  CountAllIndices:=0;
  for CellIndex:=0 to fGridCells.Count-1 do begin
   GridCell:=fGridCells[CellIndex];
   CellBoxes[CellIndex]:=GridCell.fBoundingBox;
   CellCounts[CellIndex]:=GridCell.fSegments.Count;
   inc(CountAllIndices,GridCell.fSegments.Count);
  end;
  aStreamIO.WriteWithCheck(CellBoxes[0],fGridCells.Count*SizeOf(TpvVectorPathBoundingBox));
  aStreamIO.WriteWithCheck(CellCounts[0],fGridCells.Count*SizeOf(TpvUInt32));
  aStreamIO.WriteInt32(CountAllIndices);
  if CountAllIndices>0 then begin
   SetLength(CellIndices,CountAllIndices);
   CountAllIndices:=0;
   for CellIndex:=0 to fGridCells.Count-1 do begin
    GridCell:=fGridCells[CellIndex];
    for Index:=0 to GridCell.fSegments.Count-1 do begin
     CellIndices[CountAllIndices]:=GridCell.fSegments[Index].fIndex;
     inc(CountAllIndices);
    end;
   end;
   aStreamIO.WriteWithCheck(CellIndices[0],CountAllIndices*SizeOf(TpvUInt32));
  end;
 end;

end;

procedure TpvVectorPathGPUShape.LoadFromStream(const aStreamIO:TpvStreamIO);
var Index,CellIndex,CountCells,CountAllIndices,SegmentIndex:TpvSizeInt;
    GridCell:TGridCell;
    CellBoxes:array of TpvVectorPathBoundingBox;
    CellCounts,CellIndices:array of TpvUInt32;
begin

 if assigned(fBufferPool) then begin
  fBufferPool.fSegmentsAllocator.ReleaseBufferRangeAndNil(fSegmentBufferRange);
  fBufferPool.fIndirectSegmentsAllocator.ReleaseBufferRangeAndNil(fIndirectSegmentBufferRange);
  fBufferPool.fGridCellsAllocator.ReleaseBufferRangeAndNil(fGridCellBufferRange);
 end;

 FreeAndNil(fGridCells);
 FreeAndNil(fSegmentDynamicAABBTree);
 FreeAndNil(fSegments);
 FreeAndNil(fVectorPathShape);

 fResolution:=aStreamIO.ReadInt32;
 fAccelerationStructure:=TpvVectorPathAccelerationStructure(aStreamIO.ReadUInt32);
 fCountBands:=aStreamIO.ReadInt32;
 fCountHorizontalBands:=aStreamIO.ReadInt32;
 fCountVerticalBands:=aStreamIO.ReadInt32;
 fDuplicateSharing:=TpvVectorPathDuplicateSharing(aStreamIO.ReadUInt32);
 fBoundingBoxExtent:=aStreamIO.ReadDouble;
 fBoundingBox.MinMax[0].x:=aStreamIO.ReadDouble;
 fBoundingBox.MinMax[0].y:=aStreamIO.ReadDouble;
 fBoundingBox.MinMax[1].x:=aStreamIO.ReadDouble;
 fBoundingBox.MinMax[1].y:=aStreamIO.ReadDouble;

 fVectorPathShape:=TpvVectorPathShape.Create;
 fVectorPathShape.LoadFromStream(aStreamIO);

 // The tree is only ever filled while the structure is being worked out and never asked anything
 // afterwards, so an empty one is all that is needed to keep the rest of the class happy.
 fSegmentDynamicAABBTree:=TpvVectorPathBVHDynamicAABBTree.Create;

 fSegments:=TpvVectorPathSegments.Create;
 fSegments.OwnsObjects:=true;
 TpvVectorPathShape.LoadSegmentsFromStream(aStreamIO,fSegments);

 fGridCells:=TGridCells.Create;
 fGridCells.OwnsObjects:=true;
 CountCells:=aStreamIO.ReadInt32;
 fGridCells.Count:=CountCells;
 if CountCells>0 then begin
  SetLength(CellBoxes,CountCells);
  SetLength(CellCounts,CountCells);
  aStreamIO.ReadWithCheck(CellBoxes[0],CountCells*SizeOf(TpvVectorPathBoundingBox));
  aStreamIO.ReadWithCheck(CellCounts[0],CountCells*SizeOf(TpvUInt32));
  CountAllIndices:=aStreamIO.ReadInt32;
  if CountAllIndices>0 then begin
   SetLength(CellIndices,CountAllIndices);
   aStreamIO.ReadWithCheck(CellIndices[0],CountAllIndices*SizeOf(TpvUInt32));
  end;
  CountAllIndices:=0;
  for CellIndex:=0 to CountCells-1 do begin
   GridCell:=TGridCell.Create(self,CellBoxes[CellIndex]);
   fGridCells[CellIndex]:=GridCell;
   for Index:=0 to TpvSizeInt(CellCounts[CellIndex])-1 do begin
    SegmentIndex:=CellIndices[CountAllIndices];
    inc(CountAllIndices);
    if (SegmentIndex>=0) and (SegmentIndex<fSegments.Count) then begin
     GridCell.fSegments.Add(fSegments[SegmentIndex]);
    end;
   end;
  end;
 end;

end;

destructor TpvVectorPathGPUShape.Destroy;
begin

 if assigned(fBufferPool) then begin
  fBufferPool.fSegmentsAllocator.ReleaseBufferRangeAndNil(fSegmentBufferRange);
  fBufferPool.fIndirectSegmentsAllocator.ReleaseBufferRangeAndNil(fIndirectSegmentBufferRange);
  fBufferPool.fGridCellsAllocator.ReleaseBufferRangeAndNil(fGridCellBufferRange);
  fBufferPool:=nil;
 end;

 FreeAndNil(fGridCells);
 FreeAndNil(fSegmentDynamicAABBTree);
 FreeAndNil(fSegments);
 FreeAndNil(fVectorPathShape);

 inherited Destroy;
end;

procedure TpvVectorPathGPUShape.Update;
const CLOSING_EPSILON=1e-6;
var Contour:TpvVectorPathContour;
    Segment,NewSegment:TpvVectorPathSegment;
    IndexX,IndexY:TpvSizeInt;
    tx0,tx1,ty0,ty1:TpvDouble;
    OutlineSegments:TpvVectorPathSegments;
    ContourStart,ContourEnd:TpvVectorPathVector;
    GridCell:TGridCell;
 // Where a contour begins and where it ends, which is what tells whether it comes back to where it
 // started.
 function ContourStartPoint(const aContour:TpvVectorPathContour):TpvVectorPathVector;
 var FirstSegment:TpvVectorPathSegment;
 begin
  FirstSegment:=aContour.fSegments[0];
  case FirstSegment.Type_ of
   TpvVectorPathSegmentType.Line,TpvVectorPathSegmentType.MetaWindingSettingLine:begin
    result:=TpvVectorPathSegmentLine(FirstSegment).Points[0];
   end;
   TpvVectorPathSegmentType.QuadraticCurve:begin
    result:=TpvVectorPathSegmentQuadraticCurve(FirstSegment).Points[0];
   end;
   TpvVectorPathSegmentType.CubicCurve:begin
    result:=TpvVectorPathSegmentCubicCurve(FirstSegment).Points[0];
   end;
   else begin
    result:=TpvVectorPathVector.Create(0.0,0.0);
   end;
  end;
 end;
 function ContourEndPoint(const aContour:TpvVectorPathContour):TpvVectorPathVector;
 var LastSegment:TpvVectorPathSegment;
 begin
  LastSegment:=aContour.fSegments[aContour.fSegments.Count-1];
  case LastSegment.Type_ of
   TpvVectorPathSegmentType.Line,TpvVectorPathSegmentType.MetaWindingSettingLine:begin
    result:=TpvVectorPathSegmentLine(LastSegment).Points[1];
   end;
   TpvVectorPathSegmentType.QuadraticCurve:begin
    result:=TpvVectorPathSegmentQuadraticCurve(LastSegment).Points[2];
   end;
   TpvVectorPathSegmentType.CubicCurve:begin
    result:=TpvVectorPathSegmentCubicCurve(LastSegment).Points[3];
   end;
   else begin
    result:=TpvVectorPathVector.Create(0.0,0.0);
   end;
  end;
 end;
begin

 if assigned(fBufferPool) then begin
  fBufferPool.fSegmentsAllocator.ReleaseBufferRangeAndNil(fSegmentBufferRange);
  fBufferPool.fIndirectSegmentsAllocator.ReleaseBufferRangeAndNil(fIndirectSegmentBufferRange);
  fBufferPool.fGridCellsAllocator.ReleaseBufferRangeAndNil(fGridCellBufferRange);
 end;

 FreeAndNil(fGridCells);
 FreeAndNil(fSegmentDynamicAABBTree);
 FreeAndNil(fSegments);
 FreeAndNil(fVectorPathShape);

 fVectorPathShape:=TpvVectorPathShape.Create(fSourceVectorPathShape);

 // Convert cubic Bezier curves to quadratic Bezier curves, so that the GPU shader can handle them
 // more easily just using quadratic curves. The tolerance comes from the shape, because how much
 // a unit of it is worth on screen is something only its owner knows.
 fVectorPathShape.ConvertCubicCurvesToQuadraticCurves(fVectorPathShape.CubicToQuadraticTolerance);

 fBoundingBox:=fVectorPathShape.GetBoundingBox;
 fBoundingBox.MinMax[0]:=fBoundingBox.MinMax[0]-TpvVectorPathVector.Create(fBoundingBoxExtent,fBoundingBoxExtent);
 fBoundingBox.MinMax[1]:=fBoundingBox.MinMax[1]+TpvVectorPathVector.Create(fBoundingBoxExtent,fBoundingBoxExtent);

 fSegments:=TpvVectorPathSegments.Create;
 fSegments.OwnsObjects:=true;

 fSegmentDynamicAABBTree:=TpvVectorPathBVHDynamicAABBTree.Create;
 try
  for Contour in fVectorPathShape.fContours do begin
   if Contour.fSegments.Count=0 then begin
    continue;
   end;
   for Segment in Contour.fSegments do begin
    NewSegment:=Segment.Clone;
    try
     NewSegment.fIndex:=fSegments.Count;
     fSegmentDynamicAABBTree.CreateProxy(NewSegment.GetBoundingBox,TpvPtrInt(TpvPointer(NewSegment)));
    finally
     fSegments.Add(NewSegment);
    end;
   end;
   // A contour that does not come back to where it started is closed here, the way a rasterizer
   // closes one when it fills it and the way SVG says a subpath is to be filled whether or not it
   // was told to close. The ray casting the grid is built on counts only the segments that are
   // there, so without this the winding beyond the gap comes out wrong and a whole wedge of the
   // shape gets filled that the software route leaves empty.
   ContourStart:=ContourStartPoint(Contour);
   ContourEnd:=ContourEndPoint(Contour);
   if (abs(ContourStart.x-ContourEnd.x)>CLOSING_EPSILON) or
      (abs(ContourStart.y-ContourEnd.y)>CLOSING_EPSILON) then begin
    NewSegment:=TpvVectorPathSegmentLine.Create(ContourEnd,ContourStart);
    try
     NewSegment.fIndex:=fSegments.Count;
     fSegmentDynamicAABBTree.CreateProxy(NewSegment.GetBoundingBox,TpvPtrInt(TpvPointer(NewSegment)));
    finally
     fSegments.Add(NewSegment);
    end;
   end;
  end;
 finally
  fSegmentDynamicAABBTree.Rebuild;
 end;

 fGridCells:=TGridCells.Create;
 fGridCells.OwnsObjects:=true;

 // The segments of the outline as they are right now, before any cell has added its meta winding
 // setting lines to the shape, which is what every cell is to be built from.
 OutlineSegments:=TpvVectorPathSegments.Create;
 try

  OutlineSegments.OwnsObjects:=false;
  for Segment in fSegments do begin
   OutlineSegments.Add(Segment);
  end;

  if fAccelerationStructure=TpvVectorPathAccelerationStructure.SlugBands then begin
   // The finally below is what gives the list back, so nothing is freed here.
   BuildSlugBands(OutlineSegments);
   exit;
  end;

  // Every cell has its place in the grid before any of them is built, so that a row can be put
  // together without knowing what the rows before it did.
  fGridCells.Count:=fResolution*fResolution;
  for IndexY:=0 to fGridCells.Count-1 do begin
   fGridCells[IndexY]:=nil;
  end;

  if assigned(fPasMPInstance) then begin
   fPasMPInstance.Invoke(fPasMPInstance.ParallelFor(OutlineSegments,0,fResolution-1,BuildGridCellRowsJob,1,PasMPDefaultDepth,nil));
  end else begin
   BuildGridCellRowsJob(nil,0,OutlineSegments,0,fResolution-1);
  end;

  // And only now, where there is one thread again, the meta winding setting lines each cell made
  // go over to the shape, which is what owns every segment the buffers are filled from.
  //
  // They are shared first. A meta winding setting line says nothing but "over this band of
  // heights, everything to the left of this cell adds up to this much", so two of them that carry
  // the same band and the same number are the same line. Since every cell makes its own, a row of
  // cells that nothing crosses writes the same one down again and again: at the tiger there are
  // 1004908 of them and only 74955 are different, so sharing them takes the segment buffer down to
  // a fourteenth of what it was. Nothing downstream notices, because the cells simply point at the
  // one that was kept.
  if fDuplicateSharing<>TpvVectorPathDuplicateSharing.None then begin
   ShareMetaWindingSettingLines;
  end else begin
   for IndexY:=0 to fGridCells.Count-1 do begin
    GridCell:=fGridCells[IndexY];
    if assigned(GridCell) then begin
     if assigned(GridCell.fMetaWindingSettingLines) then begin
      for IndexX:=0 to GridCell.fMetaWindingSettingLines.Count-1 do begin
       fSegments.Add(GridCell.fMetaWindingSettingLines[IndexX]);
      end;
      GridCell.fMetaWindingSettingLines.OwnsObjects:=false;
      GridCell.fMetaWindingSettingLines.Clear;
     end;
    end;
   end;
  end;

 finally
  FreeAndNil(OutlineSegments);

 end;

end;

// The two sets of bands of Eric Lengyel's Slug algorithm, built out of the same outline the grid
// would be built from. A band spans the whole of the axis the ray runs along, so it carries every
// curve the ray can meet and the winding comes out of the band alone. That is what makes the meta
// winding setting lines of the grid unnecessary here, and they are the bulk of what the grid costs.
//
// Three rules out of Lengyel's own notes are followed here. The bands overlap a little, by an
// epsilon in the space of the shape, so that a pixel sitting on a boundary still finds what it
// needs. A curve that is level never goes into a horizontal band and a curve that is upright never
// into a vertical one, since neither can move the winding of a ray running along it. And the
// curves of a band are sorted by their greatest coordinate along the ray, descending, which is
// what lets the shader stop as soon as one of them lies behind the pixel.
// Sorted by band of heights and then by winding, so that lines which say the same thing end up
// next to each other and one pass over the list finds every group.
function TpvVectorPathGPUShape_MetaWindingSettingLineSortFunc(const a,b:TpvVectorPathSegment):TpvInt32;
var LeftLine,RightLine:TpvVectorPathSegmentMetaWindingSettingLine;
    LeftLower,LeftUpper,RightLower,RightUpper:TpvDouble;
begin
 LeftLine:=TpvVectorPathSegmentMetaWindingSettingLine(a);
 RightLine:=TpvVectorPathSegmentMetaWindingSettingLine(b);
 LeftLower:=Min(LeftLine.Points[0].y,LeftLine.Points[1].y);
 LeftUpper:=Max(LeftLine.Points[0].y,LeftLine.Points[1].y);
 RightLower:=Min(RightLine.Points[0].y,RightLine.Points[1].y);
 RightUpper:=Max(RightLine.Points[0].y,RightLine.Points[1].y);
 if LeftLower<RightLower then begin
  result:=-1;
 end else if LeftLower>RightLower then begin
  result:=1;
 end else if LeftUpper<RightUpper then begin
  result:=-1;
 end else if LeftUpper>RightUpper then begin
  result:=1;
 end else if LeftLine.Winding<RightLine.Winding then begin
  result:=-1;
 end else if LeftLine.Winding>RightLine.Winding then begin
  result:=1;
 end else begin
  result:=0;
 end;
end;

procedure TpvVectorPathGPUShape.ShareMetaWindingSettingLines;
var CellIndex,LineIndex,SegmentIndex,CountAll,KeptIndex:TpvSizeInt;
    GridCell:TGridCell;
    All:array of TpvVectorPathSegment;
    Canonical:TpvVectorPathSegmentHashMap;
    Line,Kept:TpvVectorPathSegment;
begin

 All:=nil;
 try

  CountAll:=0;
  for CellIndex:=0 to fGridCells.Count-1 do begin
   GridCell:=fGridCells[CellIndex];
   if assigned(GridCell) then begin
    if assigned(GridCell.fMetaWindingSettingLines) then begin
     inc(CountAll,GridCell.fMetaWindingSettingLines.Count);
    end;
   end;
  end;
  if CountAll=0 then begin
   exit;
  end;

  SetLength(All,CountAll);
  CountAll:=0;
  for CellIndex:=0 to fGridCells.Count-1 do begin
   GridCell:=fGridCells[CellIndex];
   if assigned(GridCell) then begin
    for LineIndex:=0 to GridCell.fMetaWindingSettingLines.Count-1 do begin
     All[CountAll]:=GridCell.fMetaWindingSettingLines[LineIndex];
     inc(CountAll);
    end;
   end;
  end;

  if CountAll>1 then begin
   TpvTypedSort<TpvVectorPathSegment>.IntroSort(@All[0],0,CountAll-1,TpvVectorPathGPUShape_MetaWindingSettingLineSortFunc);
  end;

  // Which line each of them is to be replaced by: the first of its group, which is the one the
  // shape keeps.
  Canonical:=TpvVectorPathSegmentHashMap.Create(nil);
  try

   KeptIndex:=0;
   fSegments.Add(All[0]);
   Canonical.Add(All[0],All[0]);
   for LineIndex:=1 to CountAll-1 do begin
    if TpvVectorPathGPUShape_MetaWindingSettingLineSortFunc(All[KeptIndex],All[LineIndex])=0 then begin
     Canonical.Add(All[LineIndex],All[KeptIndex]);
    end else begin
     KeptIndex:=LineIndex;
     fSegments.Add(All[LineIndex]);
     Canonical.Add(All[LineIndex],All[LineIndex]);
    end;
   end;

   // Every cell now points at the kept one instead of at its own copy, and the copies go. The
   // kept ones belong to the shape from here on, which is what owns every segment the buffers are
   // filled from, so the cells let go of all of them either way.
   for CellIndex:=0 to fGridCells.Count-1 do begin
    GridCell:=fGridCells[CellIndex];
    if assigned(GridCell) then begin
     for SegmentIndex:=0 to GridCell.fSegments.Count-1 do begin
      Line:=GridCell.fSegments[SegmentIndex];
      if Line.Type_=TpvVectorPathSegmentType.MetaWindingSettingLine then begin
       if Canonical.TryGet(Line,Kept) and (Kept<>Line) then begin
        GridCell.fSegments[SegmentIndex]:=Kept;
       end;
      end;
     end;
     if assigned(GridCell.fMetaWindingSettingLines) then begin
      GridCell.fMetaWindingSettingLines.OwnsObjects:=false;
      for LineIndex:=0 to GridCell.fMetaWindingSettingLines.Count-1 do begin
       Line:=GridCell.fMetaWindingSettingLines[LineIndex];
       if Canonical.TryGet(Line,Kept) and (Kept<>Line) then begin
        Line.Free;
       end;
      end;
      GridCell.fMetaWindingSettingLines.Clear;
     end;
    end;
   end;

  finally
   FreeAndNil(Canonical);
  end;

 finally
  All:=nil;
 end;

end;

procedure TpvVectorPathGPUShape.BuildSlugBands(const aOutlineSegments:TpvVectorPathSegments);
const Epsilon=1.0/1024.0;
var Curves:TpvVectorPathSegments;
    Maxima,Minima:array of TpvVectorPathVector;
    Index,OtherIndex,BandIndex,FirstBand,LastBand,CountCurves,CountBands:TpvSizeInt;
    Segment:TpvVectorPathSegment;
    SegmentBoundingBox:TpvVectorPathBoundingBox;
    Width,Height,BandWidth,BandHeight,ScaledEpsilon:TpvDouble;
    GridCell:TGridCell;
    IsLevel,IsUpright:boolean;
 // Sorts the segments a band holds by their greatest coordinate along the axis the ray runs,
 // greatest first, which is the order the shader walks them in.
 procedure SortBand(const aGridCell:TGridCell;const aHorizontal:boolean);
 var IndexA,IndexB:TpvSizeInt;
     Temp:TpvVectorPathSegment;
     Left,Right:TpvDouble;
 begin
  for IndexA:=0 to aGridCell.fSegments.Count-2 do begin
   for IndexB:=0 to (aGridCell.fSegments.Count-2)-IndexA do begin
    if aHorizontal then begin
     Left:=Maxima[aGridCell.fSegments[IndexB].fIndex].x;
     Right:=Maxima[aGridCell.fSegments[IndexB+1].fIndex].x;
    end else begin
     Left:=Maxima[aGridCell.fSegments[IndexB].fIndex].y;
     Right:=Maxima[aGridCell.fSegments[IndexB+1].fIndex].y;
    end;
    if Left<Right then begin
     Temp:=aGridCell.fSegments[IndexB];
     aGridCell.fSegments[IndexB]:=aGridCell.fSegments[IndexB+1];
     aGridCell.fSegments[IndexB+1]:=Temp;
    end;
   end;
  end;
 end;
begin

 Width:=fBoundingBox.MinMax[1].x-fBoundingBox.MinMax[0].x;
 Height:=fBoundingBox.MinMax[1].y-fBoundingBox.MinMax[0].y;

 Curves:=TpvVectorPathSegments.Create;
 try

  Curves.OwnsObjects:=false;

  // Only real outline segments go in, and each keeps the index it has in the shape, since that is
  // what the indirect segment buffer is filled from.
  for Index:=0 to aOutlineSegments.Count-1 do begin
   Segment:=aOutlineSegments[Index];
   if Segment.Type_<>TpvVectorPathSegmentType.MetaWindingSettingLine then begin
    Curves.Add(Segment);
   end;
  end;

  CountCurves:=Curves.Count;
  if CountCurves=0 then begin
   fCountHorizontalBands:=1;
   fCountVerticalBands:=1;
   fGridCells.Count:=2;
   for Index:=0 to 1 do begin
    fGridCells[Index]:=TGridCell.Create(self,fBoundingBox);
   end;
   exit;
  end;

  SetLength(Minima,fSegments.Count);
  SetLength(Maxima,fSegments.Count);
  for Index:=0 to Curves.Count-1 do begin
   Segment:=Curves[Index];
   SegmentBoundingBox:=Segment.GetBoundingBox;
   Minima[Segment.fIndex]:=SegmentBoundingBox.MinMax[0];
   Maxima[Segment.fIndex]:=SegmentBoundingBox.MinMax[1];
  end;

  // Left to itself, Slug takes half the curve count per axis and caps it at sixteen. The aim is to
  // keep the fullest band as thin as possible, and more bands cost only index entries, never a
  // copy of a curve.
  if fCountBands>0 then begin
   CountBands:=fCountBands;
  end else begin
   CountBands:=Min(16,Max(1,CountCurves div 2));
  end;
  fCountHorizontalBands:=CountBands;
  fCountVerticalBands:=CountBands;

  BandHeight:=Height/fCountHorizontalBands;
  BandWidth:=Width/fCountVerticalBands;
  if (BandHeight<1e-12) or (BandWidth<1e-12) then begin
   fCountHorizontalBands:=1;
   fCountVerticalBands:=1;
   BandHeight:=Max(Height,1e-12);
   BandWidth:=Max(Width,1e-12);
  end;

  // The horizontal ones first and the vertical ones after them, which is the order the shader
  // expects to find them in.
  fGridCells.Count:=fCountHorizontalBands+fCountVerticalBands;
  for Index:=0 to fGridCells.Count-1 do begin
   fGridCells[Index]:=TGridCell.Create(self,fBoundingBox);
  end;

  ScaledEpsilon:=Epsilon*Max(Width,Height);

  for Index:=0 to Curves.Count-1 do begin

   Segment:=Curves[Index];
   OtherIndex:=Segment.fIndex;

   IsLevel:=abs(Maxima[OtherIndex].y-Minima[OtherIndex].y)<1e-12;
   IsUpright:=abs(Maxima[OtherIndex].x-Minima[OtherIndex].x)<1e-12;

   // A level curve crosses no ray that runs along x, so it belongs in no horizontal band.
   if not IsLevel then begin
    FirstBand:=trunc(Floor(((Minima[OtherIndex].y-ScaledEpsilon)-fBoundingBox.MinMax[0].y)/BandHeight));
    LastBand:=trunc(Floor(((Maxima[OtherIndex].y+ScaledEpsilon)-fBoundingBox.MinMax[0].y)/BandHeight));
    FirstBand:=Min(Max(FirstBand,0),fCountHorizontalBands-1);
    LastBand:=Min(Max(LastBand,0),fCountHorizontalBands-1);
    for BandIndex:=FirstBand to LastBand do begin
     fGridCells[BandIndex].fSegments.Add(Segment);
    end;
   end;

   // And an upright one crosses no ray that runs along y.
   if not IsUpright then begin
    FirstBand:=trunc(Floor(((Minima[OtherIndex].x-ScaledEpsilon)-fBoundingBox.MinMax[0].x)/BandWidth));
    LastBand:=trunc(Floor(((Maxima[OtherIndex].x+ScaledEpsilon)-fBoundingBox.MinMax[0].x)/BandWidth));
    FirstBand:=Min(Max(FirstBand,0),fCountVerticalBands-1);
    LastBand:=Min(Max(LastBand,0),fCountVerticalBands-1);
    for BandIndex:=FirstBand to LastBand do begin
     fGridCells[fCountHorizontalBands+BandIndex].fSegments.Add(Segment);
    end;
   end;

  end;

  for Index:=0 to fCountHorizontalBands-1 do begin
   SortBand(fGridCells[Index],true);
  end;
  for Index:=0 to fCountVerticalBands-1 do begin
   SortBand(fGridCells[fCountHorizontalBands+Index],false);
  end;

 finally
  FreeAndNil(Curves);
 end;

end;

procedure TpvVectorPathGPUShape.BuildGridCellRowsJob(const aJob:PPasMPJob;const aThreadIndex:TPasMPInt32;const aData:pointer;const aFromIndex,aToIndex:TPasMPNativeInt);
var IndexX,IndexY:TpvSizeInt;
    tx0,tx1,ty0,ty1,RowMinY,RowMaxY:TpvDouble;
    OutlineSegments,RowSegments:TpvVectorPathSegments;
    Segment:TpvVectorPathSegment;
    SegmentBoundingBox:TpvVectorPathBoundingBox;
begin

 OutlineSegments:=TpvVectorPathSegments(aData);

 // One list per worker rather than one for all of them, since each is only ever read by the row it
 // was filled for.
 RowSegments:=TpvVectorPathSegments.Create;
 try

  RowSegments.OwnsObjects:=false;

  for IndexY:=aFromIndex to aToIndex do begin

   ty0:=IndexY/fResolution;
   ty1:=(IndexY+1)/fResolution;

   // Both ways a segment can matter to a cell, reaching into it and feeding its starting winding
   // from the left, need it to span some of the heights the cell covers. So the segments of a row
   // of cells are picked out once here instead of every cell walking all of them, which is what
   // makes the cost of a row depend on how much of the outline really runs through it.
   RowMinY:=((fBoundingBox.MinMax[0].y*(1.0-ty0))+(fBoundingBox.MinMax[1].y*ty0))-(TpvVectorPathGPUShape.CoordinateExtents+TpvVectorPathBoundingBox.EPSILON);
   RowMaxY:=((fBoundingBox.MinMax[0].y*(1.0-ty1))+(fBoundingBox.MinMax[1].y*ty1))+(TpvVectorPathGPUShape.CoordinateExtents+TpvVectorPathBoundingBox.EPSILON);
   RowSegments.Clear;
   for Segment in OutlineSegments do begin
    SegmentBoundingBox:=Segment.GetBoundingBox;
    if (SegmentBoundingBox.MinMax[0].y<=RowMaxY) and (SegmentBoundingBox.MinMax[1].y>=RowMinY) then begin
     RowSegments.Add(Segment);
    end;
   end;

   for IndexX:=0 to fResolution-1 do begin
    tx0:=IndexX/fResolution;
    tx1:=(IndexX+1)/fResolution;
    fGridCells[(IndexY*fResolution)+IndexX]:=TGridCell.Create(self,
                                                              RowSegments,
                                                              TpvVectorPathBoundingBox.Create(TpvVectorPathVector.Create((fBoundingBox.MinMax[0].x*(1.0-tx0))+(fBoundingBox.MinMax[1].x*tx0),
                                                                                                                         (fBoundingBox.MinMax[0].y*(1.0-ty0))+(fBoundingBox.MinMax[1].y*ty0)),
                                                                                              TpvVectorPathVector.Create((fBoundingBox.MinMax[0].x*(1.0-tx1))+(fBoundingBox.MinMax[1].x*tx1),
                                                                                                                         (fBoundingBox.MinMax[0].y*(1.0-ty1))+(fBoundingBox.MinMax[1].y*ty1))));
   end;

  end;

 finally
  FreeAndNil(RowSegments);
 end;

end;

procedure TpvVectorPathGPUShape.ClearAndInvalidateBufferRanges;
begin
 if assigned(fBufferPool) then begin
  fBufferPool.fSegmentsAllocator.ReleaseBufferRangeAndNil(fSegmentBufferRange);
  fBufferPool.fIndirectSegmentsAllocator.ReleaseBufferRangeAndNil(fIndirectSegmentBufferRange);
  fBufferPool.fGridCellsAllocator.ReleaseBufferRangeAndNil(fGridCellBufferRange);
 end;
end;

procedure TpvVectorPathGPUShape.UpdateBufferPool;
var OldCount,NewCount,Index,OtherIndex:TpvSizeInt;
    Segment:TpvVectorPathSegment;
    CellListOffsets,CellListSharedWith,CellOrder:array of TpvSizeInt;
    OrderIndex,RunStart,LongestList:TpvSizeInt;
    LengthCounts:array of TpvSizeInt;
    ListHashMap:TpvVectorPathIndirectListHashMap;
    ListHash,PackedWhere:TpvUInt64;
    SameList:boolean;
    SegmentData:PpvVectorPathGPUSegmentData;
    GridCell:TpvVectorPathGPUShape.TGridCell;
    GridCellData:PpvVectorPathGPUGridCellData;
    ShapeData:PpvVectorPathGPUShapeData;
begin

 // Check if a buffer pool is assigned
 if assigned(fBufferPool) then begin

  // Update segments
  if fSegmentBufferRange.Size<fSegments.Count then begin
   fBufferPool.fSegmentsAllocator.ReleaseBufferRangeAndNil(fSegmentBufferRange);
  end;
  if fSegmentBufferRange.Offset<0 then begin
   fSegmentBufferRange:=fBufferPool.fSegmentsAllocator.AllocateBufferRange(fSegments.Count);
  end;
  if fSegmentBufferRange.Size>0 then begin
   OldCount:=length(fBufferPool.fSegments);
   NewCount:=fSegmentBufferRange.Offset+fSegmentBufferRange.Size;
   if OldCount<NewCount then begin
    inc(NewCount,NewCount);
    SetLength(fBufferPool.fSegments,NewCount);
    FillChar(fBufferPool.fSegments[OldCount],(NewCount-OldCount)*SizeOf(TpvVectorPathGPUSegmentData),#0);
   end;
   for Index:=0 to fSegments.Count-1 do begin
    Segment:=fSegments[Index];
    Segment.fIndex:=Index; // Ensure the segment index is correct
    SegmentData:=@fBufferPool.fSegments[fSegmentBufferRange.Offset+Index];
    case Segment.Type_ of
     TpvVectorPathSegmentType.Line:begin
      SegmentData^.Type_:=TpvVectorPathGPUSegmentData.TypeLine;
      SegmentData^.Winding:=0;
      SegmentData^.Point0:=TpvVectorPathSegmentLine(Segment).Points[0];
      SegmentData^.Point1:=TpvVectorPathSegmentLine(Segment).Points[1];
      SegmentData^.Point2:=TpvVector2.Null;
     end;
     TpvVectorPathSegmentType.QuadraticCurve:begin
      SegmentData^.Type_:=TpvVectorPathGPUSegmentData.TypeQuadraticCurve;
      SegmentData^.Winding:=0;
      SegmentData^.Point0:=TpvVectorPathSegmentQuadraticCurve(Segment).Points[0];
      SegmentData^.Point1:=TpvVectorPathSegmentQuadraticCurve(Segment).Points[1];
      SegmentData^.Point2:=TpvVectorPathSegmentQuadraticCurve(Segment).Points[2];
     end;
     TpvVectorPathSegmentType.MetaWindingSettingLine:begin
      SegmentData^.Type_:=TpvVectorPathGPUSegmentData.TypeMetaWindingSettingLine;
      SegmentData^.Winding:=TpvVectorPathSegmentMetaWindingSettingLine(Segment).Winding;
      SegmentData^.Point0:=TpvVectorPathSegmentMetaWindingSettingLine(Segment).Points[0];
      SegmentData^.Point1:=TpvVectorPathSegmentMetaWindingSettingLine(Segment).Points[1];
      SegmentData^.Point2:=TpvVector2.Null;
     end;
     else begin
      SegmentData^.Type_:=TpvVectorPathGPUSegmentData.TypeUnknown;
      SegmentData^.Winding:=0;
      SegmentData^.Point0:=TpvVector2.Null;
      SegmentData^.Point1:=TpvVector2.Null;
      SegmentData^.Point2:=TpvVector2.Null;
     end;
    end;
   end;
  end;

  // Update indirect segments
  //
  // Three kinds of repetition are folded away here, each one a step further than the one before.
  // Two cells that hold the same segments in the same order need only one range between them, and
  // a cell whose list is a contiguous run inside another cell's list can point into the middle of
  // that one, which is what Lengyel's notes suggest for the bands of Slug. The longest lists are
  // laid down first, so that the shorter ones have something to be found inside of.
  //
  // A hash leads the search and a hit is still compared entry by entry, so that a collision costs
  // a range rather than the wrong picture.
  SetLength(CellListOffsets,fGridCells.Count);
  SetLength(CellListSharedWith,fGridCells.Count);
  SetLength(CellOrder,fGridCells.Count);
  for Index:=0 to fGridCells.Count-1 do begin
   CellOrder[Index]:=Index;
   CellListSharedWith[Index]:=-1;
  end;
  if (fDuplicateSharing=TpvVectorPathDuplicateSharing.ContiguousRuns) and (fGridCells.Count>1) then begin
   // Longest first, since a run can only be found inside something at least as long. Counted
   // rather than compared, because the lengths are small numbers and there are many cells.
   LongestList:=0;
   for Index:=0 to fGridCells.Count-1 do begin
    LongestList:=Max(LongestList,fGridCells[Index].fSegments.Count);
   end;
   SetLength(LengthCounts,LongestList+2);
   for Index:=0 to LongestList+1 do begin
    LengthCounts[Index]:=0;
   end;
   for Index:=0 to fGridCells.Count-1 do begin
    inc(LengthCounts[fGridCells[Index].fSegments.Count]);
   end;
   // Running totals from the longest downwards, which is where each length starts in the order.
   OldCount:=0;
   for Index:=LongestList downto 0 do begin
    NewCount:=LengthCounts[Index];
    LengthCounts[Index]:=OldCount;
    inc(OldCount,NewCount);
   end;
   for Index:=0 to fGridCells.Count-1 do begin
    CellOrder[LengthCounts[fGridCells[Index].fSegments.Count]]:=Index;
    inc(LengthCounts[fGridCells[Index].fSegments.Count]);
   end;
  end;
  ListHashMap:=TpvVectorPathIndirectListHashMap.Create(TpvUInt64(-1));
  try
   NewCount:=0;
   for OrderIndex:=0 to fGridCells.Count-1 do begin
    Index:=CellOrder[OrderIndex];
    GridCell:=fGridCells[Index];
    if (fDuplicateSharing<>TpvVectorPathDuplicateSharing.None) and (GridCell.fSegments.Count>0) then begin
     ListHash:=TpvUInt64(1469598103934665603);
     for OldCount:=0 to GridCell.fSegments.Count-1 do begin
      ListHash:=(ListHash xor TpvUInt64(TpvUInt32(GridCell.fSegments[OldCount].fIndex)))*TpvUInt64(1099511628211);
     end;
     if ListHashMap.TryGet(ListHash,PackedWhere) then begin
      // The upper half says which cell holds the run and the lower half where in it it starts.
      OtherIndex:=TpvSizeInt(PackedWhere shr 32);
      RunStart:=TpvSizeInt(PackedWhere and $ffffffff);
      if (OtherIndex>=0) and
         (OtherIndex<fGridCells.Count) and
         ((RunStart+GridCell.fSegments.Count)<=fGridCells[OtherIndex].fSegments.Count) then begin
       SameList:=true;
       for OldCount:=0 to GridCell.fSegments.Count-1 do begin
        if fGridCells[OtherIndex].fSegments[RunStart+OldCount]<>GridCell.fSegments[OldCount] then begin
         SameList:=false;
         break;
        end;
       end;
       if SameList then begin
        CellListSharedWith[Index]:=OtherIndex;
        CellListOffsets[Index]:=CellListOffsets[OtherIndex]+RunStart;
        continue;
       end;
      end;
     end;
    end;
    CellListOffsets[Index]:=NewCount;
    inc(NewCount,GridCell.fSegments.Count);
    // Every contiguous run of what was just laid down becomes something a later, shorter list can
    // be found in. The first one to claim a hash keeps it, which is the longest run of it.
    case fDuplicateSharing of
     TpvVectorPathDuplicateSharing.EqualLists:begin
      // Only the list as a whole, which is what another cell holding the very same one finds.
      if GridCell.fSegments.Count>0 then begin
       ListHash:=TpvUInt64(1469598103934665603);
       for OldCount:=0 to GridCell.fSegments.Count-1 do begin
        ListHash:=(ListHash xor TpvUInt64(TpvUInt32(GridCell.fSegments[OldCount].fIndex)))*TpvUInt64(1099511628211);
       end;
       if not ListHashMap.TryGet(ListHash,PackedWhere) then begin
        ListHashMap.Add(ListHash,TpvUInt64(TpvUInt32(Index)) shl 32);
       end;
      end;
     end;
     TpvVectorPathDuplicateSharing.ContiguousRuns:begin
      // Every contiguous run of what was just laid down, so that a later and shorter list can be
      // found inside it. The first to claim a hash keeps it, which is the longest run of it.
      for OldCount:=0 to GridCell.fSegments.Count-1 do begin
       ListHash:=TpvUInt64(1469598103934665603);
       for RunStart:=OldCount to GridCell.fSegments.Count-1 do begin
        ListHash:=(ListHash xor TpvUInt64(TpvUInt32(GridCell.fSegments[RunStart].fIndex)))*TpvUInt64(1099511628211);
        if not ListHashMap.TryGet(ListHash,PackedWhere) then begin
         ListHashMap.Add(ListHash,(TpvUInt64(TpvUInt32(Index)) shl 32) or TpvUInt64(TpvUInt32(OldCount)));
        end;
       end;
      end;
     end;
     else begin
     end;
    end;
   end;
  finally
   FreeAndNil(ListHashMap);
  end;

  if fIndirectSegmentBufferRange.Size<NewCount then begin
   fBufferPool.fIndirectSegmentsAllocator.ReleaseBufferRangeAndNil(fIndirectSegmentBufferRange);
  end;
  if fIndirectSegmentBufferRange.Offset<0 then begin
   fIndirectSegmentBufferRange:=fBufferPool.fIndirectSegmentsAllocator.AllocateBufferRange(NewCount);
  end;
  if fIndirectSegmentBufferRange.Size>0 then begin
   OldCount:=length(fBufferPool.fIndirectSegments);
   NewCount:=fIndirectSegmentBufferRange.Offset+fIndirectSegmentBufferRange.Size;
   if OldCount<NewCount then begin
    inc(NewCount,NewCount);
    SetLength(fBufferPool.fIndirectSegments,NewCount);
    FillChar(fBufferPool.fIndirectSegments[OldCount],(NewCount-OldCount)*SizeOf(TpvVectorPathGPUIndirectSegmentData),#0);
   end;
   for Index:=0 to fGridCells.Count-1 do begin
    if CellListSharedWith[Index]>=0 then begin
     // Already written out by the cell this one was found inside of.
     continue;
    end;
    GridCell:=fGridCells[Index];
    for OldCount:=0 to GridCell.fSegments.Count-1 do begin
     Segment:=GridCell.fSegments[OldCount];
     fBufferPool.fIndirectSegments[(fIndirectSegmentBufferRange.Offset+CellListOffsets[Index])+OldCount]:=fSegmentBufferRange.Offset+Segment.fIndex;
    end;
   end;
  end;

  // Update grid cells
  if fGridCellBufferRange.Size<fGridCells.Count then begin
   fBufferPool.fGridCellsAllocator.ReleaseBufferRangeAndNil(fGridCellBufferRange);
  end;
  if fGridCellBufferRange.Offset<0 then begin
   fGridCellBufferRange:=fBufferPool.fGridCellsAllocator.AllocateBufferRange(fGridCells.Count);
  end;
  if fGridCellBufferRange.Size>0 then begin
   OldCount:=length(fBufferPool.fGridCells);
   NewCount:=fGridCellBufferRange.Offset+fGridCellBufferRange.Size;
   if OldCount<NewCount then begin
    inc(NewCount,NewCount);
    SetLength(fBufferPool.fGridCells,NewCount);
    FillChar(fBufferPool.fGridCells[OldCount],(NewCount-OldCount)*SizeOf(TpvVectorPathGPUGridCellData),#0);
   end;
   for Index:=0 to fGridCells.Count-1 do begin
    GridCell:=fGridCells[Index];
    GridCellData:=@fBufferPool.fGridCells[fGridCellBufferRange.Offset+Index];
    // The offset worked out above, which is the one of the cell this shares its list with
    // wherever it shares one. A shape whose cells are all empty gets no range at all, and the
    // offset of minus one that comes back from that would read as four billion rather than as
    // nothing, so it is written as a zero; the count of zero is what keeps the shader out of it.
    if fIndirectSegmentBufferRange.Offset>=0 then begin
     GridCellData^.StartIndirectSegmentIndex:=fIndirectSegmentBufferRange.Offset+CellListOffsets[Index];
    end else begin
     GridCellData^.StartIndirectSegmentIndex:=0;
    end;
    GridCellData^.CountIndirectSegments:=GridCell.fSegments.Count;
   end;
  end;

  // Update shape data
  OldCount:=length(fBufferPool.fShapes);
  if OldCount<=fShapeIndex then begin
   SetLength(fBufferPool.fShapes,(fShapeIndex+1)*2);
   FillChar(fBufferPool.fShapes[OldCount],(length(fBufferPool.fShapes)-OldCount)*SizeOf(TpvVectorPathGPUShapeData),#0);
  end;
  ShapeData:=@fBufferPool.fShapes[fShapeIndex];
  ShapeData^.Min:=fBoundingBox.Min;
  ShapeData^.Max:=fBoundingBox.Max;
  ShapeData^.Flags:=0;
  if fVectorPathShape.fFillRule=TpvVectorPathFillRule.EvenOdd then begin
   ShapeData^.Flags:=ShapeData^.Flags or 1;
  end;
  ShapeData^.StartGridCellIndex:=fGridCellBufferRange.Offset;
  if fAccelerationStructure=TpvVectorPathAccelerationStructure.SlugBands then begin
   // Bit 1 tells the shader that the cells are the two sets of bands and not a grid, with the
   // horizontal ones first and the vertical ones after them. The two counts go into the very same
   // fields the grid uses for its size per axis.
   ShapeData^.Flags:=ShapeData^.Flags or 2;
   ShapeData^.GridSizeX:=fCountVerticalBands;
   ShapeData^.GridSizeY:=fCountHorizontalBands;
  end else begin
   ShapeData^.GridSizeX:=fResolution;
   ShapeData^.GridSizeY:=fResolution;
  end;

 end;

end;

class function TpvVectorPathGPUShape.GetWindingChangeEvents(const aSegment:TpvVectorPathSegment;out aEvents:TWindingChangeEvents):TpvSizeInt;
const EPSILON=1e-8;
var P0,P1,P2,P3:TpvVectorPathVector;
    QuadA,QuadB,QuadC,QuadDisc,ParamT,FirstTurn,SecondTurn,SwapTurn:TpvDouble;
    TurnCount,RunIndex:TpvSizeInt;
    Turns:array[0..1] of TpvDouble;
    RunStart,RunEnd:TpvDouble;
    CountEvents:TpvSizeInt;
 // A run that only ever moves one way in the height counts for every height from its lower end up
 // to but not including its upper one, so it opens at the one and closes at the other. A level run
 // never crosses a scan line and so opens nothing.
 procedure AddRun(const aFromHeight,aUntilHeight:TpvDouble);
 var Direction:TpvInt32;
 begin
  if aUntilHeight>aFromHeight then begin
   Direction:=1;
  end else if aUntilHeight<aFromHeight then begin
   Direction:=-1;
  end else begin
   exit;
  end;
  aEvents[CountEvents].Y:=Min(aFromHeight,aUntilHeight);
  aEvents[CountEvents].Delta:=Direction;
  inc(CountEvents);
  aEvents[CountEvents].Y:=Max(aFromHeight,aUntilHeight);
  aEvents[CountEvents].Delta:=-Direction;
  inc(CountEvents);
 end;
 function QuadraticHeightAt(const aTime:TpvDouble):TpvDouble;
 begin
  result:=(P0.y*((1.0-aTime)*(1.0-aTime)))+
          ((P1.y*2.0)*(1.0-aTime)*aTime)+
          (P2.y*(aTime*aTime));
 end;
 function CubicHeightAt(const aTime:TpvDouble):TpvDouble;
 begin
  result:=(P0.y*(((1.0-aTime)*(1.0-aTime))*(1.0-aTime)))+
          (((P1.y*3.0)*((1.0-aTime)*(1.0-aTime)))*aTime)+
          (((P2.y*3.0)*(1.0-aTime))*(aTime*aTime))+
          (P3.y*((aTime*aTime)*aTime));
 end;
begin

 CountEvents:=0;

 case aSegment.Type_ of

  TpvVectorPathSegmentType.Line:begin

   // A line never turns around, so it is one run from end to end.

   P0:=TpvVectorPathSegmentLine(aSegment).Points[0];
   P1:=TpvVectorPathSegmentLine(aSegment).Points[1];

   AddRun(P0.y,P1.y);

  end;

  TpvVectorPathSegmentType.QuadraticCurve:begin

   P0:=TpvVectorPathSegmentQuadraticCurve(aSegment).Points[0];
   P1:=TpvVectorPathSegmentQuadraticCurve(aSegment).Points[1];
   P2:=TpvVectorPathSegmentQuadraticCurve(aSegment).Points[2];

   // Where the height stops rising and starts falling or the other way round, which is the one
   // place a quadratic curve can do so and therefore the one place it has to be cut.
   QuadA:=(P0.y-(2.0*P1.y))+P2.y;
   if abs(QuadA)>EPSILON then begin
    ParamT:=(P0.y-P1.y)/QuadA;
   end else begin
    ParamT:=-1.0;
   end;

   if (ParamT>0.0) and (ParamT<1.0) then begin
    AddRun(P0.y,QuadraticHeightAt(ParamT));
    AddRun(QuadraticHeightAt(ParamT),P2.y);
   end else begin
    AddRun(P0.y,P2.y);
   end;

  end;

  TpvVectorPathSegmentType.CubicCurve:begin

   P0:=TpvVectorPathSegmentCubicCurve(aSegment).Points[0];
   P1:=TpvVectorPathSegmentCubicCurve(aSegment).Points[1];
   P2:=TpvVectorPathSegmentCubicCurve(aSegment).Points[2];
   P3:=TpvVectorPathSegmentCubicCurve(aSegment).Points[3];

   // The same two places, which a cubic curve finds as the roots of its derivative.
   TurnCount:=0;
   QuadA:=(((-P0.y)+(3.0*P1.y))-(3.0*P2.y))+P3.y;
   QuadB:=((2.0*P0.y)-(4.0*P1.y))+(2.0*P2.y);
   QuadC:=P1.y-P0.y;
   if abs(QuadA)>EPSILON then begin
    QuadDisc:=(QuadB*QuadB)-((4.0*QuadA)*QuadC);
    if QuadDisc>=0.0 then begin
     QuadDisc:=sqrt(QuadDisc);
     for RunIndex:=0 to 1 do begin
      if RunIndex=0 then begin
       ParamT:=((-QuadB)-QuadDisc)/(2.0*QuadA);
      end else begin
       ParamT:=((-QuadB)+QuadDisc)/(2.0*QuadA);
      end;
      if (ParamT>0.0) and (ParamT<1.0) then begin
       Turns[TurnCount]:=ParamT;
       inc(TurnCount);
      end;
     end;
    end;
   end else if abs(QuadB)>EPSILON then begin
    ParamT:=(-QuadC)/QuadB;
    if (ParamT>0.0) and (ParamT<1.0) then begin
     Turns[TurnCount]:=ParamT;
     inc(TurnCount);
    end;
   end else begin
   end;

   // The runs have to come in order, since each one begins where the one before it ended.
   if TurnCount=2 then begin
    FirstTurn:=Turns[0];
    SecondTurn:=Turns[1];
    if FirstTurn>SecondTurn then begin
     SwapTurn:=FirstTurn;
     FirstTurn:=SecondTurn;
     SecondTurn:=SwapTurn;
    end;
    RunStart:=P0.y;
    RunEnd:=CubicHeightAt(FirstTurn);
    AddRun(RunStart,RunEnd);
    RunStart:=RunEnd;
    RunEnd:=CubicHeightAt(SecondTurn);
    AddRun(RunStart,RunEnd);
    AddRun(RunEnd,P3.y);
   end else if TurnCount=1 then begin
    RunEnd:=CubicHeightAt(Turns[0]);
    AddRun(P0.y,RunEnd);
    AddRun(RunEnd,P3.y);
   end else begin
    AddRun(P0.y,P3.y);
   end;

  end;

  else begin
   // A meta winding setting line is not an outline of its own, so it has nothing to contribute.
  end;

 end;

 result:=CountEvents;

end;

class function TpvVectorPathGPUShape.ComputeWindingAtPosition(const aSegments:TpvVectorPathSegments;const aX,aY:TpvDouble):TpvInt32;
// Computes the winding number at position (aX, aY) using ray casting algorithm.
// Cast a horizontal ray from (aX, aY) to the left (-infinity) and count intersections:
// - Increment winding for upward-crossing segments (dy > 0)
// - Decrement winding for downward-crossing segments (dy < 0)
// The winding number indicates how many times the path winds around the point.
// A crossing is taken half open in the height, so the lower end of a segment counts and the upper
// one does not. Where two segments meet, the shared point would otherwise be counted twice, which
// puts the winding of every point on that exact height out by one and shows up as single wrong
// scan lines. The winding code in canvas.frag follows the same rule, so that both sides agree.
const EPSILON=1e-8;
var SegmentIndex,RootIndex:TpvSizeInt;
    Segment:TpvVectorPathSegment;
    P0,P1,P2,P3:TpvVectorPathVector;
    LineDY,ParamT,ParamX:TpvDouble;
    QuadA,QuadB,QuadDisc:TpvDouble;
    ParamT0,ParamT1,ParamX0,ParamX1,QuadDY0,QuadDY1:TpvDouble;
    CubicCoef0,CubicCoef1,CubicCoef2,CubicCoef3:TpvVectorPathVector;
    CubicDY:TpvDouble;
    Roots:TpvDoubleDynamicArray;
 // Whether a crossing found at the given curve parameter, where the segment runs the given way in
 // the height, is one to count. Half open in the height and not in the parameter: a segment that
 // runs downwards reaches its lower end at a parameter of one rather than of zero, so going by the
 // parameter alone would count the lower end for one segment and the upper one for the other, and
 // the two sides of a shared point would disagree. That disagreement is also what the meta winding
 // setting lines cannot carry, since they hold one winding for a whole span of heights and would
 // then be wrong at the very height the span begins at.
 function CrossingCounts(const aParamT,aDeltaY:TpvDouble):boolean;
 begin
  if aDeltaY>0.0 then begin
   result:=(aParamT>=0.0) and (aParamT<1.0);
  end else if aDeltaY<0.0 then begin
   result:=(aParamT>0.0) and (aParamT<=1.0);
  end else begin
   result:=false;
  end;
 end;
begin

 result:=0;

 for SegmentIndex:=0 to aSegments.Count-1 do begin

  Segment:=aSegments[SegmentIndex];
  case Segment.Type_ of

   TpvVectorPathSegmentType.Line:begin

    // Linear segment: check if horizontal ray at aY intersects the line segment

    P0:=TpvVectorPathSegmentLine(Segment).Points[0];
    P1:=TpvVectorPathSegmentLine(Segment).Points[1];

    LineDY:=P1.y-P0.y;
    if abs(LineDY)>EPSILON then begin

     // Line is not horizontal, compute intersection parameter t
     ParamT:=(aY-P0.y)/LineDY;
     if CrossingCounts(ParamT,LineDY) then begin

      // Intersection is within line segment bounds
      ParamX:=P0.x+((P1.x-P0.x)*ParamT);
      if ParamX<=aX then begin

       // Intersection is to the left of test point
       if P1.y<P0.y then begin
        dec(result); // Downward crossing
       end else begin
        inc(result); // Upward crossing
       end;

      end;

     end;

    end;

   end;

   TpvVectorPathSegmentType.QuadraticCurve:begin

    // Quadratic Bézier curve: solve quadratic equation for y intersections

    P0:=TpvVectorPathSegmentQuadraticCurve(Segment).Points[0];
    P1:=TpvVectorPathSegmentQuadraticCurve(Segment).Points[1];
    P2:=TpvVectorPathSegmentQuadraticCurve(Segment).Points[2];

    // Quadratic equation: At² + Bt + C = 0, where curve(t).y = aY
    QuadA:=(P0.y-(2.0*P1.y))+P2.y;
    QuadB:=(-2.0*P0.y)+(2.0*P1.y);
    QuadDisc:=(QuadB*QuadB)-(4.0*QuadA*(P0.y-aY));

    if abs(QuadA)>EPSILON then begin

     if QuadDisc>0.0 then begin

      // Two real roots exist
      QuadDisc:=sqrt(QuadDisc);
      ParamT0:=(-QuadB-QuadDisc)/(2.0*QuadA);
      ParamT1:=(-QuadB+QuadDisc)/(2.0*QuadA);

      // Check first root. Which way the curve runs at it decides which of its two ends counts, so
      // the derivative comes before the test and not after it.
      QuadDY0:=((P1.y*(1.0-ParamT0))+(P2.y*ParamT0))-
               ((P0.y*(1.0-ParamT0))+(P1.y*ParamT0));
      if CrossingCounts(ParamT0,QuadDY0) then begin

       // Evaluate curve x-coordinate at this y-intersection
       ParamX0:=(P0.x*((1.0-ParamT0)*(1.0-ParamT0)))+
                (P1.x*2.0*(1.0-ParamT0)*ParamT0)+
                (P2.x*(ParamT0*ParamT0));
       if ParamX0<=aX then begin

        // Intersection is to the left
        if QuadDY0<0.0 then begin
         dec(result); // Downward crossing
        end else begin
         inc(result); // Upward crossing
        end;

       end;

      end;

      // Check second root
      QuadDY1:=((P1.y*(1.0-ParamT1))+(P2.y*ParamT1))-
               ((P0.y*(1.0-ParamT1))+(P1.y*ParamT1));
      if CrossingCounts(ParamT1,QuadDY1) then begin

       // Evaluate curve x-coordinate at this y-intersection
       ParamX1:=(P0.x*((1.0-ParamT1)*(1.0-ParamT1)))+
                (P1.x*2.0*(1.0-ParamT1)*ParamT1)+
                (P2.x*(ParamT1*ParamT1));
       if ParamX1<=aX then begin

        // Intersection is to the left
        if QuadDY1<0.0 then begin
         dec(result); // Downward crossing
        end else begin
         inc(result); // Upward crossing
        end;

       end;

      end;

     end;

    end else if abs(QuadB)>EPSILON then begin

     // Linear in y: single root t = (aY - P0.y) / QuadB
     ParamT:=(aY-P0.y)/QuadB;
     if CrossingCounts(ParamT,QuadB) then begin
      ParamX:=(P0.x*sqr(1.0-ParamT))+
              (P1.x*2.0*(1.0-ParamT)*ParamT)+
              (P2.x*sqr(ParamT));
      if ParamX<=aX then begin
       if QuadB<0.0 then begin
        dec(result);
       end else begin
        inc(result);
       end;
      end;
     end; 

    end else begin

     // Degenerate quadratic -> treat as line P0..P2
     LineDY:=P2.y-P0.y;
     if abs(LineDY)>EPSILON then begin
      ParamT:=(aY-P0.y)/LineDY;
      if CrossingCounts(ParamT,LineDY) then begin
       ParamX:=P0.x+((P2.x-P0.x)*ParamT);
       if ParamX<=aX then begin
        if P2.y<P0.y then begin
         dec(result);
        end else begin
         inc(result);
        end;
       end;
      end;
     end;

    end;

   end;

   TpvVectorPathSegmentType.CubicCurve:begin

    // Cubic Bézier curve: solve cubic equation for y intersections using polynomial root finding

    P0:=TpvVectorPathSegmentCubicCurve(Segment).Points[0];
    P1:=TpvVectorPathSegmentCubicCurve(Segment).Points[1];
    P2:=TpvVectorPathSegmentCubicCurve(Segment).Points[2];
    P3:=TpvVectorPathSegmentCubicCurve(Segment).Points[3];

    // Convert to polynomial form: cubic(t) = c0 + c1*t + c2*t² + c3*t³
    CubicCoef0:=P0;
    CubicCoef1:=(P0*(-3.0))+(P1*3.0);
    CubicCoef2:=(P0*3.0)+((P1*(-6.0))+(P2*3.0));
    CubicCoef3:=(P0*(-1.0))+((P1*3.0)+((P2*(-3.0))+P3));

    // Find all real roots where curve.y = aY, with the coefficients in ascending powers, which is
    // the order the polynomial keeps them in.
    Roots:=TpvPolynomial.Create([CubicCoef0.y-aY,CubicCoef1.y,CubicCoef2.y,CubicCoef3.y]).GetRoots;
    try

     for RootIndex:=0 to length(Roots)-1 do begin

      ParamT:=Roots[RootIndex];

      // Which way the curve runs at the root decides which of its two ends counts, so the
      // derivative (dy/dt) comes before the test and not after it.
      CubicDY:=(3.0*CubicCoef3.y*ParamT*ParamT)+(2.0*CubicCoef2.y*ParamT)+CubicCoef1.y;
      if CrossingCounts(ParamT,CubicDY) then begin

       // Root is within curve bounds, evaluate x-coordinate
       ParamX:=(((CubicCoef3.x*ParamT)+CubicCoef2.x)*ParamT+CubicCoef1.x)*ParamT+CubicCoef0.x;
       if ParamX<=aX then begin

        // Intersection is to the left
        if CubicDY<0.0 then begin
         dec(result); // Downward crossing
        end else begin
         inc(result); // Upward crossing
        end;

       end;

      end;

     end;

    finally
     Roots:=nil;
    end;

   end;

   else begin
   end;

  end;

 end;

end;

{ TpvVectorPathGPUBufferPool.TState } 

constructor TpvVectorPathGPUBufferPool.TState.Create(const aBufferPool:TpvVectorPathGPUBufferPool);
begin

 inherited Create;

 fBufferPool:=aBufferPool;

 fDevice:=fBufferPool.fDevice;

 fGeneration:=High(TpvUInt64);

 // Create buffers with initial size of few elements
 fSegmentsBuffer:=TpvVulkanBuffer.Create(fDevice,
                                         65536*SizeOf(TpvVectorPathGPUSegmentData),
                                         TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or
                                         TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or
                                         TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT),
                                         TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                         [],
                                         TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                         0,
                                         0,
                                         0,
                                         0,
                                         0,
                                         0,
                                         0,
                                         [],
                                         0,
                                         pvAllocationGroupIDCanvas,
                                         'VectorPathSegments');

 fIndirectSegmentsBuffer:=TpvVulkanBuffer.Create(fDevice,
                                                  65536*SizeOf(TpvVectorPathGPUIndirectSegmentData),
                                                  TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or
                                                  TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or
                                                  TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT),
                                                  TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                  [],
                                                  TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  0,
                                                  [],
                                                  0,
                                                  pvAllocationGroupIDCanvas,
                                                  'VectorPathIndirectSegments');

 fGridCellsBuffer:=TpvVulkanBuffer.Create(fDevice,
                                          65536*SizeOf(TpvVectorPathGPUGridCellData),
                                          TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or
                                          TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or
                                          TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT),
                                          TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                          [],
                                          TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                          0,
                                          0,
                                          0,
                                          0,
                                          0,
                                          0,
                                          0,
                                          [],
                                          0,
                                          pvAllocationGroupIDCanvas,
                                          'VectorPathGridCells');

 fShapesBuffer:=TpvVulkanBuffer.Create(fDevice,
                                       65536*SizeOf(TpvVectorPathGPUShapeData),
                                       TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or
                                       TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or
                                       TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT),
                                       TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                       [],
                                       TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                       0,
                                       0,
                                       0,
                                       0,
                                       0,
                                       0,
                                       0,
                                       [],
                                       0,
                                       pvAllocationGroupIDCanvas,
                                       'VectorPathShapes');

 fShapes:=nil;
 fShapeGenerations:=nil;

 // Create descriptor pool
 fDescriptorPool:=TpvVulkanDescriptorPool.Create(fDevice,
                                                 TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                 1); // Max sets
 fDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,4); // 4 SSBOs
 fDescriptorPool.Initialize;

 // Create descriptor set
 fDescriptorSet:=TpvVulkanDescriptorSet.Create(fDescriptorPool,fBufferPool.fDescriptorSetLayout);
 fDescriptorSet.WriteToDescriptorSet(0,
                                     0,
                                     1,
                                     TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                     [],
                                     [fSegmentsBuffer.DescriptorBufferInfo],
                                     [],
                                     false);
 fDescriptorSet.WriteToDescriptorSet(1,
                                     0,
                                     1,
                                     TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                     [],
                                     [fIndirectSegmentsBuffer.DescriptorBufferInfo],
                                     [],
                                     false);
 fDescriptorSet.WriteToDescriptorSet(2,
                                     0,
                                     1,
                                     TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                     [],
                                     [fGridCellsBuffer.DescriptorBufferInfo],
                                     [],
                                     false);
 fDescriptorSet.WriteToDescriptorSet(3,
                                     0,
                                     1,
                                     TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                     [],
                                     [fShapesBuffer.DescriptorBufferInfo],
                                     [],
                                     false);
 fDescriptorSet.Flush;

end;

destructor TpvVectorPathGPUBufferPool.TState.Destroy;
begin

 fShapes:=nil;
 fShapeGenerations:=nil;

 // Free Vulkan resources
 FreeAndNil(fDescriptorSet);
 FreeAndNil(fDescriptorPool);
 FreeAndNil(fShapesBuffer);
 FreeAndNil(fGridCellsBuffer);
 FreeAndNil(fIndirectSegmentsBuffer); 
 FreeAndNil(fSegmentsBuffer);

 inherited Destroy;

end;

procedure TpvVectorPathGPUBufferPool.TState.Update;
var ShapeIndex:TpvInt32;
    OldCount:TpvSizeInt;
    GPUShape:TpvVectorPathGPUShape;
    SegmentsMinIndex,SegmentsMaxIndex:TpvSizeInt;
    IndirectSegmentsMinIndex,IndirectSegmentsMaxIndex:TpvSizeInt;
    GridCellsMinIndex,GridCellsMaxIndex:TpvSizeInt;
    ShapesMinIndex,ShapesMaxIndex:TpvSizeInt;
    DescriptorSetDirty:boolean;
    Size:TVkDeviceSize;
begin

 if fGeneration<>fBufferPool.fGeneration then begin

  try

   SegmentsMinIndex:=$7fffffff;
   SegmentsMaxIndex:=0;

   IndirectSegmentsMinIndex:=$7fffffff;
   IndirectSegmentsMaxIndex:=0;

   GridCellsMinIndex:=$7fffffff;
   GridCellsMaxIndex:=0;

   ShapesMinIndex:=$7fffffff;
   ShapesMaxIndex:=0;

   DescriptorSetDirty:=false;

   for ShapeIndex:=0 to length(fBufferPool.fGPUShapes)-1 do begin

    GPUShape:=fBufferPool.fGPUShapes[ShapeIndex];
   
    if assigned(GPUShape) and 
       (((length(fShapes)<=ShapeIndex) or 
         ((ShapeIndex<length(fShapes)) and 
          (fShapes[ShapeIndex]<>GPUShape))) or
        ((length(fShapeGenerations)<=ShapeIndex) or 
         ((ShapeIndex<length(fShapeGenerations)) and 
          (fShapeGenerations[ShapeIndex]<>GPUShape.fGeneration)))) then begin

     // Track dirty ranges for all buffers affected by this shape
     if GPUShape.fSegmentBufferRange.Size>0 then begin
      if SegmentsMinIndex>GPUShape.fSegmentBufferRange.Offset then begin
       SegmentsMinIndex:=GPUShape.fSegmentBufferRange.Offset;
      end;
      if SegmentsMaxIndex<=(GPUShape.fSegmentBufferRange.Offset+GPUShape.fSegmentBufferRange.Size)-1 then begin
       SegmentsMaxIndex:=(GPUShape.fSegmentBufferRange.Offset+GPUShape.fSegmentBufferRange.Size)-1;
      end;
     end;
     
     if GPUShape.fIndirectSegmentBufferRange.Size>0 then begin
      if IndirectSegmentsMinIndex>GPUShape.fIndirectSegmentBufferRange.Offset then begin
       IndirectSegmentsMinIndex:=GPUShape.fIndirectSegmentBufferRange.Offset;
      end;
      if IndirectSegmentsMaxIndex<=(GPUShape.fIndirectSegmentBufferRange.Offset+GPUShape.fIndirectSegmentBufferRange.Size)-1 then begin
       IndirectSegmentsMaxIndex:=(GPUShape.fIndirectSegmentBufferRange.Offset+GPUShape.fIndirectSegmentBufferRange.Size)-1;
      end;
     end;
     
     if GPUShape.fGridCellBufferRange.Size>0 then begin
      if GridCellsMinIndex>GPUShape.fGridCellBufferRange.Offset then begin
       GridCellsMinIndex:=GPUShape.fGridCellBufferRange.Offset;
      end;
      if GridCellsMaxIndex<=(GPUShape.fGridCellBufferRange.Offset+GPUShape.fGridCellBufferRange.Size)-1 then begin
       GridCellsMaxIndex:=(GPUShape.fGridCellBufferRange.Offset+GPUShape.fGridCellBufferRange.Size)-1;
      end;
     end;
     
     if GPUShape.fShapeIndex>=0 then begin
      if ShapesMinIndex>GPUShape.fShapeIndex then begin
       ShapesMinIndex:=GPUShape.fShapeIndex;
      end;
      if ShapesMaxIndex<=GPUShape.fShapeIndex then begin
       ShapesMaxIndex:=GPUShape.fShapeIndex;
      end;
     end;

     // Update shape reference
     OldCount:=length(fShapes);
     if OldCount<=ShapeIndex then begin
      SetLength(fShapes,(ShapeIndex+1)*2);
      FillChar(fShapes[OldCount],(length(fShapes)-OldCount)*SizeOf(TpvVectorPathGPUShape),#0);
     end;
     fShapes[ShapeIndex]:=GPUShape;
     
     // Update shape generation     
     OldCount:=length(fShapeGenerations);
     if OldCount<=ShapeIndex then begin
      SetLength(fShapeGenerations,(ShapeIndex+1)*2);
      FillChar(fShapeGenerations[OldCount],(length(fShapeGenerations)-OldCount)*SizeOf(TpvUInt64),#0);
     end;
     fShapeGenerations[ShapeIndex]:=GPUShape.fGeneration;

    end;

   end;

   if SegmentsMinIndex<=SegmentsMaxIndex then begin
    Size:=length(fBufferPool.fSegments)*SizeOf(TpvVectorPathGPUSegmentData);
    if fSegmentsBuffer.Size<Size then begin
     FreeAndNil(fSegmentsBuffer);
     fSegmentsBuffer:=TpvVulkanBuffer.Create(fDevice,
                                             Size*2, // Allocate double size to avoid frequent reallocations
                                             TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or
                                             TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or
                                             TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT),
                                             TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                             [],
                                             TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                             0,
                                             0,
                                             0,
                                             0,
                                             0,
                                             0,
                                             0,
                                             [],
                                             0,
                                             pvAllocationGroupIDCanvas,
                                             'VectorPathSegments');
     fDescriptorSet.WriteToDescriptorSet(0,
                                         0,
                                         1,
                                         TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                         [],
                                         [fSegmentsBuffer.DescriptorBufferInfo],
                                         [],
                                         false);
     DescriptorSetDirty:=true;
     SegmentsMinIndex:=0;
     SegmentsMaxIndex:=length(fBufferPool.fSegments)-1;
    end;
    fSegmentsBuffer.UploadData(fBufferPool.fTransferQueue,
                               fBufferPool.fTransferCommandBuffer,
                               fBufferPool.fTransferFence,
                               fBufferPool.fSegments[SegmentsMinIndex],
                               SegmentsMinIndex*SizeOf(TpvVectorPathGPUSegmentData),
                               ((SegmentsMaxIndex-SegmentsMinIndex)+1)*SizeOf(TpvVectorPathGPUSegmentData),
                               TpvVulkanBufferUseTemporaryStagingBufferMode.Automatic,
                               false);
   end;

   if IndirectSegmentsMinIndex<=IndirectSegmentsMaxIndex then begin
    Size:=length(fBufferPool.fIndirectSegments)*SizeOf(TpvVectorPathGPUIndirectSegmentData);
    if fIndirectSegmentsBuffer.Size<Size then begin
     FreeAndNil(fIndirectSegmentsBuffer);
     fIndirectSegmentsBuffer:=TpvVulkanBuffer.Create(fDevice,
                                                     Size*2, // Allocate double size to avoid frequent reallocations
                                                     TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or
                                                     TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or
                                                     TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT),
                                                     TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                     [],
                                                     TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     0,
                                                     [],
                                                     0,
                                                     pvAllocationGroupIDCanvas,
                                                     'VectorPathIndirectSegments');
     fDescriptorSet.WriteToDescriptorSet(1,
                                         0,
                                         1,
                                         TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                         [],
                                         [fIndirectSegmentsBuffer.DescriptorBufferInfo],
                                         [],
                                         false);
     DescriptorSetDirty:=true;
     IndirectSegmentsMinIndex:=0;
     IndirectSegmentsMaxIndex:=length(fBufferPool.fIndirectSegments)-1;
    end;
    fIndirectSegmentsBuffer.UploadData(fBufferPool.fTransferQueue,
                                       fBufferPool.fTransferCommandBuffer,
                                       fBufferPool.fTransferFence,
                                       fBufferPool.fIndirectSegments[IndirectSegmentsMinIndex],
                                       IndirectSegmentsMinIndex*SizeOf(TpvVectorPathGPUIndirectSegmentData),
                                       ((IndirectSegmentsMaxIndex-IndirectSegmentsMinIndex)+1)*SizeOf(TpvVectorPathGPUIndirectSegmentData),
                                       TpvVulkanBufferUseTemporaryStagingBufferMode.Automatic,
                                       false);
   end;

   if GridCellsMinIndex<=GridCellsMaxIndex then begin
    Size:=length(fBufferPool.fGridCells)*SizeOf(TpvVectorPathGPUGridCellData);
    if fGridCellsBuffer.Size<Size then begin
     FreeAndNil(fGridCellsBuffer);
     fGridCellsBuffer:=TpvVulkanBuffer.Create(fDevice,
                                              Size*2, // Allocate double size to avoid frequent reallocations
                                              TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or
                                              TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or
                                              TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT),
                                              TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                              [],
                                              TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                              0,
                                              0,
                                              0,
                                              0,
                                              0,
                                              0,
                                              0,
                                              [],
                                              0,
                                              pvAllocationGroupIDCanvas,
                                              'VectorPathGridCells');
     fDescriptorSet.WriteToDescriptorSet(2,
                                         0,
                                         1,
                                         TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                         [],
                                         [fGridCellsBuffer.DescriptorBufferInfo],
                                         [],
                                         false);
     DescriptorSetDirty:=true;
     GridCellsMinIndex:=0;
     GridCellsMaxIndex:=length(fBufferPool.fGridCells)-1;
    end;
    fGridCellsBuffer.UploadData(fBufferPool.fTransferQueue,
                                fBufferPool.fTransferCommandBuffer,
                                fBufferPool.fTransferFence,
                                fBufferPool.fGridCells[GridCellsMinIndex],
                                GridCellsMinIndex*SizeOf(TpvVectorPathGPUGridCellData),
                                ((GridCellsMaxIndex-GridCellsMinIndex)+1)*SizeOf(TpvVectorPathGPUGridCellData),
                                TpvVulkanBufferUseTemporaryStagingBufferMode.Automatic,
                                false);
   end;

   if ShapesMinIndex<=ShapesMaxIndex then begin
    Size:=length(fBufferPool.fShapes)*SizeOf(TpvVectorPathGPUShapeData);
    if fShapesBuffer.Size<Size then begin
     FreeAndNil(fShapesBuffer);
     fShapesBuffer:=TpvVulkanBuffer.Create(fDevice,
                                           Size*2, // Allocate double size to avoid frequent reallocations
                                           TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_BUFFER_BIT) or
                                           TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_SRC_BIT) or
                                           TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT),
                                           TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                           [],
                                           TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                           0,
                                           0,
                                           0,
                                           0,
                                           0,
                                           0,
                                           0,
                                           [],
                                           0,
                                           pvAllocationGroupIDCanvas,
                                           'VectorPathShapes');
     fDescriptorSet.WriteToDescriptorSet(3,
                                         0,
                                         1,
                                         TVkDescriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
                                         [],
                                         [fShapesBuffer.DescriptorBufferInfo],
                                         [],
                                         false);
     DescriptorSetDirty:=true;
     ShapesMinIndex:=0;
     ShapesMaxIndex:=length(fBufferPool.fShapes)-1;
    end;
    fShapesBuffer.UploadData(fBufferPool.fTransferQueue,
                             fBufferPool.fTransferCommandBuffer,
                             fBufferPool.fTransferFence,
                             fBufferPool.fShapes[ShapesMinIndex],
                             ShapesMinIndex*SizeOf(TpvVectorPathGPUShapeData),
                             ((ShapesMaxIndex-ShapesMinIndex)+1)*SizeOf(TpvVectorPathGPUShapeData),
                             TpvVulkanBufferUseTemporaryStagingBufferMode.Automatic,
                             false);
   end;

   if DescriptorSetDirty then begin
    fDescriptorSet.Flush;
   end;

  finally
   fGeneration:=fBufferPool.fGeneration;
  end;

 end;

end;

{ TpvVectorPathGPUBufferPool }

constructor TpvVectorPathGPUBufferPool.Create(const aDevice:TpvVulkanDevice);
var Index:TpvSizeInt;
begin

 inherited Create;

 fDevice:=aDevice;

 fGPUShapes:=nil;

 fShapeIndexHashMap:=TpvVectorPathIndexHashMap.Create(-1);

 fGeneration:=0;

 // Create descriptor set layout
 fDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(fDevice);
 fDescriptorSetLayout.AddBinding(0,
                                 VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                 1,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                 []); // Segments SSBO
 fDescriptorSetLayout.AddBinding(1,
                                 VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                 1,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                 []); // Indirect segments SSBO
 fDescriptorSetLayout.AddBinding(2,
                                 VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                 1,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                 []); // Grid cells SSBO
 fDescriptorSetLayout.AddBinding(3,
                                 VK_DESCRIPTOR_TYPE_STORAGE_BUFFER,
                                 1,
                                 TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                 []); // Shapes metadata SSBO
 fDescriptorSetLayout.Initialize;

 for Index:=0 to 1 do begin
  fStates[Index]:=TState.Create(self);
 end;
 fActiveStateIndex:=0;

 fSegments:=nil;
 fSegmentsAllocator:=TpvBufferRangeAllocator.Create(0);

 fIndirectSegments:=nil;
 fIndirectSegmentsAllocator:=TpvBufferRangeAllocator.Create(0);

 fGridCells:=nil;
 fGridCellsAllocator:=TpvBufferRangeAllocator.Create(0);

 fShapes:=nil;
 fFreeShapeIndices.Initialize;
 fShapeIndexCounter:=0;

 fTransferQueue:=fDevice.TransferQueue;

 fTransferCommandPool:=TpvVulkanCommandPool.Create(fDevice,fTransferQueue.QueueFamilyIndex,TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

 fTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(fTransferCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fTransferFence:=TpvVulkanFence.Create(fDevice);

 fFragmentationFactorThreshold:=0.75;

 fPasMPInstance:=nil;

end;

destructor TpvVectorPathGPUBufferPool.Destroy;
var Index:TpvSizeInt;
begin

 for Index:=0 to 1 do begin
  FreeAndNil(fStates[Index]);
 end;

 FreeAndNil(fTransferFence);
 FreeAndNil(fTransferCommandBuffer);
 FreeAndNil(fTransferCommandPool);
 fTransferQueue:=nil;

 FreeAndNil(fDescriptorSetLayout);

 fSegments:=nil;
 FreeAndNil(fSegmentsAllocator);

 fIndirectSegments:=nil;
 FreeAndNil(fIndirectSegmentsAllocator);

 fGridCells:=nil;
 FreeAndNil(fGridCellsAllocator);

 fShapes:=nil;
 fFreeShapeIndices.Finalize;

 FreeAndNil(fShapeIndexHashMap);

 fGPUShapes:=nil;

 inherited Destroy;
end;

procedure TpvVectorPathGPUBufferPool.Update;
var CurrentStateIndex,NextStateIndex:TPasMPInt32;
    CurrentState:TpvVectorPathGPUBufferPool.TState;
begin
 CurrentStateIndex:=TPasMPInterlocked.Read(fActiveStateIndex);
 CurrentState:=fStates[CurrentStateIndex and 1];
 if CurrentState.fGeneration<>fGeneration then begin
  NextStateIndex:=(CurrentStateIndex+1) and 1;
  fStates[NextStateIndex].Update;
  TPasMPInterlocked.Write(fActiveStateIndex,NextStateIndex);
 end;
end;

function TpvVectorPathGPUBufferPool.GetActiveState:TpvVectorPathGPUBufferPool.TState;
begin
 result:=fStates[fActiveStateIndex and 1];
end;

function TpvVectorPathGPUBufferPool.GetDescriptorSet:TpvVulkanDescriptorSet;
begin
 result:=GetActiveState.fDescriptorSet;
end;

function TpvVectorPathGPUBufferPool.GetShapeOf(const aShape:TpvVectorPathShape):TpvVectorPathGPUShape;
var ShapeIndex:TpvInt32;
begin
 if fShapeIndexHashMap.TryGet(aShape,ShapeIndex) then begin
  result:=fGPUShapes[ShapeIndex];
 end else begin
  result:=nil;
 end;
end;

function TpvVectorPathGPUBufferPool.GetOrCreateShape(const aShape:TpvVectorPathShape;const aStreamIO:TpvStreamIO):TpvVectorPathGPUShape;
var ShapeIndex,OldCount:TpvInt32;
begin
 if fShapeIndexHashMap.TryGet(aShape,ShapeIndex) then begin
  result:=fGPUShapes[ShapeIndex];
 end else begin
  if not fFreeShapeIndices.Pop(ShapeIndex) then begin
   ShapeIndex:=fShapeIndexCounter;
   inc(fShapeIndexCounter);
  end;
  if ShapeIndex>=0 then begin
   OldCount:=length(fGPUShapes);
   if OldCount<=ShapeIndex then begin
    SetLength(fGPUShapes,(ShapeIndex+1)*2);
    FillChar(fGPUShapes[OldCount],(length(fGPUShapes)-OldCount)*SizeOf(TpvVectorPathGPUShape),#0);
   end;
   result:=TpvVectorPathGPUShape.Create(self,aShape,aStreamIO,fPasMPInstance);
   try
    result.fShapeIndex:=ShapeIndex;
    result.UpdateBufferPool;
    result.fGeneration:=TPasMPInterlocked.Increment(fGeneration);
    fShapeIndexHashMap.Add(aShape,ShapeIndex);
   finally
    fGPUShapes[ShapeIndex]:=result;
   end;
  end else begin
   result:=nil;
  end;
 end;
end;

function TpvVectorPathGPUBufferPool.GetOrCreateShape(const aShape:TpvVectorPathShape):TpvVectorPathGPUShape;
var ShapeIndex,OldCount:TpvInt32;
begin
 if fShapeIndexHashMap.TryGet(aShape,ShapeIndex) then begin
  result:=fGPUShapes[ShapeIndex];
 end else begin
  if not fFreeShapeIndices.Pop(ShapeIndex) then begin
   ShapeIndex:=fShapeIndexCounter;
   inc(fShapeIndexCounter);
  end;
  if ShapeIndex>=0 then begin
   OldCount:=length(fGPUShapes);
   if OldCount<=ShapeIndex then begin
    SetLength(fGPUShapes,(ShapeIndex+1)*2);
    FillChar(fGPUShapes[OldCount],(length(fGPUShapes)-OldCount)*SizeOf(TpvVectorPathGPUShape),#0);
   end;
   result:=TpvVectorPathGPUShape.Create(self,aShape,aShape.fResolution,aShape.fBoundingBoxExtent,fPasMPInstance,aShape.fAccelerationStructure,aShape.fCountBands,aShape.fDuplicateSharing);
   try
    result.fShapeIndex:=ShapeIndex;
    result.UpdateBufferPool;
    result.fGeneration:=TPasMPInterlocked.Increment(fGeneration);
    fShapeIndexHashMap.Add(aShape,ShapeIndex);
   finally
    fGPUShapes[ShapeIndex]:=result;
   end;
  end else begin
   result:=nil;
  end;
 end;
end;

procedure TpvVectorPathGPUBufferPool.RemoveShape(const aShape:TpvVectorPathShape);
var ShapeIndex:TpvInt32;
    GPUShape:TpvVectorPathGPUShape; 
begin
 if fShapeIndexHashMap.TryGet(aShape,ShapeIndex) and (ShapeIndex>=0) and (ShapeIndex<length(fGPUShapes)) then begin
  GPUShape:=fGPUShapes[ShapeIndex];
  if assigned(GPUShape) then begin
   try
    FreeAndNil(fGPUShapes[ShapeIndex]);
   finally
    try
     fShapeIndexHashMap.Delete(aShape);
    finally
     fFreeShapeIndices.Push(ShapeIndex);
    end;
   end;
  end;
 end;
end;

procedure TpvVectorPathGPUBufferPool.UpdateShape(const aShape:TpvVectorPathShape);
var ShapeIndex:TpvInt32;
    GPUShape:TpvVectorPathGPUShape;
begin
 if fShapeIndexHashMap.TryGet(aShape,ShapeIndex) and (ShapeIndex>=0) and (ShapeIndex<length(fGPUShapes)) then begin
  GPUShape:=fGPUShapes[ShapeIndex];
  if assigned(GPUShape) then begin
   GPUShape.Update;
   GPUShape.UpdateBufferPool;
   GPUShape.fGeneration:=TPasMPInterlocked.Increment(fGeneration);
  end;
 end;
end;

function TpvVectorPathGPUBufferPool.CalculateFragmentationFactor:TpvDouble;
begin
 result:=Max(fSegmentsAllocator.CalculateFragmentationFactor,
             Max(fIndirectSegmentsAllocator.CalculateFragmentationFactor,
                 fGridCellsAllocator.CalculateFragmentationFactor)); 
end;

function TpvVectorPathGPUBufferPool.VerifyUploadedBuffers(out aReport:TpvUTF8String;const aMaximalReportsPerBuffer:TpvSizeInt):TpvSizeInt;
var State:TState;
    CountMismatches,CountOutOfRange,Index:TpvSizeInt;
    Reach,FurthestReach:TpvUInt32;
 // Reads one buffer back and walks it against the array it was filled from. Only as many entries
 // as the array holds are looked at, since whatever sits past them was never written.
 procedure CheckBuffer(const aName:TpvUTF8String;
                       const aBuffer:TpvVulkanBuffer;
                       const aSource:TpvPointer;
                       const aCountEntries:TpvSizeInt;
                       const aEntrySize:TpvSizeInt);
 var ReadBack:array of TpvUInt8;
     Size:TVkDeviceSize;
     Index,CountReports:TpvSizeInt;
     SourceEntry,ReadBackEntry:PpvUInt8;
 begin

  if (not assigned(aBuffer)) or (aCountEntries<1) then begin
   aReport:=aReport+aName+': nothing to check'+#13#10;
   exit;
  end;

  Size:=TVkDeviceSize(aCountEntries)*TVkDeviceSize(aEntrySize);
  if aBuffer.Size<Size then begin
   aReport:=aReport+aName+': the buffer holds '+IntToStr(aBuffer.Size)+' bytes but '+
            IntToStr(Size)+' were written, so '+IntToStr((Size-aBuffer.Size) div aEntrySize)+
            ' entries never made it to the device'+#13#10;
   inc(CountMismatches,(Size-aBuffer.Size) div aEntrySize);
   Size:=aBuffer.Size;
  end;

  ReadBack:=nil;
  try

   SetLength(ReadBack,Size);
   aBuffer.DownloadData(fTransferQueue,fTransferCommandBuffer,fTransferFence,ReadBack[0],0,Size);

   CountReports:=0;
   for Index:=0 to (Size div TVkDeviceSize(aEntrySize))-1 do begin
    SourceEntry:=PpvUInt8(TpvPointer(TpvPtrUInt(aSource)+TpvPtrUInt(Index*aEntrySize)));
    ReadBackEntry:=@ReadBack[Index*aEntrySize];
    if not CompareMem(SourceEntry,ReadBackEntry,aEntrySize) then begin
     inc(CountMismatches);
     if CountReports<aMaximalReportsPerBuffer then begin
      inc(CountReports);
      aReport:=aReport+aName+': entry '+IntToStr(Index)+' differs'+#13#10;
     end;
    end;
   end;

   if CountReports=0 then begin
    aReport:=aReport+aName+': all '+IntToStr(Size div TVkDeviceSize(aEntrySize))+' entries agree'+#13#10;
   end;

  finally
   ReadBack:=nil;
  end;

 end;
begin

 aReport:='';
 CountMismatches:=0;

 State:=GetActiveState;
 if not assigned(State) then begin
  aReport:='no state to check'+#13#10;
  result:=0;
  exit;
 end;

 if length(fSegments)>0 then begin
  CheckBuffer('segments',State.fSegmentsBuffer,@fSegments[0],length(fSegments),SizeOf(TpvVectorPathGPUSegmentData));
 end;
 if length(fIndirectSegments)>0 then begin
  CheckBuffer('indirect segments',State.fIndirectSegmentsBuffer,@fIndirectSegments[0],length(fIndirectSegments),SizeOf(TpvVectorPathGPUIndirectSegmentData));
 end;
 if length(fGridCells)>0 then begin
  CheckBuffer('grid cells',State.fGridCellsBuffer,@fGridCells[0],length(fGridCells),SizeOf(TpvVectorPathGPUGridCellData));
 end;
 if length(fShapes)>0 then begin
  CheckBuffer('shapes',State.fShapesBuffer,@fShapes[0],length(fShapes),SizeOf(TpvVectorPathGPUShapeData));
 end;

 // What the grid cells say they point at, against what there is room for. A cell that reaches past
 // the end of the indirect segment array reads whatever happens to sit there, which is the one
 // thing a comparison of the arrays themselves cannot show.
 begin
  CountOutOfRange:=0;
  FurthestReach:=0;
  for Index:=0 to length(fGridCells)-1 do begin
   Reach:=fGridCells[Index].StartIndirectSegmentIndex+fGridCells[Index].CountIndirectSegments;
   if Reach>FurthestReach then begin
    FurthestReach:=Reach;
   end;
   if (fGridCells[Index].CountIndirectSegments>0) and (Reach>length(fIndirectSegments)) then begin
    inc(CountOutOfRange);
   end;
  end;
  aReport:=aReport+'grid cells reach up to indirect segment '+IntToStr(FurthestReach)+
           ' and there are '+IntToStr(length(fIndirectSegments))+' of them'+#13#10;
  if CountOutOfRange>0 then begin
   aReport:=aReport+IntToStr(CountOutOfRange)+' grid cells point past the end of the indirect segments'+#13#10;
   inc(CountMismatches,CountOutOfRange);
  end;
 end;

 result:=CountMismatches;

end;

procedure TpvVectorPathGPUBufferPool.Defragment(const aForce:boolean);
var Index:TpvSizeInt;
    GPUShape:TpvVectorPathGPUShape;
    Generation:TPasMPUInt64;
begin
 
 if aForce or (CalculateFragmentationFactor>fFragmentationFactorThreshold) then begin

  // The TpvBufferRangeAllocator own defragmentation isn't used here because
  // all shapes depend on multiple buffer ranges and it would be quite complex
  // to update all shapes correctly while trying just moving buffer ranges
  // around. So just use new buffer ranges in a single operation is simpler,
  // where all buffer ranges are cleared and invalidated first and then 
  // reallocated by updating each shape. This way fragmentation is avoided
  // completely instead of trying to fix it partially. 

  // Increment generation to force buffer updates
  Generation:=TPasMPInterlocked.Increment(fGeneration);

  // Pass 1: Clear and invalidate all buffer ranges
  for Index:=0 to length(fGPUShapes)-1 do begin
   GPUShape:=fGPUShapes[Index];
   if assigned(GPUShape) then begin
    GPUShape.ClearAndInvalidateBufferRanges;
   end; 
  end;

  // Pass 2: Reallocate all buffer ranges by re-updating each shape
  for Index:=0 to length(fGPUShapes)-1 do begin
   GPUShape:=fGPUShapes[Index];
   if assigned(GPUShape) then begin
    GPUShape.UpdateBufferPool;
    GPUShape.fGeneration:=Generation; // Set generation to the current generation
   end; 
  end; 

 end; 

end;

end.
