(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2020, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.Scene3D.Renderer.CameraPreset;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$m+}

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.VirtualReality,
     PasVulkan.VirtualFileSystem;

type { TpvScene3DRendererCameraPreset }
     TpvScene3DRendererCameraPreset=class
      public
       type TShaderData=packed record
             SensorSize:TpvVector2;
             FocalLength:TpvFloat;
             FlangeFocalDistance:TpvFloat;
             FocalPlaneDistance:TpvFloat;
             FNumber:TpvFloat;
             FNumberMin:TpvFloat;
             FNumberMax:TpvFloat;
             Ngon:TpvFloat;
             HighlightThreshold:TpvFloat;
             HighlightGain:TpvFloat;
             BokehChromaticAberration:TpvFloat;
            end;
            PShaderData=^TShaderData;
      private
       fSensorSize:TpvVector2;
       fSensorSizeProperty:TpvVector2Property;
       fFocalLength:TpvFloat;
       fFlangeFocalDistance:TpvFloat;
       fFocalPlaneDistance:TpvFloat;
       fFNumber:TpvFloat;
       fFNumberMin:TpvFloat;
       fFNumberMax:TpvFloat;
       fBlurKernelSize:TpvInt32;
       fNgon:TpvFloat;
       fMaxCoC:TpvFloat;
       fHighlightThreshold:TpvFloat;
       fHighlightGain:TpvFloat;
       fBokehChromaticAberration:TpvFloat;
       fAutoFocus:boolean;
       function GetFieldOfViewAngleRadians:TpvFloat;
       function GetAspectRatio:TpvFloat;
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
       procedure Assign(const aFrom:TpvScene3DRendererCameraPreset);
      published

       // Sensor size at digital cameras in mm (or film size at analog cameras)
       property SensorSize:TpvVector2Property read fSensorSizeProperty;

       // Focal length in mm
       property FocalLength:TpvFloat read fFocalLength write fFocalLength;

       // Flange focal distance in mm (distance from lens to sensor)
       property FlangeFocalDistance:TpvFloat read fFlangeFocalDistance write fFlangeFocalDistance;

       // Focal plane distance in mm (distance from lens to focused plane world point)
       property FocalPlaneDistance:TpvFloat read fFocalPlaneDistance write fFocalPlaneDistance;

       // f-number (f/n)
       property FNumber:TpvFloat read fFNumber write fFNumber;

       // minimum f-number
       property FNumberMin:TpvFloat read fFNumberMin write fFNumberMin;

       // maximum f-number
       property FNumberMax:TpvFloat read fFNumberMax write fFNumberMax;

       // ngon
       property Ngon:TpvFloat read fNgon write fNgon;

       // Blur kernel size
       property BlurKernelSize:TpvInt32 read fBlurKernelSize write fBlurKernelSize;

       // maximum CoC radius
       property MaxCoC:TpvFloat read fMaxCoC write fMaxCoC;

       // Highlight threshold
       property HighlightThreshold:TpvFloat read fHighlightThreshold write fHighlightThreshold;

       // Highlight gain
       property HighlightGain:TpvFloat read fHighlightGain write fHighlightGain;

       // Bokeh chromatic aberration/fringing
       property BokehChromaticAberration:TpvFloat read fBokehChromaticAberration write fBokehChromaticAberration;

       // Angle of field of view in radians
       property FieldOfViewAngleRadians:TpvFloat read GetFieldOfViewAngleRadians;

       // Aspect ratio
       property AspectRatio:TpvFloat read GetAspectRatio;

       // AutoFocus
       property AutoFocus:boolean read fAutoFocus write fAutoFocus;

    end;

implementation

{ TpvScene3DRendererCameraPreset }

constructor TpvScene3DRendererCameraPreset.Create;
begin
 inherited Create;
 fSensorSize:=TpvVector2.Create(36.0,24.0); // 36 x 24 mm
 fSensorSizeProperty:=TpvVector2Property.Create(@fSensorSize);
 fFocalLength:=50.0;
 fFlangeFocalDistance:=100.0;
 fFocalPlaneDistance:=4000.0;
 fFNumber:=16.0;
 fFNumberMin:=1.0;
 fFNumberMax:=16.0;
 fNgon:=6;
 fBlurKernelSize:=8;
 fMaxCoC:=0.05;
 fHighlightThreshold:=0.25;
 fHighlightGain:=1.0;
 fBokehChromaticAberration:=0.7;
 fAutoFocus:=true;
end;

destructor TpvScene3DRendererCameraPreset.Destroy;
begin
 FreeAndNil(fSensorSizeProperty);
 inherited Destroy;
end;

procedure TpvScene3DRendererCameraPreset.Assign(const aFrom:TpvScene3DRendererCameraPreset);
begin
 fSensorSize:=aFrom.fSensorSize;
 fFocalLength:=aFrom.fFocalLength;
 fFlangeFocalDistance:=aFrom.fFlangeFocalDistance;
 fFocalPlaneDistance:=aFrom.fFocalPlaneDistance;
 fFNumber:=aFrom.fFNumber;
 fFNumberMin:=aFrom.fFNumberMin;
 fFNumberMax:=aFrom.fFNumberMax;
 fBlurKernelSize:=aFrom.fBlurKernelSize;
 fNgon:=aFrom.fNgon;
 fMaxCoC:=aFrom.fMaxCoC;
 fHighlightThreshold:=aFrom.fHighlightThreshold;
 fHighlightGain:=aFrom.fHighlightGain;
 fBokehChromaticAberration:=aFrom.fBokehChromaticAberration;
 fAutoFocus:=aFrom.fAutoFocus;
end;

function TpvScene3DRendererCameraPreset.GetFieldOfViewAngleRadians:TpvFloat;
begin
 result:=2.0*ArcTan((fSensorSize.x*(fFocalPlaneDistance-fFocalLength))/(2.0*fFocalPlaneDistance*fFocalLength));
end;

function TpvScene3DRendererCameraPreset.GetAspectRatio:TpvFloat;
begin
 result:=fSensorSize.x/fSensorSize.y;
end;

initialization
finalization
end.
