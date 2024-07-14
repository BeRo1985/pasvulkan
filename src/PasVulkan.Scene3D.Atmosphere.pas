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
unit PasVulkan.Scene3D.Atmosphere;
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
     PasMP,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Collections,
     PasVulkan.Scene3D.Renderer.Array2DImage;

type TpvScene3DAtmosphere=class;

     { TpvScene3DAtmospheres }
     TpvScene3DAtmospheres=class(TpvObjectGenericList<TpvScene3DAtmosphere>)
      private
       fScene3D:TObject;
       fLock:TPasMPMultipleReaderSingleWriterLock;
      public
       constructor Create(const aScene3D:TObject); reintroduce;
       destructor Destroy; override;
       procedure ProcessReleases;
      published
       property Scene3D:TObject read fScene3D;
       property Lock:TPasMPMultipleReaderSingleWriterLock read fLock;
     end;

     { TpvScene3DAtmosphere } 
     TpvScene3DAtmosphere=class
      public
       const TRANSMITTANCE_TEXTURE_WIDTH=256;
             TRANSMITTANCE_TEXTURE_HEIGHT=64;
             SCATTERING_TEXTURE_R_SIZE=32;
             SCATTERING_TEXTURE_MU_SIZE=128;
             SCATTERING_TEXTURE_MU_S_SIZE=32;
             SCATTERING_TEXTURE_NU_SIZE=8;
             SCATTERING_TEXTURE_WIDTH=SCATTERING_TEXTURE_NU_SIZE*SCATTERING_TEXTURE_MU_S_SIZE;
             SCATTERING_TEXTURE_HEIGHT=SCATTERING_TEXTURE_MU_SIZE;
             SCATTERING_TEXTURE_DEPTH=SCATTERING_TEXTURE_R_SIZE;
             IRRADIANCE_TEXTURE_WIDTH=64;
             IRRADIANCE_TEXTURE_HEIGHT=16;
             MultiScatteringLUTRes=32;
             ScatteringOrders=4; 
       type { TDensityProfileLayer }
            TDensityProfileLayer=packed record
             public
              Width:TpvFloat;
              ExpTerm:TpvFloat;
              ExpScale:TpvFloat;
              LinearTerm:TpvFloat;
              ConstantTerm:TpvFloat;
              Unused0:TpvFloat;
              Unused1:TpvFloat;
              Unused2:TpvFloat;
              constructor Create(const aWidth,aExpTerm,aExpScale,aLinearTerm,aConstantTerm:TpvFloat);
            end;
            PDensityProfileLayer=^TDensityProfileLayer;
            { TDensityProfile }
            TDensityProfile=packed record
             public
              Layers:array[0..1] of TDensityProfileLayer;
            end; 
            { TAtmosphereParameters }
            TAtmosphereParameters=packed record             
             public
              Transform:TpvMatrix4x4; // Transform of the atmosphere for the case that the atmosphere is not centered at the origin (e.g. multiple planets)
              RayleighDensity:TDensityProfile;
              MieDensity:TDensityProfile;
              AbsorptionDensity:TDensityProfile;
              Center:TpvVector4; // w is unused, for alignment
              SolarIrradiance:TpvVector4; // w is unused, for alignment
              RayleighScattering:TpvVector4; // w is unused, for alignment
              MieScattering:TpvVector4; // w is unused, for alignment
              MieExtinction:TpvVector4; // w is unused, for alignment
              AbsorptionExtinction:TpvVector4; // w is unused, for alignment
              GroundAlbedo:TpvVector4; // w is unused, for alignment
              MiePhaseFunctionG:TpvFloat;
              SunAngularRadius:TpvFloat;
              BottomRadius:TpvFloat;
              TopRadius:TpvFloat;
              MuSMin:TpvFloat;
              procedure InitializeEarthAtmosphere;
            end;
            PAtmosphereParameters=^TAtmosphereParameters;
            { TRendererInstance }
            TRendererInstance=class
             public
              type { TKey }
                   TKey=record
                    public
                     fRendererInstance:TObject;
                    public
                     constructor Create(const aRendererInstance:TObject);
                   end;
                   PKey=^TKey;
             private
              fAtmosphere:TpvScene3DAtmosphere;
              fRendererInstance:TObject;
              fKey:TKey;
              fTransmittanceTexture:TpvScene3DRendererArray2DImage;
              fScatteringTexture:TpvScene3DRendererArray2DImage;
              fIrradianceTexture:TpvScene3DRendererArray2DImage;
              fMultiScatteringTexture:TpvScene3DRendererArray2DImage;
              fSkyViewLUTTexture:TpvScene3DRendererArray2DImage;
              fCameraVolumeTexture:TpvScene3DRendererArray2DImage;
             public
              constructor Create(const aAtmosphere:TpvScene3DAtmosphere;const aRendererInstance:TObject);
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
             published
              property Atmosphere:TpvScene3DAtmosphere read fAtmosphere;
              property RendererInstance:TObject read fRendererInstance;
              property TransmittanceTexture:TpvScene3DRendererArray2DImage read fTransmittanceTexture;
              property ScatteringTexture:TpvScene3DRendererArray2DImage read fScatteringTexture;
              property IrradianceTexture:TpvScene3DRendererArray2DImage read fIrradianceTexture;
              property MultiScatteringTexture:TpvScene3DRendererArray2DImage read fMultiScatteringTexture;
              property SkyViewLUTTexture:TpvScene3DRendererArray2DImage read fSkyViewLUTTexture;
              property CameraVolumeTexture:TpvScene3DRendererArray2DImage read fCameraVolumeTexture; 
            end;
            { TRendererInstances }
            TRendererInstances=TpvObjectGenericList<TRendererInstance>;
            { TRendererInstanceHashMap }
            TRendererInstanceHashMap=TpvHashMap<TRendererInstance.TKey,TRendererInstance>;
      private
       fScene3D:TObject;
       fAtmosphereParameters:TAtmosphereParameters;
       fPointerToAtmosphereParameters:PAtmosphereParameters;
       fRendererInstances:TRendererInstances;
       fRendererInstanceHashMap:TRendererInstanceHashMap;
       fRendererInstanceListLock:TPasMPSlimReaderWriterLock;
       fToDestroy:boolean;
       fReleaseFrameCounter:TpvInt32;
       fReady:Boolean;
      public
       constructor Create(const aScene3D:TObject);
       destructor Destroy; override;
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
       procedure Release;
       function HandleRelease:boolean;
       procedure Update(const aInFlightFrameIndex:TpvSizeInt);
      public  
       property AtmosphereParameters:PAtmosphereParameters read fPointerToAtmosphereParameters;
       property Ready:Boolean read fReady;
     end; 

implementation

uses PasVulkan.Scene3D;

{ TpvScene3DAtmosphere.TDensityProfileLayer }

constructor TpvScene3DAtmosphere.TDensityProfileLayer.Create(const aWidth,aExpTerm,aExpScale,aLinearTerm,aConstantTerm:TpvFloat);
begin
 Width:=aWidth;
 ExpTerm:=aExpTerm;
 ExpScale:=aExpScale;
 LinearTerm:=aLinearTerm;
 ConstantTerm:=aConstantTerm;
end;

{ TpvScene3DAtmosphere.TAtmosphereParameters }

procedure TpvScene3DAtmosphere.TAtmosphereParameters.InitializeEarthAtmosphere;
const EarthBottomRadius=6360.0;
      EarthTopRadius=6460.0;
      EarthRayleighScaleHeight=8.0;
      EarthMieScaleHeight=1.2;
begin
 
 // Transform
 Transform:=TpvMatrix4x4.Identity;

 // Sun
 SolarIrradiance:=TpvVector4.InlineableCreate(1.0,1.0,1.0,0.0);
 SunAngularRadius:=0.004675;

 // Planet
 BottomRadius:=EarthBottomRadius;
 TopRadius:=EarthTopRadius;
 GroundAlbedo:=TpvVector4.InlineableCreate(0.0,0.0,0.0,0.0);

 // Rayleigh scattering
 RayleighDensity.Layers[0]:=TDensityProfileLayer.Create(0.0,0.0,0.0,0.0,0.0);
 RayleighDensity.Layers[1]:=TDensityProfileLayer.Create(0.0,1.0,-1.0/EarthRayleighScaleHeight,0.0,0.0);
 RayleighScattering:=TpvVector4.InlineableCreate(0.005802,0.013558,0.033100,0.0);

 // Mie scattering
 MieDensity.Layers[0]:=TDensityProfileLayer.Create(0.0,0.0,0.0,0.0,0.0);
 MieDensity.Layers[1]:=TDensityProfileLayer.Create(0.0,1.0,-1.0/EarthMieScaleHeight,0.0,0.0);
 MieScattering:=TpvVector4.InlineableCreate(0.003996,0.003996,0.003996,0.0);
 MieExtinction:=TpvVector4.InlineableCreate(0.004440,0.004440,0.004440,0.0);
 MiePhaseFunctionG:=0.8;

 // Absorption extinction / Ozone layer
 AbsorptionDensity.Layers[0]:=TDensityProfileLayer.Create(25.0,0.0,0.0,1.0/15.0,-2.0/3.0);
 AbsorptionDensity.Layers[1]:=TDensityProfileLayer.Create(0.0,0.0,0.0,-1.0/15.0,8.0/3.0);
 AbsorptionExtinction:=TpvVector4.InlineableCreate(0.000650,0.001881,0.000085,0.0);

 MuSMin:=cos(PI*120.0/180.0);

end;

{ TpvScene3DAtmosphere.TRendererInstance.TKey }

constructor TpvScene3DAtmosphere.TRendererInstance.TKey.Create(const aRendererInstance:TObject);
begin
 fRendererInstance:=aRendererInstance;
end;

{ TpvScene3DAtmosphere.TRendererInstance }

constructor TpvScene3DAtmosphere.TRendererInstance.Create(const aAtmosphere:TpvScene3DAtmosphere;const aRendererInstance:TObject);
begin
 inherited Create;
 fAtmosphere:=aAtmosphere;
 fRendererInstance:=aRendererInstance;
 fKey:=TKey.Create(fRendererInstance);
{fTransmittanceTexture:TpvScene3DRendererArray2DImage;
 fScatteringTexture:TpvScene3DRendererArray2DImage;
 fIrradianceTexture:TpvScene3DRendererArray2DImage;
 fMultiScatteringTexture:TpvScene3DRendererArray2DImage;
 fSkyViewLUTTexture:TpvScene3DRendererArray2DImage;
 fCameraVolumeTexture:TpvScene3DRendererArray2DImage;}
end;

destructor TpvScene3DAtmosphere.TRendererInstance.Destroy;
begin
 FreeAndNil(fTransmittanceTexture);
 FreeAndNil(fScatteringTexture);
 FreeAndNil(fIrradianceTexture);
 FreeAndNil(fMultiScatteringTexture);
 FreeAndNil(fSkyViewLUTTexture);
 FreeAndNil(fCameraVolumeTexture);
 inherited Destroy;
end;

procedure TpvScene3DAtmosphere.TRendererInstance.AfterConstruction;
begin
 inherited AfterConstruction;
 if assigned(fAtmosphere) and assigned(fAtmosphere.fRendererInstanceListLock) then begin
  fAtmosphere.fRendererInstanceListLock.Acquire;
  try
   fAtmosphere.fRendererInstances.Add(self);
   fAtmosphere.fRendererInstanceHashMap.Add(fKey,self);
  finally
   fAtmosphere.fRendererInstanceListLock.Release;
  end;
 end;
end;

procedure TpvScene3DAtmosphere.TRendererInstance.BeforeDestruction;
begin
 if assigned(fAtmosphere) and assigned(fAtmosphere.fRendererInstanceListLock) then begin
  fAtmosphere.fRendererInstanceListLock.Acquire;
  try
   fAtmosphere.fRendererInstanceHashMap.Delete(fKey);
   fAtmosphere.fRendererInstances.RemoveWithoutFree(self);
  finally
   fAtmosphere.fRendererInstanceListLock.Release;
  end;
 end;
 inherited BeforeDestruction;
end;

{ TpvScene3DAtmosphere }

constructor TpvScene3DAtmosphere.Create(const aScene3D:TObject);
begin
 
 inherited Create;

 fScene3D:=aScene3D;
 
 fAtmosphereParameters.InitializeEarthAtmosphere;
 fPointerToAtmosphereParameters:=@fAtmosphereParameters;
 
 fRendererInstances:=TRendererInstances.Create(true);
 fRendererInstanceHashMap:=TRendererInstanceHashMap.Create(nil);
 fRendererInstanceListLock:=TPasMPSlimReaderWriterLock.Create;
 
 fReady:=false;

end;

destructor TpvScene3DAtmosphere.Destroy;
begin
 FreeAndNil(fRendererInstances);
 FreeAndNil(fRendererInstanceHashMap);
 FreeAndNil(fRendererInstanceListLock);
 inherited Destroy;
end;

procedure TpvScene3DAtmosphere.AfterConstruction;
begin
 inherited AfterConstruction;
 if assigned(fScene3D) then begin
  TpvScene3DAtmospheres(TpvScene3D(fScene3D).Atmospheres).Lock.AcquireWrite;
  try
   TpvScene3DAtmospheres(TpvScene3D(fScene3D).Atmospheres).Add(self);
  finally
   TpvScene3DAtmospheres(TpvScene3D(fScene3D).Atmospheres).Lock.ReleaseWrite;
  end;
 end;
end;

procedure TpvScene3DAtmosphere.BeforeDestruction;
var Index:TpvSizeInt;
begin
 if assigned(fScene3D) then begin
  TpvScene3DAtmospheres(TpvScene3D(fScene3D).Atmospheres).Lock.AcquireWrite;
  try
   Index:=TpvScene3DAtmospheres(TpvScene3D(fScene3D).Atmospheres).IndexOf(self);
   if Index>=0 then begin
    TpvScene3DAtmospheres(TpvScene3D(fScene3D).Atmospheres).Extract(Index); // not delete or remove, since we don't want to free ourself here already.
   end;
  finally
   TpvScene3DAtmospheres(TpvScene3D(fScene3D).Atmospheres).Lock.ReleaseWrite;
  end;
 end;
 inherited BeforeDestruction;
end;

procedure TpvScene3DAtmosphere.Release;
begin
 if fReleaseFrameCounter<0 then begin
  fReleaseFrameCounter:=TpvScene3D(fScene3D).CountInFlightFrames;
  fReady:=false;
 end;
end;

function TpvScene3DAtmosphere.HandleRelease:boolean;
begin
 if fReleaseFrameCounter>0 then begin
  result:=TPasMPInterlocked.Decrement(fReleaseFrameCounter)=0;
  if result then begin
   Free;
  end;
 end else begin
  result:=false;
 end;
end;

procedure TpvScene3DAtmosphere.Update(const aInFlightFrameIndex:TpvSizeInt);
begin

end;

{ TpvScene3DAtmospheres }

constructor TpvScene3DAtmospheres.Create(const aScene3D:TObject);
begin
 inherited Create(true);
 fScene3D:=aScene3D;
 fLock:=TPasMPMultipleReaderSingleWriterLock.Create;
end;

destructor TpvScene3DAtmospheres.Destroy;
begin
 FreeAndNil(fLock);
 inherited Destroy;
end;

procedure TpvScene3DAtmospheres.ProcessReleases;
var Index:TpvInt32;
    Atmosphere:TpvScene3DAtmosphere;
begin
 // Going backwards through the list, because we will remove items from the list
 fLock.AcquireRead;
 try
  Index:=Count;
  while Index>0 do begin
   dec(Index);
   Atmosphere:=Items[Index];
   if assigned(Atmosphere) then begin
    fLock.ReleaseRead;
    try
     Atmosphere.HandleRelease;
    finally
     fLock.AcquireRead;
    end;
   end;
  end;
 finally
  fLock.ReleaseRead;
 end; 
end;

end.
