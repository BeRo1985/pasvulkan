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
     PasVulkan.Types,
     PasVulkan.Math;

type { TpvScene3DAtmosphere } 
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
             ScatteringOrders=4; 
        type { TDensityProfileLayer }
             TDensityProfileLayer=record
              public
               Width:TpvFloat;
               ExpTerm:TpvFloat;
               ExpScale:TpvFloat;
               LinearTerm:TpvFloat;
               ConstantTerm:TpvFloat;
               constructor Create(const aWidth,aExpTerm,aExpScale,aLinearTerm,aConstantTerm:TpvFloat);
             end;
             PDensityProfileLayer=^TDensityProfileLayer;
             { TDensityProfile }
             TDensityProfile=array of TDensityProfileLayer;
             { TAtmosphereModel }
      private
      public
     end; 

implementation

{ TpvScene3DAtmosphere.TDensityProfileLayer }

constructor TpvScene3DAtmosphere.TDensityProfileLayer.Create(const aWidth,aExpTerm,aExpScale,aLinearTerm,aConstantTerm:TpvFloat);
begin
 Width:=aWidth;
 ExpTerm:=aExpTerm;
 ExpScale:=aExpScale;
 LinearTerm:=aLinearTerm;
 ConstantTerm:=aConstantTerm;
end;

end.
