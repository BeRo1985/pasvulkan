unit UnitVersion;
{$i ..\..\PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

interface

const ProjectManagerVersion='1.00.2018.08.09.05.26.0000';

      ProjectManagerCopyright='Copyright (C) 2018, Benjamin ''BeRo'' Rosseaux';

implementation

end.

