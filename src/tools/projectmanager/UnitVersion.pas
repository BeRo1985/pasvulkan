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

const ProjectManagerVersion='1.00.2019.08.26.09.40.0000';

      ProjectManagerCopyright='Copyright (C) 2018-2019, Benjamin ''BeRo'' Rosseaux';

implementation

end.

