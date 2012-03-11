@echo off

rem Compiling components

rem This script is run from build.bat.
rem If you run it directly set %lazpath% first
rem or have lazbuild in your PATH.

pushd components
lazbuild chsdet\chsdet.lpk %DC_ARCH%
lazbuild CmdLine\cmdbox.lpk %DC_ARCH%
lazbuild dcpcrypt\dcpcrypt.lpk %DC_ARCH%
lazbuild KASToolBar\kascomp.lpk %DC_ARCH%
lazbuild viewer\viewerpackage.lpk %DC_ARCH%
lazbuild gifanim\pkg_gifanim.lpk %DC_ARCH%
lazbuild ZVDateTimeCtrls\zvdatetimectrls.lpk %DC_ARCH%
popd
