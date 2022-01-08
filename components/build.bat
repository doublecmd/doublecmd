@echo off

rem Compiling components

rem Do not execute this script directly.
rem This script is called from ..\build.bat.

pushd components
lazbuild chsdet\chsdet.lpk %DC_ARCH%
lazbuild multithreadprocs\multithreadprocslaz.lpk %DC_ARCH%
lazbuild dcpcrypt\dcpcrypt.lpk %DC_ARCH%
lazbuild doublecmd\doublecmd_common.lpk %DC_ARCH%
lazbuild KASToolBar\kascomp.lpk %DC_ARCH%
lazbuild viewer\viewerpackage.lpk %DC_ARCH%
lazbuild gifanim\pkg_gifanim.lpk %DC_ARCH%
lazbuild synunihighlighter\synuni.lpk %DC_ARCH%
lazbuild virtualterminal\virtualterminal.lpk %DC_ARCH%
popd
