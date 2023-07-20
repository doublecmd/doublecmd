@echo off

if not exist "%1" ( md "%1" )

set REVISION_TXT="%1\revision.txt"
set REVISION_INC="%1\dcrevision.inc"

del /Q %REVISION_TXT% 2> nul
del /Q %REVISION_INC% 2> nul
copy ..\units\dcrevision.inc %REVISION_INC% > nul

git -C %1 rev-list --count master..HEAD > %REVISION_TXT%

IF ERRORLEVEL 1 goto EXIT

set /P REVISION=<%REVISION_TXT%

echo %REVISION% | find "fatal:" > nul

IF NOT ERRORLEVEL 1 goto EXIT

git -C %1 rev-parse --short HEAD > %REVISION_TXT%

IF ERRORLEVEL 1 goto EXIT

set /P COMMIT=<%REVISION_TXT%

echo // Created by Git2RevisionInc> %REVISION_INC%
echo const dcRevision = '%REVISION%';>> %REVISION_INC%
echo const dcCommit = '%COMMIT%';>> %REVISION_INC%

:EXIT

echo Git revision %REVISION% %COMMIT%