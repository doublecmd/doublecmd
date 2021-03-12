@echo off

set REVISION_TXT=%1\revision.txt
set REVISION_INC=%1\dcrevision.inc

del /Q %REVISION_TXT% 2> nul
del /Q %REVISION_INC% 2> nul
copy ..\units\dcrevision.inc %REVISION_INC% > nul

git -C %1 show -s --format="%%h %%ct" HEAD > %REVISION_TXT%

IF ERRORLEVEL 1 goto EXIT

set /P REVISION=<%REVISION_TXT%

echo %REVISION% | find "fatal:" > nul

IF NOT ERRORLEVEL 1 goto EXIT

rem Get package version and architecture
for /f "tokens=1,2" %%a in ("%REVISION%") do (
  echo // Created by Git2RevisionInc> %REVISION_INC%
  echo const dcRevision = '%%a';>> %REVISION_INC%
  echo const dcCommit = %%b;>> %REVISION_INC%
)

:EXIT

echo Git revision %REVISION%