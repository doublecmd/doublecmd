@echo off

set REVISION_TXT=%1\revision.txt
set REVISION_INC=%1\dcrevision.inc

del /Q %REVISION_TXT% 2> nul
del /Q %REVISION_INC% 2> nul
copy ..\units\dcrevision.inc %REVISION_INC% > nul

svnversion -n ..\ > %REVISION_TXT%

IF ERRORLEVEL 1 goto EXIT

set /P REVISION=<%REVISION_TXT%

echo %REVISION% | find "Unversioned" > nul

IF NOT ERRORLEVEL 1 goto EXIT

echo // Created by Svn2RevisionInc> %REVISION_INC%
echo const dcRevision = '%REVISION%';>> %REVISION_INC%

:EXIT

echo Subversion revision %REVISION%
