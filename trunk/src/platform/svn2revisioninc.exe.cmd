@echo off

set REVISION_TXT=%1\revision.txt
set REVISION_INC=%1\dcrevision.inc

del /Q %REVISION_TXT%
del /Q %REVISION_INC%

svnversion -n ..\ > %REVISION_TXT%

IF ERRORLEVEL 1 goto EXIT

set /P REVISION=<%REVISION_TXT%

echo %REVISION% | find "Unversioned"

IF NOT ERRORLEVEL 1 goto EXIT

echo // Created by Svn2RevisionInc> %REVISION_INC%
echo const dcRevision = '%REVISION%';>> %REVISION_INC%

:EXIT

echo Subversion revision %REVISION%
