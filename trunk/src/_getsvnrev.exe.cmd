rem @echo off

rem get DC revision under Windows
%1tools\svn2revisioninc.exe ..\ dcrevision.inc --c=dcRevision

echo "This command is need for successful exit code"