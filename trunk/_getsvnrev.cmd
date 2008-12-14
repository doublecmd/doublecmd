#!/bin/sh

# get DC revision under Linux
$1/tools/svn2revisioninc ./ dcrevision.inc --c=dcRevision

rem get DC revision under Windows
%1\tools\svn2revisioninc .\ dcrevision.inc --c=dcRevision

echo "This command is need for successful exit code"