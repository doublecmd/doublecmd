#!/bin/sh

# DC revision number
export DC_REVISION=$(svnversion $1 | sed -e 's/\([0-9]*\).*/\1/')

# Update dcrevision.inc
echo "// Created by Svn2RevisionInc"      >  $2/src/platform/dcrevision.inc
echo "const dcRevision = '$DC_REVISION';" >> $2/src/platform/dcrevision.inc

# Return revision
echo $DC_REVISION
