#!/bin/sh

export REVISION_INC=$2/units/dcrevision.inc

# DC revision number
export REVISION=$(git -C $1 rev-list --count HEAD)
export COMMIT=$(git -C $1 rev-parse --short HEAD)

# Update dcrevision.inc
echo "// Created by Git2RevisionInc"   >  $REVISION_INC
echo "const dcRevision = '$REVISION';" >> $REVISION_INC
echo "const dcCommit = '$COMMIT';"     >> $REVISION_INC

# Return revision
echo $REVISION
