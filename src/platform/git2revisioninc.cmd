#!/bin/sh

export REVISION_INC=$1/dcrevision.inc

rm -f $REVISION_INC
cp ../units/dcrevision.inc $REVISION_INC

export REVISION=$(git -C $1 show -s --format="%h" HEAD)
export COMMIT=$(git -C $1 show -s --format="%ct" HEAD)

if [ $REVISION ] && [ $COMMIT ]; then

  echo "// Created by Git2RevisionInc"   >  $REVISION_INC
  echo "const dcRevision = '$REVISION';" >> $REVISION_INC
  echo "const dcCommit = $COMMIT;"       >> $REVISION_INC

fi

echo "Git revision" $REVISION $COMMIT
