#!/bin/bash

#$ -cwd
#$ -q all.q
#$ -j y
#$ -S /bin/bash
#$ -pe serial 48
#$ -hard -l h="node9|node10|node11|node12|node13|node14|node15|node16"

HERE=`pwd`
#echo "HERE: "$HERE

TMPDIR=`mktemp -d`
#echo "TMPDIR: "$TMPDIR
trap "rm -rf $TMPDIR" EXIT

cp $rec $TMPDIR/
cp $lig $TMPDIR/

cd $TMPDIR

(
# EDIT HERE THE PATH TO YOUR elekit2.sh WRAPPER SCRIPT
~/bin/elekit2.sh -np 48 -rec *.pdb -lig *.mol2 \
                 -alig 2.5 -surf 0.75 -sdie 2.0 2>&1
) > $HERE/$lig".log"
