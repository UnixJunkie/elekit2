#!/bin/bash

#set -x
set -u
set -e

# <user configurable area>
HERE=/home/berenger/src/EleKit2/src
# </user configurable area>

export APBS=$HERE/../ext/apbs-1.3-src/bin/apbs
export PDB2PQR=$HERE/../ext/pdb2pqr-1.8-src/pdb2pqr.py
export OMP_NUM_THREADS=1 # turn off APBS parallelization via OpenMP

$HERE/es_tool_smlpr.native "$@"
