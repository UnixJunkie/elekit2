
set -x

export CPPFLAGS=-I/usr/lib64/python2.6/site-packages/numpy/core/include/numpy

# FBR: pdb2pqr must be run from where it is compiled
./configure --with-python=/usr/bin/python2.7 --prefix=$PWD
make
#make test
#make adv-test
