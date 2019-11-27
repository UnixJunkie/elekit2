
set -x

\rm -f config.log
make clean
make distclean
./configure --prefix=$PWD
make
#make test
#make install
