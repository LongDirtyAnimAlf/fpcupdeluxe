#!/usr/bin/bash
set -e


tarball="make-4.4.tar.gz"
make_version=${tarball%.tar.gz}

BASEDIR=$PWD                                     # the base dir for building binutils
SRCDIR=$BASEDIR/$make_version                    # the source code dir for binutils
prefix=$BASEDIR/build/$make_version              # installation directory

export CFLAGS='-O2 -pipe'
export CXXFLAGS='-O2 -pipe'
export LDFLAGS='-s'
export DEBUG_FLAGS=''


rm -rf SRCDIR || echo "No existing make directory"
tar -xzf "$tarball"

mkdir -p $BASEDIR/$make_version/make
cd $BASEDIR/$make_version/make

$SRCDIR/configure \
    --disable-nls \
    --prefix=$prefix \
    --without-guile \
    --libexecdir=$prefix/bin/libexec \
    --includedir=$prefix/bin/include \
    --infodir=$prefix/bin/info \
    --docdir=$prefix/bin/doc \
    --mandir=$prefix/bin/man \
    --datarootdir=$prefix/bin/share \
    --libdir=$prefix/bin/lib \
    2>&1 | tee make_configure.log

make    clean all 2>&1 | tee make_make.log
make    install 2>&1 | tee make_install.log
