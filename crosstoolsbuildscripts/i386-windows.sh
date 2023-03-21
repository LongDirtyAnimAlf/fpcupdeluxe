#!/usr/bin/env bash

set -e

#---------------------------------------------------------------------------------
# Basics (targets)
#---------------------------------------------------------------------------------

version=2.40
targetcpu=i686
# targetos=pc-mingw64
targetos=w64-mingw32

target=$targetcpu-$targetos

#---------------------------------------------------------------------------------
# Source and Install directories
#---------------------------------------------------------------------------------

# BASEDIR=/home/superdad/binutilsnew             # the base dir for building binutils
BASEDIR=$PWD                                     # the base dir for building binutils
SRCDIR=$BASEDIR/binutils-$version                # the source code dir for binutils
prefix=$BASEDIR/cross/$target-$version           # installation directory

export CFLAGS='-O2 -pipe'
export CXXFLAGS='-O2 -pipe'
export LDFLAGS='-s'
export DEBUG_FLAGS=''

#---------------------------------------------------------------------------------
# Build and install binutils
#---------------------------------------------------------------------------------

# cd $SRCDIR
# make clean 2>&1

mkdir -p $BASEDIR/$target/binutils
cd $BASEDIR/$target/binutils

$SRCDIR/configure \
    --target=$target \
    --disable-nls --disable-shared --disable-gprof --disable-gprofng --disable-gdb --disable-werror \
    --disable-multilib \
    --disable-threads \
    --enable-ld=default \
    --enable-64-bit-bfd \
    --prefix=$prefix \
    --libexecdir=$prefix/bin/libexec \
    --datarootdir=$prefix/bin/share \
    --includedir=$prefix/bin/include \
    --libdir=$prefix/bin/lib \
    2>&1 | tee binutils_configure.log

#    --with-zlib=no \
#    --without-zlib \


make    all 2>&1 | tee binutils_make.log
make    install 2>&1 | tee binutils_install.log
