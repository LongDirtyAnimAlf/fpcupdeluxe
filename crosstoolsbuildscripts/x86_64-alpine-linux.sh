#!/usr/bin/env bash

set -e

#---------------------------------------------------------------------------------
# Basics (targets)
#---------------------------------------------------------------------------------

version=2.40
targetcpu=x86_64
targetos=alpine-linux-musl

target=$targetcpu-$targetos

#---------------------------------------------------------------------------------
# Source and Install directories
#---------------------------------------------------------------------------------

BASEDIR=/home/superdad/binutilsnew         # the base dir for building binutils
SRCDIR=$BASEDIR/binutils-$version                # the source code dir for binutils
prefix=$BASEDIR/cross/$target-$version           # installation directory

# chmod 755 $SRCDIR/configure
# chmod 755 $SRCDIR/install-sh

# export CC=/usr/bin/clang
# export CXX=/usr/bin/clang++
# export AR=/usr/bin/ar

# export CFLAGS='-arch arm64 -O2 -pipe'
# export CXXFLAGS='-arch arm64 -O2 -pipe'
# export LDFLAGS='-arch arm64'
# export DEBUG_FLAGS=''

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
    --enable-threads \
    --enable-ld=default \
    --enable-targets=x86_64-pep \
    --enable-64-bit-bfd \
    --enable-gold \
    --enable-relro \
    --enable-deterministic-archives \
    --enable-default-execstack=no \
    --enable-default-hash-style=gnu \
    --with-pic \
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
