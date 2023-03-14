#!/usr/bin/env bash

set -e

#---------------------------------------------------------------------------------
# Basics (targets)
#---------------------------------------------------------------------------------

version=2.17
targetcpu=i386
targetos=openbsd
sourcename=binutils

target=$targetcpu-$targetos

#---------------------------------------------------------------------------------
# Source and Install directories
#---------------------------------------------------------------------------------

BASEDIR=/home/superdad/openbsd         # the base dir for building binutils
SRCDIR=$BASEDIR/$sourcename-$version                # the source code dir for binutils
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

INCLUDE_FIX=""
INCLUDE_FIX+="-D_GNU_SOURCE "
INCLUDE_FIX+="-Werror=cast-function-type "
INCLUDE_FIX+="-Werror=incompatible-pointer-types "
#INCLUDE_FIX+="-Wno-cast-function-type "
#INCLUDE_FIX+="-Wwrite-strings "
#INCLUDE_FIX+="-Wc++-compat "
#INCLUDE_FIX+="-Wstrict-prototypes "
#INCLUDE_FIX+="-Wshadow=local "
#INCLUDE_FIX+="-Wno-error=implicit-fallthrough "
#INCLUDE_FIX+="-Wno-error=unused-function "
#INCLUDE_FIX+="-Wno-error=switch "
#INCLUDE_FIX+="-Wno-error=return-type "
#INCLUDE_FIX+="-Wno-error=unused-variable "
#INCLUDE_FIX+="-Wno-error=uninitialized "
#INCLUDE_FIX+="-Wno-error=implicit-fallthrough "
#INCLUDE_FIX+="-Wno-uninitialized "
INCLUDE_FIX+="-Wno-array-bounds "
#INCLUDE_FIX+="-Wno-unknown-pragmas "
#INCLUDE_FIX+="-Wno-unused-variable "
#INCLUDE_FIX+="-Wno-deprecated-declarations "
#INCLUDE_FIX+="-Wno-cast-function-type "
#INCLUDE_FIX+="-Wno-free-nonheap-object "
#INCLUDE_FIX+="-Wno-stringop-overflow "
INCLUDE_FIX+="-Wno-unused-but-set-variable "
INCLUDE_FIX+="-Wno-unused-value "
INCLUDE_FIX+="-Wno-implicit-fallthrough "
#INCLUDE_FIX+="-Wno-stringop-truncation "
INCLUDE_FIX+="-Wno-shift-negative-value "
INCLUDE_FIX+="-Wno-unused-function "
INCLUDE_FIX+="-Wno-missing-prototypes "
INCLUDE_FIX+="-Wno-maybe-uninitialized "
#INCLUDE_FIX+="-Wno-c++-compat "
#INCLUDE_FIX+="-Wno-sign-compare "
#INCLUDE_FIX+="-Wno-attributes "
#INCLUDE_FIX+="-Wno-nonnull"


#export CFLAGS='-w -Wno-error -O2 -pipe'
#export CFLAGS="-O2 -pipe $INCLUDE_FIX -std=gnu17"
#export CXXFLAGS="-O2 -pipe -std=gnu++17"
#export CFLAGS="-O2 -pipe $INCLUDE_FIX"
#export CXXFLAGS="-O2 -pipe"
#export LDFLAGS='-s'
export DEBUG_FLAGS=''

export CFLAGS="-O2 -pipe $INCLUDE_FIX -std=gnu17"
export CXXFLAGS="-O2 -pipe -std=gnu++17"


#---------------------------------------------------------------------------------
# Build and install
#---------------------------------------------------------------------------------

# cd $SRCDIR
# make clean 2>&1

mkdir -p $BASEDIR/$target/$sourcename
cd $BASEDIR/$target/$sourcename

$SRCDIR/configure \
    --target=$target \
    --prefix=$prefix \
    --libexecdir=$prefix/bin/libexec \
    --includedir=$prefix/bin/include \
    --libdir=$prefix/bin/lib \
    --host=i686-w64-mingw32 \
    --disable-nls --disable-shared --disable-debug --disable-threads \
    --disable-werror --disable-multilib \
    --with-sysroot \
    2>&1 | tee binutils_configure.log

#   --with-system-zlib

make MAKEINFO=true all 2>&1 | tee binutils_make.log
make MAKEINFO=true install 2>&1 | tee binutils_install.log
