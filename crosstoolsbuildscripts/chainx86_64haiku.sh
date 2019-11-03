#! /usr/bin/env bash

#---------------------------------------------------------------------------------
# Source and Install directories
#---------------------------------------------------------------------------------

SRCDIR=/home/superdad/Downloads/buildtools-master/binutils                   # the source code dir for binutils
prefix=/home/superdad/Downloads/cross/x86_64-unknown-haiku                # installation directory

#---------------------------------------------------------------------------------
# set the target and compiler flags
#---------------------------------------------------------------------------------

target=x86_64-unknown-haiku

export CFLAGS='-O2 -pipe -Wno-error'
export CXXFLAGS='-O2 -pipe -Wno-error'
export LDFLAGS="-Wl,-rpath,'XORIGIN/lib' -Wl,--enable-new-dtags"
export DEBUG_FLAGS=''

#---------------------------------------------------------------------------------
# Make the right binutils
#---------------------------------------------------------------------------------

mkdir -p $target/binutils
cd $target/binutils

#---------------------------------------------------------------------------------
# Configure binutils
#---------------------------------------------------------------------------------

$SRCDIR/configure --prefix=$prefix --target=$target \
    --disable-nls --enable-shared --enable-debug --disable-threads \
    2>&1 | tee binutils_configure.log

#---------------------------------------------------------------------------------
# Dirty hack to get the rpath right
#---------------------------------------------------------------------------------

sed 's,XORIGIN,$\\\\$$\\$$\\\\$$\\$$ORIGIN,g' < Makefile > Makefile.new
mv -f Makefile.new Makefile

#---------------------------------------------------------------------------------
# Build and install binutils
#---------------------------------------------------------------------------------

make    clean 2>&1 | tee binutils_make.log
make    all 2>&1 | tee binutils_make.log
make    install 2>&1 | tee binutils_install.log
