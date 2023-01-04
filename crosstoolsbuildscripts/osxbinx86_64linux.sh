#---------------------------------------------------------------------------------
# Basics (targets)
#---------------------------------------------------------------------------------

version=2.39
targetcpu=x86_64
targetos=linux

target=$targetcpu-$targetos

#---------------------------------------------------------------------------------
# Source and Install directories
#---------------------------------------------------------------------------------

BASEDIR=/Users/superdad/Desktop/binutils         # the base dir for building binutils
SRCDIR=$BASEDIR/binutils-$version                # the source code dir for binutils
prefix=$BASEDIR/cross/$target-$version           # installation directory

# chmod 755 $SRCDIR/configure
# chmod 755 $SRCDIR/install-sh

export CC=/usr/bin/clang
export CXX=/usr/bin/clang++
export AR=/usr/bin/ar

export CFLAGS='-arch arm64 -O2 -pipe'
export CXXFLAGS='-arch arm64 -O2 -pipe'
export LDFLAGS='-arch arm64'
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
    --host=arm64-apple-darwin \
    --disable-nls --disable-shared --disable-gprof --disable-gprofng --disable-gdb --disable-werror \
    --enable-64-bit-bfd --enable-ld --enable-interwork --enable-multilib \
    --with-gcc --with-gnu-as --with-gnu-ld \
    --prefix=$prefix \
    --libexecdir=$prefix/bin/libexec \
    --datarootdir=$prefix/bin/share \
    --includedir=$prefix/bin/include \
    --libdir=$prefix/bin/lib \
    2>&1 | tee binutils_configure.log

make    all 2>&1 | tee binutils_make.log
make    install 2>&1 | tee binutils_install.log
