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

SRCDIR=/home/superdad/binutils/binutils-$version                # the source code dir for binutils
prefix=/home/superdad/binutils/cross/$target-$version           # installation directory

chmod 755 $SRCDIR/configure
chmod 755 $SRCDIR/install-sh

export CFLAGS='-O2 -pipe'
export CXXFLAGS='-O2 -pipe'
export LDFLAGS='-s'
export DEBUG_FLAGS=''
export HOST=i686-pc-mingw32
export BUILD=i686-pc-mingw32

#---------------------------------------------------------------------------------
# Build and install binutils
#---------------------------------------------------------------------------------

# cd $SRCDIR
# make clean 2>&1

mkdir -p $target/binutils
cd $target/binutils

$SRCDIR/configure \
    --host=${HOST} \
    --build=${BUILD} \
    --target=$target \
    --disable-nls --disable-shared --disable-gprof --disable-gdb --disable-werror \
    --enable-64-bit-bfd --enable-gold --enable-ld=default --enable-interwork --enable-multilib \
    --with-gcc --with-gnu-as --with-gnu-ld \
    --prefix=$prefix \
    --libexecdir=$prefix/bin/libexec \
    --datarootdir=$prefix/bin/share \
    --includedir=$prefix/bin/include \
    --libdir=$prefix/bin/lib \
    2>&1 | tee binutils_configure.log

make    all 2>&1 | tee binutils_make.log
make    install 2>&1 | tee binutils_install.log
