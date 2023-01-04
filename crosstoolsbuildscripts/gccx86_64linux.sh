#---------------------------------------------------------------------------------
# Basics (targets)
#---------------------------------------------------------------------------------

version=12.2.0
targetcpu=x86_64
targetos=linux

target=$targetcpu-$targetos

#---------------------------------------------------------------------------------
# Source and Install directories
#---------------------------------------------------------------------------------

SRCDIR=/home/superdad/gcc/gcc-$version              # the source code dir for gcc
prefix=/home/superdad/gcc/cross/$target-$version    # installation directory

#---------------------------------------------------------------------------------
# Set the path for the installed binutils
#---------------------------------------------------------------------------------

export PATH=${PATH}:/home/superdad/binutils/cross/$target/bin:/home/superdad/binutils/cross/$target-2.39/bin

#---------------------------------------------------------------------------------
# Set compiler flags
#---------------------------------------------------------------------------------

export CFLAGS='-O2 -pipe'
export CXXFLAGS='-O2 -pipe'
export LDFLAGS='-s'
export DEBUG_FLAGS=''
export HOST=i686-pc-mingw32
export BUILD=i686-pc-mingw32

#---------------------------------------------------------------------------------
# Build and install gcc
#---------------------------------------------------------------------------------

# cd $SRCDIR
# make clean 2>&1

mkdir -p $SRCDIR/$target-gcc
cd $SRCDIR/$target-gcc

../configure \
    --host=${HOST} \
    --build=${BUILD} \
    --target=$target \
    --enable-interwork --enable-multilib --enable-languages="c" \
    --with-gnu-as --with-gnu-ld --disable-nls \
    --disable-shared \
    --prefix=$prefix \
    --libexecdir=$prefix/bin/libexec \
    --includedir=$prefix/bin/include \
    --libdir=$prefix/bin/lib \
    2>&1 | tee gcc_configure.log

make all-gcc
make install-gcc

# make all-gcc 2>&1 | tee gcc_make.log
# make install-gcc 2>&1 | tee gcc_install.log
