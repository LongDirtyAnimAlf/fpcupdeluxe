#---------------------------------------------------------------------------------
# Source and Install directories
#---------------------------------------------------------------------------------

SRCDIR=/home/Alfred/binutils-2.28                   # the source code dir for binutils
prefix=/home/Alfred/cross/powerpc64-linux                # installation directory

#---------------------------------------------------------------------------------
# set the target and compiler flags
#---------------------------------------------------------------------------------

#target=arm-elf
target=powerpc64-linux-gnu

export CFLAGS='-O2 -pipe'
export CXXFLAGS='-O2 -pipe'
export LDFLAGS='-s'
export DEBUG_FLAGS=''

#---------------------------------------------------------------------------------
# Build and install binutils
#---------------------------------------------------------------------------------
# cd $SRCDIR
# make clean 2>&1

mkdir -p $target/binutils
cd $target/binutils

$SRCDIR/configure --prefix=$prefix --target=$target \
    --disable-nls --disable-shared --enable-debug --disable-threads --disable-werror \
    --with-gcc --with-gnu-as --with-gnu-ld --with-stabs \
    --enable-interwork --enable-multilib \
    2>&1 | tee binutils_configure.log

make    all 2>&1 | tee binutils_make.log
make    install 2>&1 | tee binutils_install.log
