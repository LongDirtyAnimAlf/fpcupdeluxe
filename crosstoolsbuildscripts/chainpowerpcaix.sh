#---------------------------------------------------------------------------------
# Source and Install directories
#---------------------------------------------------------------------------------

SRCDIR=/home/Alfred/binutils-2.30                   # the source code dir for binutils
prefix=/home/Alfred/cross/powerpc64-aix                # installation directory

#---------------------------------------------------------------------------------
# set the target and compiler flags
#---------------------------------------------------------------------------------

# target=arm-elf
# target=powerpc64-aix
target=powerpc-ibm-aix5.3

export CFLAGS='-O2 -pipe'
export CXXFLAGS='-O2 -pipe'
# export LDFLAGS='-s'
export DEBUG_FLAGS=''

#---------------------------------------------------------------------------------
# Build and install binutils
#---------------------------------------------------------------------------------
# cd $SRCDIR
# make clean 2>&1

mkdir -p $target/binutils
cd $target/binutils

$SRCDIR/configure --prefix=$prefix --target=$target \
    --disable-nls --disable-shared --disable-threads --disable-werror \
    --with-sysroot --disable-interwork --disable-multilib \
    2>&1 | tee binutils_configure.log

make    all 2>&1 | tee binutils_make.log
make    install 2>&1 | tee binutils_install.log
