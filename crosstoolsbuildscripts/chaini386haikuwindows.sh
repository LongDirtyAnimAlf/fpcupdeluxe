#---------------------------------------------------------------------------------
# Source and Install directories
#---------------------------------------------------------------------------------

SRCDIR=/home/Alfred/buildtools-master/binutils                   # the source code dir for binutils
# prefix=/home/Alfred/cross/i386-unknown-haiku                # installation directory
# SRCDIR=/home/Alfred/binutils-2.30                   # the source code dir for binutils
prefix=/home/Alfred/cross/i386-unknown-haiku                # installation directory

#---------------------------------------------------------------------------------
# set the target and compiler flags
#---------------------------------------------------------------------------------

#target=arm-elf
target=i386-unknown-haiku

export CFLAGS='-O2 -pipe -Wno-error'
export CXXFLAGS='-O2 -pipe -Wno-error'
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
    --disable-nls --disable-shared --enable-debug --disable-threads \
    --with-gnu-as --with-gnu-ld --with-stabs \
    --enable-interwork --enable-multilib \
    2>&1 | tee binutils_configure.log

make    all 2>&1 | tee binutils_make.log
make    install 2>&1 | tee binutils_install.log
