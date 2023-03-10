#!/usr/bin/env bash

echo ""
echo "*** Getting needed packages ***"
echo ""

pacman -S mc git bison flex texinfo patch --noconfirm

pacman -S mingw-w64-x86_64-binutils --noconfirm
pacman -S mingw-w64-x86_64-gcc --noconfirm
pacman -S mingw-w64-x86_64-libtool --noconfirm
pacman -S mingw-w64-x86_64-cmake --noconfirm
pacman -S mingw-w64-x86_64-make --noconfirm
pacman -S mingw-w64-x86_64-zlib --noconfirm
pacman -S mingw-w64-x86_64-libxml2 --noconfirm
pacman -S mingw-w64-x86_64-dlfcn --noconfirm
pacman -S mingw-w64-x86_64-autotools --noconfirm

pacman -S mingw-w64-x86_64-diffutils --noconfirm
pacman -S mingw-w64-x86_64-libiconv --noconfirm

#---------------------------------------------------------------------------------
# Get packages to compile gcc for windows
#---------------------------------------------------------------------------------

pacman -S mingw-w64-x86_64-gmp --noconfirm
pacman -S mingw-w64-x86_64-mpc --noconfirm
pacman -S mingw-w64-x86_64-mpfr --noconfirm

#---------------------------------------------------------------------------------
# Get packages to compile libtapi and ld64 for windows
#---------------------------------------------------------------------------------

# pacman -S mingw-w64-x86_64-clang --noconfirm

echo ""
echo "*** All done ***"
echo ""
