#---------------------------------------------------------------------------------
# Get packages to compile binutils for win32
#---------------------------------------------------------------------------------

pacman -S --noconfirm texinfo
pacman -S --noconfirm libtool
pacman -S --noconfirm bison
pacman -S --noconfirm mingw-w64-i686-binutils
pacman -S --noconfirm mingw-w64-i686-gcc
pacman -S --noconfirm mingw-w64-i686-zlib
pacman -S --noconfirm mingw-w64-i686-make
pacman -S --noconfirm mingw-w64-i686-diffutils
pacman -S --noconfirm mingw-w64-i686-libiconv

#---------------------------------------------------------------------------------
# Get packages to compile gcc for win32
#---------------------------------------------------------------------------------

pacman -S --noconfirm flex
pacman -S --noconfirm mingw-w64-i686-gmp
pacman -S --noconfirm mingw-w64-i686-mpc
pacman -S --noconfirm mingw-w64-i686-mpfr

#---------------------------------------------------------------------------------
# Get packages to compile sqlite3 for win32
#---------------------------------------------------------------------------------

# pacman -S --noconfirm mingw-w64-i686-autotools
# pacman -S --noconfirm mingw-w64-i686-icu
# pacman -S --noconfirm mingw-w64-i686-readline
# pacman -S --noconfirm mingw-w64-i686-zlib

#---------------------------------------------------------------------------------
# Get extras
#---------------------------------------------------------------------------------

pacman -S --noconfirm mc
