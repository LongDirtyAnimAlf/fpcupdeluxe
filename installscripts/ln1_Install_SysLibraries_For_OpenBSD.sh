# =============================================================
#               CodeTyphon Studio
#      Copyright (c) PilotLogic Software House.
#               All rights reserved.
#
#     This Script Install to OpenBSD OS
#     base libraries to Build and Run CodeTyphon
# =============================================================
#Update 18-10-2020 for OpenBSD 6.8
#Update 04-04-2021 for OpenBSD 6.9
#Update 15-10-2021 for OpenBSD 7.0
#==============================================================


ciplatiform=$1
cicpubits=$2
cicpuname=$3
ciUseMultiArch=$4
ciInstallALL=$5

#========================================================

echo "----------------------------------------------------"
echo " CodeTyphon OS Libraries Installation for" 
echo "                OpenBSD" 
echo "----------------------------------------------------"
echo "   "

sudo /sbin/ldconfig -m -v

sudo pkg_add -r xtermset
#sudo pkg_add -r zip 
#sudo pkg_add -r unzip
sudo pkg_add -r p7zip   
sudo pkg_add -r wget

sudo pkg_add -r gcc
sudo pkg_add -r gdb
sudo pkg_add -r lldb
sudo pkg_add -r binutils
sudo pkg_add -r gmake

sudo pkg_add -r libiconv
sudo pkg_add -r xorg
sudo pkg_add -r libx11
sudo pkg_add -r libXtst

sudo pkg_add -r xorg-fonts-type1
sudo pkg_add -r liberation-fonts-ttf


# Install libraries for GTK2
if [ "$ciplatiform" = 0 ] || [ "$ciInstallALL" = 1 ] ;
then
    echo "   "
    echo "[INFO] Install libraries for GTK2"
    echo "   "
    sudo pkg_add -r gtk+2
    sudo pkg_add -r gtkglext
fi

# Install libraries for QT4
if [ "$ciplatiform" = 1 ] || [ "$ciInstallALL" = 1 ] ;
then
    echo "   "
    echo "[INFO] Install libraries for QT4"
    echo "   "
    sudo pkg_add -r qt4  
fi

# Install libraries for GTK3
if [ "$ciplatiform" = 3 ] || [ "$ciInstallALL" = 1 ] ;
then
    echo "   "
    echo "[INFO] Install libraries for GTK3"
    echo "   "
    sudo pkg_add -r gtk+3
fi

# Install libraries for GTK4
if [ "$ciplatiform" = 9 ] || [ "$ciInstallALL" = 1 ] ;
then
    echo "   "
    echo "[INFO] Install libraries for GTK4"
    echo "   "
    sudo pkg_add -r gtk+4
fi

# Install libraries for QT5
if [ "$ciplatiform" = 7 ] || [ "$ciInstallALL" = 1 ] ;
 then
    echo "   "
    echo "[INFO] Install libraries for QT5"
    echo "   "
    sudo pkg_add -r qt5
    sudo pkg_add -r qt5-global
fi


# =============================================
# =============================================
# =============================================

doversionxx()
{
echo "   "
echo "[ERROR] Can NOT find OpenBSD Version ?????????" 
echo "   "
}

doversion68()
{
echo "   "
echo "[INFO] make for OpenBSD ver 6.8 some missing links......" 
echo "   "

sudo ln -s /usr/lib/libc.so.96.0 /usr/local/lib/libc.so
sudo ln -s /usr/lib/libpthread.so.26.1 /usr/local/lib/libpthread.so
sudo ln -s /usr/lib/libz.so.5.0 /usr/local/lib/libz.so

sudo ln -s /usr/X11R6/lib/libX11.so.17.1 /usr/local/lib/libX11.so
sudo ln -s /usr/X11R6/lib/libXtst.so.11.0 /usr/local/lib/libXtst.so
sudo ln -s /usr/X11R6/lib/libfreetype.so.30.0 /usr/local/lib/libfreetype.so
sudo ln -s /usr/X11R6/lib/libXxf86vm.so.6.0 /usr/local/lib/libXxf86vm.so
sudo ln -s /usr/X11R6/lib/libGL.so.17.1 /usr/local/lib/libGL.so
sudo ln -s /usr/X11R6/lib/libGLU.so.9.0 /usr/local/lib/libGLU.so

sudo ln -s /usr/local/lib/libatk-1.0.so.21809.4 /usr/local/lib/libatk-1.0.so
sudo ln -s /usr/local/lib/libgthread-2.0.so.4200.11 /usr/local/lib/libgthread-2.0.so
sudo ln -s /usr/local/lib/libgmodule-2.0.so.4200.11 /usr/local/lib/libgmodule-2.0.so
sudo ln -s /usr/local/lib/libgobject-2.0.so.4200.11 /usr/local/lib/libgobject-2.0.so
sudo ln -s /usr/local/lib/libgdk_pixbuf-2.0.so.3200.2 /usr/local/lib/libgdk_pixbuf-2.0.so
sudo ln -s /usr/local/lib/libgtk-x11-2.0.so.2400.0 /usr/local/lib/libgtk-x11-2.0.so
sudo ln -s /usr/local/lib/libgdk-x11-2.0.so.2400.0 /usr/local/lib/libgdk-x11-2.0.so
sudo ln -s /usr/local/lib/libglib-2.0.so.4201.4 /usr/local/lib/libglib-2.0.so
sudo ln -s /usr/local/lib/libcairo.so.13.0 /usr/local/lib/libcairo.so
sudo ln -s /usr/local/lib/libpango-1.0.so.3801.1 /usr/local/lib/libpango-1.0.so
sudo ln -s /usr/local/lib/libpangocairo-1.0.so.3801.1 /usr/local/lib/libpangocairo-1.0.so
sudo ln -s /usr/local/lib/libgtkglext-x11-1.0.so.0.0 /usr/local/lib/libgtkglext-x11-1.0.so
sudo ln -s /usr/local/lib/libgdkglext-x11-1.0.so.0.0 /usr/local/lib/libgdkglext-x11-1.0.so

}

doversion69()
{
echo "   "
echo "[INFO] make for OpenBSD ver 6.9 some missing links......" 
echo "   "

sudo ln -s /usr/lib/libc.so.96.0 /usr/local/lib/libc.so
sudo ln -s /usr/lib/libpthread.so.26.1 /usr/local/lib/libpthread.so
sudo ln -s /usr/lib/libz.so.5.0 /usr/local/lib/libz.so

sudo ln -s /usr/X11R6/lib/libX11.so.17.1 /usr/local/lib/libX11.so.6
sudo ln -s /usr/X11R6/lib/libXtst.so.11.0 /usr/local/lib/libXtst.so
sudo ln -s /usr/X11R6/lib/libfreetype.so.30.0 /usr/local/lib/libfreetype.so
sudo ln -s /usr/X11R6/lib/libXxf86vm.so.6.0 /usr/local/lib/libXxf86vm.so
sudo ln -s /usr/X11R6/lib/libGL.so.17.1 /usr/local/lib/libGL.so
sudo ln -s /usr/X11R6/lib/libGLU.so.9.0 /usr/local/lib/libGLU.so

sudo ln -s /usr/local/lib/libatk-1.0.so.21809.4 /usr/local/lib/libatk-1.0.so
sudo ln -s /usr/local/lib/libgthread-2.0.so.4200.12 /usr/local/lib/libgthread-2.0.so.0
sudo ln -s /usr/local/lib/libgmodule-2.0.so.4200.12 /usr/local/lib/libgmodule-2.0.so.0
sudo ln -s /usr/local/lib/libgobject-2.0.so.4200.12 /usr/local/lib/libgobject-2.0.so.0
sudo ln -s /usr/local/lib/libgdk_pixbuf-2.0.so.3200.3 /usr/local/lib/libgdk_pixbuf-2.0.so.0
sudo ln -s /usr/local/lib/libgtk-x11-2.0.so.2400.0 /usr/local/lib/libgtk-x11-2.0.so
sudo ln -s /usr/local/lib/libgdk-x11-2.0.so.2400.0 /usr/local/lib/libgdk-x11-2.0.so
sudo ln -s /usr/local/lib/libglib-2.0.so.4201.5 /usr/local/lib/libglib-2.0.so.0
sudo ln -s /usr/local/lib/libcairo.so.13.0 /usr/local/lib/libcairo.so
sudo ln -s /usr/local/lib/libpango-1.0.so.3801.2 /usr/local/lib/libpango-1.0.so.0
sudo ln -s /usr/local/lib/libpangocairo-1.0.so.3801.2 /usr/local/lib/libpangocairo-1.0.so.0
sudo ln -s /usr/local/lib/libgtkglext-x11-1.0.so.0.0 /usr/local/lib/libgtkglext-x11-1.0.so
sudo ln -s /usr/local/lib/libgdkglext-x11-1.0.so.0.0 /usr/local/lib/libgdkglext-x11-1.0.so

}

doversion70()
{
echo "   "
echo "[INFO] make for OpenBSD ver 7.0 some missing links......" 
echo "   "

sudo ln -s /usr/lib/libc.so.96.1 /usr/local/lib/libc.so.6
sudo ln -s /usr/lib/libpthread.so.26.1 /usr/local/lib/libpthread.so
sudo ln -s /usr/lib/libz.so.6.0 /usr/local/lib/libz.so.1

sudo ln -s /usr/X11R6/lib/libX11.so.17.1 /usr/local/lib/libX11.so.6
sudo ln -s /usr/X11R6/lib/libXtst.so.11.0 /usr/local/lib/libXtst.so
sudo ln -s /usr/X11R6/lib/libfreetype.so.30.0 /usr/local/lib/libfreetype.so
sudo ln -s /usr/X11R6/lib/libXxf86vm.so.6.0 /usr/local/lib/libXxf86vm.so
sudo ln -s /usr/X11R6/lib/libGL.so.17.1 /usr/local/lib/libGL.so
sudo ln -s /usr/X11R6/lib/libGLU.so.9.0 /usr/local/lib/libGLU.so

sudo ln -s /usr/local/lib/libatk-1.0.so.21809.4 /usr/local/lib/libatk-1.0.so
sudo ln -s /usr/local/lib/libgthread-2.0.so.4200.13 /usr/local/lib/libgthread-2.0.so.0
sudo ln -s /usr/local/lib/libgmodule-2.0.so.4200.13 /usr/local/lib/libgmodule-2.0.so.0
sudo ln -s /usr/local/lib/libgobject-2.0.so.4200.13 /usr/local/lib/libgobject-2.0.so.0
sudo ln -s /usr/local/lib/libgdk_pixbuf-2.0.so.3200.3 /usr/local/lib/libgdk_pixbuf-2.0.so.0
sudo ln -s /usr/local/lib/libgtk-x11-2.0.so.2400.0 /usr/local/lib/libgtk-x11-2.0.so
sudo ln -s /usr/local/lib/libgdk-x11-2.0.so.2400.0 /usr/local/lib/libgdk-x11-2.0.so
sudo ln -s /usr/local/lib/libglib-2.0.so.4201.6 /usr/local/lib/libglib-2.0.so.0
sudo ln -s /usr/local/lib/libcairo.so.13.0 /usr/local/lib/libcairo.so
sudo ln -s /usr/local/lib/libpango-1.0.so.3801.2 /usr/local/lib/libpango-1.0.so.0
sudo ln -s /usr/local/lib/libpangocairo-1.0.so.3801.2 /usr/local/lib/libpangocairo-1.0.so.0
sudo ln -s /usr/local/lib/libgtkglext-x11-1.0.so.0.0 /usr/local/lib/libgtkglext-x11-1.0.so
sudo ln -s /usr/local/lib/libgdkglext-x11-1.0.so.0.0 /usr/local/lib/libgdkglext-x11-1.0.so

}

case $(uname -r) in 
 *6.8*)
    doversion68
    ;; 
 *6.9*)
    doversion69
    ;; 
 *7.0*)
    doversion70
    ;; 
 *)
    doversionxx
    ;;
    
esac
# =============================================
# =============================================
# =============================================

sudo /sbin/ldconfig -m -v


echo "----------------------------------------------------"
echo "CodeTyphon OS Libraries Installation"
echo "Finish !!!"

#sleep 5
