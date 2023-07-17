apk add mc

apk add binutils make gcc
apk add openssl
apk add git
apk add g++
apk add 7zip
apk add musl-dev

apk add qt5-qtbase qt5-qtbase-dev qt5-qtx11extras-dev qt5-qtx11extras-dev

apk add qt6-qtbase qt6-qtbase-dev

apk add sdl2
apk add sdl2-dev

apk add sdl2_image
apk add sdl2_mixer
apk add sdl2_net
apk add sdl2_ttf

apk add sdl2_image-dev
apk add sdl2_mixer-dev
apk add sdl2_net-dev
apk add sdl2_ttf-dev

apk add dbus

setup-xorg-base

apk add alpine-desktop

apk add xfce4 xfce4-session
apk add xfce4-panel xfce4-terminal
apk add lightdm-gtk-greeter

# apk add lxdm xfdesktop

rc-update add dbus
setup-devd udev
rc-update add lightdm
# rc-update add lxdm
# apk add firefox
apk add gdk-pixbuf-xlib
apk add gtk+2.0
apk add gtk+3.0

# Add the correct links by apk magic
apk add libx11-dev gdk-pixbuf-dev gdk-pixbuf-xlib-dev gtk+ pango-dev gdb gtk+2.0-dev gcompat

# Or do it as below
ln -s /usr/lib/libX11.so.6 /usr/lib/libX11.so
ln -s /usr/lib/libgdk_pixbuf-2.0.so.0 /usr/lib/libgdk_pixbuf-2.0.so
ln -s /usr/lib/libgdk-x11-2.0.so.0 /usr/lib/libgdk-x11-2.0.so
ln -s /usr/lib/libpango-1.0.so.0 /usr/lib/libpango-1.0.so
ln -s /usr/lib/libgtk-x11-2.0.so.0 /usr/lib/libgtk-x11-2.0.so
ln -s /usr/lib/libcairo.so.2 /usr/lib/libcairo.so
ln -s /usr/lib/libatk-1.0.so.0 /usr/lib/libatk-1.0.so
ln -s /usr/lib/libpangocairo-1.0.so.0 /usr/lib/libpangocairo-1.0.so
ln -s /usr/lib/libgobject-2.0.so.0 /usr/lib/libgobject-2.0.so
ln -s /usr/lib/libglib-2.0.so.0 /usr/lib/libglib-2.0.so
ln -s /usr/lib/libgthread-2.0.so.0 /usr/lib/libgthread-2.0.so
ln -s /usr/lib/libgmodule-2.0.so.0 /usr/lib/libgmodule-2.0.so
