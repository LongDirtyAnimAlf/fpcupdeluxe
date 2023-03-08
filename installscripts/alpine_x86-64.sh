setup-xorg-base
apk add mc xfce4 xfce4-terminal lightdm-gtk-greeter dbus
rc-update add dbus
setup-devd udev
rc-update add lightdm
# apk add firefox
apk add gdk-pixbuf-xlib
apk add gtk+2.0
apk add binutils gcc musl-dev make git 7zip


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
