sudo setup-xorg-base
sudo apk add mc xfce4 xfce4-terminal lightdm-gtk-greeter dbus
sudo rc-update add dbus
sudo setup-devd udev
sudo rc-update add lightdm
# sudo apk add firefox
sudo apk add gdk-pixbuf-xlib
sudo apk add gtk+2.0
sudo apk add binutils gcc musl-dev make git 7zip

sudo ln -s /usr/lib/libX11.so.6 /usr/lib/libX11.so
sudo ln -s /usr/lib/libgdk_pixbuf-2.0.so.0 /usr/lib/libgdk_pixbuf-2.0.so
sudo ln -s /usr/lib/libgdk-x11-2.0.so.0 /usr/lib/libgdk-x11-2.0.so
sudo ln -s /usr/lib/libpango-1.0.so.0 /usr/lib/libpango-1.0.so
sudo ln -s /usr/lib/libgtk-x11-2.0.so.0 /usr/lib/libgtk-x11-2.0.so
sudo ln -s /usr/lib/libcairo.so.2 /usr/lib/libcairo.so
sudo ln -s /usr/lib/libatk-1.0.so.0 /usr/lib/libatk-1.0.so
sudo ln -s /usr/lib/libpangocairo-1.0.so.0 /usr/lib/libpangocairo-1.0.so
sudo ln -s /usr/lib/libgobject-2.0.so.0 /usr/lib/libgobject-2.0.so
sudo ln -s /usr/lib/libglib-2.0.so.0 /usr/lib/libglib-2.0.so
sudo ln -s /usr/lib/libgthread-2.0.so.0 /usr/lib/libgthread-2.0.so
sudo ln -s /usr/lib/libgmodule-2.0.so.0 /usr/lib/libgmodule-2.0.so
