sudo zypper --non-interactive install -y cross-i386-binutils
sudo zypper --non-interactive install -y gcc-32bit  
sudo zypper --non-interactive install -y libXtst-devel-32bit 
sudo zypper --non-interactive install -y glu-devel-32bit
sudo zypper --non-interactive install -y libgthread-2_0-0-32bit
sudo zypper --non-interactive install -y freetype2-devel-32bit 
sudo zypper --non-interactive install -y libxml2-devel-32bit
sudo zypper --non-interactive install -y libicu-devel-32bit 
sudo zypper --non-interactive install -y libbz2-1-32bit  
sudo zypper --non-interactive install -y libbz2-devel-32bit  
sudo zypper --non-interactive install -y libXxf86vm1-32bit
sudo zypper --non-interactive install -y gtk2-devel-32bit  
sudo zypper --non-interactive install -y gdk-pixbuf-devel-32bit 
sudo zypper --non-interactive install -y cairo-devel-32bit  
sudo zypper --non-interactive install -y pango-devel-32bit
sudo zypper --non-interactive install -y libX11-devel-32bit

sudo ( cd /usr/lib ; ln -s libgobject-2.0.so.0 libgobject-2.0.so )
sudo ( cd /usr/lib ; ln -s libglib-2.0.so.0 libglib-2.0.so )
sudo ( cd /usr/lib ; ln -s libgthread-2.0.so.0 libgthread-2.0.so )
sudo ( cd /usr/lib ; ln -s libgmodule-2.0.so.0 libgmodule-2.0.so )
sudo ( cd /usr/lib ; ln -s libatk-1.0.so.0 libatk-1.0.so )


