# https://mirrors.xtom.ee/freebsd-pkg/FreeBSD:10:amd64/latest/

sudo pkg install -y xorg open-vm-tools xf86-video-vmware xf86-input-vmmouse
sudo pkg install -y xfce
sudo pkg install -y xfce4-desktop
# sudo pkg install -y xfce4-session
sudo pkg install -y xfce4-goodies

sudo sysrc hald_enable="YES"
sudo sysrc dbus_enable="YES"
sudo sysrc moused_enable="YES"

if [ $( tty ) = "/dev/ttyv0" ]; then
  startxfce4 --with-ck-launch
fi

