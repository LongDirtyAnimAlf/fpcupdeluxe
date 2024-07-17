sudo pkg_add at-spi2-core

sudo pkg_add install xfce
sudo pkg_add install xfce-extras
sudo pkg_add install xfce-desktop

sudo sed -i 's/xconsole/#xconsole/' /etc/X11/xenodm/Xsetup_0

sudo pkg_add xfce4-power-manager

sudo pkg_add install xdg-user-dirs
xdg-user-dirs-update

rcctl enable messagebus
rcctl start messagebus
rcctl enable apmd
rcctl start apmd

# this must be done for the local user
touch .xsession
# echo 'exec ck-launch-session xfce4-session' > .xsession
echo 'exec xfce4-session' > .xsession
ln -s .xsession .xinitrc
xdg-user-dirs-update
