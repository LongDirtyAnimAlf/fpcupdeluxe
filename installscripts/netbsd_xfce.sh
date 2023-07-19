sudo pkgin -y install xfce4
sudo pkgin -y install xfce4-extras
sudo pkgin -y install xfce4-desktop

sudo pkgin -y install xdg-user-dirs
xdg-user-dirs-update

sudo pkgin -y install dbus
sudo cp /usr/pkg/share/examples/rc.d/dbus /etc/rc.d
sudo echo "dbus=YES" >> /etc/rc.conf
sudo /etc/rc.d/dbus start

sudo pkgin -y install hal
sudo cp /usr/pkg/share/examples/rc.d/hal /etc/rc.d
sudo echo "hal=YES" >> /etc/rc.conf
sudo /etc/rc.d/hal start

sudo pkgin -y install fam
sudo cp /usr/pkg/share/examples/rc.d/famd /etc/rc.d
sudo echo "famd=YES" >> /etc/rc.conf
sudo /etc/rc.d/famd start

# This is perhaps needed for login through xdm
# If not present, your login will be rejected, and the system drops you back to the XDM login screen.
# sudo echo 'DisplayManager*authName: MIT-MAGIC-COOKIE-1' >> /etc/X11/xdm/xdm-conf
# echo 'xdm=YES' >> /etc/rc.conf 

# this must be done for the local user
touch .xsession
# echo 'exec ck-launch-session xfce4-session' > .xsession
echo 'exec xfce4-session' > .xsession
ln -s .xsession .xinitrc
xdg-user-dirs-update
