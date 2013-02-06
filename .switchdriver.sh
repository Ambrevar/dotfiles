#!/bin/sh


if [ $(id -u) -ne 0 ]; then
    echo "You must be root to run this script."
    exit
fi


if [ $(pacman -Qi catalyst-utils 2>/dev/null|wc -l) -ge 2 ]; then
    pacman -Rdd --noconfirm catalyst-utils catalyst-dkms lib32-catalyst-utils
    rm -f "/etc/X11/xorg.conf"
    pacman -S --noconfirm ati-dri lib32-ati-dri  xf86-video-ati 
    sed -i 's/nomodeset//' "/boot/syslinux/syslinux.cfg"
    echo
    echo "#### Syslinux entry:"
    grep -A4 "^LABEL arch$" "/boot/syslinux/syslinux.cfg"
else
    pacman -Rs --noconfirm ati-dri lib32-ati-dri  xf86-video-ati 
    pacman -Rdd --noconfirm libgl lib32-libgl 
    pacman -S  --noconfirm catalyst-utils catalyst-dkms lib32-catalyst-utils
    aticonfig --initial
    sed -i '/^LABEL arch$/{n;n;n;s/$/ nomodeset/}' "/boot/syslinux/syslinux.cfg"
    echo
    echo "#### Syslinux entry:"
    grep -A4 "^LABEL arch$" "/boot/syslinux/syslinux.cfg"
fi
