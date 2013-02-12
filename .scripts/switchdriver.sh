#!/bin/sh

if [ -z "$(lspci|grep "VGA.*Radeon")" ]; then
    echo "You must have a Radeon graphic adapter. Exit."
    exit
fi


if [ $(id -u) -ne 0 ]; then
    echo "You must be root to run this script. Exit."
    exit
fi


if [ $(pacman -Qi catalyst-utils 2>/dev/null|wc -l) -ge 2 ]; then
    pacman -Rdd --noconfirm catalyst-utils catalyst-dkms lib32-catalyst-utils
    rm -f "/etc/X11/xorg.conf"
    pacman -S --noconfirm ati-dri lib32-ati-dri  xf86-video-ati 
    if [ -f "/boot/syslinux/syslinux.cfg" ]; then
        sed -i 's/nomodeset//' "/boot/syslinux/syslinux.cfg"
        echo
        echo "#### Syslinux entry:"
        grep -A4 "^LABEL arch$" "/boot/syslinux/syslinux.cfg"
    else
        echo "You do not seem to use Syslinux. No configuration done."
        echo "You have to remove 'nomodeset' kernel parameter manually."
    fi
else
    pacman -Rs --noconfirm ati-dri lib32-ati-dri  xf86-video-ati 
    pacman -Rdd --noconfirm libgl lib32-libgl 
    pacman -S  --noconfirm catalyst-utils catalyst-dkms lib32-catalyst-utils
    aticonfig --initial
    if [ -f "/boot/syslinux/syslinux.cfg" ]; then
        sed -i '/^LABEL arch$/{n;n;n;s/$/ nomodeset/}' "/boot/syslinux/syslinux.cfg"
        echo
        echo "#### Syslinux entry:"
        grep -A4 "^LABEL arch$" "/boot/syslinux/syslinux.cfg"
    else
        echo "You do not seem to use Syslinux. No configuration done."
        echo "You have to add 'nomodeset' kernel parameter manually."
    fi
fi
