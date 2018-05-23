#!/bin/sh

if [ "$1" = "-h" ]; then
	cat <<EOF>&2
Usage: ${0##*/}

Automatically mount external drives 'udisksctl'.

From https://wiki.archlinux.org/index.php/Udisks#udevadm_monitor.

EOF
	exit
fi

pathtoname() {
	udevadm info -p /sys/"$1" | awk -v FS== '/DEVNAME/ {print $2}'
}

stdbuf -oL -- udevadm monitor --udev -s block | while read -r -- _ _ event devpath _; do
	if [ "$event" = add ]; then
		devname=$(pathtoname "$devpath")
		udisksctl mount --block-device "$devname" --no-user-interaction
	fi
done
