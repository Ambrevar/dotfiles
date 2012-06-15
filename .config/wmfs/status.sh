#!/bin/bash
################################################################################
## WMFS Config -- Status Bar Script
## Date 2012-01-07
##
## Author: Ambrevar <ambrevar at gmail dot com>
## Adapted from AddiKT1ve <the.addikt1ve@gmail.com>
################################################################################

# Prevent script from running multiple time.
# WARNING: it will kill all process with 'wmfs' and 'status.sh' in their names!
kill $(ps U $UID | awk '/wmfs/&&/status.sh/' | grep -vi "$$\|grep\|awk" | awk '{print $1}')


## mpd info
## <mpc> is required for "now playing" informations
_mpd() {
	if [ "`mpc 2>&1 | wc -l`" -gt "1" ]; then
		if [ "`mpc | grep "^\[paused\]"`" != "" ]; then
			mpd_current="`mpc current` [pause]"
		else
			mpd_current=`mpc current`
		fi
	else
		mpd_current="\o/"
	fi
	mpd="\\#cba642\\$mpd_current"
}

# mocp author info
# <mocp> is required for "now playing" informations
_mocp_author(){
	author=`mocp -i | grep Artist | awk -F ": " '{print $2}'`
	mocp_author="\\#cba642\\$author"
}

# mocp author info
# <mocp> is required for "now playing" informations
_mocp_song(){
	SONG=`mocp -i | grep SongTitle | awk -F ": " '{print $2}'`
}

# network
# network usage stats
_network() {
	# Variables
	ethiface=eth0
	wlaniface=wlan0
	tmpdir=/tmp

	# Functions
	function rx_bytes # download
	{
		[[ -e "/sys/class/net/$1/statistics/rx_bytes" ]] \
		&& echo $(cat /sys/class/net/$1/statistics/rx_bytes)
	}
	function tx_bytes # upload
	{ 
		[[ -e "/sys/class/net/$1/statistics/tx_bytes" ]] \
		&& echo $(cat /sys/class/net/$1/statistics/tx_bytes)
	}

	# Download
    lastrxbytes=0
    if [ -f "$tmpdir/last_rxbytes" ]; then
	    lastrxbytes=$(cat "$tmpdir/last_rxbytes")
    fi
	# Upload
	lasttxbytes=0
    if [ -f "$tmpdir/last_txbytes" ]; then
	    lasttxbytes=$(cat "$tmpdir/last_txbytes")
    fi

	# Download
	rxbytes=$(rx_bytes $ethiface)
	rxresult=$((($rxbytes-lastrxbytes)/1000))
	echo $rxbytes > "$tmpdir/last_rxbytes"

	# Upload
	txbytes=$(tx_bytes $ethiface)
	txresult=$((($txbytes-lasttxbytes)/1000))
	echo $txbytes > "$tmpdir/last_txbytes"

	# Output
	network="\\#81ae51\\↓ $rxresult Ko/s | $txresult Ko/s ↑\\#ffffff\\"
}

# battery state
_battery() {
	if [ -e /sys/class/power_supply/BAT*/status ]; then

		bat_percent=$((`cat /sys/class/power_supply/BAT*/energy_now`/`cat /sys/class/power_supply/BAT*/energy_full_design | sed 's/00$//'`))
		bat_acpi=`cat /sys/class/power_supply/BAT*/status`

		# use an arrow to show if battery is charging, discharging or full/AC
		if [ "$bat_acpi" = "Discharging" ]; then
			bat_state="↓"
		elif [ "$bat_acpi" = "Charging" ]; then
			bat_state="↑"
		fi

		# blinking battery percent indicator if bat_percent < 15
		if [ "$bat_percent" -lt "15" ]; then
			bat_fail=1
			if [ "`cat /tmp/batteryfail`" ]; then
				color="\\#ff6b6b\\"
				echo 0 > /tmp/batteryfail
			else
				color="\\#435e87\\"
				echo 1 > /tmp/batteryfail
			fi
		else
			bat_fail=0
			color="\\#C0C0C0\\"
		fi

		## battery time
		## <acpi> is required
		# bat_remtime="`acpi | cut -d' ' -f5 | cut -d':' -f1,2`"

		battery="$color Bat. $bat_percent% $bat_statei\\#ffffff\\"
	else
		battery="$color On sector\\#ffffff\\"
	fi
}

# uptime
_uptime() {
	uptime=`cut -d'.' -f1 /proc/uptime`
	secs=$((${uptime}%60))
	mins=$((${uptime}/60%60))
	hours=$((${uptime}/3600%24))
	days=$((${uptime}/86400))
	uptime="${mins}m ${secs}s"

	if [ "${hours}" -ne "0" ]; then
		uptime="${hours}h ${uptime}"
	fi

	if [ "${days}" -ne "0" ]; then
		uptime="${days}d ${uptime}"
	fi

	uptime="\\#ff8200\\${uptime}\\#ffffff\\"
}

# memory usage
_memory() {
	memory_used="`free -m | sed -n 's|^-.*:[ \t]*\([0-9]*\) .*|\1|gp'`"
	memory_total="`free -m | sed -n 's|^M.*:[ \t]*\([0-9]*\) .*|\1|gp'`"
	memory="$memory_used/$memory_total Mo"
}

# volume
# <amixer> is required
_volume() {
	if [ "`amixer get Master | grep '\[off\]$'`" = "" ]; then
		volume=`amixer get Master | sed -n 's|.*\[\([0-9]*\)\%.*|\1%|pg'`
	else
		volume="[off]"
	fi
	volume="\\#47B6cA\\Vol. ${volume}\\#ffffff\\"
}

# date
_date() {
	sys_date=`date '+%a %d %b %Y'`
	date="\\#ff6b6b\\$sys_date\\#ffffff\\"
}

# Hour
_hour() {
	sys_hour=`date '+%H:%M'`
	hour="\\#1793d1\\$sys_hour \\#ffffff\\ "
}

# ompload
#
# <ompload> is required
# <cropscreen.sh> is required
_ompload() {
	[ -e /tmp/omploadurl ] && ompload_url=`cat /tmp/omploadurl`
	ompload="$ompload_url"
}

# CPU Usage
# <conky> is required
_cpuusage() {
	cpuusage="CPU "
	for i in 1 2 3 4
	do
		cpuusage+="$(conky -i2 -u 0.5 |tail -n5|grep CPU$i|cut -f2 -d':') "
	done
	cpuusage="\\#fec023\\$cpuusage\\#ffffff\\ "
}


# HD Usage
# <conky> is required
_diskusage() {
	diskusage="HDD "
	diskusage+="$(conky -i2 -u 0.5|tail -n5|grep HDD|cut -f2 -d':') "
}

# Separator
# For appearance only
_separator() {
	separator="•"
}


# concatenate arguments
statustext() {
	args=""
	for arg in $@; do
		_${arg}
		args="${args} `eval echo '$'$arg`"
	done

	# wmfs magic
	wmfs -s "$args"
}

################################################################################
## status text
##
## add <variables> from the above definition without underscore.
## Example:
## while true; do statustext volume separator date separator hour separator ; sleep 1; done
################################################################################

# The status will be constantly updated until WMFS is closed.
while [ "$(ps U $UID | awk '{print $5}' | grep ^wmfs$)" != "" ] ; do
	statustext  battery separator diskusage separator cpuusage separator network separator volume separator date separator hour separator 
	sleep 1
done

