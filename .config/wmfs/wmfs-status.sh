#!/bin/bash
################################################################################
## WMFS Status bar
## Date: 2012-03-02
################################################################################

################################################################################
## Options
################################################################################

TIMING=1

#colors
default="#222222"
green="#4E9A06"
lightgreen="#6DDD00"
grey="#7D7D7D"
dark="#1A1A1A"
dblue="#1874cd"
blue="#63b8ff"
red="#CC0000"
orange="#FFB000"
purple="#8E00FF"

# separator
sep="^R[right;2;10;$default]"

# Terminal command
# termcmd="urxvtc -e "
termcmd="lxterminal -e "

################################################################################
## Commands
################################################################################

# power
pwr(){
	if [ -e /sys/class/power_supply/BAT*/status ]; then
        pwrperc="$(awk 'sub(/,/,"") {print $4}' <(acpi -b) | cut -d , -f 1 $1)"
        if [ "$pwrperc" == "100%" ]; then
            pwr="100"
        else
            pwr="$(echo $pwrperc | cut -c1-2)"
        fi
        echo "^s[70;10;$grey;bat]^p[90;2;40;8;0;$pwr;100;$dark;$dblue]^R[110;2;1;10;$default]"
    fi
}

# cpugraph
cpugraph(){
    cpu="$(eval $(awk '/^cpu /{print "previdle=" $5 "; prevtotal=" $2+$3+$4+$5 }' /proc/stat); sleep 0.4;
	      eval $(awk '/^cpu /{print "idle=" $5 "; total=" $2+$3+$4+$5 }' /proc/stat);
	      intervaltotal=$((total-${prevtotal:-0}));
	      echo "$((100*( (intervaltotal) - ($idle-${previdle:-0}) ) / (intervaltotal) ))")"
    echo "^s[140;10;$grey;core]^g[170;2;80;8;$cpu;100;$dark;$orange;cpugraph](1;spawn;$termcmd htop)"
}

# membar
membar(){
    memu="$(free -m | sed -n 's|^-.*:[ \t]*\([0-9]*\) .*|\1|gp')"
    memt="$(free -m | sed -n 's|^M.*:[ \t]*\([0-9]*\) .*|\1|gp')"
    echo "^s[260;10;$grey;mem ]^p[290;2;80;8;0;$memu;$memt;$dark;$lightgreen](1;spawn;$termcmd htop)^R[330;2;1;10;$default]"
}

# hdd section
hdd(){
    hdd="$(df -h|grep sda3|awk '{print $5}' | cut -c1-2)"
    echo "^s[390;10;$grey;hdd]^p[415;2;40;8;0;$hdd;100;$dark;$grey](1;spawn;$termcmd ncdu)"
}

# internet section
# netup(){
#     netup="$("$HOME/bin/speedup.sh")" 
#     echo "^s[465;10;$grey;net ]^p[490;2;100;4;0;$netup;150;$dark;$dblue](1;spawn;$termcmd net-monitor)" 
# }

# netdown(){ 
#     netdown="$("$HOME/bin/speeddown.sh")" 
#     echo "^p[490;7;100;4;0;$netdown;2200;$dark;$blue](1;spawn;$termcmd net-monitor)" 
# }

# date/time section
day(){
    day="$(date +"%d")"
    echo "^s[630;10;$grey;date]^p[660;2;100;4;0;$day;31;$dark;$grey]"
}
month(){
    month="(date +"%m")"
    echo "^p[660;7;100;4;0;$month;12;$dark;$grey]"
}
hour(){
    hour="$(date +"%I")"
    echo "^s[770;10;$grey;time ]^p[800;2;80;4;0;$hour;12;$dark;$green]"
}
minute(){
    minute="$(date +"%M")"
    echo "^p[800;7;80;4;0;$minute;60;$dark;$green]^R[820;0;1;12;$default]^R[840;0;1;12;$default]^R[860;0;1;12;$default]"
}

# sound
volpcm(){
    volpcm="$(amixer get PCM | tail -1 | sed 's/.*\[\([0-9]*%\)\].*/\1/')"
        if [ "$volpcm" == "100%" ]; then
            pcm="100"
        else
            pcm="$(echo $volpcm | cut -c1-2)"
        fi
    echo "^s[10;10;$grey;vol ]^p[30;2;50;4;0;$pcm;100;$dark;$dblue]"
}



statustext()
{
    wmfs -c status "topbar $(pwr) $(cpugraph) $(membar) $(hdd) $(day) $(month) $(hour) $(minute)"
     wmfs -c status "bottombar $(volpcm)"
}

while true;
do
    statustext
    sleep $TIMING
done


################################################################################
## Not used
################################################################################

# MOC
# music(){
#     music="$(conky -c ~/.config/wmfs/conkyrc_mocp)"
#     echo "^s[270;10;#7D7D7D;$music]"
# }

# mocp control
# mocpctrl(){
#     echo "^s[90;9;$grey;mocp](1;spawn;urxvt -e mocp) ^R[125;1;16;10;$dark]^s[126;10;$grey;<<](1;spawn;mocp -r) ^R[142;1;24;10;$dark]^s[150;10;$grey;>I](1;spawn;mocp -p) ^R[167;1;16;10;$dark]^s[168;10;$grey;>>](1;spawn;mocp -f) ^R[184;1;16;10;$dark]^s[188;9;$grey;II](1;spawn;mocp -G)"
# }

# mocpbar
# mocpbar(){
#     if [ "$(pidof mocp)" ]; then
#         total="$(mocp -Q %ts)"
#         curr="$(mocp -Q %cs)"
#     else
#         total="20"
#         curr="0"
#     fi
#     echo "^p[210;1;50;10;1;$curr;$total;$dark;$grey]"
# }        

## Mail
# email(){
#     if [ "$(pidof claws-mail)" ];then
#         mail="$(claws-mail --status | awk '{print $2}')"
#     else
#         mail="0"
#     fi
#     echo "^s[20;10;$grey;mail]^s[45;10;$green;$mail]"
# }


## Volume
# volibm(){
#     volume="$(conky -c ~/.config/wmfs/conkyrc_sound)"
#     if [ "$volume" == "mute" ]; then
#         ibm="0"
#     else
#         ibm="$(conky -c ~/.config/wmfs/conkyrc_sound | awk '{printf $1*100/14}' | cut -d . -f 1 $1)"
#     fi
#     echo "^p[30;7;50;4;0;$ibm;100;$dark;$blue]"
# }
