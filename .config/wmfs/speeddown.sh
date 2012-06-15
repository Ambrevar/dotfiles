#!/bin/bash
# speeddown.sh
# sources <https://bitbucket.org/jasonwryan/workstation/src/d2045f97201e/scripts/speed.sh>

RXB=$(cat /sys/class/net/eth0/statistics/rx_bytes)
sleep 2 
RXBN=$(cat /sys/class/net/eth0/statistics/rx_bytes)
RXDIF=$(echo $((RXBN - RXB)) )

echo "$((RXDIF / 1024 / 2))"
