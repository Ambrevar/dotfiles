#!/bin/bash
# speedup.sh
# sources <https://bitbucket.org/jasonwryan/workstation/src/d2045f97201e/scripts/speed.sh>

TXB=$(cat /sys/class/net/eth0/statistics/tx_bytes)
sleep 2 
TXBN=$(cat /sys/class/net/eth0/statistics/tx_bytes)
TXDIF=$(echo $((TXBN - TXB)) )

echo "$((TXDIF / 1024 / 2))"
