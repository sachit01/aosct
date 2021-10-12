#!/bin/bash

SSHPARM="-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"

for ip in 192.168.2.10 192.168.2.11 192.168.2.12; do
    echo "[+] startup_system on ${ip}"
    sshpass -p "atpcu" ssh ${SSHPARM} atpcu@${ip} /opt/bin/startup_system.sh&
done
