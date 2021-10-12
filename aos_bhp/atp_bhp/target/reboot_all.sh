#!/bin/bash

SSHPARM="-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"

for ip in 192.168.2.10 192.168.2.11 192.168.2.12; do
    echo "[+] reboot system on ${ip}"
    sshpass -p "admin" ssh ${SSHPARM} root@${ip} /sbin/reboot
done
