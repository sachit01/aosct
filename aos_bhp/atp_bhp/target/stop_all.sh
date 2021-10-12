#!/bin/bash

cat <<EOF > /tmp/stop_linux.sh
#!/bin/bash
echo "[+] Linux: Trying to stop all programs matching '\$1'"
for i in \`ps aux | grep "\${1}" | grep -v grep | grep -v stop_linux | awk '{print \$1}' \`; do
    echo -n "[+] Killing: "
    ps aux  | grep \$i | grep -v grep
    kill -9 \$i
done    
EOF


cat <<EOF > /tmp/stop_freebsd.sh
#!/bin/bash
echo "[+] FreeBSD: Trying to stop all programs matching '\$1'"
for i in \`ps auxw | grep "\${1}" | grep -v grep | grep -v stop_freebsd | awk '{print \$2}' \`; do
    echo -n "[+] Killing: "
    ps auxw  | grep \$i | grep -v grep
    kill -9 \$i
done    
EOF

SSHPARM="-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"

sshpass -p "admin" scp ${SSHPARM} /tmp/stop_linux.sh root@192.168.2.10:/tmp/stop_linux.sh
sshpass -p "admin" scp ${SSHPARM} /tmp/stop_freebsd.sh root@192.168.2.11:/tmp/stop_freebsd.sh
sshpass -p "admin" scp ${SSHPARM} /tmp/stop_linux.sh root@192.168.2.12:/tmp/stop_linux.sh

sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.10 bash /tmp/stop_linux.sh startup_system
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.10 bash /tmp/stop_linux.sh atp_bhp
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.10 bash /tmp/stop_linux.sh vfwChanneld
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.10 bash /tmp/stop_linux.sh vioserver
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.10 bash /tmp/stop_linux.sh /opt/vfw/run/rt
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.10 bash /tmp/stop_linux.sh SDP_A

sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.11 bash /tmp/stop_freebsd.sh startup_system
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.11 bash /tmp/stop_freebsd.sh atp_bhp
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.11 bash /tmp/stop_freebsd.sh vfwChanneld
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.11 bash /tmp/stop_freebsd.sh vioserver
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.11 bash /tmp/stop_freebsd.sh /opt/vfw/run/rt
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.11 bash /tmp/stop_freebsd.sh SDP_B
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.11 bash /tmp/stop_freebsd.sh \"\ nc\ \"

sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.12 bash /tmp/stop_linux.sh startup_system
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.12 bash /tmp/stop_linux.sh dispatcher
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.12 bash /tmp/stop_linux.sh vfwChanneld
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.12 bash /tmp/stop_linux.sh vioserver
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.12 bash /tmp/stop_linux.sh /opt/vfw/run/rt
