#!/bin/bash

SSHPARM="-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"

LOGDIR="logs"

CPUS="CPU-A CPU-B CPU-C"

for CPU in ${CPUS}; do
    mkdir -p ${LOGDIR}/${CPU}
done

# Gather data from CPU-A
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.10 'tar -cf - /optdata/logs  | gzip -9 > /tmp/messages.tar.gz'
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.10 'tar -cf - /tmp/*.log | gzip -9 > /tmp/logs.tar.gz'
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.10 'tar -cf - /optdata/coredumps/*.core | gzip -9 > /tmp/coredumps.tar.gz'
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.10 'tar -cf - /opt/bin/*.sh /opt/bin/aos/*.sh /opt/bin/aos/atp_* /opt/run/vioserver /opt/run/SDP_*  | gzip -9 > /tmp/binaries.tar.gz'

# Gather data from CPU-B
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.11 'tar -cf - /tmp/*.log | gzip -9 > /tmp/logs.tar.gz'
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.11 'tar -cf - /optdata/logs  | gzip -9 > /tmp/messages.tar.gz'
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.11 'tar -cf - /optdata/coredumps/*.core | gzip -9 > /tmp/coredumps.tar.gz'
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.11 'tar -cf - /opt/bin/*.sh /opt/bin/aos/*.sh /opt/bin/aos/atp_* /opt/run/vioserver /opt/run/SDP_*  | gzip -9 > /tmp/binaries.tar.gz'

# Gather data from CPU-C
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.12 'tar -cf - /optdata/bds  | gzip -9 > /tmp/bds.tar.gz'
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.12 'tar -cf - /optdata/logs  | gzip -9 > /tmp/messages.tar.gz'
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.12 'tar -cf - /tmp/*.log | gzip -9 > /tmp/logs.tar.gz'
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.12 'tar -cf - /optdata/coredumps/*.core | gzip -9 > /tmp/coredumps.tar.gz'
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.12 'tar -cf - /opt/bin/*.sh /opt/bin/aos/*.sh /opt/bin/aos/disp* | gzip -9 > /tmp/binaries.tar.gz'

# Get all archives from CPU-A/B/C
sshpass -p "admin" scp ${SSHPARM} root@192.168.2.10:/tmp/*.tar.gz ${LOGDIR}/CPU-A
sshpass -p "admin" scp ${SSHPARM} root@192.168.2.11:/tmp/*.tar.gz ${LOGDIR}/CPU-B
sshpass -p "admin" scp ${SSHPARM} root@192.168.2.12:/tmp/*.tar.gz ${LOGDIR}/CPU-C

# Delete the archives
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.10 "rm -f /tmp/*.tar.gz"
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.11 "rm -f /tmp/*.tar.gz"
sshpass -p "admin" ssh ${SSHPARM} root@192.168.2.12 "rm -f /tmp/*.tar.gz"

# Unpack the archives
if [ "$1" == "unpack" ]; then
    for CPU in ${CPUS}; do
        (cd ${LOGDIR}/${CPU}; ls *.tar.gz | xargs -l -- tar -xvzf)
    done
else
    ls -l ${LOGDIR}/*/*
fi
