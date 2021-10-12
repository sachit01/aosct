#!/bin/bash

CONFIGS="cfg_data dispatcher_data instance_data mnt_data type_data rt_data"

if [ -z "$1" ]; then echo "Usage: $0 hil|vsim|emd [cpu_a|cpu_b|cpu_c|config]"; exit 1; fi

if [ -z "$2" ]; then PACKAGES="cpu_a cpu_b cpu_c ${CONFIGS}"; else PACKAGES="$2"; fi

if [ "${PACKAGES}" == "config" ]; then
    PACKAGES=${CONFIGS}
fi

PKG_DIR=install_packages
if [ "${1}" != "vsim" ]; then PKG_DIR=${PKG_DIR}_${1}; fi

SSHPARM="-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"

for PACKAGE in ${PACKAGES}; do
    case ${PACKAGE} in
    cpu_a)
        IP=192.168.2.10
        ;;
    cpu_b)
        IP=192.168.2.11
        ;;
    cpu_c)
        IP=192.168.2.12
        ;;
    *)
        IP=192.168.2.12
    esac

    PACKAGE_NAME=aos_bhp-${PACKAGE}.deb
    SCRIPT_NAME=install_aos_bhp-${PACKAGE}.sh

    echo "[+] clean /tmp on ${IP}"
    sshpass -p "admin" ssh ${SSHPARM} root@${IP} "rm -f /tmp/*aos_bhp*; rm -f /tmp/*.log; rm -f /tmp/*.tar.gz"

    echo "[+] clean /optdata/coredumps on ${IP}"
    sshpass -p "admin" ssh ${SSHPARM} root@${IP} "rm -f /optdata/coredumps/*.core"

    echo "[+] deploy ${PACKAGE_NAME} to ${IP}"
    sshpass -p "admin" scp -p ${SSHPARM} ${PKG_DIR}/${PACKAGE_NAME} root@${IP}:/tmp/

    echo "[+] deploy ${SCRIPT_NAME} to ${IP}"
    sshpass -p "admin" scp -p ${SSHPARM} ${PKG_DIR}/${SCRIPT_NAME} root@${IP}:/tmp/

    echo "[+] running ${SCRIPT_NAME} on ${IP}"
    sshpass -p "admin" ssh ${SSHPARM} root@${IP} "cd /tmp; chmod ugo+rx ${SCRIPT_NAME}; ./${SCRIPT_NAME}"
done
