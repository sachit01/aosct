#!/bin/bash

CONFIGS="cfg_data dispatcher_data instance_data mnt_data type_data rt_data"

PACKAGES="cpu_a cpu_b cpu_c ${CONFIGS}"

SSHPARM="-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"

for PACKAGE in ${PACKAGES}; do
    case ${PACKAGE} in
    cpu_a)
        IP=192.168.2.10
        PKGNAME=aos
        ;;
    cpu_b)
        IP=192.168.2.11
        PKGNAME=aos
        ;;
    cpu_c)
        IP=192.168.2.12
        PKGNAME=aos
        ;;
    *)
        PKGNAME=aos_${PACKAGE}
        IP=192.168.2.12
    esac

    echo "[+] uninstalling ${PKGNAME} on ${IP}"
    sshpass -p "admin" ssh ${SSHPARM} root@${IP} "mount_rw; opkg-cl remove atp > /dev/null; opkg-cl remove aosconfig > /dev/null; opkg-cl remove ${PKGNAME}; sync"
done

echo "[+] finish"
