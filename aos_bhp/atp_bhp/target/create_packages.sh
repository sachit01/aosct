#!/bin/bash

set -e

CONFIGS="cfg_data dispatcher_data instance_data mnt_data type_data rt_data"

if [ -z "$1" ]; then echo "$0: missing target env [hil|vsim|emd]"; exit 1; fi

if [ -z "$2" ]; then echo "$0: missing version"; exit 1; else VERSION="$2"; fi

if [ -z "$3" ]; then PACKAGES="cpu_a cpu_b cpu_c ${CONFIGS}"; else PACKAGES="$3"; fi

if [ "${PACKAGES}" == "config" ]; then
    PACKAGES=${CONFIGS}
fi

VFW_HOME=/localProducts/VFW-1.3/VFW/distribution
CRC64SUM=${VFW_HOME}/bin/crc64sum

function doinstall() {
    local a=`readlink -f $1`;
    local b=`basename $a`;
    local d=$2/$3
    local f=$3/$b
    local c=$f.checksum

    mkdir -p $d;
    if [ -z "$a" ]; then echo "[-] Cannot find $1: '$a' to copy" ; exit 1; fi
    if [ ! -f "$a" ]; then echo "[-] Cannot find $1: '$a' to copy" ; exit 1; fi

    echo "[+] $a -> /$3"
    if [ ${a: -3} == ".sh" ] || [ ${a: -5} == ".conf" ] || [ ${a: -4} == ".txt" ]; then
        dos2unix < $a > $d/$b
    else
        cp -p $a $d/$b

        (cd $2; echo -n / > $c; ${CRC64SUM} $f >> $c)

        echo if [ ! -f /$f ]\; then echo\; echo Error: /$f does not exist!\; exit 10\; fi >> $2/check_files
        echo if [ ! -f /$c ]\; then echo\; echo Error: /$c does not exist!\; exit 11\; fi >> $2/check_files
        echo /opt/vfw/bin/crc64sum -v -c /$c >> $2/check_files
        echo if [ \"\$\?\" -ne \"0\" ]\; then echo\; echo Error: /$f does not match expected CRC!\; exit 12\; fi >> $2/check_files
    fi
}

echo "[+] Generating packages for ENV $1"

TMPROOT=_tmp_${1}
PKG_DIR=install_packages
if [ "${1}" != "vsim" ]; then PKG_DIR=${PKG_DIR}_${1}; fi

rm -rf ${TMPROOT}
mkdir -p ${TMPROOT}
mkdir -p ${PKG_DIR}

echo "[+] Copying files:"

for PACKAGE in ${PACKAGES}; do
    if [ "${PACKAGE}" == "cpu_a" ]; then
        doinstall ../../atp_bhp/target/linux_ppc/sysctl.conf                    ${TMPROOT}/${PACKAGE} etc
        doinstall ../../atp_bhp/target/cpu_a/$1/atp_bhp_$1                      ${TMPROOT}/${PACKAGE} opt/bin/aos
        doinstall ../../atc/platform_halt/release/Linux_ppc/platform_halt       ${TMPROOT}/${PACKAGE} opt/bin/aos
        doinstall ../../../SetTimeOfDay/bin/Linux_ppc/settimeofday              ${TMPROOT}/${PACKAGE} opt/bin/aos
        doinstall ../../../TimeSyncServer/bin/Linux_ppc/TimeSyncServer          ${TMPROOT}/${PACKAGE} opt/bin/aos
        doinstall ../../atp_bhp/target/cpu_a/startup_system_generic.sh          ${TMPROOT}/${PACKAGE} opt/bin
    elif [ "${PACKAGE}" == "cpu_b" ]; then
        doinstall ../../atp_bhp/target/freebsd_arm/sysctl.conf                  ${TMPROOT}/${PACKAGE} etc
        doinstall ../../atp_bhp/target/cpu_b/$1/atp_bhp_$1                      ${TMPROOT}/${PACKAGE} opt/bin/aos
        doinstall ../../atc/platform_halt/release/FreeBSD_armv5te/platform_halt ${TMPROOT}/${PACKAGE} opt/bin/aos
        doinstall ../../../SetTimeOfDay/bin/FreeBSD_arm/settimeofday            ${TMPROOT}/${PACKAGE} opt/bin/aos
        doinstall ../../../TimeSyncServer/bin/FreeBSD_arm/TimeSyncServer        ${TMPROOT}/${PACKAGE} opt/bin/aos
        doinstall ../../atp_bhp/target/cpu_b/startup_system_generic.sh          ${TMPROOT}/${PACKAGE} opt/bin
    elif [ "${PACKAGE}" == "cpu_c" ]; then
        doinstall ../../atp_bhp/target/linux_ppc/sysctl.conf                    ${TMPROOT}/${PACKAGE} etc
        doinstall ../../atc/nvshft/nvshft                                       ${TMPROOT}/${PACKAGE} opt/bin/aos
        doinstall ../../atc/nvshft/release/Linux_ppc/nvshft_generic             ${TMPROOT}/${PACKAGE} opt/bin/aos
        doinstall ../../atc/nvshfr/release/Linux_ppc/nvshfr                     ${TMPROOT}/${PACKAGE} opt/bin/aos
        doinstall ../../atc/platform_halt/release/Linux_ppc/platform_halt       ${TMPROOT}/${PACKAGE} opt/bin/aos
        doinstall ../../dispatcher/target/$1/dispatcher_$1                      ${TMPROOT}/${PACKAGE} opt/bin/aos
        doinstall ../../../TimeSyncServer/bin/Linux_ppc/TimeSyncServer          ${TMPROOT}/${PACKAGE} opt/bin/aos
        doinstall ../../dispatcher/target/startup_system_generic.sh             ${TMPROOT}/${PACKAGE} opt/bin
        for CONFIG in ${CONFIGS}; do
            doinstall ../nvshft/data_files/${CONFIG}_def.txt                    ${TMPROOT}/${PACKAGE} optdata/data/aos
        done
    else
        doinstall ../nvshft/data_files/${PACKAGE}.bin                           ${TMPROOT}/${PACKAGE} optdata/data/aos
    fi
done

echo "[+] Prepare scripts:"

for PACKAGE in ${PACKAGES}; do
    if [ "${PACKAGE}" == "cpu_a" ] || [ "${PACKAGE}" == "cpu_b" ] || [ "${PACKAGE}" == "cpu_c" ]; then

        start=${TMPROOT}/${PACKAGE}/tmp/startup_system.sh
        valid=${TMPROOT}/${PACKAGE}/tmp/validate_files.sh
        log=/tmp/console.log

        mkdir -p `dirname $start`

        echo export LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:/opt/vfw/lib                        > $start
        echo rm -f $log \> /dev/null 2\>\&1                                              >> $start
        echo echo Checking CRC: \> $log 2\>\&1                                           >> $start
        echo /opt/bin/aos/validate_files.sh \>\> $log 2\>\&1                             >> $start
        echo if [ \"\$\?\" -eq \"0\" ]\; then                                            >> $start
        echo     /opt/bin/startup_system_generic.sh $1 \>\> $log 2\>\&1                  >> $start
        echo else                                                                        >> $start
        echo     /opt/bin/aos/platform_halt \"CRC validation failed!\" \>\> $log 2\>\&1  >> $start
        echo fi                                                                          >> $start

        echo export LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:/opt/vfw/lib                        > $valid
        cat ${TMPROOT}/${PACKAGE}/check_files                                            >> $valid

        if [ "${PACKAGE}" == "cpu_c" ]; then
            for CONFIG in ${CONFIGS}; do
                cat ${TMPROOT}/${CONFIG}/check_files                                     >> $valid
            done
        fi

        doinstall $start ${TMPROOT}/${PACKAGE} opt/bin
        doinstall $valid ${TMPROOT}/${PACKAGE} opt/bin/aos

        f=${TMPROOT}/${PACKAGE}/tmp/preinstall.sh
        mkdir -p `dirname $f`

        cat <<EOF > ${f}
mount_rw

mkdir -p /opt/bin/aos

chmod ug+rx /opt/bin
chmod ug+rx /opt/bin/aos
chmod ug+rx /opt/vfw/bin/crc64sum
EOF

        if [ "${PACKAGE}" != "cpu_b" ]; then
            cat <<EOF >> ${f}
chown root:root /usr/bin/sudo
chmod 4755 /usr/bin/sudo
EOF
        fi

        cat <<EOF >> ${f}
if [ -f /opt/startup/S99setup_arp.sh ]
then
    mv /opt/startup/S99setup_arp.sh /opt/startup/S05setup_arp.sh
fi

EOF

        f=${TMPROOT}/${PACKAGE}/tmp/postinstall.sh
        mkdir -p `dirname $f`

        cat <<EOF > ${f}
chmod ug+rwx /opt/bin/startup_system.sh /opt/bin/startup_system_generic.sh /opt/bin/aos/validate_files.sh /opt/bin/aos/TimeSyncServer
chmod o-rwx /opt/bin/startup_system.sh /opt/bin/startup_system_generic.sh /opt/bin/aos/validate_files.sh /opt/bin/aos/TimeSyncServer
chown -R atpcu:wheel /opt/bin

EOF
    fi

    if [ "${PACKAGE}" == "cpu_a" ]; then
        cat <<EOF >> ${TMPROOT}/cpu_a/tmp/preinstall.sh
killall TimeSyncServer 2> /dev/null
killall atp_bhp_$1 2> /dev/null

sleep 1

rm -f /opt/bin/aos/TimeSyncServer /opt/bin/aos/settimeofday  /opt/bin/aos/atp_bhp_$1 /opt/bin/startup_system.sh /opt/bin/startup_system_generic.sh /opt/bin/aos/validate_files.sh /etc/sysctl.conf
EOF

        cat <<EOF >> ${TMPROOT}/cpu_a/tmp/postinstall.sh
chmod ug+rwx /opt/bin/aos/atp_bhp_$1 /opt/bin/aos/platform_halt
chmod o-rwx /opt/bin/aos/atp_bhp_$1 /opt/bin/aos/platform_halt
chown root:root /opt/bin/aos/settimeofday
chmod 5555  /opt/bin/aos/settimeofday

sync

export LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:/opt/vfw/lib
EOF

    elif [ "${PACKAGE}" == "cpu_b" ]; then
        cat <<EOF >> ${TMPROOT}/cpu_b/tmp/preinstall.sh
killall TimeSyncServer 2> /dev/null
killall atp_bhp_$1 2> /dev/null

sleep 1

if grep "defaultrouter" /etc/rc.conf -q
then
    echo "defaultrouter already added to rc.conf"
else
    echo -e 'defaultrouter="192.168.2.254"' >> /etc/rc.conf 
fi

rm -f /opt/bin/aos/TimeSyncServer /opt/bin/aos/settimeofday /opt/bin/aos/atp_bhp_$1 /opt/bin/startup_system.sh /opt/bin/startup_system_generic.sh /opt/bin/aos/validate_files.sh /etc/sysctl.conf
EOF

        cat <<EOF >> ${TMPROOT}/cpu_b/tmp/postinstall.sh
chmod ug+rwx /opt/bin/aos/atp_bhp_$1 /opt/bin/aos/platform_halt
chmod o-rwx /opt/bin/aos/atp_bhp_$1 /opt/bin/aos/platform_halt
chown root /opt/bin/aos/settimeofday
chmod 5555  /opt/bin/aos/settimeofday

sync

export LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:/opt/vfw/lib
EOF

    elif [ "${PACKAGE}" == "cpu_c" ]; then
        cat <<EOF >> ${TMPROOT}/cpu_c/tmp/preinstall.sh
mkdir -p /optdata/data/aos/cfg
mkdir -p /optdata/data/aos/def

chmod ug+rwx /optdata
chmod ug+rwx /optdata/data
chmod ug+rwx /optdata/data/aos
chmod ug+rwx /optdata/data/aos/cfg
chmod ug+rwx /optdata/data/aos/def

killall TimeSyncServer 2> /dev/null
killall dispatcher_$1 2> /dev/null

sleep 1

rm -f /opt/bin/aos/TimeSyncServer /opt/bin/aos/dispatcher_$1 /opt/bin/startup_system.sh /opt/bin/startup_system_generic.sh /opt/bin/aos/validate_files.sh /etc/sysctl.conf
EOF

        cat <<EOF >> ${TMPROOT}/cpu_c/tmp/postinstall.sh
chmod ug+rwx /opt/bin/aos/dispatcher_$1 /opt/bin/aos/platform_halt /opt/bin/aos/nvshfr /opt/bin/aos/nvshft /opt/bin/aos/nvshft_generic
chmod o-rwx /opt/bin/aos/dispatcher_$1 /opt/bin/aos/platform_halt /opt/bin/aos/nvshfr /opt/bin/aos/nvshft /opt/bin/aos/nvshft_generic
chown -R atpcu:wheel /optdata/data

sync

export LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:/opt/vfw/lib
EOF

    else # a config package
        mkdir -p ${TMPROOT}/${PACKAGE}/tmp

        cat <<EOF > ${TMPROOT}/${PACKAGE}/tmp/preinstall.sh
mkdir -p /optdata/data/aos

chmod ug+rwx /optdata
chmod ug+rwx /optdata/data
chmod ug+rwx /optdata/data/aos
chmod ug+rx /opt/vfw/bin/crc64sum
EOF

        cat <<EOF > ${TMPROOT}/${PACKAGE}/tmp/postinstall.sh
chmod -R ug+rwx /optdata/data
chmod -R o-rwx /optdata/data
chown -R atpcu:wheel /optdata/data

sync

export LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:/opt/vfw/lib
EOF

    fi

    cat ${TMPROOT}/${PACKAGE}/check_files >> ${TMPROOT}/${PACKAGE}/tmp/postinstall.sh
done

# create debian archives
for PACKAGE in ${PACKAGES}; do
    PACKAGE_NAME=aos_bhp-${PACKAGE}.deb
    echo "[+] Create deb package ${PACKAGE_NAME}"

    # create 'data' archive
    if [ "${PACKAGE}" == "cpu_a" ] || [ "${PACKAGE}" == "cpu_b" ]; then
        INPUTS="opt etc"
    elif [[ "${PACKAGE}" == "cpu_c" ]]; then
        INPUTS="opt etc optdata"
    else
        INPUTS="optdata"
    fi
    mkdir -p ${TMPROOT}/${PACKAGE}_deb_control ${TMPROOT}/${PACKAGE}_deb_create
    ( cd ${TMPROOT}/${PACKAGE}; tar czf ../${PACKAGE}_deb_create/data.tar.gz ${INPUTS} )

    # generate 'control' (description) file
    CONTROL_FILE=debian_control_file.txt
    if [ "${PACKAGE}" == "cpu_a" ] || [ "${PACKAGE}" == "cpu_b" ]; then
        CONTROL_FILE=${PACKAGE}/${CONTROL_FILE}
    elif [ "${PACKAGE}" == "cpu_c" ]; then
        CONTROL_FILE=../../dispatcher/target/${PACKAGE}/${CONTROL_FILE}
    else
        CONTROL_FILE=../../dispatcher/target/config/${CONTROL_FILE}
    fi
    dos2unix < ${CONTROL_FILE} | sed "s/[$][{]VERSION[}]/${VERSION}/g" > ${TMPROOT}/${PACKAGE}_deb_control/control

    if [[ "${PACKAGE}" == *_data ]]; then
        sed "s/aosconfig/aos_${PACKAGE}/g" ${TMPROOT}/${PACKAGE}_deb_control/control > ${TMPROOT}/${PACKAGE}_deb_control/control_
        mv ${TMPROOT}/${PACKAGE}_deb_control/control_ ${TMPROOT}/${PACKAGE}_deb_control/control
    fi

    # create 'control' archive
    cp ${TMPROOT}/${PACKAGE}/tmp/postinstall.sh ${TMPROOT}/${PACKAGE}_deb_control/postinst
    cp ${TMPROOT}/${PACKAGE}/tmp/preinstall.sh  ${TMPROOT}/${PACKAGE}_deb_control/preinst
    touch                                       ${TMPROOT}/${PACKAGE}_deb_control/conffiles
    chmod ug+rwx                                ${TMPROOT}/${PACKAGE}_deb_control/postinst
    chmod ug+rwx                                ${TMPROOT}/${PACKAGE}_deb_control/preinst
    ( cd ${TMPROOT}/${PACKAGE}_deb_control/; tar czf ../${PACKAGE}_deb_create/control.tar.gz ./control ./preinst ./postinst ./conffiles )

    # create install package
    echo "2.0" > ${TMPROOT}/${PACKAGE}_deb_create/debian-binary
    ( cd ${TMPROOT}/${PACKAGE}_deb_create; ar -r ../../${PKG_DIR}/${PACKAGE_NAME} debian-binary control.tar.gz data.tar.gz )
done

# create install scripts
for PACKAGE in ${PACKAGES}; do
    PACKAGE_NAME=aos_bhp-${PACKAGE}.deb
    SCRIPT_NAME=install_aos_bhp-${PACKAGE}.sh
    echo "[+] Create install script ${SCRIPT_NAME}"

    cat <<EOF > ${PKG_DIR}/${SCRIPT_NAME}
#!/bin/sh
mount_rw
date
opkg-cl remove atp > /dev/null
opkg-cl remove aosconfig > /dev/null
opkg-cl --force-downgrade install ${PACKAGE_NAME}
sync
EOF
    chmod ug+rx ${PKG_DIR}/${SCRIPT_NAME}
done

# cleanup
echo "[+] Cleanup"
rm -rf ${TMPROOT}
