#!/bin/bash

##################################################################
#
# (C) COPYRIGHT Bombardier Transportation Signal Sweden AB, 2020
#
# We reserve all rights in this file and in the information
# contained therein. Reproduction, use or disclosure to third
# parties without express authority is strictly forbidden.
#
# Description: AOS front end script for NVSHFT with CRC calculation
#
##################################################################

set -e

NVSHFT_DIR=data_files
NVSHFT_ARG="-n"

if [ $# -eq 0 ] || [ "$1" == "-h" ]; then
    echo "Usage: $0 PATH_TO_CONFIG_FILES"
    echo "Options:"
    echo "-n       Write parameter file(s)"
    echo
    echo "-c       Write common parameter file only"
    echo "-d       Write dispatcher parameter file only"
    echo "-i       Write instance parameter file only"
    echo "-m       Write maintenance parameter file only"
    echo "-r       Write runtime parameter file only"
    echo "-t | -ts Write type parameter file only"
    echo
    echo "-p path  Path to data files (default is '${NVSHFT_DIR}')"
    echo "-h       Print this help"
    exit 1
fi

while [[ $# -gt 0 ]]
do
    case "$1" in
    -n)
        # Do nothing
        ;;
    -c)
        NVSHFT_ARG=$1
        ;;
    -d)
        NVSHFT_ARG=$1
        ;;
    -i)
        NVSHFT_ARG=$1
        ;;
    -m)
        NVSHFT_ARG=$1
        ;;
    -r)
        NVSHFT_ARG=$1
        ;;
    -t)
        NVSHFT_ARG=-ts
        ;;
    -ts)
        NVSHFT_ARG=-ts
        ;;
    -p)
        if [[ $# -gt 1 ]]; then
            NVSHFT_DIR=$2
            shift
        else
            echo "$0: Missing value for $1 option"
            exit 1
        fi
        ;;
    *)
        echo "$0: Illegal option '$1'"
        exit 1
        ;;
    esac

    shift
done

if [ ! -d "${NVSHFT_DIR}" ]; then
    echo "$0: Can't find directory '${NVSHFT_DIR}', see option -p"
    exit 1
fi

EXE_DIR=`dirname $0`
TEMP_DIR=`mktemp -d --tmpdir tmp.XXXXXXXXXX`
NVSHFT_OPT="-g ${NVSHFT_DIR} -p ${TEMP_DIR} -b ${NVSHFT_DIR}"

if [ ${NVSHFT_ARG} == "-n" ] || [ ${NVSHFT_ARG} == "-c" ]; then
    ${EXE_DIR}/calc_crc ${NVSHFT_DIR}/cfg_data.txt ${TEMP_DIR}/CFG_DATA.txt
fi
if [ ${NVSHFT_ARG} == "-n" ] || [ ${NVSHFT_ARG} == "-d" ]; then
    ${EXE_DIR}/calc_crc ${NVSHFT_DIR}/dispatcher_data.txt ${TEMP_DIR}/DISPATCHER_DATA.txt
fi
if [ ${NVSHFT_ARG} == "-n" ] || [ ${NVSHFT_ARG} == "-i" ]; then
    ${EXE_DIR}/calc_crc ${NVSHFT_DIR}/instance_data.txt ${TEMP_DIR}/INSTANCE_DATA.txt
fi
if [ ${NVSHFT_ARG} == "-n" ] || [ ${NVSHFT_ARG} == "-m" ]; then
    ${EXE_DIR}/calc_crc ${NVSHFT_DIR}/mnt_data.txt ${TEMP_DIR}/MNT_DATA.txt
fi
if [ ${NVSHFT_ARG} == "-n" ] || [ ${NVSHFT_ARG} == "-r" ]; then
    ${EXE_DIR}/calc_crc ${NVSHFT_DIR}/rt_data.txt ${TEMP_DIR}/RT_DATA.txt
fi
if [ ${NVSHFT_ARG} == "-n" ] || [ ${NVSHFT_ARG} == "-ts" ]; then
    ${EXE_DIR}/calc_crc ${NVSHFT_DIR}/type_data.txt ${TEMP_DIR}/TYPE_DATA.txt
fi

${EXE_DIR}/nvshft ${NVSHFT_ARG} ${NVSHFT_OPT}

rm -r ${TEMP_DIR}
