#!/bin/sh
#  Script %name:	startup_system_generic.sh %
#  %version:		1.0 %
#  %created_by:		ragquens %
#  %date_created:	2017-09-15 13:20 %
#
# Description: 
# Startup CPU-A in generic Lab Setup

# These env variables must be propagated to all commands called via sudo
VFW_ENV="LD_LIBRARY_PATH=/opt/vfw/run \
         VFW_FIFO_DIR=/tmp/fifos \
         VFW_REDUNDANT_SIDE=LEFT"

# The VFW channel. This is used by the
# vioserver, SDP and user application.
# It also have to be the same on both A and B
UNIQUE_ID="AOS"

# options to sudo when running as a user.
GSP_USER="-u atpcu"

# Cycle time, have to be between 25-32 ms.
# Have to be the same on both A and B.
CYCLE="32"

# priorities. 
# Don't change unless you know what you are doing.
VFW_PRIO="56"
VIOH_PRIO="55"
SDP_PRIO="52"
APP_PRIO="3"

# Commands run from this scripts
SP_CMD="/opt/vfw/run/sp -e Connected"
RT_CMD="/opt/vfw/run/rt -sr -p"
VFW_CMD="/opt/vfw/run/vfwChanneld -A 20 -O 5 -I 40 -C 35 -c 100 -T client@CoHP-B client@CoHP-C"
VIOH_CMD="/opt/run/vioserver $CYCLE 1 1 $UNIQUE_ID a"
SDP_CMD="/opt/run/SDP_A $UNIQUE_ID"
APPLICATION="/opt/bin/aos/atp_bhp_$1 -a"

# vioh log. Must be in ramdisk (/tmp)
VIOH_LOG="/tmp/vioh_a.log"

mkdir -p /tmp/fifos
cd /opt/vfw/run                  
rm /tmp/fifos/*


export LD_LIBRARY_PATH=/opt/vfw/run
export VFW_FIFO_DIR=/tmp/fifos
export VFW_REDUNDANT_SIDE=LEFT

# Kill any old vfwProcess
/opt/vfw/run/vfwChanneld -x

ping -W 5 -c 10 CoHP-B > /dev/null
if [ $? != 0 ]
then
    logger "Problems with the network, CoHP-B not responding!"
else
    logger "Ping to CoHP-B OK!"
fi


logger "Starting VFW"
sudo $VFW_ENV $RT_CMD$VFW_PRIO -- sudo $VFW_ENV $GSP_USER $SP_CMD -- $VFW_CMD > /tmp/vfwChanneld.log 2>&1
logger "VFW started"

# HIL is simulating the SDP, it should not start it
if [ "$1" != "hil" ]
then
  logger "Starting VIOH"
  sudo $VFW_ENV $RT_CMD$VIOH_PRIO -- sudo $VFW_ENV $GSP_USER -- $VIOH_CMD >$VIOH_LOG 2>&1 &

  sleep 2

  logger "Starting SDP"
  cd /opt/bin
  sudo $VFW_ENV $RT_CMD$SDP_PRIO -- sudo $VFW_ENV $GSP_USER -- $SDP_CMD > /tmp/sdp.log 2>&1 &
fi

sleep 5

# VSIM is the only one using timesync to the OPC
if [ "$1" == "vsim" ]
then
  # Start the time sync client for CoHP-A
  logger "Starting TimeSyncServer"
  /opt/bin/aos/TimeSyncServer -a -n $UNIQUE_ID  > /tmp/timesync.log 2>&1 &
fi

sleep 3

logger "Starting ATP"
sudo $VFW_ENV $RT_CMD$APP_PRIO -- sudo $VFW_ENV $GSP_USER $APPLICATION &


########################################
#
# All components starts in the background
# and this never-ending loop is needed to 
# make this script stay loaded. This is to
# ensure that all processes stay in the same
# process group.
#
########################################
while true
do
    sleep 10000
done

exit
