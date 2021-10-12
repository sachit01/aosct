#!/bin/sh
#  Script %name:	startup_system_generic.sh %
#  %version:		1.0 %
#  %created_by:		ragquens %
#  %date_created:	2017-09-15 13:20 %
#
# Description: 
# Startup CPU-C in generic Lab Setup

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

# priorities. 
# Don't change unless you know what you are doing.
VFW_PRIO="56"
NVSH_PRIO="55"
APP_PRIO="3"


# Commands run from this scripts
SP_CMD="/opt/vfw/run/sp -e Connected"
RT_CMD="/opt/vfw/run/rt -sr -p"	
VFW_CMD="/opt/vfw/run/vfwChanneld -A 20 -O 5 -I 40 -C 35 -c 100 client@CoHP-B server@CoHP-A"
NVSH_CMD="/opt/vfw/run/vfw_nvshd -u AOS -d /optdata/data/aos -c /optdata/data/aos"
APPLICATION="/opt/bin/aos/dispatcher_$1 -c"

mkdir -p /tmp/fifos                            
rm /tmp/fifos/*        
cd /opt/vfw/run

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

logger "Starting NVSH Daemon"
sudo $VFW_ENV $RT_CMD$NVSH_PRIO -- sudo $VFW_ENV $GSP_USER $NVSH_CMD > /tmp/nvsh.log 2>&1 &
logger "NVSH Daemon started"

# VSIM is the only one using timesync to the OPC
if [ "$1" == "vsim" ]
then
  # Start the time sync mini-dispatcher-mode for CoHP-C
  logger "Starting TimeSyncServer"
  /opt/bin/aos/TimeSyncServer -c -n $UNIQUE_ID  > /tmp/timesync.log 2>&1 &
fi

logger "Starting Dispatcher"
sudo $VFW_ENV $RT_CMD$APP_PRIO -- sudo $VFW_ENV $GSP_USER /opt/vfw/run/sp -e vfwStarted -- $APPLICATION &


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
