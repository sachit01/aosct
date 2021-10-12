################################################################################
#
# (C) COPYRIGHT Bombardier, 2010
#
# We reserve all rights in this file and in the information
# contained therein. Reproduction, use or disclosure to third
# parties without express authority is strictly forbidden.
#
# %full_filespec: Makefile-3.2:makefile:arn_043#202 %
# %created_by:    ljoars %
# %date_created:  2015-04-07 15:28 %
#
# Description:
#
################################################################################
# Revision History
#
# Version  Date      Sign     Change description
# 13     20130618  wdetert   SPL library name for FreeBSD/ARM is now libsplB
# 12     20130610  wdetert   merging the version 11 (WP001) and 10.1
# 10.1    20130610  wdetert   merging the version 10 (WP008) and 8.3 (adaption because of VFW T10)
# 8.2     20130515  wdetert  add the library libc.so when linking for the ARM board (OS=FreeBSD)
# 1        20101202  signada  Created
################################################################################

.EXPORT_ALL_VARIABLES:

ifndef IMPL_DIR
IMPL_DIR := ..
endif

TARGET_TYPE := BINARY
TARGET_NAME := TestATP

include $(IMPL_DIR)/config.mk
include $(IMPL_DIR)/tools.mk

# includes
INCLUDE += -I. -I$(VFW_HEADER_DIR) -I$(VFW_HEADER_DIR)/$(OS_NAME)
INCLUDE += $(ALL_USED_MODULES:%=-I%)

EXTRA_LIBS := -start-group
# EXTRA_LIBS += \
-end-group

EXTRA_LIBS := $(addprefix -Xlinker ,$(EXTRA_LIBS))

EXTRA_LIBS += $(VFW_LIB_DIR)/libVfw.so
EXTRA_LIBS += $(IMPL_DIR)/../Distribution/lib/$(OS_NAME)/libvioclient.so

ifeq ("$(OS_NAME)","Linux_ppc")
EXTRA_LIBS += -lrt
endif
 
ifeq ("$(OS_NAME)","RHEL4_x86")
EXTRA_LIBS += -lrt
endif

ifeq ("$(OS_NAME)","FreeBSD_armv5te")
EXTRA_LIBS += -Xlinker $(OS)/distribution/sysroot/usr/lib/libc.so
endif


# ALL_USED_PREFIX	=	../



include localRules.mk

