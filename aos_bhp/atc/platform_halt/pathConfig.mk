VFW_PATH      := /localProducts/CoVP_CoHP-1.1/CoVP_CoHP
PROJS_ROOT    := $(HOME)/p_drive
ATC_ROOT      := $(PROJS_ROOT)/aos_bhp/atc
BHP_ROOT      := $(PROJS_ROOT)/aos_bhp/atp_bhp

# Absolute path for Cross compiler for ARM and PowerPC respectively
CROSS_RHEL4_x86_32_FreeBSD_armv5te_arm := /localProducts/CROSS_RHEL4_x86_32_FreeBSD_armv5te_arm-1.1/CROSS_RHEL4_x86_32_FreeBSD_armv5te_arm
CROSS_RHEL4_x86_Linux_ppc      := /localProducts/CROSS_RHEL4_x86_Linux_ppc-1.0/CROSS_RHEL4_x86_Linux_ppc

CROSS_native      := $(NATIVECOMPILER)
CROSS_Linux_ppc   := $(CROSS_RHEL4_x86_Linux_ppc)
CROSS_FreeBSD_arm := $(CROSS_RHEL4_x86_32_FreeBSD_armv5te_arm)
