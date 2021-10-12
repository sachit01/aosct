# Set Environment Variables for different standard path
VFW_PATH      := /localProducts/CoVP_CoHP-1.1/CoVP_CoHP
GSP2_PATH     := /localProducts/GSP_2.2/Software/Development
FLINT_PATH    := /localProducts/FLINT-9.00L/FLINT
VIOH_PATH     := $(GSP2_PATH)/VitalIO_Handler
BDS_PATH      := $(GSP2_PATH)/BDS
PROJS_ROOT    := $(HOME)/p_drive
ATC_ROOT      := $(PROJS_ROOT)/aos_bhp/atc
CORE_ROOT     := $(PROJS_ROOT)/aos_bhp/atp_core
DISP_ROOT     := $(PROJS_ROOT)/aos_bhp/dispatcher
LINT_ROOT     := $(ATC_ROOT)/lint
SPL_ROOT      := $(PROJS_ROOT)/SPL_Delivery

# Absolute path for Cross compiler for ARM and PowerPC respectively
CROSS_RHEL4_x86_32_FreeBSD_armv5te_arm := /localProducts/CROSS_RHEL4_x86_32_FreeBSD_armv5te_arm-1.1/CROSS_RHEL4_x86_32_FreeBSD_armv5te_arm
CROSS_RHEL4_x86_Linux_ppc      := /localProducts/CROSS_RHEL4_x86_Linux_ppc-1.0/CROSS_RHEL4_x86_Linux_ppc

CROSS_native      := $(NATIVECOMPILER)
CROSS_Linux_ppc   := $(CROSS_RHEL4_x86_Linux_ppc)
CROSS_FreeBSD_arm := $(CROSS_RHEL4_x86_32_FreeBSD_armv5te_arm)
