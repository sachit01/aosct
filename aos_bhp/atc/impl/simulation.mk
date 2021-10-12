ifeq ($(ENV),SIL)
    ATC_MODULES += $(ATC_ROOT)/impl/simulation/common/
    ATC_MODULES += $(ATC_ROOT)/impl/simulation/cod_sim/
    ATC_MODULES += $(ATC_ROOT)/impl/simulation/opc_sim/
    ATC_MODULES += $(ATC_ROOT)/impl/simulation/vioh_sim/
    ATC_MODULES += $(ATC_ROOT)/impl/vfw_sim/
    ATC_MODULES += $(ATC_ROOT)/impl/bds_sim/
    INCLUDE += -I$(ATC_ROOT)/impl/simulation/splapi/
else ifeq ($(ENV),HIL)
    ATC_MODULES += $(ATC_ROOT)/impl/simulation/common/
    ATC_MODULES += $(ATC_ROOT)/impl/simulation/cod_sim/
    ATC_MODULES += $(ATC_ROOT)/impl/simulation/opc_sim/
    ATC_MODULES += $(ATC_ROOT)/impl/simulation/vioh_sim/
    INCLUDE += -I$(ATC_ROOT)/impl/simulation/splapi/
else ifeq ($(ENV),EMD)
    ATC_MODULES += $(ATC_ROOT)/impl/simulation/common/
    ATC_MODULES += $(ATC_ROOT)/impl/simulation/opc_sim/
    INCLUDE += -I$(ATC_ROOT)/impl/simulation/splapi/
endif
