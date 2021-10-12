##################################################################
#
#       Makefile for AOS Dispatcher Code
#
##################################################################
######################### Revision History #######################
#
# Version       Date    Sign      Change Description
#
# 1.0          161226  saprasad   Created Makefile for Dispatcher
# 1.1          20170103 spandita  Removed the path for test_disp.cpp
##################################################################

include buildkit.target.mk

DISP_ROOT = $(PROJS_ROOT)/aos_bhp/dispatcher

include $(DISP_ROOT)/impl/component.mk
include $(ATC_ROOT)/impl/component.mk
include $(ATC_ROOT)/impl/compiler_flags.mk
include $(ATC_ROOT)/impl/rules.mk

# These modules are not needed in Dispatcher:
ATC_MODULES := $(filter-out $(ATC_ROOT)/impl/analyzer_if/,$(ATC_MODULES))
ATC_MODULES := $(filter-out $(ATC_ROOT)/impl/crc32/,$(ATC_MODULES))
ATC_MODULES := $(filter-out $(ATC_ROOT)/impl/fixed_size_mempool/,$(ATC_MODULES))

MKDIR:=mkdir -p

ifeq ($(ENV), HIL)
    ENV_SUFFIX = hil
    MY_ADDED_DEFINES += -D__HIL
else ifeq ($(ENV), EMD)
    ENV_SUFFIX = emd
    MY_ADDED_DEFINES += -D__EMD
else ifeq ($(ENV), VSIM)
    ENV_SUFFIX = vsim
    MY_ADDED_DEFINES += -D__VSIM
else
    $(error When compiling for ENV=[HIL|VSIM|REL])
endif

MY_ADDED_DEFINES += -D_DISPATCHER

ifeq ($(TARGET), ARM)
    BOARD = cpu_b
endif
ifeq ($(TARGET), PPC)
    BOARD = cpu_a
endif

## Directory for object and binary files
OBJDIR := ./obj/$(OS_NAME)/$(ENV_SUFFIX)
DISTDIR := $(DISP_ROOT)/target/$(ENV_SUFFIX)

# The file we want (and where we want it)
BIN_DIR = $(PROJS_ROOT)/aos_bhp/dispatcher/target/$(ENV_SUFFIX)
PROJ_NAME = $(BIN_DIR)/dispatcher_$(ENV_SUFFIX)

# ----------------------- include module dirs---------------------------------
# List here all the module folders with source files.
MODULES += $(ATC_MODULES)
MODULES += $(DISP_MODULES)

#-------------------------end of module dirs---------------------------

# ---------------------------source files-------------------------------------
# List here all folders with source files.
CXX_FILES := $(foreach moduleDir,$(MODULES),$(wildcard $(moduleDir)*.cpp))
CXX_FILES := $(filter-out %/atc_bit_unpacker.cpp,$(CXX_FILES))

# -------------------------end of source files----------------------------

# Files needed to make the executable file, this will automatically add one .o file for every .cpp file.
# NOTE! If there are source files with same name but in different folders then this makefile will fail, since it will put 
#all obj files in one and same folder.
OBJ_FILES    := $(patsubst $(PROJS_ROOT)/%.cpp,$(OBJDIR)/%.o,$(CXX_FILES))

# ----------------------------include paths-------------------------------
# Include paths where to look for header files, all folder with header files need to be listed here or else there will be
# a "No such file or directory" error when building.
INCLUDE += $(foreach moduleDir,$(MODULES),$(addprefix -I,$(moduleDir)))
INCLUDE += -I$(VFW_PATH)/distribution/$(OS_NAME)/include
INCLUDE += -I$(BDS_PATH)/include
# ------------------------end of include paths----------------------------

# If user type just "make" on command line this is what is built
.DEFAULT_GOAL := all
.PHONY: all
all:$(PROJ_NAME)

CC_FLAGS += $(MY_ADDED_DEFINES)


# -----------------------------build recipes------------------------------

$(OBJDIR)/%.o: $(PROJS_ROOT)/%.cpp
	$(cxx-command)

# This is how to link the objects files into the executable file.
$(PROJ_NAME): $(OBJ_FILES)
	$(MY_MKDIR) $(BIN_DIR)
	@echo ""
	$(MY_CXX) $(OBJ_FILES) $(LD_FLAGS) $(EXTRA_LIBS) -o $(PROJ_NAME)
	@echo ""
	@echo -e " Successfully built $(PROJ_NAME)"

docs: $(DISP_HTML_FILES)

# --------------------------------clean------------------------------------
.PHONY: clean
clean:
	$(MY_RM) -rf $(OBJDIR)/*
	$(MY_RM) -rf $(BIN_DIR)/dispatcher_emd
	$(MY_RM) -rf $(BIN_DIR)/dispatcher_hil
	$(MY_RM) -rf $(BIN_DIR)/dispatcher_vsim

# --------------------------------end of clean-----------------------------

#-------------------------------- Help ------------------------------------
.SILENT:
.PHONY: help
help:
	echo -e "\t Usage\t\t\t:make [all|clean|lint] -j2 TARGET=[PPC|ARM] ENV=[HIL|VSIM] {DEBUG=[0|1]} \n"
	echo -e "\t PPC\t\t\t:To compile code for PPC "
	echo -e "\t ARM\t\t\t:To compile code for ARM "
	echo -e "\t clean\t\t\t:Clean Object and Bin file "
	echo -e "\t DEBUG\t\t\t:If set debug info will get added"

#------------------------------- End of Help ------------------------------		

# ------------------------------dependencies-------------------------------
# Automatic dependency graph generation
# -MMD means that for each .o file a .d file shall also be generated.
# -include means we include the dependency file for each object file to make into this Makefile.
CC_FLAGS += -MMD
-include $(OBJ_FILES:.o=.d)

# -------------------------------end of dependencies------------------------------
