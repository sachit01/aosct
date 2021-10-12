##################################################################
#
#                   Makefile for AOS ATP Code
#
##################################################################
######################### Revision History #######################
#
# Version       Date    Sign      Change Description
#
# 1.0          161201  arastogi   Created
# 1.1          161220  saprasad   Added Analyzer IF support in makefile
#
##################################################################

include buildkit.target.mk

BHP_ROOT = $(PROJS_ROOT)/aos_bhp/atp_bhp

include $(BHP_ROOT)/impl/component.mk
include $(CORE_ROOT)/impl/component.mk
include $(ATC_ROOT)/impl/component.mk
include $(ATC_ROOT)/impl/simulation.mk
include $(ATC_ROOT)/impl/compiler_flags.mk
include $(ATC_ROOT)/impl/rules.mk

INSTALL:=install
MKDIR:=mkdir -p
BUILD_CRC = crc

ifeq ($(ENV), HIL)
    ENV_SUFFIX = hil
    MY_ADDED_DEFINES += -D__HIL
else ifeq ($(ENV), EMD)
    ENV_SUFFIX = emd
    MY_ADDED_DEFINES += -D__EMD
else ifeq ($(ENV), SIL)
    ENV_SUFFIX = sil
    MY_ADDED_DEFINES += -D_SIL
    INCLUDE += -I$(DISP_ROOT)/impl/class_d
else ifeq ($(ENV), VSIM)
    ENV_SUFFIX = vsim
    MY_ADDED_DEFINES += -D__VSIM
    INCLUDE += -I$(SPL_ROOT)/inc
else
    $(error ENV=[HIL|EMD|VSIM|SIL])
endif

ifeq ($(TARGET), ARM)
    BOARD = cpu_b
else ifeq ($(TARGET), PPC)
    BOARD = cpu_a
else ifeq ($(TARGET), WIN32)
    BOARD = cpu_a
    EXTRA_LIBS =  -lws2_32
endif

ifeq ($(TARGET), HOST)
	MY_ADDED_DEFINES += -DGENERATE_TCC_EVENTLIST_XML_PATH=\"$(PROJS_ROOT)/aos_bhp/tools_bhp/GenerateEventXML/\"
endif

## Directory for object files
OBJDIR := ./obj/$(OS_NAME)/$(ENV_SUFFIX)
DISTDIR := $(BHP_ROOT)/target/$(BOARD)/$(ENV_SUFFIX)
ExportDISTDIR := ./Distribution

# The file we want (and where we want it)
BIN_DIR = $(PROJS_ROOT)/aos_bhp/atp_bhp/target/$(BOARD)/$(ENV_SUFFIX)
ifeq ($(TARGET), WIN32)
PROJ_NAME = $(BIN_DIR)/test_atp.exe
else
PROJ_NAME = $(BIN_DIR)/atp_bhp_$(ENV_SUFFIX)
endif

# ---------------------------module dirs---------------------------------
# List here all the module folders with source files.
MODULES += $(ATC_MODULES)
MODULES += $(CORE_MODULES)
MODULES += $(BHP_MODULES)

ifeq ($(ENV), SIL)
MODULES += $(BHP_ROOT)/impl/test_atp_vs15/
MODULES += $(DISP_ROOT)/impl/class_d/
endif

# ---------------------------source files---------------------------------
# List here all folders with source files.
# If there are both .c and .cpp files then those are listed separately here.
# For each folder here a rule (aka recepie) is needed also, see: compilation recepies
# If not there will be a "*** No rule to make target" error.

CXX_FILES := $(foreach moduleDir,$(MODULES),$(wildcard $(moduleDir)*.cpp))
CC_FILES := $(foreach moduleDir,$(MODULES),$(wildcard $(moduleDir)*.c))

ifeq ($(ENV), SIL)
	CXX_FILES := $(filter-out $(BHP_ROOT)/impl/test_atp_vs15/stdafx.cpp,$(CXX_FILES))
endif
  
# -------------------------end of source files----------------------------

# Files needed to make the executable file, this will automatically add one .o file for every .c or .cpp file.
# NOTE! If there are source files with same name but in different folders then this makefile will fail, since it will put all obj files in one and same folder.
OBJ_FILES    := $(patsubst $(PROJS_ROOT)/%.cpp,$(OBJDIR)/%.o,$(CXX_FILES))
OBJ_FILES    += $(patsubst $(PROJS_ROOT)/%.c,$(OBJDIR)/%.o, $(CC_FILES))

# ----------------------------include paths-------------------------------
# Include paths where to look for header files, all folder with header files need to be listed here or else there will be a "No such file or directory" error when building.
INCLUDE += $(foreach moduleDir,$(MODULES),$(addprefix -I,$(moduleDir)))
ifeq ($(ENV), SIL)
   INCLUDE += -I$(VFW_PATH)/distribution/Linux_ppc/include
else
   INCLUDE += -I$(VFW_PATH)/distribution/$(OS_NAME)/include
endif
INCLUDE += -I$(VIOH_PATH)/include
INCLUDE += -I$(BDS_PATH)/include


# ------------------------end of include paths----------------------------

# If user type just "make" on command line this is what is built
.DEFAULT_GOAL := all
.PHONY: all
all:$(PROJ_NAME)
#	@echo $(CXX_FILES)
#$(BUILD_CRC)

CC_FLAGS += $(MY_ADDED_DEFINES) $(EXTERNAL_DEFINES)

# -----------------------------build recipes------------------------------

$(OBJDIR)/%.o: $(PROJS_ROOT)/%.cpp
	$(cxx-command)

$(OBJDIR)/%.o: $(PROJS_ROOT)/%.c
	$(cc-command)

# This is how to link the objects files into the executable file.
$(PROJ_NAME): $(OBJ_FILES)
	$(MY_MKDIR) $(BIN_DIR)
	@echo ""
	$(MY_CXX) $(OBJ_FILES) $(LD_FLAGS) $(EXTRA_LIBS) -o $(PROJ_NAME)
	@echo ""
	@echo -e " Successfully built $(PROJ_NAME)"

ifeq ($(OS_detected),Linux)

$(PROJ_NAME): support-files

support-files:
	$(MY_MKDIR) $(BIN_DIR)
	@cp -p $(BHP_ROOT)/impl/test_atp_vs15/Debug/test_atp.ini $(BIN_DIR)
	@cp -p $(BHP_ROOT)/impl/test_atp_vs15/Debug/test_atp_cfg.ini $(BIN_DIR)
	@cp -p /usr/i686-w64-mingw32/sys-root/mingw/bin/*.dll $(BIN_DIR)

endif

# --------------------------------clean-----------------------------------
.PHONY: clean
clean:
	$(MY_RM) -rf $(OBJDIR)/*
	$(MY_RM) -rf $(BIN_DIR)/atp_bhp_emd
	$(MY_RM) -rf $(BIN_DIR)/atp_bhp_hil
	$(MY_RM) -rf $(BIN_DIR)/atp_bhp_vsim

.PHONY: distclean
distclean: clean
	$(MY_RM) -r $(DISTDIR)
	$(MY_RM) -r $(dir $(OBJDIR))


.PHONY:dist
dist:
	@echo -e ""
	@echo -e " Making distribution from $(DISTDIR)"
	@echo -e "      to $(ExportDISTDIR)"
	@if test -d $(DISTDIR); then \
		cd $(DISTDIR) && \
		for file in `find . -type f `; do \
			$(INSTALL) -m 777 -D --verbose $${file} ../../$(ExportDISTDIR)/$${file}; \
			if test $$? -ne 0 ; \
				then echo -e " !! Did you forget to check out the distributed files?" ; \
			fi ; \
		done; \
	else \
		echo -e " Could not find any files to distribute. Did you compile first?"; \
	fi

docs: $(ATC_HTML_FILES) $(CORE_HTML_FILES) $(BHP_HTML_FILES)

crc: $(PROJ_NAME).check

$(PROJ_NAME).check : $(PROJ_NAME)

$(PROJ_NAME).check :
	$(shell export LD_LIBRARY_PATH=$(CRC64SUM_PATH); $(CRC64SUM_PATH)/crc64sum $(PROJ_NAME) > $(PROJ_NAME).check)
	@echo -e " Generating CRC"


# ----------------------------help----------------------------------------
# If user give command "make help" this is what to do
help:
	@echo "Usage: make [all|clean|distclean|dist|lint] TARGET=[ARM|PPC|WIN32] ENV=[HIL|EMD|VSIM|SIL]"
	@echo ""
	@echo "Add VERBOSE=1 to get all output"
	@echo "Add TRACE=1 to build for trace"
	@echo "If a compiler is not installed, see readme.txt"
	@echo "If compilation takes too long, try adding: -j4"
	@echo ""


# ------------------------------dependencies-----------------------------
# Automatic dependency graph generation
# -MMD means that for each .o file a .d file shall also be generated.
# -include means we include the dependency file for each object file to make into this Makefile.
CC_FLAGS += -MMD
-include $(OBJ_FILES:.o=.d)

# -------------------------------end of file------------------------------
