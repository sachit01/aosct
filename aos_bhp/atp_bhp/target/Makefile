help:
	@echo 'make all VER=x.y.z            : builds all applications and packages'
	@echo 'make all-local VER=x.y.z      : copies source to ~/pdrive_local then builds all apps and packages'
	@echo '---'
	@echo 'make apps                     : builds all applications'
	@echo 'make apps ENVLIST="EMD VSIM"  : builds a subset of the applications'
	@echo 'make apps CLEAN=y             : cleans then builds all applications'
	@echo '---'
	@echo 'make apps-local               : cd:s to ~/pdrive_local then builds all applications'
	@echo 'make apps-local ENVLIST="HIL" : cd:s to ~/pdrive_local then builds a subset of the apps'
	@echo 'make apps-local CLEAN=y       : cd:s to ~/pdrive_local, cleans then builds all applications'
	@echo '---'
	@echo 'make cfg-packages VER=x.y.z   : generates the .bin files and creates the config .deb packages'
	@echo '---'
	@echo 'make lint                     : runs FlexeLint for all applications'
	@echo 'make lint-local               : copies source to ~/pdrive_local then runs FlexeLint for all apps'
	@echo '---'
	@echo 'make clean                    : runs make clean for all applications'
	@echo 'make clean-local              : cd:s to ~/pdrive_local then runs make clean for all applications'
	@echo '---'
	@echo 'make status                   : show git status'
	@echo 'make status-local             : show git status in ~/pdrive_local'
	@echo '---'
	@echo 'make prep                     : upload ssh key to targets to enable automatic login via pubkey'
	@echo 'make ping-all                 : ping all targets'
	@echo 'make ps-all                   : issue a ps on all targets'
	@echo '---'
	@echo 'make packages VER=x.y.z       : generates .deb packages in 'install_packages*' (build apps first!)'
	@echo 'make deploy-hil               : installs the HIL .deb packages on target'
	@echo 'make deploy-emd               : installs the EMD .deb packages on target'
	@echo 'make deploy-vsim              : installs the VSIM .deb packages on target'
	@echo 'make deploy-hil-cfg           : installs only the HIL config .deb package on target'
	@echo 'make deploy-emd-cfg           : installs only the EMD config .deb package on target'
	@echo 'make deploy-vsim-cfg          : installs only the VSIM config .deb package on target'
	@echo 'make uninstall                : uninstalls the .deb packages'
	@echo ''
	@echo 'Example: to build, deploy and run on HIL:'
	@echo '$$ make all-local VER=1.0.17test ENVLIST="HIL"'
	@echo '$$ make deploy-hil START=y'
	@echo ''
	@echo 'Example: to update config on HIL:'
	@echo '$$ make cfg-packages VER=1.0.17test'
	@echo '$$ make deploy-hil-cfg'
	@echo ''

SSHPARM=-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null

#1. create rsa key .ssh/id_rsa_target[.pub]
#2. append .ssh/id_rsa_target.pub to <target>/.ssh/authorized_keys, take care of not adding several times
prep:
	if [ ! -d ~/.ssh ]; then mkdir ~/.ssh; chmod 700 ~/.ssh; fi
	if [ ! -f ~/.ssh/id_rsa_target ]; then ssh-keygen -N '' -f ~/.ssh/id_rsa_target; fi
	if ! grep 192.168.2.10 ~/.ssh/config > /dev/null ; then \
	echo "Host 192.168.2.10" >> ~/.ssh/config; \
	echo "IdentityFile ~/.ssh/id_rsa_target" >> ~/.ssh/config; \
	echo "Host 192.168.2.11" >> ~/.ssh/config; \
	echo "IdentityFile ~/.ssh/id_rsa_target" >> ~/.ssh/config; \
	echo "Host 192.168.2.12" >> ~/.ssh/config; \
	echo "IdentityFile ~/.ssh/id_rsa_target" >> ~/.ssh/config; \
	fi; \
	chmod 600 ~/.ssh/config; \
	for ip in 192.168.2.10 192.168.2.11 192.168.2.12; do \
	    sshpass -p "admin" ssh $(SSHPARM) root@$$ip mount_rw; \
	done; \
	for n in root:admin atpcu:atpcu ; do \
	  user=`echo $$n | awk -F ':' '{print $$1}'`; \
	  pass=`echo $$n | awk -F ':' '{print $$2}'`; \
	  for ip in 192.168.2.10 192.168.2.11 192.168.2.12; do \
	    echo "-------- Set .ssh/id_rsa_target.pub as login for $$user:$$pass@$$ip ----------"; \
	    sshpass -p "$$pass" ssh $(SSHPARM) $$user@$$ip mkdir .ssh 2> /dev/null; \
	    sshpass -p "$$pass" ssh $(SSHPARM) $$user@$$ip chmod 700 .ssh 2> /dev/null; \
	    sshpass -p "$$pass" ssh $(SSHPARM) $$user@$$ip sed -i '' "'/$$(cat ~/.ssh/id_rsa_target.pub |  sed 's/\//\\\//g')/d'" .ssh/authorized_keys 2> /dev/null; \
	    cat ~/.ssh/id_rsa_target.pub | sshpass -p "$$pass" ssh $(SSHPARM) $$user@$$ip 'cat >> .ssh/authorized_keys' 2> /dev/null; \
	    sshpass -p "$$pass" ssh $(SSHPARM) $$user@$$ip chmod 600 .ssh/authorized_keys 2> /dev/null; \
	    echo "[+] test $$user@$$ip:"; \
	    ssh $(SSHPARM) $$user@$$ip uname -a 2> /dev/null; \
	  done; \
	done

#########################
# status

ping-all:
	@ping -c 3 192.168.2.10
	@echo
	@ping -c 3 192.168.2.11
	@echo
	@ping -c 3 192.168.2.12

ps-all:
	@echo "------------ ps on 192.168.2.10 ----------"
	@sshpass -p "admin" ssh $(SSHPARM) root@192.168.2.10 ps
	@echo "------------ ps on 192.168.2.11 ----------"
	@sshpass -p "admin" ssh $(SSHPARM) root@192.168.2.11 ps -aux
	@echo "------------ ps on 192.168.2.12 ----------"
	@sshpass -p "admin" ssh $(SSHPARM) root@192.168.2.12 ps

#########################
# compile

export TARGET
export ENV


# input variables:
# TARGET  : PPC | ARM
# ENV     : HIL | EMD | VSIM
# SRCROOT : p_drive source root or local directory to run compile in
# CLEAN   : if "y" then run make clean first
compile-one:
	@echo "[=] entering $(SRCROOT)"
	@if [ ! -d "$(SRCROOT)" ]; then \
		echo "[!] directory $(SRCROOT) does not exist"; \
	else \
		cd $(SRCROOT); set -o pipefail; \
		if [ "y" == "$$CLEAN" ]; then \
			echo "[o] make clean TARGET=$(TARGET) ENV=$(ENV)"; \
			make $(EXTRA_ARGS) -j4 clean || exit 1; \
		fi; \
		echo "[o] make $(TARGET_CMD) TARGET=$(TARGET) ENV=$(ENV)"; \
		make $(EXTRA_ARGS) -j4 $(TARGET_CMD) || exit 1; \
	fi;

TARGETLIST?=PPC ARM
ENVLIST?=EMD HIL VSIM
ENVLIST_LCASE=$(shell echo $(ENVLIST) | tr A-Z a-z)

export TARGETLIST
export ENVLIST

compile-loop:
	@for target in $(TARGETLIST); do \
		for env in $(ENVLIST); do \
			make --no-print-directory -C $(CURDIR) TARGET_CMD=$(TARGET_CMD) TARGET=$$target ENV=$$env SRCROOT=$(PROJS_ROOT)/aos_bhp/atp_bhp/impl compile-one || exit 1; \
		done; \
	done; \
	for env in $(ENVLIST); do \
		make  --no-print-directory -C $(CURDIR) TARGET_CMD=$(TARGET_CMD) TARGET=PPC ENV=$$env SRCROOT=$(PROJS_ROOT)/aos_bhp/dispatcher/impl compile-one || exit 1; \
	done;

P_DRIVE?=$(shell readlink -f $(CURDIR)/../../..)

LOCALDIR?=$(shell readlink -f ~/pdrive_local)

apps:
	@make --no-print-directory -C $(CURDIR) TARGET_CMD=all PROJS_ROOT=$(P_DRIVE) compile-loop

apps-local:
	@make --no-print-directory -C $(CURDIR) TARGET_CMD=all PROJS_ROOT=$(LOCALDIR) compile-loop

lint:
	@make --no-print-directory -C $(CURDIR) TARGET_CMD=lint PROJS_ROOT=$(P_DRIVE) compile-loop

lint-local: prepare-pdrive-local
	@make --no-print-directory -C $(CURDIR) TARGET_CMD=lint PROJS_ROOT=$(LOCALDIR) compile-loop

clean-rule:
	@make --no-print-directory -C $(CURDIR) TARGET_CMD=clean compile-loop
	@cd $(PROJS_ROOT)/aos_bhp/atc/nvshft; make TARGET=PPC clean
	@cd $(PROJS_ROOT)/aos_bhp/atc/nvshft; make TARGET=HOST ENV=SIL clean
	@cd $(PROJS_ROOT)/aos_bhp/atc/nvshfr; make TARGET=PPC clean
	@cd $(PROJS_ROOT)/aos_bhp/atc/nvshfr; make TARGET=HOST ENV=SIL clean
	@cd $(PROJS_ROOT)/aos_bhp/atc/platform_halt; make TARGET=PPC clean
	@cd $(PROJS_ROOT)/aos_bhp/atc/platform_halt; make TARGET=ARM clean

clean:
	@make PROJS_ROOT=$(P_DRIVE) clean-rule

clean-local:
	@make PROJS_ROOT=$(LOCALDIR) clean-rule

prepare-pdrive-local: dos2unix
	@echo "Prepare local builddir $(LOCALDIR)";
	@mkdir -p $(LOCALDIR)
	@mkdir -p $(LOCALDIR)/aos_bhp
	@mkdir -p $(LOCALDIR)/aos_bhp/tools_bhp
	@for d in aos_bhp/atc aos_bhp/atp_core aos_bhp/atp_bhp aos_bhp/dispatcher SPL_Delivery SetTimeOfDay TimeSyncServer; do \
		echo "[+] copy $${d}"; \
		rm -rf $(LOCALDIR)/$${d}; \
		cp -p -r $(P_DRIVE)/$${d} $(LOCALDIR)/$${d}; \
	done

all-local: check-all calc_crc nvshft nvshfr cfg-files platform_halt prepare-pdrive-local apps-local packages-local

all: check-all apps packages

check-all:
	@if [ -z "$(VER)" ]; then \
		echo "[!] Error: VER is not set"; \
		exit 1; \
	fi

#########################
# status

status:
	@for d in atc atp_core atp_bhp dispatcher tools_bhp ; do \
		cd $(P_DRIVE)/aos_bhp/$$d; echo $$d:; git status; \
	done

status-local:
	@for d in atc atp_core atp_bhp dispatcher ; do \
		cd $(LOCALDIR)/aos_bhp/$$d; echo $$d:; git status; \
	done

#########################
# build/run nvshft/nvshfr

calc_crc:
	@echo "[+] build calc_crc"
	@cd ../../atc/nvshft; make TARGET=HOST ENV=SIL calc_crc

nvshft:
	@echo "[+] build nvshft"
	@cd ../../atc/nvshft; make TARGET=PPC
	@cd ../../atc/nvshft; make TARGET=HOST ENV=SIL

nvshfr:
	@echo "[+] build nvshfr"
	@cd ../../atc/nvshfr; make TARGET=PPC
	@cd ../../atc/nvshfr; make TARGET=HOST ENV=SIL MY_ADDED_DEFINES=-DHOST

NVSHFT_DIR=../../atp_bhp/nvshft/data_files
NVSHFT_OPT=-g $(NVSHFT_DIR) -p $(NVSHFT_DIR) -b $(NVSHFT_DIR)

cfg-files:
	@echo "[+] run nvshft"
	@chmod a+rwx ../../atc/nvshft/nvshft
	@chmod a+rwx ../../atc/nvshft/nvshft_add_crc.sh
	@../../atc/nvshft/nvshft_add_crc.sh -n -p $(NVSHFT_DIR)
	@echo "[+] run nvshfr"
	@cd ../nvshft; ../../atc/nvshfr/nvshfr -a
	@rm -f $(NVSHFT_DIR)/*readout.txt

#########################
# build platform_halt

platform_halt:
	@echo "[+] build platform_halt"
	@cd ../../atc/platform_halt; make TARGET=PPC
	@cd ../../atc/platform_halt; make TARGET=ARM

#########################
# create/install/uninstall packages

dos2unix:
	@echo "[+] convert dos2unix"
	@for f in collect_logs.sh create_packages.sh deploy_packages.sh start_all.sh stop_all.sh uninstall_packages.sh; do \
		if ! dos2unix < $${f} | cmp -s - $${f} ; then \
			echo " -> convert dos2unix: $${f}"; \
			dos2unix $${f} 2>&1 > /dev/null; \
		fi; \
		chmod a+rwx $${f}; \
	done

packages: calc_crc nvshft nvshfr cfg-files platform_halt dos2unix
	@for env in $(ENVLIST_LCASE); do \
		fakeroot $(CURDIR)/create_packages.sh $$env $(VER); \
	done

packages-local:
	@chmod a+rwx $(LOCALDIR)/aos_bhp/atp_bhp/target/create_packages.sh
	@for env in $(ENVLIST_LCASE); do \
		(cd $(LOCALDIR)/aos_bhp/atp_bhp/target; fakeroot $(LOCALDIR)/aos_bhp/atp_bhp/target/create_packages.sh $$env $(VER)); \
	done
	@for f in $(LOCALDIR)/aos_bhp/atp_bhp/target/install_packages*; do \
		b=`basename $$f`; \
		echo "[+] copy deliverables to $$b"; \
		cp -p -r $$f $(CURDIR); \
	done

cfg-packages: cfg-files
	@for env in $(ENVLIST_LCASE); do \
		fakeroot $(CURDIR)/create_packages.sh $$env "$(VER)" config; \
	done

deploy-hil: dos2unix
	@$(CURDIR)/deploy_packages.sh hil
	@if [ "y" == "$$START" ]; then \
		$(CURDIR)/stop_all.sh; \
		$(CURDIR)/start_all.sh; \
	fi

deploy-hil-cfg: dos2unix
	@$(CURDIR)/deploy_packages.sh hil config

deploy-emd: dos2unix
	@$(CURDIR)/deploy_packages.sh emd
	@if [ "y" == "$$START" ]; then \
		$(CURDIR)/stop_all.sh; \
		$(CURDIR)/start_all.sh; \
	fi

deploy-emd-cfg: dos2unix
	@$(CURDIR)/deploy_packages.sh emd config

deploy-vsim: dos2unix
	@$(CURDIR)/deploy_packages.sh vsim
	@if [ "y" == "$$START" ]; then \
		$(CURDIR)/stop_all.sh; \
		$(CURDIR)/start_all.sh; \
	fi

deploy-vsim-cfg: dos2unix
	@$(CURDIR)/deploy_packages.sh vsim config

uninstall: dos2unix
	@$(CURDIR)/uninstall_packages.sh
