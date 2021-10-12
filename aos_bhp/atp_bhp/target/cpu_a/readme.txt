***************************************************************
*

* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016

*

* We reserve all rights in this file and in the information

* contained therein. Reproduction, use or disclosure to third

* parties without written authority is strictly forbidden.

*

*  DESCRIPTION:
*  Instructions on starting up ATP on the target (CPU-A)

*
***************************************************************



***************************************************************
*

* REVISION HISTORY :
*

* Date          Name        Changes

* ------------------------------------------------------------

* 2017-02-16    nsyed       Created

***************************************************************


Below give are the steps to be followed in order to load and start ATP on the target(CPU-A): 


Preparing the input files:
-----------------------------------

1. Build ATP for PPC, HIL/EMD/VSIM environment.
2. Pull the latest cfg_data.bin and mnt_data.bin from gerrit(atp_bhp\target) or generate using NVSHFT.
3. Obtain the latest and relavant (HIL/EMD/VSIM) start-up script from gerrit (atp_bhp\target\cpu_a).
4. Obtain the settimeofday executable binary from \\scan\data\SE\GO\Projects\INTERFLO150\BHP_AOS_Work\TimeSync\SetTimeOfDay\bin\Linux_ppc



Loading the files onto the target:

-----------------------------------

1. SSH into CPU-A (192.168.2.10)
2. Login as "root" and password "admin"

3. Run the command "mount_rw" on CPU-A (using Putty)

4. Place the startup script under /opt/bin (Using WinSCP)

5. Place the startup_system_generic.sh and startup_system.sh under /opt/bin (Using WinSCP)

6. Place cfg_data.bin, rt_data.bin and mnt_data.bin under /optdata/data/aos (Using WinSCP on CPU-C)

7. Place the settimeofday binary in /opt/bin/aos
8. Set the right access rights for settimeofday (chmod 5777 /opt/bin/aso/settimeofday)

9. Run the command "sync" on CPU-A

10. Follow the start-up instructions(readme.txt) for CPU-B
11. Restart the system

12. Run "top" command on CPU-A to verify that the VFW and the ATP applications are running.

13. You can also connect to the ATP console using putty (IP:192.168.2.10, Port: 30165)