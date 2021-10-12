 ./startLintAll.sh                                    FlexeLint for C/C++ (Unix) Vers. 9.00j, Copyright Gimpel Software 1985-2012
FlexeLint for C/C++ (Unix) Vers. 9.00j, Copyright Gimpel Software 1985-2012
FlexeLint for C/C++ (Unix) Vers. 9.00j, Copyright Gimpel Software 1985-2012
FlexeLint for C/C++ (Unix) Vers. 9.00j, Copyright Gimpel Software 1985-2012
FlexeLint for C/C++ (Unix) Vers. 9.00j, Copyright Gimpel Software 1985-2012
FlexeLint for C/C++ (Unix) Vers. 9.00j, Copyright Gimpel Software 1985-2012




where is the result stored? 
same ditrectory



Where in the script do you state which misra-rules we use
4:57:23 PM: Achim-Olaf Zacher/DE/Transport/Bombardier: there are several lint config with extension .lnt in the folder


Achim-Olaf Zacher/DE/Transport/Bombardier: result in files 
4:58:31 PM: Achim-Olaf Zacher/DE/Transport/Bombardier:  
4:58:34 PM: Achim-Olaf Zacher/DE/Transport/Bombardier: -rw-r--r-- 1 azacher users 305183 Jul 23 16:54 lintClientJbsd.tmp
-rw-r--r-- 1 azacher users 157221 Jul 23 16:54 lintClientJlinux.tmp
-rw-r--r-- 1 azacher users 155490 Jul 23 16:54 lintClientJx86.tmp
-rw-r--r-- 1 azacher users 122832 Jul 23 16:54 lintServerJbsd.tmp
-rw-r--r-- 1 azacher users 103765 Jul 23 16:54 lintServerJlinux.tmp
-rw-r--r-- 1 azacher users 101241 Jul 23 16:54 lintServerJx86.tmp
-rw-r--r-- 1 azacher users  72440 Jul 23 16:54 stacklnt.tmp

4:59:24 PM: Achim-Olaf Zacher/DE/Transport/Bombardier: we use all lint level 3 and all MISRA CPP 2008 as checked by lint as default
5:00:18 PM: Achim-Olaf Zacher/DE/Transport/Bombardier: deviations can be found in code with reg ex    \/[\/\*]lint
5:00:49 PM: Achim-Olaf Zacher/DE/Transport/Bombardier: deviations for project can be found in vio_project.lnt