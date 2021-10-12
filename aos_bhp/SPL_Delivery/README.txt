######################################################################
#                      Bombardier Transportation
#
#                      %created_by:     jkrupp %
#                      %version:        1 %
#                      %date_created:   2013-09-16 10:06 %
#
######################################################################
#
#   Revision    Date        Name        Description
#	1           2013-06-11  jkrupp      initial version
#
######################################################################

------------------------
SPL_Delivery project
------------------------
check out complete SPL_Delivery project

Regular users:
--------------------------------
If you want to use the SPL library with the regular API

How to:
--------------------------------
- Include directory for build
    /inc
- Lib path for build
    /lib/<drivertype>_<diversificationtype>_<basesystem>_<safetylevel>__

Examples:
For the ATPCU you have to use  the spl.lib in atpcuA_ab_tbsw_sil4__ and atpcuB_ab_tbsw_sil4__.
For the OPC you have to use the spl.lib in udp_ab_tbsw_sil4__
For the TSG with direct communication with ATPCU over Ethernet you have to use the spl.lib in sudp_a_css_sil0__.



Extended Users:
--------------------------------
If you want to use the SPL library with the regular API plus direct access to the driver under the SPL for Profibus and UDP.
This is needed for the OPC.
If you are a regular user DO NOT use this interface and DO NOT include the header files.

How to use for extended users:
--------------------------------
- same as regular users but additonal include directory for build
    /incDrv
    
    