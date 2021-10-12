/**********************************************************************
                     Bombardier Transportation
 
 Author      : $Author: bvilcens $
 Revision    : $Revision: 1.5 $
 Date        : $Date: 2006/01/30 13:54:34 $
 File name   : $Source: /home/cvs/IP-Profibus-Driver/inc/pl_driver_vipco.h,v $
 
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 
    $Log: pl_driver_vipco.h,v $
    Revision 1.5  2006/01/30 13:54:34  bvilcens
    Fix Version 0.97

    Revision 1.4  2005/11/17 13:40:18  bvilcens
    Changes in build process

    Revision 1.3  2005/10/12 07:40:12  bvilcens
    Doxygen Kommentare eingefügt.
    Minimale Änderungen.

    Revision 1.2  2005/10/11 09:55:19  bvilcens
    Stand 11.10.05
    Debug Messages
    verschiedene kleinere Änderungen

    Revision 1.1  2005/10/06 13:56:14  bvilcens
    *** empty log message ***
 
 **********************************************************************/
 
 /**
 * @mainpage Profibus IPack VxWorks library
 * 
 * This library implements the pl_driver interface used on the VCU-Lite for the VIPCO ipack profibus device @n@n
 *
 *
 *  Project: IPack Profibus@n
 *  Version: 1.00@n
 *  Author : Björn Vilcens (VIPCO GmbH)@n
 *  Build Platform: Windows XP / Tornado IDE 2.02 with patch@n
 *  Target: PPC MC68360 VxWorks 5.2.1@n
 *  Compiler: delivered with Tornado IDE@n
 *
 */

/**
 * \defgroup DRIVER_IPACK_VIPCO Driver_IPack
 * 
 *@{
 */


#ifndef _PL_DRIVER_VIPCO_H
#define _PL_DRIVER_VIPCO_H

#include "pl_driver.h"


#define VIPCO_DRIVER_VERSION 0x0120
/** @def VIPCO_DRIVER_VERSION version identifier for the driver implementation */

#define VIPCO_DRIVER_VERSION_TAG    "release\6"
/** @def VIPCO_DRIVER_VERSION_TAG additional version identifier */

/**********************************************************************************************
 * plGetDriverStatus
 * Implementation of PL_Get_Driver_Status function call.
 *
 *    Returns the Driver status as a set of bits.
 *
 *    If the returned value is 0, the caller may
 *        assume the driver is fully operational.
 *
 *    This function may be called at any momment.
 *
 *    If it is called before PL_Initialize_Bus, the returned value shall have the
 *        PL_DRIVER_STATUS_NOT_INITIALIZED bit set.
 *
 *    If it is called after PL_Initialize_Bus and before PL_Shutdown,
 *        the returned value shall have the PL_DRIVER_STATUS_NOT_INITIALIZED and
 *        the bit PL_DRIVER_STATUS_SHUTDOWN cleared.
 *
 *    If it is called after PL_Shutdown,
 *        the bit PL_DRIVER_STATUS_SHUTDOWN set.
 *
 *    If the station is exchangnig tokens with other
 *        profibus stations, the PL_DRIVER_STATUS_NOT_IN_RING shall
 *        be cleared.
 *
 *
 * Returns
 *    A 32 bit field where each bit has a meaning.
 *
 *********************************************************************************************/
spDWORD plGetDriverStatus(void);

/**********************************************************************************************
 * plInitialize
 * Implementation of PL_Initialize_Bus function call. 
 *
 *    Initializes the Profibus interface.
 *
 *    Uses the provided PL_Driver_Initialisation_Struct structure to setup
 *       the interface with the user defined Profibus FDL parameters.
 *
 * Parameters:
 *    Initialization_Data: A non-null pointer to a structure with
 *        the Profibus Bus Parameters.
 *    The declaration (const * const) implies that the function
 *        shall not change this icoming parameter.
 *        The data may be located in ROM.
 *
 * Returns
 *
 *   PL_DRIVER_NO_ERROR:
 *        Interface is properly initialized.
 *        Interface is ready.
 *        User may call any other interface service.
 *
 *   PL_DRIVER_INVAL
 *        At least one value in the Initialisation_Data is invalid.
 *        Interface is not ready.
 *        User may not call any other service.
 *
 *   PL_DRIVER_ERROR
 *        Some error occured while initialization.
 *        Interface is not ready.
 *        User may not call any other service.
 *********************************************************************************************/
spINT plInitialize(PL_Driver_Initialisation_Struct * Initialization_Data);

/**********************************************************************************************
 * plShutdown
 * Implementation of the PL_Shutdown function call.
 *
 *    Definitively shuts down the Profibus Interface.
 *
 *    The Interface is not present anymore on the Profibus Network
 *          (This station is not visible on the Network).
 *    The Interface is not active anymore on the host
 *          side (interrupts, tasks, ...)
 *
 *    The user shall not use any other PL_Driver service after
 *          this one.
 *
 * Returns
 *
 *   PL_DRIVER_NO_ERROR:
 *        Interface is properly closed.
 *        User may not call any other interface service.
 *
 *   PL_DRIVER_ERROR
 *        Some error occured while closing.
 *        User may not call any other interface service.
 *********************************************************************************************/
spINT plShutdown(void);

/**********************************************************************************************
 * plEnableSap
 * Implementation of the PL_Enable_SAP function call.
 *
 *    Instructs the Profibus interface to allow incoming/outgoing telegrams
 *       on one particular Service Access Point.
 *
 *    The caller must call this service for each SAP before trying to
 *       send/receive any telegram on this SAP.
 *
 *    The service shall ensure that the SAP shall be able to
 *       send to (receive from) any physical address.
 *
 *    The Allow_Input and Allow_Output parameters are related to the
 *       Profibus notion of Role (Initiator, Responder, Both)
 *
 * Parameters
 *   Sap_Nr:
 *        A valid EN50170 SAP number in the range [0..62].
 *
 *   Fdl_Mode:
 *        One of these two predefined constants:
 *            FDL_MODE_SDN: Profibus Send Data No acknowledge.
 *            FDL_MODE_SDA: Profibus Send Data Acknowledge.
 *
 *
 *   Allow_Input:
 *        If set, the caller wants to receive any incoming telegram having
 *           this SAP as destination.
 *        If not set, the incoming telegrams may be blocked by the interface.
 *           this SAP as destination.
 *
 *   Allow_Output:
 *        If set, the caller wants to send telegrams having through this SAP.
 *        If not set, the caller may not send telegrams through this SAP.
 *
 * Returns
 *
 *   PL_DRIVER_NO_ERROR:
 *        SAP is properly enabled.
 *        User may call send or receive according to the Allow_Input/Allow_Output
 *        parameters.
 *
 *   PL_DRIVER_INVAL
 *        At least one parameter value is invalid.
 *        SAP is not enabled.
 *        No transmission/reception is possible on this SA.
 *
 *   PL_DRIVER_ERROR
 *        Some error occured while enabling the SAP.
 *        SAP is not enabled.
 *        No transmission/reception is possible on this SA.
 *
 *********************************************************************************************/
spINT plEnableSap (const spBYTE       Sap_Nr,
                   const spBYTE       Fdl_Mode,
                   const spBOOL       Allow_Input,
                   const spBOOL       Allow_Output,
                   const spBOOL       Priority);

/**********************************************************************************************
 * plSendTelegram
 * Implementation of the PL_Send_Telegram function call.
 *
 *    Requests the interface to send a telegram over the Profibus Bus.
 *
 *    The Interface must have been successfully initialized (PL_Initialize_Bus).
 *
 *    The SAP reffered in the incoming parameter pTelegram->as.Telegram.SourceSAP
 *        must have been successfully enabled.
 *
 *    The SLL_Telegram_Struct must properly filled.
 *
 *    The PL_Send_Telegram shall send the bytes reffered in
 *       pTelegram->Data to the Profibus Network
 *       with the proper protocol.
 *
 *    The PL_Send_Telegram shall not wait for any confirmation from its
 *       Profibus interface, it shall return immediately.
 *
 * Returns
 *
 *   PL_DRIVER_NO_ERROR:
 *        Telegram was sent to the Interface.
 *
 *   PL_DRIVER_ERROR_ON_SEND:
 *        Some error occured while sending the telegram to the interface.
 *        User should abort connections on this SAP.
 *********************************************************************************************/                  
spINT plSendTelegram (const SLL_Telegram_Struct * const pTelegram);

/**********************************************************************************************
 * plGetSapStatus
 * Implementation of the PL_Get_SAP_Status function call.
 *
 *    Returns the status of one individual SAP.
 *
 *    If the returned value is 0, the caller may
 *        assume the SAP is fully operational for
 *        sending or receiving (depending on SAP_Enable parameters).
 *
 *    This function may be called at any momment.
 *
 * Returns
 *    A 32 bit field where each bit has a meaning.
 *
 *********************************************************************************************/
spDWORD plGetSapStatus (const spBYTE SapNr);

/**********************************************************************************************
 * plCollect
 * Implementation of the PL_Collect function call.
 *
 *    Collects Telegrams from the PL_Driver.
 *
 *    The Interface must have been successfully initialized (PL_Initialize_Bus).
 *
 *    The caller shall call PL_Collect while the returned value is
 *        PL_DRIVER_TELEGRAM_AVAILABLE
 *
 *    If the Driver collects any other information than incoming telegrams from
 *        the interface it is the responsibility to store this information
 *        for later processing by PL_Get_SAP_Status or PL_Get_Driver_Status.
 *
 * Parameters:
 *    pTelegram:
 *        A (non-null) pointer to a user provided structure that shall be
 *        filed by the PL_Collect.
 *
 * Returns
 *   PL_DRIVER_NO_ERROR:
 *        pTelegram is properly filled.
 *
 *   PL_DRIVER_NO_DATA:
 *        No telegram available.
 *
 *   PL_DRIVER_ERROR:
 *        Error while trying to read data from interface.        
 *********************************************************************************************/
spINT plCollectTelegram (SLL_Telegram_Struct * const pTelegram);

/**********************************************************************************************
 * plGetLifeList
 *
 * Implemenation of the Get_Life_List function call.
 * 
 *    Get the Profibus live list:
 *
 * Parameters: spBYTE * Live_List: buffer with Profibus station status values
 *             The buffer must be 127 bytes long. Each byte corresponds to one 
 *             physical address of the bus and is set to 1 if the physical address 
 *             is on the bus and to 0 if not.
 *
 * Returns:
 *   PL_DRIVER_NO_ERROR: live list returned
 *   PL_DRIVER_NO_DATA:  live list currently not available
 *   PL_DRIVER_ERROR:  live list not supported
 *
 *********************************************************************************************/
spINT plGetLifeList (spBYTE * Live_List);

/***********************************************************************************************
 * plSynch
 * Implementation of the PL_Synch function call.
 *    This function will synchronize the clocks between IP module and hostsystem.
 *    In addition a lifesign test will be performed.
 *
 *       @return 
 *   
 *   PL_DRIVER_NO_ERROR: Clock update was set inside the DPRAM. IP Module is alive 
 *   PL_DRIVER_ERROR:  Timeout of IP module recognized
 *
 *********************************************************************************************/
spINT plSynch (void);


extern PLD2_Struct PL_Driver_Vipco;


#endif

/** @} */

