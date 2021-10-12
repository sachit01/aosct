/** @file 
 * The Profibus Layer Driver definition.
 *
 * The Profibus Driver is made of 7 basic services.
 *
 * PL_Initialize:<br>
 *     Called once, the physical interface is supposed to
 *       be ready to send/receive after this.<br>
 *
 * PL_Shutdown:<br>
 *     Called once, the physical interface is supposed to
 *       be physically disconnected from the network and
 *       to be idle from the HOST copu point of view.<br>
 *
 * PL_Enable_SAP:<br>
 *     Called a few times to enable some specific SAPs before
 *       trying to send/receive on them.<br>
 *
 * PL_Send:<br>
 *     Called to send a telegram.<br>
 *
 * PL_Collect:<br>
 *     Called cyclically (based on the application cycle).
 *
 *     On each cycle, it is called has many times as needed
 *        in order to collect all the available telegrams
 *        at the interface.
 *
 *     It is the responsibility of this service to do
 *        any private house-keeping like processing
 *        aknowlegements from the interface, converting
 *        interface errors to PL_Get_Driver_Status and
 *        PL_Get_SAP_Status compatible formats.<br>
 *
 * PL_Get_Driver_Status:<br>
 *     Provide global information about the interface status.<br>
 *
 * PL_Synchronize:<br>
 *     Synchronize local clock and Profibus hardware clock<br>
 *
 * PL_Get_SAP_Status:<br>
 *     Provide local (different for each SAP) information
 *        for each SAP.
 * 
 */
 /*********************************************************************
 *                    Bombardier Transportation
 *
 *                      %created_by: ageck %
 *                      %version: 10 %
 *                      %date_created: Wed Jan 31 13:10:36 2007 %
 **********************cvs out of date********************************* 
 *                      $Author: jkusmira $
 *                      $Revision: 1.18 $
 *                      $Date: 2005/06/28 07:52:35 $
 *                      $Source: P://mpg/sl/pl/pl_driver.h,v $
 *
 *  $Log: pl_driver.h,v $
 *  Revision 1.18  2005/06/28 07:52:35  jkusmira
 *   Fixed Doxygen Comments 1 Stage
 *
 *  Revision 1.17  2005/05/20 14:23:13  mkilianj
 *  converting to doxygen style comments
 *
 *  Revision 1.16  2005/04/11 07:38:46  rsmilgin
 *  change type of Indication from spBOOL into spBYTE;
 *  spBYTE Dummy - new member of structure PL_Driver_Telegram_Struct
 *
 *  Revision 1.15  2005/04/01 14:44:06  AGeck
 *  Changed value for PL_DRIVER_FATAL_ERROR to fit in lsb byte
 *  New functions PLMan_Get_Driver/SAP_Status
 *  Completed PLMan_Get_Life_list
 *
 *  Revision 1.14  2005/03/24 15:04:06  mjoost
 *  add livelist
 *
 *  Revision 1.13  2005/03/24 08:22:19  rsmilgin
 *  Minor change in PL_Driver_Telegram_Struct
 *
 *  Revision 1.12  2005/03/11 14:54:43  mjoost
 *  added bool variable indication to pl_driver_telegram_struct
 *
 *  Revision 1.11  2005/02/24 15:52:07  mjoost
 *  added "PL_SAP_STATUS_SAP_OUT_OF_RANGE"
 *
 *  Revision 1.10  2005/02/21 16:36:35  mjoost
 *  changes according to new pl_driver interface
 *
 *
 **********************************************************************/
#ifndef PROFIBUS_LAYER_DRIVER_H_
#define PROFIBUS_LAYER_DRIVER_H_

#include "pl_fdl.h"
#include "sll_telegram.h"

#include "pl_driver_types.h"
#include "spla_protocol_parameters.h"
/**
 * @name
 * <b>Global return codes.</b>
 ***********************************************************************/
/*@{*/
/**
 * Every thing is fine. */
#define PL_DRIVER_NO_ERROR           0
/**
 * At least one parameter is invalid. */
#define PL_DRIVER_INVAL              -1
/**
 * Hardware error. */
#define PL_DRIVER_ERROR              -2
/**
 * Generic error when sending a telegram to the interface. */
#define PL_DRIVER_ERROR_ON_SEND      -4
/** 
 * Telegram available. */
#define PL_DRIVER_TELEGRAM_AVAILABLE 5
/**
 * No data. */
#define PL_DRIVER_NO_DATA            6   
/*@}*/

/* declaration of PL_Driver_Initialisation_Struct moved to spla_protocol_parameters.h */

/**
 * Initializes the Profibus interface.
 *
 *    Uses the provided PL_Driver_Initialisation_Struct structure to setup
 *       the interface with the user defined Profibus FDL parameters.
 *
 *      @param Initialization_Data     A non-null pointer to a structure with
 *                                     the Profibus Bus Parameters.
 *                                     The declaration (const * const) implies
 *                                     that the function shall not change this
 *                                     incoming parameter.
 *                                     The data may be located in ROM.
 *
 *       @return Result =PL_DRIVER_NO_ERROR:
 *                                Interface is properly initialized.
 *                                Interface is ready.
 *                                User may call any other interface service.
 *                      =PL_DRIVER_INVAL:
 *                                At least one value in the Initialisation_Data is invalid.
 *                                Interface is not ready.
 *                                User may not call any other service.
 *                      =PL_DRIVER_ERROR:
 *                                Some error occured while initialization.
 *                                Interface is not ready.
 *                                User may not call any other service.
 *****************************************************************************/
typedef spINT (* Prototype_PL2_Initialize)
                               (const PL_Driver_Initialisation_Struct * const Initialization_Data);


/**
 * Definitively shuts down the Profibus Interface.
 *
 *    The Interface is not present anymore on the Profibus Network
 *          (This station is not visible on the Network).
 *    The Interface is not active anymore on the host
 *          side (interrupts, tasks, ...)
 *
 *    The user shall not use any other PL_Driver service after
 *          this one.
 *
 *       @return Result      =PL_DRIVER_NO_ERROR:
 *                                Interface is properly closed.
 *                                User may not call any other interface service.
 *                           =PL_DRIVER_ERROR:
 *                                Some error occured while closing.
 *                                User may not call any other interface service.
 ******************************************************************************/
typedef spINT (* Prototype_PL2_Shutdown) (void);

/**
 * @name
 * <b>Driver status define.</b>
 ******************************************************************************/ 
 /*@{*/
/**
 * Board not initialized or initialization failed */
#define PL_DRIVER_STATUS_NOT_INITIALIZED  0x00000001UL
/**
 * PL_Shutdown was called.                        */
#define PL_DRIVER_STATUS_SHUTDOWN         0x00000002UL
/**
 * No tokens are exchanged (OUT_OF_RING)          */
#define PL_DRIVER_STATUS_NOT_IN_RING      0x00000004UL  
/**
 * Another station has the same physical address. */
#define PL_DRIVER_STATUS_DUPLICATE_ADDR   0x00000008UL  
/**
 * Unexpected or unrecoverable driver state */
#define PL_DRIVER_STATUS_FATAL_ERROR      0x00000080UL  
 /*@}*/
 
/**
 * Returns the Driver status as a set of bits.
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
 *       @return Result A 32 bit field where each bit has a meaning.
 *
 ******************************************************************************/
typedef spDWORD (* Prototype_PL2_Get_Driver_Status) (void);

 /**
 * @name
 * <b>Driver SAP status define.</b>
 ******************************************************************************/ 
 /*@{*/
/**
 * PL_Enable was not called or failed.            */
#define PL_SAP_STATUS_NOT_ENABLED      0x00000001UL  
/**
 * At least one negative ACK was received         */
#define PL_SAP_STATUS_NACK_DETECTED    0x00000002UL  
/**
 * Overflow of incoming fifos/queues              */
#define PL_SAP_STATUS_INPUT_OVERFLOW   0x00000004UL  
/**
 * Overflow of outgoing fifos/queues              */
#define PL_SAP_STATUS_OUTPUT_OVERFLOW  0x00000008UL  
/**
 * SAP number is out of range                     */
#define PL_SAP_STATUS_SAP_OUT_OF_RANGE 0x00000010UL  
/*@}*/

/**
 * PL_Get_SAP_Status
 *
 *    Returns the status of one individual SAP.
 *
 *    If the returned value is 0, the caller may
 *        assume the SAP is fully operational for
 *        sending or receiving (depending on SAP_Enable parameters).
 *
 *    This function may be called at any momment.
 * 
 *       @param SapNr   Variable description.
 * 
 *       @return Result A 32 bit field where each bit has a meaning.
 *
 ******************************************************************************/
typedef spDWORD (* Prototype_PL2_Get_SAP_Status) (const spBYTE SapNr);


/**
 * Instructs the Profibus interface to allow incoming/outgoing telegrams
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
 *       @param Sap_Nr  A valid EN50170 SAP number in the range [0..62].
 *
 *       @param Fdl_Mode     One of these two predefined constants:
 *                           =FDL_MODE_SDN: Profibus Send Data No acknowledge.
 *                           =FDL_MODE_SDA: Profibus Send Data Acknowledge.
 *
 *       @param Allow_Input  If set, the caller wants to receive any incoming
 *        telegram having this SAP as destination.
 *        If not set, the incoming telegrams may be blocked by the interface
 *           this SAP as destination.
 *
 *       @param Allow_Output If set, the caller wants to send telegrams having
 *        through this SAP.
 *        If not set, the caller may not send telegrams through this SAP.
 *
 *       @param Priority     Priority of messages to be sent from this SAP<br>
 *                           =spTRUE:  HIGH <br>
 *                           =spFALSE: LOW  <br>
 *
 *
 *       @return Result      =PL_DRIVER_NO_ERROR:<br>
 *                                SAP is properly enabled.
 *                                User may call send or receive according to
 *                                the Allow_Input/Allow_Output parameters.<br>
 *                           =PL_DRIVER_INVAL:<br>
 *                                At least one parameter value is invalid.
 *                                SAP is not enabled.
 *                                No transmission/reception is possible on this SA.<br>
 *                           =PL_DRIVER_ERROR:<br>
 *                                Some error occured while enabling the SAP.
 *                                SAP is not enabled.
 *                                No transmission/reception is possible on this SA.
 *
 ******************************************************************************/
typedef spINT (* Prototype_PL2_Enable_SAP)
                               (const spBYTE       Sap_Nr,
                                const spBYTE       Fdl_Mode,
                                const spBOOL       Allow_Input,
                                const spBOOL       Allow_Output,
                                const spBOOL       Priority );


/**
 *    Requests the interface to send a telegram over the Profibus Bus.
 *
 *    The Interface must have been successfully initialized (PL_Initialize_Bus).
 *
 *    The SAP reffered in the incoming parameter pTelegram->as.Telegram.SourceSAP
 *        must have been successfully enabled.
 *
 *    The PL_Driver_Telegram_Struct must properly filled.
 *
 *    The PL_Send_Telegram shall send the bytes reffered in
 *       pTelegram->Data to the Profibus Network
 *       with the proper protocol.
 *
 *    The PL_Send_Telegram shall not wait for any confirmation from its
 *       Profibus interface, it shall return immediately.
 *
 *       @param    pTelegram       (non-null) pointer to a user provided
 *                                structure that shall be filed by the PL_Collect.
 * 
 *       @return   Result         =PL_DRIVER_NO_ERROR:<br>
 *                                     Telegram was sent to the Interface.<br>
 *                                 =PL_DRIVER_ERROR_ON_SEND:<br>
 *                                     Some error occured while sending the
 *                                     telegram to the interface.
 *                                     User should abort connections on this SAP.
 ******************************************************************************/
typedef
        spINT (* Prototype_PL2_Send_Telegram) (const SLL_Telegram_Struct * const pTelegram);

/**
 *    Collects Telegrams from the PL_Driver.
 *
 *    The Interface must have been successfully initialized (PL_Initialize_Bus).
 *
 *    The caller shall call PL_Collect while the returned value is
 *        PL_DRIVER_NO_ERROR.
 *
 *    If the Driver collects any other information than incoming telegrams from
 *        the interface it is the responsibility to store this information
 *        for later processing by PL_Get_SAP_Status or PL_Get_Driver_Status.
 *
 *        @param pTelegram        A (non-null) pointer to a user provided
 *                                structure that shall be filed by the PL_Collect.
 *
 *        @return Result          =PL_DRIVER_NO_ERROR Telegram is available. 
 *                                 pTelegram is properly filled.
 *                                =PL_DRIVER_ERROR  Driver error
 *                                =PL_DRIVER_NO_DATA  No messages available.  Polling can stop.
 *                                 
 ******************************************************************************/
typedef
        spINT (* Prototype_PL2_Collect) (SLL_Telegram_Struct * const pTelegram);

/**
 * Synchronize the clocks of the main board and Profibus hardware
 *
 *       @return Result      =PL_DRIVER_NO_ERROR: 
 *                                     Synchronize sucessful
 *                           =PL_DRIVER_ERROR:
 *                                     Synchronize failed
 ******************************************************************************/
typedef spINT (* Prototype_PL2_Synchronize) (void);

/**
 * Get the Profibus live list.
 *
 *       @param Live_List    Buffer with Profibus station status values.
 *             The buffer must be 127 bytes long. Each byte corresponds to one 
 *             physical address of the bus and is set to 1 if the physical address 
 *             is on the bus and to 0 if not.
 *
 *       @return Result      =PL_DRIVER_NO_ERROR: <br>
 *                                     Live list returned.<br>
 *                           =PL_DRIVER_NO_DATA:  <br>
 *                                     Live list currently not available.<br>
 *                           =PL_DRIVER_ERROR:<br>
 *                                     Live list not supported.
 ******************************************************************************/
typedef spINT (* Prototype_PL2_Get_Live_List) (spBYTE * Live_List);

/**
 * A Profibus_Layer Driver is a structure made of pointers to driver functions.
 ********************************************************************/
typedef struct
{
    Prototype_PL2_Initialize         Initialize;
    Prototype_PL2_Shutdown           Shutdown;
    Prototype_PL2_Get_Driver_Status  Get_Driver_Status;
    Prototype_PL2_Get_SAP_Status     Get_SAP_Status;
    Prototype_PL2_Enable_SAP         Enable_SAP;
    Prototype_PL2_Send_Telegram      Send_Telegram;
    Prototype_PL2_Collect            Collect_Telegram;
    Prototype_PL2_Synchronize        Synchronize;
    Prototype_PL2_Get_Live_List      Get_Life_List;
} PLD2_Struct;


/**
 * The current Profibus Layer Driver is stored in PL_Driver.
 * The initial value points to the PL_Null_Driver.
 *****************************************************************************/
extern PLD2_Struct PL_Driver_Instance;

#endif /* PROFIBUS_LAYER_DRIVER_H_ */
