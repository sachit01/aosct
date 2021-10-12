/** @file
 *  @brief error codes definitions.
 *  Generic Profibus Safety Layer application interface (API)
 *  Corresponding to 3NGM005003_PBSL_Generic_API.doc
 *
 *  Definition: 
    -# OK == SPL_OK == 0
    -# States > 0
    -# Errors < 0
   
    Error == Function_Offset + Error_Value
    e.g.
      SPL_ERROR_BAD_OBJECT ==   -5
      SPL_INIT_ERROR       == -100
      
      SPL_INIT_BAD_OBJECT  == -105
      
 */
/*********************************************************************
 *                    Bombardier Transportation
 *
 *
 ***************************************************************************/

#ifndef SPLA_ERRORCODES_H_
#define SPLA_ERRORCODES_H_

/*************************************************************************************
**************************************************************************************
DO NOT ADD ANY INCLUDES OR IFDEFS TO THAT FILE TO KEEP APPLICATION INTERFACE SIMPLE!!!
*************************************************************************************
**************************************************************************************/

/** @defgroup FUNCTION_STATE_CODE  Function State Codes
 */
/*@{*/
#define  SPL_BUS_STATE        500 /**< Function state offset bus management.*/
#define  SPL_RECV_STATE       600 /**< Function state offset receive functions.*/
#define  SPL_GETR_STATE       700 /**< Function state offset get reason.*/
#define  SPL_STATE           1000 /**< States.*/
#define  SPL_CLOCK_STATE     1100 /**< Clock States.*/
/*@}*/
 
/** @defgroup FUNCTION_ERROR_CODE  Function Error Codes
 */
/*@{*/
#define  SPL_INIT_ERROR              (-100) /**< Function error offset init function.*/
#define  SPL_CONNECT_ERROR           (-200) /**< Function error offset connect function.*/
#define  SPL_DISCON_ERROR            (-300) /**< Function error offset disconnect function.*/
#define  SPL_SEND_ERROR                 (-400) /**< Function error offset send functions.*/
#define  SPL_BUS_ERROR               (-500) /**< Function error offset bus management.*/
#define  SPL_RECV_ERROR              (-600) /**< Function error offset receive functions.*/
#define  SPL_GETR_ERROR                 (-700) /**< Function error offset get reason function.*/
#define  SPL_GETL_ERROR                 (-800) /**< Function error offset get live list function.*/
#define  SPL_CLOCK_FN_ERROR          (-900) /**< Function error offset clock functions 1.*/
#define  SPL_ERROR_STATE            (-1000) /**< Error States.*/
#define  SPL_CREATE_ERROR           (-1200) /**< Function error offset create functions.*/
#define  SPL_CRC_ERROR               (-1300) /**< Function error offset crc functions.*/ 
#define  SPL_CLOCK_FUNCTION_ERROR   (-1400) /**< Function error offset clock functions 2.*/
#define  SPL_MANAGE_ERROR           (-1500) /**< Function error offset management functions.*/
#define  SPL_INTERNAL_ERROR        (-10000) /**< Internal error not mapable to a special function.*/
/*@}*/


/** @defgroup BASIC_ERROR_CODES  Basic Error Codes 
 */
/*@{*/
#define  SPL_ERROR_BAD_PB_ADDRESS         (-1) /**< Detected a invalid PROFIBUS address. */
#define  SPL_ERROR_BAD_DRIVER             (-2) /**< Detected a invalid PROFIBUS driver vaule. */
#define  SPL_ERROR_BAD_PARAMETER          (-3) /**< Detected a invalid pointer or wrong value in a parameter. */
#define  SPL_ERROR_DRIVER_ERROR           (-4) /**< Driver could not be initialized. */
#define  SPL_ERROR_FDL_ERROR              (-4) /**< Second name for SPL_ERROR_DRIVER_ERROR. */
#define  SPL_ERROR_BAD_OBJECT             (-5) /**< Detected a invalid connection object or another error. */
#define  SPL_ERROR_BAD_STATE              (-6) /**< Detected a connection state in which the command could not be processed. */
#define  SPL_ERROR_FINAL_DISC             (-7) /**< Detected a connection state final disconnection. */
#define  SPL_ERROR_BAD_TYPE               (-8) /**< Detected a invalid connection type (MCast/P2P). */
#define  SPL_ERROR_ALREADY_DISC           (-9) /**< Detected a disconnect request on an already disconnected connection. */
#define  SPL_ERROR_FDL_FIFO              (-10) /**< Detected a FIFO error in PL driver. */
#define  SPL_ERROR_NOT_SUPPORTED         (-14) /**<  Called a not supported function. */
#define  SPL_ERROR_CLOCK_ERROR           (-15) /**<  Error in reference clock. */

#define  SPL_ERROR_IS_CLOSED             (-16) /**<  State connection is closed. */
#define  SPL_ERROR_IS_CLOSED_2           (-17) /**<  State connection is closed final. */
#define  SPL_ERROR_IS_SAP_ERROR          (-18) /**<  State SAP error. */
#define  SPL_ERROR_IS_DRIVER_ERROR       (-19) /**<  State driver error. */

#define  SPL_ERROR_BAD_SIZE              (-23) /**<  Detected a invalid buffer/telegram size. */
#define  SPL_ERROR_CRC_ERROR             (-24) /**<  Detected a invalid CRC in buffer/telegram. */

#define  SPL_ERROR_CALL_SEQUENCE         (-27) /**<  Call init twice or try to init after shutdown... */
#define  SPL_ERROR_NOT_READY             (-28) /**<  Libary not jet ready. */
#define  SPL_ERROR_MCAST_LOST            (-29) /**<  Lost a multicast connection. */
#define  SPL_ERROR_APP_ERROR             (-30) /**<  Application error. */

#define  SPL_ERROR_INVALID_POINTER       SPL_ERROR_BAD_PARAMETER /**< (internal) Detect an invalid pointer (used in paramter check).*/
#define  SPL_ERROR_INVALID_IN_BUFFER     SPL_ERROR_BAD_PARAMETER /**< (internal) Detect an invalid input buffer (used in paramter check).*/
#define  SPL_ERROR_INVALID_OUT_POINTER   SPL_ERROR_BAD_PARAMETER /**< (internal) Detect an invalid output buffer (used in paramter check).*/

/*@}*/

#define SPL_STATE_IS_CONNECTED             1  /**<  State connection connected. */
#define SPL_STATE_IS_CONNECTING            2  /**<  State connection connection. */

#define SPL_WARNING_UNEXPECTED            12  /**<  Unexpected value in PL manager call. */
#define SPL_STATE_NO_DISCONNECT           13  /**<  Connection is not disconnected. */

#define SPL_STATE_IS_SYNCHRONIZED         20  /**<  Clock state: Synchronized. */
#define SPL_STATE_IS_SYNCHRONIZING        21  /**<  Clock state: Synchronizing. */
#define SPL_STATE_IS_SYNCHRO_LOST         22  /**<  Clock state: Synchronization lost. */

#define SPL_INFO_APP_DATA                 25  /**<  ???. */
#define SPL_INFO_CONTROL_DATA             26  /**<  ???. */


/** @defgroup GLOBAL_STATE_AND_ERROR_CODE  Global State and Error Codes
 */ 
/*@{*/
#define  SPL_OK                              0  /**< 0x00000000   : Function returned without error. */
#define  SPL_AB_ERROR                      (-99)  /**< 0xFFFFFF9D   : Comparison error in diversified version. */
/*@}*/


/** @defgroup ERROR_CODE_INIT   Init Return Codes  
    Return values for SPL_Initialyze .           
 */
/*@{*/
#define  SPL_INIT_OK                     SPL_OK                                           /**<     0 :  Initialisation ok .  */
#define  SPL_INIT_BAD_PB_ADDRESS         (SPL_INIT_ERROR     + SPL_ERROR_BAD_PB_ADDRESS)    /**<  -101 : Invalid value for My_Profibus_Address . */
#define  SPL_INIT_BAD_DRIVER             (SPL_INIT_ERROR     + SPL_ERROR_BAD_DRIVER)        /**<  -102 : Invalid value for DriverType . */
#define  SPL_INIT_DRIVER_ERROR           (SPL_INIT_ERROR     + SPL_ERROR_DRIVER_ERROR)      /**<  -104 : Driver/board initialization failed . */
#define  SPL_INIT_BAD_CALL_SEQUENCE      (SPL_INIT_ERROR     + SPL_ERROR_CALL_SEQUENCE)     /**<  -127 : */
#define  SPL_INIT_APP_ERROR              (SPL_INIT_ERROR     + SPL_ERROR_APP_ERROR)         /**<  -130 : Invalid parameters from application for Initialisation. */
/*@}*/

/** @defgroup ERROR_CODE_CONNECT   Connect Return Codes  
 *  Return values for SPL_Connect .           
 */
/*@{*/
#define  SPL_CONNECT_OK                  SPL_OK                                           /**<     0 : Connect ok .  */
#define  SPL_CONNECT_BAD_OBJECT          (SPL_CONNECT_ERROR  + SPL_ERROR_BAD_OBJECT)        /**<  -205 : Connection setup start not possible because of invalid connection object. */
#define  SPL_CONNECT_BAD_STATE           (SPL_CONNECT_ERROR  + SPL_ERROR_BAD_STATE)         /**<  -206 : Connection setup start not possible because of invalid state or type of connection . */
#define  SPL_CONNECT_FINAL_DISC          (SPL_CONNECT_ERROR  + SPL_ERROR_FINAL_DISC)        /**<  -207 : Connection setup start not possible because connection has been finally disconnected  */
#define  SPL_CONNECT_FDL_ERROR           (SPL_CONNECT_ERROR  + SPL_ERROR_FDL_ERROR)         /**<  -204 : Connection setup start not possible because of error at FDL level . */
#define  SPL_CONNECT_APP_ERROR           (SPL_CONNECT_ERROR  + SPL_ERROR_APP_ERROR)         /**<  -230 : Invalid parameters from application for Connect. */
/* @note the following return codes are not specified and therefore mapped to SPL_CONNECT_BAD_OBJECT. */
#define  SPL_CONNECT_BAD_OUT_BUFFER      SPL_CONNECT_BAD_OBJECT                           /**<  -205 : Detect an invalid output buffer (used in paramter check).*/
#define  SPL_CONNECT_FIFO_ERROR          SPL_CONNECT_BAD_OBJECT                           /**<  -205 : Detect an invalid FIFO error (used in paramter check).*/

/*@}*/
/** @defgroup ERROR_CODE_DISCON   Disconnect Return Codes  
 *  Return values for SPL_Disconnect .           
 */
/*@{*/
#define  SPL_DISCON_OK                   SPL_OK                                           /**<     0 : Disconnect ok .  */
#define  SPL_DISCON_BAD_OBJECT           (SPL_DISCON_ERROR   + SPL_ERROR_BAD_OBJECT)       /**<  -305 : Internal error, bad TheConnection parameter . */
#define  SPL_DISCON_BAD_TYPE             (SPL_DISCON_ERROR   + SPL_ERROR_BAD_TYPE)          /**<  -308 : Disconnect not possible because of type of connection . */
#define  SPL_DISCON_ALREADY_DISC         (SPL_DISCON_ERROR   + SPL_ERROR_ALREADY_DISC)      /**<  -309 : Disconnect not possible because connection is already disconnected . */
#define  SPL_DISCON_FDL_ERROR            (SPL_DISCON_ERROR   + SPL_ERROR_FDL_ERROR)         /**<  -304 : Sending of disconnect telegram not possible because of error at FDL level, connection locally disconnected . */
#define  SPL_DISCON_FDL_FIFO             (SPL_DISCON_ERROR   + SPL_ERROR_FDL_FIFO)          /**<  -310 : FDL fifo for sending overflown . */
#define  SPL_DISCON_APP_ERROR            (SPL_DISCON_ERROR   + SPL_ERROR_APP_ERROR)         /**<  -330 : Invalid parameters from application for Disconnect. */
/*@}*/
/** @defgroup ERROR_CODE_SEND   Send Return Codes  
 *  Return values for SPL_send, SPL_sendIdle and SPL_chekIdle .
 */
/*@{*/
#define  SPL_SEND_OK                     SPL_OK                                           /**<      0 : Send ok .  */
#define  SPL_SEND_BAD_OBJECT             (SPL_SEND_ERROR     + SPL_ERROR_BAD_OBJECT)        /**<  -405  : Internal error, invalid connection object in parameter . */
#define  SPL_SEND_BAD_STATE              (SPL_SEND_ERROR     + SPL_ERROR_BAD_STATE)         /**<  -406  : Sending of data/idle telegram not possible because of invalid state or type of connection . */
#define  SPL_SEND_BAD_SIZE               (SPL_SEND_ERROR     + SPL_ERROR_BAD_SIZE)          /**<  -423  : Only for SPL_send: BufferSize is greater than the max data size (= 242 for FDL_MAX_TELEGRAM_LENGTH=256 (in bytes)) . */
#define  SPL_SEND_FDL_ERROR              (SPL_SEND_ERROR     + SPL_ERROR_FDL_ERROR)         /**<  -404  : Other FDL driver error on sending . */
#define  SPL_SEND_APP_ERROR              (SPL_SEND_ERROR     + SPL_ERROR_APP_ERROR)         /**<  -430  : Invalid parameters from application for Send. */
/* @note the following return codes are not specified and therefor mapped to defined values. */
#define  SPL_SEND_BAD_IN_BUFFER          SPL_SEND_BAD_OBJECT                                 /**< -405 : Detect an invalid input buffer (used in paramter check).*/ 
#define  SPL_SEND_BAD_OUT_BUFFER         SPL_SEND_BAD_OBJECT                                 /**< -405 : Detect an invalid output buffer (used in paramter check).*/
#define  SPL_SEND_BAD_TYPE               (SPL_SEND_ERROR        + SPL_ERROR_BAD_STATE)         /**< -406 : invalid connection type. */
#define  SPL_SEND_BAD_PARAMETER          (SPL_SEND_ERROR     + SPL_ERROR_BAD_PARAMETER)      /**< -403 : Internal error, invalid parameter . */
/*@}*/

/** @defgroup ERROR_CODE_BUS   Bus Management Return Codes  
 *  Return values for SPL_Bus_Management .
 */
/*@{*/
#define  SPL_BUS_OK                      SPL_OK                                           /**<      0 : Bus ok .  */
#define  SPL_BUS_FDL_FIFO                (SPL_BUS_ERROR      + SPL_ERROR_FDL_FIFO)          /**<  -511  : FDL FIFO for receiving overflow .*/
#define  SPL_BUS_FDL_ERROR               (SPL_BUS_ERROR      + SPL_ERROR_FDL_ERROR)         /**<  -504  : FDL driver error on fetching telegrams .*/
#define  SPL_BUS_UNEXPECTED              (SPL_BUS_STATE      + SPL_WARNING_UNEXPECTED)      /**<   512  : (Warning) Unexpected telegrams have been received, with no connection to dispatch them . */
/*@}*/
/** @defgroup ERROR_CODE_RECV   Receive Return Codes  
 *  Return values for SPL_recv .  
 */
/*@{*/
#define  SPL_RECV_OK                     SPL_OK                                           /**<     0 : Receive ok .  */
#define  SPL_RECV_MCAST_LOST             (SPL_RECV_ERROR     + SPL_ERROR_MCAST_LOST)        /**<  -603 : Lost a multicast connection. */
#define  SPL_RECV_BAD_OBJECT             (SPL_RECV_ERROR     + SPL_ERROR_BAD_OBJECT)        /**<  -605 : Internal error, invalid connection object in parameter . */
#define  SPL_RECV_BAD_STATE              (SPL_RECV_ERROR     + SPL_ERROR_BAD_STATE)         /**<  -606 : */
#define  SPL_RECV_APP_ERROR              (SPL_RECV_ERROR     + SPL_ERROR_APP_ERROR)         /**<  -630 : Invalid parameters from application for Receive. */
#define  SPL_RECV_APP_DATA               (SPL_RECV_STATE     + SPL_INFO_APP_DATA)           /**<   625 : */
#define  SPL_RECV_CONTROL                (SPL_RECV_STATE     + SPL_INFO_CONTROL_DATA)       /**<   626 : */
/* @note the following return codes are not specified and therefor mapped to defined values. */
#define  SPL_RECV_FDL_ERROR              SPL_RECV_BAD_OBJECT   /* -605 : Detect a FDL error.*/   
#define  SPL_RECV_BAD_OUT_BUFFER         SPL_RECV_BAD_OBJECT   /* -605 : Detect an invalid output buffer (used in paramter check).*/   
#define  SPL_RECV_BAD_PARAMETER          SPL_RECV_BAD_OBJECT   /* -605 : Detect an invalid parameter/pointer (used in paramter check).*/      


/*@}*/
/** @defgroup ERROR_CODE_GETR  Get Reson Return Codes 
 *  Return values for SPL_Get_Reason .
 */
/*@{*/
#define  SPL_GETR_OK                     SPL_OK                                           /**<     0 : Get result ok .  */
#define  SPL_GETR_BAD_OBJECT             (SPL_GETR_ERROR     + SPL_ERROR_BAD_OBJECT)        /**<  -705 : Error, invalid connection object in parameter .*/
#define  SPL_GETR_NO_DISCONNECT          (SPL_GETR_STATE     + SPL_STATE_NO_DISCONNECT)     /**<   713 : No disconnect telegram has been received or sent since last connection setup start or connection setup never started . */
#define  SPL_GETR_INVALID_POINTER        (SPL_GETR_ERROR     + SPL_ERROR_INVALID_POINTER)   /**<  -703 : Invalid pointer - unable to return data via parameter. */
/* @note the following return codes are not specified and therefor mapped to defined values. */
#define  SPL_GETR_INVALID_TEXT_POINTER   SPL_GETR_INVALID_POINTER   /* -705 : Detect an invalid pointer (used in paramter check).*/
#define  SPL_GETR_BAD_TYPE               SPL_GETR_BAD_OBJECT        /* -705 : Detect an invalid connection type (used in paramter check).*/        

/*@}*/
/** @defgroup ERROR_CODE_GETL   Get Life-List Return Codes  
 *  Return values for SPL_Get_Livelist .
 */
/*@{*/
#define  SPL_GETL_OK                     SPL_OK                                           /**<     0 : Get live list ok .  */
#define  SPL_GETL_BAD_PARAMETER          (SPL_GETL_ERROR     + SPL_ERROR_BAD_PARAMETER)     /**<  -803 : FDL level not ready for getting live list . */
#define  SPL_GETL_NOT_SUPPORTED          (SPL_GETL_ERROR     + SPL_ERROR_NOT_SUPPORTED)     /**<  -814 : The driver or FDL level does not support the get live list function . */
#define  SPL_GETL_NOT_READY              (SPL_GETL_ERROR     + SPL_ERROR_NOT_READY)         /**<  -828 : FDL level not ready for getting live list . */
#define  SPL_GETL_APP_ERROR              (SPL_GETL_ERROR     + SPL_ERROR_APP_ERROR)         /**<  -830 : Invalid parameters from application for Get Live List. */
/*@}*/
/** @defgroup ERROR_CODE_REF_CLOCK      Reference Clock Error Codes  
 *  Return values for SPL_Reference_Clock_Master and SPL_Reference_Clock_Slave .
 */
/*@{*/
#define  SPL_CLOCK_OK                    SPL_OK                                           /**<     0 : Reference Clock (Master and Slave) ok .  */
#define  SPL_CLOCK_ERROR                 (SPL_CLOCK_FN_ERROR + SPL_ERROR_CLOCK_ERROR)       /**<  -915 : Detect error during reference clock setup. */
#define  SPL_CLOCK_BAD_PARAMETER         (SPL_CLOCK_FN_ERROR + SPL_ERROR_BAD_PARAMETER)     /**<  -903 : Detect an invalid parameter. */
#define  SPL_CLOCK_BAD_STATE             (SPL_CLOCK_FN_ERROR + SPL_ERROR_BAD_STATE)         /**<  -906 : Reference Clock in invalid state. */
#define  SPL_CLOCK_APP_ERROR             (SPL_CLOCK_FN_ERROR + SPL_ERROR_APP_ERROR)         /**<  -930 : Invalid parameters from application for Create Clock */
/*@}*/
/** @defgroup CONN_STATE_CODES  Connection State Codes 
 *  Return values (States) for SPL_isActive, ... . 
 */
/*@{*/
#define  SPL_IS_BAD_OBJECT               (SPL_ERROR_STATE    + SPL_ERROR_BAD_OBJECT)        /**< -1005  : Internal error, bad TheConnection parameter . */
#define  SPL_IS_CONNECTED                (SPL_STATE          + SPL_STATE_IS_CONNECTED)      /**<  1001  : Connection is open, data sending is permitted .*/
#define  SPL_IS_CONNECTING               (SPL_STATE          + SPL_STATE_IS_CONNECTING)     /**<  1002  : Connection is opening (only P2P), i.e. exchanging connection setup telegrams with the target . */
#define  SPL_IS_CLOSED                   (SPL_ERROR_STATE    + SPL_ERROR_IS_CLOSED)         /**< -1016  : Connection is disconnected non final(only P2P) . */
#define  SPL_IS_CLOSED_2                 (SPL_ERROR_STATE    + SPL_ERROR_IS_CLOSED_2)       /**< -1017  : Connection is final disconnected (only P2P) . */
#define  SPL_IS_SAP_ERROR                (SPL_ERROR_STATE    + SPL_ERROR_IS_SAP_ERROR)      /**< -1018  : Connection not ready because of FDL level SAP error. Details can be fetched by SPL_GET_ConnectionStatus . */
#define  SPL_IS_DRIVER_ERROR             (SPL_ERROR_STATE    + SPL_ERROR_IS_DRIVER_ERROR)   /**< -1019  : Connection not ready because of FDL level driver error. Details can be fetched by SPL_GET_Status . */
#define  SPL_IS_APP_ERROR                (SPL_ERROR_STATE    + SPL_ERROR_APP_ERROR)         /**< -1030  : Invalid parameters from application for Is Active */
/* @note the following return codes are not specified and therefor mapped to defined values. */
#define  SPL_IS_BAD_OUT_BUFFER           SPL_IS_BAD_OBJECT  /* -1005 : Detect an invalid output buffer (used in paramter check).*/
#define  SPL_IS_BAD_STATE                SPL_IS_BAD_OBJECT  /* -1005 : Detect an invalid state (used in paramter check).*/

/*@}*/

/** @defgroup CLOCK_STATE   Clock State Return Codes  
 *  Return values for SPL_Get_Clock_Status.
 */
/*@{*/
#define  SPL_CLOCK_SYNCHRONIZED          (SPL_CLOCK_STATE    + SPL_STATE_IS_SYNCHRONIZED)   /**<  1120 : lock state: Synchronized. */        
#define  SPL_CLOCK_SYNCHRONIZING         (SPL_CLOCK_STATE    + SPL_STATE_IS_SYNCHRONIZING)  /**<  1121 : lock state: Synchronizing. */       
#define  SPL_CLOCK_SYNCHRO_LOST          (SPL_CLOCK_STATE    + SPL_STATE_IS_SYNCHRO_LOST)   /**<  1122 : lock state: Synchronization lost. */
/*@}*/
/** @defgroup ERROR_CODE_CREATE   Create Return Codes  
 *  Return values for SPL_Create_P2P SPL_Create_MCast.
 */
/*@{*/
#define  SPL_CREATE_OK                   SPL_OK                                           /**<     0 : Create (peer2peer or multicast)  ok .  */
#define  SPL_CREATE_BAD_OBJECT           (SPL_CREATE_ERROR   + SPL_ERROR_BAD_OBJECT)        /**< -1205 : Internal error, invalid connection object in parameter . */
#define  SPL_CREATE_BAD_TYPE             (SPL_CREATE_ERROR   + SPL_ERROR_BAD_TYPE)          /**< -1208 : Detected a invalid connection type (MCast/P2P). */
#define  SPL_CREATE_SAP_ERROR             (SPL_CREATE_ERROR   + SPL_ERROR_IS_SAP_ERROR)      /**< -1213 : SAP already used. */
#define  SPL_CREATE_APP_ERROR            (SPL_CREATE_ERROR   + SPL_ERROR_APP_ERROR)         /**< -1230 : Invalid parameters from application for Create connection */
/*@}*/
/** @defgroup ERROR_CODE_CRC   CRC Return Codes  
 *  Return values for SPL_CheckCrcAB.
 */
/*@{*/
#define  SPL_CRC_OK                      SPL_OK                                           /**<      0 :  CRC ok .  */
#define  SPL_CRC_BAD_OBJECT              (SPL_CRC_ERROR      + SPL_ERROR_BAD_OBJECT)        /**< -1305  : Internal error, invalid connection object in parameter . */
#define  SPL_CRC_BAD_SIZE                (SPL_CRC_ERROR      + SPL_ERROR_BAD_SIZE)          /**< -1323  : Detected a invalid buffer/telegram size. */
#define  SPL_CRC_CRC_ERROR               (SPL_CRC_ERROR      + SPL_ERROR_CRC_ERROR)         /**< -1324  : Detected a invalid CRC in buffer/telegram. */
#define  SPL_CRC_APP_ERROR               (SPL_CRC_ERROR      + SPL_ERROR_CRC_ERROR)        /**<  -1330  : Invalid parameters from application for Check CRC. */
/*@}*/
/** @defgroup ERROR_CODE_CLOCK_MASTER Reference Clock Role
 */
/*@{  */
#define SPL_REFERENCE_CLOCK_MASTER      1      /**< is Clock-Master. */
#define SPL_REFERENCE_CLOCK_SLAVE       0      /**< is Clock-Slave. */
#define SPL_REFERENCE_CLOCK_UNDEFINED  (-1)    /**< is not initialised. */
/*@}*/                                                                                                                                                                   

 
/** @defgroup ERROR_CODE_MANAGE   Management Return Codes  
 */
/*@{  */ 
#define SPL_MANAGE_BAD_OBJECT             (SPL_MANAGE_ERROR + SPL_ERROR_BAD_OBJECT)           /* -1505 : Internal error, invalid connection object in parameter . */
#define SPL_MANAGE_INVALID_IN_BUFFER      (SPL_MANAGE_ERROR + SPL_ERROR_INVALID_IN_BUFFER)    /* -1505 : Internal error, invalid input buffer in parameter . */
#define SPL_MANAGE_INVALID_OUT_BUFFER     (SPL_MANAGE_ERROR + SPL_ERROR_INVALID_OUT_BUFFER)   /* -1505 : Internal error, invalid output buffer in parameter . */
/*@}*/

/** @defgroup RETURN_CODE_CONNECTION   IsP2P Return Codes  
 */
/*@{  */ 
#define SPL_CONNECTION_P2P       (+1)    /**< +1 : is P2P connection. */
#define SPL_CONNECTION_MCAST      0      /**<  0 : is MCast connection. */
#define SPL_CONNECTION_ERROR     (-1)    /**< -1 : is undefined or error. */
/*@}*/

      


/**
 * @name
 * <b>Masks for SPL_Get_Status and SPL_Get_ConnectionStatus.</b>
 ***************************************************************************/
 /*@{ */
/**
 * Mask for FDL level errors. */
#define SPL_GETS_MASK_FDL 0x000000FF
/**
 * Mask for PL_Manager errors. */
#define SPL_GETS_MASK_PL  0x00000F00
/**
 * Mask for SLL layer errors. */
#define SPL_GETS_MASK_SLL 0x0003F000
/**
 * Mask for STL layer errors. */
#define SPL_GETS_MASK_STL 0x00FC0000
/**
 * Mask for API layer errors. */
#define SPL_GETS_MASK_API 0xFF000000
 /*@} */
 


/**
 * @name
 * <b>Error bits for SPL_Get_Status and SPL_Get_ConnectionStatus.</b>
 ***************************************************************************/
/*@{*/
/**
 * Board not initialized or initialization failed.*/
#define SPL_GETS_GLB_FDL_INIT 0x00000001
/**
 * FDL level has been shut down.*/
#define SPL_GETS_GLB_FDL_SHTD 0x00000002
/**
 * No tokens are exchanged (OUT_OF_RING).*/
#define SPL_GETS_GLB_FDL_OOR  0x00000004
/**
 * Another station has the same physical address.*/
#define SPL_GETS_GLB_FDL_ADDR 0x00000008

/**
 * SAP enable was not called or failed.*/
#define SPL_GETS_CON_FDL_INIT 0x00000001
/**
 * At least one negative ACK was received.*/
#define SPL_GETS_CON_FDL_NAK  0x00000002
/**
 * Overflow of incoming fifos/queues.*/
#define SPL_GETS_CON_FDL_ROFL 0x00000004
/**
 * Overflow of outgoing fifos/queues.*/
#define SPL_GETS_CON_FDL_SOFL 0x00000008
/**
 * SAP number is out of range.*/
#define SPL_GETS_CON_FDL_OUTR 0x00000010

/**
 * FDL level fatal error.*/
#define SPL_GETS_FDL_FATAL 0x00000080

/**
 * PL manager receive FIFO overflow.*/
#define SPL_GETS_CON_PLM_ROFL 0x00000100
/**
 * PL manager internal error.*/
#define SPL_GETS_CON_PLM_INTL 0x00000800

/**
 * Disconnect by local SLL layer.*/
#define SPL_GETS_CON_SLL_LDSC 0x00001000
/**
 * Disconnect by remote SLL layer (disconnect reason received 00h-1Fh). */
#define SPL_GETS_CON_SLL_RDSC 0x00002000
/**
 *  Invalid command received. */
#define SPL_GETS_CON_SLL_INVD 0x00004000
/**
 * SLL internal error. */
#define SPL_GETS_CON_SLL_INTL 0x00020000

/**
 * Clock synchronisation lost. */
#define SPL_GETS_GLB_STL_SYNC 0x00040000

/**
 *  Disconnect by local STL layer. */
#define SPL_GETS_CON_STL_LDSC 0x00040000
/**
 * Disconnect by remote STL layer (disconnect reason received 20h - 40h).*/
#define SPL_GETS_CON_STL_RDSC 0x00080000
/**
 * Invalid command requested. */
#define SPL_GETS_CON_STL_INVD 0x00100000
/**
 * STL internal error. */
#define SPL_GETS_CON_STL_INTL 0x00800000

/**
 *  SPL_Initialize has not been called or failed.*/
#define SPL_GETS_GLB_API_INIT 0x01000000
/**
 * SPL_Shutdown has been called.*/
#define SPL_GETS_GLB_API_SHTD 0x02000000

/**
 * Sequence number overflow.*/
#define SPL_GETS_CON_API_SEQN 0x01000000
/**
 * Application message length error. */
#define SPL_GETS_CON_API_MLEN 0x02000000
/**
 * Application request invalid for safety level. */
#define SPL_GETS_CON_API_SL   0x04000000
/**
 * Application request invalid for state of connection.*/
#define SPL_GETS_CON_API_STS  0x08000000
/**
 * Application request invalid for connection type. */
#define SPL_GETS_CON_API_TYPE 0x10000000
/**
 * Application request invalid for direction of multicast or P2P.*/
#define SPL_GETS_CON_API_DIR  0x20000000
/**
 *  Disconnect by application.*/
#define SPL_GETS_CON_API_DSC  0x40000000
/**
 * Safety layer internal error.*/
#define SPL_GETS_CON_API_INTL 0x80000000UL
/*@}*/


#endif /*SPLA_ERRORCODES_H_*/
