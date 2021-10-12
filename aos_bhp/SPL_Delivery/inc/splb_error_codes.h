/** @file
 *  Definition of the error codes used by SPL B-code.
 *
 */
 /****************************************************************************
 *                    Bombardier Transportation
 *
 * %name:  splb_error_codes.h %
 * %created_by: mczaprag %
 * %version: 1.9.1.4 %
 * %date_created: Fri Mar 23 10:50:05 2007 %
 *****************************************************************************
 * Revision history:
 *
 * Version  Date        Sign     Change description
 * ---------------------------------------------------------------------------
 *    1.0   2005-11-15  sigbor   Emtpy file added to configuration
 *                               control.
 *    1.1   2005-11-16  sighaos  First version with file contents.
 *    1.2   2005-11-25  sigbor   Changes related to task 668.
 *    1.3   2005-12-01  sigbor   Changes related to task 694.
 *    1.4   2005-12-12  sighaos  Updated according to v6 of the B-code
 *                               design specification.
 *    1.5   2005-12-13  sighaos  Changes related to task 787.
 *    1.6-
 *    1.7   2006-01-12  smeindl  Adapt error codes to A values
 *    1.8   2006-02-06  sighaos  Changes related to CR 153, task 1150.
 *    1.9   2006-02-08  sigbor   Changes according to CR 170.
 *  1.9.1.1 2006-02-16  smeindl  Revised configuration
 *  1.9.1.2 2006-05-08  culbrich Changes according to CR 289, task 1751.
 *
 *****************************************************************************/

#ifndef SPLB_ERROR_CODES_H
#define SPLB_ERROR_CODES_H

#define SPLB_OK                     ( -1) /**< Function returned without error */

#define SPLB_AB_ERROR               (98) /**< A and B code comparison error in diversified version */

#define SPLB_INIT_OK                (SPLB_OK) /**< Function returned without error */
#define SPLB_INIT_BAD_PB_ADDRESS    (100) /**< Invalid value for My_Profibus_Address */
#define SPLB_INIT_BAD_CALL_SEQUENCE (126) /**< Initialisation or shutdown already occurred */

#define SPLB_CONNECT_BAD_OBJECT     (204) /**< Connection setup start not possible because of invalid value for parameter TheConnection */
#define SPLB_CONNECT_BAD_STATE      (205) /**< Connection setup start not possible because of invalid state or type of connection */
#define SPLB_CONNECT_FINAL_DISC     (206) /**< Connection setup start not possible because connection has been finally disconnected */

#define SPLB_DISCON_BAD_OBJECT      (304) /**< Internal error, bad TheConnection parameter */
#define SPLB_DISCON_BAD_TYPE        (307) /**< Disconnect not possible because of type of connection */
#define SPLB_DISCON_ALREADY_DISC    (308) /**< Disconnect not possible because connection is already disconnected */

#define SPLB_SEND_BAD_OBJECT        (404) /**< Internal error, bad TheConnection parameter */
#define SPLB_SEND_BAD_STATE         (405) /**< Connection is in a state, where no idle telegram can be sent. */
#define SPLB_SEND_FDL_FIFO          (409) /**< FDL FIFO for sending overflow */
#define SPLB_SEND_BAD_SIZE          (422) /**< Buffersize > maximum data size (= 242 for FDL_MAX_TELEGRAM_LENGTH=256 (in bytes)) */

#define SPLB_RECV_MCAST_LOST        (602) /**< Second multicast error occurred */
#define SPLB_RECV_BAD_OBJECT        (604) /**< Internal error, bad TheConnection parameter */
#define SPLB_RECV_BAD_STATE         (605) /**< Connection is in a state, where no telegram can be received. */

#define SPLB_GETR_BAD_OBJECT        (704) /**< Internal error, bad TheConnection parameter */
#define SPLB_GETR_NO_DISCONNECT     (-714) /**< No disconnect telegram has been received or sent since last connection setup start or connection setup never started */

#define SPLB_CLOCK_OK               (SPLB_OK) /**< Function returned without error */
#define SPLB_CLOCK_BAD_STATE        (905)     /**< Clock is synchronising or synchronisation lost, no reference time returned */
#define SPLB_CLOCK_ERROR            (914)     /**< Internal error in clock management */
#define SPLB_CLOCK_SYNCHRONIZED     (-1121)   /**< Clock is synchronised, connection setup can be started */
#define SPLB_CLOCK_SYNCHRONIZING    (-1122)   /**< Clock is synchronising */
#define SPLB_CLOCK_SYNCHRO_LOST     (-1123)   /**< Clock synchronisation lost, system must be restarted */
#define SPLB_CLOCK_BAD_PARAMETER    (902)     /**< A pointer passed to a function is set to SPLB_ZERO >*/

#define SPLB_CREATE_BAD_OBJECT      (1204) /**< Invalid input parameter(s), e.g. connection handle is 0U */

#define SPLB_CRC_BAD_OBJECT         (1304) /**< Object error, bad connection handle */
#define SPLB_CRC_BAD_SIZE           (1322) /**< Buffer too short to store CRC */
#define SPLB_CRC_CRC_ERROR          (1323) /**< CRC not correct */


#define SPLB_IS_BAD_OBJECT          (1004) /**< Internal error, bad TheConnection parameter */
#define SPLB_IS_CONNECTED           (-1002) /**< Connection is open, data sending is permitted */
#define SPLB_IS_CONNECTING          (-1003) /**< Connection is opening, i.e. exchanging connection setup telegrams with the target. 
                                                 The node can receive only the following telegrams: master: Connect Request, Authentication Request, Run
                                                 slave: Connect Confirm, Authentication Acknowledge, Ready to Run */
#define SPLB_IS_CLOSED              (1015) /**< Connection is disconnected */
#define SPLB_IS_CLOSED_2            (1016) /**< Connection is final disconnected */



#define SPLB_IS_CLOCK_FUNCTION_UNDEFINED    (~(-1)) /**< Clock function undefined on this node */
#define SPLB_IS_CLOCK_MASTER                (~1)    /**< This node is a clock master */
#define SPLB_IS_CLOCK_SLAVE                 (~0)    /**< This node is a clock slave */

#define SPLB_IS_P2P_ERROR                   (~(-1)) /**< Internal error in function SPLB_IsP2P */
#define SPLB_IS_P2P_TRUE                    (~1)    /**< Connection is a P2P connection */
#define SPLB_IS_P2P_FALSE                   (~0)    /**< Connection is a multicast connection */
     


#endif /* SPLB_ERROR_CODES_H */
