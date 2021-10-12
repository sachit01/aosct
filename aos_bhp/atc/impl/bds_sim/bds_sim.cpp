/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION: 
*  Simulation of BDS in the PC Environment.            
*
*  These below function-stubs are only used in SIL Environment.
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-08-11    akushwah    created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include<dia_message.h>
#include<dia_send.h>


/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/


/***************************************************************************
* BDS Init Functions
*-----------------------------------------------------------------------------
* \brief Init client interface to BDS
* Opens a socket to be used for sending UDP-packets to the server.
*
* \param [in] BDS IP-address
* \param [in] BDS portNumber
* Return value is 0 when Ok, otherwise error-code according to diaSend_e
**
******************************************************************************/
int16_t diaInit(const char* /*server_ip*/, const uint16_t /*portNumber*/)
{
  return DIA_SEND_OK;
}

/******************************************************************************
**Send message with free-format 8-bit buffer, m_msg, to Basic Diagnostics Server
*
* \sa diaSendBuffer
* Return value is 0 when Ok, otherwise error-code according to diaSend_e
*
*******************************************************************************/
int16_t diaSendBuffer(const uint8_t /*n_command*/,const uint16_t /*nid_sender*/,const uint8_t /*n_seq*/,
                      const uint32_t /*utc_reftime_seconds*/,const uint16_t /*utc_reftime_ticks*/,const int8_t /*utc_offset*/,
                      const uint64_t /*t_reftime*/,const uint32_t /*nid_msg*/,const uint8_t /*nc_severity*/,const uint16_t /*m_msg_length*/,
                      const uint8_t* /*m_msg*/)
{
  return DIA_SEND_OK;
}

/******************************************************************************
** \brief Send message with string buffer to Basic Diagnostics Server
*
* \sa diaSendString
* Return value is 0 when Ok, otherwise error-code according to diaSend_e
*
*******************************************************************************/
int16_t diaSendString(const uint8_t /*n_command*/,const uint16_t /*nid_sender*/,const uint8_t /*n_seq*/,
                      const uint32_t /*utc_reftime_seconds*/,const uint16_t /*utc_reftime_ticks*/,
                      const int8_t /*utc_offset*/,const uint64_t /*t_reftime*/,const uint32_t /*nid_msg*/,
                      const uint8_t /*nc_severity*/,uint8_t* /*m_msg*/)
{
  return DIA_SEND_OK;
}

