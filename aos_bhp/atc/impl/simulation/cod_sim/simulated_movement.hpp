#ifndef SimulatedMovement_hpp
#define SimulatedMovement_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION: 
*  This header file contains the structure for the simulated Movement telgram 
*  received from the AOSPC.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-05-03    akushwah    Created
* 2016-05-10    akushwah    Added the Doxygen tag
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/

/******************************************************************************
* DECLARATIONS    
******************************************************************************/
//Simulated Movement telegram
struct SimulatedMovement
{
  uint8_t    stx;                 //!< Start of data header
  uint8_t    ver;                 //!< Protocol Version
  uint16_t   len;                 //!< lenth
  uint8_t    nidMessageType;      //!< 10= SimulatedMovement mesage
  uint16_t   vSim;                //!< Speed (1 cm/s)
  uint16_t   aSim;                //!< Acceleration (0.01 cm/s2)
  uint16_t   nSensorMinError;     //!< Sensor Max Error (0.01 %)
  uint16_t   nSensorMaxError;     //!< Sensor Min Error (0.01 %)
  uint8_t    etx;                 //!< End of trailer
};

#endif
