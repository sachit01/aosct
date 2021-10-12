#ifndef LCSMessageInECPBTrainComposition_hpp
#define LCSMessageInECPBTrainComposition_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
* DESCRIPTION: 
*  The parsers for incoming messages are inherited from AbstractLCSMessageIn.
*  One parser per message-type.
*  Each parser is responsible for the validation and publishing of the incoming data.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-05-04    marlundg    Created
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_types.hpp"
#include "abstract_lcs_message_in.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace TG
  {
    
    /**
    * LCSMessageInECPBTrainComposition is a parser for the incoming ECPBTrainComposition message
    */
    class LCSMessageInECPBTrainComposition : public AbstractLCSMessageIn
    {
    public:
      /**
      * Constructor for LCSMessageInECPBTrainComposition which is a parser for the incoming ECPBTrainComposition message
      */
      LCSMessageInECPBTrainComposition();

      /**
      * Validates the extracted data
      *
      * @param[in] mData   The incoming message data to be parsed
      *
      * @return true if data is valid as a result of a valid incoming message
      */
      virtual bool validate(EmpMsg* const mData);

      /**
      * Invalidates the extracted data (shall be called once per ATP execution-cycle)
      */
      virtual void invalidate();

      /**
      * Access-function for any published ECPBTrainComposition 
      *
      *  @param[out] trainComposition The ECPBTrainComposition
      *    
      *  @return true if any ECPBTrainComposition is published
      */
      bool getECPBTrainComposition(ECPBTrainCompositionType & trainComposition) const;

    private:
      
      /**
      * Validates the mode
      *
      * @return true if data is valid with respect to mode
      */
      bool validateMode() const;

      /**
      * Publishes all the extracted data
      *
      * @return true if publish is successful
      */
      bool publishData() const;

      /**
      * Parses the extracted data
      *
      * @param[in] messageData   The incoming message data to be parsed     
      * @return true if data is valid with respect to parsing
      */
      bool parseMessageData(EmpMsg* const messageData);
      
      /**
      * The storage of ECPBTrainComposition which is a result of a successful parse of the incoming ECPBTrainComposition message.
      * The ECPBTrainComposition information may be accessed with the access-function during one ATP execution cycle 
      * until invalidated by a call to invalidate().
      */
      ECPBTrainCompositionType ECPBTrainComposition;
    };
  }
}
#endif
