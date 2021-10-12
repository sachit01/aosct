#ifndef decode_hpp
#define decode_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Replace this text with a short description of the classes etc defined.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-23    ljoars      Created
* 2016-04-26    lantback    Corrected namespace and init()
* 2016-04-27    lantback    Corrected namespace of Decode
* 2016-06-02    arastogi    Moved it to position folder
* 2016-09-28    saprasad    Clean up the unused code 
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_decode.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace Pos
  {
    /**
    * The class Decode instantiates the abstract class and implements 
    * the interfaces needed for both inherited classes and component.
    *
    */
    class Decode : public AbstractDecode
    {
    public:
      /** 
      * Singleton instance.
      * Only one instance of this class is allowed.
      * @return the one and only instance.
      *
      * NOTE: Singleton handling shall only be used in Adaptation, not Core!
      */
      static Decode& instance();

    protected:

      /**
      * BHP adaptation of the AbstractDecode::isRedBalise function
      */
      virtual bool isRedBalise(const uint16_t nid_c, const uint16_t nid_bg);

    private:

      /**
      * Country ID for red balise
      */
      static const uint16_t redBaliseNidC = 1023U;

      /**
      * Balise group ID for red balise
      */
      static const uint16_t redBaliseNidBg = 16383U;

      /** 
      * Singleton instance.
      * Declare constructor as private in order to prevent illegal use.
      */
      Decode();

      /** 
      * Declare copy-constructor as private in order to prevent illegal use.
      */
      Decode(const Decode&);

      /**
      * Declare assignment-operator as private in order to prevent illegal use.
      */
      Decode& operator = (const Decode&);
    };
  }
}
#endif
