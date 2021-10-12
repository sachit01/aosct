#ifndef Position_hpp
#define Position_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file defines the adaptation class for position handling.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-03-07    lantback    Created
* 2016-04-19    lantback    Use ATC::ProcComponent, init to return bool
* 2016-09-19    akushwah    Corrected Init function
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_position.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/
namespace ATP
{
  namespace Pos
  {
    /**
    * The class Position implements the adaptation logic for position calculations
    * used by the AOS.
    *
    */
    class Position : public AbstractPosition
    {
    public:

      /** Singleton instance.
      *
      *  Only one instance of this class is allowed.
      *  @return the one and only instance.
      *
      */
      static Position& instance(void);

    protected:

    private:
      /** Singleton instance.
      *
      *  Declare constructor as private in order to prevent illegal use.
      * 
      */
      Position();

      /** Declare copy-constructor as private in order to prevent illegal use.
      * 
      */
      Position(const Position&);

      /** Declare assignment-operator as private in order to prevent illegal use.
      * 
      */
      Position& operator = (const Position&);

      /**
      * The distance from where the balise can be detected
      *
      */
      static const uint16_t baliseAirGap = 150U;

      /**
      * gets the BaliseAirGap value 
      * @return value of the BaliseAirGap
      */
      virtual uint16_t getBaliseAirGap() const;
    };
  }
}
#endif
