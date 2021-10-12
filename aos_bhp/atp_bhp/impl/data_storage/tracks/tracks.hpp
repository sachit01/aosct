#ifndef Tracks_hpp
#define Tracks_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Tracks class defines the adaptation of Abstract Tracks.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-05-16    arastogi    Created
* 2016-09-19    akushwah    Corrected Init function
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_tracks.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace DS
  {
    /**
    * The class MyAOSComponent instantiates the abstract class and implements
    * the interfaces needed for both inherited classes and component.
    *
    */
    class Tracks : public AbstractTracks
    {
    public:
      /**
      * Singleton instance.
      * Only one instance of this class is allowed.
      * @return the one and only instance.
      */
      static Tracks& instance(void);

    protected:

    private:
      /**
      * Singleton instance.
      * Declare constructor as private in order to prevent illegal use.
      */
      Tracks();

      /**
      * Declare copy-constructor as private in order to prevent illegal use.
      */
      Tracks(const Tracks&);

      /**
      * Declare assignment-operator as private in order to prevent illegal use.
      */
      Tracks& operator = (const Tracks&);
    };
  }
}
#endif
