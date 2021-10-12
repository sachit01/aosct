#ifndef Odometry_hpp
#define Odometry_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  The class Odometry is the project specific implementation of the
*  AbstractOdometry class.
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-06-01    arastogi    Created
* 2016-09-19    akushwah    Corrected Init function
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "abstract_odometry.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATP
{
  namespace Pos
  {
    /**
    * The class Odometry instantiates the abstract class AbstractOdometry
    * and implements the interfaces needed for both inherited classes and
    * component.
    *
    */
    class Odometry : public AbstractOdometry
    {
    public:
      /** 
      * Singleton instance.
      * Only one instance of this class is allowed.
      * @return the one and only instance.
      *
      * NOTE: Singleton handling shall only be used in Adaptation, not Core!
      */
      static Odometry& instance(void);

    protected:

    private:
      /** 
      * Singleton instance.
      * Declare constructor as private in order to prevent illegal use.
      */
      Odometry();

      /** 
      * Declare copy-constructor as private in order to prevent illegal use.
      */
      Odometry(const Odometry&);

      /** 
      * Declare assignment-operator as private in order to prevent illegal use.
      */
      Odometry& operator = (const Odometry&);

    };
  }
}
#endif
