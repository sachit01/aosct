#ifndef MyAOSComponent_hpp
#define MyAOSComponent_hpp
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
* 2015-11-13    bhermans    Created
* 2016-03-08    lantback    Updated
* 2016-03-18    lantback    Minor corrections
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "ma_abstract_aos_component.hpp"

/******************************************************************************
* DECLARATIONS    
******************************************************************************/

namespace ATP
{
  namespace AOSNamespaceGroup
  {
    /**
    * The class MyAOSComponent instantiates the abstract class and implements 
    * the interfaces needed for both inherited classes and component.
    *
    */
    class MyAOSComponent : public MyAbstractAOSComponent
    {
    public:
      /**
      * Implements the init function.
      *
      * @return Returns true when initialization completed
      */
      virtual bool init(void);

      /**
      * Implements the run function.
      */
      virtual void run(void);

      /** 
      * Singleton instance.
      * Only one instance of this class is allowed.
      * @return the one and only instance.
      *
      * NOTE: Singleton handling shall only be used in Adaptation, not Core!
      */
      static MyAOSComponent& instance(void);

    protected:

    private:
      /** 
      * Singleton instance.
      * Declare constructor as private in order to prevent illegal use.
      */
      MyAOSComponent();

      /** 
      * Declare copy-constructor as private in order to prevent illegal use.
      */
      MyAOSComponent(const MyAOSComponent&);

      /** 
      * Declare assignment-operator as private in order to prevent illegal use.
      */
      MyAOSComponent& operator = (const MyAOSComponent&);
    };
  }
}
#endif
