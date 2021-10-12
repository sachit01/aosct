#ifndef MyAbstractAOSComponent_hpp
#define MyAbstractAOSComponent_hpp
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
* 2016-03-18    lantback    Correction of inheritance
* 2016-04-21    lantback    Make abstract constructor protected
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_base.hpp"
#include "atp_types.hpp"

/******************************************************************************
* DECLARATIONS    
******************************************************************************/
namespace ATP
{
  namespace AOSNamespaceGroup
  {
    /**
    * The class MyAbstractAOSComponent implements the interface defined by the ComponentBase class.
    *
    */
    class MyAbstractAOSComponent : public ProcComponent
    {
    public:
      /**
      * Implements the virtual init function.
      * (If needed)
      *
      * @return Returns true when initialization completed
      */
      virtual bool init(void);

      /**
      * Implements the virtual run function.
      * (If needed)
      */
      virtual void run(void);

    protected:
      /**
      * Constructor
      */
      MyAbstractAOSComponent();


    private:

    };
  }
}

#endif
