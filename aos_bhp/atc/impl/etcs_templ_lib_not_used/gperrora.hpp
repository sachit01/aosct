/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation, 2001
*
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without express authority is strictly forbidden.
*
* Module name: 
*
* %name: GPErrorA.hpp %
* %version: 1 %
* %created_by:  htoomasi %
* %date_created: 2014-09-23 13:15 %
* %Creation date of original object: 21-okt-2004 15:36:45 %
*
* Description: Declaration of the class GPErrorA
*
*******************************************************************************/
/*******************************************************************************
* Revision History
*
* Version   Date    Sign    Change description
*					Bo H	Experiment
*
*******************************************************************************/
#if !defined GPERRORA_HPP
#define GPERRORA_HPP

/**
 * This class i provided as a general error handling class to use with GP
 * containers. Special error handlers of the kind that GP requires aren't needed
 * anymore since error handler is now semaphore protected so this is for backwards
 * compatibility until GP is modified.
 * 
 */
class GPErrorA
{

public:
	static void report(const char message[]){;};
private:
};



#endif
