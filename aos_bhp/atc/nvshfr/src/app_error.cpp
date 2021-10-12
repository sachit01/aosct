/**************************************************************************
 *
 * (C) COPYRIGHT Bombardier Transportation Signal Sweden AB, 2017
 * 
 * We reserve all rights in this file and in the information 
 * contained therein. Reproduction, use or disclosure to third 
 * parties without express authority is strictly forbidden.
 *
 * Component name: TODO: <Fill out name of main class or functionality>
 *
 * %name: app_error.cpp %
 * %version: 1.1.2 %
 * %created_by: rkongari %
 * %date_created: 2019-09-19 06:58 %
 *
 * Description: TODO: <Fill out description of file>
 *
 ***************************************************************************/
/**************************************************************************
 * Revision History
 *
 * Version       Date      Sign      Change Description
 * 1.1.2         20190919  rkongari   updated review comments
 * 1.1.1         20190916  rkongari   arn_043#6330 : Lint corrections
 * 1             YYMMDD    ddigeraa   First version created
 * TODO: <Update the date above, check alignment and then remove this line>
 *
 ***************************************************************************/

/**************************************************************************
 * Includes
 ***************************************************************************/

#include "app_error.hpp"
/**************************************************************************
 * Macro definitions
 ***************************************************************************/

/**************************************************************************
 * Typedefs
 ***************************************************************************/

/**************************************************************************
 * Data declarations/definitions
 ***************************************************************************/
NVSHFR::AppError::AppError()
    :
      std::logic_error(NULL),
      type(AppError::GENERIC)
{

}

NVSHFR::AppError::AppError(std::string reason, ERROR_TYPES const error_type)
    :
      std::logic_error(reason.c_str()),
      type(error_type)
{
  // TODO Auto-generated constructor stub

}
NVSHFR::AppError::~AppError() throw ()
{
  // TODO Auto-generated destructor stub
}

