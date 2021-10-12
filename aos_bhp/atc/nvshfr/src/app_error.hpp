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
 * %name: app_error.hpp %
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
 * 1.1.2         20190919  rkongari  updated review comments
 * 1.1.1         20190916  rkongari  arn_043#6330 : Lint corrections
 * 1             YYMMDD    ddigeraa  First version created
 * TODO: <Update the date above, check alignment and then remove this line>
 *
 ***************************************************************************/
#ifndef APP_ERROR_HPP_
#define APP_ERROR_HPP_

/**************************************************************************
 * Includes
 ***************************************************************************/
#include <string>
#include <stdexcept>

/**************************************************************************
 * Macro definitions
 ***************************************************************************/

/**************************************************************************
 * Typedefs
 ***************************************************************************/

/**************************************************************************
 * Data declarations/definitions
 ***************************************************************************/
namespace NVSHFR
{
class AppError: public std::logic_error
{
public:
  enum ERROR_TYPES
  {
    GENERIC, INVALID_FILE_VERSION, INVALID_NVSH_VERSION, CRC64_ERROR
  };

  AppError();

  AppError(std::string reason, ERROR_TYPES error_type);
  ERROR_TYPES getErrorType() const
  {
    return type;
  }
  ;
  virtual ~AppError() throw ();

private:
  ERROR_TYPES type;
};

} // namespace NVSHFR
#endif /* APP_ERROR_HPP_ */
