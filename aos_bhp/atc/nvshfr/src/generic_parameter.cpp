/**************************************************************************
 *
 * (C) COPYRIGHT Bombardier Transportation Signal Sweden AB, 2017
 * 
 * We reserve all rights in this file and in the information 
 * contained therein. Reproduction, use or disclosure to third 
 * parties without express authority is strictly forbidden.
 *
 * Component name: NVSHFR
 *
 * %name: generic_parameter.cpp %
 * %version: 1.1.2 %
 * %created_by: rkongari %
 * %date_created: 2019-09-19 07:57 %
 *
 * Description: Source file for GenericParameter
 *
 ***************************************************************************/
/**************************************************************************
 * Revision History
 *
 * Version       Date      Sign      Change Description
 * 1.1.2         20190919  rkongari  Updated Review comments
 * 1.1.1         20190916  rkongari  arn_043#6330 : Lint corrections
 * 1             20170627  ddigeraa  First version created
 *
 ***************************************************************************/

/**************************************************************************
 * Includes
 ***************************************************************************/
#include "generic_parameter.hpp"

/**************************************************************************
 * Macro definitions
 ***************************************************************************/

/**************************************************************************
 * Typedefs
 ***************************************************************************/

/**************************************************************************
 * Data declarations/definitions
 ***************************************************************************/

NVSHFR::GenericParameter::GenericParameter()
    :
      size(0U),
      has_value(false), is_validated(false)
{

}

NVSHFR::GenericParameter::GenericParameter(std::string inName,
                                           uint32_t const inSize)
    :
      name(inName),
      size(inSize), has_value(false), is_validated(false)
{

}

NVSHFR::GenericParameter::~GenericParameter()
{
}

