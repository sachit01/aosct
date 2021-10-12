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
 * %name: hex_parameter.hpp %
 * %version: 3 %
 * %created_by: rkongari %
 * %date_created: 2019-09-19 08:22 %
 *
 * Description: Source file for HexParameter
 *
 ***************************************************************************/
/**************************************************************************
 * Revision History
 *
 * Version       Date      Sign      Change Description
 * 3             20190919  rkongari  Updated Review comments
 * 2             20190916  rkongari  arn_043#6330 : Lint corrections
 * 1             20170627  ddigeraa  First version created
 *
 ***************************************************************************/
#ifndef HEX_PARAMETER_HPP_
#define HEX_PARAMETER_HPP_

/**************************************************************************
 * Includes
 ***************************************************************************/
#include "generic_parameter.hpp"

#include <string>
#include <vector>

/**************************************************************************
 * Macro definitions
 ***************************************************************************/

/**************************************************************************
 * Typedefs
 ***************************************************************************/

/**************************************************************************
 * Data declarations/definitions
 ***************************************************************************/

/*! \brief HexParameter stores variable length hex values
 *
 */
namespace NVSHFR
{
class HexParameter: public GenericParameter
{
public:
  HexParameter();
  HexParameter(std::string hp_name, const uint32_t hp_length);
  virtual ~HexParameter();
  virtual GenericParameter * clone() const;

  /* Porcelain output */
  virtual bool printCfg(std::ostream &out);
  virtual void writeCondensedValueToStream(std::ostream &out);

  /* Porcelain input */
  virtual bool readValueFromBinStream(std::istream &s);
  virtual bool verifyValueFromBinStream(std::istream &s);

private:
  std::vector<int32_t> data;
};
} // namespace NVSHFR

#endif /* HEX_PARAMETER_HPP_ */
