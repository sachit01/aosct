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
 * %name: bcd_parameter.hpp %
 * %version: 1.1.2 %
 * %created_by: rkongari %
 * %date_created: 2019-09-19 07:11 %
 *
 * Description: Header file for BcdParameter
 *
 ***************************************************************************/
/**************************************************************************
 * Revision History
 *
 * Version       Date      Sign      Change Description
 * 1.1.2         20190916  rkongari  Updated Review comments
 * 1.1.1         20190916  rkongari  arn_043#6330 : Lint corrections
 * 1             20170627  ddigeraa  First version created
 *
 ***************************************************************************/
#ifndef BCD_PARAMETER_HPP_
#define BCD_PARAMETER_HPP_

/**************************************************************************
 * Includes
 ***************************************************************************/
#include "generic_parameter.hpp"

#include <stdint.h>
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

/*! \brief BcdParameter stores a BCD representation
 *
 * \warning This class behaves badly if input data does not conform to standard
 *
 */
namespace NVSHFR
{
class BcdParameter: public GenericParameter
{
public:
  BcdParameter();
  BcdParameter(std::string param_name, const uint32_t val);
  virtual ~BcdParameter();
  virtual GenericParameter * clone() const;

  /* Porcelain output */
  virtual bool printCfg(std::ostream &out);
  virtual void writeCondensedValueToStream(std::ostream &out);

  /* Porcelain input */
  virtual bool readValueFromBinStream(std::istream &s);
  virtual bool verifyValueFromBinStream(std::istream &s);
  private:

  std::vector<uint8_t> decvalue;
  uint32_t digits;

};

} // namespace NVSHFR
#endif /* BCD_PARAMETER_HPP_ */
