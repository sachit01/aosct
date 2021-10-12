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
 * %name: parameter.hpp %
 * %version: 3 %
 * %created_by: rkongari %
 * %date_created: 2019-09-19 10:45 %
 *
 * Description: Header file for Parameter
 *
 ***************************************************************************/
/**************************************************************************
 * Revision History
 *
 * Version       Date      Sign      Change Description
 * 3             190919    rkongari  Updated Review comments
 * 2             190916    rkongari  arn_043#6330 : Lint corrections
 * 1             170612    ddigeraa  First version created
 *
 ***************************************************************************/
#ifndef PARAMETER_HPP_
#define PARAMETER_HPP_

/**************************************************************************
 * Includes
 ***************************************************************************/
#include <stdint.h>
#include <string>
#include <istream>
#include <ostream>

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

using namespace std;

/*! \brief Parameter stores, and translates from binary to text
 *
 * Parameter is the representation of a parameter in ETCS core.
 * The parameter holds a name and value as well as index of the value
 * in the binary stream and if the designated value is set and validated.
 *
 */
namespace NVSHFR
{

class Parameter: public GenericParameter
{
public:

  Parameter(const string param_name, const uint32_t param_size);
  Parameter();
  virtual ~Parameter();
  virtual GenericParameter * clone() const;

  /* Porcelain output */
  virtual bool printCfg(std::ostream &out);
  virtual void writeCondensedValueToStream(ostream &out);

  /* Porcelain input */
  virtual bool readValueFromBinStream(istream &s);
  virtual bool verifyValueFromBinStream(istream &s);

  /* Plumbing */
  uint64_t getValue();

private:
  uint64_t value;
};
} // namespace NVSHFR

#endif /* PARAMETER_HPP_ */
