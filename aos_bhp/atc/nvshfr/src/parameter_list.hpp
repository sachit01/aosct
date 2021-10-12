/**************************************************************************
 *
 * (C) COPYRIGHT Bombardier Transportation Signal Sweden AB, 2017-2020
 * 
 * We reserve all rights in this file and in the information 
 * contained therein. Reproduction, use or disclosure to third 
 * parties without express authority is strictly forbidden.
 *
 * Component name: NVSHFR
 *
 * Description: Parameter list header file
 *
 ***************************************************************************/
/**************************************************************************
 * Revision History
 *
 * Version       Date      Sign      Change Description
 * -             200210    anlindel  Minor lint corrections
 * 5             190919    rkongari  Updated Review comments
 * 4             190916    rkongari  arn_043#6330 : Lint corrections
 * 3             190509    rkongari  updated for arn_043#3540,arn_043#7171
 * 1             170612    ddigeraa  First version created
 *
 ***************************************************************************/
#ifndef PARAMETER_LIST_HPP_
#define PARAMETER_LIST_HPP_

/**************************************************************************
 * Includes
 ***************************************************************************/
#include <stdint.h>
#include <vector>
#include <istream>

#include "abstract_data.hpp"
#include "generic_parameter.hpp"
#include "parameter.hpp"
/**************************************************************************
 * Macro definitions
 ***************************************************************************/

/**************************************************************************
 * Typedefs
 ***************************************************************************/

/**************************************************************************
 * Data declarations/definitions
 ***************************************************************************/

/*! \Brief Container for a set of Parameters
 *
 * Parameter list keeps a list of parameters and functionality to
 * translate binary config blobs to text representation as well as
 * verify binary blobs against already set parameters.
 *
 */

namespace NVSHFR
{
class ParameterList: public AbstractData
{
public:
  static const uint32_t CRC_SIZE = 2U;

  ParameterList(int32_t const major_ver, int32_t const minor_ver);
  ParameterList();
  virtual ~ParameterList();

  void setVersion(const int32_t in_major_version, const int32_t in_minor_version);
  void setSectionNameAndNbrOfKeysName(std::string sname,
                                      std::string nbr_of_key_name);
  ParameterList &add(GenericParameter * const parameter);

  virtual bool readValuesFromBinStream(std::istream &s, const uint32_t size);
  virtual bool verifyValuesFromBinStream(std::istream &s,
                                         const uint32_t size);

  virtual bool printCfg(std::ostream &out);

  uint32_t countElements(void) const;
  virtual uint32_t getSize(void);
  virtual uint32_t getCrc(void);

private:
  bool validateVersionFromStream(std::istream &s);

  std::vector<GenericParameter*> parameters;
  uint32_t index;
  int32_t major_version;
  int32_t minor_version;
  std::string nbr_keys_name;
  uint16_t nbr_keys_val;
  std::string section_name;
};
} // namespace NVSHFR
#endif /* PARAMETER_LIST_HPP_ */
