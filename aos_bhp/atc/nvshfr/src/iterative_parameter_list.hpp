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
 * Description: Header file for IterativeData
 *
 ***************************************************************************/
/**************************************************************************
 * Revision History
 *
 * Version       Date      Sign      Change Description
 * -             20200210  anlindel  Minor lint correction
 * 4             20190919  rkongari  Updated Review comments
 * 3             20190916  rkongari  arn_043#6330 : Lint corrections
 * 1             20170616  ddigeraa  First version created
 *
 ***************************************************************************/
#ifndef ITERATIVE_PARAMETER_LIST_HPP_
#define ITERATIVE_PARAMETER_LIST_HPP_

/**************************************************************************
 * Includes
 ***************************************************************************/
#include "abstract_data.hpp"
#include "generic_parameter.hpp"

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
namespace NVSHFR
{
class IterativeParameterList: public AbstractData
{
public:
  static const uint32_t CRC_SIZE = 2U;

  IterativeParameterList();
  virtual ~IterativeParameterList();

  void add(GenericParameter* const parameter);
  void setVersion(const int32_t in_major_version, const int32_t in_minor_version);
  void setSectionNameAndNbrOfKeysName(std::string s_name,
                                      std::string nbr_of_key_name);

  virtual bool readValuesFromBinStream(std::istream &s, uint32_t const size);
  virtual bool verifyValuesFromBinStream(std::istream &s,
                                         uint32_t const size);

  virtual bool printCfg(std::ostream &out);

  virtual uint32_t getCrc(void);
  virtual uint32_t getSize(void);

private:
  std::vector<GenericParameter*> template_parameters;
  uint32_t template_size;
  uint32_t list_iter;
  int32_t major_version;
  int32_t minor_version;
  std::string nbr_keys_name;
  uint16_t nbr_keys_val;
  std::string section_name;
  std::vector<GenericParameter*> parameters;
};
} // namespace NVSHFR

#endif /* ITERATIVE_PARAMETER_LIST_HPP_ */
