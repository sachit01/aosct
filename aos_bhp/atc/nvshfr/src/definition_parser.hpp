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
 * Description: Definition parser header file
 *
 ***************************************************************************/
/**************************************************************************
 * Revision History
 *
 * Version       Date      Sign      Change Description
 * -             20200210  anlindel  Minor lint correction.
 * 4             20190919  rkongari  Updated Review comments
 * 3             20190916  rkongari  arn_043#6330 : Lint corrections
 * 1             20170613  ddigeraa  First version created
 ***************************************************************************/
#ifndef DEFINITION_PARSER_HPP_
#define DEFINITION_PARSER_HPP_

/**************************************************************************
 * Includes
 ***************************************************************************/
#include <stdint.h>
#include <istream>
#include <string>
#include <map>
#include <list>

#include "abstract_data.hpp"
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

/*! \brief Parse definition files
 *
 * Parse definition files and return ParameterList object
 *
 */

namespace NVSHFR
{

static const uint32_t MAX_VERSION_LEN = 8U;

class DefinitionParser
{
public:
  enum list_types
  {
    NORMAL, ITERATIVE
  };

  struct entry
  {
    std::string name;
    std::string value;
  };

  struct section
  {
    int32_t minor_version;
    int32_t major_version;
    std::string nbr_of_Keys_Name;
    list_types type;
    std::list<struct entry> entries;
  };

  DefinitionParser();
  virtual ~DefinitionParser();

  /* Chaining functions */
  DefinitionParser & readDefinition(std::istream &s,
                                    std::string default_name = "DEFAULT");

  /* Output */
  AbstractData *build(std::string section_name = "DEFAULT");
  static bool parseVersion(int32_t & major, int32_t & minor,
                           std::string version_str);
  static uint32_t parseInteger(std::string str);
  static GenericParameter * newParameterFromEntry(
                                                  const struct entry & data_entry);

private:
  std::map<std::string, struct section> definitions;
};

typedef std::list<struct NVSHFR::DefinitionParser::entry>::iterator entry_iter;

} // namespace NVSHFR
#endif /* DEFINITION_PARSER_HPP_ */
