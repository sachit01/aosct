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
 * Description: Definition parser source file
 *
 ***************************************************************************/
/**************************************************************************
 * Revision History
 *
 * Version       Date      Sign      Change Description
 * -             20200210  anlindel  Corrected issue in parsing of version string.
 *                                   Other lint corrections.
 * 6             20190919  rkongari  Updated Review comments
 * 5             20190919  rkongari  Updated Review comments
 * 4             20190916  rkongari  arn_043#6330 : Lint corrections
 * 2             20170629  ddigeraa  Fixed comment bug in definition files
 * 1             20170613  ddigeraa  First version created
 ***************************************************************************/

/**************************************************************************
 * Includes
 ***************************************************************************/
#include <cerrno>
#include <cstring>
#include <sstream>
#include <stdexcept>
#include <limits>
#include <iostream>

#include "definition_parser.hpp"
#include "parameter.hpp"
#include "bcd_parameter.hpp"
#include "hex_parameter.hpp"

#include "parameter_list.hpp"
#include "iterative_parameter_list.hpp"
/**************************************************************************
 * Macro definitions
 ***************************************************************************/

/**************************************************************************
 * Typedefs
 ***************************************************************************/

/**************************************************************************
 * Data declarations/definitions
 ***************************************************************************/

NVSHFR::DefinitionParser::DefinitionParser()
{
}

NVSHFR::DefinitionParser::~DefinitionParser()
{
}

/*! \brief Parse definition from instream and return ParameterList
 *
 * Parses a definition file from instream and returns a parameter list.
 *
 * Layout of the definition:
 * 1st row: [major].[minor]
 * n rows:  param_name size_in_bytes
 *
 * Empty lines and lines beginning with # are ignored
 *
 */

/*! \brief Read out major and minr version from version string
 *
 * reads from [major].[minor] to major and minor output
 *
 * ex: instring is "3.14" results in major = 3, minor = 14
 *
 */
bool NVSHFR::DefinitionParser::parseVersion(int32_t& out_major_version, int32_t& out_minor_version, std::string str)
{
  bool status = false;
  uint32_t work_index = 0U;
  char version_str[str.size()];
  //char *end;
  int32_t major_temp = 0;
  int32_t minor_temp = 0;

  memmove(version_str, str.c_str(), str.size());
  version_str[str.size()] = '\0';

  while ((version_str[work_index] != '\0') || (MAX_VERSION_LEN > work_index))
  {
    if (version_str[work_index] == '.')
    {
      version_str[work_index] = ' ';

      stringstream stream(version_str);

      stream >> major_temp;
      stream >> minor_temp;
      // Parse major version
      if (stream.fail() || (major_temp > 0xff))
      {
        std::cerr << "Errno: " << errno << std::endl;
        std::cerr << "major_temp = " << major_temp << std::endl;
        status = false;
      }

      // Parse minor version
      else if (stream.fail() || (minor_temp > 0xff))
      {
        std::cerr << "Errno: " << errno << std::endl;
        std::cerr << "minor_temp = " << minor_temp << std::endl;
        status = false;
      }
      else
      {
        /* Copy last to ensure no side effects on error */
        out_major_version = major_temp;
        out_minor_version = minor_temp;

        status = true;
      }
      break;
    }
    work_index++;
  }
  return status;
}

NVSHFR::DefinitionParser& NVSHFR::DefinitionParser::readDefinition(
                                                                   std::istream& s,
                                                                   std::string default_name)
{
  std::string buffer;

  struct section * current_section_data = NULL;
  string current_section_name = default_name;

  enum states
  {
    NEW,
    NORMAL_STATE,
    VERSION,
    NBROFKEYS,
    COMMAND,
    IGNORE,
    SKIP,
    ERROR,
    FINISHED

  } currstate = NEW;

  int32_t major_version = 0;
  int32_t minor_version = 0;

  while ((!s.eof()) && (currstate != ERROR))
  {
    s >> buffer;
    if (s.eof())
    {
      currstate = FINISHED;
    }

    // Comments in file
    if (buffer[0] == '#')
    {
      (void) s.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    }
    else
    {

      if (currstate == NEW)
      {
        /* Delete entry if the same */
        if (definitions.find(current_section_name)
            != definitions.end())
        {
          definitions[current_section_name].entries.clear();
          current_section_data = &definitions[current_section_name];
        }
        else
        {
          struct section current_section;
          definitions[current_section_name] = current_section; /* Will be copied in to map from stack */
          current_section_data = &definitions[current_section_name];
        }
        current_section_data->type = DefinitionParser::NORMAL;

        currstate = VERSION;
      }

      /* Special command?
       * NOTE: This was part of an idea to have sections in a definition file.
       * If KMAC and KTRANS needs special handling, this might be used
       */
      if (buffer[0] == '!')
      {
        currstate = COMMAND;
        (void) buffer.erase(0U, 1U);
        if (buffer == "SECTION")
        {
          s >> current_section_name;
          currstate = NEW;
        }
        else if (buffer == "TYPE")
        {
          std::string type;
          s >> type;
          if (type == "iter")
          {
            current_section_data->type = ITERATIVE;
          }
          currstate = SKIP;
        }
        else
        {
          std::stringstream error;
          error << "Unknown command " << buffer
                << " in definition parsing";
          throw std::logic_error(error.str().c_str());
        }
      }
      else
      {
        if (buffer == "NbrOfKeys")
        {
          currstate = NBROFKEYS;
        }
      }

      /* Parse state machine */
      switch (currstate)
      {
        case VERSION:
        {
          if (buffer == "Version")
          {
            s >> buffer;
            if (!DefinitionParser::parseVersion(major_version, minor_version, buffer))
            {
              std::stringstream error;
              error << "Found ill formed version '" << buffer
                    << "' in definition parsing";
              throw std::logic_error(error.str().c_str());
            }
            current_section_data->major_version = major_version;
            current_section_data->minor_version = minor_version;
            currstate = NORMAL_STATE;
          }
          break;
        }

        case SKIP:
        {
          currstate = VERSION;
          break;
        }

        case NEW:
        {
          case IGNORE:
          break;
        }

        case NORMAL_STATE:
        {
          string value;
          struct entry entry_data;
          s >> value;

          entry_data.name = buffer;
          entry_data.value = value;

          current_section_data->entries.push_back(entry_data);
          break;
        }

        case NBROFKEYS:
        {
          string value;
          s >> value;
          current_section_data->nbr_of_Keys_Name = buffer;
          currstate = NORMAL_STATE;
          break;
        }

        case COMMAND:
        {
          break;
        }

        case FINISHED:
        {
          break;
        }

        case ERROR:
        default:
        {
          throw std::logic_error("Reached undefined layout in definition file");
        }
      }
    }
  }
  return *this;
}

NVSHFR::AbstractData* NVSHFR::DefinitionParser::build(
                                                      std::string section_name)
{
  AbstractData * return_data;
  struct section * current_section_data;
  if (definitions.find(section_name) != definitions.end())
  {
    current_section_data = &definitions[section_name];
    switch (current_section_data->type)
    {
      case NORMAL:
        {
        ParameterList* const l = new ParameterList(
                                                   current_section_data->major_version,
                                                   current_section_data->minor_version);

        if ((section_name == static_cast<std::string>("KMAC.bin"))
            || (section_name == static_cast<std::string>("KTRANS.bin")))
        {

          l->setSectionNameAndNbrOfKeysName(section_name,
                                            current_section_data->nbr_of_Keys_Name);
        }

        for (entry_iter it = current_section_data->entries.begin();
            it != current_section_data->entries.end(); it++)
        {
          (void) l->add(DefinitionParser::newParameterFromEntry((*it)));
        }
        return_data = l;
        break;
      }
      case ITERATIVE:
        {
        IterativeParameterList * const l = new IterativeParameterList();
        l->setVersion(current_section_data->major_version,
                      current_section_data->minor_version);

        if ((section_name == "KMAC.bin")
            || (section_name == "KTRANS.bin"))
        {
          l->setSectionNameAndNbrOfKeysName(section_name,
                                            current_section_data->nbr_of_Keys_Name);
        }

        for (entry_iter it = current_section_data->entries.begin();
            it != current_section_data->entries.end(); it++)
        {
          l->add(DefinitionParser::newParameterFromEntry((*it)));
        }
        return_data = l;
        break;
      }
      default:
        throw std::logic_error("Unrecognized data type");
    }

  }
  else
  {
    std::stringstream error;
    error << "Could not find section " << section_name;
    throw std::logic_error(error.str().c_str());
  }

  return return_data;
}

uint32_t NVSHFR::DefinitionParser::parseInteger(std::string str)
{
  uint32_t ret;
  std::istringstream value(str);

  value >> ret;

  if (value.fail())
  {
    stringstream ss;
    ss << "Could not read value '" << str
       << "' as integer in configuration data";
    throw std::logic_error(ss.str().c_str());
  }

  return ret;
}

NVSHFR::GenericParameter* NVSHFR::DefinitionParser::newParameterFromEntry(
                                                                          const struct entry& data_entry)
{
  GenericParameter * rptr = NULL;
  std::string value = data_entry.value;

  if (value == "BOOL_TYPE")
  {
    rptr = new Parameter(data_entry.name, 4U);
  }
  else if (value == "DWORD_TYPE")
  {
    rptr = new Parameter(data_entry.name, 4U);
  }
  else if (value == "WORD_TYPE")
  {
    rptr = new Parameter(data_entry.name, 2U);
  }
  else if (value == "BYTE_TYPE")
  {
    rptr = new Parameter(data_entry.name, 1U);
  }
  else
  {
    stringstream ss;
    ss << "Ill formed value type '" << value << "' for argument '"
       << data_entry.name
       << "'";
    throw std::logic_error(ss.str().c_str());
  }

  return rptr;
}
