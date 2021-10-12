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
 * Description: Parameter list source file
 *
 ***************************************************************************/
/**************************************************************************
 * Revision History
 *
 * Version       Date     Sign      Change Description
 * -            200210    anlindel  Minor lint corrections
 * 5            190919    rkongari  Updated Review comments
 * 4            190916    rkongari  arn_043#6330 : Lint corrections
 * 3            190509    rkongari  updated for arn_043#3540,arn_043#7171
 * 2            180604    rboyapat  arn_043#5990:CRC has been removed from
 *                                  output of NVSHFR.
 *
 ***************************************************************************/

/**************************************************************************
 * Includes
 ***************************************************************************/
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <iomanip>

#include "parameter_list.hpp"
#include "stream_helper.hpp"
#include "app_error.hpp"

/**************************************************************************
 * Macro definitions
 ***************************************************************************/

/**************************************************************************
 * Typedefs
 ***************************************************************************/
namespace NVSHFR
{
typedef std::vector<NVSHFR::GenericParameter*,
    allocator<NVSHFR::GenericParameter*> >::iterator param_iter;
}

/**************************************************************************
 * Data declarations/definitions
 ***************************************************************************/

NVSHFR::ParameterList::ParameterList(int32_t const major_ver,
                                     int32_t const minor_ver)
    :
      index(0U),
      major_version(major_ver),
      minor_version(minor_ver),
      nbr_keys_name(""),
      nbr_keys_val(0U),
      section_name("")
{
}

NVSHFR::ParameterList::~ParameterList()
{
  // Intentionally left empty
}

/*! \brief Update version of parameter list
 *
 * Update the version of parameter list to a new version. This saves complexity
 * when parsing definition files.
 *
 * \param major major version number
 * \param minor minor version number
 */
void NVSHFR::ParameterList::setVersion(const int32_t in_major_version,
                                       const int32_t in_minor_version)
{
  this->major_version = in_major_version;
  this->minor_version = in_minor_version;
}

/*! \brief Add a parameter to list
 *
 * Adds a parameter to the list. The parameter is copied, thus any updates within
 * the parameter list will not reflect back to the old value.
 *
 * Parameter indexes will be ordered in the order of inclusion, thus add order is
 * very important!
 *
 * Future improvements:
 *  * add checks so that no same parameter names is added.
 *  * Make parameter a reference so that it might be able to be used outside list
 *    as well. Only relevant if tool evolves.
 *
 *
 * \param parameter a parameter to add to the list
 * \ret reference to self for chaining.
 *
 */
NVSHFR::ParameterList &NVSHFR::ParameterList::add(
                                                  GenericParameter * const parameter)
{
  this->parameters.push_back(parameter);
  index += parameter->getSize();
  return *this;
}

/*! \brief Print configuration including version
 *
 * Prints the configuration as specified in 1DOC-1012132
 * If any parameter isn't valid the CRC-sum will be set to zero
 *
 * \param out stream to write parameters and values to.
 */
bool NVSHFR::ParameterList::printCfg(std::ostream &out)
{
  out << "Version " << static_cast<int32_t>(major_version) << "."
      << static_cast<int32_t>(minor_version)
      << std::endl;

  if ((this->section_name == "KMAC.bin")
      || (this->section_name == "KTRANS.bin"))
  {
    out << static_cast<std::string>(this->nbr_keys_name) << " "
        << static_cast<uint16_t>(this->nbr_keys_val)
        << std::endl;
  }

  for (param_iter it = this->parameters.begin(); it != this->parameters.end();
      it++)
  {
    static_cast<void>((*it)->printCfg(out));
  }

  return true;
}

/*! \brief Populate all parameters from a binary blob
 *
 * readValuesFromBinStream() reads a binary stream and populates all parameters.
 *
 * The function will also check received size and expected size and throw an exception
 * if they don't match.
 *
 * The function will also return without loading parameters if version from stream
 * does not match stored version.
 *
 */
bool NVSHFR::ParameterList::readValuesFromBinStream(std::istream& s,
                                                    const uint32_t size)
{
  bool ret_val = false;

  if (this->getSize() != size)
  {
    stringstream ss;
    ss << "Parameter list is " << this->getSize()
       << " bytes, but indata is "
       << size << " bytes";
    throw std::logic_error(ss.str().c_str());
  }

  if (!this->validateVersionFromStream(s))
  {
    static_cast<void>(s.seekg(this->index, ios_base::cur));
    ret_val = false;
  }
  else
  {
    for (param_iter it = this->parameters.begin();
        it != this->parameters.end(); it++)
    {
      static_cast<void>((*it)->readValueFromBinStream(s));
    }

    if (s.eof() || s.fail())
    {
      throw std::runtime_error("End of stream during read string");
    }
    ret_val = true;
  }
  return ret_val;
}

/*! \brief Verify all parameters from a binary blob
 *
 * verifyValuesFromBinStream() reads a binary stream and verifies all parameters.
 *
 * The function will also check received size and expected size and throw an exception
 * if they don't match.
 *
 * The function will also return without verifying parameters if version from stream
 * does not match stored version.
 *
 */
bool NVSHFR::ParameterList::verifyValuesFromBinStream(std::istream& s,
                                                      const uint32_t size)
{
  bool verify_ok = false;

  if (this->getSize() != size)
  {
    static_cast<void>(s.seekg(static_cast<int64_t>(size), ios_base::cur));
  }
  else if (!this->validateVersionFromStream(s))
  {
    static_cast<void>(s.seekg(static_cast<int64_t>(this->index),
                              ios_base::cur));
    stringstream ss;
    ss << "Version is wrong in verification!";
    throw std::logic_error(ss.str().c_str());
  }
  else
  {
    verify_ok = true;

    for (param_iter it = this->parameters.begin();
        it != this->parameters.end(); it++)
    {
      verify_ok = (*it)->verifyValueFromBinStream(s) && verify_ok;
    }
  }

  return verify_ok;
}

/*! \brief Return number of parameters in list
 *
 * Returns the number of parameters in the list.
 */
uint32_t NVSHFR::ParameterList::countElements(void) const
                                              {
  return this->parameters.size();
}

/*! \brief Get the total size of the param list
 *
 * Returns the total size parameter list expects, including version
 * number.
 *
 */
uint32_t NVSHFR::ParameterList::getSize(void)
{
  return this->index + CRC_SIZE;
}

/*! \brief return CRC32 sum for parameter list
 *
 * Returns the CRC32 sum for the parameterlist. Checksum is calculated
 * according to 1DOC-1012132
 *
 */
uint32_t NVSHFR::ParameterList::getCrc(void)
{
  stringstream condensed_data;

  condensed_data << "Version" << static_cast<int32_t>(major_version) << "."
                 << static_cast<int32_t>(minor_version);
  for (param_iter it = this->parameters.begin(); it != this->parameters.end();
      it++)
  {
    (*it)->writeCondensedValueToStream(condensed_data);
  }

  const uint32_t length = static_cast<uint32_t>(condensed_data.tellp());

  return StreamHelper::calculateCRC32(condensed_data, length, false);
}

/*! \brief check if version from stream match own version
 *
 * \ret true if match
 */
bool NVSHFR::ParameterList::validateVersionFromStream(std::istream& s)
{
  const int32_t major = static_cast<int32_t>(s.get());
  const int32_t minor = static_cast<int32_t>(s.get());

  if ((major != this->major_version) || (minor != this->minor_version))
  {
    {
      static_cast<void>(s.seekg(static_cast<int64_t>(this->index),
                                ios_base::cur));
      stringstream ss;
      ss << "Input file version " << static_cast<uint8_t>(major) << "."
         << static_cast<uint8_t>(minor)
         << " not compatible with definition version "
         << static_cast<int32_t>(this->major_version)
         << "."
         << static_cast<int32_t>(this->minor_version);
      throw AppError(ss.str(), AppError::INVALID_FILE_VERSION);
    }
  }
  return true;
}
/*
 * This function is set the NbrOfKeys value and name
 *
 */
void NVSHFR::ParameterList::setSectionNameAndNbrOfKeysName(std::string sname,
                                                           std::string nbr_of_key_name)
{
  this->section_name = sname;
  this->nbr_keys_name = nbr_of_key_name;
}
