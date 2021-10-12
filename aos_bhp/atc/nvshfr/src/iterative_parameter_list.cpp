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
 *
 * Description: Source file for IterativeData
 *
 ***************************************************************************/
/**************************************************************************
 * Revision History
 *
 * Version       Date      Sign      Change Description
 * -             20200218  mohsharm  Updation for solving issue of CRC and SRC_CRC
 *                                   mismatch of KMAC, KTRANS and RADIO CFG files.
 * -             20200210  anlindel  Minor lint corrections
 * 7             20190919  rkongari  Updated Review comments
 * 6             20190916  rkongari  arn_043#6330 : Lint corrections
 * 3             20180604  rboyapat  arn_043_5900:Printing CRC in text file
 *                                   has been removed from printCfg()
 * 2             20170629  ddigeraa  Remove Entries from list
 * 1             20170616  ddigeraa  First version created
 *
 ***************************************************************************/
/**************************************************************************
 * Includes
 ***************************************************************************/
#include "iterative_parameter_list.hpp"
#include "stream_helper.hpp"
#include "app_error.hpp"

#include <sstream>
#include <stdexcept>
#include <iomanip>

#include <iostream>
/**************************************************************************
 * Macro definitions
 ***************************************************************************/

/**************************************************************************
 * Typedefs
 ***************************************************************************/
namespace
{
typedef std::vector<NVSHFR::GenericParameter*,
    std::allocator<NVSHFR::GenericParameter*> >::iterator param_iter;
}
/**************************************************************************
 * Data declarations/definitions
 ***************************************************************************/

NVSHFR::IterativeParameterList::IterativeParameterList()
    :
      template_size(0U),
      list_iter(0U), major_version(0), minor_version(0), nbr_keys_name(
                                                                       ""),
      nbr_keys_val(0U), section_name("")
{
}

NVSHFR::IterativeParameterList::~IterativeParameterList()
{
  // TODO Auto-generated destructor stub
}

void NVSHFR::IterativeParameterList::add(GenericParameter* const parameter)
{
  if (parameter == NULL)
  {
    throw std::logic_error(
                           "Internal error: IterativeParameterList::add() got NULL param");
  }
  this->template_parameters.push_back(parameter);
  this->template_size += parameter->getSize();
}

void NVSHFR::IterativeParameterList::setVersion(const int32_t in_major_version,
                                                const int32_t in_minor_version)
{
  this->major_version = in_major_version;
  this->minor_version = in_minor_version;
}

void NVSHFR::IterativeParameterList::setSectionNameAndNbrOfKeysName(
                                                                    std::string s_name,
                                                                    std::string nbr_of_key_name)
{

  this->section_name = s_name;
  this->nbr_keys_name = nbr_of_key_name;
}

bool NVSHFR::IterativeParameterList::readValuesFromBinStream(std::istream& s,
                                                             uint32_t const size)
{
  int32_t major_v = 0;
  int32_t minor_v = 0;
  uint32_t len = 0U;
  const uint32_t template_count = this->template_parameters.size();

  if (this->template_size == 0U)
  {
    throw std::logic_error("template size is zero");
  }

  /*removing the length of the NbrOfKeys from the size*/
  if ((this->section_name == "KMAC.bin")
      || (this->section_name == "KTRANS.bin"))
  {
    len = static_cast<uint32_t>(CRC_SIZE + 2U);
  }
  else
  {
    len = static_cast<uint32_t>(CRC_SIZE);
  }

  if ((size > len) && (((size - len) % this->template_size) != 0U))
  {
    std::stringstream ss;
    ss << "Parameter list expects " << this->template_size
       << "*N bytes, but indata is "
       << size << " bytes";
    throw std::logic_error(ss.str().c_str());
  }
  else if (size < len)
  {

    std::stringstream ss;
    ss << "Received data size is < CRC SIZE "
       << "Received bytes   "
       << size << " bytes";
    throw std::logic_error(ss.str().c_str());
  }
  else
  {

    major_v = s.get();
    minor_v = s.get();

    if ((this->major_version != major_v) || (this->minor_version != minor_v))
    {
      std::stringstream ss;
      ss << "Definition version " << static_cast<int32_t>(this->major_version)
         << "."
         << static_cast<int32_t>(this->minor_version)
         << " does not match with binary version "
         << static_cast<int32_t>(major_v) << "."
         << static_cast<int32_t>(minor_v)
         << std::endl;
      throw AppError(ss.str(), AppError::INVALID_FILE_VERSION);
    }

    //reading the name and value for NbrOfKeys
    if ((this->section_name == "KMAC.bin")
        || (this->section_name == "KTRANS.bin"))
    {
      uint16_t nbr_keys = static_cast<uint16_t>(s.get());
      nbr_keys <<= 8U;
      nbr_keys |= static_cast<uint16_t>(s.get());

      this->nbr_keys_val = nbr_keys;
    }

    this->parameters.clear();
    this->parameters.resize(
                            template_count * ((size - len) / this->template_size));

    /* Populate real list */
    for (uint32_t i = 0U; i < ((size - len) / this->template_size); i++)
    {
      for (uint32_t item = 0U; item < template_count; item++)
      {
        uint32_t const pos = item + (i * template_count);
        GenericParameter * const param =
                                         this->template_parameters[item]->clone();
        (void) param->readValueFromBinStream(s);
        this->parameters[pos] = param;
      }
    }

    this->list_iter = ((size - len) / this->template_size);
  }

  return true;
}

bool NVSHFR::IterativeParameterList::verifyValuesFromBinStream(std::istream& s,
                                                               uint32_t const size)
{
  bool verify_ok = true;
  uint32_t len = 0U;
  if (this->template_size == 0U)
  {
    throw std::logic_error("template size is zero");
  }

  /*removing the length of the NbrOfKeys from the size*/
  if ((this->section_name == "KMAC.bin")
      || (this->section_name == "KTRANS.bin"))
  {
    len = static_cast<uint32_t>(CRC_SIZE + 2U);
  }
  else
  {
    len = static_cast<uint32_t>(CRC_SIZE);
  }

  if (((size - len) % this->template_size) != 0U)
  {
    std::stringstream ss;
    ss << "Parameter list expects " << this->template_size
       << "*N bytes, but indata is "
       << size << " bytes";
    throw std::logic_error(ss.str().c_str());
  }

  if (this->major_version != static_cast<int32_t>(s.get()))
  {
    verify_ok = false;
  }
  else if (this->minor_version != static_cast<int32_t>(s.get()))
  {
    verify_ok = false;
  }
  else
  {
    //reading the name and value for NbrOfKeys
    if ((this->section_name == "KMAC.bin")
        || (this->section_name == "KTRANS.bin"))
    {
      uint16_t nbr_keys = static_cast<uint16_t>(s.get());
      nbr_keys <<= 8U;
      nbr_keys |= static_cast<uint16_t>(s.get());

      if (this->nbr_keys_val != nbr_keys)
      {
        verify_ok = false;
      }
    }

    if (verify_ok == true)
    {
      for (uint32_t i = 0U;
          i < (this->list_iter * this->template_parameters.size());
          i++)
      {
        verify_ok = this->parameters[i]->verifyValueFromBinStream(s)
                    && verify_ok;
      }
    }
  }
  return verify_ok;
}

bool NVSHFR::IterativeParameterList::printCfg(std::ostream& out)
{
  out << "Version " << static_cast<int32_t>(this->major_version) << "."
      << static_cast<int32_t>(this->minor_version)
      << std::endl;
  /* out << "Entries " << (int) this->list_iter << std::endl; */

  if ((this->section_name == "KMAC.bin")
      || (this->section_name == "KTRANS.bin"))
  {
    out << this->nbr_keys_name << " "
        << static_cast<uint16_t>(this->nbr_keys_val)
        << std::endl;
  }

  for (param_iter it = this->parameters.begin(); it != this->parameters.end();
      it++)
  {
    (void) (*it)->printCfg(out);
  }

  return true;
}

/*! \brief return CRC32 sum for parameter list
 *
 * Returns the CRC32 sum for the parameterlist. Checksum is calculated
 * according to 1DOC-1012132
 *
 */
uint32_t NVSHFR::IterativeParameterList::getCrc(void)
{
  std::stringstream condensed_data;

  condensed_data << "Version" << static_cast<int32_t>(this->major_version)
                 << "."
                 << static_cast<int32_t>(this->minor_version);

  if ((this->section_name == "KMAC.bin")
      || (this->section_name == "KTRANS.bin"))
  {
    condensed_data << "NbrOfKeys"<< static_cast<uint16_t>(this->nbr_keys_val);
  }

  /*condensed_data << "Entries" << (int) this->list_iter;*/
  for (param_iter it = this->parameters.begin(); it != this->parameters.end();
      it++)
  {
    (*it)->writeCondensedValueToStream(condensed_data);
  }

  uint32_t const length = static_cast<uint32_t>(condensed_data.tellp());

  return StreamHelper::calculateCRC32(condensed_data, length, false);
}

uint32_t NVSHFR::IterativeParameterList::getSize(void)
{
  return this->template_size;
}
