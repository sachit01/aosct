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
 * %name: hex_parameter.cpp %
 * %version: 4 %
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
 * 4             20190919  rkongari  Updated Review comments
 * 3             20190916  rkongari  arn_043#6330 : Lint corrections
 * 2             20180212  anlindel  Updated to use only UPPERCASE letters (A-F) in output.
 * 1             20170627  ddigeraa  First version created
 *
 ***************************************************************************/
/**************************************************************************
 * Includes
 ***************************************************************************/
#include "hex_parameter.hpp"

#include <iomanip>
#include <stdexcept>
#include <sstream>

/**************************************************************************
 * Macro definitions
 ***************************************************************************/

/**************************************************************************
 * Typedefs
 ***************************************************************************/

/**************************************************************************
 * Data declarations/definitions
 ***************************************************************************/

NVSHFR::HexParameter::HexParameter(std::string hp_name,
                                   const uint32_t hp_length)
    :
      GenericParameter(hp_name, hp_length)
{
  data.resize(hp_length);
}

NVSHFR::HexParameter::~HexParameter()
{
}

/*! \see GenericParameter::printCfg()
 *
 */
bool NVSHFR::HexParameter::printCfg(std::ostream& out)
{
  bool pstatus = false;

  out << getName() << " ";
  if (!this->getHasValue())
  {
    out << "!UNDEFINED\n";
    pstatus = false;
  }
  /* To mess up CRC if not validated */
  else
  {
    if (!this->getIsValidated())
    {
      out << "?";
    }

    for (uint32_t i = 0U; i < this->getSize(); i++)
    {
      out << std::setfill('0') << std::setw(2) << std::hex
          << std::uppercase
          << static_cast<int32_t>(this->data[i])
          << std::dec;
    }

    out << std::endl;
    pstatus = true;
  }

  return pstatus;
}

/*! \see GenericParameter::writeCondensedValueToStream()
 *
 */
void NVSHFR::HexParameter::writeCondensedValueToStream(std::ostream& out)
{

  out << getName();
  if (!this->getHasValue())
  {
    out << "!UNDEFINED";
  }
  else
  {

    if (!this->getIsValidated())
    {
      out << "?";
    }

    for (uint32_t i = 0U; i < this->getSize(); i++)
    {
      out << std::setfill('0') << std::setw(2) << std::hex
          << std::uppercase
          << static_cast<int32_t>(this->data[i])
          << std::dec;
    }
  }
}

/*! \see GenericParameter::readValueFromBinStream()
 *
 */
bool NVSHFR::HexParameter::readValueFromBinStream(std::istream& s)
{
  for (uint32_t i = 0U; i < this->getSize(); i++)
  {
    data[i] = static_cast<int32_t>(s.get());
  }
  this->SetHasValue(true);
  return true;
}

/*! \see GenericParameter::verifyValueFromBinStream()
 *
 */
bool NVSHFR::HexParameter::verifyValueFromBinStream(std::istream& s)
{
  bool valid = true;
  for (uint32_t i = 0U; i < this->getSize(); i++)
  {
    if (data[i] != static_cast<int32_t>(s.get()))
    {
      valid = false;
      {
        std::stringstream error;
        error << "Failed to validate " << this->getName()
              << " mismatch in value at position "
              << static_cast<uint8_t>(s.peek());
        throw std::logic_error(error.str());
      }

    }
  }
  this->SetIsValidated(valid);
  return valid;
}

/*! \see GenericParameter::clone()
 *
 */
NVSHFR::GenericParameter* NVSHFR::HexParameter::clone() const
{
  return new HexParameter(*this);
}
