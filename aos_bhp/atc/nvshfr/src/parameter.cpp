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
 * %name: parameter.cpp %
 * %version: 4 %
 * %created_by: rkongari %
 * %date_created: 2019-09-19 10:45 %
 *
 * Description: Source file for Parameter
 *
 ***************************************************************************/
/**************************************************************************
 * Revision History
 *
 * Version       Date      Sign      Change Description
 * 4             190919    rkongari  Updated Review comments
 * 3             190916    rkongari  arn_043#6330 : Lint corrections
 * 2             180608    rboyapat  arn_043#6276,6030:Support for 8 byte
 *                                   config parameter added
 * 1             170612    ddigeraa  First version created
 *
 ***************************************************************************/

/**************************************************************************
 * Includes
 ***************************************************************************/
#include <iostream>
#include <stdexcept>
#include <sstream>
#include "parameter.hpp"
#include "stream_helper.hpp"

/**************************************************************************
 * Macro definitions
 ***************************************************************************/

/**************************************************************************
 * Typedefs
 ***************************************************************************/

/**************************************************************************
 * Data declarations/definitions
 ***************************************************************************/

/*! \brief Parameter initializator
 *
 * Parameter initialization will take a name and size.
 *
 * \param name name for the parameter that will be written out in text
 * \param size the size of the parameter in bytes. Valid values are 1, 2 and 4
 */
NVSHFR::Parameter::Parameter(const string param_name, const uint32_t param_size)
    :
      GenericParameter(param_name, param_size),
      value(0UL)
{
  if ((param_size != 1U) && (param_size != 2U) && (param_size != 4U)
      && (param_size != 8U))
  {
    stringstream error;
    error << "Try to parse unhandled value size: " << param_size << " for '"
          << param_name
          << "'";
    throw std::logic_error(error.str());
  }

}

NVSHFR::Parameter::~Parameter()
{
  // Intentionally left empty
}

/*! \brief Print the name and value for a parameter
 *
 * Print name and its value to an out stream.
 *
 * The textual value of non valid parameter will be "!UNDEFINED"
 * Defined but not validated prarameters will be prefixed with "?"
 *
 * Output format is name << " " << value << newline
 *
 * \param out stream to write parameter to
 */
bool NVSHFR::Parameter::printCfg(std::ostream& out)
{
  out << this->getName() << " ";
  if (this->getHasValue())
  {
    out << (this->getIsValidated() ? "" : "?") << this->value;
  }
  else
  {
    out << "!UNDEFINED";
  }
  out << endl;

  return this->getHasValue();
}

/*! \brief Write name and value without spaces or newlines
 *
 * writeCondensedValueToStream() is used for when a CRC32 is to
 * be calculated for output.
 *
 * Please note that Parameter class itself does not know about
 * the CRC32 sum, but only provides this function as convenience.
 *
 */
void NVSHFR::Parameter::writeCondensedValueToStream(ostream& out)
{
  out << this->getName() << (this->getIsValidated() ? "" : "?");
  if (this->getHasValue())
  {
    out << this->value;
  }
  else
  {
    out << "!UNDEFINED";
  }
}

/*! \brief Read value into parameter
 *
 * Read a binary value into the parameter.
 * readValueFromBinStream() will only read as many bytes as the parameter
 * have specified from the instream.
 *
 * \param s the instream the value is read from
 *
 */
bool NVSHFR::Parameter::readValueFromBinStream(istream& s)
{
  switch (getSize())
  {
    case 1:
      value = static_cast<uint32_t>(s.get());
      break;
    case 2:
      value = StreamHelper::getUint16(s);
      break;
    case 4:
      value = StreamHelper::getUint32(s);
      break;
    case 8:
      value = StreamHelper::getUint64(s);
      break;

    default:
      stringstream error;
      error << "Try to parse unhandled value size: " << getSize() << " for '"
            << getName()
            << "'";
      throw std::logic_error(error.str());
      break;
  }
  SetHasValue(true);
  return true;
}

/*! \brief Verify value from parameter
 *
 * Read a binary value form an instream and verify the parameter.
 * readValueFromBinStream() will only read as many bytes as the parameter
 * have specified from the instream.
 *
 * \param s the instream the value is read from
 * \ret true if validation went OK, false otherwise
 *
 */
bool NVSHFR::Parameter::verifyValueFromBinStream(istream& s)
{
  uint64_t read_value = 0U;
  switch (getSize())
  {
    case 1:
      read_value = static_cast<uint64_t>(s.get());
      break;
    case 2:
      read_value = StreamHelper::getUint16(s);
      break;
    case 4:
      read_value = StreamHelper::getUint32(s);
      break;
    case 8:
      read_value = StreamHelper::getUint64(s);
      break;
    default:
      stringstream error;
      error << "Try to parse unhandled value size: " << getSize() << " for '"
            << getName()
            << "'";
      throw std::logic_error(error.str());
      break;
  }

  if (read_value == value)
  {
    this->SetIsValidated(true);
  }
  else
  {
    this->SetIsValidated(false);
  }

  if (!this->getIsValidated())
  {
    std::stringstream error;
    error << "Failed to validate " << static_cast<std::string>(this->getName())
          << " mismatch in value at position "
          << static_cast<uint32_t>(s.peek());
    throw std::logic_error(error.str());
  }
  return this->getIsValidated();
}

/*! \brief Get the value of parameter
 *
 * Returns the value from the parameter. All values then need to be
 * recasted down to appropriate size.
 *
 * \ret value of the parameter
 */
uint64_t NVSHFR::Parameter::getValue()
{
  if (!this->getHasValue())
  {
    throw std::logic_error("No value in parameter");
  }
  return value;
}

/*! \brief Clone the parameter
 *
 * Returns a copy of the parameter. Modifications made to the copy does not reflect
 * the original
 *
 * TODO Write test for this
 *
 * \ret value of the parameter
 */
NVSHFR::GenericParameter* NVSHFR::Parameter::clone() const
{
  return new Parameter(*this);
}
