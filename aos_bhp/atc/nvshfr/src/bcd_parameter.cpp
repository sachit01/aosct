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
 * %name: bcd_parameter.cpp %
 * %version: 1.1.2 %
 * %created_by: rkongari %
 * %date_created: 2019-09-19 07:09 %
 *
 * Description: Source file for BcdParameter
 *
 ***************************************************************************/
/**************************************************************************
 * Revision History
 *
 * Version       Date    Sign      Change Description
 * 1.1.2         20190919  rkongari  updated review comments
 * 1.1.1         20190916  rkongari  arn_043#6330 : Lint corrections
 * 1             20170627  ddigeraa  First version created
 *
 ***************************************************************************/

/**************************************************************************
 * Includes
 ***************************************************************************/
#include "bcd_parameter.hpp"

/**************************************************************************
 * Macro definitions
 ***************************************************************************/

/**************************************************************************
 * Typedefs
 ***************************************************************************/

/**************************************************************************
 * Data declarations/definitions
 ***************************************************************************/

NVSHFR::BcdParameter::BcdParameter()
    : digits(0U)
{
}

/*! \brief
 *
 */
NVSHFR::BcdParameter::BcdParameter(std::string param_name, const uint32_t val)
    :
      GenericParameter(param_name, (val + 1U) / 2U),
      digits(val)
{
  this->decvalue.resize(val);
}

NVSHFR::BcdParameter::~BcdParameter()
{
}

/*! \see GenericParameter::printCfg()
 *
 */
bool NVSHFR::BcdParameter::printCfg(std::ostream& out)
{
  bool ret_val = true;

  out << this->getName() << " ";

  if (!this->getHasValue())
  {
    out << "!UNDEFINED\n";
    ret_val = false;
  }
  else
  {
    if (!this->getIsValidated())
    {
      out << "?";
    }
    for (uint32_t i = 0U; i < this->digits; i++)
    {
      out << std::dec << static_cast<int32_t>(this->decvalue[i]);
    }

    out << std::endl;
  }
  return ret_val;
}

/*! \see GenericParameter::writeCondensedValueToStream()
 *
 */
void NVSHFR::BcdParameter::writeCondensedValueToStream(std::ostream& out)
{
  out << this->getName();

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
    for (uint32_t i = 0U; i < this->digits; i++)
    {
      out << std::dec << static_cast<int32_t>(this->decvalue[i]);
    }
  }
}

/*! \see GenericParameter::readValueFromBinStream()
 *
 */
bool NVSHFR::BcdParameter::readValueFromBinStream(std::istream& s)
{
  for (uint32_t i = 0U; i < getSize(); i++)
  {
    uint32_t const val = static_cast<uint32_t>(s.get());
    this->decvalue[i * 2U] = static_cast<uint8_t>(val >> 4U);
    this->decvalue[(i * 2U) + 1U] = static_cast<uint8_t>(val & static_cast<uint8_t>(0x0F));
  }

  this->SetHasValue(true);
  return true;
}

/*! \see GenericParameter::clone()
 *
 */
NVSHFR::GenericParameter* NVSHFR::BcdParameter::clone() const
{
  return new BcdParameter(*this);
}

/*! \see GenericParameter::verifyValueFromBinStream()
 *
 */

bool NVSHFR::BcdParameter::verifyValueFromBinStream(std::istream& s)
{
  bool valid = true;
  for (uint32_t i = 0U; i < getSize(); i++)
  {
    uint32_t const val = static_cast<uint32_t>(s.get());
    if ((this->decvalue[i * 2U] != static_cast<uint8_t>(val >> 4))
        || (this->decvalue[(i * 2U) + 1U]
            != static_cast<uint8_t>(val & static_cast<uint8_t>(0x0F))))
    {
      valid = false;
    }
  }

  this->SetIsValidated(valid);
  return valid;
}
