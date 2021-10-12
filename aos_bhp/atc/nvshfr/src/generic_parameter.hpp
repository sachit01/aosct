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
 * %name: generic_parameter.hpp %
 * %version: 1.1.2 %
 * %created_by: rkongari %
 * %date_created: 2019-09-19 07:57 %
 *
 * Description: Header file for GenericParameter
 *
 ***************************************************************************/
/**************************************************************************
 * Revision History
 *
 * Version       Date      Sign      Change Description
 * 1.1.2         20190919  rkongari  Updated Review comments
 * 1.1.1         20190916  rkongari  arn_043#6330 : Lint corrections
 * 1             20170627  ddigeraa  First version created
 *
 *
 ***************************************************************************/
#ifndef GENERIC_PARAMETER_HPP_
#define GENERIC_PARAMETER_HPP_

/**************************************************************************
 * Includes
 ***************************************************************************/
#include <stdint.h>
#include <istream>
#include <ostream>
#include <string>

/**************************************************************************
 * Macro definitions
 ***************************************************************************/

/**************************************************************************
 * Typedefs
 ***************************************************************************/

/**************************************************************************
 * Data declarations/definitions
 ***************************************************************************/

/*! \brief GenericParameter is a common superclass for different data types
 *
 * ETCS configuration only uses integer values, but radio configuration
 * heavily relies on hex and bcd values, which isn't handled in the scope of
 * integer values.
 *
 * This class helps by creating an interface for any parameter type and it's
 * length.
 *
 */
namespace NVSHFR
{
class GenericParameter
{
public:
  GenericParameter();
  GenericParameter(std::string inName, uint32_t inSize);
  virtual ~GenericParameter();

  /*! \brief clone creates a copy of the parameter
   *
   * The new parameter should not share any data with the old parameter.
   * This means that a value change in the copy will not update the value
   * from the original.
   *
   * \ret GenericParameter the copy
   */
  virtual GenericParameter * clone() const = 0;

  /* Porcelain output */
  /*! \brief printCfg prints the textual representation of the data
   *
   * The data is written to the passed stream reference as human readable
   * and with a newline in the end.
   *
   * Example output: "foovar 1337\n"
   */
  virtual bool printCfg(std::ostream &out) = 0;

  /*! \brief Write name and value without spaces or newlines
   *
   * writeCondensedValueToStream() is used for when a CRC32 is to
   * be calculated for output.
   *
   * Please note that GenericParameter class itself does not know about
   * the CRC32 sum, but only provides this function as convenience.
   *
   */
  virtual void writeCondensedValueToStream(std::ostream &out) = 0;

  /* Porcelain input */
  /*! \brief Read value into parameter
   *
   * Read a binary value into the parameter.
   * readValueFromBinStream() will only read as many bytes as the parameter
   * have specified from the instream.
   *
   * \param s the instream the value is read from
   *
   */
  virtual bool readValueFromBinStream(std::istream &s) = 0;

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
  virtual bool verifyValueFromBinStream(std::istream &s) = 0;

  /* Plumbing */
  /*! \brief return the size in bytes for the parameter
   */
  virtual uint32_t getSize() const
  {
    return size;
  }
  ;

  /*! \brief return true if a value have been provided
   */
  virtual bool getHasValue() const
  {
    return has_value;
  }
  ;

  /*! \brief return true if value have been validated
   */
  virtual bool getIsValidated() const
  {
    return is_validated;
  }
  ;

  /*! \brief return name
   */
  virtual std::string getName() const
  {
    return name;
  }
  ;
  protected:
  /*! \brief set the HasValue
   */
  virtual void SetHasValue(const bool Hstatus)
  {
    has_value = Hstatus;
  }
  ;

  /*! \brief set the status of the is_validated
   */
  virtual void SetIsValidated(const bool Vstatus)
  {
    is_validated = Vstatus;
  }
  ;

private:
  std::string name;
  uint32_t size;
  bool has_value;
  bool is_validated;
};

} // namespace NVSHFR

#endif /* GENERIC_PARAMETER_HPP_ */
