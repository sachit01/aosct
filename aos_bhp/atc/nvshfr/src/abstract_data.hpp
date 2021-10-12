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
 * %name: abstract_data.hpp %
 * %version: 1.1.2 %
 * %created_by: rkongari %
 * %date_created: 2019-09-19 06:52 %
 *
 * Description: Header file for AbstractData
 *
 ***************************************************************************/
/**************************************************************************
 * Revision History
 *
 * Version       Date    Sign      Change Description
 * 1.1.2         20190919  rkongari  Updated review comments
 * 1.1.1         20190916  rkongari  arn_043#6330 : Lint corrections
 * 1             20170627  ddigeraa  First version created
 *
 ***************************************************************************/
#ifndef ABSTRACT_DATA_HPP_
#define ABSTRACT_DATA_HPP_

/**************************************************************************
 * Includes
 ***************************************************************************/
#include <stdint.h>
#include <istream>
#include <ostream>

/**************************************************************************
 * Macro definitions
 ***************************************************************************/

/**************************************************************************
 * Typedefs
 ***************************************************************************/

/**************************************************************************
 * Data declarations/definitions
 ***************************************************************************/

/*! \brief AbstractData is the common superclass for nvsh data body.
 *
 * Abstract data is used for parsing data blocks from a stream. The
 * class expects two identical blocks of binary data. The second block
 * is to verify the first.
 *
 * The output is a textual representation of the data put in.
 *
 */
namespace NVSHFR
{
class AbstractData
{
public:
  AbstractData();
  virtual ~AbstractData();

  virtual bool readValuesFromBinStream(std::istream &s, uint32_t size) = 0;
  virtual bool verifyValuesFromBinStream(std::istream &s, uint32_t size) = 0;

  virtual bool printCfg(std::ostream &out) = 0;

  virtual uint32_t getSize(void) = 0;
  virtual uint32_t getCrc(void) = 0;

};

} // namespace NVSHFR
#endif /* ABSTRACT_DATA_HPP_ */
