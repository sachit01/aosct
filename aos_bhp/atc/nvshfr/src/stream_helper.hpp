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
 * %name: stream_helper.hpp %
 * %version: 1.1.2 %
 * %created_by: rkongari %
 * %date_created: 2019-09-19 10:50 %
 *
 * Description: Header file for Stream helper
 *
 ***************************************************************************/
/**************************************************************************
 * Revision History
 *
 * Version       Date      Sign      Change Description
 * 1.1.2         20190919  rkongari  Updated Review comments
 * 1.1.1         20190916  rkongari  arn_043#6330 : Lint corrections
 * 1             20170613  ddigeraa  First version created
 *
 ***************************************************************************/
#ifndef STREAM_HELPER_HPP_
#define STREAM_HELPER_HPP_

/**************************************************************************
 * Includes
 ***************************************************************************/

/**************************************************************************
 * Macro definitions
 ***************************************************************************/

/**************************************************************************
 * Typedefs
 ***************************************************************************/

/**************************************************************************
 * Data declarations/definitions
 ***************************************************************************/

/*! \brief helper functions for reading data from binary streams
 *
 * Class contains simple as well as more complex functions for reading data from
 * streams.
 *
 * Class StreamHelper only contains static functions and can't be initialized.
 *
 */
namespace NVSHFR
{

static const uint32_t VFW_NVSH_FILENAME_MAX_LEN = 127U;
static const uint32_t VFW_NVSH_UNIQUEFILEID_MAX_LEN = 127U;

class StreamHelper
{
public:
  static uint16_t getUint16(std::istream &s);
  static uint32_t getUint32(std::istream &s);
  static uint64_t getUint64(std::istream &s);
  static std::string getString(std::istream &s);
  static uint32_t calculateCRC32(std::istream& s, const uint32_t length,
                                 const bool s_rewind = true);
  static uint64_t calculateCRC64(std::istream& s, const uint32_t length,
                                 const bool s_rewind = true);

private:
  StreamHelper();
  virtual ~StreamHelper();
};
} // namespace NVSHFR
#endif /* STREAM_HELPER_HPP_ */
