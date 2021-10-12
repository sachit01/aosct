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
 * Description: Header file for bin parser
 *
 ***************************************************************************/
/**************************************************************************
 * Revision History
 *
 * Version       Date      Sign      Change Description
 * -             20200210  anlindel  Minor lint correction.
 * 1.2.2         20190919  rkongari  Updated review comments
 * 1.2.1         20190916  rkongari  arn_043#6330 : Lint corrections
 * 1             20170613  ddigeraa  First version created
 *
 ***************************************************************************/
#ifndef BIN_PARSER_HPP_
#define BIN_PARSER_HPP_

/**************************************************************************
 * Includes
 ***************************************************************************/
#include <stdint.h>
#include <istream>
#include <sstream>
#include <string>
#include <vector>

#include "abstract_data.hpp"

/**************************************************************************
 * Macro definitions
 ***************************************************************************/

/**************************************************************************
 * Typedefs
 ***************************************************************************/

/**************************************************************************
 * Data declarations/definitions
 ***************************************************************************/

/*! \brief Parse NV sh binary file
 *
 * Parses A and B block as well as headers and verifies CRC-sum. Reads the
 * NVSH file format specified in 3NSS010519D0020 version 1.2
 *
 */
namespace NVSHFR
{

// Constants
static const uint32_t CRC64_BLOCK_SIZE = 2000U;
static const uint32_t CRC64_TYPE_SIZE = 8U;

class BinParser
{
public:

  BinParser();
  BinParser(AbstractData * const ParameterList, const bool dispatcher);
  virtual ~BinParser();

  bool parse(std::istream &s);

  /* \brief Structure mapping against data header.
   * struct data_header contains functionality for parsing a header from
   * an instream.
   */
  struct data_header
  {
    bool parseHeader(std::istream &s);
    uint32_t interface_version; // Interface version should always be one
    std::string filename; // The filename exluding path
    std::string unique_file_id; // An unique id for the file
    uint32_t data_sequence_number; // Always one
    uint32_t application_data_length; // Length of the following data block
    uint32_t header_size; // Internal variable for storing header size
    bool read_only_data; // Indicates if block is read only or not
  };

  /*! \brief Structure mapping for a data block
   *
   * struct block contains functionality to parsing a data block and verify CRC
   * Some structure flattening have benn done in order to keep number of structures
   * low but at the same time separated enough.
   *
   *
   *
   */
  struct block
  {
    bool parse(std::istream &s);
    bool parseCRC(std::istream &s);
    uint32_t length; // Length of the block
    struct data_header data_header; // Data header. Keeps it's own parsing routines.
    AbstractData * data; // Data block, directly parsed to ParameterList
    std::vector<uint64_t> crc; // Calculated CRC. Missmatch in CRC will result in exception
    uint32_t numblocks; // Internal value for number of CRC64 blocks
    bool verify; // Internal value for write or verify data (A or B)
  };

  struct block block_a; /* A block - will be responsible for setting data*/
  struct block block_b; /* B block - will be responsible for verifying data*/

private:
  AbstractData * parameter_list;

  bool dispatcher;
};

} // namespace NVSHFR
#endif /* BIN_PARSER_HPP_ */
