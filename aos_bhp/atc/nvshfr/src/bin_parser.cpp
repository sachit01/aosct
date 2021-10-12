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
 * %name: bin_parser.cpp %
 * %version: 1.1.3 %
 * %created_by: rkongari %
 * %date_created: 2019-09-19 07:27 %
 *
 * Description: Source file for bin parser
 *
 ***************************************************************************/
/**************************************************************************
 * Revision History
 *
 * Version       Date      Sign      Change Description
 * 1.1.3         20190919  rkongari  Updated review comments
 * 1.1.2         20190916  rkongari  arn_043#6330 : Lint corrections
 * 1.1.1         20180322  anlindel  Corrected problem in calculating number of 2000 bytes blocks of data.
 * 1             20170613  ddigeraa  First version created
 *
 ***************************************************************************/

/**************************************************************************
 * Includes
 ***************************************************************************/
#include <iostream>
#include <sstream>
#include <stdexcept>

#include "bin_parser.hpp"
#include "stream_helper.hpp"
#include "app_error.hpp"
/**************************************************************************
 * Macro definitions
 ***************************************************************************/

/**************************************************************************
 * Typedefs
 ***************************************************************************/

/**************************************************************************
 * Data declarations/definitions
 ***************************************************************************/

/*! \brief Bin parser constructor
 *
 * Bin parser needs to take a parameter list for the data body.
 *
 * Future improvement: If other data structures needs to be supported,
 *                     consider making ParameterList subclass of "StructuredData"
 *                     virtual class. That way, interpretation methods could
 *                     be swapped without the need for adding functionality in
 *                     ParameterList
 *
 */
NVSHFR::BinParser::BinParser(AbstractData * const ParameterList, const bool dispatcher)
    :
      parameter_list(ParameterList)
{
  this->block_a.data = this->parameter_list;
  this->block_a.verify = false;

  this->block_b.data = this->parameter_list;
  this->block_b.verify = true;

  this->dispatcher = dispatcher;
}

NVSHFR::BinParser::~BinParser()
{
}

/*! \brief Parse binary istream with NVSH data
 *
 * The function will send the stream to A and B block respectively.
 *
 * \ret false if readin failed
 *
 */
bool NVSHFR::BinParser::parse(std::istream& s)
{
  bool status = true;
  if (s.eof() || s.fail())
  {
    throw std::runtime_error("End of stream before NVSH parsing.");
  }

  uint32_t const start_pos = static_cast<uint32_t>(s.tellg());

  if (!block_a.parse(s))
  {
    status = false;
  }

  if (dispatcher)
  {
    (void) s.seekg(static_cast<int64_t>(start_pos));
  }

  if (!block_b.parse(s))
  {
    status = false;
  }

  static_cast<void>(s.get());
  if (!s.eof())
  {
    throw std::runtime_error("The whole NVSH file was not read.");
  }

  return status;
}

/*! \brief Parse header data
 *
 * Parse header function reads an NVSH block header into a data_header struct.
 * The stream position will be kept before the NVSH block header when function returns.
 *
 */
bool NVSHFR::BinParser::data_header::parseHeader(std::istream& s)
{
  uint32_t const start_pos = static_cast<uint32_t>(s.tellg());
  this->interface_version = StreamHelper::getUint32(s);
  this->read_only_data = (StreamHelper::getUint32(s) != 0U) ? true : false;

  this->filename = StreamHelper::getString(s);
  this->unique_file_id = StreamHelper::getString(s);
  this->data_sequence_number = StreamHelper::getUint32(s);
  this->application_data_length = StreamHelper::getUint32(s);

  this->header_size = static_cast<uint32_t>((static_cast<uint32_t>(s.tellg())
                                             - start_pos));

  if (this->interface_version != 1U)
  {
    std::stringstream ss;
    ss << "Input NVSH file format version " << this->interface_version
       << " is not compatible with NVSHFR Expected version: "
       << 1;
    throw std::runtime_error(ss.str().c_str());
  }

  (void) s.seekg(static_cast<int64_t>(start_pos));
  return true;
}

/*! \brief Parse block
 *
 * Parse a NVSH file block into structures. The parsing will do sanity checks
 * as well as calculating and verify CRC64 sums for the block.
 *
 * If stream ends prematurely, or the CRC64 sum is incorrect, an exception
 * will be thrown.
 *
 * \ret false if data readin did not go well
 *
 *
 */
bool NVSHFR::BinParser::block::parse(std::istream& s)
{
  bool ret_val = true;
  /* Read length of complete NVSH data block. */
  this->length = StreamHelper::getUint32(s);

  /* Store position where this NVSH data block starts. */
  uint32_t const start_pos = static_cast<uint32_t>(s.tellg());

  /* The length is followed by the NVSH header. */
  (void) this->data_header.parseHeader(s);

  /* The CRC for each 'CRC64_BLOCK_SIZE' bytes data block is calculated over the NVSH header and application data.
   * Calculate here how many such blocks there will be. */
  {
    uint32_t const size_to_calculate_crc_over =
                                                static_cast<uint32_t>(this->data_header.header_size
                                                                      + this->data_header.application_data_length);
    this->numblocks = (size_to_calculate_crc_over / CRC64_BLOCK_SIZE)
                      + ((size_to_calculate_crc_over % CRC64_BLOCK_SIZE) ? 1 : 0);
  }

  (void) this->parseCRC(s); // Calculate CRC64 for the data + header block

  /* Set stream at position where the application data starts. */
  (void) s.seekg((start_pos + this->data_header.header_size));

  /* Data NULL guard */
  if (this->data == NULL)
  {
    throw std::runtime_error(
                             "Binary parser tries to parse into NULL AbstractData");
  }

  // Check that data header contains correct size

  uint32_t const start_of_data = static_cast<uint32_t>(s.tellg());
  if (this->verify)
  {
    if (!this->data->verifyValuesFromBinStream(s,
                                               this->data_header.application_data_length))
    {
      ret_val = false;
    }
  }
  else
  {
    if (!this->data->readValuesFromBinStream(s,
                                             this->data_header.application_data_length))
    {
      ret_val = false;
    }
  }

  if (ret_val == true)
  {
    uint32_t const read_data =
                               static_cast<uint32_t>(static_cast<uint32_t>(s.tellg())
                                                     - start_of_data);

    // Sanity checks for read data length
    if (read_data != this->data_header.application_data_length)
    {
      std::stringstream ss;
      ss << "Read " << read_data << " bytes, but expected "
         << this->data_header.application_data_length
         << " bytes";
      throw std::runtime_error(ss.str().c_str());
    }

    // Sanity check CRC64
    for (uint32_t i = 0U; i < this->numblocks; i++)
    {
      uint64_t const stored_crc64 = StreamHelper::getUint64(s);
      if (stored_crc64 != this->crc[i])
      {
        std::stringstream ss;
        ss << "Block " << i << " have incorrect CRC. From file: "
           << stored_crc64
           << " From calculation: "
           << this->crc[i];
        throw AppError(ss.str(), AppError::CRC64_ERROR);
      }
    }

    if (s.eof() || s.fail())
    {
      throw std::runtime_error(
                               "End of stream before NVSH block parsing.");
    }

  }

  return ret_val;
}

/*! \brief Calculate CRC64 sums for block
 *
 * A data block is divided into smaller 2000bytes CRC64 blocks for CRC64 calculation.
 * This is for SIL4 data integrity. This is a SIL0 offline tool, but we want to verify
 * the CRC none the less.
 *
 * parseCRC() will resize and populate the CRC64 list in its block struct.
 *
 */
bool NVSHFR::BinParser::block::parseCRC(std::istream& s)
{
  uint32_t const len_excluding_crc = (this->length
                                      - (this->numblocks * CRC64_TYPE_SIZE));
  uint32_t const startpos = static_cast<uint32_t>(s.tellg());

  this->crc.resize(this->numblocks);

  for (uint32_t i = 0U; i < this->numblocks; i++)
  {
    if ((i + 1U) == this->numblocks) /* Last CRC block */
    {
      this->crc[i] = StreamHelper::calculateCRC64(s,
                                                  len_excluding_crc % CRC64_BLOCK_SIZE,
                                                  false);
    }
    else
    {
      this->crc[i] = StreamHelper::calculateCRC64(s, CRC64_BLOCK_SIZE,
                                                  false);
    }
  }

  (void) s.seekg(static_cast<int64_t>(startpos));
  return true;
}
