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
 * %name: stream_helper.cpp %
 * %version: 1.1.2 %
 * %created_by: rkongari %
 * %date_created: 2019-09-19 10:50 %
 *
 * Description: Source file for Stream helper
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

/**************************************************************************
 * Includes
 ***************************************************************************/
#include <stdint.h>
#include <istream>
#include <iostream>
#include <string>
#include <exception>
#include <stdexcept>
#include <sstream>

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

NVSHFR::StreamHelper::StreamHelper()
{
}

NVSHFR::StreamHelper::~StreamHelper()
{
}

/*! \brief Return uint16_t from stream
 *
 * Return a uint16_t.
 * getUint16() will throw an exception if errors in instream are detected
 *
 */
uint16_t NVSHFR::StreamHelper::getUint16(std::istream& s)
{
  uint16_t temp;

  temp = static_cast<uint16_t>(s.get());
  temp <<= 8;
  temp |= (static_cast<uint16_t>(s.get()) & static_cast<uint16_t>(0x00FF));

  if (s.eof() || s.fail())
  {
    throw std::runtime_error("End of stream during read uint16");
  }
  return temp;
}

/*! \brief Return uint32_t from stream
 *
 * Return a uint32_t.
 * getUint32() will throw an exception if errors in instream are detected
 *
 */
uint32_t NVSHFR::StreamHelper::getUint32(std::istream& s)
{
  uint32_t temp;

  temp = (static_cast<uint32_t>(s.get()) << 24);
  temp |= (static_cast<uint32_t>(s.get()) << 16);
  temp |= (static_cast<uint32_t>(s.get()) << 8);
  temp |= (static_cast<uint32_t>(s.get()));

  if (s.eof() || s.fail())
  {
    throw std::runtime_error("End of stream during read uint32");
  }
  return temp;
}

/*! \brief Return uint64_t from stream
 *
 * Return a uint64_t.
 * getUint64() will throw an exception if errors in instream are detected
 *
 */
uint64_t NVSHFR::StreamHelper::getUint64(std::istream& s)
{
  //lint -save -e571 Need to cast to 64 bit in order to shift
  uint64_t temp;

  temp = (static_cast<uint64_t>(s.get()) << 56);
  temp |= (static_cast<uint64_t>(s.get()) << 48);
  temp |= (static_cast<uint64_t>(s.get()) << 40);
  temp |= (static_cast<uint64_t>(s.get()) << 32);
  temp |= (static_cast<uint64_t>(s.get()) << 24);
  temp |= (static_cast<uint64_t>(s.get()) << 16);
  temp |= (static_cast<uint64_t>(s.get()) << 8);
  temp |= (static_cast<uint64_t>(s.get()));

  //lint -restore
  if (s.eof() || s.fail())
  {
    throw std::runtime_error("End of stream during read uint64");
  }
  return temp;
}

/*! \brief read a strin from binary stream according to NVSH spec
 *
 * NVSH sepecifies a string as a 32 bit unsigned value indicating size
 * followed by a non null terminated string.
 *
 * getString() will create a string from such data.
 *
 * If an error in the stream is detected, the helper function will throw an exception.
 *
 */
std::string NVSHFR::StreamHelper::getString(std::istream& s)
{
  uint32_t str_len;

  // Get length
  str_len = getUint32(s);

  // fail if length is to large
  if (str_len > VFW_NVSH_FILENAME_MAX_LEN)
  {
    std::stringstream ss;
    ss << "String to long: " << str_len << " bytes but can be max "
       << VFW_NVSH_FILENAME_MAX_LEN
       << " bytes";
    throw std::runtime_error("String to long!");
  }

  // Read out string to buffer
  char c_str[str_len + 1U];

  for (uint32_t i = 0U; i < str_len; i++)
  {
    c_str[i] = static_cast<char>(s.get());
  }

  c_str[str_len] = '\0';
  if (s.eof() || s.fail())
  {
    throw std::runtime_error("End of stream during read string");
  }
  // Assign c str buffer to return string

  return std::string(c_str);
}

/*! \brief calculate CRC32 sum from a stream
 *
 * calculateCRC32() will return the CRC32 from instream to a given length.
 * If stream ends before set length, an exception will be thrown.
 *
 * If param s_rewind is set, calculateCRC32() will rewind the stream to the point
 * it got it.
 *
 * Polynomial used: x32+x30+x27+x25+x22+x20+x13+x12+x11+x10+x8+x7+x6+x5+x4+1
 *
 */
uint32_t NVSHFR::StreamHelper::calculateCRC32(std::istream& s,
                                              const uint32_t length,
                                              const bool s_rewind)
{
  static const uint32_t crc32Table[256] = { 0x00000000UL, 0x4a503df1UL,
      0x94a07be2UL, 0xdef04613UL, 0x6310ca35UL, 0x2940f7c4UL,
      0xf7b0b1d7UL, 0xbde08c26UL, 0xc621946aUL, 0x8c71a99bUL,
      0x5281ef88UL, 0x18d1d279UL, 0xa5315e5fUL, 0xef6163aeUL,
      0x319125bdUL, 0x7bc1184cUL, 0xc6131525UL, 0x8c4328d4UL,
      0x52b36ec7UL, 0x18e35336UL, 0xa503df10UL, 0xef53e2e1UL,
      0x31a3a4f2UL, 0x7bf39903UL, 0x0032814fUL, 0x4a62bcbeUL,
      0x9492faadUL, 0xdec2c75cUL, 0x63224b7aUL, 0x2972768bUL,
      0xf7823098UL, 0xbdd20d69UL, 0xc67617bbUL, 0x8c262a4aUL,
      0x52d66c59UL, 0x188651a8UL, 0xa566dd8eUL, 0xef36e07fUL,
      0x31c6a66cUL, 0x7b969b9dUL, 0x005783d1UL, 0x4a07be20UL,
      0x94f7f833UL, 0xdea7c5c2UL, 0x634749e4UL, 0x29177415UL,
      0xf7e73206UL, 0xbdb70ff7UL, 0x0065029eUL, 0x4a353f6fUL,
      0x94c5797cUL, 0xde95448dUL, 0x6375c8abUL, 0x2925f55aUL,
      0xf7d5b349UL, 0xbd858eb8UL, 0xc64496f4UL, 0x8c14ab05UL,
      0x52e4ed16UL, 0x18b4d0e7UL, 0xa5545cc1UL, 0xef046130UL,
      0x31f42723UL, 0x7ba41ad2UL, 0xc6bc1287UL, 0x8cec2f76UL,
      0x521c6965UL, 0x184c5494UL, 0xa5acd8b2UL, 0xeffce543UL,
      0x310ca350UL, 0x7b5c9ea1UL, 0x009d86edUL, 0x4acdbb1cUL,
      0x943dfd0fUL, 0xde6dc0feUL, 0x638d4cd8UL, 0x29dd7129UL,
      0xf72d373aUL, 0xbd7d0acbUL, 0x00af07a2UL, 0x4aff3a53UL,
      0x940f7c40UL, 0xde5f41b1UL, 0x63bfcd97UL, 0x29eff066UL,
      0xf71fb675UL, 0xbd4f8b84UL, 0xc68e93c8UL, 0x8cdeae39UL,
      0x522ee82aUL, 0x187ed5dbUL, 0xa59e59fdUL, 0xefce640cUL,
      0x313e221fUL, 0x7b6e1feeUL, 0x00ca053cUL, 0x4a9a38cdUL,
      0x946a7edeUL, 0xde3a432fUL, 0x63dacf09UL, 0x298af2f8UL,
      0xf77ab4ebUL, 0xbd2a891aUL, 0xc6eb9156UL, 0x8cbbaca7UL,
      0x524beab4UL, 0x181bd745UL, 0xa5fb5b63UL, 0xefab6692UL,
      0x315b2081UL, 0x7b0b1d70UL, 0xc6d91019UL, 0x8c892de8UL,
      0x52796bfbUL, 0x1829560aUL, 0xa5c9da2cUL, 0xef99e7ddUL,
      0x3169a1ceUL, 0x7b399c3fUL, 0x00f88473UL, 0x4aa8b982UL,
      0x9458ff91UL, 0xde08c260UL, 0x63e84e46UL, 0x29b873b7UL,
      0xf74835a4UL, 0xbd180855UL, 0xc72818ffUL, 0x8d78250eUL,
      0x5388631dUL, 0x19d85eecUL, 0xa438d2caUL, 0xee68ef3bUL,
      0x3098a928UL, 0x7ac894d9UL, 0x01098c95UL, 0x4b59b164UL,
      0x95a9f777UL, 0xdff9ca86UL, 0x621946a0UL, 0x28497b51UL,
      0xf6b93d42UL, 0xbce900b3UL, 0x013b0ddaUL, 0x4b6b302bUL,
      0x959b7638UL, 0xdfcb4bc9UL, 0x622bc7efUL, 0x287bfa1eUL,
      0xf68bbc0dUL, 0xbcdb81fcUL, 0xc71a99b0UL, 0x8d4aa441UL,
      0x53bae252UL, 0x19eadfa3UL, 0xa40a5385UL, 0xee5a6e74UL,
      0x30aa2867UL, 0x7afa1596UL, 0x015e0f44UL, 0x4b0e32b5UL,
      0x95fe74a6UL, 0xdfae4957UL, 0x624ec571UL, 0x281ef880UL,
      0xf6eebe93UL, 0xbcbe8362UL, 0xc77f9b2eUL, 0x8d2fa6dfUL,
      0x53dfe0ccUL, 0x198fdd3dUL, 0xa46f511bUL, 0xee3f6ceaUL,
      0x30cf2af9UL, 0x7a9f1708UL, 0xc74d1a61UL, 0x8d1d2790UL,
      0x53ed6183UL, 0x19bd5c72UL, 0xa45dd054UL, 0xee0deda5UL,
      0x30fdabb6UL, 0x7aad9647UL, 0x016c8e0bUL, 0x4b3cb3faUL,
      0x95ccf5e9UL, 0xdf9cc818UL, 0x627c443eUL, 0x282c79cfUL,
      0xf6dc3fdcUL, 0xbc8c022dUL, 0x01940a78UL, 0x4bc43789UL,
      0x9534719aUL, 0xdf644c6bUL, 0x6284c04dUL, 0x28d4fdbcUL,
      0xf624bbafUL, 0xbc74865eUL, 0xc7b59e12UL, 0x8de5a3e3UL,
      0x5315e5f0UL, 0x1945d801UL, 0xa4a55427UL, 0xeef569d6UL,
      0x30052fc5UL, 0x7a551234UL, 0xc7871f5dUL, 0x8dd722acUL,
      0x532764bfUL, 0x1977594eUL, 0xa497d568UL, 0xeec7e899UL,
      0x3037ae8aUL, 0x7a67937bUL, 0x01a68b37UL, 0x4bf6b6c6UL,
      0x9506f0d5UL, 0xdf56cd24UL, 0x62b64102UL, 0x28e67cf3UL,
      0xf6163ae0UL, 0xbc460711UL, 0xc7e21dc3UL, 0x8db22032UL,
      0x53426621UL, 0x19125bd0UL, 0xa4f2d7f6UL, 0xeea2ea07UL,
      0x3052ac14UL, 0x7a0291e5UL, 0x01c389a9UL, 0x4b93b458UL,
      0x9563f24bUL, 0xdf33cfbaUL, 0x62d3439cUL, 0x28837e6dUL,
      0xf673387eUL, 0xbc23058fUL, 0x01f108e6UL, 0x4ba13517UL,
      0x95517304UL, 0xdf014ef5UL, 0x62e1c2d3UL, 0x28b1ff22UL,
      0xf641b931UL, 0xbc1184c0UL, 0xc7d09c8cUL, 0x8d80a17dUL,
      0x5370e76eUL, 0x1920da9fUL, 0xa4c056b9UL, 0xee906b48UL,
      0x30602d5bUL, 0x7a3010aaUL };

  uint32_t crc32 = 0U;
  uint32_t nTableIndex = 0U;
  const std::streampos retpos = s.tellg();

  for (uint32_t i = 0U; i < length; i++)
  {
    //*iCRC = (*iCRC >> 8) ^ this->iTable[(*iCRC & 0xFF) ^ *sData++];
    nTableIndex = (static_cast<uint32_t>(crc32 >> 24U)
                   ^ static_cast<uint32_t>(s.get()));
    if (s.eof() || s.fail())
    {
      throw std::runtime_error("CRC read outside stream on CRC32");
    }
    crc32 = crc32 << 8U;
    crc32 = crc32 ^ crc32Table[nTableIndex];
  }

  if (s.eof() || s.fail())
  {
    throw std::runtime_error("Error after reading CRC");
  }
// Rewind to where we where
  if (s_rewind)
  {
    (void) s.seekg(retpos);
  }

  return crc32;
}

/*! \brief calculate CRC64 sum from a stream
 *
 * calculateCRC64() will return the CRC64 from instream to a given length.
 * If stream ends before set length, an exception will be thrown.
 *
 * If param s_rewind is set, calculateCRC64() will rewind the stream to the point
 * it got it.
 *
 * Polynomial used: 1+x+x2+x5+x6+x7+x9+x10+x11+x14+x21+x22+x26+x28+x29+x30+x32+x33+x35+x36+x37+x44+x46+x47+x52+x54+x55+x56+x60+x61+x62+x64
 *
 */
uint64_t NVSHFR::StreamHelper::calculateCRC64(std::istream& s,
                                              const uint32_t length,
                                              const bool s_rewind)
{
  static const uint64_t crc64Table[256] =
                                          { 0x0000000000000000ULL, 0x71d0d03b74604ee7ULL,
                                              0xe3a1a076e8c09dceULL, 0x9271704d9ca0d329ULL,
                                              0xb69390d6a5e1757bULL, 0xc74340edd1813b9cULL,
                                              0x553230a04d21e8b5ULL, 0x24e2e09b3941a652ULL,
                                              0x1cf7f1963fa2a411ULL, 0x6d2721ad4bc2eaf6ULL,
                                              0xff5651e0d76239dfULL, 0x8e8681dba3027738ULL,
                                              0xaa6461409a43d16aULL, 0xdbb4b17bee239f8dULL,
                                              0x49c5c13672834ca4ULL, 0x3815110d06e30243ULL,
                                              0x39efe32c7f454822ULL, 0x483f33170b2506c5ULL,
                                              0xda4e435a9785d5ecULL, 0xab9e9361e3e59b0bULL,
                                              0x8f7c73fadaa43d59ULL, 0xfeaca3c1aec473beULL,
                                              0x6cddd38c3264a097ULL, 0x1d0d03b74604ee70ULL,
                                              0x251812ba40e7ec33ULL, 0x54c8c2813487a2d4ULL,
                                              0xc6b9b2cca82771fdULL, 0xb76962f7dc473f1aULL,
                                              0x938b826ce5069948ULL, 0xe25b52579166d7afULL,
                                              0x702a221a0dc60486ULL, 0x01faf22179a64a61ULL,
                                              0x73dfc658fe8a9044ULL, 0x020f16638aeadea3ULL,
                                              0x907e662e164a0d8aULL, 0xe1aeb615622a436dULL,
                                              0xc54c568e5b6be53fULL, 0xb49c86b52f0babd8ULL,
                                              0x26edf6f8b3ab78f1ULL, 0x573d26c3c7cb3616ULL,
                                              0x6f2837cec1283455ULL, 0x1ef8e7f5b5487ab2ULL,
                                              0x8c8997b829e8a99bULL, 0xfd5947835d88e77cULL,
                                              0xd9bba71864c9412eULL, 0xa86b772310a90fc9ULL,
                                              0x3a1a076e8c09dce0ULL, 0x4bcad755f8699207ULL,
                                              0x4a30257481cfd866ULL, 0x3be0f54ff5af9681ULL,
                                              0xa9918502690f45a8ULL, 0xd84155391d6f0b4fULL,
                                              0xfca3b5a2242ead1dULL, 0x8d736599504ee3faULL,
                                              0x1f0215d4ccee30d3ULL, 0x6ed2c5efb88e7e34ULL,
                                              0x56c7d4e2be6d7c77ULL, 0x271704d9ca0d3290ULL,
                                              0xb566749456ade1b9ULL, 0xc4b6a4af22cdaf5eULL,
                                              0xe05444341b8c090cULL, 0x9184940f6fec47ebULL,
                                              0x03f5e442f34c94c2ULL, 0x72253479872cda25ULL,
                                              0xe7bf8cb1fd152088ULL, 0x966f5c8a89756e6fULL,
                                              0x041e2cc715d5bd46ULL, 0x75cefcfc61b5f3a1ULL,
                                              0x512c1c6758f455f3ULL, 0x20fccc5c2c941b14ULL,
                                              0xb28dbc11b034c83dULL, 0xc35d6c2ac45486daULL,
                                              0xfb487d27c2b78499ULL, 0x8a98ad1cb6d7ca7eULL,
                                              0x18e9dd512a771957ULL, 0x69390d6a5e1757b0ULL,
                                              0x4ddbedf16756f1e2ULL, 0x3c0b3dca1336bf05ULL,
                                              0xae7a4d878f966c2cULL, 0xdfaa9dbcfbf622cbULL,
                                              0xde506f9d825068aaULL, 0xaf80bfa6f630264dULL,
                                              0x3df1cfeb6a90f564ULL, 0x4c211fd01ef0bb83ULL,
                                              0x68c3ff4b27b11dd1ULL, 0x19132f7053d15336ULL,
                                              0x8b625f3dcf71801fULL, 0xfab28f06bb11cef8ULL,
                                              0xc2a79e0bbdf2ccbbULL, 0xb3774e30c992825cULL,
                                              0x21063e7d55325175ULL, 0x50d6ee4621521f92ULL,
                                              0x74340edd1813b9c0ULL, 0x05e4dee66c73f727ULL,
                                              0x9795aeabf0d3240eULL, 0xe6457e9084b36ae9ULL,
                                              0x94604ae9039fb0ccULL, 0xe5b09ad277fffe2bULL,
                                              0x77c1ea9feb5f2d02ULL, 0x06113aa49f3f63e5ULL,
                                              0x22f3da3fa67ec5b7ULL, 0x53230a04d21e8b50ULL,
                                              0xc1527a494ebe5879ULL, 0xb082aa723ade169eULL,
                                              0x8897bb7f3c3d14ddULL, 0xf9476b44485d5a3aULL,
                                              0x6b361b09d4fd8913ULL, 0x1ae6cb32a09dc7f4ULL,
                                              0x3e042ba999dc61a6ULL, 0x4fd4fb92edbc2f41ULL,
                                              0xdda58bdf711cfc68ULL, 0xac755be4057cb28fULL,
                                              0xad8fa9c57cdaf8eeULL, 0xdc5f79fe08bab609ULL,
                                              0x4e2e09b3941a6520ULL, 0x3ffed988e07a2bc7ULL,
                                              0x1b1c3913d93b8d95ULL, 0x6acce928ad5bc372ULL,
                                              0xf8bd996531fb105bULL, 0x896d495e459b5ebcULL,
                                              0xb178585343785cffULL, 0xc0a8886837181218ULL,
                                              0x52d9f825abb8c131ULL, 0x2309281edfd88fd6ULL,
                                              0x07ebc885e6992984ULL, 0x763b18be92f96763ULL,
                                              0xe44a68f30e59b44aULL, 0x959ab8c87a39faadULL,
                                              0xbeafc9588e4a0ff7ULL, 0xcf7f1963fa2a4110ULL,
                                              0x5d0e692e668a9239ULL, 0x2cdeb91512eadcdeULL,
                                              0x083c598e2bab7a8cULL, 0x79ec89b55fcb346bULL,
                                              0xeb9df9f8c36be742ULL, 0x9a4d29c3b70ba9a5ULL,
                                              0xa25838ceb1e8abe6ULL, 0xd388e8f5c588e501ULL,
                                              0x41f998b859283628ULL, 0x302948832d4878cfULL,
                                              0x14cba8181409de9dULL, 0x651b78236069907aULL,
                                              0xf76a086efcc94353ULL, 0x86bad85588a90db4ULL,
                                              0x87402a74f10f47d5ULL, 0xf690fa4f856f0932ULL,
                                              0x64e18a0219cfda1bULL, 0x15315a396daf94fcULL,
                                              0x31d3baa254ee32aeULL, 0x40036a99208e7c49ULL,
                                              0xd2721ad4bc2eaf60ULL, 0xa3a2caefc84ee187ULL,
                                              0x9bb7dbe2ceade3c4ULL, 0xea670bd9bacdad23ULL,
                                              0x78167b94266d7e0aULL, 0x09c6abaf520d30edULL,
                                              0x2d244b346b4c96bfULL, 0x5cf49b0f1f2cd858ULL,
                                              0xce85eb42838c0b71ULL, 0xbf553b79f7ec4596ULL,
                                              0xcd700f0070c09fb3ULL, 0xbca0df3b04a0d154ULL,
                                              0x2ed1af769800027dULL, 0x5f017f4dec604c9aULL,
                                              0x7be39fd6d521eac8ULL, 0x0a334feda141a42fULL,
                                              0x98423fa03de17706ULL, 0xe992ef9b498139e1ULL,
                                              0xd187fe964f623ba2ULL, 0xa0572ead3b027545ULL,
                                              0x32265ee0a7a2a66cULL, 0x43f68edbd3c2e88bULL,
                                              0x67146e40ea834ed9ULL, 0x16c4be7b9ee3003eULL,
                                              0x84b5ce360243d317ULL, 0xf5651e0d76239df0ULL,
                                              0xf49fec2c0f85d791ULL, 0x854f3c177be59976ULL,
                                              0x173e4c5ae7454a5fULL, 0x66ee9c61932504b8ULL,
                                              0x420c7cfaaa64a2eaULL, 0x33dcacc1de04ec0dULL,
                                              0xa1addc8c42a43f24ULL, 0xd07d0cb736c471c3ULL,
                                              0xe8681dba30277380ULL, 0x99b8cd8144473d67ULL,
                                              0x0bc9bdccd8e7ee4eULL, 0x7a196df7ac87a0a9ULL,
                                              0x5efb8d6c95c606fbULL, 0x2f2b5d57e1a6481cULL,
                                              0xbd5a2d1a7d069b35ULL, 0xcc8afd210966d5d2ULL,
                                              0x591045e9735f2f7fULL, 0x28c095d2073f6198ULL,
                                              0xbab1e59f9b9fb2b1ULL, 0xcb6135a4effffc56ULL,
                                              0xef83d53fd6be5a04ULL, 0x9e530504a2de14e3ULL,
                                              0x0c2275493e7ec7caULL, 0x7df2a5724a1e892dULL,
                                              0x45e7b47f4cfd8b6eULL, 0x34376444389dc589ULL,
                                              0xa6461409a43d16a0ULL, 0xd796c432d05d5847ULL,
                                              0xf37424a9e91cfe15ULL, 0x82a4f4929d7cb0f2ULL,
                                              0x10d584df01dc63dbULL, 0x610554e475bc2d3cULL,
                                              0x60ffa6c50c1a675dULL, 0x112f76fe787a29baULL,
                                              0x835e06b3e4dafa93ULL, 0xf28ed68890bab474ULL,
                                              0xd66c3613a9fb1226ULL, 0xa7bce628dd9b5cc1ULL,
                                              0x35cd9665413b8fe8ULL, 0x441d465e355bc10fULL,
                                              0x7c08575333b8c34cULL, 0x0dd8876847d88dabULL,
                                              0x9fa9f725db785e82ULL, 0xee79271eaf181065ULL,
                                              0xca9bc7859659b637ULL, 0xbb4b17bee239f8d0ULL,
                                              0x293a67f37e992bf9ULL, 0x58eab7c80af9651eULL,
                                              0x2acf83b18dd5bf3bULL, 0x5b1f538af9b5f1dcULL,
                                              0xc96e23c7651522f5ULL, 0xb8bef3fc11756c12ULL,
                                              0x9c5c13672834ca40ULL, 0xed8cc35c5c5484a7ULL,
                                              0x7ffdb311c0f4578eULL, 0x0e2d632ab4941969ULL,
                                              0x36387227b2771b2aULL, 0x47e8a21cc61755cdULL,
                                              0xd599d2515ab786e4ULL, 0xa449026a2ed7c803ULL,
                                              0x80abe2f117966e51ULL, 0xf17b32ca63f620b6ULL,
                                              0x630a4287ff56f39fULL, 0x12da92bc8b36bd78ULL,
                                              0x1320609df290f719ULL, 0x62f0b0a686f0b9feULL,
                                              0xf081c0eb1a506ad7ULL, 0x815110d06e302430ULL,
                                              0xa5b3f04b57718262ULL, 0xd46320702311cc85ULL,
                                              0x4612503dbfb11facULL, 0x37c28006cbd1514bULL,
                                              0x0fd7910bcd325308ULL, 0x7e074130b9521defULL,
                                              0xec76317d25f2cec6ULL, 0x9da6e14651928021ULL,
                                              0xb94401dd68d32673ULL, 0xc894d1e61cb36894ULL,
                                              0x5ae5a1ab8013bbbdULL, 0x2b357190f473f55aULL };

  uint64_t crc64 = 0U;
  uint32_t nTableIndex = 0U;
  const uint32_t retpos = static_cast<uint32_t>(s.tellg());

  for (uint32_t i = 0U; i < length; i++)
  {
    nTableIndex = static_cast<uint32_t>(static_cast<uint32_t>(crc64 >> 56U)
                                        ^ (static_cast<uint32_t>(s.get())));
    if (s.eof() || s.fail())
    {
      std::stringstream error;
      error << "CRC read outside stream on CRC64 at: " << i
            << " requested length: "
            << length << " rdstate: "
            << static_cast<uint32_t>(s.rdstate())
            << " stream pos: "
            << s.tellg();
      throw std::runtime_error(error.str().c_str());
    }
    crc64 = crc64 << 8U;
    crc64 = crc64 ^ crc64Table[nTableIndex];
  }

  if (s.eof() || s.fail())
  {
    throw std::runtime_error("Error after reading CRC");
  }

  // Rewind to where we where
  if (s_rewind)
  {
    (void) s.seekg(static_cast<int64_t>(retpos));
  }

  return crc64;
}
