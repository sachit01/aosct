/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*
*  DESCRIPTION:
*
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2015-11-13    bhermans    Created
* 2016-03-01    bhermans    Removed AdaptedRadioChannel
* 2016-03-03    bhermans    Introduced namespace RadioCom
* 2016-03-16    bhermans    ATC::STX, vfw_TRUE
* 2016-04-19    lantback    init to return bool
* 2016-04-22    lantback    Added component type
* 2016-06-09    akushwah    Radio Channel implementation
* 2016-06-13    akushwah    Updated after review comments
* 2016-06-16    akushwah    Added Access function getChannelId()
* 2016-06-17    akushwah    Moved the Timer Creation function to initialization
* 2016-07-05    spandita    updated the condition for radio  channels3
* 2016-07-05    spandita    updated the review comment for if and else statement
* 2016-08-04    adgupta     Updated to fix Integration with TCC Sim issues
* 2016-10-10    spandita    Bugfix for Tref time for filter issue in TCC sim
* 2017-01-27    rquensel    Changed to use CrossCompareOutputChannel
* 2017-03-17    marlundg    New CRC64 calculation
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_identity.h>
#include <vfwChannel.h>
#include <vfw_timer.h>
#include <vfw_string.h>
#include "atc_util.hpp"
#include "atc_math.hpp"
#include "radio_channel.hpp"
#include "abstract_event_handler.hpp"
#include "abstract_config.hpp"
#include "abstract_console.hpp"
#include "abstract_cross_compare.hpp"
#include "channel_config.hpp"
#include "cross_compare_array.hpp"
#include "cross_compare_complex.hpp"
#include "dmi_event_codes.hpp"
#include <vfw_checkpoints.h>
#ifndef __GNUG__
extern "C" int64_t vfwGetReferenceTime(void);
#else
#include <cstdio>
#include <vfw_time.h>
#endif


/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/


/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/


namespace ATP
{
  namespace RadioCom
  {

    /** Lint doesn't allow constant zero to be used as left/right parameter for << or +, enum is allowed.
    */
    enum SpecialValueZero {
      zeroValue = 0x00000000UL
    };

    /** Table used for crc-calculation in calculateCRC() method.
    *   Target does not support 'ULL', therefore a workaround with shift operation.
    */
    static const uint64_t crcTable[256] = {
      /*(zeroValue << 32) + */  static_cast<uint32_t>(zeroValue),(static_cast<uint64_t>(0x843EED14UL) << 32) + 0xD357E50DUL,
      (static_cast<uint64_t>(0x8C43373DUL) << 32) + 0x75F82F17UL,(static_cast<uint64_t>(0x087DDA29UL) << 32) + 0xA6AFCA1AUL,
      (static_cast<uint64_t>(0x9CB8836EUL) << 32) + 0x38A7BB23UL,(static_cast<uint64_t>(0x18866E7AUL) << 32) + 0xEBF05E2EUL,
      (static_cast<uint64_t>(0x10FBB453UL) << 32) + 0x4D5F9434UL,(static_cast<uint64_t>(0x94C55947UL) << 32) + 0x9E087139UL,
      (static_cast<uint64_t>(0xBD4FEBC8UL) << 32) + 0xA218934BUL,(static_cast<uint64_t>(0x397106DCUL) << 32) + 0x714F7646UL,
      (static_cast<uint64_t>(0x310CDCF5UL) << 32) + 0xD7E0BC5CUL,(static_cast<uint64_t>(0xB53231E1UL) << 32) + 0x04B75951UL,
      (static_cast<uint64_t>(0x21F768A6UL) << 32) + 0x9ABF2868UL,(static_cast<uint64_t>(0xA5C985B2UL) << 32) + 0x49E8CD65UL,
      (static_cast<uint64_t>(0xADB45F9BUL) << 32) + 0xEF47077FUL,(static_cast<uint64_t>(0x298AB28FUL) << 32) + 0x3C10E272UL,
      (static_cast<uint64_t>(0xFEA13A85UL) << 32) + 0x9766C39BUL,(static_cast<uint64_t>(0x7A9FD791UL) << 32) + 0x44312696UL,
      (static_cast<uint64_t>(0x72E20DB8UL) << 32) + 0xE29EEC8CUL,(static_cast<uint64_t>(0xF6DCE0ACUL) << 32) + 0x31C90981UL,
      (static_cast<uint64_t>(0x6219B9EBUL) << 32) + 0xAFC178B8UL,(static_cast<uint64_t>(0xE62754FFUL) << 32) + 0x7C969DB5UL,
      (static_cast<uint64_t>(0xEE5A8ED6UL) << 32) + 0xDA3957AFUL,(static_cast<uint64_t>(0x6A6463C2UL) << 32) + 0x096EB2A2UL,
      (static_cast<uint64_t>(0x43EED14DUL) << 32) + 0x357E50D0UL,(static_cast<uint64_t>(0xC7D03C59UL) << 32) + 0xE629B5DDUL,
      (static_cast<uint64_t>(0xCFADE670UL) << 32) + 0x40867FC7UL,(static_cast<uint64_t>(0x4B930B64UL) << 32) + 0x93D19ACAUL,
      (static_cast<uint64_t>(0xDF565223UL) << 32) + 0x0DD9EBF3UL,(static_cast<uint64_t>(0x5B68BF37UL) << 32) + 0xDE8E0EFEUL,
      (static_cast<uint64_t>(0x5315651EUL) << 32) + 0x7821C4E4UL,(static_cast<uint64_t>(0xD72B880AUL) << 32) + 0xAB7621E9UL,
      (static_cast<uint64_t>(0x797C981FUL) << 32) + 0xFD9A623BUL,(static_cast<uint64_t>(0xFD42750BUL) << 32) + 0x2ECD8736UL,
      (static_cast<uint64_t>(0xF53FAF22UL) << 32) + 0x88624D2CUL,(static_cast<uint64_t>(0x71014236UL) << 32) + 0x5B35A821UL,
      (static_cast<uint64_t>(0xE5C41B71UL) << 32) + 0xC53DD918UL,(static_cast<uint64_t>(0x61FAF665UL) << 32) + 0x166A3C15UL,
      (static_cast<uint64_t>(0x69872C4CUL) << 32) + 0xB0C5F60FUL,(static_cast<uint64_t>(0xEDB9C158UL) << 32) + 0x63921302UL,
      (static_cast<uint64_t>(0xC43373D7UL) << 32) + 0x5F82F170UL,(static_cast<uint64_t>(0x400D9EC3UL) << 32) + 0x8CD5147DUL,
      (static_cast<uint64_t>(0x487044EAUL) << 32) + 0x2A7ADE67UL,(static_cast<uint64_t>(0xCC4EA9FEUL) << 32) + 0xF92D3B6AUL,
      (static_cast<uint64_t>(0x588BF0B9UL) << 32) + 0x67254A53UL,(static_cast<uint64_t>(0xDCB51DADUL) << 32) + 0xB472AF5EUL,
      (static_cast<uint64_t>(0xD4C8C784UL) << 32) + 0x12DD6544UL,(static_cast<uint64_t>(0x50F62A90UL) << 32) + 0xC18A8049UL,
      (static_cast<uint64_t>(0x87DDA29AUL) << 32) + 0x6AFCA1A0UL,(static_cast<uint64_t>(0x03E34F8EUL) << 32) + 0xB9AB44ADUL,
      (static_cast<uint64_t>(0x0B9E95A7UL) << 32) + 0x1F048EB7UL,(static_cast<uint64_t>(0x8FA078B3UL) << 32) + 0xCC536BBAUL,
      (static_cast<uint64_t>(0x1B6521F4UL) << 32) + 0x525B1A83UL,(static_cast<uint64_t>(0x9F5BCCE0UL) << 32) + 0x810CFF8EUL,
      (static_cast<uint64_t>(0x972616C9UL) << 32) + 0x27A33594UL,(static_cast<uint64_t>(0x1318FBDDUL) << 32) + 0xF4F4D099UL,
      (static_cast<uint64_t>(0x3A924952UL) << 32) + 0xC8E432EBUL,(static_cast<uint64_t>(0xBEACA446UL) << 32) + 0x1BB3D7E6UL,
      (static_cast<uint64_t>(0xB6D17E6FUL) << 32) + 0xBD1C1DFCUL,(static_cast<uint64_t>(0x32EF937BUL) << 32) + 0x6E4BF8F1UL,
      (static_cast<uint64_t>(0xA62ACA3CUL) << 32) + 0xF04389C8UL,(static_cast<uint64_t>(0x22142728UL) << 32) + 0x23146CC5UL,
      (static_cast<uint64_t>(0x2A69FD01UL) << 32) + 0x85BBA6DFUL,(static_cast<uint64_t>(0xAE571015UL) << 32) + 0x56EC43D2UL,
      (static_cast<uint64_t>(0xF2F9303FUL) << 32) + 0xFB34C476UL,(static_cast<uint64_t>(0x76C7DD2BUL) << 32) + 0x2863217BUL,
      (static_cast<uint64_t>(0x7EBA0702UL) << 32) + 0x8ECCEB61UL,(static_cast<uint64_t>(0xFA84EA16UL) << 32) + 0x5D9B0E6CUL,
      (static_cast<uint64_t>(0x6E41B351UL) << 32) + 0xC3937F55UL,(static_cast<uint64_t>(0xEA7F5E45UL) << 32) + 0x10C49A58UL,
      (static_cast<uint64_t>(0xE202846CUL) << 32) + 0xB66B5042UL,(static_cast<uint64_t>(0x663C6978UL) << 32) + 0x653CB54FUL,
      (static_cast<uint64_t>(0x4FB6DBF7UL) << 32) + 0x592C573DUL,(static_cast<uint64_t>(0xCB8836E3UL) << 32) + 0x8A7BB230UL,
      (static_cast<uint64_t>(0xC3F5ECCAUL) << 32) + 0x2CD4782AUL,(static_cast<uint64_t>(0x47CB01DEUL) << 32) + 0xFF839D27UL,
      (static_cast<uint64_t>(0xD30E5899UL) << 32) + 0x618BEC1EUL,(static_cast<uint64_t>(0x5730B58DUL) << 32) + 0xB2DC0913UL,
      (static_cast<uint64_t>(0x5F4D6FA4UL) << 32) + 0x1473C309UL,(static_cast<uint64_t>(0xDB7382B0UL) << 32) + 0xC7242604UL,
      (static_cast<uint64_t>(0x0C580ABAUL) << 32) + 0x6C5207EDUL,(static_cast<uint64_t>(0x8866E7AEUL) << 32) + 0xBF05E2E0UL,
      (static_cast<uint64_t>(0x801B3D87UL) << 32) + 0x19AA28FAUL,(static_cast<uint64_t>(0x0425D093UL) << 32) + 0xCAFDCDF7UL,
      (static_cast<uint64_t>(0x90E089D4UL) << 32) + 0x54F5BCCEUL,(static_cast<uint64_t>(0x14DE64C0UL) << 32) + 0x87A259C3UL,
      (static_cast<uint64_t>(0x1CA3BEE9UL) << 32) + 0x210D93D9UL,(static_cast<uint64_t>(0x989D53FDUL) << 32) + 0xF25A76D4UL,
      (static_cast<uint64_t>(0xB117E172UL) << 32) + 0xCE4A94A6UL,(static_cast<uint64_t>(0x35290C66UL) << 32) + 0x1D1D71ABUL,
      (static_cast<uint64_t>(0x3D54D64FUL) << 32) + 0xBBB2BBB1UL,(static_cast<uint64_t>(0xB96A3B5BUL) << 32) + 0x68E55EBCUL,
      (static_cast<uint64_t>(0x2DAF621CUL) << 32) + 0xF6ED2F85UL,(static_cast<uint64_t>(0xA9918F08UL) << 32) + 0x25BACA88UL,
      (static_cast<uint64_t>(0xA1EC5521UL) << 32) + 0x83150092UL,(static_cast<uint64_t>(0x25D2B835UL) << 32) + 0x5042E59FUL,
      (static_cast<uint64_t>(0x8B85A820UL) << 32) + 0x06AEA64DUL,(static_cast<uint64_t>(0x0FBB4534UL) << 32) + 0xD5F94340UL,
      (static_cast<uint64_t>(0x07C69F1DUL) << 32) + 0x7356895AUL,(static_cast<uint64_t>(0x83F87209UL) << 32) + 0xA0016C57UL,
      (static_cast<uint64_t>(0x173D2B4EUL) << 32) + 0x3E091D6EUL,(static_cast<uint64_t>(0x9303C65AUL) << 32) + 0xED5EF863UL,
      (static_cast<uint64_t>(0x9B7E1C73UL) << 32) + 0x4BF13279UL,(static_cast<uint64_t>(0x1F40F167UL) << 32) + 0x98A6D774UL,
      (static_cast<uint64_t>(0x36CA43E8UL) << 32) + 0xA4B63506UL,(static_cast<uint64_t>(0xB2F4AEFCUL) << 32) + 0x77E1D00BUL,
      (static_cast<uint64_t>(0xBA8974D5UL) << 32) + 0xD14E1A11UL,(static_cast<uint64_t>(0x3EB799C1UL) << 32) + 0x0219FF1CUL,
      (static_cast<uint64_t>(0xAA72C086UL) << 32) + 0x9C118E25UL,(static_cast<uint64_t>(0x2E4C2D92UL) << 32) + 0x4F466B28UL,
      (static_cast<uint64_t>(0x2631F7BBUL) << 32) + 0xE9E9A132UL,(static_cast<uint64_t>(0xA20F1AAFUL) << 32) + 0x3ABE443FUL,
      (static_cast<uint64_t>(0x752492A5UL) << 32) + 0x91C865D6UL,(static_cast<uint64_t>(0xF11A7FB1UL) << 32) + 0x429F80DBUL,
      (static_cast<uint64_t>(0xF967A598UL) << 32) + 0xE4304AC1UL,(static_cast<uint64_t>(0x7D59488CUL) << 32) + 0x3767AFCCUL,
      (static_cast<uint64_t>(0xE99C11CBUL) << 32) + 0xA96FDEF5UL,(static_cast<uint64_t>(0x6DA2FCDFUL) << 32) + 0x7A383BF8UL,
      (static_cast<uint64_t>(0x65DF26F6UL) << 32) + 0xDC97F1E2UL,(static_cast<uint64_t>(0xE1E1CBE2UL) << 32) + 0x0FC014EFUL,
      (static_cast<uint64_t>(0xC86B796DUL) << 32) + 0x33D0F69DUL,(static_cast<uint64_t>(0x4C559479UL) << 32) + 0xE0871390UL,
      (static_cast<uint64_t>(0x44284E50UL) << 32) + 0x4628D98AUL,(static_cast<uint64_t>(0xC016A344UL) << 32) + 0x957F3C87UL,
      (static_cast<uint64_t>(0x54D3FA03UL) << 32) + 0x0B774DBEUL,(static_cast<uint64_t>(0xD0ED1717UL) << 32) + 0xD820A8B3UL,
      (static_cast<uint64_t>(0xD890CD3EUL) << 32) + 0x7E8F62A9UL,(static_cast<uint64_t>(0x5CAE202AUL) << 32) + 0xADD887A4UL,
      (static_cast<uint64_t>(0x61CC8D6BUL) << 32) + 0x253E6DE1UL,(static_cast<uint64_t>(0xE5F2607FUL) << 32) + 0xF66988ECUL,
      (static_cast<uint64_t>(0xED8FBA56UL) << 32) + 0x50C642F6UL,(static_cast<uint64_t>(0x69B15742UL) << 32) + 0x8391A7FBUL,
      (static_cast<uint64_t>(0xFD740E05UL) << 32) + 0x1D99D6C2UL,(static_cast<uint64_t>(0x794AE311UL) << 32) + 0xCECE33CFUL,
      (static_cast<uint64_t>(0x71373938UL) << 32) + 0x6861F9D5UL,(static_cast<uint64_t>(0xF509D42CUL) << 32) + 0xBB361CD8UL,
      (static_cast<uint64_t>(0xDC8366A3UL) << 32) + 0x8726FEAAUL,(static_cast<uint64_t>(0x58BD8BB7UL) << 32) + 0x54711BA7UL,
      (static_cast<uint64_t>(0x50C0519EUL) << 32) + 0xF2DED1BDUL,(static_cast<uint64_t>(0xD4FEBC8AUL) << 32) + 0x218934B0UL,
      (static_cast<uint64_t>(0x403BE5CDUL) << 32) + 0xBF814589UL,(static_cast<uint64_t>(0xC40508D9UL) << 32) + 0x6CD6A084UL,
      (static_cast<uint64_t>(0xCC78D2F0UL) << 32) + 0xCA796A9EUL,(static_cast<uint64_t>(0x48463FE4UL) << 32) + 0x192E8F93UL,
      (static_cast<uint64_t>(0x9F6DB7EEUL) << 32) + 0xB258AE7AUL,(static_cast<uint64_t>(0x1B535AFAUL) << 32) + 0x610F4B77UL,
      (static_cast<uint64_t>(0x132E80D3UL) << 32) + 0xC7A0816DUL,(static_cast<uint64_t>(0x97106DC7UL) << 32) + 0x14F76460UL,
      (static_cast<uint64_t>(0x03D53480UL) << 32) + 0x8AFF1559UL,(static_cast<uint64_t>(0x87EBD994UL) << 32) + 0x59A8F054UL,
      (static_cast<uint64_t>(0x8F9603BDUL) << 32) + 0xFF073A4EUL,(static_cast<uint64_t>(0x0BA8EEA9UL) << 32) + 0x2C50DF43UL,
      (static_cast<uint64_t>(0x22225C26UL) << 32) + 0x10403D31UL,(static_cast<uint64_t>(0xA61CB132UL) << 32) + 0xC317D83CUL,
      (static_cast<uint64_t>(0xAE616B1BUL) << 32) + 0x65B81226UL,(static_cast<uint64_t>(0x2A5F860FUL) << 32) + 0xB6EFF72BUL,
      (static_cast<uint64_t>(0xBE9ADF48UL) << 32) + 0x28E78612UL,(static_cast<uint64_t>(0x3AA4325CUL) << 32) + 0xFBB0631FUL,
      (static_cast<uint64_t>(0x32D9E875UL) << 32) + 0x5D1FA905UL,(static_cast<uint64_t>(0xB6E70561UL) << 32) + 0x8E484C08UL,
      (static_cast<uint64_t>(0x18B01574UL) << 32) + 0xD8A40FDAUL,(static_cast<uint64_t>(0x9C8EF860UL) << 32) + 0x0BF3EAD7UL,
      (static_cast<uint64_t>(0x94F32249UL) << 32) + 0xAD5C20CDUL,(static_cast<uint64_t>(0x10CDCF5DUL) << 32) + 0x7E0BC5C0UL,
      (static_cast<uint64_t>(0x8408961AUL) << 32) + 0xE003B4F9UL,(static_cast<uint64_t>(0x00367B0EUL) << 32) + 0x335451F4UL,
      (static_cast<uint64_t>(0x084BA127UL) << 32) + 0x95FB9BEEUL,(static_cast<uint64_t>(0x8C754C33UL) << 32) + 0x46AC7EE3UL,
      (static_cast<uint64_t>(0xA5FFFEBCUL) << 32) + 0x7ABC9C91UL,(static_cast<uint64_t>(0x21C113A8UL) << 32) + 0xA9EB799CUL,
      (static_cast<uint64_t>(0x29BCC981UL) << 32) + 0x0F44B386UL,(static_cast<uint64_t>(0xAD822495UL) << 32) + 0xDC13568BUL,
      (static_cast<uint64_t>(0x39477DD2UL) << 32) + 0x421B27B2UL,(static_cast<uint64_t>(0xBD7990C6UL) << 32) + 0x914CC2BFUL,
      (static_cast<uint64_t>(0xB5044AEFUL) << 32) + 0x37E308A5UL,(static_cast<uint64_t>(0x313AA7FBUL) << 32) + 0xE4B4EDA8UL,
      (static_cast<uint64_t>(0xE6112FF1UL) << 32) + 0x4FC2CC41UL,(static_cast<uint64_t>(0x622FC2E5UL) << 32) + 0x9C95294CUL,
      (static_cast<uint64_t>(0x6A5218CCUL) << 32) + 0x3A3AE356UL,(static_cast<uint64_t>(0xEE6CF5D8UL) << 32) + 0xE96D065BUL,
      (static_cast<uint64_t>(0x7AA9AC9FUL) << 32) + 0x77657762UL,(static_cast<uint64_t>(0xFE97418BUL) << 32) + 0xA432926FUL,
      (static_cast<uint64_t>(0xF6EA9BA2UL) << 32) + 0x029D5875UL,(static_cast<uint64_t>(0x72D476B6UL) << 32) + 0xD1CABD78UL,
      (static_cast<uint64_t>(0x5B5EC439UL) << 32) + 0xEDDA5F0AUL,(static_cast<uint64_t>(0xDF60292DUL) << 32) + 0x3E8DBA07UL,
      (static_cast<uint64_t>(0xD71DF304UL) << 32) + 0x9822701DUL,(static_cast<uint64_t>(0x53231E10UL) << 32) + 0x4B759510UL,
      (static_cast<uint64_t>(0xC7E64757UL) << 32) + 0xD57DE429UL,(static_cast<uint64_t>(0x43D8AA43UL) << 32) + 0x062A0124UL,
      (static_cast<uint64_t>(0x4BA5706AUL) << 32) + 0xA085CB3EUL,(static_cast<uint64_t>(0xCF9B9D7EUL) << 32) + 0x73D22E33UL,
      (static_cast<uint64_t>(0x9335BD54UL) << 32) + 0xDE0AA997UL,(static_cast<uint64_t>(0x170B5040UL) << 32) + 0x0D5D4C9AUL,
      (static_cast<uint64_t>(0x1F768A69UL) << 32) + 0xABF28680UL,(static_cast<uint64_t>(0x9B48677DUL) << 32) + 0x78A5638DUL,
      (static_cast<uint64_t>(0x0F8D3E3AUL) << 32) + 0xE6AD12B4UL,(static_cast<uint64_t>(0x8BB3D32EUL) << 32) + 0x35FAF7B9UL,
      (static_cast<uint64_t>(0x83CE0907UL) << 32) + 0x93553DA3UL,(static_cast<uint64_t>(0x07F0E413UL) << 32) + 0x4002D8AEUL,
      (static_cast<uint64_t>(0x2E7A569CUL) << 32) + 0x7C123ADCUL,(static_cast<uint64_t>(0xAA44BB88UL) << 32) + 0xAF45DFD1UL,
      (static_cast<uint64_t>(0xA23961A1UL) << 32) + 0x09EA15CBUL,(static_cast<uint64_t>(0x26078CB5UL) << 32) + 0xDABDF0C6UL,
      (static_cast<uint64_t>(0xB2C2D5F2UL) << 32) + 0x44B581FFUL,(static_cast<uint64_t>(0x36FC38E6UL) << 32) + 0x97E264F2UL,
      (static_cast<uint64_t>(0x3E81E2CFUL) << 32) + 0x314DAEE8UL,(static_cast<uint64_t>(0xBABF0FDBUL) << 32) + 0xE21A4BE5UL,
      (static_cast<uint64_t>(0x6D9487D1UL) << 32) + 0x496C6A0CUL,(static_cast<uint64_t>(0xE9AA6AC5UL) << 32) + 0x9A3B8F01UL,
      (static_cast<uint64_t>(0xE1D7B0ECUL) << 32) + 0x3C94451BUL,(static_cast<uint64_t>(0x65E95DF8UL) << 32) + 0xEFC3A016UL,
      (static_cast<uint64_t>(0xF12C04BFUL) << 32) + 0x71CBD12FUL,(static_cast<uint64_t>(0x7512E9ABUL) << 32) + 0xA29C3422UL,
      (static_cast<uint64_t>(0x7D6F3382UL) << 32) + 0x0433FE38UL,(static_cast<uint64_t>(0xF951DE96UL) << 32) + 0xD7641B35UL,
      (static_cast<uint64_t>(0xD0DB6C19UL) << 32) + 0xEB74F947UL,(static_cast<uint64_t>(0x54E5810DUL) << 32) + 0x38231C4AUL,
      (static_cast<uint64_t>(0x5C985B24UL) << 32) + 0x9E8CD650UL,(static_cast<uint64_t>(0xD8A6B630UL) << 32) + 0x4DDB335DUL,
      (static_cast<uint64_t>(0x4C63EF77UL) << 32) + 0xD3D34264UL,(static_cast<uint64_t>(0xC85D0263UL) << 32) + 0x0084A769UL,
      (static_cast<uint64_t>(0xC020D84AUL) << 32) + 0xA62B6D73UL,(static_cast<uint64_t>(0x441E355EUL) << 32) + 0x757C887EUL,
      (static_cast<uint64_t>(0xEA49254BUL) << 32) + 0x2390CBACUL,(static_cast<uint64_t>(0x6E77C85FUL) << 32) + 0xF0C72EA1UL,
      (static_cast<uint64_t>(0x660A1276UL) << 32) + 0x5668E4BBUL,(static_cast<uint64_t>(0xE234FF62UL) << 32) + 0x853F01B6UL,
      (static_cast<uint64_t>(0x76F1A625UL) << 32) + 0x1B37708FUL,(static_cast<uint64_t>(0xF2CF4B31UL) << 32) + 0xC8609582UL,
      (static_cast<uint64_t>(0xFAB29118UL) << 32) + 0x6ECF5F98UL,(static_cast<uint64_t>(0x7E8C7C0CUL) << 32) + 0xBD98BA95UL,
      (static_cast<uint64_t>(0x5706CE83UL) << 32) + 0x818858E7UL,(static_cast<uint64_t>(0xD3382397UL) << 32) + 0x52DFBDEAUL,
      (static_cast<uint64_t>(0xDB45F9BEUL) << 32) + 0xF47077F0UL,(static_cast<uint64_t>(0x5F7B14AAUL) << 32) + 0x272792FDUL,
      (static_cast<uint64_t>(0xCBBE4DEDUL) << 32) + 0xB92FE3C4UL,(static_cast<uint64_t>(0x4F80A0F9UL) << 32) + 0x6A7806C9UL,
      (static_cast<uint64_t>(0x47FD7AD0UL) << 32) + 0xCCD7CCD3UL,(static_cast<uint64_t>(0xC3C397C4UL) << 32) + 0x1F8029DEUL,
      (static_cast<uint64_t>(0x14E81FCEUL) << 32) + 0xB4F60837UL,(static_cast<uint64_t>(0x90D6F2DAUL) << 32) + 0x67A1ED3AUL,
      (static_cast<uint64_t>(0x98AB28F3UL) << 32) + 0xC10E2720UL,(static_cast<uint64_t>(0x1C95C5E7UL) << 32) + 0x1259C22DUL,
      (static_cast<uint64_t>(0x88509CA0UL) << 32) + 0x8C51B314UL,(static_cast<uint64_t>(0x0C6E71B4UL) << 32) + 0x5F065619UL,
      (static_cast<uint64_t>(0x0413AB9DUL) << 32) + 0xF9A99C03UL,(static_cast<uint64_t>(0x802D4689UL) << 32) + 0x2AFE790EUL,
      (static_cast<uint64_t>(0xA9A7F406UL) << 32) + 0x16EE9B7CUL,(static_cast<uint64_t>(0x2D991912UL) << 32) + 0xC5B97E71UL,
      (static_cast<uint64_t>(0x25E4C33BUL) << 32) + 0x6316B46BUL,(static_cast<uint64_t>(0xA1DA2E2FUL) << 32) + 0xB0415166UL,
      (static_cast<uint64_t>(0x351F7768UL) << 32) + 0x2E49205FUL,(static_cast<uint64_t>(0xB1219A7CUL) << 32) + 0xFD1EC552UL,
      (static_cast<uint64_t>(0xB95C4055UL) << 32) + 0x5BB10F48UL,(static_cast<uint64_t>(0x3D62AD41UL) << 32) + 0x88E6EA45UL
    };

    /******************************************************************************
    * Constructor
    *-----------------------------------------------------------------------------
    * Save the name of the channel. This name will be used as the name of the vfw channel
    * to read/write data from/to.
    *
    ******************************************************************************/
    RadioChannel::RadioChannel(const char_t * const readChannelName, const char_t * const writeChannelName,
      const uint16_t radChnlID, const char_t * const compShtName) :radioChannelID(radChnlID),
      ATC::IOComponent(atpRadioChannelId, readChannelName, compShtName)
    {
      // index where to store the next received character
      inBufIndex = 0U;
      // index from where to start searching for a new message
      inBufIndexStart = 0U;
      // init queues index
      pushInQueueIndex = 0U;
      popInQueueIndex = 0U;
      pushOutQueueIndex = 0U;
      popOutQueueIndex = 0U;
      // init time-stamps
      prevTx_tSender = 0U;
      prevPrevTx_tSender = 0U;
      prevRx_tSender = 0U;
      //init Id
      regionId = 0U;

      outBufLen = 0U;
      connected = false;
      //Initialize the inBuf
      memset(&(*inBuf), 0, sizeof(inBuf));
      //Initialize the outBuf
      memset(&(*outBuf), 0, sizeof(outBuf));

      memset(&chstat[0], 0, sizeof(chstat));

      // Save the name of the channel.
      // we use &(*variable) in order to remove MISRA C++ 2008 Rule 5-2-12 
      static_cast<void>(vfw_strlcpy(&(*radioReadChannelName), readChannelName, sizeof(radioReadChannelName)));
      static_cast<void>(vfw_strlcpy(&(*radioWriteChannelName), writeChannelName, sizeof(radioWriteChannelName)));
      syncChannelReadDesc = static_cast<VFW_SyncChannel>(NULL);
      //Initialize the transmit mode as Idle 
      txMode = txIdle;
      lastTimeMsgRecvFrmTCC = 0;
      lastTimeMsgSendToTCC = 0;

      initDone = false;
    }

    /******************************************************************************
    * preInit
    ******************************************************************************/
    void RadioChannel::preInit()
    {
      // Channel handle returned by vfwChannelOpenRead, open a channel to be used when reading from dispatcher
      VFW_ChannelDesc const channelReadDesc = vfwChannelOpenRead(&(*radioReadChannelName), ATC::radioChMsgInQueueSize,
        ATC::maxRadioMessageSize, &radioReadChannelName[0]);
      // Open a channel to be used when writing to dispatcher
      crossCompareChannelWriteDesc.initChannel(&(radioWriteChannelName[0]), ATC::radioChMsgOutQueueSize,
        ATC::maxRadioMessageSize, true);

      if ((NULL != channelReadDesc))
      {
        // Synchronize with diversified channel (A/B)
        syncChannelReadDesc = vfwSyncAddChannel(channelReadDesc, ATC::trueVfw);
        // Not event-driven, cyclic polled
        vfwSyncChannelDeactivate(syncChannelReadDesc);
      }
      else
      {
        ATC::aosHalt(__FILE__, __LINE__, "Failed to open channels");
      }

    }

    /******************************************************************************
    * init
    *-----------------------------------------------------------------------------
    * Open the vfw channels for read/write
    * Initialize internal variables
    ******************************************************************************/
    bool RadioChannel::init()
    {
      if (!initDone) // init should be done only once.
      {
        const uint16_t eventOffset = static_cast<uint16_t>(radioChannelID * maxRadioChannelEvent);

        messageInQueueError = ATC::Event::createLogEvent(atpRadioChannelId, ATC::CoreContainer,
          (eventOffset + 1U), 0U, "Too Many Messages in One Cycle");

        writeMessageError = ATC::Event::createLogEvent(atpRadioChannelId, ATC::CoreContainer,
          (eventOffset + 2U), 0U, "Message is available to write but something failed internally");

        incomingMessageLengthError = ATC::Event::createLogEvent(atpRadioChannelId, ATC::CoreContainer,
           (eventOffset + 3U), 0U, "Illegal length in the Incoming message header");

        // init values for cross-compare
        initCrossCompare();

        //set the time to current vfw reference time
        lastTimeMsgRecvFrmTCC = vfwGetReferenceTime();
        lastTimeMsgSendToTCC = vfwGetReferenceTime();

        //Add component Name to the Trace vector of Console
        ATC::AbstractConsole::corePtr()->addTraceObj(getTrace());

        //Initialize the channel statistics for the component
        static_cast<void>(vfw_strlcpy(&(chstat[0].channelname[0]), &radioReadChannelName[0], sizeof(chstat[0].channelname)));
        static_cast<void>(vfw_strlcpy(&(chstat[0].channelType[0]), "read", sizeof(chstat[0].channelType)));
        chstat[0].numMsgCnt = 0U;
        chstat[0].numMsgBytesCnt = 0U;

        static_cast<void>(vfw_strlcpy(&(chstat[1].channelname[0]), &radioWriteChannelName[0], sizeof(chstat[1].channelname)));
        static_cast<void>(vfw_strlcpy(&(chstat[1].channelType[0]), "write", sizeof(chstat[0].channelType)));
        chstat[1].numMsgCnt = 0U;
        chstat[1].numMsgBytesCnt = 0U;

        initDone = true;
      }

      return initDone;
    }

    /******************************************************************************
    * initCrossCompare
    *-----------------------------------------------------------------------------
    * Registers internal persistent values for cross-compare
    *
    ******************************************************************************/
    void RadioChannel::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization

      if (radioChannelID == radioChannelId1)
      {
        // Only Cross Compare one of the channels
        // Divide the table into 4 parts in order to get less data for each compare
        Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareArray<uint64_t>(&crcTable[0U], 64U));
        Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareArray<uint64_t>(&crcTable[64U], 64U));
        Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareArray<uint64_t>(&crcTable[128U], 64U));
        Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareArray<uint64_t>(&crcTable[192U], 64U));
      }

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt64(&lastTimeMsgRecvFrmTCC));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt64(&lastTimeMsgSendToTCC));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&radioChannelID));
      // chstat : not vital
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&regionId));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&connected));

      // inBuf : not vital
      // inBufIndex : not vital
      // inBufIndexStart : not vital
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&pushInQueueIndex));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&popInQueueIndex));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&pushOutQueueIndex));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&popOutQueueIndex));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&prevTx_tSender));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&prevPrevTx_tSender));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&prevRx_tSender));
      // outBuf : not vital
      // outBufLen : not vital

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&messageInQueueError));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&writeMessageError));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&incomingMessageLengthError));

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&initDone));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareEnum<TxMode>(&txMode));
    }

    /******************************************************************************
    * runIn
    *-----------------------------------------------------------------------------
    * Owner or scheduler shall call runIn() once per activation.
    * Reads all incoming bytes and extracts all messages
    ******************************************************************************/
    void RadioChannel::runIn()
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "RCH_runIn");

      // read bytes from channel in network order
      while (readFromChannel() > 0)
      {
        RadioMessage thisMessage;
        // extract messages
        while (extractMessage(thisMessage))
        {
          bool resendLastTxMessage = false;
          // Check timestamp
          if (isValidTimeStamp(thisMessage.tRef, resendLastTxMessage, thisMessage.tSender))
          {
            // push to messageInQueue in case several messages received in the same cycle
            // messageInQueue is of size 5 with current protocol   
            if (!pushMessageToInQueue(thisMessage))
            {
              // Error :: overflow, too many messages in one cycle 
              ATC::AbstractEventHandler::corePtr()->reportEvent(messageInQueueError, __FILE__, __LINE__);
            }
            //Set get Connected Status as true
            connected = true;
            // ready to send a new message
            txMode = txIdle;
          }
          else
          {

            if (resendLastTxMessage)
            {
              txMode = txResend;
            }
          }//End of else(isValidTimeStamp)
        }//End of while(extracted)
      }//End of while(readFromChannel)

      // Move any remaining bytes to the start of the input buffer
      uint16_t numOfRemainingBytes = inBufIndex - inBufIndexStart;
      if ((inBufIndexStart > 0U) && (numOfRemainingBytes > 0U))
      {
        memmove(&inBuf[0], &inBuf[inBufIndexStart], numOfRemainingBytes);
      }

      inBufIndexStart = 0U;
      inBufIndex = numOfRemainingBytes;
    }

    /******************************************************************************
    * getConnected
    *-----------------------------------------------------------------------------
    * Get status of connection
    *
    * return true if a telegram has been successfully extracted during a period of time
    ******************************************************************************/
    bool RadioChannel::getConnected() const
    {
      return connected;
    }

    /******************************************************************************
    * getChannelId
    *-----------------------------------------------------------------------------
    * Get unique Id of radio Channel
    *
    * return the unique Id for the Radio channel
    ******************************************************************************/
    uint16_t RadioChannel::getChannelId() const
    {
      return radioChannelID;
    }

    /******************************************************************************
    * getTxPending
    *-----------------------------------------------------------------------------
    * Get no of messages waiting to be sent
    * The out-queue (with the current protocol) has size 1
    * Hence 0 == Queue empty and 1 == Queue full
    *
    ******************************************************************************/
    uint8_t RadioChannel::getTxPending() const
    {
      return pushOutQueueIndex;
    }

    /******************************************************************************
    * readFromChannel
    *-----------------------------------------------------------------------------
    * Read characters from the vfw channel
    *
    ******************************************************************************/
    int32_t RadioChannel::readFromChannel()
    {
      int32_t readResult = 0;

      // If there is any message in the channel queue
      if (vfwSyncChannelStat(syncChannelReadDesc) > 0U)
      {
        //Set the Current Time to last time TCC connected 
        lastTimeMsgRecvFrmTCC = vfwGetReferenceTime();

        logChannelRead(&radioReadChannelName[0]);

        // Read one message 
        VFW_ChannelCheck check = { VFW_ChannelErrorNone, 0U };
        readResult = vfwSyncChannelReadCheck(
          syncChannelReadDesc, &inBuf[inBufIndex], static_cast<uint32_t>(sizeof(inBuf) - inBufIndex), &check);

        if (check.error != VFW_ChannelErrorNone)
        {
          writeToLog(ATC::BriefLog, "Radio channel error:", static_cast<uint32_t>(check.error), __FILE__, __LINE__);
          ATC::aosHalt(__FILE__, __LINE__, "Radio channel error");
        }
        if (check.timeSinceProduced > ATC::maxChannelTransmissionTime)
        {
          writeToLog(ATC::BriefLog, "Radio message too old:", check.timeSinceProduced, __FILE__, __LINE__);
          ATC::aosHalt(__FILE__, __LINE__, "Radio message too old");
        }

        if (readResult > 0)
        {
          const uint16_t noOfBytesRead = static_cast<uint16_t>(readResult);

          Support::AbstractCrossCompare::corePtr()->addCrossCompareInputData(&inBuf[inBufIndex], noOfBytesRead);

          inBufIndex += noOfBytesRead;

          //Update channel statistics
          chstat[0].numMsgCnt++;
          chstat[0].numMsgBytesCnt += noOfBytesRead;
        }
      }
      else
      {
        //Get the time difference
        const int64_t timeDiffTCCConnectionTime = vfwGetReferenceTime() - lastTimeMsgRecvFrmTCC;
        //Get the config value
        const int64_t radioTimeOutConfigValueInMilliSec = (static_cast<int64_t>(AbstractConfig::corePtr()->getRadioTimeOut())) *
          (static_cast<int64_t>(ATC::secToMSec));

        const bool timeoutSinceLastMsgRecv = (timeDiffTCCConnectionTime > radioTimeOutConfigValueInMilliSec);

        if (timeoutSinceLastMsgRecv)
        {
          //set the connection to false
          connected = false;
        }
      }

      return readResult;
    }

    /******************************************************************************
    * writeToChannel
    *-----------------------------------------------------------------------------
    * Write bytes to the vfw channel
    * Writing in UDP format for PC Simulator Environment.
    * While in target, ATP A will send first half of message to dispatcher and ATP B will
    * send second half to dispatcher and dispatcher will combine it and send to TCC.
    ******************************************************************************/
    void RadioChannel::writeToChannel(const uint8_t * const outBuffer, uint16_t const outBufferLen)
    {
      crossCompareChannelWriteDesc.putBuffer(outBuffer, outBufferLen);
      //Update channel statistics
      chstat[1].numMsgCnt++;
      chstat[1].numMsgBytesCnt += outBufferLen;
    }

    /******************************************************************************
    * packMessage
    *-----------------------------------------------------------------------------
    * pack a RadioMessage to an outgoing buffer (in network order)
    *
    ******************************************************************************/
    uint16_t RadioChannel::packMessage(uint8_t * const outBuffer, const RadioMessage & msg)
    {
      uint64_t crc[maxMessageChunks] = { 0U,0U,0U };

      // Use vfw-functions to convert from host order to network order
      VFW_Buffer buffer;

      vfwInitBuffer(&buffer, outBuffer, ATC::maxRadioMessageSize);

      // Pack STX
      vfwPutU8(&buffer, ATC::STX);

      //Save this pointer as start location for CRC calculation
      uint8_t* crcStartPtr = vfwGetPointer(&buffer);

      // Pack Id
      vfwPutU16(&buffer, msg.id);
      // Pack Site Id
      vfwPutU8(&buffer, msg.siteId);
      // Pack Region Id
      vfwPutU8(&buffer, msg.regionId);
      // Pack data length
      vfwPutU16(&buffer, msg.dataLength);
      // Pack time-stamps
      vfwPutU16(&buffer, msg.tSender);
      vfwPutU16(&buffer, msg.tRef);

      // Pack data
      vfwCpyFromRawBuffer(&buffer, &msg.data[0], msg.dataLength);

      // Calculate the outgoingMessageChunksCounter
      uint8_t outgoingMessageChunksCounter = static_cast<uint8_t>(msg.dataLength / maxLengthOfMessageChunks);

      if ((msg.dataLength % maxLengthOfMessageChunks) != 0U)
      {
        outgoingMessageChunksCounter += 1U;
      }

      for (uint8_t i = 0U; i < outgoingMessageChunksCounter; i++)
      {
        if (i == 0U)
        {
          const uint16_t chunkLength = (msg.dataLength < maxLengthOfMessageChunks) ? msg.dataLength : maxLengthOfMessageChunks;
          const uint16_t crcLength = (RadioMessage::headerSize - 1U) + chunkLength; /* -1 since STX not included */

          // Calculate crc
          crc[i] = calculateCRC(crcStartPtr, crcLength);

          // Adjust the crcStartPtr for calculating the next CRC
          crcStartPtr += crcLength;
        }
        else
        {
          // Calculate crc
          crc[i] = calculateCRC(crcStartPtr, (((i + 1U) == outgoingMessageChunksCounter)
            ? (msg.dataLength - static_cast<uint16_t>((i * maxLengthOfMessageChunks))) : maxLengthOfMessageChunks));

          //Adjust the crcStartPtr for calculating the next CRC
          crcStartPtr = crcStartPtr + maxLengthOfMessageChunks;
        }

        vfwPutU64(&buffer, crc[i]);
      }

      // Get valid length of buffer
      return static_cast<uint16_t> (vfwGetValidSize(&buffer));
    }
    /******************************************************************************
    * handleTxTimeStamps()
    *-----------------------------------------------------------------------------
    * Create, assign and save the time-stamps for the transmitted message
    *
    ******************************************************************************/
    void RadioChannel::handleTxTimeStamps(RadioMessage & msg)
    {
      // Generate new time-stamp
      msg.tSender = generateTimeStamp();
      // Save time-stamp
      prevPrevTx_tSender = prevTx_tSender;
      // Assign the saved prevRx_tSender to the outgoing RadioMessage time stamp tRef
      msg.tRef = prevRx_tSender;
      prevTx_tSender = msg.tSender;
    }

    /******************************************************************************
    * setOutBufHeader()
    *-----------------------------------------------------------------------------
    * Assign the Id, SiteId and regionId for the transmitted message
    *
    ******************************************************************************/
    void RadioChannel::setOutBufHeader(RadioMessage & msg) const
    {
      // set the header for the Transmit message
      msg.id = AbstractConfig::corePtr()->getRadioId();
      msg.siteId = AbstractConfig::corePtr()->getSiteId();
      msg.regionId = regionId;
    }

    /******************************************************************************
    * generateTimeStamp()
    *-----------------------------------------------------------------------------
    * Generate a new time-stamp to be used as tSender in outgoing message
    *
    ******************************************************************************/
    uint16_t RadioChannel::generateTimeStamp() const
    {
      // get platform time using vfw
      int64_t platformTime = vfwGetReferenceTime();

      // Get a time stamp that is different than the last one used. Normally,
      // it should be more than 16 ms since the last message we transmitted.
      //
      uint16_t ts = static_cast<uint16_t>(static_cast<uint32_t>(platformTime) / 16U); /* The resolution should be 16 ms */
                                                               // Avoid some special cases
                                                               // New time-stamp 
      while ((ts == defaultTRef) || (ts == prevTx_tSender) || (ts == prevPrevTx_tSender))
      {
        ts++; /* Avoid using the startup time-stamps or the saved time-stamps */
      }
      return ts;
    }


    /******************************************************************************
    * runOut
    *-----------------------------------------------------------------------------
    * Owner or scheduler shall call runOut() once per activation.
    * Prepares and sends all pending outgoing messages queued
    ******************************************************************************/
    void RadioChannel::runOut()
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "RCH_runOut");

      RadioMessage thisMsg;

      if (txMode == txIdle)
      {
        // get all pending outgoing message
        while (popMessageFromOutQueue(thisMsg))
        {
          //Set the Header of the outgoing message.
          setOutBufHeader(thisMsg);
          // Create and save time-stamps
          handleTxTimeStamps(thisMsg);
          
          if (thisMsg.dataLength > 0U)
          {
            // Pack and convert to network order
            outBufLen = packMessage(&(*outBuf), thisMsg);
            if (outBufLen > 0U)
            {
              // write to vfw channel
              writeToChannel(&(*outBuf), outBufLen);
              //Set the transmit mode as Sending
              txMode = txSending;
              //set the time
              lastTimeMsgSendToTCC = vfwGetReferenceTime();
            }
            else
            {
              //Report message Error 
              ATC::AbstractEventHandler::corePtr()->reportEvent(writeMessageError, __FILE__,
                __LINE__);
            }
          }
          else
          {
            //Report message Error 
            ATC::AbstractEventHandler::corePtr()->reportEvent(writeMessageError, __FILE__,
              __LINE__);
          }
        }
      }
      else if (txMode == txResend)
      {
        if (outBufLen > 0U)
        {
          // write to vfw channel
          writeToChannel(&(*outBuf), outBufLen);
          //Set the transmit mode as Sending
          txMode = txSending;
          //set the connection
          lastTimeMsgSendToTCC = vfwGetReferenceTime();
        }
        else
        {
          //Report message Error 
          ATC::AbstractEventHandler::corePtr()->reportEvent(writeMessageError, __FILE__,
            __LINE__);
        }
      }
      else if (txMode == txSending)
      {
        //Get the time difference
        const int64_t timeDiffTCCConnectionTime = vfwGetReferenceTime() - lastTimeMsgSendToTCC;
        //Get the config value
        const int64_t radioTimeOutConfigValueInMilliSec = (static_cast<int64_t>(AbstractConfig::corePtr()->getRadioTimeOut())) *
          (static_cast<int64_t>(ATC::secToMSec));

        const bool timeoutSinceLastMsgSent = (timeDiffTCCConnectionTime > radioTimeOutConfigValueInMilliSec);

        if (timeoutSinceLastMsgSent)
        {
          txMode = txIdle;
        }
      }
      else
      {
        txMode = txIdle;
      }
    }

    /******************************************************************************
    * popMessageFromOutQueue
    *-----------------------------------------------------------------------------
    * Find first occurrence of STX in inBuf
    * Returns pointer to the first occurrence or NULL if not found
    ******************************************************************************/
    bool RadioChannel::popMessageFromOutQueue(RadioMessage & msg)
    {
      bool messageRead;
      // Any message left to read ?
      if (popOutQueueIndex < pushOutQueueIndex)
      {
        msg = messageOutQueue[popOutQueueIndex];
        popOutQueueIndex++;
        messageRead = true;
      }
      else
      {
        // all items read from the queue
        // init and prepare for new items
        pushOutQueueIndex = 0U;
        popOutQueueIndex = 0U;
        messageRead = false;
      }

      return messageRead;
    }

    /******************************************************************************
    * findStartOfTelegram
    ******************************************************************************/
    bool RadioChannel::findStartOfTelegram(uint16_t & availableBytes)
    {
      bool stxFound = false;

      while (inBufIndexStart < inBufIndex)
      {
        if (ATC::STX == inBuf[inBufIndexStart])
        {
          stxFound = true;
          break;
        }

        inBufIndexStart++;
      }

      availableBytes = inBufIndex - inBufIndexStart;

      return stxFound;
    }

    /******************************************************************************
    * calculate CRC
    *-----------------------------------------------------------------------------
    * Calculate Cyclic Redundancy Code
    * Returns the calculated CRC
    ******************************************************************************/
    uint64_t RadioChannel::calculateCRC(const uint8_t*  start, uint16_t const length)
    {
      uint64_t crc = crcInitValue;
      uint16_t bytesLeft = length;

      if (NULL != start)
      {
        while (bytesLeft > 0U)
        {
          crc = crcTable[*start ^ static_cast<uint8_t>(crc >> 56)] ^ (crc << 8);
          start++;
          bytesLeft--;
        }
      }
      else
      {
        ATC::aosHalt(__FILE__, __LINE__, "NULL pointer for start passed to calculateCRC()");
      }

      return crc;
    }

    /******************************************************************************
    * selfTest
    ******************************************************************************/
    bool RadioChannel::selfTest(void)
    {
      uint8_t b[9] =
      {
        static_cast<uint8_t>('1'),
        static_cast<uint8_t>('2'),
        static_cast<uint8_t>('3'),
        static_cast<uint8_t>('4'),
        static_cast<uint8_t>('5'),
        static_cast<uint8_t>('6'),
        static_cast<uint8_t>('7'),
        static_cast<uint8_t>('8'),
        static_cast<uint8_t>('9')
      };

      uint64_t crc = calculateCRC(&b[0], 9U);
      uint64_t val = (static_cast<uint64_t>(0xA8F79275UL) << 32) + 0x3965808BUL;

      return (crc == val);
    }

    /******************************************************************************
    * pushMessageInQueue
    *-----------------------------------------------------------------------------
    * Push message to queue
    * Returns true if successful
    ******************************************************************************/
    bool RadioChannel::pushMessageToInQueue(const RadioMessage & msg)
    {
      bool messagePushed;
      if (pushInQueueIndex < radioMessageInQueueSize)
      {
        // Do not copy entire record
        // Copy-constructor will only copy the part of the message being used
        messageInQueue[pushInQueueIndex] = msg;
        pushInQueueIndex++;
        messagePushed = true;
      }
      else
      {
        messagePushed = false;
      }

      return messagePushed;
    }

    /******************************************************************************
    * readMessage
    *-----------------------------------------------------------------------------
    * Read a message and pop message from queue
    * Returns true if successful
    ******************************************************************************/
    bool RadioChannel::readMessage(RadioMessage & msg)
    {
      // This function should be called repeatedly till it returns false in order to get all
      // the messages from the queue. This will return false when all the read is done.

      bool messageRead;
      if (popInQueueIndex < pushInQueueIndex)
      {
        // Do not copy entire record
        // Copy-constructor will only copy the part of the message being used
        msg = messageInQueue[popInQueueIndex];
        popInQueueIndex++;
        messageRead = true;
      }
      else
      {
        // all items read from the queue
        // init and prepare for new items
        pushInQueueIndex = 0U;
        popInQueueIndex = 0U;
        messageRead = false;
      }

      return messageRead;
    }
    /******************************************************************************
    * peekMessage
    *-----------------------------------------------------------------------------
    * Peek any available  message
    * Returns true if successful
    ******************************************************************************/
    bool RadioChannel::peekMessage(RadioMessage & msg) const
    {
      bool messageRead;
      if (popInQueueIndex < pushInQueueIndex)
      {
        // Do not copy entire record
        // Copy-constructor will only copy the part of the message being used
        // Do not pop the msg from the InQueue 
        msg = messageInQueue[popInQueueIndex];
        messageRead = true;
      }
      else
      {
        messageRead = false;
      }
      return messageRead;
    }

    /******************************************************************************
    * isValidTimeStamp
    *-----------------------------------------------------------------------------
    * Validate message timestamp
    *
    *
    * Returns true if successful
    ******************************************************************************/
    bool RadioChannel::isValidTimeStamp(uint16_t const tRef, bool & resendLastMessage, uint16_t const tRecSender)
    {
      bool validTimeStamp;
      if (tRef == defaultTRef)
      {
        // TCC restarted ?
        // Accept and log
        // Clear saved time-stamps
        validTimeStamp = true;
        // save the current received time-stamp from TCC as prevRx_tSender to be used as received 
        //timestamp in the next message transmitted to TCC
        prevRx_tSender = tRecSender;
      }
      else if (tRef == prevTx_tSender)
      {
        // Timestamp is as expected
        validTimeStamp = true;
        // save the current received time-stamp from TCC as prevRx_tSender to be used as received 
        //timestamp in the next message transmitted to TCC
        prevRx_tSender = tRecSender;
      }
      else if (tRef == prevPrevTx_tSender)
      {
        // Last message has already been sent as a response to TCC
        // but for some reason TCC sends the same time-stamp again
        // indicating that the last message from AOS to TCC was lost
        // resend!
        validTimeStamp = false;
        resendLastMessage = true;
      }
      //when AOS restarts and TCC is polling.
      else if (prevTx_tSender == 0U)
      {
        // AOS restarted ?
        // Accept and log
        // Clear saved time-stamps
        validTimeStamp = true;
        // save the current received time-stamp from TCC as prevRx_tSender to be used as received
        //timestamp in the next message transmitted to TCC
        prevRx_tSender = tRecSender;
      }
      else
      {
        validTimeStamp = false;
        // This is a bit simplified
        // To be defined:
        // Error handling, retry etc..
      }
      return validTimeStamp;
    }

    /******************************************************************************
    * validateMessage
    *-----------------------------------------------------------------------------
    * Validate message header and checksum
    * Returns true if successful
    ******************************************************************************/
    bool RadioChannel::validateMessage(const uint8_t * const buffer, const RadioMessage & msg, uint8_t const stx,
      const uint64_t extractedCRC[maxMessageChunks], const uint8_t numOfMessageChunks)
    {
      uint64_t crcValidate[maxMessageChunks] = { 0U,0U,0U };
      bool validated = false;
      bool isCRCEqual = true;

      const uint8_t*bufferPtrForCRCCalc = buffer;
      // Skip first byte (STX), since it is not included in the CRC-calculation and thus passed in a separate argument.
      ++bufferPtrForCRCCalc;

      // Sanity check that stx character extracted Ok
      if (ATC::STX == stx)
      {
        // Calculate CRC
        for (uint8_t i = 0U; i < numOfMessageChunks; i++)
        {
          if (i == 0U)
          {
            const uint16_t chunkLength = (msg.dataLength < maxLengthOfMessageChunks) ? msg.dataLength : maxLengthOfMessageChunks;
            const uint16_t crcLength = (RadioMessage::headerSize - 1U) + chunkLength; /* -1 since STX not included */

            // Calculate crc for validating incoming Message
            crcValidate[i] = calculateCRC(bufferPtrForCRCCalc, crcLength);

            // Adjust buffer to calculate next CRC
            bufferPtrForCRCCalc += crcLength;
          }
          else
          {
            // Calculate crc for validating incoming Message
            crcValidate[i] = calculateCRC(bufferPtrForCRCCalc, (((i + 1U) == numOfMessageChunks)
              ? (msg.dataLength - static_cast<uint16_t>((i * maxLengthOfMessageChunks))) : maxLengthOfMessageChunks));

            // Adjust buffer to calculate next CRC
            bufferPtrForCRCCalc += maxLengthOfMessageChunks;
          }

          // Compare the extracted and calculated CRC for each message chunks
          if (crcValidate[i] != extractedCRC[i])
          {
            isCRCEqual = false;
          }
        }

        if (isCRCEqual)
        {
          uint16_t radioId = AbstractConfig::corePtr()->getRadioId();
          uint8_t siteId = AbstractConfig::corePtr()->getSiteId();

          // Check identity
          if ((msg.id == radioId) && (msg.siteId == siteId))
          {
            validated = true;
          }
        }
      }
      return validated;
    }
    /******************************************************************************
    * extractMessage
    *-----------------------------------------------------------------------------
    * Extract message from characters in inBuf
    * Returns true if message extracted and validate successfully.
    ******************************************************************************/
    bool RadioChannel::extractMessage(RadioMessage & msg)
    {
      bool keepReading = true;
      bool messageExtracted = false;

      /** Array to store the CRC for extracted Message and use them for comparing with
      * calculated CRC while validating the incoming message
      */
      uint64_t extractedCRC[maxMessageChunks] = { 0U,0U,0U };

      // Scan inBuf for the first/next <STX> character found in inBuf
      // with sufficient amount of available characters to be a candidate for a valid message
      uint16_t availableChars = 0U;
      bool stxFound = findStartOfTelegram(availableChars);

      // A minimum characters needed for <STX>, ID, SITE_ID, REGION ID, LEN, time-stamps and checksums
      while (keepReading && stxFound &&
        (availableChars > (RadioMessage::headerSize + sizeof(uint64_t))))
      {
        uint8_t* posStart = &inBuf[inBufIndexStart];

        // Use vfw-functions to convert from network order to host order
        VFW_Buffer buffer;

        vfwInitBuffer(&buffer, posStart, availableChars);
        // Set buffer in read mode. Valid size is set to the amount of data 
        // filled in the raw_buffer. 
        vfwSetReadBuffer(&buffer, availableChars);

        // STX
        uint8_t stx = vfwGetU8(&buffer);
        // ID
        msg.id = vfwGetU16(&buffer);
        // SITE ID
        msg.siteId = vfwGetU8(&buffer);
        // Region ID
        msg.regionId = vfwGetU8(&buffer);
        // Extract the 2-byte LEN field 
        msg.dataLength = vfwGetU16(&buffer);

        // Calculate the number of chunks
        const uint8_t numOfMessageChunks = static_cast<uint8_t>(
          ATC::ATCMath::instance().unsignDivRoundUp(msg.dataLength, maxLengthOfMessageChunks, __FILE__, __LINE__));

        // Size of incoming message: size of RadioHeader + size of CRC + data length
        const uint16_t radioMessageSize = static_cast<uint16_t>(
          RadioMessage::headerSize + (numOfMessageChunks * sizeof(uint64_t)) + msg.dataLength);

        // Check the length of the message
        if ((numOfMessageChunks <= maxMessageChunks) && (radioMessageSize <= ATC::maxRadioMessageSize))
        {
          // Check if we have the whole message
          if (radioMessageSize <= availableChars)
          {
            // Extract the 2-byte T_SENDER field
            msg.tSender = vfwGetU16(&buffer);
            // Extract the 2-byte T_REF field
            msg.tRef = vfwGetU16(&buffer);

            // Extract the message data
            vfwCpyToRawBuffer(&msg.data[0], &buffer, msg.dataLength);

            for (uint8_t i = 0U; i < numOfMessageChunks; i++)
            {
              extractedCRC[i] = vfwGetU64(&buffer);
            }

            if (validateMessage(posStart, msg, stx, &extractedCRC[0], numOfMessageChunks))
            {
              // All good, break the loop
              messageExtracted = true;
              keepReading = false;

              // Consume the valid message
              inBufIndexStart += radioMessageSize;

              // Set RegionId as it is read from validated message
              regionId = msg.regionId;
            }
            else
            {
              // Report message error
              getTrace()->write(ATC::detailedTrace, "Mismatch in CRC");

              // Skip this STX byte and search for next occurrence of STX
              inBufIndexStart++;
              stxFound = findStartOfTelegram(availableChars);
            }
          }
          else
          {
            // The whole message hasn't been received yet, so try again in next cycle
            keepReading = false;
          }
        }
        else
        {
          // Report message error
          getTrace()->write(ATC::detailedTrace, "Illegal length");
          ATC::AbstractEventHandler::corePtr()->reportEvent(incomingMessageLengthError, __FILE__, __LINE__);

          // Skip this STX byte and search for next occurrence of STX
          inBufIndexStart++;
          stxFound = findStartOfTelegram(availableChars);
        }
      }

      return messageExtracted;
    }
    /******************************************************************************
    * writeMessage
    *-----------------------------------------------------------------------------
    * Append a radio-message to the messageOutQueue
    * Returns true if message written successfully.
    ******************************************************************************/
    bool RadioChannel::writeMessage(const RadioMessage & msg)
    {
      bool messageWritten;
      if (pushOutQueueIndex < radioMessageOutQueueSize)
      {
        // Do not copy entire record
        // Copy-constructor will only copy the part of the message being used
        messageOutQueue[pushOutQueueIndex] = msg;

        pushOutQueueIndex++;
        messageWritten = true;
      }
      else
      {
        messageWritten = false;
      }

      return messageWritten;
    }


    /******************************************************************************
    * isCentral
    ******************************************************************************/
    bool RadioChannel::isCentral() const
    {
      return false;
    }

    /******************************************************************************
    * logChannelRead
    ******************************************************************************/
    void RadioChannel::logChannelRead(const char_t* const channelName) const
    {
      int32_t ret;
      char_t  buffer[512];

      //lint -e{586} snprintf is needed here
      ret = snprintf(&buffer[0], sizeof(buffer), "Reading from Channel : %s", channelName);

      if ((ret > 0)  &&  (static_cast<size_t>(ret) < sizeof(buffer)))
      {
        writeToLog(ATC::ChannelLog, &buffer[0], __FILE__, __LINE__);
      }
    }

    /******************************************************************************
    * consoleCall
    ******************************************************************************/
    bool RadioChannel::consoleCall(const uint32_t argc, const ATC::ConsoleArguments argv)
    {
      /*
      This functions parses the arguments searches for the "help" or any other DMI Channel
      component specific command calls and handles it. Returns true if completeley handled
      else returns false. returning false will let other components handle the call. help always returns false.
      */

      bool retVal = false;
      char_t  buff[100];

      // Handle help call at first. argc cannot be 0 as there is a check before consoleCall()
      if ((ATC::isTextMatch(&argv[0][0], "chstat", sizeof("chstat"))) && (1U == argc))
      {
        for (uint8_t cnt = 0U; cnt < numVfwChannelsRadioChannels; cnt++)
        {
          //lint -e{586} snprintf is needed here
          const int32_t ret = snprintf(&buff[0], sizeof(buff), "%-30s%-14s%-15u%-12u", chstat[cnt].channelname, chstat[cnt].channelType,
            chstat[cnt].numMsgCnt, chstat[cnt].numMsgBytesCnt);

          if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buff)))
          {
            ATC::AbstractConsole::corePtr()->writeWithNewline(&buff[0]);
          }
        }
      }

      return retVal;
    }

  }
}

