/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file implements the methods of the OBRDMessageHandler class.

******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-10-25    sunilk    Created

*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <cstdio>
#include <cstring>
#include <vfw_string.h>
#include <vfw_timer.h>
#include "atc_base.hpp"
#include "config.hpp"
#include "atc_util.hpp"
#include "log_handler.hpp"
#include "obrd_message_handler.hpp"
#include "channel_config.hpp"
#include "abstract_console.hpp"
#include "abstract_cross_compare.hpp"
#include "abstract_config.hpp"
#include "tims_event_ids.hpp"
#include "dmi_event_codes.hpp"

#ifndef __GNUG__
extern "C" void vfwGetTimeOfDay(struct timespec * const timespec_p, struct tm * const tm_p);
#else
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
  namespace TG
  {

    /** Lint doesn't allow constant zero to be used as left/right parameter for << or +, enum is allowed.
    */
    enum SpecialValueZero {
      zeroValue = 0x00000000UL
    };

    /** Table used for crc-calculation in calculateCRC() method.
    *   Target does not support 'NULL', therefore a workaround with shift operation.
    */
    static const uint64_t crcTable[256] = {
      /*(zeroValue)) << 32) + */static_cast<uint32_t>(zeroValue),(static_cast<uint64_t>(0x843EED14UL) << 32) + 0xD357E50DUL,
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
    ******************************************************************************/
    OBRDMessageHandler::OBRDMessageHandler(
      const char_t* const readChannelName, const char_t* const writeChannelName, ATC::TraceInterface* const trace_) :
      validationIncomingMessageFailed(ATC::Event::createLogEvent(atpTimsId,
        ATC::AdaptationContainer, eventIdObrdValidationIncomingMessageFailed, 0U, "Validation of OBRD message failed: ", true)),
      establishedConnectionWithOBRD(ATC::Event::createLogEvent(atpTimsId,
        ATC::AdaptationContainer, eventIdObrdEstablishedConnection, 0U, "Connection established with OBRD BOS")),
      lostConnectionWithOBRD(ATC::Event::createLogEvent(atpTimsId,
        ATC::AdaptationContainer, eventIdObrdLostConnection, 0U, "Connection lost with OBRD BOS")),
      wrongOBRDTypeMessageRecvd(ATC::Event::createLogEvent(atpTimsId,
        ATC::AdaptationContainer, eventIdObrdWrongMessageType, 0U, "Invalid Message received from OBRD")),
      wrongOBRDProtocolVersionRecvd(ATC::Event::createLogEvent(atpTimsId,
        ATC::AdaptationContainer, eventIdObrdWrongProtocolVersion, DMICom::wrongOBRDProtocolVersionRecvd, "Wrong OBRD protocol version received")),
      protocolVersionParser(trace_),
      unitStatusParser(trace_),
      protocolVersionCreator(trace_),
      rejectMessageCreator(trace_),
      trace(trace_),
      initDone(false)
    {
      // Save the name of the channel.
      static_cast<void>(vfw_strlcpy(&obrdReadChannelName[0], readChannelName, sizeof(obrdReadChannelName)));
      static_cast<void>(vfw_strlcpy(&obrdWriteChannelName[0], writeChannelName, sizeof(obrdWriteChannelName)));

      syncChannelReadDesc = static_cast<VFW_SyncChannel>(NULL);
      lastMessageReceivedTime = 0;
      connectionLossTimeout = 0;
      protocolVersionMatched = false;
      sendProtocolVersionMessage = false;
      isUnitStatusReportRecv = false;
      sendRejectMessage = false;
      rejectionReason = OBRDRejectReasonUndefined;
      connectedToOBRD = false;
      memset(&channelStatistics[0], 0, sizeof(channelStatistics));
      connectionTimeoutWithOBRD = false;

      // index where to store the next received character
      inBufWriteIndex = 0U;
      // index from where to start searching for a new message
      inBufReadIndex = 0U;

      //Initialize the inBuf
      memset(&inBuf[0], 0, sizeof(inBuf));
    }

    /******************************************************************************
    * preInit
    ******************************************************************************/
    void OBRDMessageHandler::preInit()
    {

      // Open a channel to be used when reading from dispatcher
      VFW_ChannelDesc channelReadDesc = vfwChannelOpenRead(&obrdReadChannelName[0], ATC::obrdMessageInQueueSize,
        ATC::maxOBRDMsgSize, &obrdReadChannelName[0]);

      // Open a channel to be used when writing to dispatcher
      crossCompareWriteChannel.initChannel(&obrdWriteChannelName[0], ATC::obrdMessageInQueueSize,
        ATC::maxOBRDMsgSize, true);

      if (static_cast<VFW_ChannelDesc>(NULL) != channelReadDesc)
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
    ******************************************************************************/
    bool OBRDMessageHandler::init()
    {
      // Only initialize once
      if (!initDone)
      {
        //lint -e{1960} Cast is unavoidable here, because of external interfaces
        connectionLossTimeout = static_cast<int64_t>(Config::instance().getOBRDReportTimeout() * 1000U); //ms

        //lint --e{586} 'new' is acceptable during initialization
        Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&statusReport.unitTrackAndPos.track));
        Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&statusReport.unitTrackAndPos.position));
        Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&statusReport.lastCarBrakePressure));
        Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint64(&statusReport.timeOfMeasurement));

        // Initialize the channel statistics for the component
        static_cast<void>(vfw_strlcpy(&(channelStatistics[0].channelname[0]), &obrdReadChannelName[0], sizeof(channelStatistics[0].channelname)));
        static_cast<void>(vfw_strlcpy(&(channelStatistics[0].channelType[0]), "read", sizeof(channelStatistics[0].channelType)));
        static_cast<void>(vfw_strlcpy(&(channelStatistics[1].channelname[0]), &obrdWriteChannelName[0], sizeof(channelStatistics[1].channelname)));
        static_cast<void>(vfw_strlcpy(&(channelStatistics[1].channelType[0]), "write", sizeof(channelStatistics[0].channelType)));

        initDone = true;
      }
      return initDone;
    }

    /******************************************************************************
    * runIn
    ******************************************************************************/
    void OBRDMessageHandler::runIn()
    {
      const int64_t timeNow = vfwGetReferenceTime();

      //Reset the flags
      sendProtocolVersionMessage = false;
      sendRejectMessage = false;
      rejectionReason = OBRDRejectReasonUndefined;
      isUnitStatusReportRecv = false;

      // Check if there is any message in the channel queue
      while (vfwSyncChannelStat(syncChannelReadDesc) > 0U)
      {
        VFW_ChannelCheck check = { VFW_ChannelErrorNone, 0U };
        int32_t readResult = vfwSyncChannelReadCheck(
          syncChannelReadDesc, &inBuf[inBufWriteIndex], static_cast<uint32_t>(sizeof(inBuf) - inBufWriteIndex), &check);

        if (check.error != VFW_ChannelErrorNone)
        {
          ATC::AbstractLogHandler::corePtr()->writeToLog(
            ATC::BriefLog, "OBRD channel error:", static_cast<uint32_t>(check.error), "TM", __FILE__, __LINE__);
          ATC::aosHalt(__FILE__, __LINE__, "OBRD channel error");
        }
        if (check.timeSinceProduced > ATC::maxChannelTransmissionTime)
        {
          ATC::AbstractLogHandler::corePtr()->writeToLog(
            ATC::BriefLog, "OBRD message too old:", check.timeSinceProduced, "TM", __FILE__, __LINE__);
          ATC::aosHalt(__FILE__, __LINE__, "OBRD message too old");
        }

        // Read was successful?
        if (readResult > 0)
        {
          const uint16_t noOfBytesRead = static_cast<uint16_t>(readResult);

          ATC::AbstractLogHandler::corePtr()->logRU(
            ATP::LogHandler::Ifc_OBRD, ATC::AbstractLogHandler::Ifc_In, &inBuf[inBufWriteIndex], noOfBytesRead);

          Support::AbstractCrossCompare::corePtr()->addCrossCompareInputData(&inBuf[inBufWriteIndex], noOfBytesRead);

          channelStatistics[0].numMsgBytesCnt += static_cast<uint32_t>(noOfBytesRead);

          //Decoding incoming message
          inBufWriteIndex += noOfBytesRead;
          OBRDMessage thisMessage;

          //to extract all messages from input buffer
          while (extractMessage(thisMessage))
          {
            channelStatistics[0].numMsgCnt++;

            lastMessageReceivedTime = timeNow;

            OBRDMessageIn *ptrParser = static_cast<OBRDMessageIn*>(NULL);

            // Fetch the proper parser for received message type
            if (thisMessage.obrdPacketData.packetType == obrdPTypeProtocolVersion)
            {
              //Reseting the protocol version flag
              protocolVersionMatched = false;
              ptrParser = &protocolVersionParser;

              //Connection timeout should reset
              connectionTimeoutWithOBRD = false;

              // Parse, validate and publish the data if valid
              if (!ptrParser->validate(&thisMessage.obrdPacketData))
              {
                trace->write(ATC::briefTrace, "Protocol Version message validation failed");
                //report log event and send message to DMI for the wrong protocol version received.
                ATC::AbstractEventHandler::corePtr()->reportEvent(wrongOBRDProtocolVersionRecvd, __FILE__, __LINE__);
                sendRejectMessage = true;
                rejectionReason = OBRDRejectReasonWrongProtocolVersion;
              }
              else
              {
                trace->write(ATC::briefTrace, "Protocol Version message validated");
                //Protocol version matched
                protocolVersionMatched = true;
                sendProtocolVersionMessage = true;
              }
            }
            else if (thisMessage.obrdPacketData.packetType == obrdPTypeUnitStatus)
            {
              if (protocolVersionMatched)
              {
                ptrParser = &unitStatusParser;
                // Parse, validate and publish the data if valid
                if (ptrParser->validate(&thisMessage.obrdPacketData))
                {
                  if (!connectedToOBRD)
                  {
                    connectedToOBRD = true;
                    // OBRD Connection established
                    ATC::AbstractEventHandler::corePtr()->reportEvent(establishedConnectionWithOBRD, __FILE__, __LINE__);
                  }
                  //Unit Status Report received and parsed successfully 
                  isUnitStatusReportRecv = true;
                  unitStatusParser.getStatusReport(statusReport);
                }
                else
                {
                  validationIncomingMessageFailed.setDynamicText("Unit Status");
                  ATC::AbstractEventHandler::corePtr()->reportEvent(validationIncomingMessageFailed, __FILE__, __LINE__);
                }
              }
              else //If Protocol version not matched while receiving OBRD status unit message
              {
                if (connectionTimeoutWithOBRD)
                {
                  trace->write(ATC::briefTrace, "Message other than Protocol Version message received while timedout");
                  connectionTimeoutWithOBRD = false;
                  rejectionReason = OBRDRejectReasonConnectionTimedOut;
                }
                else
                {
                  trace->write(ATC::briefTrace, "Message other than Protocol Version message received before handshaking");
                  //If OBRD Unit Status message sent before the protocol version match 
                  rejectionReason = OBRDRejectReasonProtocolVersionNotInit;
                }
                sendRejectMessage = true;
              }
            }
            else
            {
              //Wrong Message Type received
              ATC::AbstractEventHandler::corePtr()->reportEvent(wrongOBRDTypeMessageRecvd, __FILE__, __LINE__);
              trace->write(ATC::briefTrace, "Invalid OBRD message received");
            }
          }
        }

        // Move any remaining bytes to the start of the input buffer
        uint16_t numOfRemainingBytes = inBufWriteIndex - inBufReadIndex;
        if ((inBufReadIndex > 0U) && (numOfRemainingBytes > 0U))
        {
          memmove(&inBuf[0], &inBuf[inBufReadIndex], numOfRemainingBytes);
        }

        inBufReadIndex = 0U;
        inBufWriteIndex = numOfRemainingBytes;
      }

      // Check timeout towards OBRD
      if ((lastMessageReceivedTime != 0) && ((timeNow - lastMessageReceivedTime) >= connectionLossTimeout))
      {
        lastMessageReceivedTime = 0;

        if (connectedToOBRD)
        {
          trace->write(ATC::briefTrace, "OBRD Connection Timeout");
          // Report event if connection was lost
          ATC::AbstractEventHandler::corePtr()->reportEvent(lostConnectionWithOBRD, __FILE__, __LINE__);
          connectedToOBRD = false;
          protocolVersionMatched = false;
          connectionTimeoutWithOBRD = true;
        }
      }
    }

    /******************************************************************************
    * runOut
    ******************************************************************************/
    void OBRDMessageHandler::runOut()
    {
      OBRDMessageOut *pCreator = static_cast<OBRDMessageOut*>(NULL);

      if (sendProtocolVersionMessage)
      {
        pCreator = &protocolVersionCreator;
      }
      else if (sendRejectMessage)
      {
        pCreator = &rejectMessageCreator;

        rejectMessageCreator.setRejectReason(rejectionReason);
      }
      else
      {
        //do nothing
      }

      if (static_cast<OBRDMessageOut*>(NULL) != pCreator)
      {
        OBRDMessage thisMessage;

        // Assemble data -> Put network message
        if (assembleMessageData(pCreator, thisMessage))
        {
          trace->write(ATC::briefTrace, "Outgoing message validated OK");

          // Pack and convert to network order
          uint8_t outBuf[ATC::maxOBRDMsgSize];
          uint16_t outBufLen = packMessage(thisMessage, &outBuf[0], sizeof(outBuf));

          // Cross compare outgoing data and write message to dispatcher for further processing in Class-D and transmitting to LCS
          crossCompareWriteChannel.putBuffer(&outBuf[0], outBufLen);
          crossCompareWriteChannel.useNextMessage();

          ATC::AbstractLogHandler::corePtr()->logRU(
            ATP::LogHandler::Ifc_OBRD, ATC::AbstractLogHandler::Ifc_Out, &outBuf[0], outBufLen);

          channelStatistics[1].numMsgCnt++;
          channelStatistics[1].numMsgBytesCnt += outBufLen;
        }
      }
    }

    /******************************************************************************
    * extractMessage
    *-----------------------------------------------------------------------------
    * Extract message from characters in inBuf
    * Returns true if message extracted and validate successfully.
    ******************************************************************************/
    bool OBRDMessageHandler::extractMessage(OBRDMessage & msg)
    {
      bool messageExtracted = false;
      bool keepReading = true;
      bool stxFound = true;
      uint16_t messageSize = 0U;
      bool isIllegalMessage = false;

      //fetch configured loco id
      uint16_t configLocoId = AbstractConfig::corePtr()->getRadioId();
      //fetch configured site id
      uint8_t configSiteId = AbstractConfig::corePtr()->getSiteId();

      // Scan inBuf for the first/next <STX> character found in inBuf
      // with sufficient amount of available characters to be a candidate for a valid message

      uint16_t availableChars = inBufWriteIndex - inBufReadIndex;
      uint8_t* posStart = &inBuf[inBufReadIndex];
      // Use vfw-functions to convert from network order to host order
      //initialize to avoid lint error.
      VFW_Buffer vfwBuffer = {static_cast<VFW_Bufferp>(NULL),static_cast<uint8_t*>(NULL), 0U,0U,0U,0U };

      if(availableChars > 0U)
      {
        vfwInitBuffer(&vfwBuffer, posStart, availableChars);
        // Set buffer in read mode. Valid size is set to the amount of data 
        // filled in the raw_buffer. 
        vfwSetReadBuffer(&vfwBuffer, availableChars);

        // STX
        uint8_t messageStart = vfwGetU8(&vfwBuffer);
        if (messageStart != ATC::STX)
        {
          //prepare the dynamic text to be sent while reporting event.
          validationIncomingMessageFailed.setDynamicText("STX not found");
          ATC::AbstractEventHandler::corePtr()->reportEvent(validationIncomingMessageFailed, __FILE__, __LINE__);
          stxFound = false;
        }
      }
       
      while (stxFound && keepReading &&
        (availableChars > (OBRDMessage::headerSize)))
      {
        uint8_t* crcStart = vfwGetPointer(&vfwBuffer); // For the CRC check

        //message size
        if (vfwGetValidSize(&vfwBuffer) >= 2U)
        {
          messageSize = vfwGetU16(&vfwBuffer);
          if ((messageSize + 1U) <= availableChars)
          {

            //site id
            if (vfwGetValidSize(&vfwBuffer) >= 2U)
            {
              msg.siteId = vfwGetU16(&vfwBuffer);
            }
            else
            {
              isIllegalMessage = true;
            }
            // Receiver ID
            for (uint8_t i = 0U; i < sizeof(msg.receiverId); ++i)
            {

              char_t c = '\0';
              if (vfwGetValidSize(&vfwBuffer) >= 1U)
              {
                c = static_cast<char_t>(vfwGetU8(&vfwBuffer));
              }
              else
              {
                isIllegalMessage = true;
              }

              msg.receiverId[i] = c;

              if (c == '\0')
              {
                break;
              }
            }
            msg.receiverId[sizeof(msg.receiverId) - 1U] = '\0';

            //Sender ID
            for (uint8_t i = 0U; i < sizeof(msg.senderId); ++i)
            {
              char_t c = '\0';
              if (vfwGetValidSize(&vfwBuffer) >= 1U)
              {
                c = static_cast<char_t>(vfwGetU8(&vfwBuffer));
              }
              else
              {
                isIllegalMessage = true;
              }
              msg.senderId[i] = c;

              if (c == '\0')
              {
                break;
              }
            }
            msg.senderId[sizeof(msg.senderId) - 1U] = '\0';

            /* To DO: Need to check the sender and receiver ID with local values
            For now it is hard coded */

            //Loco Id + timestamp + protocol part
            if (vfwGetValidSize(&vfwBuffer) >= (2U + 8U + 4U))
            {
              msg.locoId = vfwGetU16(&vfwBuffer);
              // Extract the 8-byte Time Stamp of sender field 
              msg.tSender = vfwGetU64(&vfwBuffer);
              //Extract the protocol part
              msg.obrdPacketData.packetType = vfwGetU16(&vfwBuffer);
              msg.obrdPacketData.packetSize = vfwGetU16(&vfwBuffer);
            }
            else
            {
              isIllegalMessage = true;
            }

            if (isIllegalMessage)
            {
              //prepare the dynamic text to be sent while reporting event.
              validationIncomingMessageFailed.setDynamicText("Illegal Message");
              ATC::AbstractEventHandler::corePtr()->reportEvent(validationIncomingMessageFailed, __FILE__, __LINE__);

              // Skip this STX byte and search for next occurrence of STX
              inBufReadIndex++;
              stxFound = findStartOfTelegram(availableChars);
              isIllegalMessage = false;
            }
            else
            {
              if (msg.obrdPacketData.packetSize < OBRDDataPacket::headerSize)
              {
                validationIncomingMessageFailed.setDynamicText("RL_PACKET too small");
                ATC::AbstractEventHandler::corePtr()->reportEvent(validationIncomingMessageFailed, __FILE__, __LINE__);
              }
              else if (msg.obrdPacketData.packetSize > sizeof(msg.obrdPacketData.messageData))
              {
                validationIncomingMessageFailed.setDynamicText("RL_PACKET too large");
                ATC::AbstractEventHandler::corePtr()->reportEvent(validationIncomingMessageFailed, __FILE__, __LINE__);
              }
              else
              {
                msg.obrdPacketData.dataLength = (msg.obrdPacketData.packetSize - OBRDDataPacket::headerSize);

                //Packet message data
                vfwCpyToRawBuffer(&(msg.obrdPacketData.messageData[0]), &vfwBuffer, msg.obrdPacketData.dataLength);

                if (vfwGetValidSize(&vfwBuffer) >= 8U)
                {
                  // Check CRC, site_id and loco_id
                  uint64_t crcRecv = vfwGetU64(&vfwBuffer);
                  uint64_t crcCalc = calculateCRC(crcStart, messageSize - OBRDMessage::crcSize);

                  if (crcCalc != crcRecv)
                  {
                    trace->write(ATC::briefTrace, "CRC Mismatch");
                    validationIncomingMessageFailed.setDynamicText("CRC Mismatch");
                    ATC::AbstractEventHandler::corePtr()->reportEvent(validationIncomingMessageFailed, __FILE__, __LINE__);
                  }
                  else if (configSiteId != msg.siteId)
                  {
                    //site id is wrong for the received message, rejecting the message
                    validationIncomingMessageFailed.setDynamicText("Wrong Site Id");
                    ATC::AbstractEventHandler::corePtr()->reportEvent(validationIncomingMessageFailed, __FILE__, __LINE__);
                    sendRejectMessage = true;
                    rejectionReason = OBRDRejectReasonWrongSiteId;
                  }
                  else if (configLocoId != msg.locoId)
                  {
                    //loco id is wrong for the received message rejecting the message
                    validationIncomingMessageFailed.setDynamicText("Wrong Loco Id");
                    ATC::AbstractEventHandler::corePtr()->reportEvent(validationIncomingMessageFailed, __FILE__, __LINE__);
                    sendRejectMessage = true;
                    rejectionReason = OBRDRejectReasonWrongLocomotiveIdentity;
                  }
                  else
                  {
                    messageExtracted = true;
                    keepReading = false;

                    // Consume the valid message
                    inBufReadIndex += messageSize + 1U;
                  }
                }
                else
                {
                  validationIncomingMessageFailed.setDynamicText("Illegal Message");
                  ATC::AbstractEventHandler::corePtr()->reportEvent(validationIncomingMessageFailed, __FILE__, __LINE__);
                }
              }

              if (messageExtracted != true)
              {
                // Skip this STX byte and search for next occurrence of STX
                inBufReadIndex++;
                stxFound = findStartOfTelegram(availableChars);
              }

              //ToDo: To Remove after integration testing
              char_t buf[200];
              const int32_t ret = snprintf(&buf[0], sizeof(buf),
                "OBRD message:: Message Length: %d, Site Id:%d, Sender Id: %s, Receiver Id: %s, LocoId: %d, Sender Time Stamp: %lld, Message Type: %d  ",
                messageSize, msg.siteId, msg.senderId, msg.receiverId, msg.locoId, msg.tSender, msg.obrdPacketData.packetType);

              if ((ret > 0) && (static_cast<size_t>(ret) < sizeof(buf)))
              {
                trace->write(ATC::briefTrace, &buf[0]);
              }
            }
          }
          else
          {
            if ((messageSize + 1U) > (ATC::maxOBRDMsgSize))
            {
              //prepare the dynamic text to be sent while reporting event.
              validationIncomingMessageFailed.setDynamicText("Illegal length");
              ATC::AbstractEventHandler::corePtr()->reportEvent(validationIncomingMessageFailed, __FILE__, __LINE__);

              // Skip this STX byte and search for next occurrence of STX
              inBufReadIndex++;
              stxFound = findStartOfTelegram(availableChars);

            }
            else
            {
              // The whole message hasn't been received yet, so try again in next cycle
              keepReading = false;
            }
          }
        }
        else
        {
          //prepare the dynamic text to be sent while reporting event.
          validationIncomingMessageFailed.setDynamicText("Illegal Message");
          ATC::AbstractEventHandler::corePtr()->reportEvent(validationIncomingMessageFailed, __FILE__, __LINE__);
          // Skip this STX byte and search for next occurrence of STX
          inBufReadIndex++;
          stxFound = findStartOfTelegram(availableChars);
        }
      }

      return messageExtracted;
    }


    /******************************************************************************
    * calculateCRC
    ******************************************************************************/
    uint64_t OBRDMessageHandler::calculateCRC(const uint8_t *start, const uint16_t length) const
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
    * packMessage
    *-----------------------------------------------------------------------------
    * pack outgoing buffer (in network order)
    *
    ******************************************************************************/
    uint16_t OBRDMessageHandler::packMessage(const OBRDMessage& msg, uint8_t * const outBuffer, uint32_t const outBufferSize) const
    {
      // Use vfw-functions to convert from host order to network order
      VFW_Buffer buffer;

      vfwInitBuffer(&buffer, outBuffer, outBufferSize);

      // Pack STX
      vfwPutU8(&buffer, ATC::STX);

      //Save this pointer as start location for CRC calculation
      uint8_t* crcStartPtr = vfwGetPointer(&buffer);

      uint16_t receiverIdLen = static_cast<uint16_t>(strnlen(&msg.receiverId[0], sizeof(msg.receiverId) - 1U));
      uint16_t senderIdLen = static_cast<uint16_t>(strnlen(&msg.senderId[0], sizeof(msg.senderId) - 1U));
      uint16_t crcSize = OBRDMessage::headerSize + (receiverIdLen + 1U) + (senderIdLen + 1U) + msg.obrdPacketData.packetSize;
      uint16_t messageSize = crcSize + OBRDMessage::crcSize;

      // Pack Id
      vfwPutU16(&buffer, messageSize);
      // Pack Site Id
      vfwPutU16(&buffer, msg.siteId);
      // Pack receiver Id
      vfwCpyFromRawBuffer(&buffer, &msg.receiverId[0], receiverIdLen);
      // Null Character
      vfwPutU8(&buffer, 0U);
      // Pack sender Id
      vfwCpyFromRawBuffer(&buffer, &msg.senderId[0], senderIdLen);
      // Null character
      vfwPutU8(&buffer, 0U);
      // Pack data length
      vfwPutU16(&buffer, msg.locoId);
      // Pack time-stamps
      vfwPutU64(&buffer, msg.tSender);

      //Pack Packet Id
      vfwPutU16(&buffer, msg.obrdPacketData.packetType);
      //Pack Packet Size
      vfwPutU16(&buffer, msg.obrdPacketData.packetSize);
      // Pack data
      vfwCpyFromRawBuffer(&buffer, &msg.obrdPacketData.messageData[0], msg.obrdPacketData.dataLength);

      uint64_t crc = calculateCRC(crcStartPtr, crcSize);
      vfwPutU64(&buffer, crc);

      // Get valid length of buffer
      return static_cast<uint16_t> (vfwGetValidSize(&buffer));
    }

    /******************************************************************************
    * assembleMessageData()
    ******************************************************************************/
    bool OBRDMessageHandler::assembleMessageData(OBRDMessageOut* const pCreator, OBRDMessage& msg) const
    {
      //site id
      msg.siteId = AbstractConfig::corePtr()->getSiteId();

      // Receiver ID
      static_cast<void>(vfw_strlcpy(&msg.receiverId[0], receiverId, sizeof(msg.receiverId)));

      //Sender Id
      static_cast<void>(vfw_strlcpy(&msg.senderId[0], senderId, sizeof(msg.senderId)));

      //Loco Id
      msg.locoId = AbstractConfig::corePtr()->getRadioId();

      // Generate new time-stamp
      ATC::getUTCTime(msg.tSender);

      // Data Packet
      bool success = pCreator->assemblePacketData(msg.obrdPacketData);

      return success;
    }

    /******************************************************************************
    * getStatusReport
    ******************************************************************************/
    bool OBRDMessageHandler::getStatusReport(OBRDUnitStatusReport &report) const
    {
      report = statusReport;

      return isUnitStatusReportRecv;
    }

    /******************************************************************************
    * findStartOfTelegram
    ******************************************************************************/
    bool OBRDMessageHandler::findStartOfTelegram(uint16_t & availableBytes)
    {
      bool stxFound = false;

      while (inBufReadIndex < inBufWriteIndex)
      {
        if (ATC::STX == inBuf[inBufReadIndex])
        {
          stxFound = true;
          break;
        }

        inBufReadIndex++;
      }

      availableBytes = inBufWriteIndex - inBufReadIndex;

      return stxFound;
    }
  }
}

