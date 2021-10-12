/**************************************************************************
 *
 * (C) COPYRIGHT Bombardier Transportation Signal Sweden AB, 2019
 *
 * We reserve all rights in this file and in the information
 * contained therein. Reproduction, use or disclosure to third
 * parties without express authority is strictly forbidden.
 *
 * Component name: NVSHFR
 *
 * %name: nvshfr_main.hpp %
 * %version: 2 %
 * %created_by: rkongari %
 * %date_created: 2019-09-19 11:43 %
 *
 * Description: Header file for main
 *
 ***************************************************************************/
/**************************************************************************
 * Revision History
 * Version       Date      Sign      Change Description
 *
 * -          200508  vivsharm       Updated binary version 5.0.13
 * -          200429  mohakuma       DEV 415945 : Implement new RT parameters-
 *                                   CMD_VALID and CMD_DISTANCE
 * -          200306  vivsharm       Updated binary version 5.0.12
 * -          200220  rboyapat       arn_043#8568:Updated as per ETC21332.
 * -          200221  vivsharm       Updated binary version 5.0.11
 * -          200214  vivsharm       Updated binary version 5.0.10
 * -          200207  vivsharm       Updated binary version 5.0.9
 * -          200131  vivsharm       Updated binary version 5.0.8
 * -          200129  rboyapat       arn_043#8569:IFS version for RT parameters changed to 4.0
 * -          191227  vivsharm       Updated version for build 5.0.62
 * -          191212  mohsharm       arn_043#5080,arn_043#8406, arn_043#8531 and arn_043#8508
 *                                   Aligned with Configuration Parameter Specification v4.3
 * -          191215  vivsharm       Updated version for build 5.0.60
 * 2.0        190919  rkongari       Updated Review comments
 * 1.0        190916  rkongari	     Created new file
 ***************************************************************************/
#ifndef NVSHFR_MAIN_HPP_
#define NVSHFR_MAIN_HPP_
namespace NVSHFR
{
/**************************************************************************
 * Includes
 ***************************************************************************/

/**************************************************************************
 * Macro definitions
 ***************************************************************************/

// Constants
// NVSHFR version
const uint32_t NVSHFR_MAJOR = 1U;

const uint32_t NVSHFR_MINOR = 0U;

const uint32_t NVSHFR_REVISION = 0U;

// Interface version
const uint32_t INTERFACE_VERSION = 1U;

#ifdef HOST

const std::string INPUT_PATH = "data_files/";
const std::string OUTPUT_PATH = "data_files/";
const std::string DEFINITON_PATH = "data_files/";

#else

const std::string INPUT_PATH = "/optdata/data/aos/";
const std::string OUTPUT_PATH = "/optdata/data/aos/cfg/";
const std::string DEFINITON_PATH = "/optdata/data/aos/";

#endif

const std::string DISP_BIN_NAME = "dispatcher_data.bin";
const std::string DISP_DEF_NAME = "dispatcher_data_def.txt";

const std::string CMN_BIN_NAME = "cfg_data.bin";
const std::string CMN_DEF_NAME = "cfg_data_def.txt";

const std::string INST_BIN_NAME = "instance_data.bin";
const std::string INST_DEF_NAME = "instance_data_def.txt";

const std::string MNT_BIN_NAME = "mnt_data.bin";
const std::string MNT_DEF_NAME = "mnt_data_def.txt";

const std::string RT_BIN_NAME = "rt_data.bin";
const std::string RT_DEF_NAME = "rt_data_def.txt";

const std::string TYPE_BIN_NAME = "type_data.bin";
const std::string TYPE_DEF_NAME = "type_data_def.txt";

const std::string CFG_DISP_PATH = INPUT_PATH + DISP_BIN_NAME;
const std::string CFG_CMN_PATH  = INPUT_PATH + CMN_BIN_NAME;
const std::string CFG_INST_PATH = INPUT_PATH + INST_BIN_NAME;
const std::string CFG_MNT_PATH  = INPUT_PATH + MNT_BIN_NAME;
const std::string CFG_RT_PATH   = INPUT_PATH + RT_BIN_NAME;
const std::string CFG_TYPE_PATH = INPUT_PATH + TYPE_BIN_NAME;

/**************************************************************************
 * Typedefs
 ***************************************************************************/

/**************************************************************************
 * Data declarations/definitions
 ***************************************************************************/

/*! \brief BcdParameter stores a BCD representation
 *
 * \warning This class behaves badly if input data does not conform to standard
 *
 */

struct config
{
  std::string definition_path;

  std::string binary_file;

  bool print_cmn_data;

  bool print_disp_data;

  bool print_instance_data;

  bool print_mnt_data;

  bool print_rt_data;

  bool print_type_data;

  bool print_to_file;

  bool print_everything;

  bool legacy_mode;

  bool custom_data;

} conf;

void printHelp(void);

bool parseArguments(const int32_t argc, char * const argv[]);

bool printToStreams(std::istream &definition, std::istream &bin_in,
                    std::ostream &out,
                    const bool dispatcher,
                    std::string section = "");

bool printCfg(std::string definitionfile, std::string binfile,
              std::string outfile,
              std::string binName,
              const bool dispatcher,
              std::string definition_section = "");

int32_t printCustomFile(std::string file);

} // namespace NVSHFR

#endif /* NVSHFR_MAIN_HPP_ */
