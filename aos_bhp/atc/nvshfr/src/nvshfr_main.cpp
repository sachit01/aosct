//*****************************************************************************
//* RESERVATION OF RIGHTS:
//* This document and its contents are the property of
//* Bombardier Inc. or its subsidiaries. This document contains
//* confidential proprietary information. The reproduction,
//* distribution, utilisation or the communication of this
//* document or any part thereof, without express authorization
//* is strictly prohibited. Offenders will be held liable for
//* the payment of damages.
//*
//* (c) 2016-2020, Bombardier Inc. or its subsidiaries. All rights reserved.
//*
//* Module name: NVSH File Reader
//*
//*
//* Description: This file consists of main function of NVSHFR component.
//*
//*******************************************************************************
//* Revision History
//*
//* Version    Date    Sign       Change description
//*
//*  -         200220  rboyapat   arn_043#8568:Updated as per ETC21332.
//* -          200210  anlindel   Minor lint correction.
//* -          200206  anlindel   Corrected issue with "nvshfr -a" function.
//* 2.0        190919  rkongari   Updated Review comments
//* 1.0        190916  rkongari	  arn_043#6330:updated the name of the file and corrected the lint warnings

//* below revision history is related to old main.cpp file

//* 2.1.24     190704   W.Detert  CR 5751 Online Key Management, KMAC.bin is now in target directory /optdata/data,
//*                               nvshfr binary version 5.0.5
//* 2.1.23     190517   vivsharm  Official Build P8e.ETCS_Core (nvshfr) binary version 5.0.4
//* 2.1.22     190514   rkongari  updated IFS ATPCU Config version
//* 2.1.21     190510   vivsharm  Official Build P8e.ETCS_Core (nvshfr) binary version 5.0.3
//* 2.1.20     190509    rkongari updated for arn_043#3540,arn_043#7171
//* 2.1.18.1.2 190503   vivsharm  Official Build P8e.ETCS_Core (nvshfr) binary version 5.0.2
//* 2.1.18.1.1 190503   anlindel  Aligned with config spec version 4.2.
//* 2.1.18     181203   vaggarwa  Official Build P8e.ETCS_Core (nvshfr) binary version 5.0.1
//* 2.1.17     181128   rboyapat  arn_043#5754:N_Installed_Modems added as per new
//*                               config. spec 4.1
//*
//*******************************************************************************

#include <stdint.h>
#include <cstdlib>
#include <unistd.h>
#include <string.h>

#include <iostream>
#include <fstream>
#include <iomanip>

#include "nvshfr_main.hpp"
#include "abstract_data.hpp"
#include "definition_parser.hpp"
#include "bin_parser.hpp"
#include "stream_helper.hpp"
#include "app_error.hpp"

void NVSHFR::printHelp(void)
{
  std::cout << "Usage: NVSHFR [options]\n"
            "Options:\n"
            "-h       Print this help\n"
            "-c       Read out common parameter file\n"
            "-d       Read out dispatcher parameter file\n"
            "-i       Read out instance parameter file\n"
            "-m       Read out maintenance parameter file\n"
            "-r       Read out runtime parameter file\n"
            "-ts      Read out type parameter file\n"
            "-a       Save parameter files to disk\n"
            "-v       Print version\n"
            "\n"
            "-g path  Path to definition files\n";

  std::cout << "\n\nConstants: "
            << std::endl << " dispatcher bin:  "
            << NVSHFR::CFG_DISP_PATH
            << std::endl << " common bin:      "
            << NVSHFR::CFG_CMN_PATH
            << std::endl << " instance bin:    "
            << NVSHFR::CFG_INST_PATH
            << std::endl << " maintenance bin: "
            << NVSHFR::CFG_MNT_PATH
            << std::endl << " runtime bin:     "
            << NVSHFR::CFG_RT_PATH
            << std::endl << " type bin:        "
            << NVSHFR::CFG_TYPE_PATH
            << std::endl << " output path:     "
            << NVSHFR::OUTPUT_PATH
            << std::endl << " definition path: "
            << NVSHFR::DEFINITON_PATH
            << std::endl;
}

bool NVSHFR::parseArguments(const int32_t argc, char * const argv[])
{
  bool isArgParseSuccessful = true;

  conf.print_cmn_data = false;
  conf.print_disp_data = false;
  conf.print_instance_data = false;
  conf.print_mnt_data = false;
  conf.print_rt_data = false;
  conf.print_type_data = false;
  conf.print_everything = true;
  conf.print_to_file = false;
  conf.legacy_mode = false;

  conf.custom_data = false;
  conf.definition_path = DEFINITON_PATH;

  if (argv != NULL)
  {
    for (int32_t i = 1; (i < argc) && isArgParseSuccessful; ++i)
    {
      if (strncmp(argv[i], "-h", 3) == 0)
      {
        printHelp();
        isArgParseSuccessful = false;
      }
      else if (strncmp(argv[i], "-v", 3) == 0)
      {
        std::cout << "NVSHFR version " << NVSHFR_MAJOR << '.' << NVSHFR_MINOR << '.' << NVSHFR_REVISION << std::endl;
        std::cout << "This version of NVSHFR is compatible with the version " << INTERFACE_VERSION
                  << " of NVSH file interface" << std::endl;
        isArgParseSuccessful = false;
      }
      else if (strncmp(argv[i], "-c", 3) == 0)
      {
        conf.print_cmn_data = true;
        conf.print_everything = false;
        conf.legacy_mode = true;
      }
      else if (strncmp(argv[i], "-d", 3) == 0)
      {
        conf.print_disp_data = true;
        conf.print_everything = false;
        conf.legacy_mode = true;
      }
      else if (strncmp(argv[i], "-i", 3) == 0)
      {
        conf.print_instance_data = true;
        conf.print_everything = false;
        conf.legacy_mode = true;
      }
      else if (strncmp(argv[i], "-m", 3) == 0)
      {
        conf.print_mnt_data = true;
        conf.print_everything = false;
        conf.legacy_mode = true;
      }
      else if (strncmp(argv[i], "-r", 3) == 0)
      {
        conf.print_rt_data = true;
        conf.print_everything = false;
        conf.legacy_mode = true;
      }
      else if (strncmp(argv[i], "-ts", 4) == 0)
      {
        conf.print_type_data = true;
        conf.print_everything = false;
        conf.legacy_mode = true;
      }
      else if (strncmp(argv[i], "-a", 3) == 0)
      {
        conf.print_to_file = true;
        conf.legacy_mode = true;
      }
      else if (strncmp(argv[i], "-g", 3) == 0)
      {
        ++i;
        if (i < argc)
        {
          conf.definition_path = argv[i];
        }
        else
        {
          std::cerr << "\nNVSHFR: Invalid argument(s)\n";
          printHelp();
          isArgParseSuccessful = false;
        }
      }
      else
      {
        std::cerr << "\nNVSHFR: Invalid argument(s)\n";
        printHelp();
        isArgParseSuccessful = false;
      }
    }

    if (isArgParseSuccessful == true)
    {
      if (conf.custom_data)
      {
        conf.print_to_file = false;
      }

      if ((conf.legacy_mode == static_cast<bool>(true))
          && (conf.custom_data == static_cast<bool>(true)))
      {
        std::cerr << "Custom data don't work with legacy options\n";
        printHelp();
        isArgParseSuccessful = false;
      }
    }
  }
  else
  {
    std::cerr << "Unable to parse input, terminating application.\n";
    isArgParseSuccessful = false;
  }

  return isArgParseSuccessful;
}

bool NVSHFR::printToStreams(std::istream &definition, std::istream &bin_in,
                            std::ostream &out,
                            const bool dispatcher,
                            std::string section)
{

  bool print_status = false;
  NVSHFR::DefinitionParser parser;
  NVSHFR::AbstractData *paramlist = NULL;

  /* Read definitions */
  (void) parser.readDefinition(definition);

  if (section == "")
  {
    paramlist = parser.build();
  }
  else
  {
    paramlist = parser.build(section);
  }

  if (paramlist == NULL)
  {
    std::cerr
    << "Internal error in printToStreams(): configuration list is NULL\n";
    print_status = false;

  }
  else
  {

    NVSHFR::BinParser binary_parser(paramlist, dispatcher);
    if (!binary_parser.parse(bin_in))
    {
      std::cerr << "Failed parsing bin file, unknown reason\n";
      delete (paramlist);
      print_status = false;
    }
    else
    {
      uint32_t crc = paramlist->getCrc();

      // Scrambled-CRC-value shall be calculated by swapping first two bytes with the last two bytes of CRC
      crc = static_cast<uint32_t>(((crc << 16U) & 0xffff0000U)
                                  | ((crc >> 16U) & 0x0000ffffU));

      (void) paramlist->printCfg(out);
      out << "\nNVSHFR_Version " << NVSHFR_MAJOR << '.' << NVSHFR_MINOR
          << '.'
          << NVSHFR_REVISION << std::endl << std::endl;
      out << "SCR_CRC " << std::setfill('0') << std::setw(8) << std::hex
          << std::uppercase
          << crc << std::dec << std::endl;

      delete (paramlist);
      print_status = true;
    }

  }

  return print_status;
}

bool NVSHFR::printCfg(std::string definitionfile, std::string binfile,
                      std::string outfile,
                      std::string binName,
                      const bool dispatcher,
                      std::string definition_section)
{
  std::fstream fs;
  std::fstream in;
  std::fstream out;
  bool pcfg_status = false;
  if (static_cast<bool>(true) == conf.print_to_file)
  {
    std::cout << "Open bin file '" << binfile << "'\n";
  }

  fs.open(definitionfile.c_str(), std::fstream::in);
  if (fs.fail())
  {
    std::cerr << "NVSHFR: Could not open definition file \""
              << definitionfile
              << "\"\n";
    pcfg_status = false;

  }
  else
  {
    in.open(binfile.c_str(), std::fstream::in | std::fstream::binary);
    if (in.fail())
    {
      std::cerr << "NVSHFR: bin file '" << binName
                << "' does not exist\n";
      pcfg_status = false;
    }
    else
    {
      if (static_cast<bool>(true) == conf.print_to_file)
      {
        out.open(outfile.c_str(), std::fstream::out);
        static_cast<void>(printToStreams(fs, in, out,
                                         dispatcher, definition_section));
        out.close();
      }
      else
      {
        static_cast<void>(printToStreams(fs, in, std::cout,
                                         dispatcher, definition_section));
      }
      pcfg_status = true;

    }
  }
  return pcfg_status;
}

int32_t NVSHFR::printCustomFile(std::string file)
{
  int32_t ret_val = -10;
  std::string type;
  std::string definition_file_name;
  std::string section = "";
  std::fstream fs;
  std::fstream in;

  in.open(file.c_str(), std::fstream::in | std::fstream::binary);
  if (in.fail())
  {
    std::cerr << "NVSHFR: bin file '" << file << "' does not exist\n";
    ret_val = -9;
  }
  else
  {

    static_cast<void>(in.seekg(static_cast<int64_t>(12)));
    type = NVSHFR::StreamHelper::getString(in);
    static_cast<void>(in.seekg(static_cast<int64_t>(0)));

    if (type == CMN_BIN_NAME)
    {
      definition_file_name = conf.definition_path + CMN_DEF_NAME;
    }
    else if (type == DISP_BIN_NAME)
    {
      definition_file_name = conf.definition_path + DISP_DEF_NAME;
    }
    else if (type == INST_BIN_NAME)
    {
      definition_file_name = conf.definition_path + INST_DEF_NAME;
    }
    else if (type == MNT_BIN_NAME)
    {
      definition_file_name = conf.definition_path + MNT_DEF_NAME;
    }
    else if (type == RT_BIN_NAME)
    {
      definition_file_name = conf.definition_path + RT_DEF_NAME;
    }
    else if (type == TYPE_BIN_NAME)
    {
      definition_file_name = conf.definition_path + TYPE_DEF_NAME;
    }

    fs.open(definition_file_name.c_str(), std::fstream::in);
    if (fs.fail())
    {
      std::cerr << "NVSHFR: " << definition_file_name
                << " does not exist\n";
      ret_val = -9;
    }
    else
    {
      static_cast<void>(printToStreams(fs, in, std::cout, type == DISP_BIN_NAME, section));
      ret_val = -10;
    }
  }
  return ret_val;
}

int32_t main(const int32_t argc, char * const argv[])
{
  int32_t retval = 0;
  /* code */

  if (!NVSHFR::parseArguments(argc, argv))
  {
    retval = -3;
  }
  else if (NVSHFR::conf.custom_data)
  {
    retval = NVSHFR::printCustomFile(NVSHFR::conf.binary_file);

  }
  else if ((!NVSHFR::conf.print_to_file) && NVSHFR::conf.print_everything)
  {
    NVSHFR::printHelp();
    retval = -3;
  }
  else
  {
    try
    {
      if ((NVSHFR::conf.print_cmn_data || NVSHFR::conf.print_everything) && (retval == 0)) //lint !e948 !e944 !e774 Retval not neccessary to check, but done any way to avoid problems in future restructure of code.
      {
        if (!NVSHFR::printCfg(NVSHFR::conf.definition_path + NVSHFR::CMN_DEF_NAME,
                              NVSHFR::CFG_CMN_PATH,
                              NVSHFR::OUTPUT_PATH + "CFG_DATA_READOUT.txt",
                              NVSHFR::CMN_BIN_NAME,
                              false))
        {
          retval = -2;
        }
      }

      if ((NVSHFR::conf.print_disp_data || NVSHFR::conf.print_everything) && (retval == 0)) //lint !e948 !e944 !e774 Retval not neccessary to check, but done any way to avoid problems in future restructure of code.
      {
        if (!NVSHFR::printCfg(NVSHFR::conf.definition_path + NVSHFR::DISP_DEF_NAME,
                              NVSHFR::CFG_DISP_PATH,
                              NVSHFR::OUTPUT_PATH + "DISPATCHER_DATA_READOUT.txt",
                              NVSHFR::DISP_BIN_NAME,
                              true))
        {
          retval = -2;
        }
      }

      if ((NVSHFR::conf.print_instance_data || NVSHFR::conf.print_everything) && (retval == 0))
      {
        if (!NVSHFR::printCfg(NVSHFR::conf.definition_path + NVSHFR::INST_DEF_NAME,
                              NVSHFR::CFG_INST_PATH,
                              NVSHFR::OUTPUT_PATH + "INSTANCE_DATA_READOUT.txt",
                              NVSHFR::INST_BIN_NAME,
                              false))
        {
          retval = -2;
        }
      }

      if ((NVSHFR::conf.print_mnt_data || NVSHFR::conf.print_everything) && (retval == 0))
      {
        if (!NVSHFR::printCfg(NVSHFR::conf.definition_path + NVSHFR::MNT_DEF_NAME,
                              NVSHFR::CFG_MNT_PATH,
                              NVSHFR::OUTPUT_PATH + "MNT_DATA_READOUT.txt",
                              NVSHFR::MNT_BIN_NAME,
                              false))
        {
          retval = -2;
        }
      }

      if ((NVSHFR::conf.print_rt_data || NVSHFR::conf.print_everything) && (retval == 0))
      {
        if (!NVSHFR::printCfg(NVSHFR::conf.definition_path + NVSHFR::RT_DEF_NAME,
                              NVSHFR::CFG_RT_PATH,
                              NVSHFR::OUTPUT_PATH + "RT_DATA_READOUT.txt",
                              NVSHFR::RT_BIN_NAME,
                              false))
        {
          retval = -2;
        }
      }

      if ((NVSHFR::conf.print_type_data || NVSHFR::conf.print_everything) && (retval == 0))
      {
        if (!NVSHFR::printCfg(NVSHFR::conf.definition_path + NVSHFR::TYPE_DEF_NAME,
                              NVSHFR::CFG_TYPE_PATH,
                              NVSHFR::OUTPUT_PATH + "TYPE_DATA_READOUT.txt",
                              NVSHFR::TYPE_BIN_NAME,
                              false))
        {
          retval = -2;
        }
      }
    }
    catch (NVSHFR::AppError const &e)
    {
      /* Add generic error texts to satisfy requirements */
      switch (e.getErrorType())
      {
        case NVSHFR::AppError::INVALID_FILE_VERSION: /* ETC21323 */
        case NVSHFR::AppError::INVALID_NVSH_VERSION: /* ETC21322 */
          std::cerr
          << "NVSHFR: Input NVSH file not compatible with version of NVSHFR.\n";
          retval = static_cast<int16_t>(100
                                        + static_cast<int16_t>(e.getErrorType()));
          break;
        case NVSHFR::AppError::CRC64_ERROR: /* ETC21324 */
          std::cerr
          << "NVSHFR: Input NVSH file contains incorrect CRC.\n";
          retval = static_cast<int16_t>(100
                                        + static_cast<int16_t>(e.getErrorType()));
          break;
        case NVSHFR::AppError::GENERIC:
          default:
          std::cerr << e.what();
          retval = static_cast<int16_t>(100
                                        + static_cast<int16_t>(e.getErrorType()));
          break;
      }
    }
  }

  return retval;
}

//*************************** end of file **************************************

