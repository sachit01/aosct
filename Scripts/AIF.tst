-- VectorCAST 6.4v (08/01/17)
-- Test Case Script
-- 
-- Environment    : AIF
-- Unit(s) Under Test: abstract_analyzer_if analyzer_if
-- 
-- Script Features
TEST.SCRIPT_FEATURE:C_DIRECT_ARRAY_INDEXING
TEST.SCRIPT_FEATURE:CPP_CLASS_OBJECT_REVISION
TEST.SCRIPT_FEATURE:MULTIPLE_UUT_SUPPORT
TEST.SCRIPT_FEATURE:MIXED_CASE_NAMES
TEST.SCRIPT_FEATURE:STANDARD_SPACING_R2
TEST.SCRIPT_FEATURE:OVERLOADED_CONST_SUPPORT
TEST.SCRIPT_FEATURE:UNDERSCORE_NULLPTR
TEST.SCRIPT_FEATURE:FULL_PARAMETER_TYPES
TEST.SCRIPT_FEATURE:STRUCT_DTOR_ADDS_POINTER
TEST.SCRIPT_FEATURE:STATIC_HEADER_FUNCS_IN_UUTS
--

-- Subprogram: <<INIT>>

-- Test Case: Instance.001
TEST.SUBPROGRAM:<<INIT>>
TEST.NEW
TEST.NAME:Instance.001
TEST.COMPOUND_ONLY
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::AnalyzerIF::consoleCall

-- Test Case: AnalyzerIF_consoleCall.001
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::consoleCall
TEST.NEW
TEST.NAME:AnalyzerIF_consoleCall.001
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.argc:1
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.argv[0]:<<malloc 5>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.argv[0]:"help"
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractConsole::writeWithNewline.str:"Analyzer      To print the detailed information of Registered Measurement variables"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_consoleCall.002
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::consoleCall
TEST.NEW
TEST.NAME:AnalyzerIF_consoleCall.002
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.argc:1
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.argv[0]:<<malloc 9>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.argv[0]:"analyzer"
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractConsole::writeWithNewline.str:"No registered measurable Value"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.return:true
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_consoleCall.003
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::consoleCall
TEST.NEW
TEST.NAME:AnalyzerIF_consoleCall.003
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.argc:1
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.argv[0]:<<malloc 9>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.argv[0]:"analyzer"
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractConsole::writeWithNewline.str:"Name                            Description                             Unit    MinVal      MaxVal      MeasuredValue","TestData1                       TestData1                               unit    2           25          10          ","speed                           current vehicle speed                   cm/s    -32768      32767       50          ","TestData2                       TestData2                               unit    1           555         25         ","TestData3                       TestData3                               unit    4           90          45          ","TestData4                       TestData4                               unit    2           388         49          ","TestData5                       TestData5                               boolean 0           1           1           ","TestData6                       TestData6                               boolean 0           1           0           ","TestData7                       TestData7                               unit    123         234         100         "
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.return:true
TEST.STUB_VAL_USER_CODE:uut_prototype_stubs.ATC::AbstractAnalyzerIF::EnumValueGetter::getValue.return
<<uut_prototype_stubs.ATC::AbstractAnalyzerIF::EnumValueGetter::getValue.return>> = ( 1 );
TEST.END_STUB_VAL_USER_CODE:
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_consoleCall.004
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::consoleCall
TEST.NEW
TEST.NAME:AnalyzerIF_consoleCall.004
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.argc:1
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.argv[0]:<<malloc 5>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.argv[0]:"ebay"
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_consoleCall.005
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::consoleCall
TEST.NEW
TEST.NAME:AnalyzerIF_consoleCall.005
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.argc:2
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.argv[0]:<<malloc 14>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.argv[0]:"analyzer","help"
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_consoleCall.006
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::consoleCall
TEST.NEW
TEST.NAME:AnalyzerIF_consoleCall.006
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.argc:1
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.argv[0]:<<malloc 9>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.argv[0]:"analyzer"
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractConsole::writeWithNewline.str:"No registered measurable Value"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.return:true
TEST.STUB_VAL_USER_CODE:uut_prototype_stubs.ATC::AbstractAnalyzerIF::EnumValueGetter::getValue.return
<<uut_prototype_stubs.ATC::AbstractAnalyzerIF::EnumValueGetter::getValue.return>> = ( 1 );
TEST.END_STUB_VAL_USER_CODE:
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_consoleCall.007
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::consoleCall
TEST.NEW
TEST.NAME:AnalyzerIF_consoleCall.007
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.argc:1
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.argv[0]:<<malloc 9>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.argv[0]:"analyzer"
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractConsole::writeWithNewline.str:"Name                            Description                             Unit    MinVal      MaxVal      MeasuredValue"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.return:true
TEST.STUB_VAL_USER_CODE:uut_prototype_stubs.ATC::AbstractAnalyzerIF::EnumValueGetter::getValue.return
<<uut_prototype_stubs.ATC::AbstractAnalyzerIF::EnumValueGetter::getValue.return>> = ( 1 );
TEST.END_STUB_VAL_USER_CODE:
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_consoleCall.008
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::consoleCall
TEST.NEW
TEST.NAME:AnalyzerIF_consoleCall.008
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.argc:1
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.argv[0]:<<malloc 9>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.argv[0]:"analyzer"
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::consoleCall.return:true
TEST.STUB_VAL_USER_CODE:uut_prototype_stubs.ATC::AbstractAnalyzerIF::EnumValueGetter::getValue.return
<<uut_prototype_stubs.ATC::AbstractAnalyzerIF::EnumValueGetter::getValue.return>> = ( 1 );
TEST.END_STUB_VAL_USER_CODE:
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::AnalyzerIF::init

-- Test Case: AnalyzerIF_init.001
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::init
TEST.NEW
TEST.NAME:AnalyzerIF_init.001
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::initConnection.return:true
TEST.VALUE:uut_prototype_stubs.vfwGetSide.return:VFW_A_SIDE
TEST.VALUE:uut_prototype_stubs.ATP::AbstractConfig::getAnalyzerIFPort.return:30160
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractBasicIP::initConnection.connectionId:3
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractBasicIP::initConnection.connectionType:ConnectionTypeTcpHost
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractBasicIP::initConnection.portNum:30160
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractBasicIP::initConnection.sendBufSize:4096
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractBasicIP::initConnection.recvBufSize:20
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::init.return:true
TEST.VALUE_USER_CODE:<<ATC::AbstractAnalyzerIF instance>>
<<ATC::AbstractAnalyzerIF instance>> = ( ATC::AbstractAnalyzerIF::corePtr() );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_init.002
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::init
TEST.NEW
TEST.NAME:AnalyzerIF_init.002
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.vfwGetSide.return:VFW_B_SIDE
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::init.return:true
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool

-- Test Case: AnalyzerIF_registerMeasurement_bool.001
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_bool.001
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.nameOfMeasData:"TestData5"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.descr:"TestData5"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.measuredValue[0]:true
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.return:true
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_bool.002
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_bool.002
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.nameOfMeasData:"TestData6"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.descr:"TestData6"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.measuredValue[0]:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.return:true
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_bool.003
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_bool.003
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.descr:<<malloc 4>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.descr:"ch1"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.measuredValue[0]:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.nameOfMeasData
<<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.nameOfMeasData>> = ( NULL );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_bool.004
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_bool.004
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.nameOfMeasData:"TestData6"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.descr:<<malloc 4>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.descr:"ch6"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.measuredValue[0]:false
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"MeasureList is full"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.return:(50)true,false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_bool.005
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_bool.005
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.nameOfMeasData:"TestData5"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.measuredValue[0]:true
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.descr
<<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.descr>> = ( NULL );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_bool.006
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_bool.006
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.nameOfMeasData:"TestData5"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.descr:"TestData5"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.measuredValue:<<null>>
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_bool.007
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_bool.007
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.nameOfMeasData:<<malloc 4100>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.descr:"TestData5"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.measuredValue[0]:true
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.return:true
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.nameOfMeasData.nameOfMeasData.nameOfMeasData[0]
int i=0;
for(i=0;i<4100;i++){
<<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.nameOfMeasData>>[i] = ( 'B' );
}
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_bool.008
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_bool.008
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.nameOfMeasData:<<malloc 4025>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.descr:"TestData5"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.measuredValue[0]:true
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.return:true
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.nameOfMeasData.nameOfMeasData.nameOfMeasData[0]
int i=0;
for(i=0;i<4025;i++){
<<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool.nameOfMeasData>>[i] = ( 'B' );
}
TEST.END_VALUE_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool

-- Test Case: AnalyzerIF_registerMeasurement_int16.001
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_int16.001
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.nameOfMeasData:<<malloc 6>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.nameOfMeasData:"speed"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.descr:<<malloc 22>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.descr:"current vehicle speed"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.unit:<<malloc 5>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.unit:"cm/s"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.minValue:-32768
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.maxValue:32767
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.measuredValue[0]:50
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.return:true
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_int16.002
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_int16.002
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.descr:<<malloc 4>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.descr:"ch1"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.unit:<<malloc 22>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.unit:"Current Vehicle Speed"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.minValue:-32768
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.maxValue:32767
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.measuredValue[0]:50
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.nameOfMeasData
<<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.nameOfMeasData>> = ( NULL );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_int16.003
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_int16.003
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.nameOfMeasData:<<malloc 6>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.nameOfMeasData:"speed"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.descr:<<malloc 22>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.descr:"current vehicle speed"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.unit:<<malloc 5>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.unit:"cm/s"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.minValue:-32768
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.maxValue:32767
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.measuredValue[0]:50
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"MeasureList is full"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.return:(50)true,false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_int16.004
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_int16.004
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.nameOfMeasData:<<malloc 6>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.nameOfMeasData:"Test2"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.unit:<<malloc 4>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.unit:"sec"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.minValue:-32768
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.maxValue:32767
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.measuredValue[0]:50
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.descr
<<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.descr>> = ( NULL );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_int16.005
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_int16.005
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.nameOfMeasData:<<malloc 6>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.nameOfMeasData:"Test2"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.descr:<<malloc 4>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.descr:"ch1"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.minValue:-32768
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.maxValue:32767
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.measuredValue[0]:50
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.unit
<<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.unit>> = ( NULL );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_int16.006
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_int16.006
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.nameOfMeasData:<<malloc 6>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.nameOfMeasData:"Test2"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.descr:<<malloc 4>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.descr:"ch1"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.unit:<<malloc 4>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.unit:"sec"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.minValue:-32768
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.maxValue:32767
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.measuredValue:<<null>>
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool

-- Test Case: AnalyzerIF_registerMeasurement_int32.001
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_int32.001
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.nameOfMeasData:"TestData3"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.descr:"TestData3"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.unit:<<malloc 5>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.unit:"unit"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.minValue:4
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.maxValue:90
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.measuredValue[0]:45
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.return:true
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_int32.002
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_int32.002
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.descr:"TestData3"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.unit:<<malloc 5>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.unit:"unit"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.minValue:4
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.maxValue:90
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.measuredValue[0]:45
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.nameOfMeasData
<<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.nameOfMeasData>> = ( NULL );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_int32.003
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_int32.003
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.nameOfMeasData:"TestData3"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.descr:"TestData3"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.unit:<<malloc 5>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.unit:"unit"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.minValue:4
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.maxValue:90
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.measuredValue[0]:45
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"MeasureList is full"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.return:(50)true,false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_int32.004
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_int32.004
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.nameOfMeasData:"TestData3"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.unit:<<malloc 5>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.unit:"unit"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.minValue:4
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.maxValue:90
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.measuredValue[0]:45
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.descr
<<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.descr>> = ( NULL );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_int32.005
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_int32.005
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.nameOfMeasData:"TestData3"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.descr:"TestData3"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.minValue:4
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.maxValue:90
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.measuredValue[0]:45
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.unit
<<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.unit>> = ( NULL );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_int32.006
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_int32.006
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.nameOfMeasData:"TestData3"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.descr:"TestData3"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.unit:<<malloc 5>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.unit:"unit"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.minValue:4
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.maxValue:90
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.measuredValue:<<null>>
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool

-- Test Case: AnalyzerIF_registerMeasurement_uint16.001
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_uint16.001
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.nameOfMeasData:"TestData2"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.descr:"TestData2"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.unit:<<malloc 5>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.unit:"unit"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.minValue:1
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.maxValue:555
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.measuredValue[0]:25
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.return:true
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_uint16.002
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_uint16.002
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.descr:"TestData2"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.unit:<<malloc 5>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.unit:"unit"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.minValue:1
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.maxValue:555
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.measuredValue[0]:25
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.nameOfMeasData
<<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.nameOfMeasData>> = ( NULL );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_uint16.003
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_uint16.003
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.nameOfMeasData:"TestData2"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.descr:"TestData2"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.unit:<<malloc 5>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.unit:"unit"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.minValue:1
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.maxValue:555
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.measuredValue[0]:25
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"MeasureList is full"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.return:(50)true,false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_uint16.004
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_uint16.004
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.nameOfMeasData:"TestData2"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.unit:<<malloc 5>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.unit:"unit"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.minValue:1
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.maxValue:555
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.measuredValue[0]:25
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.descr
<<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.descr>> = ( NULL );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_uint16.005
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_uint16.005
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.nameOfMeasData:"TestData2"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.descr:"TestData2"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.minValue:1
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.maxValue:555
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.measuredValue[0]:25
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.unit
<<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.unit>> = ( NULL );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_uint16.006
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_uint16.006
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.nameOfMeasData:"TestData2"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.descr:"TestData2"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.unit:<<malloc 5>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.unit:"unit"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.minValue:1
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.maxValue:555
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.measuredValue:<<null>>
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool

-- Test Case: AnalyzerIF_registerMeasurement_enum.001
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_enum.001
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.nameOfMeasData:"TestData7"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.descr:"TestData7"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.unit:<<malloc 5>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.unit:"unit"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.minValue:123
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.maxValue:234
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.return:true
TEST.VALUE_USER_CODE:<<testcase>>
class AbstractPosition 
    {
    public:

   enum PosAccuracyState
    {
      
      PosUnknown = 0,
    
      PosApprox = 1,
     
      PosKnown = 2,
      
      PosDoubtfull = 3
    };

PosAccuracyState  accuracyState;


   PosAccuracyState AbstractPosition::getAccuracyState(void) const
    {
     return accuracyState;
   }
};
class AccuracyStateGetter : public ATC::AbstractAnalyzerIF::EnumValueGetter
{ 

public:

AccuracyStateGetter(const AbstractPosition* const abstractPos) 
        : abstractPosition(abstractPos)
{}
virtual unsigned int getValue() const {return static_cast<unsigned int>(abstractPosition->getAccuracyState());}
private:
     AccuracyStateGetter()
{int a;};
    const AbstractPosition* abstractPosition;
};
AccuracyStateGetter abc;
AccuracyStateGetter *accuracyStateGetter;
accuracyStateGetter= &(abc);

<<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.valueGetter>> = (&abc);

TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.valueGetter
class TestGetter : public ATC::AbstractAnalyzerIF::EnumValueGetter 
{ 
private: 
int testValue; 
public: 
TestGetter(int value) 
{ 
testValue=value; 
} 
virtual uint32_t getValue() const 
{ 
return testValue; 
} 
}; 
TestGetter* getTest=new TestGetter(100); <<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.valueGetter>> = ( getTest );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_enum.002
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_enum.002
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.descr:<<malloc 4>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.descr:"ch1"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.unit:<<malloc 3>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.unit:"kg"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.minValue:123
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.maxValue:234
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.nameOfMeasData
<<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.nameOfMeasData>> = ( NULL );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.valueGetter
class TestGetter : public ATC::AbstractAnalyzerIF::EnumValueGetter 
{ 
private: 
int testValue; 
public: 
TestGetter(int value) 
{ 
testValue=value; 
} 
virtual uint32_t getValue() const 
{ 
return testValue; 
} 
}; 
TestGetter* getTest=new TestGetter(100); <<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.valueGetter>> = ( getTest );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_enum.003
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_enum.003
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.nameOfMeasData:"TestData7"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.unit:<<malloc 5>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.unit:"unit"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.minValue:123
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.maxValue:234
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.descr
<<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.descr>> = ( NULL );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.valueGetter
class TestGetter : public ATC::AbstractAnalyzerIF::EnumValueGetter 
{ 
private: 
int testValue; 
public: 
TestGetter(int value) 
{ 
testValue=value; 
} 
virtual uint32_t getValue() const 
{ 
return testValue; 
} 
}; 
TestGetter* getTest=new TestGetter(100); <<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.valueGetter>> = ( getTest );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_enum.004
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_enum.004
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.nameOfMeasData:"TestData7"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.descr:"TestData7"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.minValue:123
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.maxValue:234
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.unit
<<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.unit>> = ( NULL );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.valueGetter
class TestGetter : public ATC::AbstractAnalyzerIF::EnumValueGetter 
{ 
private: 
int testValue; 
public: 
TestGetter(int value) 
{ 
testValue=value; 
} 
virtual uint32_t getValue() const 
{ 
return testValue; 
} 
}; 
TestGetter* getTest=new TestGetter(100); <<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.valueGetter>> = ( getTest );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_enum.005
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_enum.005
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.nameOfMeasData:"TestData7"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.descr:"TestData7"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.unit:<<malloc 5>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.unit:"unit"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.minValue:123
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.maxValue:234
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.valueGetter
<<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.valueGetter>> = ( NULL );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_enum.006
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_enum.006
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.nameOfMeasData:"TestData7"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.descr:"TestData7"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.unit:<<malloc 5>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.unit:"unit"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.minValue:123
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.maxValue:234
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"MeasureList is full"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.return:(50)true,false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.valueGetter
class TestGetter : public ATC::AbstractAnalyzerIF::EnumValueGetter 
{ 
private: 
int testValue; 
public: 
TestGetter(int value) 
{ 
testValue=value; 
} 
virtual uint32_t getValue() const 
{ 
return testValue; 
} 
}; 
TestGetter* getTest=new TestGetter(100); <<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool.valueGetter>> = ( getTest );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool

-- Test Case: AnalyzerIF_registerMeasurement_uint32.001
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_uint32.001
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.nameOfMeasData:"TestData4"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.descr:"TestData4"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.unit:<<malloc 5>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.unit:"unit"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.minValue:2
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.maxValue:388
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.measuredValue[0]:49
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.return:true
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = (&ATP::AnalyzerIF::instance());
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_uint32.002
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_uint32.002
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.descr:"TestData4"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.unit:<<malloc 5>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.unit:"unit"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.minValue:2
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.maxValue:388
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.measuredValue[0]:49
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = (&ATP::AnalyzerIF::instance());
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.nameOfMeasData
<<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.nameOfMeasData>> = ( NULL );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_uint32.003
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_uint32.003
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.nameOfMeasData:"TestData4"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.descr:"TestData4"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.unit:<<malloc 5>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.unit:"unit"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.minValue:2
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.maxValue:388
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.measuredValue[0]:49
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"MeasureList is full"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.return:(50)true,false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = (&ATP::AnalyzerIF::instance());
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_uint32.004
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_uint32.004
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.nameOfMeasData:"TestData4"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.unit:<<malloc 5>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.unit:"unit"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.minValue:2
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.maxValue:388
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.measuredValue[0]:49
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = (&ATP::AnalyzerIF::instance());
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.descr
<<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.descr>> = ( NULL );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_uint32.005
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_uint32.005
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.nameOfMeasData:"TestData4"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.descr:"TestData4"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.minValue:2
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.maxValue:388
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.measuredValue[0]:49
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = (&ATP::AnalyzerIF::instance());
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.unit
<<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.unit>> = ( NULL );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_uint32.006
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_uint32.006
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.nameOfMeasData:"TestData4"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.descr:"TestData4"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.unit:<<malloc 5>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.unit:"unit"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.minValue:2
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.maxValue:388
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.measuredValue:<<null>>
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = (&ATP::AnalyzerIF::instance());
TEST.END_VALUE_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool

-- Test Case: AnalyzerIF_registerMeasurement_uint8.001
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_uint8.001
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.nameOfMeasData:"TestData1"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.descr:"TestData1"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.unit:<<malloc 5>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.unit:"unit"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.minValue:2
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.maxValue:25
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.measuredValue[0]:10
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.return:true
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_uint8.002
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_uint8.002
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.descr:"TestData1"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.unit:<<malloc 5>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.unit:"unit"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.minValue:2
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.maxValue:25
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.measuredValue[0]:4
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.nameOfMeasData
<<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.nameOfMeasData>> = ( NULL );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_uint8.003
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_uint8.003
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.nameOfMeasData:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.nameOfMeasData:"TestData1"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.descr:<<malloc 10>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.descr:"TestData1"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.unit:<<malloc 5>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.unit:"unit"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.minValue:2
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.maxValue:25
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.measuredValue[0]:10
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"MeasureList is full"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.return:(50)true,false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_uint8.004
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_uint8.004
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.nameOfMeasData:<<malloc 6>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.nameOfMeasData:"Test1"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.unit:<<malloc 3>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.unit:"gm"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.minValue:1
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.maxValue:100
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.measuredValue[0]:20
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.descr
<<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.descr>> = ( NULL );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_uint8.005
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_uint8.005
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.nameOfMeasData:<<malloc 6>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.nameOfMeasData:"Test1"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.descr:<<malloc 4>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.descr:"ch3"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.minValue:1
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.maxValue:100
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.measuredValue:<<malloc 1>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.measuredValue[0]:20
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.unit
<<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.unit>> = ( NULL );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_registerMeasurement_uint8.006
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool
TEST.NEW
TEST.NAME:AnalyzerIF_registerMeasurement_uint8.006
TEST.COMPOUND_ONLY
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.nameOfMeasData:<<malloc 6>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.nameOfMeasData:"Test1"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.descr:<<malloc 4>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.descr:"ch3"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.unit:<<malloc 3>>
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.unit:"gm"
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.minValue:1
TEST.VALUE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.maxValue:100
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Register Measure Data is NULL"
TEST.EXPECTED:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.return:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = ( &ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.VALUE_USER_CODE:analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.measuredValue
<<analyzer_if.(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool.measuredValue>> = ( NULL );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::AnalyzerIF::run

-- Test Case: AnalyzerIF_run.001
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::run
TEST.NEW
TEST.NAME:AnalyzerIF_run.001
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf:"START\n"
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.return:6
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.return:24
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::getConnectionStatus.return:ConnectionStatusConnected
TEST.VALUE:uut_prototype_stubs.vfwGetReferenceTime.return:100
TEST.VALUE:uut_prototype_stubs.ATP::AbstractConfig::corePtr.return:<<null>>
TEST.VALUE:uut_prototype_stubs.ATP::AbstractConfig::getSendCycleAIF.return:1
TEST.VALUE:uut_prototype_stubs.ATC::vcast_concrete_AbstractApplicationBase::getApplicationName.return:<<malloc 21>>
TEST.VALUE:uut_prototype_stubs.ATC::vcast_concrete_AbstractApplicationBase::getApplicationName.return:"ATP_APPLICATION_NAME"
TEST.VALUE:uut_prototype_stubs.ATC::vcast_concrete_AbstractApplicationBase::getApplicationVersionString.return:<<malloc 19>>
TEST.VALUE:uut_prototype_stubs.ATC::vcast_concrete_AbstractApplicationBase::getApplicationVersionString.return:"ATP_VERSION_STRING"
TEST.VALUE:uut_prototype_stubs.ATC::AbstractApplicationBase::corePtr.return.ATC::vcast_concrete_AbstractApplicationBase.<<constructor>>.vcast_concrete_AbstractApplicationBase().<<call>>:0
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.connectionId:3
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.bufLength:8#146#,8#624#,8#44#,8#30#,8#33#,8#30#
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractBasicIP::getConnectionStatus.connectionId:3
TEST.STUB_EXP_USER_CODE:uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.buf.buf.buf[0]
char buf[]= "[UnitDataStart]\r\nName ATP_APPLICATION_NAME\r\nVersion ATP_VERSION_STRING\r\nProtocolVer 1\r\n[UnitDataEnd]\r\n";

char bufm[]=("[MeasurablesStart]\r\n\"TestData1\";\"TestData1\";BYTE;\"unit\";2;25\r\n\"speed\";\"current vehicle speed\";SWORD;\"cm/s\";-32768;32767\r\n\"TestData2\";\"TestData2\";WORD;\"unit\";1;555\r\n\"TestData3\";\"TestData3\";SDWORD;\"unit\";4;90\r\n\"TestData4\";\"TestData4\";DWORD;\"unit\";2;388\r\n\"TestData5\";\"TestData5\";BYTE;\"boolean\";0;1\r\n\"TestData6\";\"TestData6\";BYTE;\"boolean\";0;1\r\n\"TestData7\";\"TestData7\";DWORD;\"unit\";123;234\r\n[MeasurablesEnd]\r\n");

char buf1[]="[ParametersStart]\r\n[ParametersEnd]\r\n";
char buf2[]="Unknown command: START\r\n";
char buf3[]= "0;10;50;25;45;49;1;0;100\r\n";

for(int i=0;i <strlen(buf); i++)
{
if( <<uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.buf>>[i] == ( buf[i] ) )
{
printf("\ndata matched %c\n",<<uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.buf>>[i]);
}
else
break;
}

for(int i=0;i <strlen(bufm); i++)
{
if( <<uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.buf>>[i] == ( bufm[i] ) )
{
printf("\ndata matched %c\n",<<uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.buf>>[i]);
}
else
break;
}


for(int i=0;i <strlen(buf1); i++)
{
if( <<uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.buf>>[i] == ( buf1[i] ) )
{
printf("\ndata matched %c\n",<<uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.buf>>[i]);
}
else 
break;
}
for(int i=0;i <strlen(buf2); i++)
{
if( <<uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.buf>>[i] == ( buf2[i] ) )
{
printf("\ndata matched %c\n",<<uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.buf>>[i]);
}
else 
break;
}

for(int i=0;i <strlen(buf3); i++)
{
if( <<uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.buf>>[i] == ( buf3[i] ) )
{
printf("\ndata matched %c\n",<<uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.buf>>[i]);
}
else 
break;
}

for(int i=0;i <strlen(buf2); i++)
{
if( <<uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.buf>>[i] == ( buf2[i] ) )
{
printf("\ndata matched %c\n",<<uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.buf>>[i]);
}
else 
break;
}



TEST.END_STUB_EXP_USER_CODE:
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = (&ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_run.002
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::run
TEST.NEW
TEST.NAME:AnalyzerIF_run.002
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[0]:83
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[1]:84
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[2]:79
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[3]:80
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[4]:10
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.return:6
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.return:18
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::getConnectionStatus.return:ConnectionStatusConnected
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.connectionId:3
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.buf:"Unknown command: STOP\r\n"
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.bufLength:23
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = (&ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_run.003
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::run
TEST.NEW
TEST.NAME:AnalyzerIF_run.003
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::getConnectionStatus.return:ConnectionStatusDisconnected
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = (&ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_run.004
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::run
TEST.NEW
TEST.NAME:AnalyzerIF_run.004
TEST.COMPOUND_ONLY
TEST.VALUE:<<OPTIONS>>.MULTI_RETURN_SPANS_RANGE:TRUE
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::corePtr.return.ATC::vcast_concrete_AbstractBasicIP.<<constructor>>.vcast_concrete_AbstractBasicIP(ATC::AbstractBasicIP::ConnectionControlBlock*const,const uint8_t).<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.return:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.return:24
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::getConnectionStatus.return:ConnectionStatusConnected
TEST.VALUE:uut_prototype_stubs.vfwGetReferenceTime.return:100
TEST.VALUE:uut_prototype_stubs.ATP::AbstractConfig::corePtr.return:<<null>>
TEST.VALUE:uut_prototype_stubs.ATP::AbstractConfig::getSendCycleAIF.return:1
TEST.VALUE:uut_prototype_stubs.ATC::AbstractApplicationBase::corePtr.return.ATC::vcast_concrete_AbstractApplicationBase.<<constructor>>.vcast_concrete_AbstractApplicationBase().<<call>>:0
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.connectionId:3
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = (&ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_run.005
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::run
TEST.NEW
TEST.NAME:AnalyzerIF_run.005
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::corePtr.return.ATC::vcast_concrete_AbstractBasicIP.<<constructor>>.vcast_concrete_AbstractBasicIP(ATC::AbstractBasicIP::ConnectionControlBlock*const,const uint8_t).<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[0]:83
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[1]:84
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[2]:65
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[3]:82
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[4]:84
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[5]:10
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.return:6
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::getConnectionStatus.return:ConnectionStatusConnected
TEST.VALUE:uut_prototype_stubs.vfwGetReferenceTime.return:100
TEST.VALUE:uut_prototype_stubs.ATP::AbstractConfig::corePtr.return:<<null>>
TEST.VALUE:uut_prototype_stubs.ATP::AbstractConfig::getSendCycleAIF.return:1
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = (&ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_run.006
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::run
TEST.NEW
TEST.NAME:AnalyzerIF_run.006
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.connectionId:1
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[0]:83
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[1]:84
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[2]:65
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[3]:82
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[4]:84
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[5]:10
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.return:6
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.return:24
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::getConnectionStatus.connectionId:1
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::getConnectionStatus.return:ConnectionStatusDisconnected
TEST.VALUE:uut_prototype_stubs.vfwGetReferenceTime.return:100
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = (&ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_run.007
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::run
TEST.NEW
TEST.NAME:AnalyzerIF_run.007
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::corePtr.return.ATC::vcast_concrete_AbstractBasicIP.<<constructor>>.vcast_concrete_AbstractBasicIP(ATC::AbstractBasicIP::ConnectionControlBlock*const,const uint8_t).<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[0]:83
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[1]:84
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[2]:65
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[3]:82
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[4]:84
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[5]:13
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.return:6
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.return:24
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::getConnectionStatus.return:ConnectionStatusConnected
TEST.VALUE:uut_prototype_stubs.vfwGetReferenceTime.return:100
TEST.VALUE:uut_prototype_stubs.ATP::AbstractConfig::getSendCycleAIF.return:1
TEST.VALUE:uut_prototype_stubs.ATC::vcast_concrete_AbstractApplicationBase::getApplicationName.return:<<malloc 4100>>
TEST.VALUE:uut_prototype_stubs.ATC::vcast_concrete_AbstractApplicationBase::getApplicationVersionString.return:<<malloc 4100>>
TEST.VALUE:uut_prototype_stubs.ATC::AbstractApplicationBase::corePtr.return.ATC::vcast_concrete_AbstractApplicationBase.<<constructor>>.vcast_concrete_AbstractApplicationBase().<<call>>:0
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.connectionId:3
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.buf:"[UnitDataStart]\r\nProtocolVer 1\r\n[UnitDataEnd]\r\n","[ParametersStart]\r\n[ParametersEnd]\r\n"
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.bufLength:8#57#,8#44#
TEST.STUB_VAL_USER_CODE:uut_prototype_stubs.ATC::vcast_concrete_AbstractApplicationBase::getApplicationName.return.return.return[0]
for(int i=0;i<4100;i++){
<<uut_prototype_stubs.ATC::vcast_concrete_AbstractApplicationBase::getApplicationName.return>>[i] = ( 'a' );}
TEST.END_STUB_VAL_USER_CODE:
TEST.STUB_VAL_USER_CODE:uut_prototype_stubs.ATC::vcast_concrete_AbstractApplicationBase::getApplicationVersionString.return.return.return[0]
for(int i=0;i<4100;i++){<<uut_prototype_stubs.ATC::vcast_concrete_AbstractApplicationBase::getApplicationVersionString.return>>[i] = ( 'b' );}
TEST.END_STUB_VAL_USER_CODE:
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = (&ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_run.008
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::run
TEST.NEW
TEST.NAME:AnalyzerIF_run.008
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::corePtr.return.ATC::vcast_concrete_AbstractBasicIP.<<constructor>>.vcast_concrete_AbstractBasicIP(ATC::AbstractBasicIP::ConnectionControlBlock*const,const uint8_t).<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[0]:1
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[1]:1
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[2]:1
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[3]:1
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[4]:11
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[5]:1
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[6]:1
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[7]:1
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[8]:1
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[9]:1
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[10]:1
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[11]:1
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[12]:1
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[13]:1
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[14]:1
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[15]:1
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[16]:1
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[17]:1
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[18]:1
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[19]:13
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.return:20
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.return:24
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::getConnectionStatus.return:ConnectionStatusConnected
TEST.VALUE:uut_prototype_stubs.vfwGetReferenceTime.return:100
TEST.VALUE:uut_prototype_stubs.ATP::AbstractConfig::getSendCycleAIF.return:1
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = (&ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_run.009
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::run
TEST.NEW
TEST.NAME:AnalyzerIF_run.009
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::corePtr.return.ATC::vcast_concrete_AbstractBasicIP.<<constructor>>.vcast_concrete_AbstractBasicIP(ATC::AbstractBasicIP::ConnectionControlBlock*const,const uint8_t).<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[0]:83
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[1]:84
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[2]:65
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[3]:82
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[4]:84
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[5]:13
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.return:6
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.return:24
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::getConnectionStatus.return:ConnectionStatusConnected
TEST.VALUE:uut_prototype_stubs.vfwGetReferenceTime.return:100
TEST.VALUE:uut_prototype_stubs.ATP::AbstractConfig::getSendCycleAIF.return:1
TEST.VALUE:uut_prototype_stubs.ATC::vcast_concrete_AbstractApplicationBase::getApplicationName.return:<<malloc 4070>>
TEST.VALUE:uut_prototype_stubs.ATC::vcast_concrete_AbstractApplicationBase::getApplicationVersionString.return:<<malloc 4070>>
TEST.VALUE:uut_prototype_stubs.ATC::AbstractApplicationBase::corePtr.return.ATC::vcast_concrete_AbstractApplicationBase.<<constructor>>.vcast_concrete_AbstractApplicationBase().<<call>>:0
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.connectionId:3
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.buf:"[ParametersStart]\r\n[ParametersEnd]\r\n"
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.bufLength:8#44#
TEST.STUB_VAL_USER_CODE:uut_prototype_stubs.ATC::vcast_concrete_AbstractApplicationBase::getApplicationName.return.return.return[0]
for(int i=0;i<4070;i++){
<<uut_prototype_stubs.ATC::vcast_concrete_AbstractApplicationBase::getApplicationName.return>>[i] = ( 'a' );}
TEST.END_STUB_VAL_USER_CODE:
TEST.STUB_VAL_USER_CODE:uut_prototype_stubs.ATC::vcast_concrete_AbstractApplicationBase::getApplicationVersionString.return.return.return[0]
for(int i=0;i<4070;i++){<<uut_prototype_stubs.ATC::vcast_concrete_AbstractApplicationBase::getApplicationVersionString.return>>[i] = ( 'b' );}
TEST.END_STUB_VAL_USER_CODE:
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = (&ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: AnalyzerIF_run.010
TEST.UNIT:analyzer_if
TEST.SUBPROGRAM:(cl)ATP::AnalyzerIF::run
TEST.NEW
TEST.NAME:AnalyzerIF_run.010
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::corePtr.return.ATC::vcast_concrete_AbstractBasicIP.<<constructor>>.vcast_concrete_AbstractBasicIP(ATC::AbstractBasicIP::ConnectionControlBlock*const,const uint8_t).<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[0]:83
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[1]:84
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[2]:65
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[3]:82
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[4]:84
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.buf[5]:13
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::readBuf.return:6
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.return:24
TEST.VALUE:uut_prototype_stubs.ATC::AbstractBasicIP::getConnectionStatus.return:ConnectionStatusConnected
TEST.VALUE:uut_prototype_stubs.vfwGetReferenceTime.return:100
TEST.VALUE:uut_prototype_stubs.ATP::AbstractConfig::getSendCycleAIF.return:1
TEST.VALUE:uut_prototype_stubs.ATC::vcast_concrete_AbstractApplicationBase::getApplicationName.return:<<malloc 3035>>
TEST.VALUE:uut_prototype_stubs.ATC::vcast_concrete_AbstractApplicationBase::getApplicationVersionString.return:<<malloc 3100>>
TEST.VALUE:uut_prototype_stubs.ATC::AbstractApplicationBase::corePtr.return.ATC::vcast_concrete_AbstractApplicationBase.<<constructor>>.vcast_concrete_AbstractApplicationBase().<<call>>:0
TEST.EXPECTED:USER_GLOBALS_VCAST.<<GLOBAL>>.IS_ERROR_REPORTED:false
TEST.EXPECTED:uut_prototype_stubs.ATC::AbstractBasicIP::writeBuf.connectionId:3
TEST.EXPECTED:uut_prototype_stubs.ATC::BaseComponent::writeToLog.text:"Analyzer IF : BasicIP Write failed!"
TEST.STUB_VAL_USER_CODE:uut_prototype_stubs.ATC::vcast_concrete_AbstractApplicationBase::getApplicationName.return.return.return[0]
for(int i=0;i<3035;i++){
<<uut_prototype_stubs.ATC::vcast_concrete_AbstractApplicationBase::getApplicationName.return>>[i] = ( 'a' );}
TEST.END_STUB_VAL_USER_CODE:
TEST.STUB_VAL_USER_CODE:uut_prototype_stubs.ATC::vcast_concrete_AbstractApplicationBase::getApplicationVersionString.return.return.return[0]
for(int i=0;i<3100;i++){<<uut_prototype_stubs.ATC::vcast_concrete_AbstractApplicationBase::getApplicationVersionString.return>>[i] = ( 'b' );}
TEST.END_STUB_VAL_USER_CODE:
TEST.VALUE_USER_CODE:<<ATP::AnalyzerIF instance>>
<<ATP::AnalyzerIF instance>> = (&ATP::AnalyzerIF::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0001_Report_Informtion
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "2", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool", "1", "AnalyzerIF_registerMeasurement_uint8.001"
TEST.SLOT: "4", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool", "1", "AnalyzerIF_registerMeasurement_int16.001"
TEST.SLOT: "5", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool", "1", "AnalyzerIF_registerMeasurement_uint16.001"
TEST.SLOT: "6", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool", "1", "AnalyzerIF_registerMeasurement_int32.001"
TEST.SLOT: "7", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool", "1", "AnalyzerIF_registerMeasurement_uint32.001"
TEST.SLOT: "8", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool", "1", "AnalyzerIF_registerMeasurement_bool.001"
TEST.SLOT: "9", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool", "1", "AnalyzerIF_registerMeasurement_bool.002"
TEST.SLOT: "10", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool", "1", "AnalyzerIF_registerMeasurement_enum.001"
TEST.SLOT: "11", "analyzer_if", "(cl)ATP::AnalyzerIF::run", "4", "AnalyzerIF_run.001"
TEST.SLOT: "12", "analyzer_if", "(cl)ATP::AnalyzerIF::run", "2", "AnalyzerIF_run.002"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0002_Console_Help_Command
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::consoleCall", "1", "AnalyzerIF_consoleCall.001"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0003_Console_Analyzer_Command
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::consoleCall", "1", "AnalyzerIF_consoleCall.006"
TEST.SLOT: "4", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool", "1", "AnalyzerIF_registerMeasurement_uint8.001"
TEST.SLOT: "5", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool", "1", "AnalyzerIF_registerMeasurement_int16.001"
TEST.SLOT: "6", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool", "1", "AnalyzerIF_registerMeasurement_uint16.001"
TEST.SLOT: "7", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool", "1", "AnalyzerIF_registerMeasurement_int32.001"
TEST.SLOT: "8", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool", "1", "AnalyzerIF_registerMeasurement_uint32.001"
TEST.SLOT: "9", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool", "1", "AnalyzerIF_registerMeasurement_bool.001"
TEST.SLOT: "10", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool", "1", "AnalyzerIF_registerMeasurement_bool.002"
TEST.SLOT: "11", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool", "1", "AnalyzerIF_registerMeasurement_enum.001"
TEST.SLOT: "12", "analyzer_if", "(cl)ATP::AnalyzerIF::consoleCall", "1", "AnalyzerIF_consoleCall.003"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0004_Console_analyzer_Command_NoData
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::consoleCall", "1", "AnalyzerIF_consoleCall.002"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0005_Console_Invalid_Command
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::consoleCall", "1", "AnalyzerIF_consoleCall.004"
TEST.SLOT: "4", "analyzer_if", "(cl)ATP::AnalyzerIF::consoleCall", "1", "AnalyzerIF_consoleCall.005"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0006_Invalid_RegisterData_uint8
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool", "1", "AnalyzerIF_registerMeasurement_uint8.002"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0007_Invalid_RegisterData_int16
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool", "1", "AnalyzerIF_registerMeasurement_int16.002"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0008_Invalid_RegisterData_uint16
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool", "1", "AnalyzerIF_registerMeasurement_uint16.002"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0009_Invalid_RegisterData_int32
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool", "1", "AnalyzerIF_registerMeasurement_int32.002"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0010_Invalid_RegisterData_uint32
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool", "1", "AnalyzerIF_registerMeasurement_uint32.002"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0011_MeasurelistFull_uint8
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool", "51", "AnalyzerIF_registerMeasurement_uint8.003"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0012_MeasurelistFull_int16
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool", "51", "AnalyzerIF_registerMeasurement_int16.003"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0013_MeasurelistFull_uint16
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool", "51", "AnalyzerIF_registerMeasurement_uint16.003"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0014_MeasurelistFull_int32
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool", "51", "AnalyzerIF_registerMeasurement_int32.003"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0015_MeasurelistFull_uint32
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool", "51", "AnalyzerIF_registerMeasurement_uint32.003"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0016_MeasurelistFull_Bool
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool", "51", "AnalyzerIF_registerMeasurement_bool.004"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0017_MeasurelistFull_enum
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool", "51", "AnalyzerIF_registerMeasurement_enum.006"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0018_Invalid_RegisterData_Bool
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool", "1", "AnalyzerIF_registerMeasurement_bool.003"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0019_Invalid_RegisterData_enum
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool", "1", "AnalyzerIF_registerMeasurement_enum.002"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0020_Invalid_RegisterData_uint8.001
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool", "1", "AnalyzerIF_registerMeasurement_uint8.004"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0021_Invalid_RegisterData_uint8.002
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool", "1", "AnalyzerIF_registerMeasurement_uint8.005"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0022_Invalid_RegisterData_uint8.003
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint8_t,const uint8_t,const uint8_t*const)bool", "1", "AnalyzerIF_registerMeasurement_uint8.006"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0023_Invalid_RegisterData_int16.001
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool", "1", "AnalyzerIF_registerMeasurement_int16.004"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0024_Invalid_RegisterData_int16.002
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool", "1", "AnalyzerIF_registerMeasurement_int16.005"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0025_Invalid_RegisterData_int16.003
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int16_t,const int16_t,const int16_t*const)bool", "1", "AnalyzerIF_registerMeasurement_int16.006"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0026_Invalid_RegisterData_uint16.001
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool", "1", "AnalyzerIF_registerMeasurement_uint16.004"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0027_Invalid_RegisterData_uint16.002
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool", "1", "AnalyzerIF_registerMeasurement_uint16.005"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0028_Invalid_RegisterData_uint16.003
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint16_t,const uint16_t,const uint16_t*const)bool", "1", "AnalyzerIF_registerMeasurement_uint16.006"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0029_Invalid_RegisterData_int32.001
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool", "1", "AnalyzerIF_registerMeasurement_int32.004"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0030_Invalid_RegisterData_int32.002
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool", "1", "AnalyzerIF_registerMeasurement_int32.005"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0031_Invalid_RegisterData_int32.003
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const int32_t,const int32_t,const int32_t*const)bool", "1", "AnalyzerIF_registerMeasurement_int32.006"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0032_Invalid_RegisterData_uint32.001
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool", "1", "AnalyzerIF_registerMeasurement_uint32.004"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0033_Invalid_RegisterData_uint32.002
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool", "1", "AnalyzerIF_registerMeasurement_uint32.005"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0034_Invalid_RegisterData_uint32.003
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const uint32_t*const)bool", "1", "AnalyzerIF_registerMeasurement_uint32.006"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0035_Invalid_RegisterData_Bool.001
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool", "1", "AnalyzerIF_registerMeasurement_bool.005"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0036_Invalid_RegisterData_Bool.002
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool", "1", "AnalyzerIF_registerMeasurement_bool.006"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0037_Invalid_RegisterData_enum.001
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool", "1", "AnalyzerIF_registerMeasurement_enum.003"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0038_Invalid_RegisterData_enum.002
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool", "1", "AnalyzerIF_registerMeasurement_enum.004"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0039_Invalid_RegisterData_enum.003
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const char_t*const,const uint32_t,const uint32_t,const ATC::AbstractAnalyzerIF::EnumValueGetter*)bool", "1", "AnalyzerIF_registerMeasurement_enum.005"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0040_Report_Information_aifState
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::run", "2", "AnalyzerIF_run.004"
TEST.SLOT: "4", "analyzer_if", "(cl)ATP::AnalyzerIF::run", "1", "AnalyzerIF_run.005"
TEST.SLOT: "5", "analyzer_if", "(cl)ATP::AnalyzerIF::run", "2", "AnalyzerIF_run.004"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0041_AOS_Not_Connected
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.002"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::run", "1", "AnalyzerIF_run.003"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0042_Reset_OutputBuffer
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool", "1", "AnalyzerIF_registerMeasurement_bool.008"
TEST.SLOT: "4", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool", "1", "AnalyzerIF_registerMeasurement_bool.008"
TEST.SLOT: "5", "analyzer_if", "(cl)ATP::AnalyzerIF::run", "1", "AnalyzerIF_run.007"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0043_Console_MeasureData_Overflow
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool", "1", "AnalyzerIF_registerMeasurement_bool.007"
TEST.SLOT: "4", "analyzer_if", "(cl)ATP::AnalyzerIF::consoleCall", "1", "AnalyzerIF_consoleCall.007"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0044_Invalid_UnitData
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::run", "1", "AnalyzerIF_run.010"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:AIF0045_ProtocolVersion_UnitData_Not_Appended
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Instance.001"
TEST.SLOT: "2", "analyzer_if", "(cl)ATP::AnalyzerIF::init", "1", "AnalyzerIF_init.001"
TEST.SLOT: "3", "analyzer_if", "(cl)ATP::AnalyzerIF::registerMeasurement(const char_t*const,const char_t*const,const bool*const)bool", "1", "AnalyzerIF_registerMeasurement_bool.008"
TEST.SLOT: "4", "analyzer_if", "(cl)ATP::AnalyzerIF::run", "1", "AnalyzerIF_run.009"
TEST.END
--
