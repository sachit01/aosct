-- VectorCAST 6.4v (08/01/17)
-- Test Case Script
-- 
-- Environment    : RH
-- Unit(s) Under Test: abstract_log_handler abstract_radio_handler radio_handler
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

-- Test Case: Initialize.001
TEST.SUBPROGRAM:<<INIT>>
TEST.NEW
TEST.NAME:Initialize.001
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.vfwGetSide.return:VFW_A_SIDE
TEST.CONSTRUCTOR_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>.ATP::RadioCom::RadioHandler
<<ATP::RadioCom::RadioHandler instance>> = (&ATP::RadioCom::RadioHandler::instance());
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: Initialize.002
TEST.SUBPROGRAM:<<INIT>>
TEST.NEW
TEST.NAME:Initialize.002
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.vfwGetSide.return:VFW_A_SIDE
TEST.CONSTRUCTOR_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>.ATP::RadioCom::RadioHandler
<<ATP::RadioCom::RadioHandler instance>> = (&ATP::RadioCom::RadioHandler::instance());
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.CONSTRUCTOR_USER_CODE:radio_handler.<<GLOBAL>>.ATP::RadioCom::coreRadioHandlerInstancePtr.ATP::RadioCom::vcast_concrete_AbstractRadioHandler
<<radio_handler.<<GLOBAL>>.ATP::RadioCom::coreRadioHandlerInstancePtr>> = (&ATP::RadioCom::RadioHandler::instance());
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: Initialize.003
TEST.SUBPROGRAM:<<INIT>>
TEST.NEW
TEST.NAME:Initialize.003
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.vfwGetSide.return:VFW_A_SIDE
TEST.CONSTRUCTOR_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>.ATP::RadioCom::RadioHandler
<<ATP::RadioCom::RadioHandler instance>> = (&ATP::RadioCom::RadioHandler::instance());
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.CONSTRUCTOR_USER_CODE:radio_handler.<<GLOBAL>>.ATP::RadioCom::coreRadioHandlerInstancePtr.ATP::RadioCom::vcast_concrete_AbstractRadioHandler
<<radio_handler.<<GLOBAL>>.ATP::RadioCom::coreRadioHandlerInstancePtr>> = (&ATP::RadioCom::RadioHandler::instance());
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::RadioCom::RadioHandler::getConnected()bool const

-- Test Case: RadioHandler_getConnected.001
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::getConnected()bool const
TEST.NEW
TEST.NAME:RadioHandler_getConnected.001
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::getConnected()bool const.return:true
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: RadioHandler_getConnected.002
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::getConnected()bool const
TEST.NEW
TEST.NAME:RadioHandler_getConnected.002
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:false
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::getConnected()bool const.return:false
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: RadioHandler_getConnected.003
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::getConnected()bool const
TEST.NEW
TEST.NAME:RadioHandler_getConnected.003
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::getConnected()bool const.return:true
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::RadioCom::RadioHandler::getConnected(const uint16_t)bool const

-- Test Case: RadioHandler_getConnected_boolconst.001
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::getConnected(const uint16_t)bool const
TEST.NEW
TEST.NAME:RadioHandler_getConnected_boolconst.001
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getChannelId.return:1,2,3
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.VALUE:radio_handler.(cl)ATP::RadioCom::RadioHandler::getConnected(const uint16_t)bool const.chId:1,2,3
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::getConnected(const uint16_t)bool const.return:true
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: RadioHandler_getConnected_boolconst.002
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::getConnected(const uint16_t)bool const
TEST.NEW
TEST.NAME:RadioHandler_getConnected_boolconst.002
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getChannelId.return:1
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:false
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::getConnected(const uint16_t)bool const.return:false
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: RadioHandler_getConnected_boolconst.003
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::getConnected(const uint16_t)bool const
TEST.NEW
TEST.NAME:RadioHandler_getConnected_boolconst.003
TEST.COMPOUND_ONLY
TEST.VALUE:<<OPTIONS>>.MULTI_RETURN_SPANS_RANGE:TRUE
TEST.VALUE:<<OPTIONS>>.MULTI_RETURN_SPANS_COMPOUND_ITERATIONS:TRUE
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.VECTORCAST_INT1:VARY FROM:0 TO:1 BY: 1
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getChannelId.return:1,2
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.VALUE:radio_handler.(cl)ATP::RadioCom::RadioHandler::getConnected(const uint16_t)bool const.chId:1,2
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::getConnected(const uint16_t)bool const.return:true
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: RadioHandler_getConnected_boolconst.004
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::getConnected(const uint16_t)bool const
TEST.NEW
TEST.NAME:RadioHandler_getConnected_boolconst.004
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getChannelId.return:2,3
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.VALUE:radio_handler.(cl)ATP::RadioCom::RadioHandler::getConnected(const uint16_t)bool const.chId:2,3
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::getConnected(const uint16_t)bool const.return:true
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::RadioCom::RadioHandler::getNumOfTCCConnected

-- Test Case: RadioHandler_getNumOfTCCConnected.001
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::getNumOfTCCConnected
TEST.NEW
TEST.NAME:RadioHandler_getNumOfTCCConnected.001
TEST.COMPOUND_ONLY
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::getNumOfTCCConnected.return:3
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: RadioHandler_getNumOfTCCConnected.002
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::getNumOfTCCConnected
TEST.NEW
TEST.NAME:RadioHandler_getNumOfTCCConnected.002
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:false
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::getNumOfTCCConnected.return:0
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: RadioHandler_getNumOfTCCConnected.003
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::getNumOfTCCConnected
TEST.NEW
TEST.NAME:RadioHandler_getNumOfTCCConnected.003
TEST.COMPOUND_ONLY
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::getNumOfTCCConnected.return:2
TEST.END

-- Test Case: RadioHandler_getNumOfTCCConnected.004
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::getNumOfTCCConnected
TEST.NEW
TEST.NAME:RadioHandler_getNumOfTCCConnected.004
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::getNumOfTCCConnected.return:1
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: RadioHandler_getNumOfTCCConnected.005
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::getNumOfTCCConnected
TEST.NEW
TEST.NAME:RadioHandler_getNumOfTCCConnected.005
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::getNumOfTCCConnected.return:2
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: RadioHandler_getNumOfTCCConnected.006
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::getNumOfTCCConnected
TEST.NEW
TEST.NAME:RadioHandler_getNumOfTCCConnected.006
TEST.COMPOUND_ONLY
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::getNumOfTCCConnected.return:0
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::RadioCom::RadioHandler::getNumPositionMessages

-- Test Case: RadioHandler_getNumPositionMessages.001
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::getNumPositionMessages
TEST.NEW
TEST.NAME:RadioHandler_getNumPositionMessages.001
TEST.COMPOUND_ONLY
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::getNumPositionMessages.return:0
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: RadioHandler_getNumPositionMessages.002
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::getNumPositionMessages
TEST.NEW
TEST.NAME:RadioHandler_getNumPositionMessages.002
TEST.COMPOUND_ONLY
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::getNumPositionMessages.return:2
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: RadioHandler_getNumPositionMessages.003
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::getNumPositionMessages
TEST.NEW
TEST.NAME:RadioHandler_getNumPositionMessages.003
TEST.COMPOUND_ONLY
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::getNumPositionMessages.return:1
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::RadioCom::RadioHandler::getRegionRadioChannelTimeout

-- Test Case: RadioHandle_getRegionRadioChannelTimeout.001
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::getRegionRadioChannelTimeout
TEST.NEW
TEST.NAME:RadioHandle_getRegionRadioChannelTimeout.001
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::AbstractConfig::getRadioPollsLost.return:5
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getPollingTime.chId:2,3
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getPollingTime.return:20,10
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::corePtr.return.ATP::Kernel::vcast_concrete_AbstractMessageHandler.<<constructor>>.vcast_concrete_AbstractMessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getChannelId.return:2,3
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::getRegionRadioChannelTimeout.return:50000
TEST.END

-- Test Case: RadioHandle_getRegionRadioChannelTimeout.002
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::getRegionRadioChannelTimeout
TEST.NEW
TEST.NAME:RadioHandle_getRegionRadioChannelTimeout.002
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::corePtr.return.ATP::Kernel::vcast_concrete_AbstractMessageHandler.<<constructor>>.vcast_concrete_AbstractMessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:false
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::getRegionRadioChannelTimeout.return:0
TEST.END

-- Test Case: RadioHandle_getRegionRadioChannelTimeout.003
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::getRegionRadioChannelTimeout
TEST.NEW
TEST.NAME:RadioHandle_getRegionRadioChannelTimeout.003
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::AbstractConfig::getRadioPollsLost.return:5
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getPollingTime.chId:2,3
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getPollingTime.return:20,25
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::corePtr.return.ATP::Kernel::vcast_concrete_AbstractMessageHandler.<<constructor>>.vcast_concrete_AbstractMessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getChannelId.return:2,3
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::getRegionRadioChannelTimeout.return:100000
TEST.END

-- Test Case: RadioHandle_getRegionRadioChannelTimeout.004
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::getRegionRadioChannelTimeout
TEST.NEW
TEST.NAME:RadioHandle_getRegionRadioChannelTimeout.004
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::AbstractConfig::getRadioPollsLost.return:5
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getPollingTime.chId:2
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getPollingTime.return:20
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::corePtr.return.ATP::Kernel::vcast_concrete_AbstractMessageHandler.<<constructor>>.vcast_concrete_AbstractMessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getChannelId.return:2
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::getRegionRadioChannelTimeout.return:100000
TEST.END

-- Test Case: RadioHandle_getRegionRadioChannelTimeout.005
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::getRegionRadioChannelTimeout
TEST.NEW
TEST.NAME:RadioHandle_getRegionRadioChannelTimeout.005
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::AbstractConfig::getRadioPollsLost.return:5
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getPollingTime.chId:3
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getPollingTime.return:25
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::corePtr.return.ATP::Kernel::vcast_concrete_AbstractMessageHandler.<<constructor>>.vcast_concrete_AbstractMessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getChannelId.return:3
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::getRegionRadioChannelTimeout.return:125000
TEST.END

-- Subprogram: (cl)ATP::RadioCom::RadioHandler::getTCCTimeoutStatus

-- Test Case: RadioHandler_getTCCTimeoutStatus.001
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::getTCCTimeoutStatus
TEST.NEW
TEST.NAME:RadioHandler_getTCCTimeoutStatus.001
TEST.COMPOUND_ONLY
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::getTCCTimeoutStatus.return:true
TEST.END

-- Test Case: RadioHandler_getTCCTimeoutStatus.002
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::getTCCTimeoutStatus
TEST.NEW
TEST.NAME:RadioHandler_getTCCTimeoutStatus.002
TEST.COMPOUND_ONLY
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::getTCCTimeoutStatus.return:false
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::RadioCom::RadioHandler::getTCCTimeoutVal

-- Test Case: RadioHandler_getTCCTimeoutVal.001
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::getTCCTimeoutVal
TEST.NEW
TEST.NAME:RadioHandler_getTCCTimeoutVal.001
TEST.COMPOUND_ONLY
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::getTCCTimeoutVal.return:100
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::RadioCom::RadioHandler::init

-- Test Case: RadioHandler_init.001
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::init
TEST.NEW
TEST.NAME:RadioHandler_init.001
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::init.return:(3)true
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::init.return:true
TEST.STUB_EXP_USER_CODE:uut_prototype_stubs.ATC::AbstractApplicationBase::addComponent.comp
{{ <<uut_prototype_stubs.ATC::AbstractApplicationBase::addComponent.comp>> == ( &(ATP::RadioCom::RadioHandler::instance().radioChannel1)), (&(ATP::RadioCom::RadioHandler::instance().radioChannel2)), (&(ATP::RadioCom::RadioHandler::instance().radioChannel3) ) }}
TEST.END_STUB_EXP_USER_CODE:
TEST.VALUE_USER_CODE:<<ATP::RadioCom::AbstractRadioHandler instance>>
<<ATP::RadioCom::AbstractRadioHandler instance>> = ( ATP::RadioCom::AbstractRadioHandler::corePtr() );
TEST.END_VALUE_USER_CODE:
TEST.CONSTRUCTOR_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>.ATP::RadioCom::RadioHandler
<<ATP::RadioCom::RadioHandler instance>> = (&ATP::RadioCom::RadioHandler::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: RadioHandler_init.002
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::init
TEST.NEW
TEST.NAME:RadioHandler_init.002
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::init.return:(3)false
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::init.return:false
TEST.STUB_EXP_USER_CODE:uut_prototype_stubs.ATC::AbstractApplicationBase::addComponent.comp
{{ <<uut_prototype_stubs.ATC::AbstractApplicationBase::addComponent.comp>> == ( &(ATP::RadioCom::RadioHandler::instance().radioChannel1)), (&(ATP::RadioCom::RadioHandler::instance().radioChannel2)), (&(ATP::RadioCom::RadioHandler::instance().radioChannel3) ) }}
TEST.END_STUB_EXP_USER_CODE:
TEST.CONSTRUCTOR_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>.ATP::RadioCom::RadioHandler
<<ATP::RadioCom::RadioHandler instance>> = (&ATP::RadioCom::RadioHandler::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: RadioHandler_init.003
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::init
TEST.NEW
TEST.NAME:RadioHandler_init.003
TEST.COMPOUND_ONLY
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.VECTORCAST_INT_CC:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::init.return:true
TEST.EXPECTED:uut_prototype_stubs.vfwPutU32.value:(2)0
TEST.EXPECTED:uut_prototype_stubs.vfwPutU8.value:(2)0,(3)255,0
TEST.EXPECTED:uut_prototype_stubs.vfwPutI64.value:0
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::init.return:true
TEST.STUB_EXP_USER_CODE:uut_prototype_stubs.ATC::AbstractApplicationBase::addComponent.comp
{{ <<uut_prototype_stubs.ATC::AbstractApplicationBase::addComponent.comp>> == ( &(ATP::RadioCom::RadioHandler::instance().radioChannel1)), (&(ATP::RadioCom::RadioHandler::instance().radioChannel2)), (&(ATP::RadioCom::RadioHandler::instance().radioChannel3) ) }}
TEST.END_STUB_EXP_USER_CODE:
TEST.CONSTRUCTOR_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>.ATP::RadioCom::RadioHandler
<<ATP::RadioCom::RadioHandler instance>> = (&ATP::RadioCom::RadioHandler::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: RadioHandler_init.004
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::init
TEST.NEW
TEST.NAME:RadioHandler_init.004
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::init.return:true,(2)false
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::init.return:false
TEST.STUB_EXP_USER_CODE:uut_prototype_stubs.ATC::AbstractApplicationBase::addComponent.comp
{{ <<uut_prototype_stubs.ATC::AbstractApplicationBase::addComponent.comp>> == ( &(ATP::RadioCom::RadioHandler::instance().radioChannel1)), (&(ATP::RadioCom::RadioHandler::instance().radioChannel2)), (&(ATP::RadioCom::RadioHandler::instance().radioChannel3) ) }}
TEST.END_STUB_EXP_USER_CODE:
TEST.CONSTRUCTOR_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>.ATP::RadioCom::RadioHandler
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: RadioHandler_init.005
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::init
TEST.NEW
TEST.NAME:RadioHandler_init.005
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::init.return:(2)true,false
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::init.return:false
TEST.STUB_EXP_USER_CODE:uut_prototype_stubs.ATC::AbstractApplicationBase::addComponent.comp
{{ <<uut_prototype_stubs.ATC::AbstractApplicationBase::addComponent.comp>> == ( &(ATP::RadioCom::RadioHandler::instance().radioChannel1)), (&(ATP::RadioCom::RadioHandler::instance().radioChannel2)), (&(ATP::RadioCom::RadioHandler::instance().radioChannel3) ) }}
TEST.END_STUB_EXP_USER_CODE:
TEST.CONSTRUCTOR_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>.ATP::RadioCom::RadioHandler
<<ATP::RadioCom::RadioHandler instance>> = (&ATP::RadioCom::RadioHandler::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: RadioHandler_init.006
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::init
TEST.NEW
TEST.NAME:RadioHandler_init.006
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::init.return:(2)true
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::init.return:true
TEST.STUB_EXP_USER_CODE:uut_prototype_stubs.ATC::AbstractApplicationBase::addComponent.comp
{{ <<uut_prototype_stubs.ATC::AbstractApplicationBase::addComponent.comp>> == ( &(ATP::RadioCom::RadioHandler::instance().radioChannel1)), (&(ATP::RadioCom::RadioHandler::instance().radioChannel2)), (&(ATP::RadioCom::RadioHandler::instance().radioChannel3) ) }}
TEST.END_STUB_EXP_USER_CODE:
TEST.CONSTRUCTOR_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>.ATP::RadioCom::RadioHandler
<<ATP::RadioCom::RadioHandler instance>> = (&ATP::RadioCom::RadioHandler::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: RadioHandler_init.007
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::init
TEST.NEW
TEST.NAME:RadioHandler_init.007
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::init.return:(3)true
TEST.VALUE:radio_handler.<<GLOBAL>>.(cl).ATP::RadioCom::RadioHandler.ATP::RadioCom::RadioHandler.<<constructor>>.RadioHandler(const ATP::RadioCom::RadioHandler&).VCAST_PARAM_1.ATP::RadioCom::RadioHandler.<<constructor>>.RadioHandler(const ATP::RadioCom::RadioHandler&).VCAST_PARAM_1.ATP::RadioCom::RadioHandler.<<constructor>>.RadioHandler(const ATP::RadioCom::RadioHandler&).<<call>>:0
TEST.VALUE:radio_handler.<<GLOBAL>>.(cl).ATP::RadioCom::RadioHandler.ATP::RadioCom::RadioHandler.<<constructor>>.RadioHandler(const ATP::RadioCom::RadioHandler&).VCAST_PARAM_1.ATP::RadioCom::RadioHandler.<<constructor>>.RadioHandler(const ATP::RadioCom::RadioHandler&).<<call>>:0
TEST.VALUE:radio_handler.<<GLOBAL>>.(cl).ATP::RadioCom::RadioHandler.ATP::RadioCom::RadioHandler.<<constructor>>.RadioHandler(const ATP::RadioCom::RadioHandler&).<<call>>:0
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::init.return:true
TEST.STUB_EXP_USER_CODE:uut_prototype_stubs.ATC::AbstractApplicationBase::addComponent.comp
{{ <<uut_prototype_stubs.ATC::AbstractApplicationBase::addComponent.comp>> == ( &(ATP::RadioCom::RadioHandler::instance().radioChannel1)), (&(ATP::RadioCom::RadioHandler::instance().radioChannel2)), (&(ATP::RadioCom::RadioHandler::instance().radioChannel3) ) }}
TEST.END_STUB_EXP_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::RadioCom::RadioHandler::isYardModeTimerExpired

-- Test Case: RadioHandler_isYardModeTimerExpired.001
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::isYardModeTimerExpired
TEST.NEW
TEST.NAME:RadioHandler_isYardModeTimerExpired.001
TEST.COMPOUND_ONLY
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::isYardModeTimerExpired.return:false
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: RadioHandler_isYardModeTimerExpired.002
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::isYardModeTimerExpired
TEST.NEW
TEST.NAME:RadioHandler_isYardModeTimerExpired.002
TEST.COMPOUND_ONLY
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::isYardModeTimerExpired.return:true
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::RadioCom::RadioHandler::preInit

-- Test Case: RadioHandler_preInit.001
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::preInit
TEST.NEW
TEST.NAME:RadioHandler_preInit.001
TEST.COMPOUND_ONLY
TEST.CONSTRUCTOR_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>.ATP::RadioCom::RadioHandler
<<ATP::RadioCom::RadioHandler instance>> = (&ATP::RadioCom::RadioHandler::instance() );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Subprogram: (cl)ATP::RadioCom::RadioHandler::readMessage

-- Test Case: RadioHandler_readMessage.001
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::readMessage
TEST.NEW
TEST.NAME:RadioHandler_readMessage.001
TEST.COMPOUND_ONLY
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.user_msg.ATP::RadioMessage.dataLength:30
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.user_msg.ATP::RadioMessage.data:<<malloc 26>>
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.user_msg.ATP::RadioMessage.data:"ReadPositionReportMessage"
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getChannelId.return:1
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::readMessage.return:true
TEST.VALUE:radio_handler.(cl)ATP::RadioCom::RadioHandler::readMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::readMessage.msg.ATP::RadioMessage.dataLength:30
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::readMessage.msg.ATP::RadioMessage.data:"ReadPositionReportMessage"
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::readMessage.channelId:1
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::readMessage.return:true
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: RadioHandler_readMessage.002
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::readMessage
TEST.NEW
TEST.NAME:RadioHandler_readMessage.002
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getChannelId.return:1
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::readMessage.return:false
TEST.VALUE:radio_handler.(cl)ATP::RadioCom::RadioHandler::readMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.EXPECTED:radio_handler.(cl)ATP::RadioCom::RadioHandler::readMessage.return:false
TEST.END

-- Subprogram: (cl)ATP::RadioCom::RadioHandler::run

-- Test Case: RadioHandler_run.001
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::run
TEST.NEW
TEST.NAME:RadioHandler_run.001
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::isCentral.return:false
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.message.message.ATP::RadioMessage.dataLength:16
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.message.message.ATP::RadioMessage.data:<<malloc 17>>
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.message.message.ATP::RadioMessage.data:"MessageAvailable"
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::corePtr.return.ATP::Kernel::vcast_concrete_AbstractMessageHandler.<<constructor>>.vcast_concrete_AbstractMessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.return:true
TEST.EXPECTED:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.dataLength:16
TEST.EXPECTED:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.data:"MessageAvailable"
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: RadioHandler_run.002
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::run
TEST.NEW
TEST.NAME:RadioHandler_run.002
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.message.message.ATP::RadioMessage.dataLength:16
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.message.message.ATP::RadioMessage.data:<<malloc 17>>
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.message.message.ATP::RadioMessage.data:"MessageAvailable"
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.EXPECTED:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.data:"MessageAvailable"
TEST.END

-- Test Case: RadioHandler_run.003
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::run
TEST.NEW
TEST.NAME:RadioHandler_run.003
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.message.message.ATP::RadioMessage.dataLength:16
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.message.message.ATP::RadioMessage.data:<<malloc 17>>
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.message.message.ATP::RadioMessage.data:"MessageAvailable"
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.return:false
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.EXPECTED:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.data:"MessageAvailable"
TEST.END

-- Test Case: RadioHandler_run.006
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::run
TEST.NEW
TEST.NAME:RadioHandler_run.006
TEST.COMPOUND_ONLY
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.user_msg.ATP::RadioMessage.id:1
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.user_msg.ATP::RadioMessage.dataLength:40
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.user_msg.ATP::RadioMessage.data:<<malloc 25>>
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.user_msg.ATP::RadioMessage.data:"AckPositionreportmessage"
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.return:false
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::validateProtocolVersion.radioMessageToPeek.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::validateProtocolVersion.return:false
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getDefaultPositionReport.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::corePtr.return.ATP::Kernel::vcast_concrete_AbstractMessageHandler.<<constructor>>.vcast_concrete_AbstractMessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getChannelId.return:1
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true,(2)false
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.return:true
TEST.EXPECTED:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::ackDefaultPositionReport.chId:1
TEST.EXPECTED:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::ackDefaultPositionReport.connected:true
TEST.EXPECTED:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.id:1
TEST.EXPECTED:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.dataLength:40
TEST.EXPECTED:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.data:"AckPositionreportmessage"
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: RadioHandler_run.007
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::run
TEST.NEW
TEST.NAME:RadioHandler_run.007
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::isCentral.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.message.message.ATP::RadioMessage.dataLength:16
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.message.message.ATP::RadioMessage.data:<<malloc 17>>
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.message.message.ATP::RadioMessage.data:"MessageAvailable"
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getRegistrationArea.message.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getRegistrationArea.message.ATP::RadioMessage.id:1
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getRegistrationArea.message.ATP::RadioMessage.siteId:1
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getRegistrationArea.message.ATP::RadioMessage.regionId:1
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getRegistrationArea.message.ATP::RadioMessage.dataLength:20
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getRegistrationArea.message.ATP::RadioMessage.data:<<malloc 17>>
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getRegistrationArea.message.ATP::RadioMessage.data:"MessageAvailable"
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getRegistrationArea.return:false
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::corePtr.return.ATP::Kernel::vcast_concrete_AbstractMessageHandler.<<constructor>>.vcast_concrete_AbstractMessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.msg.ATP::RadioMessage.id:1
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.msg.ATP::RadioMessage.siteId:1
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.msg.ATP::RadioMessage.regionId:1
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.msg.ATP::RadioMessage.dataLength:20
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.msg.ATP::RadioMessage.data:<<malloc 17>>
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.msg.ATP::RadioMessage.data:"MessageAvailable"
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.id:1
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.siteId:1
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.regionId:1
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.dataLength:20
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.return:true
TEST.END

-- Test Case: RadioHandler_run.008
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::run
TEST.NEW
TEST.NAME:RadioHandler_run.008
TEST.COMPOUND_ONLY
TEST.VALUE:<<OPTIONS>>.MULTI_RETURN_SPANS_RANGE:TRUE
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.user_msg.ATP::RadioMessage.dataLength:40
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.user_msg.ATP::RadioMessage.data:<<malloc 20>>
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.user_msg.ATP::RadioMessage.data:"validateAreaRequest"
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::isCentral.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.return:false
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::validateAreaRequest.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getRegistrationArea.message.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getRegistrationArea.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getDefaultPositionReport.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::corePtr.return.ATP::Kernel::vcast_concrete_AbstractMessageHandler.<<constructor>>.vcast_concrete_AbstractMessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getChannelId.return:1
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.return:true
TEST.EXPECTED:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.dataLength:40
TEST.EXPECTED:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.data:"validateAreaRequest"
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: RadioHandler_run.009
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::run
TEST.NEW
TEST.NAME:RadioHandler_run.009
TEST.COMPOUND_ONLY
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.user_msg.ATP::RadioMessage.dataLength:16
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.user_msg.ATP::RadioMessage.data:<<malloc 17>>
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.user_msg.ATP::RadioMessage.data:"MessageAvailable"
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::isCentral.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.message.message.ATP::RadioMessage.dataLength:16
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.message.message.ATP::RadioMessage.data:<<malloc 17>>
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.message.message.ATP::RadioMessage.data:"MessageAvailable"
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getRegistrationArea.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::corePtr.return.ATP::Kernel::vcast_concrete_AbstractMessageHandler.<<constructor>>.vcast_concrete_AbstractMessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.return:true
TEST.CONSTRUCTOR_USER_CODE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage
<<uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg>> = ( &(<<USER_GLOBALS_VCAST.<<GLOBAL>>.user_msg>>) );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.CONSTRUCTOR_USER_CODE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getRegistrationArea.message.ATP::RadioMessage
<<uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getRegistrationArea.message>> = ( &(<<USER_GLOBALS_VCAST.<<GLOBAL>>.user_msg>>) );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: RadioHandler_run.012
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::run
TEST.NEW
TEST.NAME:RadioHandler_run.012
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::corePtr.return.ATP::Kernel::vcast_concrete_AbstractMessageHandler.<<constructor>>.vcast_concrete_AbstractMessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.return:false
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: RadioHandler_run.014
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::run
TEST.NEW
TEST.NAME:RadioHandler_run.014
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::isCentral.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::corePtr.return.ATP::Kernel::vcast_concrete_AbstractMessageHandler.<<constructor>>.vcast_concrete_AbstractMessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getChannelId.return:1
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:false
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.CONSTRUCTOR_USER_CODE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getRegistrationArea.message.ATP::RadioMessage
<<uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getRegistrationArea.message>> = ( &<<USER_GLOBALS_VCAST.<<GLOBAL>>.user_msg>> );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: RadioHandler_run.015
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::run
TEST.NEW
TEST.NAME:RadioHandler_run.015
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::AbstractConfig::getRadioTimeOutYardMode.return:1
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::corePtr.return.ATP::Kernel::vcast_concrete_AbstractMessageHandler.<<constructor>>.vcast_concrete_AbstractMessageHandler().<<call>>:0
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: RadioHandler_run.016
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::run
TEST.NEW
TEST.NAME:RadioHandler_run.016
TEST.COMPOUND_ONLY
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.user_msg.ATP::RadioMessage.id:1
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::isCentral.return:false
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.return:false
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::validateProtocolVersion.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getProtocolVersionResponse.message.ATP::RadioMessage.dataLength:40
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getProtocolVersionResponse.message.ATP::RadioMessage.data:<<malloc 27>>
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getProtocolVersionResponse.message.ATP::RadioMessage.data:"getProtocolVersionResponse"
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getProtocolVersionResponse.return:false,true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::corePtr.return.ATP::Kernel::vcast_concrete_AbstractMessageHandler.<<constructor>>.vcast_concrete_AbstractMessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getChannelId.return:1
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::readMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::readMessage.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.msg.ATP::RadioMessage.dataLength:40
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.msg.ATP::RadioMessage.data:<<malloc 19>>
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.msg.ATP::RadioMessage.data:"radioMessageToPeek"
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.return:true
TEST.EXPECTED:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::validateProtocolVersion.radioMessageToPeek.ATP::RadioMessage.dataLength:40
TEST.EXPECTED:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::validateProtocolVersion.radioMessageToPeek.ATP::RadioMessage.data:"radioMessageToPeek"
TEST.EXPECTED:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.dataLength:40
TEST.EXPECTED:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.data:"getProtocolVersionResponse"
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: RadioHandler_run.019
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::run
TEST.NEW
TEST.NAME:RadioHandler_run.019
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::isCentral.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::validateAreaRequest.return:false,true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getRegistrationArea.return:false
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getPollingTime.return:15
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::corePtr.return.ATP::Kernel::vcast_concrete_AbstractMessageHandler.<<constructor>>.vcast_concrete_AbstractMessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getChannelId.return:1
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.return:true
TEST.EXPECTED:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getPollingTime.chId:1
TEST.EXPECTED:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::setRegistrationAreaMessageSentToCentralTCC.valueToSet:false
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.CONSTRUCTOR_USER_CODE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getRegistrationArea.message.ATP::RadioMessage
<<uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getRegistrationArea.message>> = ( &<<USER_GLOBALS_VCAST.<<GLOBAL>>.user_msg>> );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- Test Case: RadioHandler_run.020
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::run
TEST.NEW
TEST.NAME:RadioHandler_run.020
TEST.COMPOUND_ONLY
TEST.VALUE:<<OPTIONS>>.MULTI_RETURN_SPANS_RANGE:TRUE
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.VECTORCAST_INT1:VARY FROM:0 TO:3 BY: 1
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.user_msg.ATP::RadioMessage.dataLength:22
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.user_msg.ATP::RadioMessage.data:<<malloc 6>>
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.user_msg.ATP::RadioMessage.data:"hello"
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.message.message.ATP::RadioMessage.dataLength:40
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.message.message.ATP::RadioMessage.data:<<malloc 73>>
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.message.message.ATP::RadioMessage.data:"AbortSetup","DriverInformation","StartUpMessage","TrainRegistrationInformation"
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getDefaultPositionReport.message.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getDefaultPositionReport.message.ATP::RadioMessage.dataLength:40
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getDefaultPositionReport.message.ATP::RadioMessage.data:<<malloc 15>>
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getDefaultPositionReport.message.ATP::RadioMessage.data:"PositionReport"
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getDefaultPositionReport.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::corePtr.return.ATP::Kernel::vcast_concrete_AbstractMessageHandler.<<constructor>>.vcast_concrete_AbstractMessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getChannelId.return:1
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.return:true
TEST.EXPECTED:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.dataLength:40
TEST.EXPECTED:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.data:"AbortSetup","DriverInformation","StartUpMessage","TrainRegistrationInformation"
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: RadioHandler_run.021
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::run
TEST.NEW
TEST.NAME:RadioHandler_run.021
TEST.COMPOUND_ONLY
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.user_msg.ATP::RadioMessage.dataLength:40
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.user_msg.ATP::RadioMessage.data:<<malloc 15>>
TEST.VALUE:USER_GLOBALS_VCAST.<<GLOBAL>>.user_msg.ATP::RadioMessage.data:"PositionReport"
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.return:false
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::validateAreaRequest.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::validateProtocolVersion.return:false
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getDefaultPositionReport.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::corePtr.return.ATP::Kernel::vcast_concrete_AbstractMessageHandler.<<constructor>>.vcast_concrete_AbstractMessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getChannelId.return:1
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.return:true
TEST.EXPECTED:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::ackDefaultPositionReport.chId:1
TEST.EXPECTED:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::ackDefaultPositionReport.connected:true
TEST.EXPECTED:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.dataLength:40
TEST.EXPECTED:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.data:"PositionReport"
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: RadioHandler_run.022
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::run
TEST.NEW
TEST.NAME:RadioHandler_run.022
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::corePtr.return.ATP::Kernel::vcast_concrete_AbstractMessageHandler.<<constructor>>.vcast_concrete_AbstractMessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.return:false
TEST.END

-- Test Case: RadioHandler_run.023
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::run
TEST.NEW
TEST.NAME:RadioHandler_run.023
TEST.COMPOUND_ONLY
TEST.VALUE:<<OPTIONS>>.MULTI_RETURN_SPANS_RANGE:TRUE
TEST.VALUE:uut_prototype_stubs.vfwGetReferenceTime.return:100
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:false
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: RadioHandler_run.024
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::run
TEST.NEW
TEST.NAME:RadioHandler_run.024
TEST.COMPOUND_ONLY
TEST.VALUE:<<OPTIONS>>.MULTI_RETURN_SPANS_RANGE:TRUE
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::corePtr.return.ATP::Kernel::vcast_concrete_AbstractMessageHandler.<<constructor>>.vcast_concrete_AbstractMessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: RadioHandler_run.025
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::run
TEST.NEW
TEST.NAME:RadioHandler_run.025
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.message.message.ATP::RadioMessage.dataLength:40
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.message.message.ATP::RadioMessage.data:<<malloc 11>>
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.message.message.ATP::RadioMessage.data:"AbortSetup"
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getDefaultPositionReport.message.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getDefaultPositionReport.message.ATP::RadioMessage.dataLength:40
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getDefaultPositionReport.message.ATP::RadioMessage.data:<<malloc 15>>
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getDefaultPositionReport.message.ATP::RadioMessage.data:"PositionReport"
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getDefaultPositionReport.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::corePtr.return.ATP::Kernel::vcast_concrete_AbstractMessageHandler.<<constructor>>.vcast_concrete_AbstractMessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getChannelId.return:1
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.return:true
TEST.EXPECTED:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.dataLength:40
TEST.EXPECTED:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.data:"AbortSetup"
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: RadioHandler_run.026
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::run
TEST.NEW
TEST.NAME:RadioHandler_run.026
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.message.message.ATP::RadioMessage.dataLength:40
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.message.message.ATP::RadioMessage.data:<<malloc 18>>
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.message.message.ATP::RadioMessage.data:"DriverInformation"
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::readMessage.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getDefaultPositionReport.message.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getDefaultPositionReport.message.ATP::RadioMessage.dataLength:40
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getDefaultPositionReport.message.ATP::RadioMessage.data:<<malloc 15>>
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getDefaultPositionReport.message.ATP::RadioMessage.data:"PositionReport"
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getDefaultPositionReport.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::corePtr.return.ATP::Kernel::vcast_concrete_AbstractMessageHandler.<<constructor>>.vcast_concrete_AbstractMessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getChannelId.return:1
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.return:true
TEST.EXPECTED:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.dataLength:40
TEST.EXPECTED:uut_prototype_stubs.ATP::RadioCom::RadioChannel::writeMessage.msg.ATP::RadioMessage.data:"DriverInformation"
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.END

-- Test Case: RadioHandler_run.027
TEST.UNIT:radio_handler
TEST.SUBPROGRAM:(cl)ATP::RadioCom::RadioHandler::run
TEST.NEW
TEST.NAME:RadioHandler_run.027
TEST.COMPOUND_ONLY
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::isCentral.return:true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::validateAreaRequest.return:false,true
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getRegistrationArea.return:false
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getPollingTime.return:15
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getNumberOfPollsLost.return:10
TEST.VALUE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::corePtr.return.ATP::Kernel::vcast_concrete_AbstractMessageHandler.<<constructor>>.vcast_concrete_AbstractMessageHandler().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getChannelId.return:1
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::getConnected.return:true
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.msg.ATP::RadioMessage.<<constructor>>.RadioMessage().<<call>>:0
TEST.VALUE:uut_prototype_stubs.ATP::RadioCom::RadioChannel::peekMessage.return:true
TEST.EXPECTED:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getPollingTime.chId:1
TEST.EXPECTED:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getNumberOfPollsLost.chId:1
TEST.EXPECTED:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::setRegistrationAreaMessageSentToCentralTCC.valueToSet:false
TEST.EXPECTED:uut_prototype_stubs.ATP::RadioCom::RadioChannel::setRadioTimeout.radioTimeoutinMs:150000
TEST.VALUE_USER_CODE:<<ATP::RadioCom::RadioHandler instance>>
<<ATP::RadioCom::RadioHandler instance>> = ( &ATP::RadioCom::RadioHandler::instance() );
TEST.END_VALUE_USER_CODE:
TEST.CONSTRUCTOR_USER_CODE:uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getRegistrationArea.message.ATP::RadioMessage
<<uut_prototype_stubs.ATP::Kernel::AbstractMessageHandler::getRegistrationArea.message>> = ( &<<USER_GLOBALS_VCAST.<<GLOBAL>>.user_msg>> );
TEST.END_CONSTRUCTOR_USER_CODE:
TEST.END

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:RH0001_Radio_link_Enabled
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Initialize.001"
TEST.SLOT: "2", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::preInit", "1", "RadioHandler_preInit.001"
TEST.SLOT: "3", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::init", "2", "RadioHandler_init.001"
TEST.SLOT: "4", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getConnected()bool const", "1", "RadioHandler_getConnected.001"
TEST.SLOT: "5", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getNumOfTCCConnected", "1", "RadioHandler_getNumOfTCCConnected.001"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:RH0002_Verify_PositionReport_message
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Initialize.001"
TEST.SLOT: "2", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::preInit", "1", "RadioHandler_preInit.001"
TEST.SLOT: "3", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::init", "1", "RadioHandler_init.001"
TEST.SLOT: "4", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::run", "1", "RadioHandler_run.006"
TEST.SLOT: "5", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getNumPositionMessages", "1", "RadioHandler_getNumPositionMessages.003"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:RH0003_Verify_Read_message
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Initialize.001"
TEST.SLOT: "2", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::preInit", "1", "RadioHandler_preInit.001"
TEST.SLOT: "3", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::init", "1", "RadioHandler_init.001"
TEST.SLOT: "4", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::run", "1", "RadioHandler_run.012"
TEST.SLOT: "5", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::readMessage", "1", "RadioHandler_readMessage.001"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:RH0004_initialisation_one_channel_failed
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Initialize.001"
TEST.SLOT: "2", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::preInit", "1", "RadioHandler_preInit.001"
TEST.SLOT: "3", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::init", "1", "RadioHandler_init.005"
TEST.SLOT: "4", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getNumOfTCCConnected", "1", "RadioHandler_getNumOfTCCConnected.005"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:RH0005_initialisation_two_channel_failed
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Initialize.001"
TEST.SLOT: "2", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::preInit", "1", "RadioHandler_preInit.001"
TEST.SLOT: "3", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::init", "1", "RadioHandler_init.004"
TEST.SLOT: "4", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getNumOfTCCConnected", "1", "RadioHandler_getNumOfTCCConnected.004"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:RH0006_Channel_Initialized_TCC_Not_Connected
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Initialize.001"
TEST.SLOT: "2", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::preInit", "1", "RadioHandler_preInit.001"
TEST.SLOT: "3", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::init", "1", "RadioHandler_init.001"
TEST.SLOT: "4", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getConnected()bool const", "1", "RadioHandler_getConnected.002"
TEST.SLOT: "5", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getNumOfTCCConnected", "1", "RadioHandler_getNumOfTCCConnected.002"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:RH0007_ReadMsg_When_msg_not_available
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Initialize.001"
TEST.SLOT: "2", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::preInit", "1", "RadioHandler_preInit.001"
TEST.SLOT: "3", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::init", "1", "RadioHandler_init.001"
TEST.SLOT: "4", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::readMessage", "1", "RadioHandler_readMessage.002"
TEST.SLOT: "5", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getNumPositionMessages", "1", "RadioHandler_getNumPositionMessages.001"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:RH0008_initialisation_all_channel_failed
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Initialize.001"
TEST.SLOT: "2", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::preInit", "1", "RadioHandler_preInit.001"
TEST.SLOT: "3", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::init", "1", "RadioHandler_init.002"
TEST.SLOT: "4", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getConnected()bool const", "1", "RadioHandler_getConnected.002"
TEST.SLOT: "5", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getNumOfTCCConnected", "1", "RadioHandler_getNumOfTCCConnected.002"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:RH0009_Compare_initcrosscomparedata
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Initialize.002"
TEST.SLOT: "2", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::preInit", "1", "RadioHandler_preInit.001"
TEST.SLOT: "3", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::init", "1", "RadioHandler_init.003"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:RH0011_Calculate_counter_enter_In_Yard
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Initialize.001"
TEST.SLOT: "2", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::preInit", "1", "RadioHandler_preInit.001"
TEST.SLOT: "3", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::init", "1", "RadioHandler_init.001"
TEST.SLOT: "4", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::isYardModeTimerExpired", "1", "RadioHandler_isYardModeTimerExpired.001"
TEST.SLOT: "5", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::run", "13", "RadioHandler_run.015"
TEST.SLOT: "6", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::isYardModeTimerExpired", "1", "RadioHandler_isYardModeTimerExpired.002"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:RH0012_Number_Of_TCC_Connected_using_chID
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Initialize.001"
TEST.SLOT: "2", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::preInit", "1", "RadioHandler_preInit.001"
TEST.SLOT: "3", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::init", "1", "RadioHandler_init.001"
TEST.SLOT: "4", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getConnected(const uint16_t)bool const", "1", "RadioHandler_getConnected_boolconst.001"
TEST.SLOT: "5", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getNumOfTCCConnected", "1", "RadioHandler_getNumOfTCCConnected.001"
TEST.SLOT: "6", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getConnected(const uint16_t)bool const", "1", "RadioHandler_getConnected_boolconst.002"
TEST.SLOT: "7", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getNumOfTCCConnected", "1", "RadioHandler_getNumOfTCCConnected.006"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:RH0013_handle_central_valid_area_req_not_received_and_received
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Initialize.001"
TEST.SLOT: "2", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::preInit", "1", "RadioHandler_preInit.001"
TEST.SLOT: "3", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::init", "1", "RadioHandler_init.001"
TEST.SLOT: "4", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::run", "1", "RadioHandler_run.019"
TEST.SLOT: "5", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::run", "1", "RadioHandler_run.008"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:RH0014_validate_protocol_version
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Initialize.001"
TEST.SLOT: "2", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::preInit", "1", "RadioHandler_preInit.001"
TEST.SLOT: "3", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::init", "1", "RadioHandler_init.001"
TEST.SLOT: "4", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::run", "1", "RadioHandler_run.016"
TEST.SLOT: "5", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getNumPositionMessages", "1", "RadioHandler_getNumPositionMessages.001"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:RH0015_Replacement_Position_Report_Message_3_TCC_PendingMessage
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Initialize.001"
TEST.SLOT: "2", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::preInit", "1", "RadioHandler_preInit.001"
TEST.SLOT: "3", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::init", "1", "RadioHandler_init.001"
TEST.SLOT: "4", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getConnected()bool const", "1", "RadioHandler_getConnected.001"
TEST.SLOT: "5", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getNumOfTCCConnected", "1", "RadioHandler_getNumOfTCCConnected.001"
TEST.SLOT: "6", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::run", "1", "RadioHandler_run.021"
TEST.SLOT: "7", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::run", "1", "RadioHandler_run.020"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:RH0016_TccTimeoutStauts_ChannelNotConnected_And_Connected
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Initialize.001"
TEST.SLOT: "2", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::preInit", "1", "RadioHandler_preInit.001"
TEST.SLOT: "3", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::init", "1", "RadioHandler_init.002"
TEST.SLOT: "4", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::run", "1", "RadioHandler_run.023"
TEST.SLOT: "5", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getTCCTimeoutVal", "1", "RadioHandler_getTCCTimeoutVal.001"
TEST.SLOT: "6", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getTCCTimeoutStatus", "1", "RadioHandler_getTCCTimeoutStatus.001"
TEST.SLOT: "7", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::init", "1", "RadioHandler_init.001"
TEST.SLOT: "8", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::run", "1", "RadioHandler_run.024"
TEST.SLOT: "9", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getTCCTimeoutStatus", "1", "RadioHandler_getTCCTimeoutStatus.002"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:RH0017_Replacement_Position_Report_Message_2_TCC_AbortMessage
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Initialize.001"
TEST.SLOT: "2", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::preInit", "1", "RadioHandler_preInit.001"
TEST.SLOT: "3", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::init", "1", "RadioHandler_init.005"
TEST.SLOT: "4", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getConnected()bool const", "1", "RadioHandler_getConnected.001"
TEST.SLOT: "5", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getNumOfTCCConnected", "1", "RadioHandler_getNumOfTCCConnected.003"
TEST.SLOT: "6", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::run", "1", "RadioHandler_run.021"
TEST.SLOT: "7", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::run", "1", "RadioHandler_run.025"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:RH0018_Replacement_Position_Report_Message_2_TCC_DriverInformation
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Initialize.001"
TEST.SLOT: "2", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::preInit", "1", "RadioHandler_preInit.001"
TEST.SLOT: "3", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::init", "1", "RadioHandler_init.005"
TEST.SLOT: "4", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getConnected()bool const", "1", "RadioHandler_getConnected.001"
TEST.SLOT: "5", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getNumOfTCCConnected", "1", "RadioHandler_getNumOfTCCConnected.003"
TEST.SLOT: "6", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::run", "1", "RadioHandler_run.021"
TEST.SLOT: "7", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::run", "1", "RadioHandler_run.026"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:RH0019_Update_Radio_Channel_Timeout
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Initialize.001"
TEST.SLOT: "2", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::preInit", "1", "RadioHandler_preInit.001"
TEST.SLOT: "3", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::init", "1", "RadioHandler_init.001"
TEST.SLOT: "4", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::run", "1", "RadioHandler_run.027"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:RH0020_Region_Radio_Channel_Timeout_Two_Channels_No_Channel
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Initialize.001"
TEST.SLOT: "2", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::preInit", "1", "RadioHandler_preInit.001"
TEST.SLOT: "3", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::init", "1", "RadioHandler_init.001"
TEST.SLOT: "4", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getRegionRadioChannelTimeout", "1", "RadioHandle_getRegionRadioChannelTimeout.002"
TEST.SLOT: "5", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getRegionRadioChannelTimeout", "1", "RadioHandle_getRegionRadioChannelTimeout.001"
TEST.SLOT: "6", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getRegionRadioChannelTimeout", "1", "RadioHandle_getRegionRadioChannelTimeout.003"
TEST.END
--

-- COMPOUND TESTS

TEST.SUBPROGRAM:<<COMPOUND>>
TEST.NEW
TEST.NAME:RH0021_Region_Radio_Channel_Timeout_One_Channels
TEST.SLOT: "1", "<<INIT>>", "<<INIT>>", "1", "Initialize.001"
TEST.SLOT: "2", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::preInit", "1", "RadioHandler_preInit.001"
TEST.SLOT: "3", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::init", "1", "RadioHandler_init.001"
TEST.SLOT: "4", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getRegionRadioChannelTimeout", "1", "RadioHandle_getRegionRadioChannelTimeout.004"
TEST.SLOT: "5", "radio_handler", "(cl)ATP::RadioCom::RadioHandler::getRegionRadioChannelTimeout", "1", "RadioHandle_getRegionRadioChannelTimeout.005"
TEST.END
--
