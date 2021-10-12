--- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
--- Debug boilerplate
--- from: https://wiki.wireshark.org/Lua/Examples?action=AttachFile&do=get&target=dissector.lua

local debug_level = {
    DISABLED = 0,
    LEVEL_1  = 1,
    LEVEL_2  = 2
}

local DEBUG = debug_level.LEVEL_2

local default_settings =
{
    debug_level  = DEBUG,
    port         = 30130, -- dmi port
    heur_enabled = false,
}

-- for testing purposes, we want to be able to pass in changes to the defaults
-- from the command line; because you can't set lua preferences from the command
-- line using the '-o' switch (the preferences don't exist until this script is
-- loaded, so the command line thinks they're invalid preferences being set)
-- so we pass them in as command arguments insetad, and handle it here:
local args={...} -- get passed-in args
if args and #args > 0 then
    for _, arg in ipairs(args) do
        local name, value = arg:match("(.+)=(.+)")
        if name and value then
            if tonumber(value) then
                value = tonumber(value)
            elseif value == "true" or value == "TRUE" then
                value = true
            elseif value == "false" or value == "FALSE" then
                value = false
            elseif value == "DISABLED" then
                value = debug_level.DISABLED
            elseif value == "LEVEL_1" then
                value = debug_level.LEVEL_1
            elseif value == "LEVEL_2" then
                value = debug_level.LEVEL_2
            else
                error("invalid commandline argument value")
            end
        else
            error("invalid commandline argument syntax")
        end

        default_settings[name] = value
    end
end

local dprint = function() end
local dprint2 = function() end
local function reset_debug_level()
    if default_settings.debug_level > debug_level.DISABLED then
        dprint = function(...)
            print(table.concat({"Lua:", ...}," "))
        end

        if default_settings.debug_level > debug_level.LEVEL_1 then
            dprint2 = dprint
        end
    end
end

-- call it now
reset_debug_level()

dprint2("Wireshark version = ", get_version())
dprint2("Lua version = ", _VERSION)


--- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

json          = require "json"
utilwireshark = require "utilwireshark"

function FIELD_DECODE(ctx, spec, fld, subflds)
   local l = spec.Length;
   local r = ctx.tvbuf:range(ctx.off,l);
   dprint2(" > FIELD_DECODE: ", spec.type, ctx.off, "l:", l);

   local v = 0;
   if (l <= 4) then
      v = r:uint()
   end
   local ftree = ctx.stack:top():add(fld, r)
   for k,v in ipairs(subflds) do
      ftree:add(v, r);
   end
   ctx.off = ctx.off + l; --- spec.Length;
   return v
end

function BLOCKCNT_DECODE(ctx,f,fld,blocks)
   local v = f(ctx,fld,{});
   local b = nil
   --- blocks has only one entry
   for key,b0 in pairs(blocks) do
      b = blocks[key]
   end
   local j = 0;
   
   for i = 1,v do
      if (b ~= nil) then
	 b.func(ctx);
      end
      j = j + 1;
      if (j > 1000) then
	 break;
      end
   end
end

function MSG_DECODE_HEADER(ctx)
end

function MSG_BLOCKS_DECODE(ctx, blocks)
end

function MSG_ERROR(ctx)
end


--- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
--- Decode of fields

-------------------------------
FIELD_SPEC_ATP_Mode = json.decode('{"Min": 0, "Resolution": null, "Max": 19, "Format": "UINT", "fldname": "ATP_Mode", "Detail": "ATP mode", "Default": 0, "Length": 1, "parts": [], "type": "ATP_Mode", "Display": "", "Special": [{"text": "Undefined", "type": "Field", "value": 0}, {"text": "Power up", "type": "Field", "value": 2}, {"text": "Train configuration", "type": "Field", "value": 2}, {"text": "Train registration", "type": "Field", "value": 3}, {"text": "Balise Search", "type": "Field", "value": 4}, {"text": "Normal", "type": "Field", "value": 5}, {"text": "Shunting", "type": "Field", "value": 6}, {"text": "Location", "type": "Field", "value": 7}, {"text": "AutomaticUnload", "type": "Field", "value": 8}, {"text": "Yard", "type": "Field", "value": 9}, {"text": "Unregistered", "type": "Field", "value": 10}, {"text": "PoweringDown", "type": "Field", "value": 11}, {"text": "SafetyHalt", "type": "Field", "value": 12}, {"text": "Sleeping", "type": "Field", "value": 13}, {"text": "StaffResponsible", "type": "Field", "value": 14}, {"text": "ShuntingRoute", "type": "Field", "value": 15}, {"text": "Possession", "type": "Field", "value": 16}, {"text": "Split", "type": "Field", "value": 17}, {"text": "Join", "type": "Field", "value": 18}, {"text": "SafeBrakeToStop", "type": "Field", "value": 19}]}');
function FIELD_DECODE_ATP_Mode(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_ATP_Mode, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_ATP_State = json.decode('{"Min": 0, "Resolution": null, "Max": 19, "Format": "UINT", "fldname": "ATP_State", "Detail": "ATP state", "Default": 0, "Length": 1, "parts": [], "type": "ATP_State", "Display": "", "Special": [{"text": "Undefined", "type": "Field", "value": 0}, {"text": "BasicSystemStartUp", "type": "Field", "value": 1}, {"text": "ApplicationStartUp", "type": "Field", "value": 2}, {"text": "Inactive", "type": "Field", "value": 3}, {"text": "ActivationInitiation", "type": "Field", "value": 4}, {"text": "ActivationTest", "type": "Field", "value": 5}, {"text": "Active", "type": "Field", "value": 6}, {"text": "FatalFailureState", "type": "Field", "value": 7}, {"text": "SystemRestart", "type": "Field", "value": 8}, {"text": "Power down", "type": "Field", "value": 9}]}');
function FIELD_DECODE_ATP_State(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_ATP_State, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_ATP_Mode_Extra = json.decode('{"Min": 0, "Resolution": null, "Max": 2, "Format": "UINT", "fldname": "ATP_Mode_Extra", "Detail": "ATP mode extra", "Default": 0, "Length": 1, "parts": [], "type": "ATP_Mode_Extra", "Display": "", "Special": [{"text": "Undefined", "type": "Field", "value": 0}, {"text": "Manual/Allow driver to Select Mode Configuration, Yard", "type": "Field", "value": 1}, {"text": "ReConfig", "type": "Field", "value": 2}]}');
function FIELD_DECODE_ATP_Mode_Extra(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_ATP_Mode_Extra, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Driver_Verification_State = json.decode('{"Min": 0, "Resolution": null, "Max": 5, "Format": "UINT", "fldname": "Driver_Verification_State", "Detail": "Driver verification state", "Default": 0, "Length": 1, "parts": [], "type": "Driver_Verification_State", "Display": "", "Special": [{"text": "Undefined", "type": "Field", "value": 0}, {"text": "NoActionState", "type": "Field", "value": 1}, {"text": "InputState", "type": "Field", "value": 2}, {"text": "VerificationState", "type": "Field", "value": 3}, {"text": "RedoInputState", "type": "Field", "value": 4}, {"text": "Logged on", "type": "Field", "value": 5}]}');
function FIELD_DECODE_Driver_Verification_State(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Driver_Verification_State, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Train_State = json.decode('{"Min": 0, "Default": null, "Format": "BITMASK", "fldname": "Train_State", "Detail": "Train state", "Length": 1, "parts": [], "type": "Train_State", "Display": "", "Special": [{"text": "Radio available", "type": "Bit", "value": 0}, {"text": "TIMS available", "type": "Bit", "value": 1}, {"text": "TIMS Ok", "type": "Bit", "value": 2}, {"text": "Odometer invalid", "type": "Bit", "value": 3}, {"text": "Stop Train request", "type": "Bit", "value": 4}, {"text": "Configuration rejected", "type": "Bit", "value": 5}, {"text": "Train integrity granted by driver", "type": "Bit", "value": 6}]}');
function FIELD_DECODE_Train_State(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Train_State, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Locomotive_State = json.decode('{"Min": 0, "Default": null, "Format": "BITMASK", "fldname": "Locomotive_State", "Detail": "Locomotive status", "Length": 4, "parts": [], "type": "Locomotive_State", "Display": "", "Special": [{"text": "Safety Halt", "type": "Bit", "value": 0}, {"text": "EmergencyAlert from driver", "type": "Bit", "value": 1}, {"text": "TIMS Integrity Broken", "type": "Bit", "value": 2}, {"text": "Braking event", "type": "Bit", "value": 3}, {"text": "HandlingDone", "type": "Bit", "value": 4}, {"text": "TrainIdling", "type": "Bit", "value": 5}, {"text": "TIMS Integrity Manual Override from Driver", "type": "Bit", "value": 6}, {"text": "MA Timeout", "type": "Bit", "value": 7}, {"text": "ATP Reset", "type": "Bit", "value": 8}, {"text": "ATP_needs to be reset", "type": "Bit", "value": 9}, {"text": "ATP intervention", "type": "Bit", "value": 10}, {"text": "Brake release requested", "type": "Bit", "value": 11}, {"text": "Manual TIMS confirmation", "type": "Bit", "value": 12}, {"text": "Slip detected", "type": "Bit", "value": 13}, {"text": "Free rolling", "type": "Bit", "value": 14}, {"text": "Emergency Alert active", "type": "Bit", "value": 15}, {"text": "AttentionNeeded (ATO)", "type": "Bit", "value": 16}, {"text": "Not ready to drive (ATO)", "type": "Bit", "value": 17}, {"text": "Safe for boarding (ATO)", "type": "Bit", "value": 18}]}');
function FIELD_DECODE_Locomotive_State(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Locomotive_State, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_ATO_Mode = json.decode('{"Min": 0, "Resolution": null, "Max": 5, "Format": "UINT", "fldname": "ATO_Mode", "Detail": "ATO Mode", "Default": 0, "Length": 1, "parts": [], "type": "ATO_Mode", "Display": "", "Special": [{"text": "ATOModeUndefined", "type": "Field", "value": 0}, {"text": "ATOModeManual", "type": "Field", "value": 1}, {"text": "ATOModeSupervisedAutomatic", "type": "Field", "value": 2}, {"text": "ATOModeAutomatic", "type": "Field", "value": 3}, {"text": "ATOModeRemote", "type": "Field", "value": 4}]}');
function FIELD_DECODE_ATO_Mode(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_ATO_Mode, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_ATO_SwitchPos = json.decode('{"Min": 0, "Resolution": null, "Max": 4, "Format": "UINT", "fldname": "ATO_SwitchPos", "Detail": "Wanted ATO switch position", "Default": 0, "Length": 1, "parts": [], "type": "ATO_SwitchPos", "Display": "", "Special": [{"text": "Undefined", "type": "Field", "value": 0}, {"text": "Manual", "type": "Field", "value": 1}, {"text": "Supervised Automatic", "type": "Field", "value": 2}, {"text": "Automatic", "type": "Field", "value": 3}, {"text": "Illegal", "type": "Field", "value": 4}]}');
function FIELD_DECODE_ATO_SwitchPos(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_ATO_SwitchPos, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_AdditionalStatus1 = json.decode('{"Min": 0, "Default": null, "Format": "BITMASK", "fldname": "AdditionalStatus1", "Detail": "Additional status-bits 1", "Length": 1, "parts": [], "type": "AdditionalStatus1", "Display": "", "Special": [{"text": "ATO Enable", "type": "Bit", "value": 0}, {"text": "Driving forward", "type": "Bit", "value": 1}, {"text": "Standstill Event active", "type": "Bit", "value": 2}, {"text": "Brake Test possible", "type": "Bit", "value": 3}, {"text": "Brake Test notification", "type": "Bit", "value": 4}, {"text": "Brake Test mandatory", "type": "Bit", "value": 5}]}');
function FIELD_DECODE_AdditionalStatus1(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_AdditionalStatus1, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_AdditionalStatus2 = json.decode('{"Min": 0, "Default": null, "Format": "BITMASK", "fldname": "AdditionalStatus2", "Detail": "Additional status-bits 2", "Length": 1, "parts": [], "type": "AdditionalStatus2", "Display": "", "Special": [{"Note": "todo", "text": "Com status", "type": "Bit", "len": 2, "value": 0}]}');
function FIELD_DECODE_AdditionalStatus2(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_AdditionalStatus2, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Confirm_change = json.decode('{"Min": 0, "Resolution": null, "Max": 5, "Format": "UINT", "fldname": "Confirm_change", "Detail": "Confirm change to mode needed", "Default": 0, "Length": 1, "parts": [], "type": "Confirm_change", "Display": "", "Special": [{"text": "Nothing to confirm", "type": "Field", "value": 0}, {"text": "Yard", "type": "Field", "value": 1}, {"text": "Shunting Route", "type": "Field", "value": 2}, {"text": "Staff Responsible", "type": "Field", "value": 3}]}');
function FIELD_DECODE_Confirm_change(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Confirm_change, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Allow = json.decode('{"Min": 0, "Default": null, "Format": "BITMASK", "fldname": "Allow", "Detail": "Allowed  to bit definitions", "Length": 1, "parts": [], "type": "Allow", "Display": "", "Special": [{"text": "Allowed to login", "type": "Bit", "value": 0}, {"text": "Allowed to enter Yard mode", "type": "Bit", "value": 1}, {"text": "Allowed to enter Possession", "type": "Bit", "value": 2}, {"text": "Allowed to enter Shunting", "type": "Bit", "value": 3}]}');
function FIELD_DECODE_Allow(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Allow, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_BreakStatus = json.decode('{"Min": 0, "Resolution": null, "Max": 5, "Format": "UINT", "fldname": "BreakStatus", "Detail": "Brake Test status values", "Default": 0, "Length": 1, "parts": [], "type": "BreakStatus", "Display": "", "Special": [{"text": "No status", "type": "Field", "value": 0}, {"text": "Brake Test in progress", "type": "Field", "value": 1}, {"text": "Brake Test aborted", "type": "Field", "value": 2}, {"text": "Brake Test failed", "type": "Field", "value": 3}, {"text": "Brake Test successful", "type": "Field", "value": 4}]}');
function FIELD_DECODE_BreakStatus(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_BreakStatus, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_PermittedDirection = json.decode('{"Min": 0, "Resolution": null, "Max": 4, "Format": "UINT", "fldname": "PermittedDirection", "Detail": "Permitted driving direction", "Default": 0, "Length": 1, "parts": [], "type": "PermittedDirection", "Display": "", "Special": [{"text": "Undefined", "type": "Field", "value": 0}, {"text": "None", "type": "Field", "value": 1}, {"text": "Forward", "type": "Field", "value": 2}, {"text": "Reverse", "type": "Field", "value": 3}, {"text": "Both", "type": "Field", "value": 4}]}');
function FIELD_DECODE_PermittedDirection(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_PermittedDirection, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_StatusSignals = json.decode('{"Min": 0, "Default": null, "Format": "BITMASK", "fldname": "StatusSignals", "Detail": "StatusSignals", "Length": 1, "parts": [], "type": "StatusSignals", "Display": "", "Special": [{"text": "In BCA", "type": "Bit", "value": 0}, {"text": "ATP Warning", "type": "Bit", "value": 1}, {"text": "ATP Intervention", "type": "Bit", "value": 2}, {"text": "Flash SB Brake button", "type": "Bit", "value": 3}, {"text": "Radio Available", "type": "Bit", "value": 4}, {"text": "Service Brakes applied", "type": "Bit", "value": 5}, {"text": "Emergency Brakes applied", "type": "Bit", "value": 6}, {"text": "Flash EB Brake button", "type": "Bit", "value": 7}]}');
function FIELD_DECODE_StatusSignals(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_StatusSignals, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_VisibibleSignals = json.decode('{"Min": 0, "Default": null, "Format": "BITMASK", "fldname": "VisibibleSignals", "Detail": "VisibibleSignals", "Length": 1, "parts": [], "type": "VisibibleSignals", "Display": "", "Special": [{"text": "Indicate Permitted direction", "type": "Bit", "value": 0}, {"text": "Indicate Permitted speed", "type": "Bit", "value": 1}, {"text": "Indicate Target speed", "type": "Bit", "value": 2}, {"text": "Indicate Remaining distance to target point", "type": "Bit", "value": 3}, {"text": "Indicate Remaining distance to BCA", "type": "Bit", "value": 4}, {"text": "Indicate Predicted distance to stand still location", "type": "Bit", "value": 5}, {"text": "Indicate Predicted speed at brake target", "type": "Bit", "value": 6}, {"text": "Indicate Time to intervention", "type": "Bit", "value": 7}]}');
function FIELD_DECODE_VisibibleSignals(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_VisibibleSignals, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_TimsRequired = json.decode('{"Min": 0, "Default": null, "Format": "BITMASK", "fldname": "TimsRequired", "Detail": "Tims", "Length": 1, "parts": [], "type": "TimsRequired", "Display": "", "Special": [{"text": "TIMS Required", "type": "Bit", "value": 0}, {"text": "Cars connected at B == 0, Cars connected at A == 1", "type": "Bit", "value": 1}]}');
function FIELD_DECODE_TimsRequired(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_TimsRequired, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_ErrorBlock = json.decode('{"Min": 0, "Resolution": null, "Max": 4, "Format": "UINT", "fldname": "ErrorBlock", "Detail": "Error type", "Default": 0, "Length": 1, "parts": [], "type": "ErrorBlock", "Display": "", "Special": [{"text": "UndefBlock", "type": "Field", "value": 0}, {"text": "ACT", "type": "Field", "value": 1}, {"text": "BRK", "type": "Field", "value": 2}, {"text": "CARH", "type": "Field", "value": 3}, {"text": "COM", "type": "Field", "value": 4}, {"text": "COMATO", "type": "Field", "value": 5}, {"text": "COMMMI", "type": "Field", "value": 6}, {"text": "LOG", "type": "Field", "value": 7}, {"text": "ELOG", "type": "Field", "value": 8}, {"text": "MMICL", "type": "Field", "value": 9}, {"text": "MON100", "type": "Field", "value": 10}, {"text": "MON250", "type": "Field", "value": 11}, {"text": "MON500", "type": "Field", "value": 12}, {"text": "RCOND", "type": "Field", "value": 13}, {"text": "SEL", "type": "Field", "value": 14}, {"text": "START", "type": "Field", "value": 15}, {"text": "SUP", "type": "Field", "value": 16}, {"text": "VCOM", "type": "Field", "value": 17}, {"text": "COMATP2", "type": "Field", "value": 18}, {"text": "CONFIG", "type": "Field", "value": 19}]}');
function FIELD_DECODE_ErrorBlock(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_ErrorBlock, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_ErrorGrade = json.decode('{"Min": 0, "Resolution": null, "Max": 4, "Format": "UINT", "fldname": "ErrorGrade", "Detail": "Error grade", "Default": 0, "Length": 1, "parts": [], "type": "ErrorGrade", "Display": "", "Special": [{"text": "Undefined", "type": "Field", "value": 0}, {"text": "Fatal error A", "type": "Field", "value": 1}, {"text": "Fatal error B", "type": "Field", "value": 2}, {"text": "Fatal error C", "type": "Field", "value": 3}, {"text": "Minor error A", "type": "Field", "value": 4}, {"text": "Minor error B", "type": "Field", "value": 5}, {"text": "Minor error C", "type": "Field", "value": 6}, {"text": "Log error A", "type": "Field", "value": 7}, {"text": "Log error B", "type": "Field", "value": 8}, {"text": "Log error C", "type": "Field", "value": 9}]}');
function FIELD_DECODE_ErrorGrade(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_ErrorGrade, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_AcceptStatus = json.decode('{"Min": 0, "Default": null, "Format": "BITMASK", "fldname": "AcceptStatus", "Detail": "Accept Status", "Length": 1, "parts": [], "type": "AcceptStatus", "Display": "", "Special": [{"text": "Accepted without TIMS", "type": "Bit", "value": 0}]}');
function FIELD_DECODE_AcceptStatus(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_AcceptStatus, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Confirm = json.decode('{"Min": 0, "Resolution": null, "Max": 4, "Format": "UINT", "fldname": "Confirm", "Detail": "Confirmation", "Default": 0, "Length": 1, "parts": [], "type": "Confirm", "Display": "", "Special": [{"text": "Undefined", "type": "Field", "value": 0}, {"text": "OK", "type": "Field", "value": 1}, {"text": "Not OK", "type": "Field", "value": 2}]}');
function FIELD_DECODE_Confirm(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Confirm, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_DMIButtonStatus = json.decode('{"Min": 0, "Resolution": null, "Max": 23, "Format": "UINT", "fldname": "DMIButtonStatus", "Detail": "DMI button status", "Default": 0, "Length": 1, "parts": [], "type": "DMIButtonStatus", "Display": "", "Special": [{"text": "Undefined", "type": "Field", "value": 0}, {"text": "BrakeRelease, 2 = TICConfirmation, 3 = DerailInhibit, 4 = TrainConfig,", "type": "Field", "value": 1}, {"text": "ManualHandling,", "type": "Field", "value": 5}, {"text": "HandlingDone,", "type": "Field", "value": 6}, {"text": "EnterYardMode,", "type": "Field", "value": 7}, {"text": "AlertButton,", "type": "Field", "value": 8}, {"text": "Test,", "type": "Field", "value": 9}, {"text": "ResetATP,", "type": "Field", "value": 10}, {"text": "RetryConfig", "type": "Field", "value": 11}, {"text": "Spare", "type": "Field", "value": 12}, {"text": "DriverLogout", "type": "Field", "value": 13}, {"text": "Possession", "type": "Field", "value": 14}, {"text": "Shunting", "type": "Field", "value": 15}, {"text": "Spare", "type": "Field", "value": 16}, {"text": "Start Brake Test", "type": "Field", "value": 17}, {"text": "Abort Brake Test", "type": "Field", "value": 18}, {"text": "Spare", "type": "Field", "value": 19}, {"text": "Spare", "type": "Field", "value": 20}, {"text": "Confirm change to Yard", "type": "Field", "value": 21}, {"text": "Confirm change to Shunting Route", "type": "Field", "value": 22}, {"text": "Confirm change to Staff Responsible", "type": "Field", "value": 23}]}');
function FIELD_DECODE_DMIButtonStatus(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_DMIButtonStatus, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_SpeedReason = json.decode('{"Min": 0, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "SpeedReason", "Detail": "Reason", "Default": 0, "Length": 1, "parts": [], "type": "SpeedReason", "Display": "", "Special": [{"text": "Undefined", "type": "Field", "value": 0}, {"text": "Point Straight", "type": "Field", "value": 1}, {"text": "Point Curve", "type": "Field", "value": 2}, {"text": "Point Passed", "type": "Field", "value": 3}, {"text": "Location", "type": "Field", "value": 4}, {"text": "Other", "type": "Field", "value": 5}, {"text": "End of MA", "type": "Field", "value": 6}, {"text": "TSR (temporary speed restriction)", "type": "Field", "value": 7}, {"text": "Conditional target (e.g. pantograph shift)", "type": "Field", "value": 8}, {"text": "Pantograph shift to none", "type": "Field", "value": 9}, {"text": "Pantograph shift to roof", "type": "Field", "value": 10}, {"text": "Pantograph shift to side", "type": "Field", "value": 11}, {"text": "reference for odometer value", "type": "Field", "value": 255}]}');
function FIELD_DECODE_SpeedReason(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_SpeedReason, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_TicStatus = json.decode('{"Min": 0, "Default": null, "Format": "BITMASK", "fldname": "TicStatus", "Detail": "TIC Status", "Length": 2, "parts": [], "type": "TicStatus", "Display": "", "Special": [{"text": "TIC_TrainConfigInput", "type": "Bit", "value": 0}, {"text": "TIC_DerailInhibit", "type": "Bit", "value": 1}, {"text": "TIC_Derail", "type": "Bit", "value": 2}, {"text": "TIC_PlatformError", "type": "Bit", "value": 3}, {"text": "TIC_FrontRightDerail", "type": "Bit", "value": 8}, {"text": "TIC_FrontLeftDerail", "type": "Bit", "value": 9}, {"text": "TIC_RearRightDerail", "type": "Bit", "value": 10}, {"text": "TIC_RearLeftDerail", "type": "Bit", "value": 11}, {"text": "TIC_CarDumpBottom", "type": "Bit", "value": 12}, {"text": "TIC_CarDumpTop", "type": "Bit", "value": 13}, {"text": "TIC_CarDumpClosed", "type": "Bit", "value": 14}, {"text": "TIC_LoadWeightBad", "type": "Bit", "value": 15}]}');
function FIELD_DECODE_TicStatus(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_TicStatus, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_PlatformStatus = json.decode('{"Min": 0, "Resolution": null, "Max": 23, "Format": "UINT", "fldname": "PlatformStatus", "Detail": "Platform Status", "Default": 0, "Length": 1, "parts": [], "type": "PlatformStatus", "Display": "", "Special": [{"text": "cpsUndefined", "type": "Field", "value": 0}, {"text": "cpsDOWN", "type": "Field", "value": 1}, {"text": "cpsGoingUP", "type": "Field", "value": 2}, {"text": "cpsUP", "type": "Field", "value": 3}, {"text": "cpsGoingDOWN", "type": "Field", "value": 4}]}');
function FIELD_DECODE_PlatformStatus(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_PlatformStatus, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_M_END_OF_MESSAGE = json.decode('{"Min": 0, "Resolution": null, "Max": 0, "Format": "UINT", "fldname": "M_END_OF_MESSAGE", "Detail": "End of message for variable length messages", "Default": 0, "Length": 1, "parts": [], "type": "M_END_OF_MESSAGE", "Display": "", "Special": []}');
function FIELD_DECODE_M_END_OF_MESSAGE(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_M_END_OF_MESSAGE, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_CarNumber = json.decode('{"Format": "UINT", "fldname": "CarNumber", "Length": 2, "parts": [], "type": "CarNumber", "Display": "", "Special": []}');
function FIELD_DECODE_CarNumber(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_CarNumber, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_CarName = json.decode('{"Format": "INT", "fldname": "CarName", "Length": 20, "parts": [], "type": "CarName", "Display": "", "Special": []}');
function FIELD_DECODE_CarName(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_CarName, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_CarID = json.decode('{"Format": "UINT", "fldname": "CarID", "Length": 2, "parts": [], "type": "CarID", "Display": "", "Special": []}');
function FIELD_DECODE_CarID(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_CarID, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_CarTyp = json.decode('{"Format": "UINT", "fldname": "CarTyp", "Length": 1, "parts": [], "type": "CarTyp", "Display": "", "Special": []}');
function FIELD_DECODE_CarTyp(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_CarTyp, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_OdoPosition = json.decode('{"Format": "INT", "fldname": "OdoPosition", "Length": 2, "parts": [], "type": "OdoPosition", "Display": "", "Special": []}');
function FIELD_DECODE_OdoPosition(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_OdoPosition, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_NewSpeed = json.decode('{"Format": "UINT", "fldname": "NewSpeed", "Length": 1, "parts": [], "type": "NewSpeed", "Display": "", "Special": []}');
function FIELD_DECODE_NewSpeed(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_NewSpeed, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Newgradient = json.decode('{"Format": "INT", "fldname": "Newgradient", "Length": 1, "parts": [], "type": "Newgradient", "Display": "", "Special": []}');
function FIELD_DECODE_Newgradient(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Newgradient, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_char = json.decode('{"Format": "INT", "fldname": "char", "Length": 1, "parts": [], "type": "char", "Display": "", "Special": []}');
function FIELD_DECODE_char(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_char, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_AreaID = json.decode('{"Format": "UINT", "fldname": "AreaID", "Length": 1, "parts": [], "type": "AreaID", "Display": "", "Special": []}');
function FIELD_DECODE_AreaID(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_AreaID, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_CarNumer = json.decode('{"Format": "UINT", "fldname": "CarNumer", "Length": 2, "parts": [], "type": "CarNumer", "Display": "", "Special": []}');
function FIELD_DECODE_CarNumer(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_CarNumer, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_LoadWeight = json.decode('{"Format": "UINT", "fldname": "LoadWeight", "Length": 2, "parts": [], "type": "LoadWeight", "Display": "", "Special": []}');
function FIELD_DECODE_LoadWeight(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_LoadWeight, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_RemainToNextBreak = json.decode('{"Format": "UINT", "fldname": "RemainToNextBreak", "Length": 2, "parts": [], "type": "RemainToNextBreak", "Display": "", "Special": []}');
function FIELD_DECODE_RemainToNextBreak(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_RemainToNextBreak, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_PermittedSpeed = json.decode('{"Format": "UINT", "fldname": "PermittedSpeed", "Length": 1, "parts": [], "type": "PermittedSpeed", "Display": "", "Special": []}');
function FIELD_DECODE_PermittedSpeed(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_PermittedSpeed, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_TargetSpeed = json.decode('{"Format": "UINT", "fldname": "TargetSpeed", "Length": 1, "parts": [], "type": "TargetSpeed", "Display": "", "Special": []}');
function FIELD_DECODE_TargetSpeed(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_TargetSpeed, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_TimeToIntervene = json.decode('{"Format": "UINT", "fldname": "TimeToIntervene", "Length": 1, "parts": [], "type": "TimeToIntervene", "Display": "", "Special": []}');
function FIELD_DECODE_TimeToIntervene(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_TimeToIntervene, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_RemainDistanceToTarget = json.decode('{"Format": "UINT", "fldname": "RemainDistanceToTarget", "Length": 2, "parts": [], "type": "RemainDistanceToTarget", "Display": "", "Special": []}');
function FIELD_DECODE_RemainDistanceToTarget(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_RemainDistanceToTarget, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_RemainDistanceToBCA = json.decode('{"Format": "UINT", "fldname": "RemainDistanceToBCA", "Length": 2, "parts": [], "type": "RemainDistanceToBCA", "Display": "", "Special": []}');
function FIELD_DECODE_RemainDistanceToBCA(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_RemainDistanceToBCA, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_PredictDistanceToStillLocation = json.decode('{"Format": "UINT", "fldname": "PredictDistanceToStillLocation", "Length": 2, "parts": [], "type": "PredictDistanceToStillLocation", "Display": "", "Special": []}');
function FIELD_DECODE_PredictDistanceToStillLocation(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_PredictDistanceToStillLocation, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_PredictRelativeSpeedAtBreakTarget = json.decode('{"Format": "UINT", "fldname": "PredictRelativeSpeedAtBreakTarget", "Length": 1, "parts": [], "type": "PredictRelativeSpeedAtBreakTarget", "Display": "", "Special": []}');
function FIELD_DECODE_PredictRelativeSpeedAtBreakTarget(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_PredictRelativeSpeedAtBreakTarget, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_CurrentSpeed = json.decode('{"Format": "UINT", "fldname": "CurrentSpeed", "Length": 2, "parts": [], "type": "CurrentSpeed", "Display": "", "Special": []}');
function FIELD_DECODE_CurrentSpeed(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_CurrentSpeed, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_CurrentOdometer = json.decode('{"Format": "INT", "fldname": "CurrentOdometer", "Length": 2, "parts": [], "type": "CurrentOdometer", "Display": "", "Special": []}');
function FIELD_DECODE_CurrentOdometer(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_CurrentOdometer, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_LeadTrackSection = json.decode('{"Format": "UINT", "fldname": "LeadTrackSection", "Length": 2, "parts": [], "type": "LeadTrackSection", "Display": "", "Special": []}');
function FIELD_DECODE_LeadTrackSection(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_LeadTrackSection, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_LeadingPosition1 = json.decode('{"Format": "UINT", "fldname": "LeadingPosition1", "Length": 2, "parts": [], "type": "LeadingPosition1", "Display": "", "Special": []}');
function FIELD_DECODE_LeadingPosition1(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_LeadingPosition1, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_LeadingPosition2 = json.decode('{"Format": "UINT", "fldname": "LeadingPosition2", "Length": 2, "parts": [], "type": "LeadingPosition2", "Display": "", "Special": []}');
function FIELD_DECODE_LeadingPosition2(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_LeadingPosition2, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_TrailingTrackSection = json.decode('{"Format": "UINT", "fldname": "TrailingTrackSection", "Length": 2, "parts": [], "type": "TrailingTrackSection", "Display": "", "Special": []}');
function FIELD_DECODE_TrailingTrackSection(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_TrailingTrackSection, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_TrailingPosition1 = json.decode('{"Format": "UINT", "fldname": "TrailingPosition1", "Length": 2, "parts": [], "type": "TrailingPosition1", "Display": "", "Special": []}');
function FIELD_DECODE_TrailingPosition1(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_TrailingPosition1, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_TrailingPosition2 = json.decode('{"Format": "UINT", "fldname": "TrailingPosition2", "Length": 2, "parts": [], "type": "TrailingPosition2", "Display": "", "Special": []}');
function FIELD_DECODE_TrailingPosition2(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_TrailingPosition2, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_TrainName = json.decode('{"Format": "INT", "fldname": "TrainName", "Length": 20, "parts": [], "type": "TrainName", "Display": "", "Special": []}');
function FIELD_DECODE_TrainName(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_TrainName, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_TrainLength = json.decode('{"Format": "UINT", "fldname": "TrainLength", "Length": 2, "parts": [], "type": "TrainLength", "Display": "", "Special": []}');
function FIELD_DECODE_TrainLength(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_TrainLength, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_DistanceFromBaliseToTrainFront = json.decode('{"Format": "UINT", "fldname": "DistanceFromBaliseToTrainFront", "Length": 2, "parts": [], "type": "DistanceFromBaliseToTrainFront", "Display": "", "Special": []}');
function FIELD_DECODE_DistanceFromBaliseToTrainFront(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_DistanceFromBaliseToTrainFront, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_DistanceFromBaliseToTrainEnd = json.decode('{"Format": "UINT", "fldname": "DistanceFromBaliseToTrainEnd", "Length": 2, "parts": [], "type": "DistanceFromBaliseToTrainEnd", "Display": "", "Special": []}');
function FIELD_DECODE_DistanceFromBaliseToTrainEnd(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_DistanceFromBaliseToTrainEnd, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_ErrorNumber = json.decode('{"Format": "UINT", "fldname": "ErrorNumber", "Length": 1, "parts": [], "type": "ErrorNumber", "Display": "", "Special": []}');
function FIELD_DECODE_ErrorNumber(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_ErrorNumber, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_LineNumber = json.decode('{"Format": "UINT", "fldname": "LineNumber", "Length": 2, "parts": [], "type": "LineNumber", "Display": "", "Special": []}');
function FIELD_DECODE_LineNumber(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_LineNumber, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_TimeStamp = json.decode('{"Format": "UINT", "fldname": "TimeStamp", "Length": 4, "parts": [], "type": "TimeStamp", "Display": "", "Special": []}');
function FIELD_DECODE_TimeStamp(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_TimeStamp, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_TextType = json.decode('{"Format": "UINT", "fldname": "TextType", "Length": 1, "parts": [], "type": "TextType", "Display": "", "Special": []}');
function FIELD_DECODE_TextType(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_TextType, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Time = json.decode('{"Format": "UINT", "fldname": "Time", "Length": 4, "parts": [], "type": "Time", "Display": "", "Special": []}');
function FIELD_DECODE_Time(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Time, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_CarNameListCnt = json.decode('{"Format": "UINT", "fldname": "CarNameListCnt", "Length": 2, "parts": [], "type": "CarNameListCnt", "Display": "", "Special": []}');
function FIELD_DECODE_CarNameListCnt(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_CarNameListCnt, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_DMIStatusWord = json.decode('{"Format": "UINT", "fldname": "DMIStatusWord", "Length": 2, "parts": [], "type": "DMIStatusWord", "Display": "", "Special": []}');
function FIELD_DECODE_DMIStatusWord(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_DMIStatusWord, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_CarListCnt = json.decode('{"Format": "UINT", "fldname": "CarListCnt", "Length": 2, "parts": [], "type": "CarListCnt", "Display": "", "Special": []}');
function FIELD_DECODE_CarListCnt(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_CarListCnt, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_DriverID = json.decode('{"Format": "INT", "fldname": "DriverID", "Length": 20, "parts": [], "type": "DriverID", "Display": "", "Special": []}');
function FIELD_DECODE_DriverID(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_DriverID, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Password = json.decode('{"Format": "INT", "fldname": "Password", "Length": 10, "parts": [], "type": "Password", "Display": "", "Special": []}');
function FIELD_DECODE_Password(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Password, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_CarStatusCnt = json.decode('{"Format": "UINT", "fldname": "CarStatusCnt", "Length": 2, "parts": [], "type": "CarStatusCnt", "Display": "", "Special": []}');
function FIELD_DECODE_CarStatusCnt(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_CarStatusCnt, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_SpeedBlockCnt = json.decode('{"Format": "UINT", "fldname": "SpeedBlockCnt", "Length": 2, "parts": [], "type": "SpeedBlockCnt", "Display": "", "Special": []}');
function FIELD_DECODE_SpeedBlockCnt(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_SpeedBlockCnt, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_GradientBlockCnt = json.decode('{"Format": "UINT", "fldname": "GradientBlockCnt", "Length": 2, "parts": [], "type": "GradientBlockCnt", "Display": "", "Special": []}');
function FIELD_DECODE_GradientBlockCnt(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_GradientBlockCnt, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_DMIStartupStatus = json.decode('{"Format": "UINT", "fldname": "DMIStartupStatus", "Length": 2, "parts": [], "type": "DMIStartupStatus", "Display": "", "Special": []}');
function FIELD_DECODE_DMIStartupStatus(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_DMIStartupStatus, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_CompatVersion = json.decode('{"Format": "UINT", "fldname": "CompatVersion", "Length": 1, "parts": [], "type": "CompatVersion", "Display": "", "Special": []}');
function FIELD_DECODE_CompatVersion(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_CompatVersion, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_ReconfigTims = json.decode('{"Format": "UINT", "fldname": "ReconfigTims", "Length": 1, "parts": [], "type": "ReconfigTims", "Display": "", "Special": []}');
function FIELD_DECODE_ReconfigTims(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_ReconfigTims, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_ConfirmationRequest = json.decode('{"Format": "UINT", "fldname": "ConfirmationRequest", "Length": 1, "parts": [], "type": "ConfirmationRequest", "Display": "", "Special": []}');
function FIELD_DECODE_ConfirmationRequest(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_ConfirmationRequest, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_TextMessageID = json.decode('{"Format": "UINT", "fldname": "TextMessageID", "Length": 1, "parts": [], "type": "TextMessageID", "Display": "", "Special": []}');
function FIELD_DECODE_TextMessageID(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_TextMessageID, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_AtpNotificationMode = json.decode('{"Format": "UINT", "fldname": "AtpNotificationMode", "Length": 1, "parts": [], "type": "AtpNotificationMode", "Display": "", "Special": []}');
function FIELD_DECODE_AtpNotificationMode(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_AtpNotificationMode, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_AtpNotificationText = json.decode('{"Format": "INT", "fldname": "AtpNotificationText", "Length": 100, "parts": [], "type": "AtpNotificationText", "Display": "", "Special": []}');
function FIELD_DECODE_AtpNotificationText(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_AtpNotificationText, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_HistoryCnt = json.decode('{"Format": "UINT", "fldname": "HistoryCnt", "Length": 2, "parts": [], "type": "HistoryCnt", "Display": "", "Special": []}');
function FIELD_DECODE_HistoryCnt(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_HistoryCnt, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_AreaCnt = json.decode('{"Format": "UINT", "fldname": "AreaCnt", "Length": 2, "parts": [], "type": "AreaCnt", "Display": "", "Special": []}');
function FIELD_DECODE_AreaCnt(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_AreaCnt, fld, subflds)
end
        
--- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

--- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
--- Decode of blocks

-------------------------------
fld_CarNameListEntry = ProtoField.new("CarNameListEntry","CarNameListEntry",ftypes.NONE)


BF_CarNameListEntry_00_CarNumber = ProtoField.uint16( "BF_CarNameListEntry_00_CarNumber", "CarNumber", base.DEC )

BF_CarNameListEntry_01_CarName = ProtoField.string( "BF_CarNameListEntry_01_CarName", "CarName", base.ASCII )
function BLOCK_DECODE_CarNameListEntry(ctx)
    local btree = ctx.stack:top():add(fld_CarNameListEntry,ctx.tvbuf:range(ctx.off,22));
    ctx.stack:push(btree);

    FIELD_DECODE_CarNumber(ctx,BF_CarNameListEntry_00_CarNumber,{  })
    FIELD_DECODE_CarName(ctx,BF_CarNameListEntry_01_CarName,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_CarListEntry = ProtoField.new("CarListEntry","CarListEntry",ftypes.NONE)


BF_CarListEntry_00_CarID = ProtoField.uint16( "BF_CarListEntry_00_CarID", "CarID", base.DEC )

BF_CarListEntry_01_CarTyp = ProtoField.uint8( "BF_CarListEntry_01_CarTyp", "CarTyp", base.DEC )
function BLOCK_DECODE_CarListEntry(ctx)
    local btree = ctx.stack:top():add(fld_CarListEntry,ctx.tvbuf:range(ctx.off,3));
    ctx.stack:push(btree);

    FIELD_DECODE_CarID(ctx,BF_CarListEntry_00_CarID,{  })
    FIELD_DECODE_CarTyp(ctx,BF_CarListEntry_01_CarTyp,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_SpeedBlock = ProtoField.new("SpeedBlock","SpeedBlock",ftypes.NONE)


BF_SpeedBlock_00_OdoPosition = ProtoField.int16( "BF_SpeedBlock_00_OdoPosition", "Odometer position", base.DEC )

BF_SpeedBlock_01_NewSpeed = ProtoField.uint8( "BF_SpeedBlock_01_NewSpeed", "Odometer position", base.DEC )

BF_SpeedBlock_02_SpeedReason = ProtoField.uint8( "BF_SpeedBlock_02_SpeedReason", "SpeedReason", base.DEC  ,{ [0] = "Undefined",[1] = "Point Straight",[2] = "Point Curve",[3] = "Point Passed",[4] = "Location",[5] = "Other",[6] = "End of MA",[7] = "TSR (temporary speed restriction)",[8] = "Conditional target (e.g. pantograph shift)",[9] = "Pantograph shift to none",[10] = "Pantograph shift to roof",[11] = "Pantograph shift to side",[255] = "reference for odometer value" } )
function BLOCK_DECODE_SpeedBlock(ctx)
    local btree = ctx.stack:top():add(fld_SpeedBlock,ctx.tvbuf:range(ctx.off,4));
    ctx.stack:push(btree);

    FIELD_DECODE_OdoPosition(ctx,BF_SpeedBlock_00_OdoPosition,{  })
    FIELD_DECODE_NewSpeed(ctx,BF_SpeedBlock_01_NewSpeed,{  })
    FIELD_DECODE_SpeedReason(ctx,BF_SpeedBlock_02_SpeedReason,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_GradientBlock = ProtoField.new("GradientBlock","GradientBlock",ftypes.NONE)


BF_GradientBlock_00_OdoPosition = ProtoField.int16( "BF_GradientBlock_00_OdoPosition", "Odometer position", base.DEC )

BF_GradientBlock_01_Newgradient = ProtoField.int8( "BF_GradientBlock_01_Newgradient", "New Gradient", base.DEC )
function BLOCK_DECODE_GradientBlock(ctx)
    local btree = ctx.stack:top():add(fld_GradientBlock,ctx.tvbuf:range(ctx.off,3));
    ctx.stack:push(btree);

    FIELD_DECODE_OdoPosition(ctx,BF_GradientBlock_00_OdoPosition,{  })
    FIELD_DECODE_Newgradient(ctx,BF_GradientBlock_01_Newgradient,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_HistoryBlock = ProtoField.new("HistoryBlock","HistoryBlock",ftypes.NONE)


BF_HistoryBlock_00_char = ProtoField.int8( "BF_HistoryBlock_00_char", "char", base.DEC )
function BLOCK_DECODE_HistoryBlock(ctx)
    local btree = ctx.stack:top():add(fld_HistoryBlock,ctx.tvbuf:range(ctx.off,1));
    ctx.stack:push(btree);

    FIELD_DECODE_char(ctx,BF_HistoryBlock_00_char,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_AreaBlock = ProtoField.new("AreaBlock","AreaBlock",ftypes.NONE)


BF_AreaBlock_00_AreaID = ProtoField.uint8( "BF_AreaBlock_00_AreaID", "AreaID", base.DEC )
function BLOCK_DECODE_AreaBlock(ctx)
    local btree = ctx.stack:top():add(fld_AreaBlock,ctx.tvbuf:range(ctx.off,1));
    ctx.stack:push(btree);

    FIELD_DECODE_AreaID(ctx,BF_AreaBlock_00_AreaID,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_CarStatus = ProtoField.new("CarStatus","CarStatus",ftypes.NONE)


BF_CarStatus_00_CarNumer = ProtoField.uint16( "BF_CarStatus_00_CarNumer", "CarNumer", base.DEC )

BF_CarStatus_01_LoadWeight = ProtoField.uint16( "BF_CarStatus_01_LoadWeight", "Load Weight", base.DEC )
BF_CarStatus_02_TicStatus_bit0 = ProtoField.uint16( "BF_CarStatus_02_TicStatus.bit0", "TIC_TrainConfigInput", base.HEX, nil, 1 )
BF_CarStatus_02_TicStatus_bit1 = ProtoField.uint16( "BF_CarStatus_02_TicStatus.bit1", "TIC_DerailInhibit", base.HEX, nil, 2 )
BF_CarStatus_02_TicStatus_bit2 = ProtoField.uint16( "BF_CarStatus_02_TicStatus.bit2", "TIC_Derail", base.HEX, nil, 4 )
BF_CarStatus_02_TicStatus_bit3 = ProtoField.uint16( "BF_CarStatus_02_TicStatus.bit3", "TIC_PlatformError", base.HEX, nil, 8 )
BF_CarStatus_02_TicStatus_bit8 = ProtoField.uint16( "BF_CarStatus_02_TicStatus.bit8", "TIC_FrontRightDerail", base.HEX, nil, 256 )
BF_CarStatus_02_TicStatus_bit9 = ProtoField.uint16( "BF_CarStatus_02_TicStatus.bit9", "TIC_FrontLeftDerail", base.HEX, nil, 512 )
BF_CarStatus_02_TicStatus_bit10 = ProtoField.uint16( "BF_CarStatus_02_TicStatus.bit10", "TIC_RearRightDerail", base.HEX, nil, 1024 )
BF_CarStatus_02_TicStatus_bit11 = ProtoField.uint16( "BF_CarStatus_02_TicStatus.bit11", "TIC_RearLeftDerail", base.HEX, nil, 2048 )
BF_CarStatus_02_TicStatus_bit12 = ProtoField.uint16( "BF_CarStatus_02_TicStatus.bit12", "TIC_CarDumpBottom", base.HEX, nil, 4096 )
BF_CarStatus_02_TicStatus_bit13 = ProtoField.uint16( "BF_CarStatus_02_TicStatus.bit13", "TIC_CarDumpTop", base.HEX, nil, 8192 )
BF_CarStatus_02_TicStatus_bit14 = ProtoField.uint16( "BF_CarStatus_02_TicStatus.bit14", "TIC_CarDumpClosed", base.HEX, nil, 16384 )
BF_CarStatus_02_TicStatus_bit15 = ProtoField.uint16( "BF_CarStatus_02_TicStatus.bit15", "TIC_LoadWeightBad", base.HEX, nil, 32768 )

BF_CarStatus_02_TicStatus = ProtoField.uint16( "BF_CarStatus_02_TicStatus", "TicStatus", base.HEX )

BF_CarStatus_03_PlatformStatus = ProtoField.uint8( "BF_CarStatus_03_PlatformStatus", "PlatformStatus", base.DEC  ,{ [0] = "cpsUndefined",[1] = "cpsDOWN",[2] = "cpsGoingUP",[3] = "cpsUP",[4] = "cpsGoingDOWN" } )
function BLOCK_DECODE_CarStatus(ctx)
    local btree = ctx.stack:top():add(fld_CarStatus,ctx.tvbuf:range(ctx.off,7));
    ctx.stack:push(btree);

    FIELD_DECODE_CarNumer(ctx,BF_CarStatus_00_CarNumer,{  })
    FIELD_DECODE_LoadWeight(ctx,BF_CarStatus_01_LoadWeight,{  })
    FIELD_DECODE_TicStatus(ctx,BF_CarStatus_02_TicStatus,{ BF_CarStatus_02_TicStatus_bit0,BF_CarStatus_02_TicStatus_bit1,BF_CarStatus_02_TicStatus_bit2,BF_CarStatus_02_TicStatus_bit3,BF_CarStatus_02_TicStatus_bit8,BF_CarStatus_02_TicStatus_bit9,BF_CarStatus_02_TicStatus_bit10,BF_CarStatus_02_TicStatus_bit11,BF_CarStatus_02_TicStatus_bit12,BF_CarStatus_02_TicStatus_bit13,BF_CarStatus_02_TicStatus_bit14,BF_CarStatus_02_TicStatus_bit15 })
    FIELD_DECODE_PlatformStatus(ctx,BF_CarStatus_03_PlatformStatus,{  })
    ctx.stack:pop(1);
end

--- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

--- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
--- Decode of messages

-------------------------------


MF_ATP_mode_and_states_00_ATP_Mode = ProtoField.uint8( "tcc.ATP_mode_and_states.ATP_Mode", "ATP_Mode", base.DEC  ,{ [0] = "Undefined",[2] = "Power up",[2] = "Train configuration",[3] = "Train registration",[4] = "Balise Search",[5] = "Normal",[6] = "Shunting",[7] = "Location",[8] = "AutomaticUnload",[9] = "Yard",[10] = "Unregistered",[11] = "PoweringDown",[12] = "SafetyHalt",[13] = "Sleeping",[14] = "StaffResponsible",[15] = "ShuntingRoute",[16] = "Possession",[17] = "Split",[18] = "Join",[19] = "SafeBrakeToStop" }  )

MF_ATP_mode_and_states_01_ATP_State = ProtoField.uint8( "tcc.ATP_mode_and_states.ATP_State", "ATP_State", base.DEC  ,{ [0] = "Undefined",[1] = "BasicSystemStartUp",[2] = "ApplicationStartUp",[3] = "Inactive",[4] = "ActivationInitiation",[5] = "ActivationTest",[6] = "Active",[7] = "FatalFailureState",[8] = "SystemRestart",[9] = "Power down" }  )

MF_ATP_mode_and_states_02_ATP_Mode_Extra = ProtoField.uint8( "tcc.ATP_mode_and_states.ATP_Mode_Extra", "ATP_Mode_Extra", base.DEC  ,{ [0] = "Undefined",[1] = "Manual/Allow driver to Select Mode Configuration, Yard",[2] = "ReConfig" }  )

MF_ATP_mode_and_states_03_Driver_Verification_State = ProtoField.uint8( "tcc.ATP_mode_and_states.Driver_Verification_State", "Driver_Verification_State", base.DEC  ,{ [0] = "Undefined",[1] = "NoActionState",[2] = "InputState",[3] = "VerificationState",[4] = "RedoInputState",[5] = "Logged on" }  )
MF_ATP_mode_and_states_04_Train_State_bit0 = ProtoField.uint8( "MF_ATP_mode_and_states_04_Train_State.bit0", "Radio available", base.HEX, nil, 1 )
MF_ATP_mode_and_states_04_Train_State_bit1 = ProtoField.uint8( "MF_ATP_mode_and_states_04_Train_State.bit1", "TIMS available", base.HEX, nil, 2 )
MF_ATP_mode_and_states_04_Train_State_bit2 = ProtoField.uint8( "MF_ATP_mode_and_states_04_Train_State.bit2", "TIMS Ok", base.HEX, nil, 4 )
MF_ATP_mode_and_states_04_Train_State_bit3 = ProtoField.uint8( "MF_ATP_mode_and_states_04_Train_State.bit3", "Odometer invalid", base.HEX, nil, 8 )
MF_ATP_mode_and_states_04_Train_State_bit4 = ProtoField.uint8( "MF_ATP_mode_and_states_04_Train_State.bit4", "Stop Train request", base.HEX, nil, 16 )
MF_ATP_mode_and_states_04_Train_State_bit5 = ProtoField.uint8( "MF_ATP_mode_and_states_04_Train_State.bit5", "Configuration rejected", base.HEX, nil, 32 )
MF_ATP_mode_and_states_04_Train_State_bit6 = ProtoField.uint8( "MF_ATP_mode_and_states_04_Train_State.bit6", "Train integrity granted by driver", base.HEX, nil, 64 )

MF_ATP_mode_and_states_04_Train_State = ProtoField.uint8( "tcc.ATP_mode_and_states.Train_State", "Train_State", base.HEX  )
MF_ATP_mode_and_states_05_Locomotive_State_bit0 = ProtoField.uint32( "MF_ATP_mode_and_states_05_Locomotive_State.bit0", "Safety Halt", base.HEX, nil, 1 )
MF_ATP_mode_and_states_05_Locomotive_State_bit1 = ProtoField.uint32( "MF_ATP_mode_and_states_05_Locomotive_State.bit1", "EmergencyAlert from driver", base.HEX, nil, 2 )
MF_ATP_mode_and_states_05_Locomotive_State_bit2 = ProtoField.uint32( "MF_ATP_mode_and_states_05_Locomotive_State.bit2", "TIMS Integrity Broken", base.HEX, nil, 4 )
MF_ATP_mode_and_states_05_Locomotive_State_bit3 = ProtoField.uint32( "MF_ATP_mode_and_states_05_Locomotive_State.bit3", "Braking event", base.HEX, nil, 8 )
MF_ATP_mode_and_states_05_Locomotive_State_bit4 = ProtoField.uint32( "MF_ATP_mode_and_states_05_Locomotive_State.bit4", "HandlingDone", base.HEX, nil, 16 )
MF_ATP_mode_and_states_05_Locomotive_State_bit5 = ProtoField.uint32( "MF_ATP_mode_and_states_05_Locomotive_State.bit5", "TrainIdling", base.HEX, nil, 32 )
MF_ATP_mode_and_states_05_Locomotive_State_bit6 = ProtoField.uint32( "MF_ATP_mode_and_states_05_Locomotive_State.bit6", "TIMS Integrity Manual Override from Driver", base.HEX, nil, 64 )
MF_ATP_mode_and_states_05_Locomotive_State_bit7 = ProtoField.uint32( "MF_ATP_mode_and_states_05_Locomotive_State.bit7", "MA Timeout", base.HEX, nil, 128 )
MF_ATP_mode_and_states_05_Locomotive_State_bit8 = ProtoField.uint32( "MF_ATP_mode_and_states_05_Locomotive_State.bit8", "ATP Reset", base.HEX, nil, 256 )
MF_ATP_mode_and_states_05_Locomotive_State_bit9 = ProtoField.uint32( "MF_ATP_mode_and_states_05_Locomotive_State.bit9", "ATP_needs to be reset", base.HEX, nil, 512 )
MF_ATP_mode_and_states_05_Locomotive_State_bit10 = ProtoField.uint32( "MF_ATP_mode_and_states_05_Locomotive_State.bit10", "ATP intervention", base.HEX, nil, 1024 )
MF_ATP_mode_and_states_05_Locomotive_State_bit11 = ProtoField.uint32( "MF_ATP_mode_and_states_05_Locomotive_State.bit11", "Brake release requested", base.HEX, nil, 2048 )
MF_ATP_mode_and_states_05_Locomotive_State_bit12 = ProtoField.uint32( "MF_ATP_mode_and_states_05_Locomotive_State.bit12", "Manual TIMS confirmation", base.HEX, nil, 4096 )
MF_ATP_mode_and_states_05_Locomotive_State_bit13 = ProtoField.uint32( "MF_ATP_mode_and_states_05_Locomotive_State.bit13", "Slip detected", base.HEX, nil, 8192 )
MF_ATP_mode_and_states_05_Locomotive_State_bit14 = ProtoField.uint32( "MF_ATP_mode_and_states_05_Locomotive_State.bit14", "Free rolling", base.HEX, nil, 16384 )
MF_ATP_mode_and_states_05_Locomotive_State_bit15 = ProtoField.uint32( "MF_ATP_mode_and_states_05_Locomotive_State.bit15", "Emergency Alert active", base.HEX, nil, 32768 )
MF_ATP_mode_and_states_05_Locomotive_State_bit16 = ProtoField.uint32( "MF_ATP_mode_and_states_05_Locomotive_State.bit16", "AttentionNeeded (ATO)", base.HEX, nil, 65536 )
MF_ATP_mode_and_states_05_Locomotive_State_bit17 = ProtoField.uint32( "MF_ATP_mode_and_states_05_Locomotive_State.bit17", "Not ready to drive (ATO)", base.HEX, nil, 131072 )
MF_ATP_mode_and_states_05_Locomotive_State_bit18 = ProtoField.uint32( "MF_ATP_mode_and_states_05_Locomotive_State.bit18", "Safe for boarding (ATO)", base.HEX, nil, 262144 )

MF_ATP_mode_and_states_05_Locomotive_State = ProtoField.uint32( "tcc.ATP_mode_and_states.Locomotive_State", "Locomotive_State", base.HEX  )

MF_ATP_mode_and_states_06_ATO_Mode = ProtoField.uint8( "tcc.ATP_mode_and_states.ATO_Mode", "ATO_Mode", base.DEC  ,{ [0] = "ATOModeUndefined",[1] = "ATOModeManual",[2] = "ATOModeSupervisedAutomatic",[3] = "ATOModeAutomatic",[4] = "ATOModeRemote" }  )

MF_ATP_mode_and_states_07_ATO_SwitchPos = ProtoField.uint8( "tcc.ATP_mode_and_states.ATO_SwitchPos", "ATO_SwitchPos", base.DEC  ,{ [0] = "Undefined",[1] = "Manual",[2] = "Supervised Automatic",[3] = "Automatic",[4] = "Illegal" }  )
MF_ATP_mode_and_states_08_AdditionalStatus1_bit0 = ProtoField.uint8( "MF_ATP_mode_and_states_08_AdditionalStatus1.bit0", "ATO Enable", base.HEX, nil, 1 )
MF_ATP_mode_and_states_08_AdditionalStatus1_bit1 = ProtoField.uint8( "MF_ATP_mode_and_states_08_AdditionalStatus1.bit1", "Driving forward", base.HEX, nil, 2 )
MF_ATP_mode_and_states_08_AdditionalStatus1_bit2 = ProtoField.uint8( "MF_ATP_mode_and_states_08_AdditionalStatus1.bit2", "Standstill Event active", base.HEX, nil, 4 )
MF_ATP_mode_and_states_08_AdditionalStatus1_bit3 = ProtoField.uint8( "MF_ATP_mode_and_states_08_AdditionalStatus1.bit3", "Brake Test possible", base.HEX, nil, 8 )
MF_ATP_mode_and_states_08_AdditionalStatus1_bit4 = ProtoField.uint8( "MF_ATP_mode_and_states_08_AdditionalStatus1.bit4", "Brake Test notification", base.HEX, nil, 16 )
MF_ATP_mode_and_states_08_AdditionalStatus1_bit5 = ProtoField.uint8( "MF_ATP_mode_and_states_08_AdditionalStatus1.bit5", "Brake Test mandatory", base.HEX, nil, 32 )

MF_ATP_mode_and_states_08_AdditionalStatus1 = ProtoField.uint8( "tcc.ATP_mode_and_states.AdditionalStatus1", "AdditionalStatus1", base.HEX  )
MF_ATP_mode_and_states_09_AdditionalStatus2_bit0 = ProtoField.uint8( "MF_ATP_mode_and_states_09_AdditionalStatus2.bit0", "Com status", base.HEX, nil, 1 )

MF_ATP_mode_and_states_09_AdditionalStatus2 = ProtoField.uint8( "tcc.ATP_mode_and_states.AdditionalStatus2", "AdditionalStatus2", base.HEX  )

MF_ATP_mode_and_states_10_Confirm_change = ProtoField.uint8( "tcc.ATP_mode_and_states.Confirm_change", "Confirm_change", base.DEC  ,{ [0] = "Nothing to confirm",[1] = "Yard",[2] = "Shunting Route",[3] = "Staff Responsible" }  )
MF_ATP_mode_and_states_11_Allow_bit0 = ProtoField.uint8( "MF_ATP_mode_and_states_11_Allow.bit0", "Allowed to login", base.HEX, nil, 1 )
MF_ATP_mode_and_states_11_Allow_bit1 = ProtoField.uint8( "MF_ATP_mode_and_states_11_Allow.bit1", "Allowed to enter Yard mode", base.HEX, nil, 2 )
MF_ATP_mode_and_states_11_Allow_bit2 = ProtoField.uint8( "MF_ATP_mode_and_states_11_Allow.bit2", "Allowed to enter Possession", base.HEX, nil, 4 )
MF_ATP_mode_and_states_11_Allow_bit3 = ProtoField.uint8( "MF_ATP_mode_and_states_11_Allow.bit3", "Allowed to enter Shunting", base.HEX, nil, 8 )

MF_ATP_mode_and_states_11_Allow = ProtoField.uint8( "tcc.ATP_mode_and_states.Allow", "Allow", base.HEX  )

MF_ATP_mode_and_states_12_BreakStatus = ProtoField.uint8( "tcc.ATP_mode_and_states.BreakStatus", "BreakStatus", base.DEC  ,{ [0] = "No status",[1] = "Brake Test in progress",[2] = "Brake Test aborted",[3] = "Brake Test failed",[4] = "Brake Test successful" }  )

MF_ATP_mode_and_states_13_RemainToNextBreak = ProtoField.uint16( "tcc.ATP_mode_and_states.RemainToNextBreak", "Remaining time to next mandatory Brake Test", base.DEC  )
function MSG_DECODE_ATP_mode_and_states(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_ATP_Mode(ctx,MF_ATP_mode_and_states_00_ATP_Mode, {  })
    FIELD_DECODE_ATP_State(ctx,MF_ATP_mode_and_states_01_ATP_State, {  })
    FIELD_DECODE_ATP_Mode_Extra(ctx,MF_ATP_mode_and_states_02_ATP_Mode_Extra, {  })
    FIELD_DECODE_Driver_Verification_State(ctx,MF_ATP_mode_and_states_03_Driver_Verification_State, {  })
    FIELD_DECODE_Train_State(ctx,MF_ATP_mode_and_states_04_Train_State, { MF_ATP_mode_and_states_04_Train_State_bit0,MF_ATP_mode_and_states_04_Train_State_bit1,MF_ATP_mode_and_states_04_Train_State_bit2,MF_ATP_mode_and_states_04_Train_State_bit3,MF_ATP_mode_and_states_04_Train_State_bit4,MF_ATP_mode_and_states_04_Train_State_bit5,MF_ATP_mode_and_states_04_Train_State_bit6 })
    FIELD_DECODE_Locomotive_State(ctx,MF_ATP_mode_and_states_05_Locomotive_State, { MF_ATP_mode_and_states_05_Locomotive_State_bit0,MF_ATP_mode_and_states_05_Locomotive_State_bit1,MF_ATP_mode_and_states_05_Locomotive_State_bit2,MF_ATP_mode_and_states_05_Locomotive_State_bit3,MF_ATP_mode_and_states_05_Locomotive_State_bit4,MF_ATP_mode_and_states_05_Locomotive_State_bit5,MF_ATP_mode_and_states_05_Locomotive_State_bit6,MF_ATP_mode_and_states_05_Locomotive_State_bit7,MF_ATP_mode_and_states_05_Locomotive_State_bit8,MF_ATP_mode_and_states_05_Locomotive_State_bit9,MF_ATP_mode_and_states_05_Locomotive_State_bit10,MF_ATP_mode_and_states_05_Locomotive_State_bit11,MF_ATP_mode_and_states_05_Locomotive_State_bit12,MF_ATP_mode_and_states_05_Locomotive_State_bit13,MF_ATP_mode_and_states_05_Locomotive_State_bit14,MF_ATP_mode_and_states_05_Locomotive_State_bit15,MF_ATP_mode_and_states_05_Locomotive_State_bit16,MF_ATP_mode_and_states_05_Locomotive_State_bit17,MF_ATP_mode_and_states_05_Locomotive_State_bit18 })
    FIELD_DECODE_ATO_Mode(ctx,MF_ATP_mode_and_states_06_ATO_Mode, {  })
    FIELD_DECODE_ATO_SwitchPos(ctx,MF_ATP_mode_and_states_07_ATO_SwitchPos, {  })
    FIELD_DECODE_AdditionalStatus1(ctx,MF_ATP_mode_and_states_08_AdditionalStatus1, { MF_ATP_mode_and_states_08_AdditionalStatus1_bit0,MF_ATP_mode_and_states_08_AdditionalStatus1_bit1,MF_ATP_mode_and_states_08_AdditionalStatus1_bit2,MF_ATP_mode_and_states_08_AdditionalStatus1_bit3,MF_ATP_mode_and_states_08_AdditionalStatus1_bit4,MF_ATP_mode_and_states_08_AdditionalStatus1_bit5 })
    FIELD_DECODE_AdditionalStatus2(ctx,MF_ATP_mode_and_states_09_AdditionalStatus2, { MF_ATP_mode_and_states_09_AdditionalStatus2_bit0 })
    FIELD_DECODE_Confirm_change(ctx,MF_ATP_mode_and_states_10_Confirm_change, {  })
    FIELD_DECODE_Allow(ctx,MF_ATP_mode_and_states_11_Allow, { MF_ATP_mode_and_states_11_Allow_bit0,MF_ATP_mode_and_states_11_Allow_bit1,MF_ATP_mode_and_states_11_Allow_bit2,MF_ATP_mode_and_states_11_Allow_bit3 })
    FIELD_DECODE_BreakStatus(ctx,MF_ATP_mode_and_states_12_BreakStatus, {  })
    FIELD_DECODE_RemainToNextBreak(ctx,MF_ATP_mode_and_states_13_RemainToNextBreak, {  })
    
end

-------------------------------


MF_Driver_info_00_PermittedDirection = ProtoField.uint8( "tcc.Driver_info.PermittedDirection", "PermittedDirection", base.DEC  ,{ [0] = "Undefined",[1] = "None",[2] = "Forward",[3] = "Reverse",[4] = "Both" }  )

MF_Driver_info_01_PermittedSpeed = ProtoField.uint8( "tcc.Driver_info.PermittedSpeed", "Permitted speed", base.DEC  )

MF_Driver_info_02_TargetSpeed = ProtoField.uint8( "tcc.Driver_info.TargetSpeed", "Target speed", base.DEC  )

MF_Driver_info_03_TimeToIntervene = ProtoField.uint8( "tcc.Driver_info.TimeToIntervene", "Time to intervention", base.DEC  )

MF_Driver_info_04_RemainDistanceToTarget = ProtoField.uint16( "tcc.Driver_info.RemainDistanceToTarget", "Remaining distance to target point", base.DEC  )

MF_Driver_info_05_RemainDistanceToBCA = ProtoField.uint16( "tcc.Driver_info.RemainDistanceToBCA", "Remaining distance to BCA", base.DEC  )

MF_Driver_info_06_PredictDistanceToStillLocation = ProtoField.uint16( "tcc.Driver_info.PredictDistanceToStillLocation", "Predicted distance to stand still location", base.DEC  )

MF_Driver_info_07_PredictRelativeSpeedAtBreakTarget = ProtoField.uint8( "tcc.Driver_info.PredictRelativeSpeedAtBreakTarget", "Predicted relative speed at brake target", base.DEC  )
MF_Driver_info_08_StatusSignals_bit0 = ProtoField.uint8( "MF_Driver_info_08_StatusSignals.bit0", "In BCA", base.HEX, nil, 1 )
MF_Driver_info_08_StatusSignals_bit1 = ProtoField.uint8( "MF_Driver_info_08_StatusSignals.bit1", "ATP Warning", base.HEX, nil, 2 )
MF_Driver_info_08_StatusSignals_bit2 = ProtoField.uint8( "MF_Driver_info_08_StatusSignals.bit2", "ATP Intervention", base.HEX, nil, 4 )
MF_Driver_info_08_StatusSignals_bit3 = ProtoField.uint8( "MF_Driver_info_08_StatusSignals.bit3", "Flash SB Brake button", base.HEX, nil, 8 )
MF_Driver_info_08_StatusSignals_bit4 = ProtoField.uint8( "MF_Driver_info_08_StatusSignals.bit4", "Radio Available", base.HEX, nil, 16 )
MF_Driver_info_08_StatusSignals_bit5 = ProtoField.uint8( "MF_Driver_info_08_StatusSignals.bit5", "Service Brakes applied", base.HEX, nil, 32 )
MF_Driver_info_08_StatusSignals_bit6 = ProtoField.uint8( "MF_Driver_info_08_StatusSignals.bit6", "Emergency Brakes applied", base.HEX, nil, 64 )
MF_Driver_info_08_StatusSignals_bit7 = ProtoField.uint8( "MF_Driver_info_08_StatusSignals.bit7", "Flash EB Brake button", base.HEX, nil, 128 )

MF_Driver_info_08_StatusSignals = ProtoField.uint8( "tcc.Driver_info.StatusSignals", "StatusSignals", base.HEX  )
MF_Driver_info_09_VisibibleSignals_bit0 = ProtoField.uint8( "MF_Driver_info_09_VisibibleSignals.bit0", "Indicate Permitted direction", base.HEX, nil, 1 )
MF_Driver_info_09_VisibibleSignals_bit1 = ProtoField.uint8( "MF_Driver_info_09_VisibibleSignals.bit1", "Indicate Permitted speed", base.HEX, nil, 2 )
MF_Driver_info_09_VisibibleSignals_bit2 = ProtoField.uint8( "MF_Driver_info_09_VisibibleSignals.bit2", "Indicate Target speed", base.HEX, nil, 4 )
MF_Driver_info_09_VisibibleSignals_bit3 = ProtoField.uint8( "MF_Driver_info_09_VisibibleSignals.bit3", "Indicate Remaining distance to target point", base.HEX, nil, 8 )
MF_Driver_info_09_VisibibleSignals_bit4 = ProtoField.uint8( "MF_Driver_info_09_VisibibleSignals.bit4", "Indicate Remaining distance to BCA", base.HEX, nil, 16 )
MF_Driver_info_09_VisibibleSignals_bit5 = ProtoField.uint8( "MF_Driver_info_09_VisibibleSignals.bit5", "Indicate Predicted distance to stand still location", base.HEX, nil, 32 )
MF_Driver_info_09_VisibibleSignals_bit6 = ProtoField.uint8( "MF_Driver_info_09_VisibibleSignals.bit6", "Indicate Predicted speed at brake target", base.HEX, nil, 64 )
MF_Driver_info_09_VisibibleSignals_bit7 = ProtoField.uint8( "MF_Driver_info_09_VisibibleSignals.bit7", "Indicate Time to intervention", base.HEX, nil, 128 )

MF_Driver_info_09_VisibibleSignals = ProtoField.uint8( "tcc.Driver_info.VisibibleSignals", "VisibibleSignals", base.HEX  )
function MSG_DECODE_Driver_info(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_PermittedDirection(ctx,MF_Driver_info_00_PermittedDirection, {  })
    FIELD_DECODE_PermittedSpeed(ctx,MF_Driver_info_01_PermittedSpeed, {  })
    FIELD_DECODE_TargetSpeed(ctx,MF_Driver_info_02_TargetSpeed, {  })
    FIELD_DECODE_TimeToIntervene(ctx,MF_Driver_info_03_TimeToIntervene, {  })
    FIELD_DECODE_RemainDistanceToTarget(ctx,MF_Driver_info_04_RemainDistanceToTarget, {  })
    FIELD_DECODE_RemainDistanceToBCA(ctx,MF_Driver_info_05_RemainDistanceToBCA, {  })
    FIELD_DECODE_PredictDistanceToStillLocation(ctx,MF_Driver_info_06_PredictDistanceToStillLocation, {  })
    FIELD_DECODE_PredictRelativeSpeedAtBreakTarget(ctx,MF_Driver_info_07_PredictRelativeSpeedAtBreakTarget, {  })
    FIELD_DECODE_StatusSignals(ctx,MF_Driver_info_08_StatusSignals, { MF_Driver_info_08_StatusSignals_bit0,MF_Driver_info_08_StatusSignals_bit1,MF_Driver_info_08_StatusSignals_bit2,MF_Driver_info_08_StatusSignals_bit3,MF_Driver_info_08_StatusSignals_bit4,MF_Driver_info_08_StatusSignals_bit5,MF_Driver_info_08_StatusSignals_bit6,MF_Driver_info_08_StatusSignals_bit7 })
    FIELD_DECODE_VisibibleSignals(ctx,MF_Driver_info_09_VisibibleSignals, { MF_Driver_info_09_VisibibleSignals_bit0,MF_Driver_info_09_VisibibleSignals_bit1,MF_Driver_info_09_VisibibleSignals_bit2,MF_Driver_info_09_VisibibleSignals_bit3,MF_Driver_info_09_VisibibleSignals_bit4,MF_Driver_info_09_VisibibleSignals_bit5,MF_Driver_info_09_VisibibleSignals_bit6,MF_Driver_info_09_VisibibleSignals_bit7 })
    
end

-------------------------------


MF_Speed_distance_00_CurrentSpeed = ProtoField.uint16( "tcc.Speed_distance.CurrentSpeed", "Current speed", base.DEC  )

MF_Speed_distance_01_CurrentOdometer = ProtoField.int16( "tcc.Speed_distance.CurrentOdometer", "Current Odometer", base.DEC  )

MF_Speed_distance_02_LeadTrackSection = ProtoField.uint16( "tcc.Speed_distance.LeadTrackSection", "Leading track section", base.DEC  )

MF_Speed_distance_03_LeadingPosition1 = ProtoField.uint16( "tcc.Speed_distance.LeadingPosition1", "Leading position1", base.DEC  )

MF_Speed_distance_04_LeadingPosition2 = ProtoField.uint16( "tcc.Speed_distance.LeadingPosition2", "Leading position2", base.DEC  )

MF_Speed_distance_05_TrailingTrackSection = ProtoField.uint16( "tcc.Speed_distance.TrailingTrackSection", "Trailing track section", base.DEC  )

MF_Speed_distance_06_TrailingPosition1 = ProtoField.uint16( "tcc.Speed_distance.TrailingPosition1", "Trailing position1", base.DEC  )

MF_Speed_distance_07_TrailingPosition2 = ProtoField.uint16( "tcc.Speed_distance.TrailingPosition2", "Trailing position2", base.DEC  )
function MSG_DECODE_Speed_distance(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_CurrentSpeed(ctx,MF_Speed_distance_00_CurrentSpeed, {  })
    FIELD_DECODE_CurrentOdometer(ctx,MF_Speed_distance_01_CurrentOdometer, {  })
    FIELD_DECODE_LeadTrackSection(ctx,MF_Speed_distance_02_LeadTrackSection, {  })
    FIELD_DECODE_LeadingPosition1(ctx,MF_Speed_distance_03_LeadingPosition1, {  })
    FIELD_DECODE_LeadingPosition2(ctx,MF_Speed_distance_04_LeadingPosition2, {  })
    FIELD_DECODE_TrailingTrackSection(ctx,MF_Speed_distance_05_TrailingTrackSection, {  })
    FIELD_DECODE_TrailingPosition1(ctx,MF_Speed_distance_06_TrailingPosition1, {  })
    FIELD_DECODE_TrailingPosition2(ctx,MF_Speed_distance_07_TrailingPosition2, {  })
    
end

-------------------------------


MF_TrainConfigData_00_TrainName = ProtoField.string( "tcc.TrainConfigData.TrainName", "Train name", base.ASCII  )

MF_TrainConfigData_01_TrainLength = ProtoField.uint16( "tcc.TrainConfigData.TrainLength", "Train length", base.DEC  )
MF_TrainConfigData_02_TimsRequired_bit0 = ProtoField.uint8( "MF_TrainConfigData_02_TimsRequired.bit0", "TIMS Required", base.HEX, nil, 1 )
MF_TrainConfigData_02_TimsRequired_bit1 = ProtoField.uint8( "MF_TrainConfigData_02_TimsRequired.bit1", "Cars connected at B == 0, Cars connected at A == 1", base.HEX, nil, 2 )

MF_TrainConfigData_02_TimsRequired = ProtoField.uint8( "tcc.TrainConfigData.TimsRequired", "TimsRequired", base.HEX  )

MF_TrainConfigData_03_DistanceFromBaliseToTrainFront = ProtoField.uint16( "tcc.TrainConfigData.DistanceFromBaliseToTrainFront", "DistanceFromBaliseToTrainFront", base.DEC  )

MF_TrainConfigData_04_DistanceFromBaliseToTrainEnd = ProtoField.uint16( "tcc.TrainConfigData.DistanceFromBaliseToTrainEnd", "DistanceFromBaliseToTrainEnd", base.DEC  )
function MSG_DECODE_TrainConfigData(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_TrainName(ctx,MF_TrainConfigData_00_TrainName, {  })
    FIELD_DECODE_TrainLength(ctx,MF_TrainConfigData_01_TrainLength, {  })
    FIELD_DECODE_TimsRequired(ctx,MF_TrainConfigData_02_TimsRequired, { MF_TrainConfigData_02_TimsRequired_bit0,MF_TrainConfigData_02_TimsRequired_bit1 })
    FIELD_DECODE_DistanceFromBaliseToTrainFront(ctx,MF_TrainConfigData_03_DistanceFromBaliseToTrainFront, {  })
    FIELD_DECODE_DistanceFromBaliseToTrainEnd(ctx,MF_TrainConfigData_04_DistanceFromBaliseToTrainEnd, {  })
    
end

-------------------------------


MF_Error_messages_00_ErrorBlock = ProtoField.uint8( "tcc.Error_messages.ErrorBlock", "ErrorBlock", base.DEC  ,{ [0] = "UndefBlock",[1] = "ACT",[2] = "BRK",[3] = "CARH",[4] = "COM",[5] = "COMATO",[6] = "COMMMI",[7] = "LOG",[8] = "ELOG",[9] = "MMICL",[10] = "MON100",[11] = "MON250",[12] = "MON500",[13] = "RCOND",[14] = "SEL",[15] = "START",[16] = "SUP",[17] = "VCOM",[18] = "COMATP2",[19] = "CONFIG" }  )

MF_Error_messages_01_ErrorNumber = ProtoField.uint8( "tcc.Error_messages.ErrorNumber", "Error number", base.DEC  )

MF_Error_messages_02_LineNumber = ProtoField.uint16( "tcc.Error_messages.LineNumber", "LineNo", base.DEC  )

MF_Error_messages_03_TimeStamp = ProtoField.uint32( "tcc.Error_messages.TimeStamp", "Time stamp", base.DEC  )

MF_Error_messages_04_ErrorGrade = ProtoField.uint8( "tcc.Error_messages.ErrorGrade", "ErrorGrade", base.DEC  ,{ [0] = "Undefined",[1] = "Fatal error A",[2] = "Fatal error B",[3] = "Fatal error C",[4] = "Minor error A",[5] = "Minor error B",[6] = "Minor error C",[7] = "Log error A",[8] = "Log error B",[9] = "Log error C" }  )

MF_Error_messages_05_TextType = ProtoField.uint8( "tcc.Error_messages.TextType", "Text Type", base.DEC  )
function MSG_DECODE_Error_messages(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_ErrorBlock(ctx,MF_Error_messages_00_ErrorBlock, {  })
    FIELD_DECODE_ErrorNumber(ctx,MF_Error_messages_01_ErrorNumber, {  })
    FIELD_DECODE_LineNumber(ctx,MF_Error_messages_02_LineNumber, {  })
    FIELD_DECODE_TimeStamp(ctx,MF_Error_messages_03_TimeStamp, {  })
    FIELD_DECODE_ErrorGrade(ctx,MF_Error_messages_04_ErrorGrade, {  })
    FIELD_DECODE_TextType(ctx,MF_Error_messages_05_TextType, {  })
    
end

-------------------------------

function MSG_DECODE_ManualConfigSelected(ctx)
    MSG_DECODE_HEADER(ctx);

    
end

-------------------------------


MF_Time_Message_00_Time = ProtoField.uint32( "tcc.Time_Message.Time", "Timevalue", base.DEC  )
function MSG_DECODE_Time_Message(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_Time(ctx,MF_Time_Message_00_Time, {  })
    
end

-------------------------------
 


MF_CarNameList_00_CarNameListCnt = ProtoField.uint16( "tcc.CarNameList.CarNameListCnt", "CarNameListCnt", base.DEC  )

function MSG_DECODE_CarNameList(ctx)
    MSG_DECODE_HEADER(ctx);

    BLOCKCNT_DECODE(ctx, FIELD_DECODE_CarNameListCnt, MF_CarNameList_00_CarNameListCnt, { 
    [1] = { id=1, func=BLOCK_DECODE_CarNameListEntry, min=-1, max=-1 } })
    
end

-------------------------------

function MSG_DECODE_TrainVsTrackDirWanted(ctx)
    MSG_DECODE_HEADER(ctx);

    
end

-------------------------------


MF_DMI_Status_00_DMIStatusWord = ProtoField.uint16( "tcc.DMI_Status.DMIStatusWord", "DMI status word", base.DEC  )
function MSG_DECODE_DMI_Status(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_DMIStatusWord(ctx,MF_DMI_Status_00_DMIStatusWord, {  })
    
end

-------------------------------

MF_CarListAnswer_CarList_00_AcceptStatus_bit0 = ProtoField.uint8( "MF_CarListAnswer_CarList_00_AcceptStatus.bit0", "Accepted without TIMS", base.HEX, nil, 1 )

MF_CarListAnswer_CarList_00_AcceptStatus = ProtoField.uint8( "tcc.CarListAnswer_CarList.AcceptStatus", "AcceptStatus", base.HEX  ) 


MF_CarListAnswer_CarList_01_CarListCnt = ProtoField.uint16( "tcc.CarListAnswer_CarList.CarListCnt", "CarListCnt", base.DEC  )

function MSG_DECODE_CarListAnswer_CarList(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_AcceptStatus(ctx,MF_CarListAnswer_CarList_00_AcceptStatus, { MF_CarListAnswer_CarList_00_AcceptStatus_bit0 })
    BLOCKCNT_DECODE(ctx, FIELD_DECODE_CarListCnt, MF_CarListAnswer_CarList_01_CarListCnt, { 
    [2] = { id=2, func=BLOCK_DECODE_CarListEntry, min=-1, max=-1 } })
    
end

-------------------------------


MF_Confirmation_00_Confirm = ProtoField.uint8( "tcc.Confirmation.Confirm", "Confirm", base.DEC  ,{ [0] = "Undefined",[1] = "OK",[2] = "Not OK" }  )
function MSG_DECODE_Confirmation(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_Confirm(ctx,MF_Confirmation_00_Confirm, {  })
    
end

-------------------------------


MF_DriverIDandPassword_00_DriverID = ProtoField.string( "tcc.DriverIDandPassword.DriverID", "DriverID", base.ASCII  )

MF_DriverIDandPassword_01_Password = ProtoField.string( "tcc.DriverIDandPassword.Password", "Password", base.ASCII  )
function MSG_DECODE_DriverIDandPassword(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_DriverID(ctx,MF_DriverIDandPassword_00_DriverID, {  })
    FIELD_DECODE_Password(ctx,MF_DriverIDandPassword_01_Password, {  })
    
end

-------------------------------

function MSG_DECODE_LocoVsTrainDir(ctx)
    MSG_DECODE_HEADER(ctx);

    
end

-------------------------------


MF_DMI_ToATP_Data_00_DMIButtonStatus = ProtoField.uint8( "tcc.DMI_ToATP_Data.DMIButtonStatus", "DMIButtonStatus", base.DEC  ,{ [0] = "Undefined",[1] = "BrakeRelease, 2 = TICConfirmation, 3 = DerailInhibit, 4 = TrainConfig,",[5] = "ManualHandling,",[6] = "HandlingDone,",[7] = "EnterYardMode,",[8] = "AlertButton,",[9] = "Test,",[10] = "ResetATP,",[11] = "RetryConfig",[12] = "Spare",[13] = "DriverLogout",[14] = "Possession",[15] = "Shunting",[16] = "Spare",[17] = "Start Brake Test",[18] = "Abort Brake Test",[19] = "Spare",[20] = "Spare",[21] = "Confirm change to Yard",[22] = "Confirm change to Shunting Route",[23] = "Confirm change to Staff Responsible" }  )
function MSG_DECODE_DMI_ToATP_Data(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_DMIButtonStatus(ctx,MF_DMI_ToATP_Data_00_DMIButtonStatus, {  })
    
end

-------------------------------

function MSG_DECODE_SubMenuButton(ctx)
    MSG_DECODE_HEADER(ctx);

    
end

-------------------------------

function MSG_DECODE_TrainVsTrackDir(ctx)
    MSG_DECODE_HEADER(ctx);

    
end

-------------------------------

function MSG_DECODE_RegistrationArea(ctx)
    MSG_DECODE_HEADER(ctx);

    
end

-------------------------------

function MSG_DECODE_ErasePlanningArea(ctx)
    MSG_DECODE_HEADER(ctx);

    
end

-------------------------------

function MSG_DECODE_TextMessage(ctx)
    MSG_DECODE_HEADER(ctx);

    
end

-------------------------------
 


MF_CarStatusList_00_CarStatusCnt = ProtoField.uint16( "tcc.CarStatusList.CarStatusCnt", "CarStatusCnt", base.DEC  )

function MSG_DECODE_CarStatusList(ctx)
    MSG_DECODE_HEADER(ctx);

    BLOCKCNT_DECODE(ctx, FIELD_DECODE_CarStatusCnt, MF_CarStatusList_00_CarStatusCnt, { 
    [7] = { id=7, func=BLOCK_DECODE_CarStatus, min=-1, max=-1 } })
    
end

-------------------------------
 


MF_CeilingSpeedList_00_SpeedBlockCnt = ProtoField.uint16( "tcc.CeilingSpeedList.SpeedBlockCnt", "SpeedBlockCnt", base.DEC  )

function MSG_DECODE_CeilingSpeedList(ctx)
    MSG_DECODE_HEADER(ctx);

    BLOCKCNT_DECODE(ctx, FIELD_DECODE_SpeedBlockCnt, MF_CeilingSpeedList_00_SpeedBlockCnt, { 
    [3] = { id=3, func=BLOCK_DECODE_SpeedBlock, min=-1, max=-1 } })
    
end

-------------------------------
 


MF_GradientDataList_00_GradientBlockCnt = ProtoField.uint16( "tcc.GradientDataList.GradientBlockCnt", "GradientBlockCnt", base.DEC  )

function MSG_DECODE_GradientDataList(ctx)
    MSG_DECODE_HEADER(ctx);

    BLOCKCNT_DECODE(ctx, FIELD_DECODE_GradientBlockCnt, MF_GradientDataList_00_GradientBlockCnt, { 
    [4] = { id=4, func=BLOCK_DECODE_GradientBlock, min=-1, max=-1 } })
    
end

-------------------------------


MF_DMIStartup_00_DMIStartupStatus = ProtoField.uint16( "tcc.DMIStartup.DMIStartupStatus", "DMIStartupStatus", base.DEC  )

MF_DMIStartup_01_CompatVersion = ProtoField.uint8( "tcc.DMIStartup.CompatVersion", "CompatVersion", base.DEC  )
function MSG_DECODE_DMIStartup(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_DMIStartupStatus(ctx,MF_DMIStartup_00_DMIStartupStatus, {  })
    FIELD_DECODE_CompatVersion(ctx,MF_DMIStartup_01_CompatVersion, {  })
    
end

-------------------------------


MF_ReConfigurationSelected_00_ReconfigTims = ProtoField.uint8( "tcc.ReConfigurationSelected.ReconfigTims", "ReconfigTims", base.DEC  )
function MSG_DECODE_ReConfigurationSelected(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_ReconfigTims(ctx,MF_ReConfigurationSelected_00_ReconfigTims, {  })
    
end

-------------------------------


MF_TrainName_00_TrainName = ProtoField.string( "tcc.TrainName.TrainName", "TrainName", base.ASCII  )
function MSG_DECODE_TrainName(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_TrainName(ctx,MF_TrainName_00_TrainName, {  })
    
end

-------------------------------


MF_PredefinedTextMessage_00_ConfirmationRequest = ProtoField.uint8( "tcc.PredefinedTextMessage.ConfirmationRequest", "ConfirmationRequest", base.DEC  )

MF_PredefinedTextMessage_01_TextMessageID = ProtoField.uint8( "tcc.PredefinedTextMessage.TextMessageID", "TextMessageID", base.DEC  )
function MSG_DECODE_PredefinedTextMessage(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_ConfirmationRequest(ctx,MF_PredefinedTextMessage_00_ConfirmationRequest, {  })
    FIELD_DECODE_TextMessageID(ctx,MF_PredefinedTextMessage_01_TextMessageID, {  })
    
end

-------------------------------


MF_AtpNotification_00_AtpNotificationMode = ProtoField.uint8( "tcc.AtpNotification.AtpNotificationMode", "AtpNotificationMode", base.DEC  )

MF_AtpNotification_01_AtpNotificationText = ProtoField.string( "tcc.AtpNotification.AtpNotificationText", "AtpNotificationText", base.ASCII  )
function MSG_DECODE_AtpNotification(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_AtpNotificationMode(ctx,MF_AtpNotification_00_AtpNotificationMode, {  })
    FIELD_DECODE_AtpNotificationText(ctx,MF_AtpNotification_01_AtpNotificationText, {  })
    
end

-------------------------------
 


MF_StartUpHistory_00_HistoryCnt = ProtoField.uint16( "tcc.StartUpHistory.HistoryCnt", "HistoryCnt", base.DEC  )

function MSG_DECODE_StartUpHistory(ctx)
    MSG_DECODE_HEADER(ctx);

    BLOCKCNT_DECODE(ctx, FIELD_DECODE_HistoryCnt, MF_StartUpHistory_00_HistoryCnt, { 
    [5] = { id=5, func=BLOCK_DECODE_HistoryBlock, min=-1, max=-1 } })
    
end

-------------------------------


MF_Version_00_CompatVersion = ProtoField.uint8( "tcc.Version.CompatVersion", "CompatVersion", base.DEC  )
function MSG_DECODE_Version(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_CompatVersion(ctx,MF_Version_00_CompatVersion, {  })
    
end

-------------------------------
 


MF_AreaRequest_00_AreaCnt = ProtoField.uint16( "tcc.AreaRequest.AreaCnt", "AreaCnt", base.DEC  )

function MSG_DECODE_AreaRequest(ctx)
    MSG_DECODE_HEADER(ctx);

    BLOCKCNT_DECODE(ctx, FIELD_DECODE_AreaCnt, MF_AreaRequest_00_AreaCnt, { 
    [6] = { id=6, func=BLOCK_DECODE_AreaBlock, min=-1, max=-1 } })
    
end

--- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

function MESSAGE_DISPATCH(ctx)
    n = MESSAGE_ID(ctx)
    if(n == 1) then
        MSG_DECODE_ATP_mode_and_states(ctx)
    elseif(n == 3) then
        MSG_DECODE_Driver_info(ctx)
    elseif(n == 5) then
        MSG_DECODE_Speed_distance(ctx)
    elseif(n == 8) then
        MSG_DECODE_TrainConfigData(ctx)
    elseif(n == 10) then
        MSG_DECODE_Error_messages(ctx)
    elseif(n == 13) then
        MSG_DECODE_ManualConfigSelected(ctx)
    elseif(n == 18) then
        MSG_DECODE_Time_Message(ctx)
    elseif(n == 19) then
        MSG_DECODE_CarNameList(ctx)
    elseif(n == 20) then
        MSG_DECODE_TrainVsTrackDirWanted(ctx)
    elseif(n == 22) then
        MSG_DECODE_DMI_Status(ctx)
    elseif(n == 23) then
        MSG_DECODE_CarListAnswer_CarList(ctx)
    elseif(n == 26) then
        MSG_DECODE_Confirmation(ctx)
    elseif(n == 27) then
        MSG_DECODE_DriverIDandPassword(ctx)
    elseif(n == 33) then
        MSG_DECODE_LocoVsTrainDir(ctx)
    elseif(n == 34) then
        MSG_DECODE_DMI_ToATP_Data(ctx)
    elseif(n == 36) then
        MSG_DECODE_SubMenuButton(ctx)
    elseif(n == 39) then
        MSG_DECODE_TrainVsTrackDir(ctx)
    elseif(n == 40) then
        MSG_DECODE_ErasePlanningArea(ctx)
    elseif(n == 42) then
        MSG_DECODE_TextMessage(ctx)
    elseif(n == 44) then
        MSG_DECODE_CarStatusList(ctx)
    elseif(n == 46) then
        MSG_DECODE_CeilingSpeedList(ctx)
    elseif(n == 47) then
        MSG_DECODE_GradientDataList(ctx)
    elseif(n == 48) then
        MSG_DECODE_DMIStartup(ctx)
    elseif(n == 49) then
        MSG_DECODE_RegistrationArea(ctx)
    elseif(n == 51) then
        MSG_DECODE_ReConfigurationSelected(ctx)
    elseif(n == 52) then
        MSG_DECODE_TrainName(ctx)
    elseif(n == 57) then
        MSG_DECODE_PredefinedTextMessage(ctx)
    elseif(n == 58) then
        MSG_DECODE_AtpNotification(ctx)
    elseif(n == 60) then
        MSG_DECODE_StartUpHistory(ctx)
    elseif(n == 61) then
        MSG_DECODE_Version(ctx)
    elseif(n == 62) then
        MSG_DECODE_AreaRequest(ctx)
    else MSG_ERROR(ctx); end
end
messageids = { 
[1] = 'ATP_mode_and_states',
[3] = 'Driver_info',
[5] = 'Speed_distance',
[8] = 'TrainConfigData',
[10] = 'Error_messages',
[13] = 'ManualConfigSelected',
[18] = 'Time_Message',
[19] = 'CarNameList',
[20] = 'TrainVsTrackDirWanted',
[22] = 'DMI_Status',
[23] = 'CarListAnswer_CarList',
[26] = 'Confirmation',
[27] = 'DriverIDandPassword',
[33] = 'LocoVsTrainDir',
[34] = 'DMI_ToATP_Data',
[36] = 'SubMenuButton',
[39] = 'TrainVsTrackDir',
[49] = 'RegistrationArea',
[40] = 'ErasePlanningArea',
[42] = 'TextMessage',
[44] = 'CarStatusList',
[46] = 'CeilingSpeedList',
[47] = 'GradientDataList',
[48] = 'DMIStartup',
[51] = 'ReConfigurationSelected',
[52] = 'TrainName',
[57] = 'PredefinedTextMessage',
[58] = 'AtpNotification',
[60] = 'StartUpHistory',
[61] = 'Version',
[62] = 'AreaRequest'
};


fld_messageid    = ProtoField.uint16("dmi.MessageID",  "MessageID",  base.DEC, messageids)
function MESSAGE_ID(ctx)

   r = ctx.tvbuf:range(ctx.off,1);
   ctx.stack:top():add(fld_messageid, r)
   ctx.off = ctx.off + 1;

   v = r:uint();
   
   -- Remove the ack-bit to get the actual message id
   if(v >= 128) then
   	v = v-128
   end

   dprint2("[+] MESSAGE_ID: ", v);
   return v;
end

--- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

--- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

local ef_too_short = ProtoExpert.new("dmi.too_short.expert", "dmi message too short",
                                     expert.group.MALFORMED, expert.severity.ERROR)

dmi = Proto("DMI", "DMI Message Protocol")

local DMI_HDR_LEN = 5

fld_startchar = ProtoField.uint8 ( "dmi.ms",      "Message start char",      base.HEX)
fld_ht        = ProtoField.uint8 ( "dmi.ht",      "Header type",             base.HEX)
fld_mn        = ProtoField.uint8 ( "dmi.mn",      "Message number",          base.HEX)
fld_dl        = ProtoField.uint16( "dmi.dl",      "Data length",             base.HEX)
fld_crc       = ProtoField.uint16( "dmi.crc",     "Crc",                     base.HEX)

dmi.fields = {
   fld_startchar,
   fld_ht,
   fld_mn,
   fld_dl,
   fld_messageid,

   BF_CarNameListEntry_00_CarNumber,BF_CarNameListEntry_01_CarName,fld_CarNameListEntry,BF_CarListEntry_00_CarID,BF_CarListEntry_01_CarTyp,fld_CarListEntry,BF_SpeedBlock_00_OdoPosition,BF_SpeedBlock_01_NewSpeed,BF_SpeedBlock_02_SpeedReason,fld_SpeedBlock,BF_GradientBlock_00_OdoPosition,BF_GradientBlock_01_Newgradient,fld_GradientBlock,BF_HistoryBlock_00_char,fld_HistoryBlock,BF_AreaBlock_00_AreaID,fld_AreaBlock,BF_CarStatus_00_CarNumer,BF_CarStatus_01_LoadWeight,BF_CarStatus_02_TicStatus,BF_CarStatus_02_TicStatus_bit0,BF_CarStatus_02_TicStatus_bit1,BF_CarStatus_02_TicStatus_bit2,BF_CarStatus_02_TicStatus_bit3,BF_CarStatus_02_TicStatus_bit8,BF_CarStatus_02_TicStatus_bit9,BF_CarStatus_02_TicStatus_bit10,BF_CarStatus_02_TicStatus_bit11,BF_CarStatus_02_TicStatus_bit12,BF_CarStatus_02_TicStatus_bit13,BF_CarStatus_02_TicStatus_bit14,BF_CarStatus_02_TicStatus_bit15,BF_CarStatus_03_PlatformStatus,fld_CarStatus,MF_ATP_mode_and_states_00_ATP_Mode,MF_ATP_mode_and_states_01_ATP_State,MF_ATP_mode_and_states_02_ATP_Mode_Extra,MF_ATP_mode_and_states_03_Driver_Verification_State,MF_ATP_mode_and_states_04_Train_State,MF_ATP_mode_and_states_04_Train_State_bit0,MF_ATP_mode_and_states_04_Train_State_bit1,MF_ATP_mode_and_states_04_Train_State_bit2,MF_ATP_mode_and_states_04_Train_State_bit3,MF_ATP_mode_and_states_04_Train_State_bit4,MF_ATP_mode_and_states_04_Train_State_bit5,MF_ATP_mode_and_states_04_Train_State_bit6,MF_ATP_mode_and_states_05_Locomotive_State,MF_ATP_mode_and_states_05_Locomotive_State_bit0,MF_ATP_mode_and_states_05_Locomotive_State_bit1,MF_ATP_mode_and_states_05_Locomotive_State_bit2,MF_ATP_mode_and_states_05_Locomotive_State_bit3,MF_ATP_mode_and_states_05_Locomotive_State_bit4,MF_ATP_mode_and_states_05_Locomotive_State_bit5,MF_ATP_mode_and_states_05_Locomotive_State_bit6,MF_ATP_mode_and_states_05_Locomotive_State_bit7,MF_ATP_mode_and_states_05_Locomotive_State_bit8,MF_ATP_mode_and_states_05_Locomotive_State_bit9,MF_ATP_mode_and_states_05_Locomotive_State_bit10,MF_ATP_mode_and_states_05_Locomotive_State_bit11,MF_ATP_mode_and_states_05_Locomotive_State_bit12,MF_ATP_mode_and_states_05_Locomotive_State_bit13,MF_ATP_mode_and_states_05_Locomotive_State_bit14,MF_ATP_mode_and_states_05_Locomotive_State_bit15,MF_ATP_mode_and_states_05_Locomotive_State_bit16,MF_ATP_mode_and_states_05_Locomotive_State_bit17,MF_ATP_mode_and_states_05_Locomotive_State_bit18,MF_ATP_mode_and_states_06_ATO_Mode,MF_ATP_mode_and_states_07_ATO_SwitchPos,MF_ATP_mode_and_states_08_AdditionalStatus1,MF_ATP_mode_and_states_08_AdditionalStatus1_bit0,MF_ATP_mode_and_states_08_AdditionalStatus1_bit1,MF_ATP_mode_and_states_08_AdditionalStatus1_bit2,MF_ATP_mode_and_states_08_AdditionalStatus1_bit3,MF_ATP_mode_and_states_08_AdditionalStatus1_bit4,MF_ATP_mode_and_states_08_AdditionalStatus1_bit5,MF_ATP_mode_and_states_09_AdditionalStatus2,MF_ATP_mode_and_states_09_AdditionalStatus2_bit0,MF_ATP_mode_and_states_10_Confirm_change,MF_ATP_mode_and_states_11_Allow,MF_ATP_mode_and_states_11_Allow_bit0,MF_ATP_mode_and_states_11_Allow_bit1,MF_ATP_mode_and_states_11_Allow_bit2,MF_ATP_mode_and_states_11_Allow_bit3,MF_ATP_mode_and_states_12_BreakStatus,MF_ATP_mode_and_states_13_RemainToNextBreak,MF_ATP_mode_and_states_14_M_END_OF_MESSAGE,MF_Driver_info_00_PermittedDirection,MF_Driver_info_01_PermittedSpeed,MF_Driver_info_02_TargetSpeed,MF_Driver_info_03_TimeToIntervene,MF_Driver_info_04_RemainDistanceToTarget,MF_Driver_info_05_RemainDistanceToBCA,MF_Driver_info_06_PredictDistanceToStillLocation,MF_Driver_info_07_PredictRelativeSpeedAtBreakTarget,MF_Driver_info_08_StatusSignals,MF_Driver_info_08_StatusSignals_bit0,MF_Driver_info_08_StatusSignals_bit1,MF_Driver_info_08_StatusSignals_bit2,MF_Driver_info_08_StatusSignals_bit3,MF_Driver_info_08_StatusSignals_bit4,MF_Driver_info_08_StatusSignals_bit5,MF_Driver_info_08_StatusSignals_bit6,MF_Driver_info_08_StatusSignals_bit7,MF_Driver_info_09_VisibibleSignals,MF_Driver_info_09_VisibibleSignals_bit0,MF_Driver_info_09_VisibibleSignals_bit1,MF_Driver_info_09_VisibibleSignals_bit2,MF_Driver_info_09_VisibibleSignals_bit3,MF_Driver_info_09_VisibibleSignals_bit4,MF_Driver_info_09_VisibibleSignals_bit5,MF_Driver_info_09_VisibibleSignals_bit6,MF_Driver_info_09_VisibibleSignals_bit7,MF_Driver_info_10_M_END_OF_MESSAGE,MF_Speed_distance_00_CurrentSpeed,MF_Speed_distance_01_CurrentOdometer,MF_Speed_distance_02_LeadTrackSection,MF_Speed_distance_03_LeadingPosition1,MF_Speed_distance_04_LeadingPosition2,MF_Speed_distance_05_TrailingTrackSection,MF_Speed_distance_06_TrailingPosition1,MF_Speed_distance_07_TrailingPosition2,MF_Speed_distance_08_M_END_OF_MESSAGE,MF_TrainConfigData_00_TrainName,MF_TrainConfigData_01_TrainLength,MF_TrainConfigData_02_TimsRequired,MF_TrainConfigData_02_TimsRequired_bit0,MF_TrainConfigData_02_TimsRequired_bit1,MF_TrainConfigData_03_DistanceFromBaliseToTrainFront,MF_TrainConfigData_04_DistanceFromBaliseToTrainEnd,MF_TrainConfigData_05_M_END_OF_MESSAGE,MF_Error_messages_00_ErrorBlock,MF_Error_messages_01_ErrorNumber,MF_Error_messages_02_LineNumber,MF_Error_messages_03_TimeStamp,MF_Error_messages_04_ErrorGrade,MF_Error_messages_05_TextType,MF_Error_messages_06_M_END_OF_MESSAGE,MF_ManualConfigSelected_00_M_END_OF_MESSAGE,MF_Time_Message_00_Time,MF_Time_Message_01_M_END_OF_MESSAGE,MF_CarNameList_00_CarNameListCnt,MF_CarNameList_01_M_END_OF_MESSAGE,MF_TrainVsTrackDirWanted_00_M_END_OF_MESSAGE,MF_DMI_Status_00_DMIStatusWord,MF_DMI_Status_01_M_END_OF_MESSAGE,MF_CarListAnswer_CarList_00_AcceptStatus,MF_CarListAnswer_CarList_00_AcceptStatus_bit0,MF_CarListAnswer_CarList_01_CarListCnt,MF_CarListAnswer_CarList_02_M_END_OF_MESSAGE,MF_Confirmation_00_Confirm,MF_Confirmation_01_M_END_OF_MESSAGE,MF_DriverIDandPassword_00_DriverID,MF_DriverIDandPassword_01_Password,MF_DriverIDandPassword_02_M_END_OF_MESSAGE,MF_LocoVsTrainDir_00_M_END_OF_MESSAGE,MF_DMI_ToATP_Data_00_DMIButtonStatus,MF_DMI_ToATP_Data_01_M_END_OF_MESSAGE,MF_SubMenuButton_00_M_END_OF_MESSAGE,MF_TrainVsTrackDir_00_M_END_OF_MESSAGE,MF_RegistrationArea_00_M_END_OF_MESSAGE,MF_ErasePlanningArea_00_M_END_OF_MESSAGE,MF_TextMessage_00_M_END_OF_MESSAGE,MF_CarStatusList_00_CarStatusCnt,MF_CarStatusList_01_M_END_OF_MESSAGE,MF_CeilingSpeedList_00_SpeedBlockCnt,MF_CeilingSpeedList_01_M_END_OF_MESSAGE,MF_GradientDataList_00_GradientBlockCnt,MF_GradientDataList_01_M_END_OF_MESSAGE,MF_DMIStartup_00_DMIStartupStatus,MF_DMIStartup_01_CompatVersion,MF_DMIStartup_02_M_END_OF_MESSAGE,MF_ReConfigurationSelected_00_ReconfigTims,MF_ReConfigurationSelected_01_M_END_OF_MESSAGE,MF_TrainName_00_TrainName,MF_TrainName_01_M_END_OF_MESSAGE,MF_PredefinedTextMessage_00_ConfirmationRequest,MF_PredefinedTextMessage_01_TextMessageID,MF_PredefinedTextMessage_02_M_END_OF_MESSAGE,MF_AtpNotification_00_AtpNotificationMode,MF_AtpNotification_01_AtpNotificationText,MF_AtpNotification_02_M_END_OF_MESSAGE,MF_StartUpHistory_00_HistoryCnt,MF_StartUpHistory_01_M_END_OF_MESSAGE,MF_Version_00_CompatVersion,MF_Version_01_M_END_OF_MESSAGE,MF_AreaRequest_00_AreaCnt,MF_AreaRequest_01_M_END_OF_MESSAGE,

   fld_crc

};

function dmi.dissector(tvbuf, pktinfo, root)
   dprint2("dmi.dissector called")
   pktinfo.cols.protocol:set("DMI")

   local crclen = 4

   local pktlen = tvbuf:reported_length_remaining()
   local tree = root:add(dmi, tvbuf:range(0,pktlen))

   -- now let's check it's not too short
   if pktlen < DMI_HDR_LEN then
      tree:add_proto_expert_info(ef_too_short)
      dprint("packet length",pktlen,"too short")
      return
   end

   tree:add(fld_startchar, tvbuf:range(0,1))
   tree:add(fld_ht,    tvbuf:range(1,1))
   tree:add(fld_mn,    tvbuf:range(2,1))
   tree:add(fld_dl,    tvbuf:range(3,2))

   ctx = { off = 5,
	   tvbuf = tvbuf,
	   pktinfo = pktinfo,
	   stack = utilwireshark.stack:Create(),
	   limit = pktlen - crclen --- crc
   };
   ctx.stack:push(tree);
   MESSAGE_DISPATCH(ctx);
   ctx.stack:pop(1);
   
   
end

DissectorTable.get("tcp.port"):add(default_settings.port, dmi)
