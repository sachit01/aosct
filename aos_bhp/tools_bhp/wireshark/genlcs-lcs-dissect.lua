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
    port         = 30150, -- LSC port
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

--- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
--- LCS protocol decode

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
   for _,b0 in ipairs(blocks) do
      b = b0
   end
   local j = 0;
   for i in 1,v do
      if (b0 ~= nil) then
	 b0.func(ctx);
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


ef_too_short = ProtoExpert.new("lcs.too_short.expert", "LCS message too short", expert.group.MALFORMED, expert.severity.ERROR)
ef_todo = ProtoExpert.new("lcs.todo", "todo: not finished", expert.group.MALFORMED, expert.severity.WARN)

lcs = Proto("lcs", "LCS Message Protocol")

local LCS_HDR_LEN = 10

fld_header        = ProtoField.uint8 ( "lcs.version",        "Protocol Version",      base.HEX)
fld_messagetype   = ProtoField.uint16( "lcs.messagetype",    "Message ID",            base.DEC,
				       {  [50001] = "AOS Status Message",
					  [50002] = "ATP Command Message",
					  [50003] = "ATO Driving Message",
					  [50004] = "Movement Authority",
					  [50005] = "ATP Warning Curve Message",
					  [50009] = "Train Composition Message",
					  [50011] = "Path Message",
					  [50012] = "ATO Command Message",
					  [50101] = "Train Status Message",
					  [50102] = "ECPB Train Composition Message" })
fld_messageversion= ProtoField.uint8 ( "lcs.messageversion", "Message Version",       base.DEC)
fld_flags         = ProtoField.uint8 ( "lcs.flags",          "Falgs",                 base.HEX)
fld_datalength    = ProtoField.bytes ( "lcs.len",            "Data Len",              base.NONE)

fld_sendmsgnum    = ProtoField.uint32( "lcs.sendmsgnum",     "Message Number",        base.DEC)
fld_timestamp     = ProtoField.uint32( "lcs.timestamp",      "Message Timestamp",     base.DEC)
fld_variablesize  = ProtoField.uint8 ( "lcs.variablesize",   "Variable Size",         base.DEC)
fld_varfields     = ProtoField.bytes ( "lcs.varfields",   "Variable Fields",         base.NONE)



--- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
--- Decode of fields

-------------------------------
FIELD_SPEC_CabinMode = json.decode('{"Min": 0, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "CabinMode", "Detail": "ATO Mode Cabin Selector", "Default": 0, "Length": 1, "parts": [], "type": "CabinMode", "Display": "", "Special": [{"text": "Manual", "type": "Field", "value": 0}, {"text": "Supervised6", "type": "Field", "value": 1}, {"text": "Automatic6", "type": "Field", "value": 2}, {"text": "No ATO Mode Asserted", "type": "Field", "value": 255}]}');
function FIELD_DECODE_CabinMode(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_CabinMode, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_TrainIdle = json.decode('{"Min": 0, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "TrainIdle", "Detail": "Train Idling1", "Default": 0, "Length": 1, "parts": [], "type": "TrainIdle", "Display": "", "Special": [{"text": "MA exists", "type": "Field", "value": 0}, {"text": "Train is Idling", "type": "Field", "value": 1}, {"text": "Not Asserted5", "type": "Field", "value": 255}]}');
function FIELD_DECODE_TrainIdle(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_TrainIdle, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_TrainIdle = json.decode('{"Min": 0, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "TrainIdle", "Detail": "Train Idling1", "Default": 0, "Length": 1, "parts": [], "type": "TrainIdle", "Display": "", "Special": [{"text": "MA exists", "type": "Field", "value": 0}, {"text": "Train is Idling", "type": "Field", "value": 1}, {"text": "Not Asserted5", "type": "Field", "value": 255}]}');
function FIELD_DECODE_TrainIdle(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_TrainIdle, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_FrontOrientation = json.decode('{"Min": 0, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "FrontOrientation", "Detail": "Train orientation on front track", "Default": 0, "Length": 1, "parts": [], "type": "FrontOrientation", "Display": "", "Special": [{"text": "Lead locomotive towards Leg 0", "type": "Field", "value": 0}, {"text": "Towards Leg 1", "type": "Field", "value": 1}]}');
function FIELD_DECODE_FrontOrientation(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_FrontOrientation, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_RearOrientation = json.decode('{"Min": 0, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "RearOrientation", "Detail": "Train orientation on rear track", "Default": 0, "Length": 1, "parts": [], "type": "RearOrientation", "Display": "", "Special": [{"text": "Lead locomotive towards Leg 0", "type": "Field", "value": 0}, {"text": "Towards Leg 1", "type": "Field", "value": 1}]}');
function FIELD_DECODE_RearOrientation(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_RearOrientation, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_TravelDirection = json.decode('{"Min": 0, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "TravelDirection", "Detail": "Current travel direction", "Default": 0, "Length": 1, "parts": [], "type": "TravelDirection", "Display": "", "Special": [{"text": "Locomotive leading (Forward)", "type": "Field", "value": 0}, {"text": "Locomotive trailing (Reverse) For single locomotives forward is the A-end leading (short hood)", "type": "Field", "value": 1}]}');
function FIELD_DECODE_TravelDirection(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_TravelDirection, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_SystemBlueFlag = json.decode('{"Min": 0, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "SystemBlueFlag", "Detail": "System Blue Flag", "Default": 0, "Length": 1, "parts": [], "type": "SystemBlueFlag", "Display": "", "Special": [{"text": "Not Active", "type": "Field", "value": 0}, {"text": "Active", "type": "Field", "value": 1}]}');
function FIELD_DECODE_SystemBlueFlag(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_SystemBlueFlag, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_LimitedSupervision = json.decode('{"Min": 0, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "LimitedSupervision", "Detail": "Limited supervised mode4", "Default": 0, "Length": 1, "parts": [], "type": "LimitedSupervision", "Display": "", "Special": [{"text": "Not Active", "type": "Field", "value": 0}, {"text": "Active", "type": "Field", "value": 1}, {"text": "Not Asserted", "type": "Field", "value": 255}]}');
function FIELD_DECODE_LimitedSupervision(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_LimitedSupervision, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_ATO_Mode_Cabin_Sel_Status = json.decode('{"Min": 0, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "ATO_Mode_Cabin_Sel_Status", "Detail": "ATO Mode Cabin Selector Status", "Default": 0, "Length": 1, "parts": [], "type": "ATO_Mode_Cabin_Sel_Status", "Display": "", "Special": [{"text": "Manual", "type": "Field", "value": 0}, {"text": "Supervised", "type": "Field", "value": 1}, {"text": "Automatic", "type": "Field", "value": 2}, {"text": "No ATO Mode Asserted", "type": "Field", "value": 255}]}');
function FIELD_DECODE_ATO_Mode_Cabin_Sel_Status(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_ATO_Mode_Cabin_Sel_Status, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_ATO_Driver_Mode = json.decode('{"Min": 0, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "ATO_Driver_Mode", "Detail": "ATO Mode Cabin Selector Status", "Default": 0, "Length": 1, "parts": [], "type": "ATO_Driver_Mode", "Display": "", "Special": [{"text": "None", "type": "Field", "value": 0}, {"text": "ETA Pacing", "type": "Field", "value": 1}, {"text": "Ceiling Speed", "type": "Field", "value": 2}, {"text": "Loading", "type": "Field", "value": 3}, {"text": "Precision Stop", "type": "Field", "value": 4}, {"text": "Unloading (Free Rolling)", "type": "Field", "value": 5}, {"text": "Not Asserted", "type": "Field", "value": 255}]}');
function FIELD_DECODE_ATO_Driver_Mode(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_ATO_Driver_Mode, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_M_END_OF_MESSAGE = json.decode('{"Min": 0, "Resolution": null, "Max": 0, "Format": "UINT", "fldname": "M_END_OF_MESSAGE", "Detail": "End of message for variable length messages", "Default": 0, "Length": 1, "parts": [], "type": "M_END_OF_MESSAGE", "Display": "", "Special": []}');
function FIELD_DECODE_M_END_OF_MESSAGE(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_M_END_OF_MESSAGE, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_FrontTrack = json.decode('{"Format": "UINT", "fldname": "FrontTrack", "Length": 2, "parts": [], "type": "FrontTrack", "Display": "", "Special": []}');
function FIELD_DECODE_FrontTrack(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_FrontTrack, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_FrontPos = json.decode('{"Format": "UINT", "fldname": "FrontPos", "Length": 4, "parts": [], "type": "FrontPos", "Display": "", "Special": []}');
function FIELD_DECODE_FrontPos(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_FrontPos, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_RearTrack = json.decode('{"Format": "UINT", "fldname": "RearTrack", "Length": 2, "parts": [], "type": "RearTrack", "Display": "", "Special": []}');
function FIELD_DECODE_RearTrack(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_RearTrack, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_RearPos = json.decode('{"Format": "UINT", "fldname": "RearPos", "Length": 4, "parts": [], "type": "RearPos", "Display": "", "Special": []}');
function FIELD_DECODE_RearPos(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_RearPos, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_AOSSpeed = json.decode('{"Format": "UINT", "fldname": "AOSSpeed", "Length": 2, "parts": [], "type": "AOSSpeed", "Display": "", "Special": []}');
function FIELD_DECODE_AOSSpeed(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_AOSSpeed, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Time = json.decode('{"Format": "UINT", "fldname": "Time", "Length": 4, "parts": [], "type": "Time", "Display": "", "Special": []}');
function FIELD_DECODE_Time(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Time, fld, subflds)
end
        
--- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

--- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
--- Decode of blocks

--- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

--- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
--- Decode of messages

-------------------------------


MF_AOS_status_message_00_CabinMode = ProtoField.uint8( "tcc.AOS_status_message.CabinMode", "CabinMode", base.DEC  ,{ [0] = "Manual",[1] = "Supervised6",[2] = "Automatic6",[255] = "No ATO Mode Asserted" }  )

MF_AOS_status_message_01_TrainIdle = ProtoField.uint8( "tcc.AOS_status_message.TrainIdle", "TrainIdle", base.DEC  ,{ [0] = "MA exists",[1] = "Train is Idling",[255] = "Not Asserted5" }  )

MF_AOS_status_message_02_FrontTrack = ProtoField.uint16( "tcc.AOS_status_message.FrontTrack", "Track Id Front of train", base.DEC  )

MF_AOS_status_message_03_FrontPos = ProtoField.uint32( "tcc.AOS_status_message.FrontPos", "Position on Track of Front of train", base.DEC  )

MF_AOS_status_message_04_FrontOrientation = ProtoField.uint8( "tcc.AOS_status_message.FrontOrientation", "FrontOrientation", base.DEC  ,{ [0] = "Lead locomotive towards Leg 0",[1] = "Towards Leg 1" }  )

MF_AOS_status_message_05_RearTrack = ProtoField.uint16( "tcc.AOS_status_message.RearTrack", "Track Id Front of train", base.DEC  )

MF_AOS_status_message_06_RearPos = ProtoField.uint32( "tcc.AOS_status_message.RearPos", "Position on Track of Front of train", base.DEC  )

MF_AOS_status_message_07_RearOrientation = ProtoField.uint8( "tcc.AOS_status_message.RearOrientation", "RearOrientation", base.DEC  ,{ [0] = "Lead locomotive towards Leg 0",[1] = "Towards Leg 1" }  )

MF_AOS_status_message_08_TravelDirection = ProtoField.uint8( "tcc.AOS_status_message.TravelDirection", "TravelDirection", base.DEC  ,{ [0] = "Locomotive leading (Forward)",[1] = "Locomotive trailing (Reverse) For single locomotives forward is the A-end leading (short hood)" }  )

MF_AOS_status_message_09_AOSSpeed = ProtoField.uint16( "tcc.AOS_status_message.AOSSpeed", "AOSSpeed", base.DEC  )

MF_AOS_status_message_10_SystemBlueFlag = ProtoField.uint8( "tcc.AOS_status_message.SystemBlueFlag", "SystemBlueFlag", base.DEC  ,{ [0] = "Not Active",[1] = "Active" }  )

MF_AOS_status_message_11_Time = ProtoField.uint32( "tcc.AOS_status_message.Time", "Time", base.DEC  )

MF_AOS_status_message_12_LimitedSupervision = ProtoField.uint8( "tcc.AOS_status_message.LimitedSupervision", "LimitedSupervision", base.DEC  ,{ [0] = "Not Active",[1] = "Active",[255] = "Not Asserted" }  )
function MSG_DECODE_AOS_status_message(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_CabinMode(ctx,MF_AOS_status_message_00_CabinMode, {  })
    FIELD_DECODE_TrainIdle(ctx,MF_AOS_status_message_01_TrainIdle, {  })
    FIELD_DECODE_FrontTrack(ctx,MF_AOS_status_message_02_FrontTrack, {  })
    FIELD_DECODE_FrontPos(ctx,MF_AOS_status_message_03_FrontPos, {  })
    FIELD_DECODE_FrontOrientation(ctx,MF_AOS_status_message_04_FrontOrientation, {  })
    FIELD_DECODE_RearTrack(ctx,MF_AOS_status_message_05_RearTrack, {  })
    FIELD_DECODE_RearPos(ctx,MF_AOS_status_message_06_RearPos, {  })
    FIELD_DECODE_RearOrientation(ctx,MF_AOS_status_message_07_RearOrientation, {  })
    FIELD_DECODE_TravelDirection(ctx,MF_AOS_status_message_08_TravelDirection, {  })
    FIELD_DECODE_AOSSpeed(ctx,MF_AOS_status_message_09_AOSSpeed, {  })
    FIELD_DECODE_SystemBlueFlag(ctx,MF_AOS_status_message_10_SystemBlueFlag, {  })
    FIELD_DECODE_Time(ctx,MF_AOS_status_message_11_Time, {  })
    FIELD_DECODE_LimitedSupervision(ctx,MF_AOS_status_message_12_LimitedSupervision, {  })
    
end

-------------------------------

function MSG_DECODE_ATP_Command_Message(ctx)
    MSG_DECODE_HEADER(ctx);

    
end

-------------------------------

function MSG_DECODE_ATO_Driving_Message(ctx)
    MSG_DECODE_HEADER(ctx);

    
end

-------------------------------

function MSG_DECODE_Movement_Authority(ctx)
    MSG_DECODE_HEADER(ctx);

    
end

-------------------------------

function MSG_DECODE_ATP_Warning_Curve_Message(ctx)
    MSG_DECODE_HEADER(ctx);

    
end

-------------------------------

function MSG_DECODE_Train_Composition_Message(ctx)
    MSG_DECODE_HEADER(ctx);

    
end

-------------------------------

function MSG_DECODE_Path_Message(ctx)
    MSG_DECODE_HEADER(ctx);

    
end

-------------------------------

function MSG_DECODE_ATO_Command_Message(ctx)
    MSG_DECODE_HEADER(ctx);

    
end

-------------------------------


MF_Train_Status_Message_00_ATO_Mode_Cabin_Sel_Status = ProtoField.uint8( "tcc.Train_Status_Message.ATO_Mode_Cabin_Sel_Status", "ATO_Mode_Cabin_Sel_Status", base.DEC  ,{ [0] = "Manual",[1] = "Supervised",[2] = "Automatic",[255] = "No ATO Mode Asserted" }  )

MF_Train_Status_Message_01_ATO_Driver_Mode = ProtoField.uint8( "tcc.Train_Status_Message.ATO_Driver_Mode", "ATO_Driver_Mode", base.DEC  ,{ [0] = "None",[1] = "ETA Pacing",[2] = "Ceiling Speed",[3] = "Loading",[4] = "Precision Stop",[5] = "Unloading (Free Rolling)",[255] = "Not Asserted" }  )
function MSG_DECODE_Train_Status_Message(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_ATO_Mode_Cabin_Sel_Status(ctx,MF_Train_Status_Message_00_ATO_Mode_Cabin_Sel_Status, {  })
    FIELD_DECODE_ATO_Driver_Mode(ctx,MF_Train_Status_Message_01_ATO_Driver_Mode, {  })
    
end

-------------------------------

function MSG_DECODE_ECPB_Train_Composition_Message(ctx)
    MSG_DECODE_HEADER(ctx);

    
end

--- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

function MESSAGE_DISPATCH(ctx)
    n = MESSAGE_ID(ctx)
    if(n == 50001) then
        MSG_DECODE_AOS_status_message(ctx)
    elseif(n == 50002) then
        MSG_DECODE_ATP_Command_Message(ctx)
    elseif(n == 50003) then
        MSG_DECODE_ATO_Driving_Message(ctx)
    elseif(n == 50004) then
        MSG_DECODE_Movement_Authority(ctx)
    elseif(n == 50005) then
        MSG_DECODE_ATP_Warning_Curve_Message(ctx)
    elseif(n == 50009) then
        MSG_DECODE_Train_Composition_Message(ctx)
    elseif(n == 50011) then
        MSG_DECODE_Path_Message(ctx)
    elseif(n == 50012) then
        MSG_DECODE_ATO_Command_Message(ctx)
    elseif(n == 50101) then
        MSG_DECODE_Train_Status_Message(ctx)
    elseif(n == 50102) then
        MSG_DECODE_ECPB_Train_Composition_Message(ctx)
    else MSG_ERROR(ctx); end
end
messageids = { 
[50001] = 'AOS_status_message',
[50002] = 'ATP_Command_Message',
[50003] = 'ATO_Driving_Message',
[50004] = 'Movement_Authority',
[50005] = 'ATP_Warning_Curve_Message',
[50009] = 'Train_Composition_Message',
[50011] = 'Path_Message',
[50012] = 'ATO_Command_Message',
[50101] = 'Train_Status_Message',
[50102] = 'ECPB_Train_Composition_Message'
};



lcs.fields = {
   fld_header,
   fld_messagetype,
   fld_messageversion,
   fld_flags,
   fld_datalength,
   fld_sendmsgnum,


   MF_AOS_status_message_00_CabinMode,MF_AOS_status_message_01_TrainIdle,MF_AOS_status_message_02_FrontTrack,MF_AOS_status_message_03_FrontPos,MF_AOS_status_message_04_FrontOrientation,MF_AOS_status_message_05_RearTrack,MF_AOS_status_message_06_RearPos,MF_AOS_status_message_07_RearOrientation,MF_AOS_status_message_08_TravelDirection,MF_AOS_status_message_09_AOSSpeed,MF_AOS_status_message_10_SystemBlueFlag,MF_AOS_status_message_11_Time,MF_AOS_status_message_12_LimitedSupervision,MF_AOS_status_message_13_M_END_OF_MESSAGE,MF_ATP_Command_Message_00_M_END_OF_MESSAGE,MF_ATO_Driving_Message_00_M_END_OF_MESSAGE,MF_Movement_Authority_00_M_END_OF_MESSAGE,MF_ATP_Warning_Curve_Message_00_M_END_OF_MESSAGE,MF_Train_Composition_Message_00_M_END_OF_MESSAGE,MF_Path_Message_00_M_END_OF_MESSAGE,MF_ATO_Command_Message_00_M_END_OF_MESSAGE,MF_Train_Status_Message_00_ATO_Mode_Cabin_Sel_Status,MF_Train_Status_Message_01_ATO_Driver_Mode,MF_Train_Status_Message_02_M_END_OF_MESSAGE,MF_ECPB_Train_Composition_Message_00_M_END_OF_MESSAGE,
   

   fld_timestamp,
   fld_variablesize,
   fld_varfields
};

lcs.experts = {
   ef_too_short,
   ef_todo
}

function MESSAGE_ID(ctx)
   v = ctx.messageidv;
   dprint2("[+] MESSAGE_ID: ", v);
   return v;
end

function lcs.dissector(tvbuf, pktinfo, root)
   dprint2("----\nlcs.dissector called")
   pktinfo.cols.protocol:set("LCS")

   local pktlen = tvbuf:reported_length_remaining()
   local tree = root:add(lcs, tvbuf:range(0,pktlen))

   -- now let's check it's not too short
   if pktlen < LCS_HDR_LEN then
      tree:add_proto_expert_info(ef_too_short)
      dprint("packet length",pktlen,"too short")
      return
   end

   tree:add(fld_header,        tvbuf:range(0,1))
   local messageid = tvbuf:range(1,2);
   tree:add(fld_messagetype,   messageid)
   tree:add(fld_messageversion,tvbuf:range(3,1))
   tree:add(fld_flags,         tvbuf:range(4,1))
   tree:add(fld_datalength,    tvbuf:range(5,3))
   tree:add(fld_sendmsgnum,    tvbuf:range(8,4))
   tree:add(fld_timestamp,     tvbuf:range(12,4))
   local h=tvbuf:range(16,1)
   tree:add(fld_variablesize,  h)
   local varsize = h:uint()

   if varsize ~= 0 then 
     tree:add(fld_varfields, tvbuf:range(17,varsize))
   end

   local messageidv = messageid:uint()

   ctx = { off = 17 + varsize,
	   messageidv = messageidv,
	   tvbuf = tvbuf,
	   pktinfo = pktinfo,
	   stack = utilwireshark.stack:Create(),
	   limit = pktlen --- crc
   };
   ctx.stack:push(tree);
   MESSAGE_DISPATCH(ctx);
   ctx.stack:pop(1);

end

--- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

--- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
--- ClassD protocol decode
--- multilevel protocols see:
--- http://torsten-traenkner.de/linux/development/wireshark.php

classd = Proto("ClassD", "Class-D Protocol")

classd_stx           = ProtoField.uint8  ( "classd.stx",          "Start byte",       base.HEX)
classd_header        = ProtoField.uint8  ( "classd.protoversion", "Protocol Version", base.HEX)
classd_msgnr         = ProtoField.uint32 ( "classd.msgnr",        "Message Number",   base.HEX)
classd_msgtype       = ProtoField.uint8  ( "classd.msgtype",      "Message Type",     base.HEX)
classd_msgversion    = ProtoField.uint8  ( "classd.msgversion",   "Message Version",  base.HEX)
classd_datalen       = ProtoField.uint32 ( "classd.datalen",      "Data Length",      base.HEX)

CLASSD_HDR_LEN = 12

classd.fields = {
   classd_stx,        
   classd_header,     
   classd_msgnr,      
   classd_msgtype,    
   classd_msgversion, 
   classd_datalen         
};

function classd.dissector(tvbuf, pktinfo, root)
   dprint2("classd.dissector called")
   pktinfo.cols.protocol:set("ClassD")

   local pktlen = tvbuf:reported_length_remaining()
   local tree = root:add(classd, tvbuf:range(0,pktlen))

   -- now let's check it's not too short
   if pktlen < CLASSD_HDR_LEN then
      tree:add_proto_expert_info(ef_too_short)
      dprint("packet length",pktlen,"too short")
      return
   end
   
   tree:add(classd_stx,       tvbuf:range(0,1))
   tree:add(classd_header,    tvbuf:range(1,1))
   tree:add(classd_msgnr,     tvbuf:range(2,4))
   tree:add(classd_msgtype,   tvbuf:range(6,1))
   tree:add(classd_msgversion,tvbuf:range(7,1))
   local dl = tvbuf:range(8,4);
   tree:add(classd_datalen,   dl)
   local dlv = dl:int();
   
   if pktlen < (CLASSD_HDR_LEN+dlv) then
      tree:add_proto_expert_info(ef_too_short)
      dprint("packet length for lcs",pktlen,"too short, lcs datalen is ", dlv)
      return
   end

   --- todo: probably loog at classd_msgtype and choose protocol to use. For now use lcs
   Dissector.get("lcs"):call(tvbuf(12, dlv):tvb(), pktinfo, tree)

end


--- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

DissectorTable.get("tcp.port"):add(default_settings.port, classd)

