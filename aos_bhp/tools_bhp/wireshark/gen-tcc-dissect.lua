
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
    port         = 30132, -- tcc port
    heur_enabled = false,
}

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
end

function MSG_DECODE_HEADER(ctx)
end

function MSG_ENCODE_HEADER(ctx)
end

function MSG_ENCODE_HEADER_CALC(ctx)
end

function MSG_BLOCKS_DECODE(ctx, blocks)
   local pktlen = ctx.tvbuf:reported_length_remaining()
   while(ctx.off + 1 <= pktlen) do
      local r = ctx.tvbuf:range(ctx.off,1);
      local bid = r:uint();
      ctx.off = ctx.off + 1;
      if (bid == 0) then
	 if (ctx.off < ctx.limit) then
	    dprint2(" > !!! null when till remaining bytes exist: cur:" .. ctx.off .. " limit:" .. ctx.limit);
	 end;
	 break;
      elseif (blocks[bid] ~= nil) then
	 dprint2(" > BLOCK_DECODE: ", bid);
	 blocks[bid].func(ctx);
      else
	 dprint2(" > Cannot find BLOCK_DECODE: ", bid);
      end
   end
end


function MSG_ERROR(ctx)
end


--- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
--- Decode of fields

-------------------------------
FIELD_SPEC_A_BRAKEABILITY = json.decode('{"Min": 1, "Resolution": "0.01cm/s", "Max": 65535, "Format": "UINT", "fldname": "A_BRAKEABILITY", "Detail": "Deceleration capability", "Default": null, "Length": 2, "parts": [], "type": "A_BRAKEABILITY", "Display": "", "Special": [{"text": 0, "type": "Illegal"}]}');
function FIELD_DECODE_A_BRAKEABILITY(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_A_BRAKEABILITY, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_B_DIRECTION = json.decode('{"Min": 0, "Resolution": null, "Max": 7, "Format": "BITMASK", "fldname": "B_DIRECTION", "Detail": "Orientation and travel direction of the train as a bitmask.", "Default": null, "Length": 1, "parts": [], "type": "B_DIRECTION", "Display": "", "Special": [{"text": "Driving direction (0 - forward, 1 - Reverse)", "type": "Bit", "value": 0}, {"text": "Orientation in track (0 - as track, 1 - opposite)", "type": "Bit", "value": 1}, {"text": "Locomotive orientation (0- B-end facing cars, 1 - A-end facing cars)", "type": "Bit", "value": 2}]}');
function FIELD_DECODE_B_DIRECTION(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_B_DIRECTION, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_B_TRAIN_CORE_STATUS = json.decode('{"Min": 0, "Resolution": null, "Max": 4294967295, "Format": "BITMASK", "fldname": "B_TRAIN_CORE_STATUS", "Detail": "Status of the train as a bitmask.", "Default": null, "Length": 4, "parts": [], "type": "B_TRAIN_CORE_STATUS", "Display": "", "Special": [{"text": "Safety Halt, AOS", "type": "Bit", "value": 0}, {"text": "EA from driver", "type": "Bit", "value": 1}, {"text": "TIMS Integrity Broken", "type": "Bit", "value": 2}, {"text": "Braking event, AOS", "type": "Bit", "value": 3}, {"text": "Handling done", "type": "Bit", "value": 4}, {"text": "Train Idling", "type": "Bit", "value": 5}, {"text": "TIMS Integrity manual override from Driver", "type": "Bit", "value": 6}, {"text": "MA time out", "type": "Bit", "value": 7}, {"text": "ATP reset", "type": "Bit", "value": 8}, {"text": "ATP needs to be reset", "type": "Bit", "value": 9}, {"text": "ATP intervention", "type": "Bit", "value": 10}, {"text": "Brake release wanted", "type": "Bit", "value": 11}, {"text": "Manual TIMS confirmation", "type": "Bit", "value": 12}, {"text": "Slip detected", "type": "Bit", "value": 13}, {"text": "Free rolling", "type": "Bit", "value": 14}, {"text": "Emergency Alert active", "type": "Bit", "value": 15}, {"text": "Attention needed", "type": "Bit", "value": 16}, {"text": "Not ready to drive", "type": "Bit", "value": 17}, {"text": "Safe for boarding is active", "type": "Bit", "value": 18}, {"text": "Not Used (19)", "type": "Bit", "value": 19}, {"text": "Not Used (20)", "type": "Bit", "value": 20}, {"text": "Not Used (21)", "type": "Bit", "value": 21}, {"text": "Not Used (22)", "type": "Bit", "value": 22}, {"text": "Not Used (23)", "type": "Bit", "value": 23}, {"text": "Not Used (24)", "type": "Bit", "value": 24}, {"text": "Not Used (25)", "type": "Bit", "value": 25}, {"text": "Not Used (26)", "type": "Bit", "value": 26}, {"text": "Not Used (27)", "type": "Bit", "value": 27}, {"text": "Not Used (28)", "type": "Bit", "value": 28}, {"text": "Not Used (29)", "type": "Bit", "value": 29}, {"text": "Not Used (30)", "type": "Bit", "value": 30}, {"text": "Not Used (31)", "type": "Bit", "value": 31}]}');
function FIELD_DECODE_B_TRAIN_CORE_STATUS(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_B_TRAIN_CORE_STATUS, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_B_TIC_STATUS = json.decode('{"Min": 0, "Resolution": null, "Max": 65535, "Format": "BITMASK", "fldname": "B_TIC_STATUS", "Detail": "TIC unit status", "Default": null, "Length": 2, "parts": [], "type": "B_TIC_STATUS", "Display": "", "Special": [{"text": "Derail, front right", "type": "Bit", "value": 0}, {"text": "Derail, front left", "type": "Bit", "value": 1}, {"text": "Derail, rear right", "type": "Bit", "value": 2}, {"text": "Derail, rear left", "type": "Bit", "value": 3}, {"text": "Car dump bottom", "type": "Bit", "value": 4}, {"text": "Car dump top", "type": "Bit", "value": 5}, {"text": "Car dump closed", "type": "Bit", "value": 6}, {"text": "Load weight bad", "type": "Bit", "value": 7}, {"text": "Train config input", "type": "Bit", "value": 8}]}');
function FIELD_DECODE_B_TIC_STATUS(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_B_TIC_STATUS, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_D_MA_MARGIN = json.decode('{"Min": 0, "Resolution": "cm", "Max": 65535, "Format": "UINT", "fldname": "D_MA_MARGIN", "Detail": "Allowed margin for the vehicle to stop before the end of the MA.", "Default": null, "Length": 2, "parts": [], "type": "D_MA_MARGIN", "Display": "", "Special": [{"text": 0, "type": "Illegal"}]}');
function FIELD_DECODE_D_MA_MARGIN(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_D_MA_MARGIN, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_D_MAX_DIST = json.decode('{"Min": -8388608, "Resolution": "cm", "Max": 8388607, "Format": "INT", "fldname": "D_MAX_DIST", "Detail": "Max distance for balise search", "Default": null, "Length": 3, "parts": [], "type": "D_MAX_DIST", "Display": "", "Special": []}');
function FIELD_DECODE_D_MAX_DIST(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_D_MAX_DIST, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_D_SAFETY_MARGIN = json.decode('{"Min": 0, "Resolution": "cm", "Max": 65535, "Format": "UINT", "fldname": "D_SAFETY_MARGIN", "Detail": "Margin between the end of MA and the point of conflict.", "Default": null, "Length": 2, "parts": [], "type": "D_SAFETY_MARGIN", "Display": "", "Special": []}');
function FIELD_DECODE_D_SAFETY_MARGIN(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_D_SAFETY_MARGIN, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_D_POSITION = json.decode('{"Min": -2147483647, "Resolution": "cm", "Max": 2147483648, "Format": "INT", "fldname": "D_POSITION", "Detail": "Position within NID_TRACK_ECTION track section.", "Default": null, "Length": 4, "parts": [], "type": "D_POSITION", "Display": "", "Special": []}');
function FIELD_DECODE_D_POSITION(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_D_POSITION, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_D_REVERSE = json.decode('{"Min": 0, "Resolution": null, "Max": 65535, "Format": "UINT", "fldname": "D_REVERSE", "Detail": "Maximum distance which the train is allowed to move from the point here a Profile stop indication is recieved.", "Default": null, "Length": 2, "parts": [], "type": "D_REVERSE", "Display": "", "Special": []}');
function FIELD_DECODE_D_REVERSE(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_D_REVERSE, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_D_WINDOW = json.decode('{"Min": 0, "Resolution": "cm", "Max": 65535, "Format": "UINT", "fldname": "D_WINDOW", "Detail": "Currently calculated tolerance in position determination.", "Default": null, "Length": 2, "parts": [], "type": "D_WINDOW", "Display": "", "Special": []}');
function FIELD_DECODE_D_WINDOW(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_D_WINDOW, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_G_GRADIENT = json.decode('{"Min": -128, "Resolution": "1 per mill", "Max": 127, "Format": "INT", "fldname": "G_GRADIENT", "Detail": "New gradient", "Default": null, "Length": 1, "parts": [], "type": "G_GRADIENT", "Display": "", "Special": []}');
function FIELD_DECODE_G_GRADIENT(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_G_GRADIENT, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_L_LOCOMOTIVE = json.decode('{"Min": 0, "Resolution": "cm", "Max": 65535, "Format": "UINT", "fldname": "L_LOCOMOTIVE", "Detail": "Length of the locomotive", "Default": null, "Length": 2, "parts": [], "type": "L_LOCOMOTIVE", "Display": "", "Special": []}');
function FIELD_DECODE_L_LOCOMOTIVE(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_L_LOCOMOTIVE, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_L_TRAIN = json.decode('{"Min": 0, "Resolution": "cm", "Max": 1677721, "Format": "UINT", "fldname": "L_TRAIN", "Detail": "Total length of the train (locomotive and all cars)", "Default": null, "Length": 3, "parts": [], "type": "L_TRAIN", "Display": "", "Special": [{"text": 0, "type": "Illegal"}]}');
function FIELD_DECODE_L_TRAIN(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_L_TRAIN, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_M_END_OF_MESSAGE = json.decode('{"Min": 0, "Resolution": null, "Max": 0, "Format": "UINT", "fldname": "M_END_OF_MESSAGE", "Detail": "End of message for variable length messages", "Default": 0, "Length": 1, "parts": [], "type": "M_END_OF_MESSAGE", "Display": "", "Special": []}');
function FIELD_DECODE_M_END_OF_MESSAGE(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_M_END_OF_MESSAGE, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_M_VERSION = json.decode('{"Min": 0, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "M_VERSION", "Detail": "Version", "Default": null, "Length": 1, "parts": [], "type": "M_VERSION", "Display": "", "Special": []}');
function FIELD_DECODE_M_VERSION(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_M_VERSION, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_M_TEXT = json.decode('{"Min": null, "Resolution": null, "Max": null, "Format": "STRING", "fldname": "M_TEXT", "Detail": "Text string", "Default": null, "Length": 99, "parts": [], "type": "M_TEXT", "Display": "", "Special": []}');
function FIELD_DECODE_M_TEXT(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_M_TEXT, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_N_LENGTH = json.decode('{"Min": 1, "Resolution": 1, "Max": 65535, "Format": "UINT", "fldname": "N_LENGTH", "Detail": "Number of bytes of application data to/from external system", "Default": null, "Length": 2, "parts": [], "type": "N_LENGTH", "Display": "", "Special": [{"text": 0, "type": "Illegal"}]}');
function FIELD_DECODE_N_LENGTH(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_N_LENGTH, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_N_VALUE = json.decode('{"Min": 1, "Resolution": 1, "Max": 65535, "Format": "UINT", "fldname": "N_VALUE", "Detail": "Any value", "Default": null, "Length": 2, "parts": [], "type": "N_VALUE", "Display": "", "Special": []}');
function FIELD_DECODE_N_VALUE(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_N_VALUE, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_NID_AREA = json.decode('{"Min": 1, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "NID_AREA", "Detail": "Region area identification", "Default": null, "Length": 1, "parts": [], "type": "NID_AREA", "Display": "", "Special": []}');
function FIELD_DECODE_NID_AREA(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_NID_AREA, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_NID_BLOCK_TYPE = json.decode('{"Min": 1, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "NID_BLOCK_TYPE", "Detail": "Data block identification", "Default": 1, "Length": 1, "parts": [], "type": "NID_BLOCK_TYPE", "Display": "", "Special": []}');
function FIELD_DECODE_NID_BLOCK_TYPE(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_NID_BLOCK_TYPE, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_NID_BG = json.decode('{"Min": 1, "Resolution": null, "Max": 65535, "Format": "UINT", "fldname": "NID_BG", "Detail": "Balise identification", "Default": null, "Length": 2, "parts": [], "type": "NID_BG", "Display": "", "Special": []}');
function FIELD_DECODE_NID_BG(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_NID_BG, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_NID_ERROR_NO = json.decode('{"Min": 0, "Resolution": null, "Max": 4294967295, "Format": "UINT", "fldname": "NID_ERROR_NO", "Detail": "Numeric code for the error", "Default": null, "Length": 4, "parts": [], "type": "NID_ERROR_NO", "Display": "", "Special": []}');
function FIELD_DECODE_NID_ERROR_NO(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_NID_ERROR_NO, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_NID_LOCATION_TYPE = json.decode('{"Min": 0, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "NID_LOCATION_TYPE", "Detail": "Location type description", "Default": 0, "Length": 1, "parts": [], "type": "NID_LOCATION_TYPE", "Display": "", "Special": []}');
function FIELD_DECODE_NID_LOCATION_TYPE(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_NID_LOCATION_TYPE, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_NID_MSG = json.decode('{"Min": 0, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "NID_MSG", "Detail": "Identification of the message", "Default": 0, "Length": 1, "parts": [], "type": "NID_MSG", "Display": "", "Special": []}');
function FIELD_DECODE_NID_MSG(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_NID_MSG, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_TS_STATE = json.decode('{"Min": 1, "Resolution": null, "Max": 2, "Format": "UINT", "fldname": "Q_TS_STATE", "Detail": "Indicates the state of the train setup", "Default": 0, "Length": 1, "parts": [], "type": "Q_TS_STATE", "Display": "", "Special": [{"text": "Temporary", "type": "Field", "value": 1}, {"text": "Permanent", "type": "Field", "value": 2}]}');
function FIELD_DECODE_Q_TS_STATE(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_TS_STATE, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_M_BRAKE_SYSTEM = json.decode('{"Min": 0, "Resolution": null, "Max": 2, "Format": "UINT", "fldname": "M_BRAKE_SYSTEM", "Detail": "Defines the type of brake system in use", "Default": 0, "Length": 1, "parts": [], "type": "M_BRAKE_SYSTEM", "Display": "", "Special": [{"text": "Undefined", "type": "Field", "value": 0}, {"text": "Pneumatic brake system", "type": "Field", "value": 1}, {"text": "ECPB", "type": "Field", "value": 2}]}');
function FIELD_DECODE_M_BRAKE_SYSTEM(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_M_BRAKE_SYSTEM, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_M_LOADED = json.decode('{"Min": 0, "Resolution": null, "Max": 1, "Format": "UINT", "fldname": "M_LOADED", "Detail": "Indication if train is loaded or empty", "Default": 1, "Length": 1, "parts": [], "type": "M_LOADED", "Display": "", "Special": [{"text": "Train is empty", "type": "Field", "value": 0}, {"text": "Train is loaded", "type": "Field", "value": 1}]}');
function FIELD_DECODE_M_LOADED(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_M_LOADED, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_D_OVERLAP = json.decode('{"Min": 0, "Resolution": "1 cm", "Max": 65535, "Format": "UINT", "fldname": "D_OVERLAP", "Detail": "Margin between the end of MA and the point of conflict.", "Default": null, "Length": 2, "parts": [], "type": "D_OVERLAP", "Display": "", "Special": []}');
function FIELD_DECODE_D_OVERLAP(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_D_OVERLAP, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_N_ADHESION = json.decode('{"Min": 50, "Resolution": "1 %", "Max": 100, "Format": "UINT", "fldname": "N_ADHESION", "Detail": "Brake ability in percent of normal brake ability", "Default": 100, "Length": 1, "parts": [], "type": "N_ADHESION", "Display": "", "Special": []}');
function FIELD_DECODE_N_ADHESION(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_N_ADHESION, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_NID_EVENT_NO = json.decode('{"Min": 0, "Resolution": null, "Max": 4294967295, "Format": "UINT", "fldname": "NID_EVENT_NO", "Detail": "Numeric event code", "Default": null, "Length": 4, "parts": [], "type": "NID_EVENT_NO", "Display": "", "Special": []}');
function FIELD_DECODE_NID_EVENT_NO(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_NID_EVENT_NO, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_D_BRAKE_DISTANCE = json.decode('{"Min": 0, "Resolution": "1 cm", "Max": 16777215, "Format": "UINT", "fldname": "D_BRAKE_DISTANCE", "Detail": "Worst case Braking distance", "Default": null, "Length": 3, "parts": [], "type": "D_BRAKE_DISTANCE", "Display": "", "Special": []}');
function FIELD_DECODE_D_BRAKE_DISTANCE(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_D_BRAKE_DISTANCE, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_NID_MESSAGE_TYPE = json.decode('{"Min": 1, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "NID_MESSAGE_TYPE", "Detail": "Message type identification", "Default": null, "Length": 1, "parts": [], "type": "NID_MESSAGE_TYPE", "Display": "", "Special": []}');
function FIELD_DECODE_NID_MESSAGE_TYPE(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_NID_MESSAGE_TYPE, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_NID_PREVIOUS_TRACK = json.decode('{"Min": 0, "Resolution": null, "Max": 65535, "Format": "UINT", "fldname": "NID_PREVIOUS_TRACK", "Detail": "Identification of the previous track", "Default": 0, "Length": 2, "parts": [], "type": "NID_PREVIOUS_TRACK", "Display": "", "Special": []}');
function FIELD_DECODE_NID_PREVIOUS_TRACK(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_NID_PREVIOUS_TRACK, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_NID_SYSTEM = json.decode('{"Min": 0, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "NID_SYSTEM", "Detail": "External system identification", "Default": 0, "Length": 1, "parts": [], "type": "NID_SYSTEM", "Display": "", "Special": []}');
function FIELD_DECODE_NID_SYSTEM(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_NID_SYSTEM, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_NID_TRACK = json.decode('{"Min": 0, "Resolution": null, "Max": 65535, "Format": "UINT", "fldname": "NID_TRACK", "Detail": "Track section identity", "Default": null, "Length": 2, "parts": [], "type": "NID_TRACK", "Display": "", "Special": []}');
function FIELD_DECODE_NID_TRACK(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_NID_TRACK, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_NID_VEHICLE = json.decode('{"Min": 0, "Resolution": null, "Max": 65535, "Format": "UINT", "fldname": "NID_VEHICLE", "Detail": "Numeric vehicle identififer", "Default": null, "Length": 2, "parts": [], "type": "NID_VEHICLE", "Display": "", "Special": []}');
function FIELD_DECODE_NID_VEHICLE(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_NID_VEHICLE, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_DIRECTION = json.decode('{"Min": 0, "Resolution": null, "Max": 3, "Format": "UINT", "fldname": "Q_DIRECTION", "Detail": "Valid direction", "Default": 0, "Length": 1, "parts": [], "type": "Q_DIRECTION", "Display": "", "Special": [{"text": "Undefined", "type": "Field", "value": 0}, {"text": "Forward", "type": "Field", "value": 1}, {"text": "Reverse", "type": "Field", "value": 2}, {"text": "Both", "type": "Field", "value": 3}]}');
function FIELD_DECODE_Q_DIRECTION(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_DIRECTION, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_NID_VEHICLE_TYPE = json.decode('{"Min": 0, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "NID_VEHICLE_TYPE", "Detail": "Vehicle type identification", "Default": null, "Length": 1, "parts": [], "type": "NID_VEHICLE_TYPE", "Display": "", "Special": []}');
function FIELD_DECODE_NID_VEHICLE_TYPE(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_NID_VEHICLE_TYPE, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_ABORT = json.decode('{"Min": 0, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "Q_ABORT", "Detail": "Further information regarding the set-up abort", "Default": null, "Length": 1, "parts": [], "type": "Q_ABORT", "Display": "", "Special": [{"text": "Aborted by driver", "type": "Field", "value": 1}]}');
function FIELD_DECODE_Q_ABORT(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_ABORT, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_ACKNOWLEDGE = json.decode('{"Min": 0, "Resolution": null, "Max": 1, "Format": "UINT", "fldname": "Q_ACKNOWLEDGE", "Detail": "Used for acknowledge of a request", "Default": 0, "Length": 1, "parts": [], "type": "Q_ACKNOWLEDGE", "Display": "", "Special": [{"text": "Request not acknowledged", "type": "Field", "value": 0}, {"text": "Request acknowledged", "type": "Field", "value": 1}]}');
function FIELD_DECODE_Q_ACKNOWLEDGE(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_ACKNOWLEDGE, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_ALERT = json.decode('{"Min": 0, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "Q_ALERT", "Detail": "Emergency alert code", "Default": 0, "Length": 1, "parts": [], "type": "Q_ALERT", "Display": "", "Special": [{"text": "Undefined", "type": "Field", "value": 0}, {"text": "Initiated by driver", "type": "Field", "value": 1}, {"text": "Initiated by dispatcher", "type": "Field", "value": 2}, {"text": "Position report outside set route", "type": "Field", "value": 3}, {"text": "Points inside set route in error", "type": "Field", "value": 4}, {"text": "Profile control triggered", "type": "Field", "value": 5}, {"text": "Powerless section", "type": "Field", "value": 6}, {"text": "Gate forced open", "type": "Field", "value": 7}, {"text": "Location closed", "type": "Field", "value": 8}, {"text": "Location error", "type": "Field", "value": 9}, {"text": "Location maintenance", "type": "Field", "value": 10}, {"text": "Location shutdown", "type": "Field", "value": 11}, {"text": "Location stopped", "type": "Field", "value": 12}, {"text": "Other train/Other train in error", "type": "Field", "value": 13}, {"text": "Route conflict", "type": "Field", "value": 14}, {"text": "Emergency Stop Area", "type": "Field", "value": 15}, {"text": "TIMS error", "type": "Field", "value": 16}]}');
function FIELD_DECODE_Q_ALERT(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_ALERT, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_ATO_MODE = json.decode('{"Min": 0, "Resolution": null, "Max": 4, "Format": "UINT", "fldname": "Q_ATO_MODE", "Detail": "ATO Mode", "Default": 0, "Length": 1, "parts": [], "type": "Q_ATO_MODE", "Display": "", "Special": [{"text": "Undefined", "type": "Field", "value": 0}, {"text": "Manual", "type": "Field", "value": 1}, {"text": "Supervised automatic", "type": "Field", "value": 2}, {"text": "Automatic", "type": "Field", "value": 3}, {"text": "Remote control", "type": "Field", "value": 4}]}');
function FIELD_DECODE_Q_ATO_MODE(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_ATO_MODE, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_ATP_MODE = json.decode('{"Min": 0, "Resolution": null, "Max": 19, "Format": "UINT", "fldname": "Q_ATP_MODE", "Detail": "ATP Mode", "Default": 0, "Length": 1, "parts": [], "type": "Q_ATP_MODE", "Display": "", "Special": [{"text": "Undefined", "type": "Field", "value": 0}, {"text": "Power Up", "type": "Field", "value": 1}, {"text": "Configuration", "type": "Field", "value": 2}, {"text": "Registration", "type": "Field", "value": 3}, {"text": "Balise Search", "type": "Field", "value": 4}, {"text": "Normal(Full ATP)", "type": "Field", "value": 5}, {"text": "Shunting", "type": "Field", "value": 6}, {"text": "Location", "type": "Field", "value": 7}, {"text": "Automatic Unload", "type": "Field", "value": 8}, {"text": "Yard", "type": "Field", "value": 9}, {"text": "Unregistered", "type": "Field", "value": 10}, {"text": "Power Down", "type": "Field", "value": 11}, {"text": "Safety Halt (Fatal Failure)", "type": "Field", "value": 12}, {"text": "Sleeping", "type": "Field", "value": 13}, {"text": "Staff Responsible", "type": "Field", "value": 14}, {"text": "Shunting Route", "type": "Field", "value": 15}, {"text": "Possession", "type": "Field", "value": 16}, {"text": "Split", "type": "Field", "value": 17}, {"text": "Join", "type": "Field", "value": 18}, {"text": "Safe Brake to Stop", "type": "Field", "value": 19}]}');
function FIELD_DECODE_Q_ATP_MODE(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_ATP_MODE, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_ERROR_LEVEL = json.decode('{"Min": 1, "Resolution": null, "Max": 3, "Format": "UINT", "fldname": "Q_ERROR_LEVEL", "Detail": "Error classification", "Default": null, "Length": 1, "parts": [], "type": "Q_ERROR_LEVEL", "Display": "", "Special": [{"text": "Fatal", "type": "Field", "value": 1}, {"text": "Minor", "type": "Field", "value": 2}, {"text": "Log", "type": "Field", "value": 3}]}');
function FIELD_DECODE_Q_ERROR_LEVEL(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_ERROR_LEVEL, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_LOGON_STATUS = json.decode('{"Min": 0, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "Q_LOGON_STATUS", "Detail": "Response from the stationary system, regarding driver logon", "Default": null, "Length": 1, "parts": [], "type": "Q_LOGON_STATUS", "Display": "", "Special": [{"text": "Unsuccessful logon", "type": "Field", "value": 0}, {"text": "Successfull logon, Normal privilege", "type": "Field", "value": 1}]}');
function FIELD_DECODE_Q_LOGON_STATUS(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_LOGON_STATUS, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_MESSAGE_STATUS = json.decode('{"Min": 0, "Resolution": null, "Max": 1, "Format": "UINT", "fldname": "Q_MESSAGE_STATUS", "Detail": "Response from AOS regarding message accepted or not.", "Default": null, "Length": 1, "parts": [], "type": "Q_MESSAGE_STATUS", "Display": "", "Special": [{"text": "Message not accepted", "type": "Field", "value": 0}, {"text": "Message accepted", "type": "Field", "value": 1}]}');
function FIELD_DECODE_Q_MESSAGE_STATUS(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_MESSAGE_STATUS, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_PANTO_POSITION = json.decode('{"Min": 0, "Resolution": null, "Max": 1, "Format": "UINT", "fldname": "Q_PANTO_POSITION", "Detail": "Position of pantograph", "Default": null, "Length": 1, "parts": [], "type": "Q_PANTO_POSITION", "Display": "", "Special": [{"text": "None, all pantographs shall be down/in", "type": "Field", "value": 0}, {"text": "Centre, centre pantograph shall be up", "type": "Field", "value": 1}, {"text": "Side, side pantograph shall be out", "type": "Field", "value": 2}]}');
function FIELD_DECODE_Q_PANTO_POSITION(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_PANTO_POSITION, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_POSITION = json.decode('{"Min": 0, "Resolution": null, "Max": 3, "Format": "UINT", "fldname": "Q_POSITION", "Detail": "Qualifier for the position in a PositionReport message", "Default": 0, "Length": 1, "parts": [], "type": "Q_POSITION", "Display": "", "Special": [{"text": "Unknown", "type": "Field", "value": 0}, {"text": "Approximate", "type": "Field", "value": 1}, {"text": "Known", "type": "Field", "value": 2}, {"text": "Doubtful", "type": "Field", "value": 3}]}');
function FIELD_DECODE_Q_POSITION(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_POSITION, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_POWER = json.decode('{"Min": 0, "Resolution": null, "Max": 1, "Format": "UINT", "fldname": "Q_POWER", "Detail": "Locomotive power up or down request from TCC", "Default": null, "Length": 1, "parts": [], "type": "Q_POWER", "Display": "", "Special": [{"text": "Power down", "type": "Field", "value": 0}, {"text": "Power up", "type": "Field", "value": 1}]}');
function FIELD_DECODE_Q_POWER(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_POWER, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_PRESSURE_SENSOR = json.decode('{"Min": 0, "Resolution": null, "Max": 1, "Format": "UINT", "fldname": "Q_PRESSURE_SENSOR", "Detail": "If the car have a pressure sensor", "Default": null, "Length": 1, "parts": [], "type": "Q_PRESSURE_SENSOR", "Display": "", "Special": [{"text": "Pressure sensor not available", "type": "Field", "value": 0}, {"text": "Pressure sensor available", "type": "Field", "value": 1}]}');
function FIELD_DECODE_Q_PRESSURE_SENSOR(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_PRESSURE_SENSOR, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_PROFILE = json.decode('{"Min": 0, "Resolution": null, "Max": 1, "Format": "UINT", "fldname": "Q_PROFILE", "Detail": "Status of profile gauger", "Default": null, "Length": 1, "parts": [], "type": "Q_PROFILE", "Display": "", "Special": [{"text": "Profile control released", "type": "Field", "value": 0}, {"text": "Profile control triggered", "type": "Field", "value": 1}]}');
function FIELD_DECODE_Q_PROFILE(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_PROFILE, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_ROUTE_TYPE = json.decode('{"Min": 1, "Resolution": null, "Max": 9, "Format": "UINT", "fldname": "Q_ROUTE_TYPE", "Detail": "Description of route type.", "Default": null, "Length": 1, "parts": [], "type": "Q_ROUTE_TYPE", "Display": "", "Special": [{"text": "Undefined", "type": "Field", "value": 0}, {"text": "Location start", "type": "Field", "value": 1}, {"text": "Location End", "type": "Field", "value": 2}, {"text": "Re-registration", "type": "Field", "value": 3}, {"text": "Shunting Route", "type": "Field", "value": 4}, {"text": "Normal", "type": "Field", "value": 5}, {"text": "Join", "type": "Field", "value": 6}, {"text": "Split", "type": "Field", "value": 7}, {"text": "Staff Responsible (SR)", "type": "Field", "value": 8}, {"text": "Unconditional shortening of MA", "type": "Field", "value": 9}]}');
function FIELD_DECODE_Q_ROUTE_TYPE(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_ROUTE_TYPE, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_SETUP = json.decode('{"Min": 0, "Resolution": null, "Max": 3, "Format": "UINT", "fldname": "Q_SETUP", "Detail": "Reason for TrainSetup mesage", "Default": null, "Length": 1, "parts": [], "type": "Q_SETUP", "Display": "", "Special": [{"text": "Registration", "type": "Field", "value": 0}, {"text": "Reconfiguration", "type": "Field", "value": 1}, {"text": "Reregistration", "type": "Field", "value": 2}, {"text": "Reposition", "type": "Field", "value": 3}]}');
function FIELD_DECODE_Q_SETUP(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_SETUP, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_SIGNAL = json.decode('{"Min": 0, "Resolution": null, "Max": 3, "Format": "UINT", "fldname": "Q_SIGNAL", "Detail": "Type of acoustic signal", "Default": null, "Length": 1, "parts": [], "type": "Q_SIGNAL", "Display": "", "Special": [{"text": "Undefined", "type": "Field", "value": 0}, {"text": "Sound type 1", "type": "Field", "value": 1}, {"text": "Sound type 2", "type": "Field", "value": 2}, {"text": "Sound type 3", "type": "Field", "value": 3}]}');
function FIELD_DECODE_Q_SIGNAL(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_SIGNAL, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_SPEED = json.decode('{"Min": 0, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "Q_SPEED", "Detail": "Reason for change in ceiling speed", "Default": null, "Length": 1, "parts": [], "type": "Q_SPEED", "Display": "", "Special": [{"text": "Undefined", "type": "Field", "value": 0}, {"text": "Points straight", "type": "Field", "value": 1}, {"text": "Points curve", "type": "Field", "value": 2}, {"text": "Points passed", "type": "Field", "value": 3}, {"text": "Location", "type": "Field", "value": 4}, {"text": "Temporary speed restriction", "type": "Field", "value": 5}, {"text": "Other", "type": "Field", "value": 255}]}');
function FIELD_DECODE_Q_SPEED(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_SPEED, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_STOP = json.decode('{"Min": 0, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "Q_STOP", "Detail": "Reason for stop message", "Default": null, "Length": 1, "parts": [], "type": "Q_STOP", "Display": "", "Special": [{"text": "Undefined", "type": "Field", "value": 0}, {"text": "Order cancelled", "type": "Field", "value": 1}]}');
function FIELD_DECODE_Q_STOP(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_STOP, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_SUPERVISION = json.decode('{"Min": 0, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "Q_SUPERVISION", "Detail": "Type of supervision", "Default": null, "Length": 1, "parts": [], "type": "Q_SUPERVISION", "Display": "", "Special": [{"text": "No supervision", "type": "Field", "value": 0}, {"text": "Supervise with brake curve", "type": "Field", "value": 1}, {"text": "Supervise without brake curve, emergency brake", "type": "Field", "value": 2}]}');
function FIELD_DECODE_Q_SUPERVISION(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_SUPERVISION, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_TIC_AVAILABLE = json.decode('{"Min": 0, "Resolution": null, "Max": 1, "Format": "UINT", "fldname": "Q_TIC_AVAILABLE", "Detail": "Indication if TIC is available in the train", "Default": null, "Length": 1, "parts": [], "type": "Q_TIC_AVAILABLE", "Display": "", "Special": [{"text": "TIC not available, train set manual configuration", "type": "Field", "value": 0}, {"text": "TIC is available, train set automatic configuration", "type": "Field", "value": 1}]}');
function FIELD_DECODE_Q_TIC_AVAILABLE(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_TIC_AVAILABLE, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_TIMS_AVAILABLE = json.decode('{"Min": 0, "Resolution": null, "Max": 1, "Format": "UINT", "fldname": "Q_TIMS_AVAILABLE", "Detail": "Indication if TIMS is available/shall be used", "Default": 1, "Length": 1, "parts": [], "type": "Q_TIMS_AVAILABLE", "Display": "", "Special": [{"text": "TIMS not available/shall not be used", "type": "Field", "value": 0}, {"text": "TIMS available/shall be used", "type": "Field", "value": 1}]}');
function FIELD_DECODE_Q_TIMS_AVAILABLE(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_TIMS_AVAILABLE, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_TRACK_DATA_TYPE = json.decode('{"Min": 0, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "Q_TRACK_DATA_TYPE", "Detail": "Defines type of track data.", "Default": null, "Length": 1, "parts": [], "type": "Q_TRACK_DATA_TYPE", "Display": "", "Special": []}');
function FIELD_DECODE_Q_TRACK_DATA_TYPE(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_TRACK_DATA_TYPE, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_TRAIN_END = json.decode('{"Min": 0, "Resolution": null, "Max": 1, "Format": "UINT", "fldname": "Q_TRAIN_END", "Detail": "Defines type of track data.", "Length": 1, "parts": [], "type": "Q_TRAIN_END", "Display": "", "Special": [{"text": "Rear of train (train length delay)", "type": "Field", "value": 0}, {"text": "Front of train", "type": "Field", "value": 1}]}');
function FIELD_DECODE_Q_TRAIN_END(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_TRAIN_END, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_UNREGISTRATION = json.decode('{"Min": 0, "Resolution": null, "Max": 255, "Format": "UINT", "fldname": "Q_UNREGISTRATION", "Detail": "Why the Stationary system has unregistered the train.", "Default": null, "Length": 1, "parts": [], "type": "Q_UNREGISTRATION", "Display": "", "Special": [{"text": "Unregistered by dispatcher", "type": "Field", "value": 0}, {"text": "Configuration rejected, unknown vehicle in train", "type": "Field", "value": 1}, {"text": "Configuration rejected, a vehicle is part of another train", "type": "Field", "value": 2}, {"text": "Registration rejected, unexpected balise identity", "type": "Field", "value": 3}, {"text": "Registration rejected, conflict with another train route", "type": "Field", "value": 4}, {"text": "Registration aborted by driver or ATP", "type": "Field", "value": 5}, {"text": "Registration rejected, wrong driving direction", "type": "Field", "value": 6}, {"text": "Registration rejected, not possible to set route", "type": "Field", "value": 7}, {"text": "Configuration rejected, to many cars in train", "type": "Field", "value": 8}, {"text": "Configuration rejected, duplicated car identities", "type": "Field", "value": 9}]}');
function FIELD_DECODE_Q_UNREGISTRATION(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_UNREGISTRATION, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_T_BRAKE_RESPONSE = json.decode('{"Min": 0, "Resolution": "milliseconds", "Max": 65535, "Format": "UINT", "fldname": "T_BRAKE_RESPONSE", "Detail": "Reaction time for brake application.", "Default": 65535, "Length": 2, "parts": [], "type": "T_BRAKE_RESPONSE", "Display": "", "Special": []}');
function FIELD_DECODE_T_BRAKE_RESPONSE(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_T_BRAKE_RESPONSE, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_T_CLOCK = json.decode('{"Min": 891514000, "Resolution": "Seconds", "Max": 4294967295, "Format": "UINT", "fldname": "T_CLOCK", "Detail": "Number of seconds since 00:00 GMT 1970-01-01, with adjustment for local time zone", "Default": null, "Length": 4, "parts": [], "type": "T_CLOCK", "Display": "", "Special": []}');
function FIELD_DECODE_T_CLOCK(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_T_CLOCK, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_T_MAX_PLATFORM_UP = json.decode('{"Min": 0, "Resolution": "Seconds", "Max": 255, "Format": "UINT", "fldname": "T_MAX_PLATFORM_UP", "Detail": "Maximum time for the platform to reach its upper position", "Default": null, "Length": 1, "parts": [], "type": "T_MAX_PLATFORM_UP", "Display": "", "Special": []}');
function FIELD_DECODE_T_MAX_PLATFORM_UP(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_T_MAX_PLATFORM_UP, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_T_PLATFORM_UPPER = json.decode('{"Min": 0, "Resolution": "seconds", "Max": 255, "Format": "UINT", "fldname": "T_PLATFORM_UPPER", "Detail": "Time for the platform to stay in the upper position", "Default": null, "Length": 1, "parts": [], "type": "T_PLATFORM_UPPER", "Display": "", "Special": []}');
function FIELD_DECODE_T_PLATFORM_UPPER(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_T_PLATFORM_UPPER, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_T_PRE_PRESSURISATION = json.decode('{"Min": 0, "Resolution": "seconds", "Max": 255, "Format": "UINT", "fldname": "T_PRE_PRESSURISATION", "Detail": "Time for the platform pre-pressurization", "Default": null, "Length": 1, "parts": [], "type": "T_PRE_PRESSURISATION", "Display": "", "Special": []}');
function FIELD_DECODE_T_PRE_PRESSURISATION(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_T_PRE_PRESSURISATION, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_T_REMOTE = json.decode('{"Min": 0, "Resolution": "Seconds", "Max": 255, "Format": "UINT", "fldname": "T_REMOTE", "Detail": "Time limit for remote order", "Default": null, "Length": 1, "parts": [], "type": "T_REMOTE", "Display": "", "Special": []}');
function FIELD_DECODE_T_REMOTE(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_T_REMOTE, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_T_UNLOAD_ACTION = json.decode('{"Min": 0, "Resolution": "seconds", "Max": 255, "Format": "UINT", "fldname": "T_UNLOAD_ACTION", "Detail": "Timeout for unload action", "Default": null, "Length": 1, "parts": [], "type": "T_UNLOAD_ACTION", "Display": "", "Special": [{"text": 0, "type": "Illegal"}]}');
function FIELD_DECODE_T_UNLOAD_ACTION(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_T_UNLOAD_ACTION, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_T_UNLOAD_HATCH_OPEN = json.decode('{"Min": 0, "Resolution": "seconds", "Max": 255, "Format": "UINT", "fldname": "T_UNLOAD_HATCH_OPEN", "Detail": "Time for the unload hatch to be open", "Default": null, "Length": 1, "parts": [], "type": "T_UNLOAD_HATCH_OPEN", "Display": "", "Special": []}');
function FIELD_DECODE_T_UNLOAD_HATCH_OPEN(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_T_UNLOAD_HATCH_OPEN, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_T_VALID = json.decode('{"Min": 0, "Resolution": "minutes", "Max": 255, "Format": "UINT", "fldname": "T_VALID", "Detail": "Timeout for execution of the Movement Authority", "Default": null, "Length": 1, "parts": [], "type": "T_VALID", "Display": "", "Special": []}');
function FIELD_DECODE_T_VALID(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_T_VALID, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_T_WAITING_TIME = json.decode('{"Min": 0, "Resolution": "minutes", "Max": 255, "Format": "UINT", "fldname": "T_WAITING_TIME", "Detail": "Time to wait", "Default": null, "Length": 1, "parts": [], "type": "T_WAITING_TIME", "Display": "", "Special": []}');
function FIELD_DECODE_T_WAITING_TIME(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_T_WAITING_TIME, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_TID_DRIVER = json.decode('{"Min": null, "Resolution": null, "Max": null, "Format": "STRING", "fldname": "TID_DRIVER", "Detail": "Driver identification code", "Default": null, "Length": 20, "parts": [], "type": "TID_DRIVER", "Display": "", "Special": []}');
function FIELD_DECODE_TID_DRIVER(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_TID_DRIVER, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_TID_LOCATION = json.decode('{"Min": null, "Resolution": null, "Max": null, "Format": "STRING", "fldname": "TID_LOCATION", "Detail": "Location identification", "Default": null, "Length": 20, "parts": [], "type": "TID_LOCATION", "Display": "", "Special": []}');
function FIELD_DECODE_TID_LOCATION(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_TID_LOCATION, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_TID_PASSWORD = json.decode('{"Min": null, "Resolution": null, "Max": null, "Format": "STRING", "fldname": "TID_PASSWORD", "Detail": "Driver password", "Default": null, "Length": 20, "parts": [], "type": "TID_PASSWORD", "Display": "", "Special": []}');
function FIELD_DECODE_TID_PASSWORD(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_TID_PASSWORD, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_TID_TEXT_STRING = json.decode('{"Min": null, "Resolution": null, "Max": null, "Format": "STRING", "fldname": "TID_TEXT_STRING", "Detail": "String for parameters, events etc.", "Default": null, "Length": 20, "parts": [], "type": "TID_TEXT_STRING", "Display": "", "Special": []}');
function FIELD_DECODE_TID_TEXT_STRING(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_TID_TEXT_STRING, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_Q_PROTOCOL_RESPONSE = json.decode('{"Min": 0, "Resolution": null, "Max": 3, "Format": "UINT", "fldname": "Q_PROTOCOL_RESPONSE", "Detail": "Protocol version status", "Default": null, "Length": 1, "parts": [], "type": "Q_PROTOCOL_RESPONSE", "Display": "", "Special": [{"text": "Protocol check request (TCC request)", "type": "Field", "value": 0}, {"text": "Protocols match (AOS response)", "type": "Field", "value": 1}, {"text": "Protocols mismatch, waiting for new version (AOS response)", "type": "Field", "value": 2}, {"text": "Unrecoverable mismatch (TCC termination of link)", "type": "Field", "value": 3}]}');
function FIELD_DECODE_Q_PROTOCOL_RESPONSE(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_Q_PROTOCOL_RESPONSE, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_TID_TRAIN_NAME = json.decode('{"Min": null, "Resolution": null, "Max": null, "Format": "STRING", "fldname": "TID_TRAIN_NAME", "Detail": "Name of the train defined in the stationary system, used for presentation on the MMI.", "Default": null, "Length": 20, "parts": [], "type": "TID_TRAIN_NAME", "Display": "", "Special": []}');
function FIELD_DECODE_TID_TRAIN_NAME(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_TID_TRAIN_NAME, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_TID_VEHICLE_NAME = json.decode('{"Min": null, "Resolution": null, "Max": null, "Format": "STRING", "fldname": "TID_VEHICLE_NAME", "Detail": "Name of the vehicle.", "Default": null, "Length": 20, "parts": [], "type": "TID_VEHICLE_NAME", "Display": "", "Special": []}');
function FIELD_DECODE_TID_VEHICLE_NAME(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_TID_VEHICLE_NAME, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_V_SPEED = json.decode('{"Min": 0, "Resolution": "cm/s", "Max": 65535, "Format": "UINT", "fldname": "V_SPEED", "Detail": "The requested velocity of the train in cm/s", "Default": 0, "Length": 2, "parts": [], "type": "V_SPEED", "Display": "", "Special": []}');
function FIELD_DECODE_V_SPEED(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_V_SPEED, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_W_WEIGHT = json.decode('{"Min": 0, "Resolution": "1 Mg", "Max": 65535, "Format": "UINT", "fldname": "W_WEIGHT", "Detail": "Weight in Mg (1000000 g)", "Default": 0, "Length": 2, "parts": [], "type": "W_WEIGHT", "Display": "", "Special": []}');
function FIELD_DECODE_W_WEIGHT(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_W_WEIGHT, fld, subflds)
end
        
-------------------------------
FIELD_SPEC_M_END_OF_MESSAGE = json.decode('{"fldname": "M_END_OF_MESSAGE", "parts": [], "type": "M_END_OF_MESSAGE", "Display": "", "Special": []}');
function FIELD_DECODE_M_END_OF_MESSAGE(ctx,fld,subflds)
    return FIELD_DECODE(ctx, FIELD_SPEC_M_END_OF_MESSAGE, fld, subflds)
end
        
--- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

--- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
--- Decode of blocks

-------------------------------
fld_ACOUSTIC_SIGNAL = ProtoField.new("ACOUSTIC_SIGNAL","ACOUSTIC_SIGNAL",ftypes.NONE)


BF_ACOUSTIC_SIGNAL_00_NID_TRACK = ProtoField.uint16( "BF_ACOUSTIC_SIGNAL_00_NID_TRACK", "NID_TRACK", base.DEC )

BF_ACOUSTIC_SIGNAL_01_D_POSITION = ProtoField.int32( "BF_ACOUSTIC_SIGNAL_01_D_POSITION", "D_POSITION", base.DEC )

BF_ACOUSTIC_SIGNAL_02_Q_SIGNAL = ProtoField.uint8( "BF_ACOUSTIC_SIGNAL_02_Q_SIGNAL", "Q_SIGNAL", base.DEC  ,{ [0] = "Undefined",[1] = "Sound type 1",[2] = "Sound type 2",[3] = "Sound type 3" } )
function BLOCK_DECODE_ACOUSTIC_SIGNAL(ctx)
    local btree = ctx.stack:top():add(fld_ACOUSTIC_SIGNAL,ctx.tvbuf:range(ctx.off,7));
    ctx.stack:push(btree);

    FIELD_DECODE_NID_TRACK(ctx,BF_ACOUSTIC_SIGNAL_00_NID_TRACK,{  })
    FIELD_DECODE_D_POSITION(ctx,BF_ACOUSTIC_SIGNAL_01_D_POSITION,{  })
    FIELD_DECODE_Q_SIGNAL(ctx,BF_ACOUSTIC_SIGNAL_02_Q_SIGNAL,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_AOS_VERSION = ProtoField.new("AOS_VERSION","AOS_VERSION",ftypes.NONE)


BF_AOS_VERSION_00_M_VERSION = ProtoField.uint8( "BF_AOS_VERSION_00_M_VERSION", "ATP Major", base.DEC )

BF_AOS_VERSION_01_M_VERSION = ProtoField.uint8( "BF_AOS_VERSION_01_M_VERSION", "ATP Minor", base.DEC )

BF_AOS_VERSION_02_M_VERSION = ProtoField.uint8( "BF_AOS_VERSION_02_M_VERSION", "ATP Sub", base.DEC )

BF_AOS_VERSION_03_M_VERSION = ProtoField.uint8( "BF_AOS_VERSION_03_M_VERSION", "ATO Major", base.DEC )

BF_AOS_VERSION_04_M_VERSION = ProtoField.uint8( "BF_AOS_VERSION_04_M_VERSION", "ATO Minor", base.DEC )

BF_AOS_VERSION_05_M_VERSION = ProtoField.uint8( "BF_AOS_VERSION_05_M_VERSION", "ATO Sub", base.DEC )
function BLOCK_DECODE_AOS_VERSION(ctx)
    local btree = ctx.stack:top():add(fld_AOS_VERSION,ctx.tvbuf:range(ctx.off,6));
    ctx.stack:push(btree);

    FIELD_DECODE_M_VERSION(ctx,BF_AOS_VERSION_00_M_VERSION,{  })
    FIELD_DECODE_M_VERSION(ctx,BF_AOS_VERSION_01_M_VERSION,{  })
    FIELD_DECODE_M_VERSION(ctx,BF_AOS_VERSION_02_M_VERSION,{  })
    FIELD_DECODE_M_VERSION(ctx,BF_AOS_VERSION_03_M_VERSION,{  })
    FIELD_DECODE_M_VERSION(ctx,BF_AOS_VERSION_04_M_VERSION,{  })
    FIELD_DECODE_M_VERSION(ctx,BF_AOS_VERSION_05_M_VERSION,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_CANCEL_AREA = ProtoField.new("CANCEL_AREA","CANCEL_AREA",ftypes.NONE)

function BLOCK_DECODE_CANCEL_AREA(ctx)
    local btree = ctx.stack:top():add(fld_CANCEL_AREA,ctx.tvbuf:range(ctx.off,0));
    ctx.stack:push(btree);

    ctx.stack:pop(1);
end

-------------------------------
fld_ATO_PROFILE_CONTROL = ProtoField.new("ATO_PROFILE_CONTROL","ATO_PROFILE_CONTROL",ftypes.NONE)


BF_ATO_PROFILE_CONTROL_00_Q_PROFILE = ProtoField.uint8( "BF_ATO_PROFILE_CONTROL_00_Q_PROFILE", "Q_PROFILE", base.DEC  ,{ [0] = "Profile control released",[1] = "Profile control triggered" } )

BF_ATO_PROFILE_CONTROL_01_D_REVERSE = ProtoField.uint16( "BF_ATO_PROFILE_CONTROL_01_D_REVERSE", "D_REVERSE", base.DEC )
BF_ATO_PROFILE_CONTROL_02_B_DIRECTION_bit0 = ProtoField.uint8( "BF_ATO_PROFILE_CONTROL_02_B_DIRECTION.bit0", "Driving direction (0 - forward, 1 - Reverse)", base.HEX, nil, 1 )
BF_ATO_PROFILE_CONTROL_02_B_DIRECTION_bit1 = ProtoField.uint8( "BF_ATO_PROFILE_CONTROL_02_B_DIRECTION.bit1", "Orientation in track (0 - as track, 1 - opposite)", base.HEX, nil, 2 )
BF_ATO_PROFILE_CONTROL_02_B_DIRECTION_bit2 = ProtoField.uint8( "BF_ATO_PROFILE_CONTROL_02_B_DIRECTION.bit2", "Locomotive orientation (0- B-end facing cars, 1 - A-end facing cars)", base.HEX, nil, 4 )

BF_ATO_PROFILE_CONTROL_02_B_DIRECTION = ProtoField.uint8( "BF_ATO_PROFILE_CONTROL_02_B_DIRECTION", "B_DIRECTION", base.HEX )
function BLOCK_DECODE_ATO_PROFILE_CONTROL(ctx)
    local btree = ctx.stack:top():add(fld_ATO_PROFILE_CONTROL,ctx.tvbuf:range(ctx.off,4));
    ctx.stack:push(btree);

    FIELD_DECODE_Q_PROFILE(ctx,BF_ATO_PROFILE_CONTROL_00_Q_PROFILE,{  })
    FIELD_DECODE_D_REVERSE(ctx,BF_ATO_PROFILE_CONTROL_01_D_REVERSE,{  })
    FIELD_DECODE_B_DIRECTION(ctx,BF_ATO_PROFILE_CONTROL_02_B_DIRECTION,{ BF_ATO_PROFILE_CONTROL_02_B_DIRECTION_bit0,BF_ATO_PROFILE_CONTROL_02_B_DIRECTION_bit1,BF_ATO_PROFILE_CONTROL_02_B_DIRECTION_bit2 })
    ctx.stack:pop(1);
end

-------------------------------
fld_CONFIGURATION_DATA = ProtoField.new("CONFIGURATION_DATA","CONFIGURATION_DATA",ftypes.NONE)


BF_CONFIGURATION_DATA_00_TID_TEXT_STRING = ProtoField.string( "BF_CONFIGURATION_DATA_00_TID_TEXT_STRING", "Parameter name", base.ASCII )

BF_CONFIGURATION_DATA_01_TID_TEXT_STRING = ProtoField.string( "BF_CONFIGURATION_DATA_01_TID_TEXT_STRING", "Value string", base.ASCII )
function BLOCK_DECODE_CONFIGURATION_DATA(ctx)
    local btree = ctx.stack:top():add(fld_CONFIGURATION_DATA,ctx.tvbuf:range(ctx.off,40));
    ctx.stack:push(btree);

    FIELD_DECODE_TID_TEXT_STRING(ctx,BF_CONFIGURATION_DATA_00_TID_TEXT_STRING,{  })
    FIELD_DECODE_TID_TEXT_STRING(ctx,BF_CONFIGURATION_DATA_01_TID_TEXT_STRING,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_CONFIRM_CONFIG = ProtoField.new("CONFIRM_CONFIG","CONFIRM_CONFIG",ftypes.NONE)


BF_CONFIRM_CONFIG_00_Q_SETUP = ProtoField.uint8( "BF_CONFIRM_CONFIG_00_Q_SETUP", "Q_SETUP", base.DEC  ,{ [0] = "Registration",[1] = "Reconfiguration",[2] = "Reregistration",[3] = "Reposition" } )
function BLOCK_DECODE_CONFIRM_CONFIG(ctx)
    local btree = ctx.stack:top():add(fld_CONFIRM_CONFIG,ctx.tvbuf:range(ctx.off,1));
    ctx.stack:push(btree);

    FIELD_DECODE_Q_SETUP(ctx,BF_CONFIRM_CONFIG_00_Q_SETUP,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_ATO_STOP_POSITION = ProtoField.new("ATO_STOP_POSITION","ATO_STOP_POSITION",ftypes.NONE)


BF_ATO_STOP_POSITION_00_NID_TRACK = ProtoField.uint16( "BF_ATO_STOP_POSITION_00_NID_TRACK", "NID_TRACK", base.DEC )

BF_ATO_STOP_POSITION_01_D_POSITION = ProtoField.int32( "BF_ATO_STOP_POSITION_01_D_POSITION", "D_POSITION", base.DEC )
function BLOCK_DECODE_ATO_STOP_POSITION(ctx)
    local btree = ctx.stack:top():add(fld_ATO_STOP_POSITION,ctx.tvbuf:range(ctx.off,6));
    ctx.stack:push(btree);

    FIELD_DECODE_NID_TRACK(ctx,BF_ATO_STOP_POSITION_00_NID_TRACK,{  })
    FIELD_DECODE_D_POSITION(ctx,BF_ATO_STOP_POSITION_01_D_POSITION,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_SAFE_FOR_BOARDING_ACTIVATE = ProtoField.new("SAFE_FOR_BOARDING_ACTIVATE","SAFE_FOR_BOARDING_ACTIVATE",ftypes.NONE)

function BLOCK_DECODE_SAFE_FOR_BOARDING_ACTIVATE(ctx)
    local btree = ctx.stack:top():add(fld_SAFE_FOR_BOARDING_ACTIVATE,ctx.tvbuf:range(ctx.off,0));
    ctx.stack:push(btree);

    ctx.stack:pop(1);
end

-------------------------------
fld_BALISE_DATA = ProtoField.new("BALISE_DATA","BALISE_DATA",ftypes.NONE)


BF_BALISE_DATA_00_NID_TRACK = ProtoField.uint16( "BF_BALISE_DATA_00_NID_TRACK", "NID_TRACK", base.DEC )

BF_BALISE_DATA_01_D_POSITION = ProtoField.int32( "BF_BALISE_DATA_01_D_POSITION", "D_POSITION", base.DEC )

BF_BALISE_DATA_02_NID_BG = ProtoField.uint16( "BF_BALISE_DATA_02_NID_BG", "NID_BG", base.DEC )
function BLOCK_DECODE_BALISE_DATA(ctx)
    local btree = ctx.stack:top():add(fld_BALISE_DATA,ctx.tvbuf:range(ctx.off,8));
    ctx.stack:push(btree);

    FIELD_DECODE_NID_TRACK(ctx,BF_BALISE_DATA_00_NID_TRACK,{  })
    FIELD_DECODE_D_POSITION(ctx,BF_BALISE_DATA_01_D_POSITION,{  })
    FIELD_DECODE_NID_BG(ctx,BF_BALISE_DATA_02_NID_BG,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_LOCATION_DATA = ProtoField.new("LOCATION_DATA","LOCATION_DATA",ftypes.NONE)


BF_LOCATION_DATA_00_TID_LOCATION = ProtoField.string( "BF_LOCATION_DATA_00_TID_LOCATION", "TID_LOCATION", base.ASCII )

BF_LOCATION_DATA_01_NID_LOCATION_TYPE = ProtoField.uint8( "BF_LOCATION_DATA_01_NID_LOCATION_TYPE", "NID_LOCATION_TYPE", base.DEC )
function BLOCK_DECODE_LOCATION_DATA(ctx)
    local btree = ctx.stack:top():add(fld_LOCATION_DATA,ctx.tvbuf:range(ctx.off,21));
    ctx.stack:push(btree);

    FIELD_DECODE_TID_LOCATION(ctx,BF_LOCATION_DATA_00_TID_LOCATION,{  })
    FIELD_DECODE_NID_LOCATION_TYPE(ctx,BF_LOCATION_DATA_01_NID_LOCATION_TYPE,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_AREAS = ProtoField.new("AREAS","AREAS",ftypes.NONE)


BF_AREAS_00_NID_AREA = ProtoField.uint8( "BF_AREAS_00_NID_AREA", "NID_AREA", base.DEC )
function BLOCK_DECODE_AREAS(ctx)
    local btree = ctx.stack:top():add(fld_AREAS,ctx.tvbuf:range(ctx.off,1));
    ctx.stack:push(btree);

    FIELD_DECODE_NID_AREA(ctx,BF_AREAS_00_NID_AREA,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_CAR2_CONFIG_DATA = ProtoField.new("CAR2_CONFIG_DATA","CAR2_CONFIG_DATA",ftypes.NONE)


BF_CAR2_CONFIG_DATA_00_NID_VEHICLE = ProtoField.uint16( "BF_CAR2_CONFIG_DATA_00_NID_VEHICLE", "NID_VEHICLE", base.DEC )

BF_CAR2_CONFIG_DATA_01_Q_PRESSURE_SENSOR = ProtoField.uint8( "BF_CAR2_CONFIG_DATA_01_Q_PRESSURE_SENSOR", "Q_PRESSURE_SENSOR", base.DEC  ,{ [0] = "Pressure sensor not available",[1] = "Pressure sensor available" } )
function BLOCK_DECODE_CAR2_CONFIG_DATA(ctx)
    local btree = ctx.stack:top():add(fld_CAR2_CONFIG_DATA,ctx.tvbuf:range(ctx.off,3));
    ctx.stack:push(btree);

    FIELD_DECODE_NID_VEHICLE(ctx,BF_CAR2_CONFIG_DATA_00_NID_VEHICLE,{  })
    FIELD_DECODE_Q_PRESSURE_SENSOR(ctx,BF_CAR2_CONFIG_DATA_01_Q_PRESSURE_SENSOR,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_CAR3_CONFIG_DATA = ProtoField.new("CAR3_CONFIG_DATA","CAR3_CONFIG_DATA",ftypes.NONE)


BF_CAR3_CONFIG_DATA_00_NID_VEHICLE = ProtoField.uint16( "BF_CAR3_CONFIG_DATA_00_NID_VEHICLE", "NID_VEHICLE", base.DEC )

BF_CAR3_CONFIG_DATA_01_Q_PRESSURE_SENSOR = ProtoField.uint8( "BF_CAR3_CONFIG_DATA_01_Q_PRESSURE_SENSOR", "Q_PRESSURE_SENSOR", base.DEC  ,{ [0] = "Pressure sensor not available",[1] = "Pressure sensor available" } )

BF_CAR3_CONFIG_DATA_02_T_UNLOAD_ACTION = ProtoField.uint8( "BF_CAR3_CONFIG_DATA_02_T_UNLOAD_ACTION", "T_UNLOAD_ACTION", base.DEC )

BF_CAR3_CONFIG_DATA_03_T_UNLOAD_HATCH_OPEN = ProtoField.uint8( "BF_CAR3_CONFIG_DATA_03_T_UNLOAD_HATCH_OPEN", "T_UNLOAD_HATCH_OPEN", base.DEC )
function BLOCK_DECODE_CAR3_CONFIG_DATA(ctx)
    local btree = ctx.stack:top():add(fld_CAR3_CONFIG_DATA,ctx.tvbuf:range(ctx.off,5));
    ctx.stack:push(btree);

    FIELD_DECODE_NID_VEHICLE(ctx,BF_CAR3_CONFIG_DATA_00_NID_VEHICLE,{  })
    FIELD_DECODE_Q_PRESSURE_SENSOR(ctx,BF_CAR3_CONFIG_DATA_01_Q_PRESSURE_SENSOR,{  })
    FIELD_DECODE_T_UNLOAD_ACTION(ctx,BF_CAR3_CONFIG_DATA_02_T_UNLOAD_ACTION,{  })
    FIELD_DECODE_T_UNLOAD_HATCH_OPEN(ctx,BF_CAR3_CONFIG_DATA_03_T_UNLOAD_HATCH_OPEN,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_CAR_STATUS_DATA = ProtoField.new("CAR_STATUS_DATA","CAR_STATUS_DATA",ftypes.NONE)


BF_CAR_STATUS_DATA_00_NID_VEHICLE = ProtoField.uint16( "BF_CAR_STATUS_DATA_00_NID_VEHICLE", "NID_VEHICLE", base.DEC )
BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit0 = ProtoField.uint16( "BF_CAR_STATUS_DATA_01_B_TIC_STATUS.bit0", "Derail, front right", base.HEX, nil, 1 )
BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit1 = ProtoField.uint16( "BF_CAR_STATUS_DATA_01_B_TIC_STATUS.bit1", "Derail, front left", base.HEX, nil, 2 )
BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit2 = ProtoField.uint16( "BF_CAR_STATUS_DATA_01_B_TIC_STATUS.bit2", "Derail, rear right", base.HEX, nil, 4 )
BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit3 = ProtoField.uint16( "BF_CAR_STATUS_DATA_01_B_TIC_STATUS.bit3", "Derail, rear left", base.HEX, nil, 8 )
BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit4 = ProtoField.uint16( "BF_CAR_STATUS_DATA_01_B_TIC_STATUS.bit4", "Car dump bottom", base.HEX, nil, 16 )
BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit5 = ProtoField.uint16( "BF_CAR_STATUS_DATA_01_B_TIC_STATUS.bit5", "Car dump top", base.HEX, nil, 32 )
BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit6 = ProtoField.uint16( "BF_CAR_STATUS_DATA_01_B_TIC_STATUS.bit6", "Car dump closed", base.HEX, nil, 64 )
BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit7 = ProtoField.uint16( "BF_CAR_STATUS_DATA_01_B_TIC_STATUS.bit7", "Load weight bad", base.HEX, nil, 128 )
BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit8 = ProtoField.uint16( "BF_CAR_STATUS_DATA_01_B_TIC_STATUS.bit8", "Train config input", base.HEX, nil, 256 )

BF_CAR_STATUS_DATA_01_B_TIC_STATUS = ProtoField.uint16( "BF_CAR_STATUS_DATA_01_B_TIC_STATUS", "B_TIC_STATUS", base.HEX )
function BLOCK_DECODE_CAR_STATUS_DATA(ctx)
    local btree = ctx.stack:top():add(fld_CAR_STATUS_DATA,ctx.tvbuf:range(ctx.off,4));
    ctx.stack:push(btree);

    FIELD_DECODE_NID_VEHICLE(ctx,BF_CAR_STATUS_DATA_00_NID_VEHICLE,{  })
    FIELD_DECODE_B_TIC_STATUS(ctx,BF_CAR_STATUS_DATA_01_B_TIC_STATUS,{ BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit0,BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit1,BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit2,BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit3,BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit4,BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit5,BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit6,BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit7,BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit8 })
    ctx.stack:pop(1);
end

-------------------------------
fld_CEILING_SPEED_DATA = ProtoField.new("CEILING_SPEED_DATA","CEILING_SPEED_DATA",ftypes.NONE)


BF_CEILING_SPEED_DATA_00_NID_TRACK = ProtoField.uint16( "BF_CEILING_SPEED_DATA_00_NID_TRACK", "NID_TRACK", base.DEC )

BF_CEILING_SPEED_DATA_01_D_POSITION = ProtoField.int32( "BF_CEILING_SPEED_DATA_01_D_POSITION", "D_POSITION", base.DEC )

BF_CEILING_SPEED_DATA_02_V_SPEED = ProtoField.uint16( "BF_CEILING_SPEED_DATA_02_V_SPEED", "V_SPEED", base.DEC )

BF_CEILING_SPEED_DATA_03_Q_TRAIN_END = ProtoField.uint8( "BF_CEILING_SPEED_DATA_03_Q_TRAIN_END", "Q_TRAIN_END", base.DEC  ,{ [0] = "Rear of train (train length delay)",[1] = "Front of train" } )

BF_CEILING_SPEED_DATA_04_Q_SPEED = ProtoField.uint8( "BF_CEILING_SPEED_DATA_04_Q_SPEED", "Q_SPEED", base.DEC  ,{ [0] = "Undefined",[1] = "Points straight",[2] = "Points curve",[3] = "Points passed",[4] = "Location",[5] = "Temporary speed restriction",[255] = "Other" } )
function BLOCK_DECODE_CEILING_SPEED_DATA(ctx)
    local btree = ctx.stack:top():add(fld_CEILING_SPEED_DATA,ctx.tvbuf:range(ctx.off,10));
    ctx.stack:push(btree);

    FIELD_DECODE_NID_TRACK(ctx,BF_CEILING_SPEED_DATA_00_NID_TRACK,{  })
    FIELD_DECODE_D_POSITION(ctx,BF_CEILING_SPEED_DATA_01_D_POSITION,{  })
    FIELD_DECODE_V_SPEED(ctx,BF_CEILING_SPEED_DATA_02_V_SPEED,{  })
    FIELD_DECODE_Q_TRAIN_END(ctx,BF_CEILING_SPEED_DATA_03_Q_TRAIN_END,{  })
    FIELD_DECODE_Q_SPEED(ctx,BF_CEILING_SPEED_DATA_04_Q_SPEED,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_CONFIG_CONFIRMATION = ProtoField.new("CONFIG_CONFIRMATION","CONFIG_CONFIRMATION",ftypes.NONE)


BF_CONFIG_CONFIRMATION_00_Q_ACKNOWLEDGE = ProtoField.uint8( "BF_CONFIG_CONFIRMATION_00_Q_ACKNOWLEDGE", "Q_ACKNOWLEDGE", base.DEC  ,{ [0] = "Request not acknowledged",[1] = "Request acknowledged" } )
function BLOCK_DECODE_CONFIG_CONFIRMATION(ctx)
    local btree = ctx.stack:top():add(fld_CONFIG_CONFIRMATION,ctx.tvbuf:range(ctx.off,1));
    ctx.stack:push(btree);

    FIELD_DECODE_Q_ACKNOWLEDGE(ctx,BF_CONFIG_CONFIRMATION_00_Q_ACKNOWLEDGE,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_ERROR_MESSAGE_DATA = ProtoField.new("ERROR_MESSAGE_DATA","ERROR_MESSAGE_DATA",ftypes.NONE)


BF_ERROR_MESSAGE_DATA_00_Q_ERROR_LEVEL = ProtoField.uint8( "BF_ERROR_MESSAGE_DATA_00_Q_ERROR_LEVEL", "Q_ERROR_LEVEL", base.DEC  ,{ [1] = "Fatal",[2] = "Minor",[3] = "Log" } )

BF_ERROR_MESSAGE_DATA_01_NID_ERROR_NO = ProtoField.uint32( "BF_ERROR_MESSAGE_DATA_01_NID_ERROR_NO", "NID_ERROR_NO", base.DEC )
function BLOCK_DECODE_ERROR_MESSAGE_DATA(ctx)
    local btree = ctx.stack:top():add(fld_ERROR_MESSAGE_DATA,ctx.tvbuf:range(ctx.off,5));
    ctx.stack:push(btree);

    FIELD_DECODE_Q_ERROR_LEVEL(ctx,BF_ERROR_MESSAGE_DATA_00_Q_ERROR_LEVEL,{  })
    FIELD_DECODE_NID_ERROR_NO(ctx,BF_ERROR_MESSAGE_DATA_01_NID_ERROR_NO,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_ETA_CONFIRMATION = ProtoField.new("ETA_CONFIRMATION","ETA_CONFIRMATION",ftypes.NONE)


BF_ETA_CONFIRMATION_00_Q_ACKNOWLEDGE = ProtoField.uint8( "BF_ETA_CONFIRMATION_00_Q_ACKNOWLEDGE", "Q_ACKNOWLEDGE", base.DEC  ,{ [0] = "Request not acknowledged",[1] = "Request acknowledged" } )

BF_ETA_CONFIRMATION_01_T_CLOCK = ProtoField.uint32( "BF_ETA_CONFIRMATION_01_T_CLOCK", "T_CLOCK", base.DEC )
function BLOCK_DECODE_ETA_CONFIRMATION(ctx)
    local btree = ctx.stack:top():add(fld_ETA_CONFIRMATION,ctx.tvbuf:range(ctx.off,5));
    ctx.stack:push(btree);

    FIELD_DECODE_Q_ACKNOWLEDGE(ctx,BF_ETA_CONFIRMATION_00_Q_ACKNOWLEDGE,{  })
    FIELD_DECODE_T_CLOCK(ctx,BF_ETA_CONFIRMATION_01_T_CLOCK,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_EVENT_DATA = ProtoField.new("EVENT_DATA","EVENT_DATA",ftypes.NONE)


BF_EVENT_DATA_00_NID_EVENT_NO = ProtoField.uint32( "BF_EVENT_DATA_00_NID_EVENT_NO", "NID_EVENT_NO", base.DEC )
function BLOCK_DECODE_EVENT_DATA(ctx)
    local btree = ctx.stack:top():add(fld_EVENT_DATA,ctx.tvbuf:range(ctx.off,4));
    ctx.stack:push(btree);

    FIELD_DECODE_NID_EVENT_NO(ctx,BF_EVENT_DATA_00_NID_EVENT_NO,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_ETA_REQUEST = ProtoField.new("ETA_REQUEST","ETA_REQUEST",ftypes.NONE)


BF_ETA_REQUEST_00_T_CLOCK = ProtoField.uint32( "BF_ETA_REQUEST_00_T_CLOCK", "T_CLOCK", base.DEC )
function BLOCK_DECODE_ETA_REQUEST(ctx)
    local btree = ctx.stack:top():add(fld_ETA_REQUEST,ctx.tvbuf:range(ctx.off,4));
    ctx.stack:push(btree);

    FIELD_DECODE_T_CLOCK(ctx,BF_ETA_REQUEST_00_T_CLOCK,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_EVENT_DATA_TEXT = ProtoField.new("EVENT_DATA_TEXT","EVENT_DATA_TEXT",ftypes.NONE)


BF_EVENT_DATA_TEXT_00_NID_EVENT_NO = ProtoField.uint32( "BF_EVENT_DATA_TEXT_00_NID_EVENT_NO", "NID_EVENT_NO", base.DEC )

BF_EVENT_DATA_TEXT_01_TID_TEXT_STRING = ProtoField.string( "BF_EVENT_DATA_TEXT_01_TID_TEXT_STRING", "TID_TEXT_STRING", base.ASCII )
function BLOCK_DECODE_EVENT_DATA_TEXT(ctx)
    local btree = ctx.stack:top():add(fld_EVENT_DATA_TEXT,ctx.tvbuf:range(ctx.off,24));
    ctx.stack:push(btree);

    FIELD_DECODE_NID_EVENT_NO(ctx,BF_EVENT_DATA_TEXT_00_NID_EVENT_NO,{  })
    FIELD_DECODE_TID_TEXT_STRING(ctx,BF_EVENT_DATA_TEXT_01_TID_TEXT_STRING,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_EXTERNAL_DATA = ProtoField.new("EXTERNAL_DATA","EXTERNAL_DATA",ftypes.NONE)


BF_EXTERNAL_DATA_00_NID_SYSTEM = ProtoField.uint8( "BF_EXTERNAL_DATA_00_NID_SYSTEM", "NID_SYSTEM", base.DEC )

BF_EXTERNAL_DATA_01_N_LENGTH = ProtoField.uint16( "BF_EXTERNAL_DATA_01_N_LENGTH", "N_LENGTH", base.DEC )
function BLOCK_DECODE_EXTERNAL_DATA(ctx)
    local btree = ctx.stack:top():add(fld_EXTERNAL_DATA,ctx.tvbuf:range(ctx.off,3));
    ctx.stack:push(btree);

    FIELD_DECODE_NID_SYSTEM(ctx,BF_EXTERNAL_DATA_00_NID_SYSTEM,{  })
    FIELD_DECODE_N_LENGTH(ctx,BF_EXTERNAL_DATA_01_N_LENGTH,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_GRADIENT_DATA = ProtoField.new("GRADIENT_DATA","GRADIENT_DATA",ftypes.NONE)


BF_GRADIENT_DATA_00_NID_TRACK = ProtoField.uint16( "BF_GRADIENT_DATA_00_NID_TRACK", "NID_TRACK", base.DEC )

BF_GRADIENT_DATA_01_D_POSITION = ProtoField.int32( "BF_GRADIENT_DATA_01_D_POSITION", "D_POSITION", base.DEC )

BF_GRADIENT_DATA_02_G_GRADIENT = ProtoField.int8( "BF_GRADIENT_DATA_02_G_GRADIENT", "G_GRADIENT", base.DEC )
function BLOCK_DECODE_GRADIENT_DATA(ctx)
    local btree = ctx.stack:top():add(fld_GRADIENT_DATA,ctx.tvbuf:range(ctx.off,7));
    ctx.stack:push(btree);

    FIELD_DECODE_NID_TRACK(ctx,BF_GRADIENT_DATA_00_NID_TRACK,{  })
    FIELD_DECODE_D_POSITION(ctx,BF_GRADIENT_DATA_01_D_POSITION,{  })
    FIELD_DECODE_G_GRADIENT(ctx,BF_GRADIENT_DATA_02_G_GRADIENT,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_KEEP_TRACK_DATA = ProtoField.new("KEEP_TRACK_DATA","KEEP_TRACK_DATA",ftypes.NONE)


BF_KEEP_TRACK_DATA_00_NID_TRACK = ProtoField.uint16( "BF_KEEP_TRACK_DATA_00_NID_TRACK", "NID_TRACK", base.DEC )

BF_KEEP_TRACK_DATA_01_D_POSITION = ProtoField.int32( "BF_KEEP_TRACK_DATA_01_D_POSITION", "D_POSITION", base.DEC )
function BLOCK_DECODE_KEEP_TRACK_DATA(ctx)
    local btree = ctx.stack:top():add(fld_KEEP_TRACK_DATA,ctx.tvbuf:range(ctx.off,6));
    ctx.stack:push(btree);

    FIELD_DECODE_NID_TRACK(ctx,BF_KEEP_TRACK_DATA_00_NID_TRACK,{  })
    FIELD_DECODE_D_POSITION(ctx,BF_KEEP_TRACK_DATA_01_D_POSITION,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_LAST_BALISE = ProtoField.new("LAST_BALISE","LAST_BALISE",ftypes.NONE)


BF_LAST_BALISE_00_NID_BG = ProtoField.uint16( "BF_LAST_BALISE_00_NID_BG", "NID_BG", base.DEC )
function BLOCK_DECODE_LAST_BALISE(ctx)
    local btree = ctx.stack:top():add(fld_LAST_BALISE,ctx.tvbuf:range(ctx.off,2));
    ctx.stack:push(btree);

    FIELD_DECODE_NID_BG(ctx,BF_LAST_BALISE_00_NID_BG,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_LOAD_FINISHED = ProtoField.new("LOAD_FINISHED","LOAD_FINISHED",ftypes.NONE)

function BLOCK_DECODE_LOAD_FINISHED(ctx)
    local btree = ctx.stack:top():add(fld_LOAD_FINISHED,ctx.tvbuf:range(ctx.off,0));
    ctx.stack:push(btree);

    ctx.stack:pop(1);
end

-------------------------------
fld_LOCOMOTIVE_POWER = ProtoField.new("LOCOMOTIVE_POWER","LOCOMOTIVE_POWER",ftypes.NONE)


BF_LOCOMOTIVE_POWER_00_Q_POWER = ProtoField.uint8( "BF_LOCOMOTIVE_POWER_00_Q_POWER", "Q_POWER", base.DEC  ,{ [0] = "Power down",[1] = "Power up" } )
function BLOCK_DECODE_LOCOMOTIVE_POWER(ctx)
    local btree = ctx.stack:top():add(fld_LOCOMOTIVE_POWER,ctx.tvbuf:range(ctx.off,1));
    ctx.stack:push(btree);

    FIELD_DECODE_Q_POWER(ctx,BF_LOCOMOTIVE_POWER_00_Q_POWER,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_MAX_SEARCH_DIST = ProtoField.new("MAX_SEARCH_DIST","MAX_SEARCH_DIST",ftypes.NONE)


BF_MAX_SEARCH_DIST_00_D_MAX_DIST = ProtoField.int24( "BF_MAX_SEARCH_DIST_00_D_MAX_DIST", "D_MAX_DIST", base.DEC )
function BLOCK_DECODE_MAX_SEARCH_DIST(ctx)
    local btree = ctx.stack:top():add(fld_MAX_SEARCH_DIST,ctx.tvbuf:range(ctx.off,3));
    ctx.stack:push(btree);

    FIELD_DECODE_D_MAX_DIST(ctx,BF_MAX_SEARCH_DIST_00_D_MAX_DIST,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_PANTOGRAPH_SHIFT = ProtoField.new("PANTOGRAPH_SHIFT","PANTOGRAPH_SHIFT",ftypes.NONE)


BF_PANTOGRAPH_SHIFT_00_NID_TRACK = ProtoField.uint16( "BF_PANTOGRAPH_SHIFT_00_NID_TRACK", "NID_TRACK", base.DEC )

BF_PANTOGRAPH_SHIFT_01_D_POSITION = ProtoField.int32( "BF_PANTOGRAPH_SHIFT_01_D_POSITION", "D_POSITION", base.DEC )

BF_PANTOGRAPH_SHIFT_02_Q_PANTO_POSITION = ProtoField.uint8( "BF_PANTOGRAPH_SHIFT_02_Q_PANTO_POSITION", "To Leg1 Position", base.DEC  ,{ [0] = "None, all pantographs shall be down/in",[1] = "Centre, centre pantograph shall be up",[2] = "Side, side pantograph shall be out" } )

BF_PANTOGRAPH_SHIFT_03_Q_SUPERVISION = ProtoField.uint8( "BF_PANTOGRAPH_SHIFT_03_Q_SUPERVISION", "To Leg1 Supervision", base.DEC  ,{ [0] = "No supervision",[1] = "Supervise with brake curve",[2] = "Supervise without brake curve, emergency brake" } )

BF_PANTOGRAPH_SHIFT_04_Q_PANTO_POSITION = ProtoField.uint8( "BF_PANTOGRAPH_SHIFT_04_Q_PANTO_POSITION", "To Leg0 Position", base.DEC  ,{ [0] = "None, all pantographs shall be down/in",[1] = "Centre, centre pantograph shall be up",[2] = "Side, side pantograph shall be out" } )

BF_PANTOGRAPH_SHIFT_05_Q_SUPERVISION = ProtoField.uint8( "BF_PANTOGRAPH_SHIFT_05_Q_SUPERVISION", "To Leg0 Supervision", base.DEC  ,{ [0] = "No supervision",[1] = "Supervise with brake curve",[2] = "Supervise without brake curve, emergency brake" } )
function BLOCK_DECODE_PANTOGRAPH_SHIFT(ctx)
    local btree = ctx.stack:top():add(fld_PANTOGRAPH_SHIFT,ctx.tvbuf:range(ctx.off,10));
    ctx.stack:push(btree);

    FIELD_DECODE_NID_TRACK(ctx,BF_PANTOGRAPH_SHIFT_00_NID_TRACK,{  })
    FIELD_DECODE_D_POSITION(ctx,BF_PANTOGRAPH_SHIFT_01_D_POSITION,{  })
    FIELD_DECODE_Q_PANTO_POSITION(ctx,BF_PANTOGRAPH_SHIFT_02_Q_PANTO_POSITION,{  })
    FIELD_DECODE_Q_SUPERVISION(ctx,BF_PANTOGRAPH_SHIFT_03_Q_SUPERVISION,{  })
    FIELD_DECODE_Q_PANTO_POSITION(ctx,BF_PANTOGRAPH_SHIFT_04_Q_PANTO_POSITION,{  })
    FIELD_DECODE_Q_SUPERVISION(ctx,BF_PANTOGRAPH_SHIFT_05_Q_SUPERVISION,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_PANTO_START_POSITION = ProtoField.new("PANTO_START_POSITION","PANTO_START_POSITION",ftypes.NONE)


BF_PANTO_START_POSITION_00_Q_PANTO_POSITION = ProtoField.uint8( "BF_PANTO_START_POSITION_00_Q_PANTO_POSITION", "Q_PANTO_POSITION", base.DEC  ,{ [0] = "None, all pantographs shall be down/in",[1] = "Centre, centre pantograph shall be up",[2] = "Side, side pantograph shall be out" } )
function BLOCK_DECODE_PANTO_START_POSITION(ctx)
    local btree = ctx.stack:top():add(fld_PANTO_START_POSITION,ctx.tvbuf:range(ctx.off,1));
    ctx.stack:push(btree);

    FIELD_DECODE_Q_PANTO_POSITION(ctx,BF_PANTO_START_POSITION_00_Q_PANTO_POSITION,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_VEHICLE_TYPE_DATA = ProtoField.new("VEHICLE_TYPE_DATA","VEHICLE_TYPE_DATA",ftypes.NONE)


BF_VEHICLE_TYPE_DATA_00_NID_VEHICLE_TYPE = ProtoField.uint8( "BF_VEHICLE_TYPE_DATA_00_NID_VEHICLE_TYPE", "NID_VEHICLE_TYPE", base.DEC )

BF_VEHICLE_TYPE_DATA_01_W_WEIGHT = ProtoField.uint16( "BF_VEHICLE_TYPE_DATA_01_W_WEIGHT", "W_WEIGHT", base.DEC )

BF_VEHICLE_TYPE_DATA_02_W_WEIGHT = ProtoField.uint16( "BF_VEHICLE_TYPE_DATA_02_W_WEIGHT", "W_WEIGHT", base.DEC )

BF_VEHICLE_TYPE_DATA_03_W_WEIGHT = ProtoField.uint16( "BF_VEHICLE_TYPE_DATA_03_W_WEIGHT", "W_WEIGHT", base.DEC )

BF_VEHICLE_TYPE_DATA_04_W_WEIGHT = ProtoField.uint16( "BF_VEHICLE_TYPE_DATA_04_W_WEIGHT", "W_WEIGHT", base.DEC )

BF_VEHICLE_TYPE_DATA_05_W_WEIGHT = ProtoField.uint16( "BF_VEHICLE_TYPE_DATA_05_W_WEIGHT", "W_WEIGHT", base.DEC )

BF_VEHICLE_TYPE_DATA_06_W_WEIGHT = ProtoField.uint16( "BF_VEHICLE_TYPE_DATA_06_W_WEIGHT", "W_WEIGHT", base.DEC )
function BLOCK_DECODE_VEHICLE_TYPE_DATA(ctx)
    local btree = ctx.stack:top():add(fld_VEHICLE_TYPE_DATA,ctx.tvbuf:range(ctx.off,13));
    ctx.stack:push(btree);

    FIELD_DECODE_NID_VEHICLE_TYPE(ctx,BF_VEHICLE_TYPE_DATA_00_NID_VEHICLE_TYPE,{  })
    FIELD_DECODE_W_WEIGHT(ctx,BF_VEHICLE_TYPE_DATA_01_W_WEIGHT,{  })
    FIELD_DECODE_W_WEIGHT(ctx,BF_VEHICLE_TYPE_DATA_02_W_WEIGHT,{  })
    FIELD_DECODE_W_WEIGHT(ctx,BF_VEHICLE_TYPE_DATA_03_W_WEIGHT,{  })
    FIELD_DECODE_W_WEIGHT(ctx,BF_VEHICLE_TYPE_DATA_04_W_WEIGHT,{  })
    FIELD_DECODE_W_WEIGHT(ctx,BF_VEHICLE_TYPE_DATA_05_W_WEIGHT,{  })
    FIELD_DECODE_W_WEIGHT(ctx,BF_VEHICLE_TYPE_DATA_06_W_WEIGHT,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_PARTLY_MA = ProtoField.new("PARTLY_MA","PARTLY_MA",ftypes.NONE)

function BLOCK_DECODE_PARTLY_MA(ctx)
    local btree = ctx.stack:top():add(fld_PARTLY_MA,ctx.tvbuf:range(ctx.off,0));
    ctx.stack:push(btree);

    ctx.stack:pop(1);
end

-------------------------------
fld_POSSESSION_REQUEST = ProtoField.new("POSSESSION_REQUEST","POSSESSION_REQUEST",ftypes.NONE)

function BLOCK_DECODE_POSSESSION_REQUEST(ctx)
    local btree = ctx.stack:top():add(fld_POSSESSION_REQUEST,ctx.tvbuf:range(ctx.off,0));
    ctx.stack:push(btree);

    ctx.stack:pop(1);
end

-------------------------------
fld_PROTOCOL_VERSION = ProtoField.new("PROTOCOL_VERSION","PROTOCOL_VERSION",ftypes.NONE)


BF_PROTOCOL_VERSION_00_M_VERSION = ProtoField.uint8( "BF_PROTOCOL_VERSION_00_M_VERSION", "Major Version", base.DEC )

BF_PROTOCOL_VERSION_01_M_VERSION = ProtoField.uint8( "BF_PROTOCOL_VERSION_01_M_VERSION", "Minor Version", base.DEC )

BF_PROTOCOL_VERSION_02_M_VERSION = ProtoField.uint8( "BF_PROTOCOL_VERSION_02_M_VERSION", "Sub version", base.DEC )
function BLOCK_DECODE_PROTOCOL_VERSION(ctx)
    local btree = ctx.stack:top():add(fld_PROTOCOL_VERSION,ctx.tvbuf:range(ctx.off,3));
    ctx.stack:push(btree);

    FIELD_DECODE_M_VERSION(ctx,BF_PROTOCOL_VERSION_00_M_VERSION,{  })
    FIELD_DECODE_M_VERSION(ctx,BF_PROTOCOL_VERSION_01_M_VERSION,{  })
    FIELD_DECODE_M_VERSION(ctx,BF_PROTOCOL_VERSION_02_M_VERSION,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_SPEED_CHANGE_POSITION = ProtoField.new("SPEED_CHANGE_POSITION","SPEED_CHANGE_POSITION",ftypes.NONE)


BF_SPEED_CHANGE_POSITION_00_NID_TRACK = ProtoField.uint16( "BF_SPEED_CHANGE_POSITION_00_NID_TRACK", "NID_TRACK", base.DEC )

BF_SPEED_CHANGE_POSITION_01_D_POSITION = ProtoField.int32( "BF_SPEED_CHANGE_POSITION_01_D_POSITION", "D_POSITION", base.DEC )

BF_SPEED_CHANGE_POSITION_02_V_SPEED = ProtoField.uint16( "BF_SPEED_CHANGE_POSITION_02_V_SPEED", "V_SPEED", base.DEC )
function BLOCK_DECODE_SPEED_CHANGE_POSITION(ctx)
    local btree = ctx.stack:top():add(fld_SPEED_CHANGE_POSITION,ctx.tvbuf:range(ctx.off,8));
    ctx.stack:push(btree);

    FIELD_DECODE_NID_TRACK(ctx,BF_SPEED_CHANGE_POSITION_00_NID_TRACK,{  })
    FIELD_DECODE_D_POSITION(ctx,BF_SPEED_CHANGE_POSITION_01_D_POSITION,{  })
    FIELD_DECODE_V_SPEED(ctx,BF_SPEED_CHANGE_POSITION_02_V_SPEED,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_RELEASE_BRAKE = ProtoField.new("RELEASE_BRAKE","RELEASE_BRAKE",ftypes.NONE)

function BLOCK_DECODE_RELEASE_BRAKE(ctx)
    local btree = ctx.stack:top():add(fld_RELEASE_BRAKE,ctx.tvbuf:range(ctx.off,0));
    ctx.stack:push(btree);

    ctx.stack:pop(1);
end

-------------------------------
fld_REQ_CAR_STATUS = ProtoField.new("REQ_CAR_STATUS","REQ_CAR_STATUS",ftypes.NONE)

function BLOCK_DECODE_REQ_CAR_STATUS(ctx)
    local btree = ctx.stack:top():add(fld_REQ_CAR_STATUS,ctx.tvbuf:range(ctx.off,0));
    ctx.stack:push(btree);

    ctx.stack:pop(1);
end

-------------------------------
fld_SET_TIME = ProtoField.new("SET_TIME","SET_TIME",ftypes.NONE)


BF_SET_TIME_00_T_CLOCK = ProtoField.uint32( "BF_SET_TIME_00_T_CLOCK", "T_CLOCK", base.DEC )
function BLOCK_DECODE_SET_TIME(ctx)
    local btree = ctx.stack:top():add(fld_SET_TIME,ctx.tvbuf:range(ctx.off,4));
    ctx.stack:push(btree);

    FIELD_DECODE_T_CLOCK(ctx,BF_SET_TIME_00_T_CLOCK,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_SHUNTING_REQUEST = ProtoField.new("SHUNTING_REQUEST","SHUNTING_REQUEST",ftypes.NONE)

function BLOCK_DECODE_SHUNTING_REQUEST(ctx)
    local btree = ctx.stack:top():add(fld_SHUNTING_REQUEST,ctx.tvbuf:range(ctx.off,0));
    ctx.stack:push(btree);

    ctx.stack:pop(1);
end

-------------------------------
fld_TEXT_MESSAGE = ProtoField.new("TEXT_MESSAGE","TEXT_MESSAGE",ftypes.NONE)


BF_TEXT_MESSAGE_00_M_TEXT = ProtoField.string( "BF_TEXT_MESSAGE_00_M_TEXT", "M_TEXT", base.ASCII )
function BLOCK_DECODE_TEXT_MESSAGE(ctx)
    local btree = ctx.stack:top():add(fld_TEXT_MESSAGE,ctx.tvbuf:range(ctx.off,99));
    ctx.stack:push(btree);

    FIELD_DECODE_M_TEXT(ctx,BF_TEXT_MESSAGE_00_M_TEXT,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_TRACKS = ProtoField.new("TRACKS","TRACKS",ftypes.NONE)


BF_TRACKS_00_NID_TRACK = ProtoField.uint16( "BF_TRACKS_00_NID_TRACK", "NID_TRACK", base.DEC )
function BLOCK_DECODE_TRACKS(ctx)
    local btree = ctx.stack:top():add(fld_TRACKS,ctx.tvbuf:range(ctx.off,2));
    ctx.stack:push(btree);

    FIELD_DECODE_NID_TRACK(ctx,BF_TRACKS_00_NID_TRACK,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_TRACK_DATA_ITEM = ProtoField.new("TRACK_DATA_ITEM","TRACK_DATA_ITEM",ftypes.NONE)


BF_TRACK_DATA_ITEM_00_Q_TRACK_DATA_TYPE = ProtoField.uint8( "BF_TRACK_DATA_ITEM_00_Q_TRACK_DATA_TYPE", "Q_TRACK_DATA_TYPE", base.DEC )

BF_TRACK_DATA_ITEM_01_NID_TRACK = ProtoField.uint16( "BF_TRACK_DATA_ITEM_01_NID_TRACK", "NID_TRACK", base.DEC )

BF_TRACK_DATA_ITEM_02_D_POSITION = ProtoField.int32( "BF_TRACK_DATA_ITEM_02_D_POSITION", "D_POSITION", base.DEC )

BF_TRACK_DATA_ITEM_03_Q_DIRECTION = ProtoField.uint8( "BF_TRACK_DATA_ITEM_03_Q_DIRECTION", "Q_DIRECTION", base.DEC  ,{ [0] = "Undefined",[1] = "Forward",[2] = "Reverse",[3] = "Both" } )

BF_TRACK_DATA_ITEM_04_N_VALUE = ProtoField.uint16( "BF_TRACK_DATA_ITEM_04_N_VALUE", "N_VALUE", base.DEC )
function BLOCK_DECODE_TRACK_DATA_ITEM(ctx)
    local btree = ctx.stack:top():add(fld_TRACK_DATA_ITEM,ctx.tvbuf:range(ctx.off,10));
    ctx.stack:push(btree);

    FIELD_DECODE_Q_TRACK_DATA_TYPE(ctx,BF_TRACK_DATA_ITEM_00_Q_TRACK_DATA_TYPE,{  })
    FIELD_DECODE_NID_TRACK(ctx,BF_TRACK_DATA_ITEM_01_NID_TRACK,{  })
    FIELD_DECODE_D_POSITION(ctx,BF_TRACK_DATA_ITEM_02_D_POSITION,{  })
    FIELD_DECODE_Q_DIRECTION(ctx,BF_TRACK_DATA_ITEM_03_Q_DIRECTION,{  })
    FIELD_DECODE_N_VALUE(ctx,BF_TRACK_DATA_ITEM_04_N_VALUE,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_TRACK_DATA = ProtoField.new("TRACK_DATA","TRACK_DATA",ftypes.NONE)


BF_TRACK_DATA_00_NID_TRACK = ProtoField.uint16( "BF_TRACK_DATA_00_NID_TRACK", "NID_TRACK", base.DEC )

BF_TRACK_DATA_01_D_POSITION = ProtoField.int32( "BF_TRACK_DATA_01_D_POSITION", "D_POSITION", base.DEC )

BF_TRACK_DATA_02_D_POSITION = ProtoField.int32( "BF_TRACK_DATA_02_D_POSITION", "D_POSITION", base.DEC )
BF_TRACK_DATA_03_B_DIRECTION_bit0 = ProtoField.uint8( "BF_TRACK_DATA_03_B_DIRECTION.bit0", "Driving direction (0 - forward, 1 - Reverse)", base.HEX, nil, 1 )
BF_TRACK_DATA_03_B_DIRECTION_bit1 = ProtoField.uint8( "BF_TRACK_DATA_03_B_DIRECTION.bit1", "Orientation in track (0 - as track, 1 - opposite)", base.HEX, nil, 2 )
BF_TRACK_DATA_03_B_DIRECTION_bit2 = ProtoField.uint8( "BF_TRACK_DATA_03_B_DIRECTION.bit2", "Locomotive orientation (0- B-end facing cars, 1 - A-end facing cars)", base.HEX, nil, 4 )

BF_TRACK_DATA_03_B_DIRECTION = ProtoField.uint8( "BF_TRACK_DATA_03_B_DIRECTION", "B_DIRECTION", base.HEX )

BF_TRACK_DATA_04_NID_PREVIOUS_TRACK = ProtoField.uint16( "BF_TRACK_DATA_04_NID_PREVIOUS_TRACK", "NID_PREVIOUS_TRACK", base.DEC )
function BLOCK_DECODE_TRACK_DATA(ctx)
    local btree = ctx.stack:top():add(fld_TRACK_DATA,ctx.tvbuf:range(ctx.off,13));
    ctx.stack:push(btree);

    FIELD_DECODE_NID_TRACK(ctx,BF_TRACK_DATA_00_NID_TRACK,{  })
    FIELD_DECODE_D_POSITION(ctx,BF_TRACK_DATA_01_D_POSITION,{  })
    FIELD_DECODE_D_POSITION(ctx,BF_TRACK_DATA_02_D_POSITION,{  })
    FIELD_DECODE_B_DIRECTION(ctx,BF_TRACK_DATA_03_B_DIRECTION,{ BF_TRACK_DATA_03_B_DIRECTION_bit0,BF_TRACK_DATA_03_B_DIRECTION_bit1,BF_TRACK_DATA_03_B_DIRECTION_bit2 })
    FIELD_DECODE_NID_PREVIOUS_TRACK(ctx,BF_TRACK_DATA_04_NID_PREVIOUS_TRACK,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_TRAIN_NAME = ProtoField.new("TRAIN_NAME","TRAIN_NAME",ftypes.NONE)


BF_TRAIN_NAME_00_TID_TRAIN_NAME = ProtoField.string( "BF_TRAIN_NAME_00_TID_TRAIN_NAME", "TID_TRAIN_NAME", base.ASCII )
function BLOCK_DECODE_TRAIN_NAME(ctx)
    local btree = ctx.stack:top():add(fld_TRAIN_NAME,ctx.tvbuf:range(ctx.off,20));
    ctx.stack:push(btree);

    FIELD_DECODE_TID_TRAIN_NAME(ctx,BF_TRAIN_NAME_00_TID_TRAIN_NAME,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_VEHICLE_ID_DATA = ProtoField.new("VEHICLE_ID_DATA","VEHICLE_ID_DATA",ftypes.NONE)


BF_VEHICLE_ID_DATA_00_NID_VEHICLE_TYPE = ProtoField.uint8( "BF_VEHICLE_ID_DATA_00_NID_VEHICLE_TYPE", "NID_VEHICLE_TYPE", base.DEC )

BF_VEHICLE_ID_DATA_01_NID_VEHICLE = ProtoField.uint16( "BF_VEHICLE_ID_DATA_01_NID_VEHICLE", "NID_VEHICLE", base.DEC )

BF_VEHICLE_ID_DATA_02_TID_VEHICLE_NAME = ProtoField.string( "BF_VEHICLE_ID_DATA_02_TID_VEHICLE_NAME", "TID_VEHICLE_NAME", base.ASCII )
function BLOCK_DECODE_VEHICLE_ID_DATA(ctx)
    local btree = ctx.stack:top():add(fld_VEHICLE_ID_DATA,ctx.tvbuf:range(ctx.off,23));
    ctx.stack:push(btree);

    FIELD_DECODE_NID_VEHICLE_TYPE(ctx,BF_VEHICLE_ID_DATA_00_NID_VEHICLE_TYPE,{  })
    FIELD_DECODE_NID_VEHICLE(ctx,BF_VEHICLE_ID_DATA_01_NID_VEHICLE,{  })
    FIELD_DECODE_TID_VEHICLE_NAME(ctx,BF_VEHICLE_ID_DATA_02_TID_VEHICLE_NAME,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_VEHICLE_LIST_DATA = ProtoField.new("VEHICLE_LIST_DATA","VEHICLE_LIST_DATA",ftypes.NONE)


BF_VEHICLE_LIST_DATA_00_N_VALUE = ProtoField.uint16( "BF_VEHICLE_LIST_DATA_00_N_VALUE", "N_VALUE", base.DEC )

BF_VEHICLE_LIST_DATA_01_NID_VEHICLE_TYPE = ProtoField.uint8( "BF_VEHICLE_LIST_DATA_01_NID_VEHICLE_TYPE", "NID_VEHICLE_TYPE", base.DEC )
function BLOCK_DECODE_VEHICLE_LIST_DATA(ctx)
    local btree = ctx.stack:top():add(fld_VEHICLE_LIST_DATA,ctx.tvbuf:range(ctx.off,3));
    ctx.stack:push(btree);

    FIELD_DECODE_N_VALUE(ctx,BF_VEHICLE_LIST_DATA_00_N_VALUE,{  })
    FIELD_DECODE_NID_VEHICLE_TYPE(ctx,BF_VEHICLE_LIST_DATA_01_NID_VEHICLE_TYPE,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_WAITING_TIME = ProtoField.new("WAITING_TIME","WAITING_TIME",ftypes.NONE)


BF_WAITING_TIME_00_T_WAITING_TIME = ProtoField.uint8( "BF_WAITING_TIME_00_T_WAITING_TIME", "T_WAITING_TIME", base.DEC )
function BLOCK_DECODE_WAITING_TIME(ctx)
    local btree = ctx.stack:top():add(fld_WAITING_TIME,ctx.tvbuf:range(ctx.off,1));
    ctx.stack:push(btree);

    FIELD_DECODE_T_WAITING_TIME(ctx,BF_WAITING_TIME_00_T_WAITING_TIME,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_DEPARTURE_WARNING = ProtoField.new("DEPARTURE_WARNING","DEPARTURE_WARNING",ftypes.NONE)


BF_DEPARTURE_WARNING_00_Q_SIGNAL = ProtoField.uint8( "BF_DEPARTURE_WARNING_00_Q_SIGNAL", "Q_SIGNAL", base.DEC  ,{ [0] = "Undefined",[1] = "Sound type 1",[2] = "Sound type 2",[3] = "Sound type 3" } )
function BLOCK_DECODE_DEPARTURE_WARNING(ctx)
    local btree = ctx.stack:top():add(fld_DEPARTURE_WARNING,ctx.tvbuf:range(ctx.off,1));
    ctx.stack:push(btree);

    FIELD_DECODE_Q_SIGNAL(ctx,BF_DEPARTURE_WARNING_00_Q_SIGNAL,{  })
    ctx.stack:pop(1);
end

-------------------------------
fld_YARD_REQUEST = ProtoField.new("YARD_REQUEST","YARD_REQUEST",ftypes.NONE)

function BLOCK_DECODE_YARD_REQUEST(ctx)
    local btree = ctx.stack:top():add(fld_YARD_REQUEST,ctx.tvbuf:range(ctx.off,0));
    ctx.stack:push(btree);

    ctx.stack:pop(1);
end

--- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

--- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
--- Decode of messages

-------------------------------


MF_ApproximatePosition_00_NID_MSG = ProtoField.uint8( "tcc.ApproximatePosition.NID_MSG", "NID_MSG", base.DEC  )

MF_ApproximatePosition_01_NID_TRACK = ProtoField.uint16( "tcc.ApproximatePosition.NID_TRACK", "Train front Position, Track", base.DEC  )

MF_ApproximatePosition_02_D_POSITION = ProtoField.int32( "tcc.ApproximatePosition.D_POSITION", "Train front Position, Distance", base.DEC  )
function MSG_DECODE_ApproximatePosition(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_NID_MSG(ctx,MF_ApproximatePosition_00_NID_MSG, {  })
    FIELD_DECODE_NID_TRACK(ctx,MF_ApproximatePosition_01_NID_TRACK, {  })
    FIELD_DECODE_D_POSITION(ctx,MF_ApproximatePosition_02_D_POSITION, {  })
    
    MSG_BLOCKS_DECODE(ctx, { 
    [3] = { id=3, func=BLOCK_DECODE_TRACK_DATA, min=1, max=-1 } });
    
end

-------------------------------

function MSG_DECODE_AreaRequest(ctx)
    MSG_DECODE_HEADER(ctx);

    
    MSG_BLOCKS_DECODE(ctx, { 
    [42] = { id=42, func=BLOCK_DECODE_AREAS, min=1, max=-1 } });
    
end

-------------------------------


MF_ATORemoteControl_00_V_SPEED = ProtoField.uint16( "tcc.ATORemoteControl.V_SPEED", "ATO speed", base.DEC  )

MF_ATORemoteControl_01_T_REMOTE = ProtoField.uint8( "tcc.ATORemoteControl.T_REMOTE", "T_REMOTE", base.DEC  )
MF_ATORemoteControl_02_B_DIRECTION_bit0 = ProtoField.uint8( "MF_ATORemoteControl_02_B_DIRECTION.bit0", "Driving direction (0 - forward, 1 - Reverse)", base.HEX, nil, 1 )
MF_ATORemoteControl_02_B_DIRECTION_bit1 = ProtoField.uint8( "MF_ATORemoteControl_02_B_DIRECTION.bit1", "Orientation in track (0 - as track, 1 - opposite)", base.HEX, nil, 2 )
MF_ATORemoteControl_02_B_DIRECTION_bit2 = ProtoField.uint8( "MF_ATORemoteControl_02_B_DIRECTION.bit2", "Locomotive orientation (0- B-end facing cars, 1 - A-end facing cars)", base.HEX, nil, 4 )

MF_ATORemoteControl_02_B_DIRECTION = ProtoField.uint8( "tcc.ATORemoteControl.B_DIRECTION", "B_DIRECTION", base.HEX  )
function MSG_DECODE_ATORemoteControl(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_V_SPEED(ctx,MF_ATORemoteControl_00_V_SPEED, {  })
    FIELD_DECODE_T_REMOTE(ctx,MF_ATORemoteControl_01_T_REMOTE, {  })
    FIELD_DECODE_B_DIRECTION(ctx,MF_ATORemoteControl_02_B_DIRECTION, { MF_ATORemoteControl_02_B_DIRECTION_bit0,MF_ATORemoteControl_02_B_DIRECTION_bit1,MF_ATORemoteControl_02_B_DIRECTION_bit2 })
    
    MSG_BLOCKS_DECODE(ctx, { 
    [13] = { id=13, func=BLOCK_DECODE_LOAD_FINISHED, min=-1, max=1 } });
    
end

-------------------------------

function MSG_DECODE_CommandMessage(ctx)
    MSG_DECODE_HEADER(ctx);

    
    MSG_BLOCKS_DECODE(ctx, { 
    [7] = { id=7, func=BLOCK_DECODE_TRAIN_NAME, min=-1, max=1 },
    [8] = { id=8, func=BLOCK_DECODE_SET_TIME, min=-1, max=1 },
    [12] = { id=12, func=BLOCK_DECODE_RELEASE_BRAKE, min=-1, max=1 },
    [21] = { id=21, func=BLOCK_DECODE_SAFE_FOR_BOARDING_ACTIVATE, min=-1, max=1 },
    [14] = { id=14, func=BLOCK_DECODE_TEXT_MESSAGE, min=-1, max=1 } });
    
end

-------------------------------


MF_ConfigurationData_00_NID_MSG = ProtoField.uint8( "tcc.ConfigurationData.NID_MSG", "NID_MSG", base.DEC  )
function MSG_DECODE_ConfigurationData(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_NID_MSG(ctx,MF_ConfigurationData_00_NID_MSG, {  })
    
    MSG_BLOCKS_DECODE(ctx, { 
    [38] = { id=38, func=BLOCK_DECODE_CONFIGURATION_DATA, min=1, max=-1 } });
    
end

-------------------------------


MF_DriverLogonStatus_00_Q_LOGON_STATUS = ProtoField.uint8( "tcc.DriverLogonStatus.Q_LOGON_STATUS", "Q_LOGON_STATUS", base.DEC  ,{ [0] = "Unsuccessful logon",[1] = "Successfull logon, Normal privilege" }  )
function MSG_DECODE_DriverLogonStatus(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_Q_LOGON_STATUS(ctx,MF_DriverLogonStatus_00_Q_LOGON_STATUS, {  })
    
    MSG_BLOCKS_DECODE(ctx, { 
    [8] = { id=8, func=BLOCK_DECODE_SET_TIME, min=1, max=1 } });
    
end

-------------------------------


MF_EmergencyAlert_00_Q_ALERT = ProtoField.uint8( "tcc.EmergencyAlert.Q_ALERT", "Q_ALERT", base.DEC  ,{ [0] = "Undefined",[1] = "Initiated by driver",[2] = "Initiated by dispatcher",[3] = "Position report outside set route",[4] = "Points inside set route in error",[5] = "Profile control triggered",[6] = "Powerless section",[7] = "Gate forced open",[8] = "Location closed",[9] = "Location error",[10] = "Location maintenance",[11] = "Location shutdown",[12] = "Location stopped",[13] = "Other train/Other train in error",[14] = "Route conflict",[15] = "Emergency Stop Area",[16] = "TIMS error" }  )
function MSG_DECODE_EmergencyAlert(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_Q_ALERT(ctx,MF_EmergencyAlert_00_Q_ALERT, {  })
    
end

-------------------------------

function MSG_DECODE_ExternalData(ctx)
    MSG_DECODE_HEADER(ctx);

    
    MSG_BLOCKS_DECODE(ctx, { 
    [33] = { id=33, func=BLOCK_DECODE_EXTERNAL_DATA, min=1, max=1 } });
    
end

-------------------------------

function MSG_DECODE_JoinCommand(ctx)
    MSG_DECODE_HEADER(ctx);

    
end

-------------------------------


MF_MovementAuthority_00_NID_MSG = ProtoField.uint8( "tcc.MovementAuthority.NID_MSG", "NID_MSG", base.DEC  )

MF_MovementAuthority_01_T_VALID = ProtoField.uint8( "tcc.MovementAuthority.T_VALID", "T_VALID", base.DEC  )

MF_MovementAuthority_02_V_SPEED = ProtoField.uint16( "tcc.MovementAuthority.V_SPEED", "Target speed", base.DEC  )

MF_MovementAuthority_03_G_GRADIENT = ProtoField.int8( "tcc.MovementAuthority.G_GRADIENT", "G_GRADIENT", base.DEC  )

MF_MovementAuthority_04_M_LOADED = ProtoField.uint8( "tcc.MovementAuthority.M_LOADED", "M_LOADED", base.DEC  ,{ [0] = "Train is empty",[1] = "Train is loaded" }  )

MF_MovementAuthority_05_N_ADHESION = ProtoField.uint8( "tcc.MovementAuthority.N_ADHESION", "N_ADHESION", base.DEC  )
MF_MovementAuthority_06_B_DIRECTION_bit0 = ProtoField.uint8( "MF_MovementAuthority_06_B_DIRECTION.bit0", "Driving direction (0 - forward, 1 - Reverse)", base.HEX, nil, 1 )
MF_MovementAuthority_06_B_DIRECTION_bit1 = ProtoField.uint8( "MF_MovementAuthority_06_B_DIRECTION.bit1", "Orientation in track (0 - as track, 1 - opposite)", base.HEX, nil, 2 )
MF_MovementAuthority_06_B_DIRECTION_bit2 = ProtoField.uint8( "MF_MovementAuthority_06_B_DIRECTION.bit2", "Locomotive orientation (0- B-end facing cars, 1 - A-end facing cars)", base.HEX, nil, 4 )

MF_MovementAuthority_06_B_DIRECTION = ProtoField.uint8( "tcc.MovementAuthority.B_DIRECTION", "B_DIRECTION", base.HEX  )

MF_MovementAuthority_07_Q_ROUTE_TYPE = ProtoField.uint8( "tcc.MovementAuthority.Q_ROUTE_TYPE", "Q_ROUTE_TYPE", base.DEC  ,{ [0] = "Undefined",[1] = "Location start",[2] = "Location End",[3] = "Re-registration",[4] = "Shunting Route",[5] = "Normal",[6] = "Join",[7] = "Split",[8] = "Staff Responsible (SR)",[9] = "Unconditional shortening of MA" }  )

MF_MovementAuthority_08_NID_TRACK = ProtoField.uint16( "tcc.MovementAuthority.NID_TRACK", "NID_TRACK", base.DEC  )

MF_MovementAuthority_09_D_POSITION = ProtoField.int32( "tcc.MovementAuthority.D_POSITION", "D_POSITION", base.DEC  )

MF_MovementAuthority_10_NID_TRACK = ProtoField.uint16( "tcc.MovementAuthority.NID_TRACK", "NID_TRACK", base.DEC  )

MF_MovementAuthority_11_D_POSITION = ProtoField.int32( "tcc.MovementAuthority.D_POSITION", "D_POSITION", base.DEC  )

MF_MovementAuthority_12_D_MA_MARGIN = ProtoField.uint16( "tcc.MovementAuthority.D_MA_MARGIN", "D_MA_MARGIN", base.DEC  )

MF_MovementAuthority_13_D_OVERLAP = ProtoField.uint16( "tcc.MovementAuthority.D_OVERLAP", "D_OVERLAP", base.DEC  )
function MSG_DECODE_MovementAuthority(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_NID_MSG(ctx,MF_MovementAuthority_00_NID_MSG, {  })
    FIELD_DECODE_T_VALID(ctx,MF_MovementAuthority_01_T_VALID, {  })
    FIELD_DECODE_V_SPEED(ctx,MF_MovementAuthority_02_V_SPEED, {  })
    FIELD_DECODE_G_GRADIENT(ctx,MF_MovementAuthority_03_G_GRADIENT, {  })
    FIELD_DECODE_M_LOADED(ctx,MF_MovementAuthority_04_M_LOADED, {  })
    FIELD_DECODE_N_ADHESION(ctx,MF_MovementAuthority_05_N_ADHESION, {  })
    FIELD_DECODE_B_DIRECTION(ctx,MF_MovementAuthority_06_B_DIRECTION, { MF_MovementAuthority_06_B_DIRECTION_bit0,MF_MovementAuthority_06_B_DIRECTION_bit1,MF_MovementAuthority_06_B_DIRECTION_bit2 })
    FIELD_DECODE_Q_ROUTE_TYPE(ctx,MF_MovementAuthority_07_Q_ROUTE_TYPE, {  })
    FIELD_DECODE_NID_TRACK(ctx,MF_MovementAuthority_08_NID_TRACK, {  })
    FIELD_DECODE_D_POSITION(ctx,MF_MovementAuthority_09_D_POSITION, {  })
    FIELD_DECODE_NID_TRACK(ctx,MF_MovementAuthority_10_NID_TRACK, {  })
    FIELD_DECODE_D_POSITION(ctx,MF_MovementAuthority_11_D_POSITION, {  })
    FIELD_DECODE_D_MA_MARGIN(ctx,MF_MovementAuthority_12_D_MA_MARGIN, {  })
    FIELD_DECODE_D_OVERLAP(ctx,MF_MovementAuthority_13_D_OVERLAP, {  })
    
    MSG_BLOCKS_DECODE(ctx, { 
    [22] = { id=22, func=BLOCK_DECODE_DEPARTURE_WARNING, min=0, max=1 },
    [18] = { id=18, func=BLOCK_DECODE_KEEP_TRACK_DATA, min=0, max=1 },
    [19] = { id=19, func=BLOCK_DECODE_PARTLY_MA, min=0, max=1 },
    [17] = { id=17, func=BLOCK_DECODE_MAX_SEARCH_DIST, min=0, max=1 },
    [9] = { id=9, func=BLOCK_DECODE_LOCATION_DATA, min=0, max=1 },
    [10] = { id=10, func=BLOCK_DECODE_ATO_STOP_POSITION, min=0, max=1 },
    [3] = { id=3, func=BLOCK_DECODE_TRACK_DATA, min=0, max=0 },
    [4] = { id=4, func=BLOCK_DECODE_BALISE_DATA, min=0, max=0 },
    [5] = { id=5, func=BLOCK_DECODE_GRADIENT_DATA, min=0, max=0 },
    [6] = { id=6, func=BLOCK_DECODE_CEILING_SPEED_DATA, min=0, max=0 },
    [16] = { id=16, func=BLOCK_DECODE_TRACK_DATA_ITEM, min=0, max=0 } });
    
end

-------------------------------


MF_Path_00_V_SPEED = ProtoField.uint16( "tcc.Path.V_SPEED", "V_SPEED", base.DEC  )

MF_Path_01_NID_TRACK = ProtoField.uint16( "tcc.Path.NID_TRACK", "Next target, Track Id", base.DEC  )

MF_Path_02_D_POSITION = ProtoField.int32( "tcc.Path.D_POSITION", "Next target, Distance on Track", base.DEC  )
function MSG_DECODE_Path(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_V_SPEED(ctx,MF_Path_00_V_SPEED, {  })
    FIELD_DECODE_NID_TRACK(ctx,MF_Path_01_NID_TRACK, {  })
    FIELD_DECODE_D_POSITION(ctx,MF_Path_02_D_POSITION, {  })
    
    MSG_BLOCKS_DECODE(ctx, { 
    [41] = { id=41, func=BLOCK_DECODE_TRACKS, min=1, max=-1 },
    [39] = { id=39, func=BLOCK_DECODE_ETA_REQUEST, min=-1, max=1 },
    [40] = { id=40, func=BLOCK_DECODE_SPEED_CHANGE_POSITION, min=0, max=0 } });
    
end

-------------------------------

function MSG_DECODE_PositionReportRequest(ctx)
    MSG_DECODE_HEADER(ctx);

    
    MSG_BLOCKS_DECODE(ctx, { 
    [15] = { id=15, func=BLOCK_DECODE_WAITING_TIME, min=0, max=1 },
    [34] = { id=34, func=BLOCK_DECODE_CONFIRM_CONFIG, min=0, max=1 } });
    
end

-------------------------------


MF_PossessionAcknowledge_00_Q_ACKNOWLEDGE = ProtoField.uint8( "tcc.PossessionAcknowledge.Q_ACKNOWLEDGE", "Q_ACKNOWLEDGE", base.DEC  ,{ [0] = "Request not acknowledged",[1] = "Request acknowledged" }  )

MF_PossessionAcknowledge_01_V_SPEED = ProtoField.uint16( "tcc.PossessionAcknowledge.V_SPEED", "V_SPEED", base.DEC  )
function MSG_DECODE_PossessionAcknowledge(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_Q_ACKNOWLEDGE(ctx,MF_PossessionAcknowledge_00_Q_ACKNOWLEDGE, {  })
    FIELD_DECODE_V_SPEED(ctx,MF_PossessionAcknowledge_01_V_SPEED, {  })
    
end

-------------------------------


MF_RejectConfiguration_00_Q_UNREGISTRATION = ProtoField.uint8( "tcc.RejectConfiguration.Q_UNREGISTRATION", "Q_UNREGISTRATION", base.DEC  ,{ [0] = "Unregistered by dispatcher",[1] = "Configuration rejected, unknown vehicle in train",[2] = "Configuration rejected, a vehicle is part of another train",[3] = "Registration rejected, unexpected balise identity",[4] = "Registration rejected, conflict with another train route",[5] = "Registration aborted by driver or ATP",[6] = "Registration rejected, wrong driving direction",[7] = "Registration rejected, not possible to set route",[8] = "Configuration rejected, to many cars in train",[9] = "Configuration rejected, duplicated car identities" }  )
function MSG_DECODE_RejectConfiguration(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_Q_UNREGISTRATION(ctx,MF_RejectConfiguration_00_Q_UNREGISTRATION, {  })
    
end

-------------------------------

function MSG_DECODE_RevokeEmergencyAlert(ctx)
    MSG_DECODE_HEADER(ctx);

    
end

-------------------------------


MF_ShuntingAcknowledge_00_Q_ACKNOWLEDGE = ProtoField.uint8( "tcc.ShuntingAcknowledge.Q_ACKNOWLEDGE", "Q_ACKNOWLEDGE", base.DEC  ,{ [0] = "Request not acknowledged",[1] = "Request acknowledged" }  )

MF_ShuntingAcknowledge_01_V_SPEED = ProtoField.uint16( "tcc.ShuntingAcknowledge.V_SPEED", "V_SPEED", base.DEC  )
function MSG_DECODE_ShuntingAcknowledge(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_Q_ACKNOWLEDGE(ctx,MF_ShuntingAcknowledge_00_Q_ACKNOWLEDGE, {  })
    FIELD_DECODE_V_SPEED(ctx,MF_ShuntingAcknowledge_01_V_SPEED, {  })
    
end

-------------------------------


MF_StopTrain_00_Q_STOP = ProtoField.uint8( "tcc.StopTrain.Q_STOP", "Q_STOP", base.DEC  ,{ [0] = "Undefined",[1] = "Order cancelled" }  )
function MSG_DECODE_StopTrain(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_Q_STOP(ctx,MF_StopTrain_00_Q_STOP, {  })
    
end

-------------------------------


MF_TrainSetup_00_NID_MSG = ProtoField.uint8( "tcc.TrainSetup.NID_MSG", "NID_MSG", base.DEC  )

MF_TrainSetup_01_Q_SETUP = ProtoField.uint8( "tcc.TrainSetup.Q_SETUP", "Q_SETUP", base.DEC  ,{ [0] = "Registration",[1] = "Reconfiguration",[2] = "Reregistration",[3] = "Reposition" }  )

MF_TrainSetup_02_Q_TS_STATE = ProtoField.uint8( "tcc.TrainSetup.Q_TS_STATE", "Q_TS_STATE", base.DEC  ,{ [1] = "Temporary",[2] = "Permanent" }  )

MF_TrainSetup_03_V_SPEED = ProtoField.uint16( "tcc.TrainSetup.V_SPEED", "V_SPEED", base.DEC  )

MF_TrainSetup_04_L_TRAIN = ProtoField.uint24( "tcc.TrainSetup.L_TRAIN", "L_TRAIN", base.DEC  )

MF_TrainSetup_05_Q_TIC_AVAILABLE = ProtoField.uint8( "tcc.TrainSetup.Q_TIC_AVAILABLE", "Q_TIC_AVAILABLE", base.DEC  ,{ [0] = "TIC not available, train set manual configuration",[1] = "TIC is available, train set automatic configuration" }  )

MF_TrainSetup_06_Q_TIMS_AVAILABLE = ProtoField.uint8( "tcc.TrainSetup.Q_TIMS_AVAILABLE", "Q_TIMS_AVAILABLE", base.DEC  ,{ [0] = "TIMS not available/shall not be used",[1] = "TIMS available/shall be used" }  )
MF_TrainSetup_07_B_DIRECTION_bit0 = ProtoField.uint8( "MF_TrainSetup_07_B_DIRECTION.bit0", "Driving direction (0 - forward, 1 - Reverse)", base.HEX, nil, 1 )
MF_TrainSetup_07_B_DIRECTION_bit1 = ProtoField.uint8( "MF_TrainSetup_07_B_DIRECTION.bit1", "Orientation in track (0 - as track, 1 - opposite)", base.HEX, nil, 2 )
MF_TrainSetup_07_B_DIRECTION_bit2 = ProtoField.uint8( "MF_TrainSetup_07_B_DIRECTION.bit2", "Locomotive orientation (0- B-end facing cars, 1 - A-end facing cars)", base.HEX, nil, 4 )

MF_TrainSetup_07_B_DIRECTION = ProtoField.uint8( "tcc.TrainSetup.B_DIRECTION", "B_DIRECTION", base.HEX  )

MF_TrainSetup_08_M_BRAKE_SYSTEM = ProtoField.uint8( "tcc.TrainSetup.M_BRAKE_SYSTEM", "M_BRAKE_SYSTEM", base.DEC  ,{ [0] = "Undefined",[1] = "Pneumatic brake system",[2] = "ECPB" }  )
function MSG_DECODE_TrainSetup(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_NID_MSG(ctx,MF_TrainSetup_00_NID_MSG, {  })
    FIELD_DECODE_Q_SETUP(ctx,MF_TrainSetup_01_Q_SETUP, {  })
    FIELD_DECODE_Q_TS_STATE(ctx,MF_TrainSetup_02_Q_TS_STATE, {  })
    FIELD_DECODE_V_SPEED(ctx,MF_TrainSetup_03_V_SPEED, {  })
    FIELD_DECODE_L_TRAIN(ctx,MF_TrainSetup_04_L_TRAIN, {  })
    FIELD_DECODE_Q_TIC_AVAILABLE(ctx,MF_TrainSetup_05_Q_TIC_AVAILABLE, {  })
    FIELD_DECODE_Q_TIMS_AVAILABLE(ctx,MF_TrainSetup_06_Q_TIMS_AVAILABLE, {  })
    FIELD_DECODE_B_DIRECTION(ctx,MF_TrainSetup_07_B_DIRECTION, { MF_TrainSetup_07_B_DIRECTION_bit0,MF_TrainSetup_07_B_DIRECTION_bit1,MF_TrainSetup_07_B_DIRECTION_bit2 })
    FIELD_DECODE_M_BRAKE_SYSTEM(ctx,MF_TrainSetup_08_M_BRAKE_SYSTEM, {  })
    
    MSG_BLOCKS_DECODE(ctx, { 
    [7] = { id=7, func=BLOCK_DECODE_TRAIN_NAME, min=0, max=1 },
    [31] = { id=31, func=BLOCK_DECODE_VEHICLE_TYPE_DATA, min=0, max=0 },
    [23] = { id=23, func=BLOCK_DECODE_VEHICLE_ID_DATA, min=0, max=0 },
    [36] = { id=36, func=BLOCK_DECODE_VEHICLE_LIST_DATA, min=0, max=0 } });
    
end

-------------------------------


MF_Unregistration_00_Q_UNREGISTRATION = ProtoField.uint8( "tcc.Unregistration.Q_UNREGISTRATION", "Q_UNREGISTRATION", base.DEC  ,{ [0] = "Unregistered by dispatcher",[1] = "Configuration rejected, unknown vehicle in train",[2] = "Configuration rejected, a vehicle is part of another train",[3] = "Registration rejected, unexpected balise identity",[4] = "Registration rejected, conflict with another train route",[5] = "Registration aborted by driver or ATP",[6] = "Registration rejected, wrong driving direction",[7] = "Registration rejected, not possible to set route",[8] = "Configuration rejected, to many cars in train",[9] = "Configuration rejected, duplicated car identities" }  )
function MSG_DECODE_Unregistration(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_Q_UNREGISTRATION(ctx,MF_Unregistration_00_Q_UNREGISTRATION, {  })
    
end

-------------------------------


MF_YardAcknowledge_00_Q_ACKNOWLEDGE = ProtoField.uint8( "tcc.YardAcknowledge.Q_ACKNOWLEDGE", "Q_ACKNOWLEDGE", base.DEC  ,{ [0] = "Request not acknowledged",[1] = "Request acknowledged" }  )

MF_YardAcknowledge_01_V_SPEED = ProtoField.uint16( "tcc.YardAcknowledge.V_SPEED", "V_SPEED", base.DEC  )
function MSG_DECODE_YardAcknowledge(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_Q_ACKNOWLEDGE(ctx,MF_YardAcknowledge_00_Q_ACKNOWLEDGE, {  })
    FIELD_DECODE_V_SPEED(ctx,MF_YardAcknowledge_01_V_SPEED, {  })
    
end

-------------------------------


MF_AbortSetup_00_Q_ABORT = ProtoField.uint8( "tcc.AbortSetup.Q_ABORT", "Q_ABORT", base.DEC  ,{ [1] = "Aborted by driver" }  )
function MSG_DECODE_AbortSetup(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_Q_ABORT(ctx,MF_AbortSetup_00_Q_ABORT, {  })
    
end

-------------------------------


MF_DriverInformation_00_TID_DRIVER = ProtoField.string( "tcc.DriverInformation.TID_DRIVER", "TID_DRIVER", base.ASCII  )

MF_DriverInformation_01_TID_PASSWORD = ProtoField.string( "tcc.DriverInformation.TID_PASSWORD", "TID_PASSWORD", base.ASCII  )
function MSG_DECODE_DriverInformation(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_TID_DRIVER(ctx,MF_DriverInformation_00_TID_DRIVER, {  })
    FIELD_DECODE_TID_PASSWORD(ctx,MF_DriverInformation_01_TID_PASSWORD, {  })
    
    MSG_BLOCKS_DECODE(ctx, { 
    [25] = { id=25, func=BLOCK_DECODE_CANCEL_AREA, min=0, max=1 } });
    
end

-------------------------------


MF_MessageAcknowledge_00_NID_MSG = ProtoField.uint8( "tcc.MessageAcknowledge.NID_MSG", "NID_MSG", base.DEC  )

MF_MessageAcknowledge_01_Q_MESSAGE_STATUS = ProtoField.uint8( "tcc.MessageAcknowledge.Q_MESSAGE_STATUS", "Q_MESSAGE_STATUS", base.DEC  ,{ [0] = "Message not accepted",[1] = "Message accepted" }  )
function MSG_DECODE_MessageAcknowledge(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_NID_MSG(ctx,MF_MessageAcknowledge_00_NID_MSG, {  })
    FIELD_DECODE_Q_MESSAGE_STATUS(ctx,MF_MessageAcknowledge_01_Q_MESSAGE_STATUS, {  })
    
end

-------------------------------


MF_PositionReport_00_NID_TRACK = ProtoField.uint16( "tcc.PositionReport.NID_TRACK", "Trailing position, Track", base.DEC  )

MF_PositionReport_01_D_POSITION = ProtoField.int32( "tcc.PositionReport.D_POSITION", "Position of Trailing end of train in Track", base.DEC  )

MF_PositionReport_02_NID_TRACK = ProtoField.uint16( "tcc.PositionReport.NID_TRACK", "Leading position, Track", base.DEC  )

MF_PositionReport_03_D_POSITION = ProtoField.int32( "tcc.PositionReport.D_POSITION", "Position of Leading end of train in Track", base.DEC  )

MF_PositionReport_04_Q_POSITION = ProtoField.uint8( "tcc.PositionReport.Q_POSITION", "Q_POSITION", base.DEC  ,{ [0] = "Unknown",[1] = "Approximate",[2] = "Known",[3] = "Doubtful" }  )
MF_PositionReport_05_B_DIRECTION_bit0 = ProtoField.uint8( "MF_PositionReport_05_B_DIRECTION.bit0", "Driving direction (0 - forward, 1 - Reverse)", base.HEX, nil, 1 )
MF_PositionReport_05_B_DIRECTION_bit1 = ProtoField.uint8( "MF_PositionReport_05_B_DIRECTION.bit1", "Orientation in track (0 - as track, 1 - opposite)", base.HEX, nil, 2 )
MF_PositionReport_05_B_DIRECTION_bit2 = ProtoField.uint8( "MF_PositionReport_05_B_DIRECTION.bit2", "Locomotive orientation (0- B-end facing cars, 1 - A-end facing cars)", base.HEX, nil, 4 )

MF_PositionReport_05_B_DIRECTION = ProtoField.uint8( "tcc.PositionReport.B_DIRECTION", "B_DIRECTION", base.HEX  )

MF_PositionReport_06_V_SPEED = ProtoField.uint16( "tcc.PositionReport.V_SPEED", "Current speed", base.DEC  )
MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit0 = ProtoField.uint32( "MF_PositionReport_07_B_TRAIN_CORE_STATUS.bit0", "Safety Halt, AOS", base.HEX, nil, 1 )
MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit1 = ProtoField.uint32( "MF_PositionReport_07_B_TRAIN_CORE_STATUS.bit1", "EA from driver", base.HEX, nil, 2 )
MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit2 = ProtoField.uint32( "MF_PositionReport_07_B_TRAIN_CORE_STATUS.bit2", "TIMS Integrity Broken", base.HEX, nil, 4 )
MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit3 = ProtoField.uint32( "MF_PositionReport_07_B_TRAIN_CORE_STATUS.bit3", "Braking event, AOS", base.HEX, nil, 8 )
MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit4 = ProtoField.uint32( "MF_PositionReport_07_B_TRAIN_CORE_STATUS.bit4", "Handling done", base.HEX, nil, 16 )
MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit5 = ProtoField.uint32( "MF_PositionReport_07_B_TRAIN_CORE_STATUS.bit5", "Train Idling", base.HEX, nil, 32 )
MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit6 = ProtoField.uint32( "MF_PositionReport_07_B_TRAIN_CORE_STATUS.bit6", "TIMS Integrity manual override from Driver", base.HEX, nil, 64 )
MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit7 = ProtoField.uint32( "MF_PositionReport_07_B_TRAIN_CORE_STATUS.bit7", "MA time out", base.HEX, nil, 128 )
MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit8 = ProtoField.uint32( "MF_PositionReport_07_B_TRAIN_CORE_STATUS.bit8", "ATP reset", base.HEX, nil, 256 )
MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit9 = ProtoField.uint32( "MF_PositionReport_07_B_TRAIN_CORE_STATUS.bit9", "ATP needs to be reset", base.HEX, nil, 512 )
MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit10 = ProtoField.uint32( "MF_PositionReport_07_B_TRAIN_CORE_STATUS.bit10", "ATP intervention", base.HEX, nil, 1024 )
MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit11 = ProtoField.uint32( "MF_PositionReport_07_B_TRAIN_CORE_STATUS.bit11", "Brake release wanted", base.HEX, nil, 2048 )
MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit12 = ProtoField.uint32( "MF_PositionReport_07_B_TRAIN_CORE_STATUS.bit12", "Manual TIMS confirmation", base.HEX, nil, 4096 )
MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit13 = ProtoField.uint32( "MF_PositionReport_07_B_TRAIN_CORE_STATUS.bit13", "Slip detected", base.HEX, nil, 8192 )
MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit14 = ProtoField.uint32( "MF_PositionReport_07_B_TRAIN_CORE_STATUS.bit14", "Free rolling", base.HEX, nil, 16384 )
MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit15 = ProtoField.uint32( "MF_PositionReport_07_B_TRAIN_CORE_STATUS.bit15", "Emergency Alert active", base.HEX, nil, 32768 )
MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit16 = ProtoField.uint32( "MF_PositionReport_07_B_TRAIN_CORE_STATUS.bit16", "Attention needed", base.HEX, nil, 65536 )
MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit17 = ProtoField.uint32( "MF_PositionReport_07_B_TRAIN_CORE_STATUS.bit17", "Not ready to drive", base.HEX, nil, 131072 )
MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit18 = ProtoField.uint32( "MF_PositionReport_07_B_TRAIN_CORE_STATUS.bit18", "Safe for boarding is active", base.HEX, nil, 262144 )

MF_PositionReport_07_B_TRAIN_CORE_STATUS = ProtoField.uint32( "tcc.PositionReport.B_TRAIN_CORE_STATUS", "B_TRAIN_CORE_STATUS", base.HEX  )

MF_PositionReport_08_D_WINDOW = ProtoField.uint16( "tcc.PositionReport.D_WINDOW", "D_WINDOW", base.DEC  )

MF_PositionReport_09_NID_TRACK = ProtoField.uint16( "tcc.PositionReport.NID_TRACK", "Target track", base.DEC  )

MF_PositionReport_10_D_POSITION = ProtoField.int32( "tcc.PositionReport.D_POSITION", "Target position", base.DEC  )

MF_PositionReport_11_Q_ATP_MODE = ProtoField.uint8( "tcc.PositionReport.Q_ATP_MODE", "Q_ATP_MODE", base.DEC  ,{ [0] = "Undefined",[1] = "Power Up",[2] = "Configuration",[3] = "Registration",[4] = "Balise Search",[5] = "Normal(Full ATP)",[6] = "Shunting",[7] = "Location",[8] = "Automatic Unload",[9] = "Yard",[10] = "Unregistered",[11] = "Power Down",[12] = "Safety Halt (Fatal Failure)",[13] = "Sleeping",[14] = "Staff Responsible",[15] = "Shunting Route",[16] = "Possession",[17] = "Split",[18] = "Join",[19] = "Safe Brake to Stop" }  )

MF_PositionReport_12_Q_ATO_MODE = ProtoField.uint8( "tcc.PositionReport.Q_ATO_MODE", "Q_ATO_MODE", base.DEC  ,{ [0] = "Undefined",[1] = "Manual",[2] = "Supervised automatic",[3] = "Automatic",[4] = "Remote control" }  )

MF_PositionReport_13_M_BRAKE_SYSTEM = ProtoField.uint8( "tcc.PositionReport.M_BRAKE_SYSTEM", "M_BRAKE_SYSTEM", base.DEC  ,{ [0] = "Undefined",[1] = "Pneumatic brake system",[2] = "ECPB" }  )

MF_PositionReport_14_D_BRAKE_DISTANCE = ProtoField.uint24( "tcc.PositionReport.D_BRAKE_DISTANCE", "D_BRAKE_DISTANCE", base.DEC  )
function MSG_DECODE_PositionReport(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_NID_TRACK(ctx,MF_PositionReport_00_NID_TRACK, {  })
    FIELD_DECODE_D_POSITION(ctx,MF_PositionReport_01_D_POSITION, {  })
    FIELD_DECODE_NID_TRACK(ctx,MF_PositionReport_02_NID_TRACK, {  })
    FIELD_DECODE_D_POSITION(ctx,MF_PositionReport_03_D_POSITION, {  })
    FIELD_DECODE_Q_POSITION(ctx,MF_PositionReport_04_Q_POSITION, {  })
    FIELD_DECODE_B_DIRECTION(ctx,MF_PositionReport_05_B_DIRECTION, { MF_PositionReport_05_B_DIRECTION_bit0,MF_PositionReport_05_B_DIRECTION_bit1,MF_PositionReport_05_B_DIRECTION_bit2 })
    FIELD_DECODE_V_SPEED(ctx,MF_PositionReport_06_V_SPEED, {  })
    FIELD_DECODE_B_TRAIN_CORE_STATUS(ctx,MF_PositionReport_07_B_TRAIN_CORE_STATUS, { MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit0,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit1,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit2,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit3,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit4,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit5,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit6,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit7,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit8,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit9,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit10,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit11,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit12,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit13,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit14,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit15,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit16,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit17,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit18 })
    FIELD_DECODE_D_WINDOW(ctx,MF_PositionReport_08_D_WINDOW, {  })
    FIELD_DECODE_NID_TRACK(ctx,MF_PositionReport_09_NID_TRACK, {  })
    FIELD_DECODE_D_POSITION(ctx,MF_PositionReport_10_D_POSITION, {  })
    FIELD_DECODE_Q_ATP_MODE(ctx,MF_PositionReport_11_Q_ATP_MODE, {  })
    FIELD_DECODE_Q_ATO_MODE(ctx,MF_PositionReport_12_Q_ATO_MODE, {  })
    FIELD_DECODE_M_BRAKE_SYSTEM(ctx,MF_PositionReport_13_M_BRAKE_SYSTEM, {  })
    FIELD_DECODE_D_BRAKE_DISTANCE(ctx,MF_PositionReport_14_D_BRAKE_DISTANCE, {  })
    
    MSG_BLOCKS_DECODE(ctx, { 
    [28] = { id=28, func=BLOCK_DECODE_POSSESSION_REQUEST, min=0, max=1 },
    [29] = { id=29, func=BLOCK_DECODE_SHUNTING_REQUEST, min=0, max=1 },
    [32] = { id=32, func=BLOCK_DECODE_YARD_REQUEST, min=0, max=1 },
    [27] = { id=27, func=BLOCK_DECODE_LAST_BALISE, min=0, max=1 },
    [26] = { id=26, func=BLOCK_DECODE_AOS_VERSION, min=0, max=1 },
    [7] = { id=7, func=BLOCK_DECODE_TRAIN_NAME, min=0, max=1 },
    [20] = { id=20, func=BLOCK_DECODE_ETA_CONFIRMATION, min=0, max=1 },
    [30] = { id=30, func=BLOCK_DECODE_EVENT_DATA, min=0, max=0 },
    [24] = { id=24, func=BLOCK_DECODE_EVENT_DATA_TEXT, min=0, max=0 },
    [33] = { id=33, func=BLOCK_DECODE_EXTERNAL_DATA, min=0, max=0 } });
    
end

-------------------------------


MF_RegistrationArea_00_NID_AREA = ProtoField.uint8( "tcc.RegistrationArea.NID_AREA", "NID_AREA", base.DEC  )
function MSG_DECODE_RegistrationArea(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_NID_AREA(ctx,MF_RegistrationArea_00_NID_AREA, {  })
    
end

-------------------------------

MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit0 = ProtoField.uint32( "MF_StartUpMessage_00_B_TRAIN_CORE_STATUS.bit0", "Safety Halt, AOS", base.HEX, nil, 1 )
MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit1 = ProtoField.uint32( "MF_StartUpMessage_00_B_TRAIN_CORE_STATUS.bit1", "EA from driver", base.HEX, nil, 2 )
MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit2 = ProtoField.uint32( "MF_StartUpMessage_00_B_TRAIN_CORE_STATUS.bit2", "TIMS Integrity Broken", base.HEX, nil, 4 )
MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit3 = ProtoField.uint32( "MF_StartUpMessage_00_B_TRAIN_CORE_STATUS.bit3", "Braking event, AOS", base.HEX, nil, 8 )
MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit4 = ProtoField.uint32( "MF_StartUpMessage_00_B_TRAIN_CORE_STATUS.bit4", "Handling done", base.HEX, nil, 16 )
MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit5 = ProtoField.uint32( "MF_StartUpMessage_00_B_TRAIN_CORE_STATUS.bit5", "Train Idling", base.HEX, nil, 32 )
MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit6 = ProtoField.uint32( "MF_StartUpMessage_00_B_TRAIN_CORE_STATUS.bit6", "TIMS Integrity manual override from Driver", base.HEX, nil, 64 )
MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit7 = ProtoField.uint32( "MF_StartUpMessage_00_B_TRAIN_CORE_STATUS.bit7", "MA time out", base.HEX, nil, 128 )
MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit8 = ProtoField.uint32( "MF_StartUpMessage_00_B_TRAIN_CORE_STATUS.bit8", "ATP reset", base.HEX, nil, 256 )
MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit9 = ProtoField.uint32( "MF_StartUpMessage_00_B_TRAIN_CORE_STATUS.bit9", "ATP needs to be reset", base.HEX, nil, 512 )
MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit10 = ProtoField.uint32( "MF_StartUpMessage_00_B_TRAIN_CORE_STATUS.bit10", "ATP intervention", base.HEX, nil, 1024 )
MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit11 = ProtoField.uint32( "MF_StartUpMessage_00_B_TRAIN_CORE_STATUS.bit11", "Brake release wanted", base.HEX, nil, 2048 )
MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit12 = ProtoField.uint32( "MF_StartUpMessage_00_B_TRAIN_CORE_STATUS.bit12", "Manual TIMS confirmation", base.HEX, nil, 4096 )
MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit13 = ProtoField.uint32( "MF_StartUpMessage_00_B_TRAIN_CORE_STATUS.bit13", "Slip detected", base.HEX, nil, 8192 )
MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit14 = ProtoField.uint32( "MF_StartUpMessage_00_B_TRAIN_CORE_STATUS.bit14", "Free rolling", base.HEX, nil, 16384 )
MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit15 = ProtoField.uint32( "MF_StartUpMessage_00_B_TRAIN_CORE_STATUS.bit15", "Emergency Alert active", base.HEX, nil, 32768 )
MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit16 = ProtoField.uint32( "MF_StartUpMessage_00_B_TRAIN_CORE_STATUS.bit16", "Attention needed", base.HEX, nil, 65536 )
MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit17 = ProtoField.uint32( "MF_StartUpMessage_00_B_TRAIN_CORE_STATUS.bit17", "Not ready to drive", base.HEX, nil, 131072 )
MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit18 = ProtoField.uint32( "MF_StartUpMessage_00_B_TRAIN_CORE_STATUS.bit18", "Safe for boarding is active", base.HEX, nil, 262144 )

MF_StartUpMessage_00_B_TRAIN_CORE_STATUS = ProtoField.uint32( "tcc.StartUpMessage.B_TRAIN_CORE_STATUS", "B_TRAIN_CORE_STATUS", base.HEX  )

MF_StartUpMessage_01_L_LOCOMOTIVE = ProtoField.uint16( "tcc.StartUpMessage.L_LOCOMOTIVE", "L_LOCOMOTIVE", base.DEC  )

MF_StartUpMessage_02_Q_TIC_AVAILABLE = ProtoField.uint8( "tcc.StartUpMessage.Q_TIC_AVAILABLE", "Q_TIC_AVAILABLE", base.DEC  ,{ [0] = "TIC not available, train set manual configuration",[1] = "TIC is available, train set automatic configuration" }  )

MF_StartUpMessage_03_Q_TIMS_AVAILABLE = ProtoField.uint8( "tcc.StartUpMessage.Q_TIMS_AVAILABLE", "Q_TIMS_AVAILABLE", base.DEC  ,{ [0] = "TIMS not available/shall not be used",[1] = "TIMS available/shall be used" }  )
MF_StartUpMessage_04_B_DIRECTION_bit0 = ProtoField.uint8( "MF_StartUpMessage_04_B_DIRECTION.bit0", "Driving direction (0 - forward, 1 - Reverse)", base.HEX, nil, 1 )
MF_StartUpMessage_04_B_DIRECTION_bit1 = ProtoField.uint8( "MF_StartUpMessage_04_B_DIRECTION.bit1", "Orientation in track (0 - as track, 1 - opposite)", base.HEX, nil, 2 )
MF_StartUpMessage_04_B_DIRECTION_bit2 = ProtoField.uint8( "MF_StartUpMessage_04_B_DIRECTION.bit2", "Locomotive orientation (0- B-end facing cars, 1 - A-end facing cars)", base.HEX, nil, 4 )

MF_StartUpMessage_04_B_DIRECTION = ProtoField.uint8( "tcc.StartUpMessage.B_DIRECTION", "B_DIRECTION", base.HEX  )

MF_StartUpMessage_05_M_BRAKE_SYSTEM = ProtoField.uint8( "tcc.StartUpMessage.M_BRAKE_SYSTEM", "M_BRAKE_SYSTEM", base.DEC  ,{ [0] = "Undefined",[1] = "Pneumatic brake system",[2] = "ECPB" }  )
function MSG_DECODE_StartUpMessage(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_B_TRAIN_CORE_STATUS(ctx,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS, { MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit0,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit1,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit2,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit3,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit4,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit5,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit6,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit7,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit8,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit9,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit10,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit11,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit12,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit13,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit14,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit15,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit16,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit17,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit18 })
    FIELD_DECODE_L_LOCOMOTIVE(ctx,MF_StartUpMessage_01_L_LOCOMOTIVE, {  })
    FIELD_DECODE_Q_TIC_AVAILABLE(ctx,MF_StartUpMessage_02_Q_TIC_AVAILABLE, {  })
    FIELD_DECODE_Q_TIMS_AVAILABLE(ctx,MF_StartUpMessage_03_Q_TIMS_AVAILABLE, {  })
    FIELD_DECODE_B_DIRECTION(ctx,MF_StartUpMessage_04_B_DIRECTION, { MF_StartUpMessage_04_B_DIRECTION_bit0,MF_StartUpMessage_04_B_DIRECTION_bit1,MF_StartUpMessage_04_B_DIRECTION_bit2 })
    FIELD_DECODE_M_BRAKE_SYSTEM(ctx,MF_StartUpMessage_05_M_BRAKE_SYSTEM, {  })
    
    MSG_BLOCKS_DECODE(ctx, { 
    [37] = { id=37, func=BLOCK_DECODE_CONFIG_CONFIRMATION, min=0, max=1 },
    [23] = { id=23, func=BLOCK_DECODE_VEHICLE_ID_DATA, min=0, max=0 },
    [36] = { id=36, func=BLOCK_DECODE_VEHICLE_LIST_DATA, min=0, max=0 } });
    
end

-------------------------------


MF_ProtocolVersion_00_Q_PROTOCOL_RESPONSE = ProtoField.uint8( "tcc.ProtocolVersion.Q_PROTOCOL_RESPONSE", "Q_PROTOCOL_RESPONSE", base.DEC  ,{ [0] = "Protocol check request (TCC request)",[1] = "Protocols match (AOS response)",[2] = "Protocols mismatch, waiting for new version (AOS response)",[3] = "Unrecoverable mismatch (TCC termination of link)" }  )
function MSG_DECODE_ProtocolVersion(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_Q_PROTOCOL_RESPONSE(ctx,MF_ProtocolVersion_00_Q_PROTOCOL_RESPONSE, {  })
    
    MSG_BLOCKS_DECODE(ctx, { 
    [2] = { id=2, func=BLOCK_DECODE_PROTOCOL_VERSION, min=0, max=1 } });
    
end

-------------------------------


MF_TrainRegistrationInformation_00_NID_BG = ProtoField.uint16( "tcc.TrainRegistrationInformation.NID_BG", "NID_BG", base.DEC  )
MF_TrainRegistrationInformation_01_B_DIRECTION_bit0 = ProtoField.uint8( "MF_TrainRegistrationInformation_01_B_DIRECTION.bit0", "Driving direction (0 - forward, 1 - Reverse)", base.HEX, nil, 1 )
MF_TrainRegistrationInformation_01_B_DIRECTION_bit1 = ProtoField.uint8( "MF_TrainRegistrationInformation_01_B_DIRECTION.bit1", "Orientation in track (0 - as track, 1 - opposite)", base.HEX, nil, 2 )
MF_TrainRegistrationInformation_01_B_DIRECTION_bit2 = ProtoField.uint8( "MF_TrainRegistrationInformation_01_B_DIRECTION.bit2", "Locomotive orientation (0- B-end facing cars, 1 - A-end facing cars)", base.HEX, nil, 4 )

MF_TrainRegistrationInformation_01_B_DIRECTION = ProtoField.uint8( "tcc.TrainRegistrationInformation.B_DIRECTION", "B_DIRECTION", base.HEX  )
function MSG_DECODE_TrainRegistrationInformation(ctx)
    MSG_DECODE_HEADER(ctx);

    FIELD_DECODE_NID_BG(ctx,MF_TrainRegistrationInformation_00_NID_BG, {  })
    FIELD_DECODE_B_DIRECTION(ctx,MF_TrainRegistrationInformation_01_B_DIRECTION, { MF_TrainRegistrationInformation_01_B_DIRECTION_bit0,MF_TrainRegistrationInformation_01_B_DIRECTION_bit1,MF_TrainRegistrationInformation_01_B_DIRECTION_bit2 })
    
end

--- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

function MESSAGE_DISPATCH(ctx)
    n = MESSAGE_ID(ctx)
    if(n == 1) then
        MSG_DECODE_PositionReportRequest(ctx)
    elseif(n == 2) then
        MSG_DECODE_DriverLogonStatus(ctx)
    elseif(n == 3) then
        MSG_DECODE_EmergencyAlert(ctx)
    elseif(n == 4) then
        MSG_DECODE_MovementAuthority(ctx)
    elseif(n == 5) then
        MSG_DECODE_TrainSetup(ctx)
    elseif(n == 6) then
        MSG_DECODE_Unregistration(ctx)
    elseif(n == 7) then
        MSG_DECODE_ATORemoteControl(ctx)
    elseif(n == 8) then
        MSG_DECODE_StopTrain(ctx)
    elseif(n == 9) then
        MSG_DECODE_RevokeEmergencyAlert(ctx)
    elseif(n == 10) then
        MSG_DECODE_ApproximatePosition(ctx)
    elseif(n == 11) then
        MSG_DECODE_PossessionAcknowledge(ctx)
    elseif(n == 12) then
        MSG_DECODE_ShuntingAcknowledge(ctx)
    elseif(n == 13) then
        MSG_DECODE_JoinCommand(ctx)
    elseif(n == 14) then
        MSG_DECODE_ExternalData(ctx)
    elseif(n == 15) then
        MSG_DECODE_ConfigurationData(ctx)
    elseif(n == 16) then
        MSG_DECODE_CommandMessage(ctx)
    elseif(n == 17) then
        MSG_DECODE_Path(ctx)
    elseif(n == 18) then
        MSG_DECODE_RejectConfiguration(ctx)
    elseif(n == 19) then
        MSG_DECODE_AreaRequest(ctx)
    elseif(n == 20) then
        MSG_DECODE_YardAcknowledge(ctx)
    elseif(n == 128) then
        MSG_DECODE_DriverInformation(ctx)
    elseif(n == 129) then
        MSG_DECODE_StartUpMessage(ctx)
    elseif(n == 130) then
        MSG_DECODE_AbortSetup(ctx)
    elseif(n == 131) then
        MSG_DECODE_TrainRegistrationInformation(ctx)
    elseif(n == 132) then
        MSG_DECODE_PositionReport(ctx)
    elseif(n == 133) then
        MSG_DECODE_MessageAcknowledge(ctx)
    elseif(n == 134) then
        MSG_DECODE_RegistrationArea(ctx)
    elseif(n == 200) then
        MSG_DECODE_ProtocolVersion(ctx)
    else MSG_ERROR(ctx); end
end
messageids = { 
[10] = 'ApproximatePosition',
[19] = 'AreaRequest',
[7] = 'ATORemoteControl',
[16] = 'CommandMessage',
[15] = 'ConfigurationData',
[2] = 'DriverLogonStatus',
[3] = 'EmergencyAlert',
[14] = 'ExternalData',
[13] = 'JoinCommand',
[4] = 'MovementAuthority',
[17] = 'Path',
[1] = 'PositionReportRequest',
[11] = 'PossessionAcknowledge',
[18] = 'RejectConfiguration',
[9] = 'RevokeEmergencyAlert',
[12] = 'ShuntingAcknowledge',
[8] = 'StopTrain',
[5] = 'TrainSetup',
[6] = 'Unregistration',
[20] = 'YardAcknowledge',
[130] = 'AbortSetup',
[128] = 'DriverInformation',
[133] = 'MessageAcknowledge',
[132] = 'PositionReport',
[134] = 'RegistrationArea',
[129] = 'StartUpMessage',
[200] = 'ProtocolVersion',
[131] = 'TrainRegistrationInformation'
};


fld_messageid    = ProtoField.uint16("tcc.MessageID",  "MessageID",  base.DEC, messageids)
function MESSAGE_ID(ctx)

   r = ctx.tvbuf:range(ctx.off,1);
   ctx.stack:top():add(fld_messageid, r)
   ctx.off = ctx.off + 1;

   v = r:uint();
   dprint2("[+] MESSAGE_ID: ", v);
   return v;
end

--- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

local ef_too_short = ProtoExpert.new("tcc.too_short.expert", "TCC message too short",
                                     expert.group.MALFORMED, expert.severity.ERROR)

tcc = Proto("TCC", "TCC Message Protocol")

local TCC_HDR_LEN = 5

fld_version    = ProtoField.uint8 ( "tcc.stx",      "STX",          base.HEX)
fld_radioId    = ProtoField.uint16( "tcc.radioId",  "RadioId",      base.DEC)
fld_siteId     = ProtoField.uint8 ( "tcc.siteId",   "SiteId",       base.DEC)
fld_regionId   = ProtoField.uint8 ( "tcc.regionId", "RegionId",     base.DEC)
fld_len        = ProtoField.uint16( "tcc.len",      "Len",          base.DEC)
fld_timesender = ProtoField.uint16( "tcc.sender",   "Timesender",   base.DEC)
fld_timeref    = ProtoField.uint16( "tcc.ref",      "Timereceiver", base.DEC)
fld_crc        = ProtoField.bytes ( "tcc.crc",      "CRC",          base.NONE)

tcc.fields = {
   fld_version,
   fld_radioId,
   fld_siteId,
   fld_regionId,
   fld_len,
   fld_timesender,
   fld_timeref,
   fld_messageid,
   BF_ACOUSTIC_SIGNAL_00_NID_TRACK,BF_ACOUSTIC_SIGNAL_01_D_POSITION,BF_ACOUSTIC_SIGNAL_02_Q_SIGNAL,fld_ACOUSTIC_SIGNAL,BF_AOS_VERSION_00_M_VERSION,BF_AOS_VERSION_01_M_VERSION,BF_AOS_VERSION_02_M_VERSION,BF_AOS_VERSION_03_M_VERSION,BF_AOS_VERSION_04_M_VERSION,BF_AOS_VERSION_05_M_VERSION,fld_AOS_VERSION,fld_CANCEL_AREA,BF_ATO_PROFILE_CONTROL_00_Q_PROFILE,BF_ATO_PROFILE_CONTROL_01_D_REVERSE,BF_ATO_PROFILE_CONTROL_02_B_DIRECTION,BF_ATO_PROFILE_CONTROL_02_B_DIRECTION_bit0,BF_ATO_PROFILE_CONTROL_02_B_DIRECTION_bit1,BF_ATO_PROFILE_CONTROL_02_B_DIRECTION_bit2,fld_ATO_PROFILE_CONTROL,BF_CONFIGURATION_DATA_00_TID_TEXT_STRING,BF_CONFIGURATION_DATA_01_TID_TEXT_STRING,fld_CONFIGURATION_DATA,BF_CONFIRM_CONFIG_00_Q_SETUP,fld_CONFIRM_CONFIG,BF_ATO_STOP_POSITION_00_NID_TRACK,BF_ATO_STOP_POSITION_01_D_POSITION,fld_ATO_STOP_POSITION,fld_SAFE_FOR_BOARDING_ACTIVATE,BF_BALISE_DATA_00_NID_TRACK,BF_BALISE_DATA_01_D_POSITION,BF_BALISE_DATA_02_NID_BG,fld_BALISE_DATA,BF_LOCATION_DATA_00_TID_LOCATION,BF_LOCATION_DATA_01_NID_LOCATION_TYPE,fld_LOCATION_DATA,BF_AREAS_00_NID_AREA,fld_AREAS,BF_CAR2_CONFIG_DATA_00_NID_VEHICLE,BF_CAR2_CONFIG_DATA_01_Q_PRESSURE_SENSOR,fld_CAR2_CONFIG_DATA,BF_CAR3_CONFIG_DATA_00_NID_VEHICLE,BF_CAR3_CONFIG_DATA_01_Q_PRESSURE_SENSOR,BF_CAR3_CONFIG_DATA_02_T_UNLOAD_ACTION,BF_CAR3_CONFIG_DATA_03_T_UNLOAD_HATCH_OPEN,fld_CAR3_CONFIG_DATA,BF_CAR_STATUS_DATA_00_NID_VEHICLE,BF_CAR_STATUS_DATA_01_B_TIC_STATUS,BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit0,BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit1,BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit2,BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit3,BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit4,BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit5,BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit6,BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit7,BF_CAR_STATUS_DATA_01_B_TIC_STATUS_bit8,fld_CAR_STATUS_DATA,BF_CEILING_SPEED_DATA_00_NID_TRACK,BF_CEILING_SPEED_DATA_01_D_POSITION,BF_CEILING_SPEED_DATA_02_V_SPEED,BF_CEILING_SPEED_DATA_03_Q_TRAIN_END,BF_CEILING_SPEED_DATA_04_Q_SPEED,fld_CEILING_SPEED_DATA,BF_CONFIG_CONFIRMATION_00_Q_ACKNOWLEDGE,fld_CONFIG_CONFIRMATION,BF_ERROR_MESSAGE_DATA_00_Q_ERROR_LEVEL,BF_ERROR_MESSAGE_DATA_01_NID_ERROR_NO,fld_ERROR_MESSAGE_DATA,BF_ETA_CONFIRMATION_00_Q_ACKNOWLEDGE,BF_ETA_CONFIRMATION_01_T_CLOCK,fld_ETA_CONFIRMATION,BF_EVENT_DATA_00_NID_EVENT_NO,fld_EVENT_DATA,BF_ETA_REQUEST_00_T_CLOCK,fld_ETA_REQUEST,BF_EVENT_DATA_TEXT_00_NID_EVENT_NO,BF_EVENT_DATA_TEXT_01_TID_TEXT_STRING,fld_EVENT_DATA_TEXT,BF_EXTERNAL_DATA_00_NID_SYSTEM,BF_EXTERNAL_DATA_01_N_LENGTH,fld_EXTERNAL_DATA,BF_GRADIENT_DATA_00_NID_TRACK,BF_GRADIENT_DATA_01_D_POSITION,BF_GRADIENT_DATA_02_G_GRADIENT,fld_GRADIENT_DATA,BF_KEEP_TRACK_DATA_00_NID_TRACK,BF_KEEP_TRACK_DATA_01_D_POSITION,fld_KEEP_TRACK_DATA,BF_LAST_BALISE_00_NID_BG,fld_LAST_BALISE,fld_LOAD_FINISHED,BF_LOCOMOTIVE_POWER_00_Q_POWER,fld_LOCOMOTIVE_POWER,BF_MAX_SEARCH_DIST_00_D_MAX_DIST,fld_MAX_SEARCH_DIST,BF_PANTOGRAPH_SHIFT_00_NID_TRACK,BF_PANTOGRAPH_SHIFT_01_D_POSITION,BF_PANTOGRAPH_SHIFT_02_Q_PANTO_POSITION,BF_PANTOGRAPH_SHIFT_03_Q_SUPERVISION,BF_PANTOGRAPH_SHIFT_04_Q_PANTO_POSITION,BF_PANTOGRAPH_SHIFT_05_Q_SUPERVISION,fld_PANTOGRAPH_SHIFT,BF_PANTO_START_POSITION_00_Q_PANTO_POSITION,fld_PANTO_START_POSITION,BF_VEHICLE_TYPE_DATA_00_NID_VEHICLE_TYPE,BF_VEHICLE_TYPE_DATA_01_W_WEIGHT,BF_VEHICLE_TYPE_DATA_02_W_WEIGHT,BF_VEHICLE_TYPE_DATA_03_W_WEIGHT,BF_VEHICLE_TYPE_DATA_04_W_WEIGHT,BF_VEHICLE_TYPE_DATA_05_W_WEIGHT,BF_VEHICLE_TYPE_DATA_06_W_WEIGHT,fld_VEHICLE_TYPE_DATA,fld_PARTLY_MA,fld_POSSESSION_REQUEST,BF_PROTOCOL_VERSION_00_M_VERSION,BF_PROTOCOL_VERSION_01_M_VERSION,BF_PROTOCOL_VERSION_02_M_VERSION,fld_PROTOCOL_VERSION,BF_SPEED_CHANGE_POSITION_00_NID_TRACK,BF_SPEED_CHANGE_POSITION_01_D_POSITION,BF_SPEED_CHANGE_POSITION_02_V_SPEED,fld_SPEED_CHANGE_POSITION,fld_RELEASE_BRAKE,fld_REQ_CAR_STATUS,BF_SET_TIME_00_T_CLOCK,fld_SET_TIME,fld_SHUNTING_REQUEST,BF_TEXT_MESSAGE_00_M_TEXT,fld_TEXT_MESSAGE,BF_TRACKS_00_NID_TRACK,fld_TRACKS,BF_TRACK_DATA_ITEM_00_Q_TRACK_DATA_TYPE,BF_TRACK_DATA_ITEM_01_NID_TRACK,BF_TRACK_DATA_ITEM_02_D_POSITION,BF_TRACK_DATA_ITEM_03_Q_DIRECTION,BF_TRACK_DATA_ITEM_04_N_VALUE,fld_TRACK_DATA_ITEM,BF_TRACK_DATA_00_NID_TRACK,BF_TRACK_DATA_01_D_POSITION,BF_TRACK_DATA_02_D_POSITION,BF_TRACK_DATA_03_B_DIRECTION,BF_TRACK_DATA_03_B_DIRECTION_bit0,BF_TRACK_DATA_03_B_DIRECTION_bit1,BF_TRACK_DATA_03_B_DIRECTION_bit2,BF_TRACK_DATA_04_NID_PREVIOUS_TRACK,fld_TRACK_DATA,BF_TRAIN_NAME_00_TID_TRAIN_NAME,fld_TRAIN_NAME,BF_VEHICLE_ID_DATA_00_NID_VEHICLE_TYPE,BF_VEHICLE_ID_DATA_01_NID_VEHICLE,BF_VEHICLE_ID_DATA_02_TID_VEHICLE_NAME,fld_VEHICLE_ID_DATA,BF_VEHICLE_LIST_DATA_00_N_VALUE,BF_VEHICLE_LIST_DATA_01_NID_VEHICLE_TYPE,fld_VEHICLE_LIST_DATA,BF_WAITING_TIME_00_T_WAITING_TIME,fld_WAITING_TIME,BF_DEPARTURE_WARNING_00_Q_SIGNAL,fld_DEPARTURE_WARNING,fld_YARD_REQUEST,MF_ApproximatePosition_00_NID_MSG,MF_ApproximatePosition_01_NID_TRACK,MF_ApproximatePosition_02_D_POSITION,MF_ApproximatePosition_03_M_END_OF_MESSAGE,MF_AreaRequest_00_M_END_OF_MESSAGE,MF_ATORemoteControl_00_V_SPEED,MF_ATORemoteControl_01_T_REMOTE,MF_ATORemoteControl_02_B_DIRECTION,MF_ATORemoteControl_02_B_DIRECTION_bit0,MF_ATORemoteControl_02_B_DIRECTION_bit1,MF_ATORemoteControl_02_B_DIRECTION_bit2,MF_ATORemoteControl_03_M_END_OF_MESSAGE,MF_CommandMessage_00_M_END_OF_MESSAGE,MF_ConfigurationData_00_NID_MSG,MF_ConfigurationData_01_M_END_OF_MESSAGE,MF_DriverLogonStatus_00_Q_LOGON_STATUS,MF_DriverLogonStatus_01_M_END_OF_MESSAGE,MF_EmergencyAlert_00_Q_ALERT,MF_EmergencyAlert_01_M_END_OF_MESSAGE,MF_ExternalData_00_M_END_OF_MESSAGE,MF_JoinCommand_00_M_END_OF_MESSAGE,MF_MovementAuthority_00_NID_MSG,MF_MovementAuthority_01_T_VALID,MF_MovementAuthority_02_V_SPEED,MF_MovementAuthority_03_G_GRADIENT,MF_MovementAuthority_04_M_LOADED,MF_MovementAuthority_05_N_ADHESION,MF_MovementAuthority_06_B_DIRECTION,MF_MovementAuthority_06_B_DIRECTION_bit0,MF_MovementAuthority_06_B_DIRECTION_bit1,MF_MovementAuthority_06_B_DIRECTION_bit2,MF_MovementAuthority_07_Q_ROUTE_TYPE,MF_MovementAuthority_08_NID_TRACK,MF_MovementAuthority_09_D_POSITION,MF_MovementAuthority_10_NID_TRACK,MF_MovementAuthority_11_D_POSITION,MF_MovementAuthority_12_D_MA_MARGIN,MF_MovementAuthority_13_D_OVERLAP,MF_MovementAuthority_14_M_END_OF_MESSAGE,MF_Path_00_V_SPEED,MF_Path_01_NID_TRACK,MF_Path_02_D_POSITION,MF_Path_03_M_END_OF_MESSAGE,MF_PositionReportRequest_00_M_END_OF_MESSAGE,MF_PossessionAcknowledge_00_Q_ACKNOWLEDGE,MF_PossessionAcknowledge_01_V_SPEED,MF_PossessionAcknowledge_02_M_END_OF_MESSAGE,MF_RejectConfiguration_00_Q_UNREGISTRATION,MF_RejectConfiguration_01_M_END_OF_MESSAGE,MF_RevokeEmergencyAlert_00_M_END_OF_MESSAGE,MF_ShuntingAcknowledge_00_Q_ACKNOWLEDGE,MF_ShuntingAcknowledge_01_V_SPEED,MF_ShuntingAcknowledge_02_M_END_OF_MESSAGE,MF_StopTrain_00_Q_STOP,MF_StopTrain_01_M_END_OF_MESSAGE,MF_TrainSetup_00_NID_MSG,MF_TrainSetup_01_Q_SETUP,MF_TrainSetup_02_Q_TS_STATE,MF_TrainSetup_03_V_SPEED,MF_TrainSetup_04_L_TRAIN,MF_TrainSetup_05_Q_TIC_AVAILABLE,MF_TrainSetup_06_Q_TIMS_AVAILABLE,MF_TrainSetup_07_B_DIRECTION,MF_TrainSetup_07_B_DIRECTION_bit0,MF_TrainSetup_07_B_DIRECTION_bit1,MF_TrainSetup_07_B_DIRECTION_bit2,MF_TrainSetup_08_M_BRAKE_SYSTEM,MF_TrainSetup_09_M_END_OF_MESSAGE,MF_Unregistration_00_Q_UNREGISTRATION,MF_Unregistration_01_M_END_OF_MESSAGE,MF_YardAcknowledge_00_Q_ACKNOWLEDGE,MF_YardAcknowledge_01_V_SPEED,MF_YardAcknowledge_02_M_END_OF_MESSAGE,MF_AbortSetup_00_Q_ABORT,MF_AbortSetup_01_M_END_OF_MESSAGE,MF_DriverInformation_00_TID_DRIVER,MF_DriverInformation_01_TID_PASSWORD,MF_DriverInformation_02_M_END_OF_MESSAGE,MF_MessageAcknowledge_00_NID_MSG,MF_MessageAcknowledge_01_Q_MESSAGE_STATUS,MF_MessageAcknowledge_02_M_END_OF_MESSAGE,MF_PositionReport_00_NID_TRACK,MF_PositionReport_01_D_POSITION,MF_PositionReport_02_NID_TRACK,MF_PositionReport_03_D_POSITION,MF_PositionReport_04_Q_POSITION,MF_PositionReport_05_B_DIRECTION,MF_PositionReport_05_B_DIRECTION_bit0,MF_PositionReport_05_B_DIRECTION_bit1,MF_PositionReport_05_B_DIRECTION_bit2,MF_PositionReport_06_V_SPEED,MF_PositionReport_07_B_TRAIN_CORE_STATUS,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit0,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit1,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit2,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit3,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit4,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit5,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit6,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit7,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit8,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit9,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit10,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit11,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit12,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit13,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit14,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit15,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit16,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit17,MF_PositionReport_07_B_TRAIN_CORE_STATUS_bit18,MF_PositionReport_08_D_WINDOW,MF_PositionReport_09_NID_TRACK,MF_PositionReport_10_D_POSITION,MF_PositionReport_11_Q_ATP_MODE,MF_PositionReport_12_Q_ATO_MODE,MF_PositionReport_13_M_BRAKE_SYSTEM,MF_PositionReport_14_D_BRAKE_DISTANCE,MF_PositionReport_15_M_END_OF_MESSAGE,MF_RegistrationArea_00_NID_AREA,MF_RegistrationArea_01_M_END_OF_MESSAGE,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit0,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit1,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit2,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit3,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit4,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit5,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit6,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit7,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit8,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit9,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit10,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit11,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit12,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit13,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit14,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit15,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit16,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit17,MF_StartUpMessage_00_B_TRAIN_CORE_STATUS_bit18,MF_StartUpMessage_01_L_LOCOMOTIVE,MF_StartUpMessage_02_Q_TIC_AVAILABLE,MF_StartUpMessage_03_Q_TIMS_AVAILABLE,MF_StartUpMessage_04_B_DIRECTION,MF_StartUpMessage_04_B_DIRECTION_bit0,MF_StartUpMessage_04_B_DIRECTION_bit1,MF_StartUpMessage_04_B_DIRECTION_bit2,MF_StartUpMessage_05_M_BRAKE_SYSTEM,MF_StartUpMessage_06_M_END_OF_MESSAGE,MF_ProtocolVersion_00_Q_PROTOCOL_RESPONSE,MF_ProtocolVersion_01_M_END_OF_MESSAGE,MF_TrainRegistrationInformation_00_NID_BG,MF_TrainRegistrationInformation_01_B_DIRECTION,MF_TrainRegistrationInformation_01_B_DIRECTION_bit0,MF_TrainRegistrationInformation_01_B_DIRECTION_bit1,MF_TrainRegistrationInformation_01_B_DIRECTION_bit2,MF_TrainRegistrationInformation_02_M_END_OF_MESSAGE,
   fld_crc
};

function tcc.dissector(tvbuf, pktinfo, root)
   dprint2("tcc.dissector called")
   pktinfo.cols.protocol:set("TCC")

   local pktlen = tvbuf:reported_length_remaining()
   local tree = root:add(tcc, tvbuf:range(0,pktlen))

   -- now let's check it's not too short
   if pktlen < TCC_HDR_LEN then
      tree:add_proto_expert_info(ef_too_short)
      dprint("packet length",pktlen,"too short")
      return
   end

   tree:add(fld_version,    tvbuf:range(0,1))
   tree:add(fld_radioId,    tvbuf:range(1,2))
   tree:add(fld_siteId,     tvbuf:range(3,1))
   tree:add(fld_regionId,   tvbuf:range(4,1))
   tree:add(fld_len,        tvbuf:range(5,2))
   tree:add(fld_timesender, tvbuf:range(7,2))
   tree:add(fld_timeref,    tvbuf:range(9,2))

   local mlen = tvbuf:range(5,2):uint()
   local crclen = 8
   dprint2("pklen-crc:" , (pktlen-crclen), " 11+mlen:", (11+mlen), " crclen: ", crclen )

   ctx = { off = 11,
	   tvbuf = tvbuf,
	   pktinfo = pktinfo,
	   stack = utilwireshark.stack:Create(),
	   limit = pktlen - crclen --- crc
   };
   ctx.stack:push(tree);
   MESSAGE_DISPATCH(ctx);
   ctx.stack:pop(1);

   tree:add(fld_crc,    tvbuf:range(pktlen - crclen, crclen))
end

DissectorTable.get("tcp.port"):add(default_settings.port, tcc)
