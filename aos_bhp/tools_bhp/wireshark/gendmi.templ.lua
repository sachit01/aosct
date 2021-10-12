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

{{code}}

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

   {{fldlist['j':',']}},

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
