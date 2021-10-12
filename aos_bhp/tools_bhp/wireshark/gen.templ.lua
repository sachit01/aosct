
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

{{code}}

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
   {{fldlist['j':',']}},
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
