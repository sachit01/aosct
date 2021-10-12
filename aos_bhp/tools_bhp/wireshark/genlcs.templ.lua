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


{{code}}


lcs.fields = {
   fld_header,
   fld_messagetype,
   fld_messageversion,
   fld_flags,
   fld_datalength,
   fld_sendmsgnum,


   {{fldlist['j':',']}},
   

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

