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

local ef_too_short = ProtoExpert.new("dmi.too_short.expert", "dmi message too short",
                                     expert.group.MALFORMED, expert.severity.ERROR)

dmi = Proto("DMI", "DMI Message Protocol")

local DMI_HDR_LEN = 5

fld_startchar = ProtoField.uint8 ( "dmi.ms",      "Message start char",      base.HEX)
fld_ht        = ProtoField.uint8 ( "dmi.ht",      "Header type",             base.HEX)
fld_mn        = ProtoField.uint8 ( "dmi.mn",      "Message number",          base.HEX)
fld_dl        = ProtoField.uint16( "dmi.dl",      "Data length",             base.HEX)
fld_messagetype = ProtoField.uint8( "dmi.messagetype",      "Message type",
				    base.DEC, {
				       [1] = "ATP-mode and states",
				       [3] = "Driver info"      ,	
				       [5] = "Speed & distance",	
				       [8] = "TrainConfigData"	 ,       
				       [10]= "Error messages"	  ,       
				       [13]= "ManualConfigSelected",	 
				       [18]= "Time",	                 
				       [19]= "CarNameList",	         
				       [20]= "TrainVsTrackDirWanted",	 
				       [22]= "DMI_Status",	        
				       [23]= "CarListAnswer, CarList",   	 
				       [26]= "Confirmation",	         
				       [27]= "DriverIDandPassword",	 
				       [33]= "LocoVsTrainDir",	         
				       [34]= "DMI_ToATP_Data",	         
				       [36]= "SubMenuButton",	         
				       [39]= "TrainVsTrackDir",	         
				       [49]= "RegistrationArea",	
				       [40]= "ErasePlanningArea",	 
				       [42]= "TextMessage",	         
				       [44]= "CarStatusList",	         
				       [46]= "CeilingSpeedList",	
				       [47]= "GradientDataList",	
				       [48]= "DMIStartup",	        
				       [51]= "ReConfigurationSelected",	
				       [52]= "TrainName",	        
				       [57]= "PredefinedTextMessage",	
				       [58]= "AtpNotification",	        
				       [60]= "StartUpHistory",	        
				       [61]= "Version",	                
				       [62]= "AreaRequest"
})

fld_atp_mode_data0 = ProtoField.uint8( "dmi.atp_mode", "ATP Mode",
				       base.DEC, {
					  [0] = "Undefined",
					  [1] = "Power up" ,
					  [2] = "Train configuration",
					  [3] = "Train registration",
					  [4] = "Balise Search",
					  [5] = "Normal",
					  [6] = "Shunting",
					  [7] = "Location",
					  [8] = "AutomaticUnload",
					  [9] = "Yard",
					  [10] = "Unregistered",
					  [11] = "PoweringDown",
					  [12] = "SafetyHalt",
					  [13] = "Sleeping",
					  [14] = "StaffResponsible",
					  [15] = "ShuntingRoute",
					  [16] = "Possession",
					  [17] = "Split",
					  [18] = "Join",
					  [19] = "SafeBrakeToStop" })


fld_atp_state_data1 = ProtoField.uint8( "dmi.atp_state", "ATP State",
				       base.DEC, {
					  [0] = "Undefined",
					  [1] = "BasicSystemStartUp",
					  [2] = "ApplicationStartUp" ,
					  [3] = "Inactive", 
					  [4] = "ActivationInitiation", 
					  [5] = "ActivationTest", 
					  [6] = "Active", 
					  [7] = "FatalFailureState",
					  [8] = "SystemRestart",
					  [9] = "Power down" });

fld_atp_mode_data2_config = ProtoField.uint8( "dmi.substate_config", "Config substate",
				       base.DEC, {
					  [0] = "Undefined",
					  [1] = "Manual" ,
					  [2] = "ReConfig(or Re-registration)" });
fld_atp_mode_data2_powerup = ProtoField.uint8( "dmi.substate_powerup", "Powerup substate",
				       base.DEC, {
					  [0] = "Undefined",
					  [1] = "Allow driver to select Mode config, yard" });

fld_atp_mode_data2_other = ProtoField.uint8( "dmi.substate_other", "ATP State substate", base.DEC);



function ATP_mode_and_states(tree, tvbuf)
   local mode = tvbuf:range(6,1)
   tree:add(fld_atp_mode_data0, mode)
   tree:add(fld_atp_state_data1, tvbuf:range(7,1))
   local v = mode:int();
   if (v == 1) then --- power up
      tree:add(fld_atp_mode_data2_powerup, tvbuf:range(8,1))
   elseif (v == 2) then --- train config
      tree:add(fld_atp_mode_data2_config, tvbuf:range(8,1))
   else
      tree:add(fld_atp_mode_data2_other, tvbuf:range(8,1))
   end
   --- todo:
end


function Driver_info(tree, tvbuf)
   --- todo:
end
function Speed_distance(tree, tvbuf)
   --- todo:
end
function TrainConfigData(tree, tvbuf)
   --- todo:
end
function Error_messages(tree, tvbuf)       
   --- todo:
end
function ManualConfigSelected(tree, tvbuf)
   --- todo:
end
function Time_Message(tree, tvbuf)          
   --- todo:
end
function CarNameList(tree, tvbuf)	         
   --- todo:
end
function TrainVsTrackDirWanted(tree, tvbuf)
   --- todo:
end



fld_dmi_status = ProtoField.uint16( "dmi.dmi_status.data0", "DMI Status word", base.HEX)
function DMI_Status(tree, tvbuf)
   tree:add(fld_dmi_status, tvbuf:range(6,2));
end


function CarListAnswer_CarList(tree, tvbuf)
   --- todo:
end
function Confirmation(tree, tvbuf) 
   --- todo:
end
function DriverIDandPassword(tree, tvbuf)
   --- todo:
end
function LocoVsTrainDir(tree, tvbuf)	         
   --- todo:
end
function DMI_ToATP_Data(tree, tvbuf)	         
   --- todo:
end
function SubMenuButton(tree, tvbuf)        
   --- todo:
end
function TrainVsTrackDir(tree, tvbuf)	         
   --- todo:
end
function RegistrationArea(tree, tvbuf)	
   --- todo:
end
function ErasePlanningArea(tree, tvbuf)	 
   --- todo:
end
function TextMessage(tree, tvbuf)	         
   --- todo:
end
function CarStatusList(tree, tvbuf)	         
   --- todo:
end
function CeilingSpeedList(tree, tvbuf)	
   --- todo:
end
function GradientDataList(tree, tvbuf)	
   --- todo:
end
function DMIStartup(tree, tvbuf)	        
   --- todo:
end
function ReConfigurationSelected(tree, tvbuf)
   --- todo:
end
function TrainName(tree, tvbuf)
   --- todo:
end
function PredefinedTextMessage(tree, tvbuf)
   --- todo:
end
function AtpNotification(tree, tvbuf)        
   --- todo:
end
function StartUpHistory(tree, tvbuf)	        
   --- todo:
end
function Version(tree, tvbuf)	                
   --- todo:
end
function AreaRequest(tree, tvbuf)
   --- todo:
end


dmi.fields = {
   fld_startchar,
   fld_ht,
   fld_mn,
   fld_dl,
   fld_messagetype,
   
   fld_atp_mode_data0,
   fld_atp_state_data1,
   fld_atp_mode_data2_config,
   fld_atp_mode_data2_powerup,
   fld_atp_mode_data2_other,

   fld_dmi_status

   
};

function dmi.dissector(tvbuf, pktinfo, root)
   dprint2("dmi.dissector called")
   pktinfo.cols.protocol:set("DMI")

   local pktlen = tvbuf:reported_length_remaining()
   local tree = root:add(dmi, tvbuf:range(0,pktlen))

   -- now let's check it's not too short
   if pktlen < DMI_HDR_LEN then
      tree:add_proto_expert_info(ef_too_short)
      dprint("packet length",pktlen,"too short")
      return
   end

   tree:add(fld_startchar,    tvbuf:range(0,1))
   tree:add(fld_ht,    tvbuf:range(1,1))
   tree:add(fld_mn,    tvbuf:range(2,1))
   tree:add(fld_dl,    tvbuf:range(3,1))

   local d = tvbuf:range(5,1); 
   tree:add(fld_messagetype,   d)
   local v = d:int();

   local lookup = {
      [1] = ATP_mode_and_states,
      [3] = Driver_info     ,	
      [5] = Speed_distance,	
      [8] = TrainConfigData	 ,       
      [10]= Error_messages	  ,       
      [13]= ManualConfigSelected,	 
      [18]= Time_Message,	                 
      [19]= CarNameList,	         
      [20]= TrainVsTrackDirWanted,	 
      [22]= DMI_Status,	        
      [23]= CarListAnswer_CarList,   	 
      [26]= Confirmation,	         
      [27]= DriverIDandPassword,	 
      [33]= LocoVsTrainDir,	         
      [34]= DMI_ToATP_Data,	         
      [36]= SubMenuButton,	         
      [39]= TrainVsTrackDir,	         
      [49]= RegistrationArea,	
      [40]= ErasePlanningArea,	 
      [42]= TextMessage,	         
      [44]= CarStatusList,	         
      [46]= CeilingSpeedList,	
      [47]= GradientDataList,	
      [48]= DMIStartup,	        
      [51]= ReConfigurationSelected,	
      [52]= TrainName,	        
      [57]= PredefinedTextMessage,	
      [58]= AtpNotification,	        
      [60]= StartUpHistory,	        
      [61]= Version,	                
      [62]= AreaRequest
   };

   if (lookup[v] ~= nil) then
      lookup[v](tree, tvbuf)
   end
   
end

DissectorTable.get("tcp.port"):add(default_settings.port, dmi)


