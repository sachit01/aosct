#*******************************************************************************
#
# (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
#
# We reserve all rights in this file and in the information
# contained therein. Reproduction, use or disclosure to third
# parties without written authority is strictly forbidden.
#
#  DESCRIPTION:
#  Templates for the Event-XML body.
#
#******************************************************************************

#******************************************************************************
#
# REVISION HISTORY :
#
# Date          Name        Changes
# ---------------------------------------------------------------------------
# 2018-08-02    marlundg    Created
#
#******************************************************************************

#******************************************************************************
# INCLUDE FILES
#******************************************************************************

#******************************************************************************
# DECLARATIONS
#******************************************************************************
templateXML = """    <rcs:LogEventTypeInfo>
      <eventType>products/AOS/event_types/{id}/{isDynamic}</eventType>
      <description>
        <desc>
          <lang>en</lang>
          <text>{description}</text>
        </desc>
      </description>
      <severity>{severity}</severity>
      {ifDynamicText}
    </rcs:LogEventTypeInfo>
"""

templateDynamicXML = """  <interpretation>
        <list>
          <item>
            <key>dynamic_text</key>
            <description>
              <desc>
                <lang>en</lang>
                <text>DynamicText</text>
              </desc>
            </description>
            <dataType>string</dataType>
          </item>
          <text>
            <desc>
              <lang>en</lang>
              <text>{description}{dynamicVar}</text>
            </desc>
          </text>
        </list>
      </interpretation>"""

templateBodyXML = """<?xml version="1.0" encoding="UTF-8"?>
<rcs:Message xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
             xmlns:rcs="http://www.transport.bombardier.com/2014/RcsCmm">
  <hdr>
      <refBase>http://www.bhp.com/waio</refBase>
      <source>systemUnits/AOS</source>
  </hdr>
  <data>
{body}  </data>
</rcs:Message>
"""
