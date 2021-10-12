* Wireshark dissector gen-tcc-dissect.lua

  - gen-tcc-dissect.lua is generated from the XML definition in XML_Ver5.8/*
    via script "python gendissect.py -u"
    Gnu make can be used to start generation of dissector and start tests on sample packet captures.
  - to test you can run (with wireshark/tshark in PATH):
    tshark -X lua_script:gen-tcc-dissect.lua -r packet.test0/tsetup.pcap -V
    packet.test0/tsetup.pcap is a pregenerated TCC packet capture gen-tcc-dissect.lua loads json.lua and utilwireshark.lua in the same directory

** Features

  - Fields are searchable, i.e. filter with "tcc.Messageid==132"
  - Bitfiels are decoded
  - String mapping for integers as defined in the "special" tags

** Howto run

   - Dissect with tshark (with wireshark/tshark in PATH and when standing inside .):
     tshark -X lua_script:gen-tcc-dissect.lua -V
   - Use Wireshark GUI (with wireshark/tshark in PATH and when standing inside .):
     wireshark -X lua_script:gen-tcc-dissect.lua 
   - The lua scripts can also be installed in %appdata%/wireshark/plugins/ .
     gen-tcc-dissect.lua requires the files
     + json.lua
     + utilwireshark.lua
     to be copied in the same directory
     
** Example dissect

    ...
    [SEQ/ACK analysis]
        [Bytes in flight: 76]
        [Bytes sent since last PSH flag: 76]
    TCP payload (76 bytes)
TCC Message Protocol
    STX: 0x02
    RadioId: 2
    SiteId: 6
    RegionId: 8
    Len: 57
    Timesender: 3663
    Timereceiver: 62248
    MessageID: PositionReport (132)
    Trailing position, Track: 0
    Position of Trailing end of train in Track: 0
    Leading position, Track: 0
    Position of Leading end of train in Track: 0
    Q_POSITION: Unknown (0)
    B_DIRECTION: 0x00
        .... ...0 = Driving direction. 0 = forward (locomotive first), 1 = reverse: 0x0
        .... ..0. = Orientation in Track. 0 = as track (loco closest to leg 1), 1 = opposite: 0x0
        .... .0.. = Locomotive orientation. 0 = B end facing cars, 1 = A end facing cars: 0x0
    Current speed: 0
    B_TRAIN_CORE_STATUS: 0x00000000
        .... .... .... .... .... .... .... ...0 = Safety Halt, AOS: 0x0
        .... .... .... .... .... .... .... ..0. = EA from driver: 0x0
        .... .... .... .... .... .... .... .0.. = TIMS Integrity Broken: 0x0
        .... .... .... .... .... .... .... 0... = Braking event, AOS: 0x0
        .... .... .... .... .... .... ...0 .... = Handling done: 0x0
        .... .... .... .... .... .... ..0. .... = Train Idling: 0x0
        .... .... .... .... .... .... .0.. .... = TIMS Integrity manual override from Driver: 0x0
        .... .... .... .... .... .... 0... .... = MA time out: 0x0
        .... .... .... .... .... ...0 .... .... = ATP reset: 0x0
        .... .... .... .... .... ..0. .... .... = ATP needs to be reset: 0x0
        .... .... .... .... .... .0.. .... .... = ATP intervention: 0x0
        .... .... .... .... .... 0... .... .... = Brake release requested: 0x0
        .... .... .... .... ...0 .... .... .... = Manual TIMS confirmation: 0x0
        .... .... .... .... ..0. .... .... .... = Slip detected: 0x0
        .... .... .... .... .0.. .... .... .... = Free rolling: 0x0
        .... .... .... .... 0... .... .... .... = EA active: 0x0
        .... .... .... ...0 .... .... .... .... = Attention needed: 0x0
        .... .... .... ..0. .... .... .... .... = Not ready to drive: 0x0
        .... .... .... .0.. .... .... .... .... = Safe for boarding is active: 0x0
    D_WINDOW: 500
    Target track: 0
    Target position: 0
    Q_ATP_MODE: Registration (3)
    Q_ATO_MODE: Manual (1)
    M_BRAKE_SYSTEM: ECPB (2)
    D_BRAKE_DISTANCE: 16777215
    TRAIN_NAME
        TID_TRAIN_NAME: BHP-TRAIN
    CRC: 70ab756a1bc6248a
    ...
    
