object DataModuleDMI: TDataModuleDMI
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 294
  Width = 491
  object ClientDataSetDMIEvents: TClientDataSet
    Aggregates = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterPost = ClientDataSetDMIEventsAfterScroll
    AfterScroll = ClientDataSetDMIEventsAfterScroll
    Left = 80
    Top = 48
  end
  object ClientDataSetDMITrainComp: TClientDataSet
    Aggregates = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    AfterScroll = ClientDataSetDMITrainCompAfterScroll
    Left = 272
    Top = 56
  end
end
