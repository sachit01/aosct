
unit UnitFatal;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, UnitMainArea, MMITypes, UnitExchange, ColorButton2, MMIStd,
  GestureMgr;

type
  TFormFatal = class(TForm)
    FatalLabel1: TLabel;
    FatalLabel2: TLabel;
    GestureManager: TGestureManager;
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure SetMessage;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    Procedure ScreenChange;
  end;

var
  FormFatal: TFormFatal;

implementation

uses UnitMMIFrame, UnitViewLog, UnitAOSTerminal;

{$R *.DFM}

const
  FatalFormLogErrorCreate = 1;

procedure TFormFatal.FormCreate(Sender: TObject);
begin


  ScreenChange;
  SetMessage;

end;

procedure TFormFatal.FormGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
              { Allow alternative screens }
  if (FormMMIFrame.AltScreens > 0) then
  begin

    if (EventInfo.GestureID = sgiRight) then
    begin
      if FormViewLog.Visible then
        FormViewLog.Hide
      else
      begin
        FormViewLog.LogRoot := FormMMIFrame.LogPath;
        FormViewLog.Show;
      end;
    end
    else if (EventInfo.GestureID = sgiLeft) then
    begin
      if FormAOSTerminal.Visible then
        FormAOSTerminal.Hide
      else
        FormAOSTerminal.Show;

    end;

  end;

end;

procedure TFormFatal.SetMessage;
begin

  FatalLabel1.Caption:=InterfaceString(ifFatal1) ;
  FatalLabel2.Caption:=InterfaceString(ifFatal2) ;

  FatalLabel1.Left:=0;
  FatalLabel1.Width:=FormMMIFrame.Width;
  FatalLabel1.Alignment:=taCenter;

  FatalLabel2.Left:=0;
  FatalLabel2.Width:=FormMMIFrame.Width;
  FatalLabel2.Alignment:=taCenter;

end;

Procedure TFormFatal.ScreenChange;
Begin

      { AdjustLabel will also adjust font size }
  AdjustLabel(FatalLabel1);
  AdjustLabel(FatalLabel2);


End;


constructor TFormFatal.Create(AOwner: TComponent);
begin
  Try
    inherited Create(AOwner);
    Left := 0;
    Top := 0 ;

    Parent := FormMMIFrame.PanelMMIFrame;
    Align := alClient;


  Except
    on E: Exception do
    begin

      FormMMIFrame.WriteMMIStatus('FATAL',MS_SW_ERROR,MS_FATAL_ERROR,FatalFormLogErrorCreate);
      if FormMMIFrame.TestMode then
        MessageBox(Handle,PWideChar('Error in Create:'+E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
      Application.Terminate ;

    end;
 End;
end;


end.
