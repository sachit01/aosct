unit UnitSleepingModeForm;

interface

uses
 Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, UnitMainArea, MMITypes, UnitExchange, ColorButton2, MMIStd,
  GestureMgr, UnitMainLayer2, ImgList, Menus, DB, ExtCtrls, Buttons, Grids,
  DBGrids, MMIPanel;

type
  TFormSleepingMode = class(TFormMainLayer2)
    LabelSleeping: TLabel;
    procedure FormCreate(Sender: TObject);
    Procedure ScreenChange;
    procedure FormShow(Sender: TObject);
    procedure ColorButtonConfigurationClick(Sender: TObject);
  private
    { Private declarations }
    procedure SetMessage;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure UpdateDMI(Refresh : Boolean);
  end;

var
  FormSleepingMode: TFormSleepingMode;

  const
  FatalFormLogErrorCreate = 1;

implementation

uses UnitMMIFrame, UnitViewLog, UnitConfirmSleepModeExit;
{$R *.dfm}

procedure TFormSleepingMode.FormCreate(Sender: TObject);
begin
  inherited;
  ScreenChange;
  SetMessage;

end;

procedure TFormSleepingMode.FormShow(Sender: TObject);
begin
  inherited;
  UpdateDMI(true);

end;

procedure TFormSleepingMode.ScreenChange;
begin  {ScreenChange}
        AdjustLabel(LabelSleeping);

end;

{ *********************************************************
* UpdateDMI
* Description: Call this method to update this form
* Arguments: Update all when Refresh = True
********************************************************* }
procedure TFormSleepingMode.UpdateDMI(Refresh : Boolean);
begin
   inherited;
end;

procedure TFormSleepingMode.SetMessage;
begin

  LabelSleeping.Caption:=InterfaceString(ifSleepingMsg) ;

  LabelSleeping.Left:=0;
  LabelSleeping.Width:=FormMMIFrame.Width;
  LabelSleeping.Alignment:=taCenter;

end;

procedure TFormSleepingMode.ColorButtonConfigurationClick(Sender: TObject);
begin
  FormConfirmExitSleepingMode.Show;
 // inherited;

end;

constructor TFormSleepingMode.Create(AOwner: TComponent);
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

