
unit UnitSafetyHalt;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, UnitMainArea, MMITypes, UnitExchange, ColorButton2, MMIStd,
  GestureMgr, UnitMainLayer1, ImgList, Menus, DB, ExtCtrls, Buttons, Grids,
  DBGrids, MMIPanel;

type
  TFormSafetyHalt = class(TFormMainLayer1)
    FatalLabel1: TLabel;
    FatalLabel2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    procedure SetMessage;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    Procedure ScreenChange;
  end;

var
  FormSafetyHalt: TFormSafetyHalt;

implementation

uses UnitMMIFrame, UnitViewLog;

{$R *.DFM}

const
  FatalFormLogErrorCreate = 1;

procedure TFormSafetyHalt.FormCreate(Sender: TObject);
begin

  inherited;

  ScreenChange;
  SetMessage;

end;


procedure TFormSafetyHalt.FormShow(Sender: TObject);
begin
  inherited;
  UpdateDMI(true);

end;

procedure TFormSafetyHalt.SetMessage;
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

Procedure TFormSafetyHalt.ScreenChange;
Begin

      { AdjustLabel will also adjust font size }
  AdjustLabel(FatalLabel1);
  AdjustLabel(FatalLabel2);


End;


constructor TFormSafetyHalt.Create(AOwner: TComponent);
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
