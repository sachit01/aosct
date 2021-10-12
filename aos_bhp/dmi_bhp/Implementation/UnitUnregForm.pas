unit UnitUnregForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ColorButton2, UnitMainArea, MMITypes, UnitExchange, MMIStd,
  UnitMainLayer2, ImgList, Menus, DB, ExtCtrls, Buttons, Grids, DBGrids,
  MMIPanel, GestureMgr, UnitMainLayer1;

type
  TFormUnreg = class(TFormMainLayer2)
    UnregLabel1: TLabel;
    Procedure ScreenChange;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  FormUnreg: TFormUnreg;

implementation

uses UnitMMIFrame;

{$R *.DFM}

const

  UnregLogErrorCreate = 1;

constructor TFormUnreg.Create(AOwner: TComponent);
begin
  Try
    inherited Create(AOwner);
{    Left := 0;
    Top := 0 ;
}

    UnregLabel1.Caption:=InterfaceString(ifUnreg1) ;
    UnregLabel1.Width := Width;


    Parent := FormMMIFrame.PanelMMIFrame;
    Align := alClient;

    ScreenChange;

  Except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('UNREG',MS_SW_ERROR,MS_UNREG,UnregLogErrorCreate);
      if FormMMIFrame.TestMode then
        MessageBox(Handle,PWideChar('Error in Create:'+E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
      Application.Terminate ;
    end;
 End;
end;

procedure TFormUnreg.FormShow(Sender: TObject);
begin
  inherited;
  UpdateDMI(true);

end;

procedure TFormUnreg.ScreenChange;
begin  {ScreenChange}

    Left := 0;
    Top := 0 ;
{    Width := Screen.Width;
    Height := Screen.Height;}

    Parent := FormMMIFrame.PanelMMIFrame;
    Align := alClient;


    AdjustLabel(UnregLabel1);

end;
end.
