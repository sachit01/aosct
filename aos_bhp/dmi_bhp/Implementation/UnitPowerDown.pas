unit UnitPowerDown;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ColorButton2, UnitMainArea, MMITypes, UnitExchange, MMIStd, UnitMainLayer1,
  GestureMgr, ImgList, Menus, DB, ExtCtrls, Buttons, Grids, DBGrids, MMIPanel;

type
  TFormPowerDown = class(TFormMainLayer1)
    LabelPowerDownMessage: TLabel;
    Procedure ScreenChange;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  FormPowerDown: TFormPowerDown;

implementation

uses UnitMMIFrame;

const

  PowerDownErrorCreate = 1;

{$R *.DFM}
constructor TFormPowerDown.Create(AOwner: TComponent);
begin
  Try
    inherited Create(AOwner);

    LabelPowerDownMessage.Caption:=InterfaceString(ifATPPowerDown) ;

    Parent := FormMMIFrame.PanelMMIFrame;
    Align := alClient;

    ScreenChange;

  Except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('POWERDN',MS_SW_ERROR,MS_PDOWN,PowerDownErrorCreate);
      if FormMMIFrame.TestMode then
        MessageBox(Handle,PWideChar('Error in Create:'+E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
      Application.Terminate ;
    end;
 End;
end;

procedure TFormPowerDown.FormShow(Sender: TObject);
begin
  inherited;
  UpdateDMI(true);

end;

procedure TFormPowerDown.ScreenChange;
begin  {ScreenChange}

    Left := 0;
    Top := 0 ;

    Parent := FormMMIFrame.PanelMMIFrame;
    Align := alClient;


    AdjustLabel(LabelPowerDownMessage);

end;

end.
