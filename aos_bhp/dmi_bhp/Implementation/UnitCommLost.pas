
unit UnitCommLost;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, UnitMainArea, MMITypes, MMIStd;

type
  TFormCommLost = class(TForm)
    CommLostLabel: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    Procedure ScreenChange;
  end;

var
  FormCommLost: TFormCommLost;

implementation

uses UnitMMIFrame;

{$R *.DFM}

const
  CommLostLogErrorCreate = 1;

procedure TFormCommLost.FormCreate(Sender: TObject);
begin
  ScreenChange;
end;

Procedure TFormCommLost.ScreenChange;
Begin

  AdjustLabel(CommLostLabel);

End;

constructor TFormCommLost.Create(AOwner: TComponent);
begin
  Try
    inherited Create(AOwner);
    Left := 0;
    Top := 0 ;

    Parent := FormMMIFrame.PanelMMIFrame;
    Align := alClient;

    CommLostlabel.Caption:=InterfaceString(ifCommLost) ;
    CommLostlabel.Width := Width;

  Except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('COMMLOST',MS_SW_ERROR,MS_COMM_LOST,CommLostLogErrorCreate);
      if FormMMIFrame.TestMode then
        MessageBox(Handle,PWideChar('Error in Create:'+E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
      Application.Terminate ;
    end;
 End;
end;
end.
