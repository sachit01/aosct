
(*****************************************************************
           © COPYRIGHT Bombardier Transportation, SWEDEN 2011
           ==================================================

    The copyright to the computer program herein is the
    property of Bombardier Transportation AB, Sweden. All rights reserved.
    The program may be used and/or copied only with the
    written permission from Bombardier Transportation AB,
    or in accordance with the terms and conditions stipulated in the
    agreement/contract under which the program has been supplied.


    NAME :  Flash.pas

    PROJECT :  LKAB

    Ver    Author           Date    Reason
    ---    ------           ------  ------
    1.0.0  Daniel Persson   980126  First version
           Antbäck          100909  Automatically pick exe file version
                                    and use as program version
           Bo Hermansson    111109  GetVersion moved to MMIStd

    DESCRIPTION :  Flash unit that displays an .ani file

    INTERFACE :
*********************************************************)

unit UnitFlash;

interface

{****************************************************************************
* UNIT USES                                                                 *
****************************************************************************}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, MMIStd;

{****************************************************************************
* UNIT TYPE DECLARATIONS                                                    *
****************************************************************************}
type
  TFormFlash = class(TForm)
    Panel1: TPanel;
    Image: TImage;
    Panel2: TPanel;
    LabelVersion: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

{****************************************************************************
* UNIT VAR DECLARATIONS                                                     *
****************************************************************************}
var
  FormFlash: TFormFlash;

implementation
{$R *.DFM}
{****************************************************************************
* USES                                                                      *
****************************************************************************}
Uses
 UnitMainArea ;


{****************************************************************************
* CONST DECLARATIONS                                                        *
****************************************************************************}

{****************************************************************************
* TYPE DECLARATIONS                                                         *
****************************************************************************}

{****************************************************************************
* VAR DECLARATIONS                                                          *
****************************************************************************}

{****************************************************************************
* FORWARD DECLARATIONS                                                      *
****************************************************************************}

{****************************************************************************
* FUNCTIONS AND PROCEDURES                                                  *
****************************************************************************}
procedure TFormFlash.FormCreate(Sender: TObject);
begin
  Parent := FormMainArea;
  LabelVersion.Caption := GetVersion(Application.ExeName);
end;

{****************************************************************************
* INITIALIZATION PART                                                       *
****************************************************************************}

initialization

{****************************************************************************
* FINALIZATION PART                                                         *
****************************************************************************}
finalization

{****************************************************************************
* EXPORTS DECLARATIONS                                                     *
****************************************************************************}

{****************************************************************************
* RESOURCE STRING DECLARATIONS                                              *
****************************************************************************}
end.
