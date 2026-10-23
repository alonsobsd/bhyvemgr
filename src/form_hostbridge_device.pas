{ SPDX-License-Identifier: BSD-3-Clause

Copyright (c) 2024-2026, J. Alonso Cárdenas <acardenas@bsd-peru.org>

}

unit form_hostbridge_device;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ComCtrls;

type

  { TFormHostbridgeDevice }

  TFormHostbridgeDevice = class(TForm)
    BitBtnSave: TBitBtn;
    BitBtnClose: TBitBtn;
    ComboBoxHostbridgeDevice: TComboBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    StatusBarHostbridgeDevice: TStatusBar;
    procedure BitBtnCloseClick(Sender: TObject);
  private
    procedure FillComboDevice(Combo: TComboBox);
  public
    FormAction : String;
    function FormValidate():Boolean;
    procedure LoadDefaultValues();
  end;

var
  FormHostbridgeDevice: TFormHostbridgeDevice;

implementation

{$R *.lfm}

uses
  unit_global;

{ TFormHostbridgeDevice }

procedure TFormHostbridgeDevice.BitBtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormHostbridgeDevice.FillComboDevice(Combo: TComboBox);
begin
  Combo.Clear;
  Combo.Items.Add('amd_hostbridge');
  Combo.Items.Add('hostbridge');
end;

function TFormHostbridgeDevice.FormValidate(): Boolean;
begin
  Result:=True;

  if ComboBoxHostbridgeDevice.ItemIndex = -1 then Result:=False;
end;

procedure TFormHostbridgeDevice.LoadDefaultValues();
begin
  FillComboDevice(ComboBoxHostbridgeDevice);
  ComboBoxHostbridgeDevice.ItemIndex:=ComboBoxHostbridgeDevice.Items.IndexOf('hostbridge');
end;

end.

