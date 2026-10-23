{ SPDX-License-Identifier: BSD-3-Clause

Copyright (c) 2024-2026, J. Alonso Cárdenas <acardenas@bsd-peru.org>

}

unit form_input_device;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ComCtrls;

type

  { TFormInputDevice }

  TFormInputDevice = class(TForm)
    BitBtnSave: TBitBtn;
    BitBtnClose: TBitBtn;
    ComboBoxInputDevice: TComboBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    StatusBarInputDevice: TStatusBar;
    procedure BitBtnCloseClick(Sender: TObject);
  private
    procedure FillComboDevice(Combo: TComboBox);
  public
    FormAction : String;
    function FormValidate():Boolean;
    procedure LoadDefaultValues();
  end;

var
  FormInputDevice: TFormInputDevice;

implementation

{$R *.lfm}

uses
  unit_global, unit_util;

{ TFormInputDevice }

procedure TFormInputDevice.BitBtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormInputDevice.FillComboDevice(Combo: TComboBox);
var
  i : Integer;
  TmpList : TStringList;
begin
  TmpList:=TStringList.Create();
  TmpList.Text:=GetEventDeviceList('/dev/input', 'event*');
  TmpList.Sorted:=True;

  Combo.Clear;
  for i:=0 to TmpList.Count-1 do
  begin
    Combo.Items.Add(TmpList[i]);
  end;

  TmpList.Free;
end;

function TFormInputDevice.FormValidate(): Boolean;
begin
  Result:=True;

  if ComboBoxInputDevice.ItemIndex = -1 then Result:=False;
end;

procedure TFormInputDevice.LoadDefaultValues();
begin
  FillComboDevice(ComboBoxInputDevice);
end;

end.

