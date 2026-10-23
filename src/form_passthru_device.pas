{ SPDX-License-Identifier: BSD-3-Clause

Copyright (c) 2024-2026, J. Alonso Cárdenas <acardenas@bsd-peru.org>

}

unit form_passthru_device;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  EditBtn, ComCtrls;

type

  { TFormPassthruDevice }

  TFormPassthruDevice = class(TForm)
    BitBtnSave: TBitBtn;
    BitBtnClose: TBitBtn;
    ComboBoxDevice: TComboBox;
    EditDescripcion: TEdit;
    FileNameEditRom: TFileNameEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    StatusBarPassthruDevice: TStatusBar;
    procedure BitBtnCloseClick(Sender: TObject);
    procedure ComboBoxDeviceChange(Sender: TObject);
  private
    procedure FillComboDevice(Combo: TComboBox);
  public
    FormAction : String;
    function FormValidate():Boolean;
    procedure LoadDefaultValues();
  end;

var
  FormPassthruDevice: TFormPassthruDevice;

implementation

{$R *.lfm}

uses
  unit_global, unit_util, unit_language;

{ TFormPassthruDevice }

procedure TFormPassthruDevice.ComboBoxDeviceChange(Sender: TObject);
begin
  if ComboBoxDevice.ItemIndex <> -1 then
  begin
    EditDescripcion.Text:=GetPciDeviceDescripcion(ComboBoxDevice.Text);
  end;
end;

procedure TFormPassthruDevice.BitBtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormPassthruDevice.FillComboDevice(Combo: TComboBox);
var
  i : Integer;
  PciList : TStringList;
begin
  PciList:=TStringList.Create();
  PciList.Text:=GetPciDeviceList('ppt');

  Combo.Clear;

  for i:=0 to PciList.Count-1 do
  begin
    Combo.Items.Add('ppt'+PciList[i]);
  end;

  PciList.Free;
end;

function TFormPassthruDevice.FormValidate(): Boolean;
begin
  Result:=True;

  if ComboBoxDevice.ItemIndex=-1 then Result:=False
  else if (Trim(EditDescripcion.Text).IsEmpty) then Result:=False
  else if not (Trim(FileNameEditRom.Text).IsEmpty) and not (FileExists(FileNameEditRom.FileName)) then Result:=False;
end;

procedure TFormPassthruDevice.LoadDefaultValues();
begin
  StatusBarPassthruDevice.SimpleText:=EmptyStr;
  FillComboDevice(ComboBoxDevice);

  EditDescripcion.Text:=EmptyStr;

  if ComboBoxDevice.Items.Count = 0 then
  begin
    StatusBarPassthruDevice.Font.Color:=clRed;
    StatusBarPassthruDevice.SimpleText:=ppt_devices_status;
  end;
end;

end.

