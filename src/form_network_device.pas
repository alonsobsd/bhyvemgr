{ SPDX-License-Identifier: BSD-3-Clause

Copyright (c) 2024-2026, J. Alonso Cárdenas <acardenas@bsd-peru.org>

}

unit form_network_device;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ComCtrls, SpinEx;

type

  { TFormNetworkDevice }

  TFormNetworkDevice = class(TForm)
    BitBtnSave: TBitBtn;
    BitBtnClose: TBitBtn;
    ComboBoxDevice: TComboBox;
    EditBackend: TEdit;
    EditMac: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label13: TLabel;
    Label4: TLabel;
    SpinEditExMtu: TSpinEditEx;
    StatusBarNetworkDevice: TStatusBar;
    procedure BitBtnCloseClick(Sender: TObject);
  private
    procedure FillComboDevice(Combo: TComboBox);
  public
    FormAction : String;
    BackendDevice : String;
    MacAddress : String;
    function FormValidate():Boolean;
    procedure LoadDefaultValues();
  end;

var
  FormNetworkDevice: TFormNetworkDevice;

implementation

{$R *.lfm}

uses
  unit_global, unit_util;

{ TFormNetworkDevice }

function TFormNetworkDevice.FormValidate(): Boolean;
begin
  Result:=True;

  if ComboBoxDevice.ItemIndex=-1 then Result:=False
  else if not CheckNetworkDeviceName(Trim(EditBackend.Text)) then Result:=False
  else if Trim(EditMac.Text).IsEmpty then Result:=False
  else if SpinEditExMtu.Value = 0 then Result:=False
end;

procedure TFormNetworkDevice.BitBtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormNetworkDevice.FillComboDevice(Combo: TComboBox);
begin
  Combo.Clear;
  Combo.Items.Add('e1000');
  Combo.Items.Add('virtio-net');
end;

procedure TFormNetworkDevice.LoadDefaultValues();
begin
  FillComboDevice(ComboBoxDevice);
  ComboBoxDevice.ItemIndex:=ComboBoxDevice.Items.IndexOf('virtio-net');
  EditBackend.Text:=BackendDevice;
  EditMac.Text:=MacAddress;
  SpinEditExMtu.Text:='1500';
end;

end.

