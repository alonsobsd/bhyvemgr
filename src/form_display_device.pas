{ SPDX-License-Identifier: BSD-3-Clause

Copyright (c) 2024-2026, J. Alonso Cárdenas <acardenas@bsd-peru.org>

}

unit form_display_device;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ComCtrls;

type

  { TFormDisplayDevice }

  TFormDisplayDevice = class(TForm)
    BitBtnClose: TBitBtn;
    BitBtnSave: TBitBtn;
    CheckBoxOnlyLocalhost: TCheckBox;
    CheckBoxWaitVnc: TCheckBox;
    CheckBoxUsePassword: TCheckBox;
    ComboBoxHost: TComboBox;
    ComboBoxResolution: TComboBox;
    ComboBoxVga: TComboBox;
    EditPassword: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    StatusBarDisplayDevice: TStatusBar;
    procedure BitBtnCloseClick(Sender: TObject);
    procedure CheckBoxOnlyLocalhostChange(Sender: TObject);
    procedure CheckBoxUsePasswordChange(Sender: TObject);
  private
    procedure FillComboVga(Combo: TComboBox);
  public
    FormAction : String;
    HostPort : String;
    function FormValidate():Boolean;
    procedure LoadDefaultValues();
  end;

var
  FormDisplayDevice: TFormDisplayDevice;

implementation

{$R *.lfm}

uses
  unit_component, unit_util, unit_global;

{ TFormDisplayDevice }

procedure TFormDisplayDevice.CheckBoxOnlyLocalhostChange(Sender: TObject);
begin
  if CheckBoxOnlyLocalhost.Checked then
    ComboBoxHost.ItemIndex:=ComboBoxHost.Items.IndexOf('127.0.0.1:'+HostPort)
  else
    ComboBoxHost.ItemIndex:=ComboBoxHost.Items.IndexOf('0.0.0.0:'+HostPort);
end;

procedure TFormDisplayDevice.BitBtnCloseClick(Sender: TObject);
begin
  Close
end;

procedure TFormDisplayDevice.CheckBoxUsePasswordChange(Sender: TObject);
begin
  if CheckBoxUsePassword.Checked then
    EditPassword.Enabled:=True
  else
  begin
    EditPassword.Clear;
    EditPassword.Enabled:=False;
  end;
end;

function TFormDisplayDevice.FormValidate(): Boolean;
begin
  Result:=True;

  if CheckBoxUsePassword.Checked and (Trim(EditPassword.Text).IsEmpty) then
    Result:=False;
end;

procedure TFormDisplayDevice.FillComboVga(Combo: TComboBox);
begin
  Combo.Clear;
  Combo.Items.Add('io');
  Combo.Items.Add('on');
  Combo.Items.Add('off');
end;

procedure TFormDisplayDevice.LoadDefaultValues();
begin
  if FormAction = 'Add' then
    HostPort:=GetNewVncPortNumber();

  FillComboVga(ComboBoxVga);
  FillComboResolution(ComboBoxResolution);

  ComboBoxVga.ItemIndex:=ComboBoxVga.Items.IndexOf('io');
  ComboBoxResolution.ItemIndex:=ComboBoxResolution.Items.IndexOf('1024x768');
  CheckBoxOnlyLocalhost.Checked:=True;
  CheckBoxWaitVnc.Checked:=True;
  CheckBoxUsePassword.Checked:=False;
  EditPassword.Enabled:=False;
  EditPassword.Clear;

  ComboBoxHost.Clear;
  ComboBoxHost.Enabled:=True;

  if GetOsreldate.ToInt64 >= 1500509 then
    ComboBoxHost.Items.Add('unix:vnc.sock');

  ComboBoxHost.Items.Add('0.0.0.0:'+HostPort);
  ComboBoxHost.Items.Add('127.0.0.1:'+HostPort);

  if UseIpv6 = 'yes' then
  begin
    ComboBoxHost.Items.Add('[::]:'+HostPort);
    ComboBoxHost.Items.Add('[::1]:'+HostPort);
  end;
end;

end.

