{ SPDX-License-Identifier: BSD-3-Clause

Copyright (c) 2024-2026, J. Alonso Cárdenas <acardenas@bsd-peru.org>

}

unit form_lpc_device;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ComCtrls;

type

  { TFormLpcDevice }

  TFormLpcDevice = class(TForm)
    BitBtnSave: TBitBtn;
    BitBtnClose: TBitBtn;
    CheckBoxPcDebug: TCheckBox;
    CheckBoxCom1: TCheckBox;
    CheckBoxCom2: TCheckBox;
    CheckBoxCom3: TCheckBox;
    CheckBoxCom4: TCheckBox;
    ComboBoxCom1: TComboBox;
    ComboBoxCom2: TComboBox;
    ComboBoxCom3: TComboBox;
    ComboBoxCom4: TComboBox;
    ComboBoxBootrom: TComboBox;
    ComboBoxBootvars: TComboBox;
    ComboBoxFwcfg: TComboBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    StatusBarLpcDevice: TStatusBar;
    procedure BitBtnCloseClick(Sender: TObject);
    procedure CheckBoxCom1Change(Sender: TObject);
    procedure CheckBoxCom2Change(Sender: TObject);
    procedure CheckBoxCom3Change(Sender: TObject);
    procedure CheckBoxCom4Change(Sender: TObject);
  private

  public
    FormAction : String;
    FormVmName : String;
    function FormValidate():Boolean;
    procedure LoadDefaultValues();
  end;

var
  FormLpcDevice: TFormLpcDevice;

implementation

{$R *.lfm}

uses
  unit_component, unit_global, unit_util;

{ TFormLpcDevice }

procedure TFormLpcDevice.CheckBoxCom1Change(Sender: TObject);
begin
  if CheckBoxCom1.Checked then
    ComboBoxCom1.Enabled:=True
  else
    ComboBoxCom1.Enabled:=False;
end;

procedure TFormLpcDevice.BitBtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormLpcDevice.CheckBoxCom2Change(Sender: TObject);
begin
  if CheckBoxCom2.Checked then
    ComboBoxCom2.Enabled:=True
  else
    ComboBoxCom2.Enabled:=False;
end;

procedure TFormLpcDevice.CheckBoxCom3Change(Sender: TObject);
begin
  if CheckBoxCom3.Checked then
    ComboBoxCom3.Enabled:=True
  else
    ComboBoxCom3.Enabled:=False;
end;

procedure TFormLpcDevice.CheckBoxCom4Change(Sender: TObject);
begin
  if CheckBoxCom4.Checked then
    ComboBoxCom4.Enabled:=True
  else
    ComboBoxCom4.Enabled:=False;
end;

function TFormLpcDevice.FormValidate(): Boolean;
begin
  Result:=True;

  if ComboBoxBootrom.ItemIndex=-1 then Result:=False
  else if CheckBoxCom1.Checked and (ComboBoxCom1.ItemIndex = -1) then Result:=False
  else if CheckBoxCom2.Checked and (ComboBoxCom2.ItemIndex = -1) then Result:=False
  else if CheckBoxCom3.Checked and (ComboBoxCom3.ItemIndex = -1) then Result:=False
  else if CheckBoxCom4.Checked and (ComboBoxCom4.ItemIndex = -1) then Result:=False
  else if ComboBoxFwcfg.ItemIndex=-1 then Result:=False
end;

procedure TFormLpcDevice.LoadDefaultValues();
begin
  ComboBoxBootrom.Clear;
  FillComboBootrom(ComboBoxBootrom);
  ComboBoxBootvars.Clear;
  FillComboBootvars(ComboBoxBootvars);
  ComboBoxFwcfg.Clear;
  FillComboFwcfg(ComboBoxFwcfg);

  {$ifdef CPUAMD64}
  ComboBoxBootrom.ItemIndex:=ComboBoxBootrom.Items.IndexOf('BHYVE_UEFI.fd');
  {$endif CPUAMD64}
  {$ifdef CPUAARCH64}
  ComboBoxBootrom.ItemIndex:=ComboBoxBootrom.Items.IndexOf('u-boot.bin');
  {$endif CPUAARCH64}

  { Remove when bhyve will updated on FreeBSD 13.x and 14.x }
  if GetOsreldate.ToInt64 > 1500023 then
  begin
    ComboBoxBootrom.Enabled:=False;
    ComboBoxBootvars.Enabled:=False;
  end;

  CheckBoxCom1.Checked:=False;
  ComboBoxCom1.Clear;
  ComboBoxCom1.Enabled:=False;
  ComboBoxCom1.Items.Add('/dev/nmdm-'+FormVmName+'.1A');

  if (GetOsreldate.ToInt64 > 1500023) then
  begin
    if FormAction = 'Add' then
    begin
      ComboBoxCom1.Items.Add('tcp=0.0.0.0:'+GetNewComPortNumber());
      ComboBoxCom1.Items.Add('tcp=127.0.0.1:'+GetNewComPortNumber());

      if UseIpv6 = 'yes' then
      begin
        ComboBoxCom1.Items.Add('tcp=[::]:'+GetNewComPortNumber());
        ComboBoxCom1.Items.Add('tcp=[::1]:'+GetNewComPortNumber());
      end;
    end;
  end;

  ComboBoxCom2.Clear;
  ComboBoxCom2.Enabled:=False;
  ComboBoxCom2.Items.Add('/dev/nmdm-'+FormVmName+'.2A');

  ComboBoxCom3.Clear;
  ComboBoxCom3.Enabled:=False;
  ComboBoxCom3.Items.Add('/dev/nmdm-'+FormVmName+'.3A');

  ComboBoxCom4.Clear;
  ComboBoxCom4.Enabled:=False;
  ComboBoxCom4.Items.Add('/dev/nmdm-'+FormVmName+'.4A');

  ComboBoxFwcfg.ItemIndex:=ComboBoxFwcfg.Items.IndexOf('bhyve');
end;

end.

