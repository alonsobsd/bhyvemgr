{ BSD 3-Clause License

Copyright (c) 2024, Alonso Cárdenas <acardenas@bsd-peru.org>

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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
    CheckBoxPcDebug: TCheckBox;
    CheckBoxCom1: TCheckBox;
    CheckBoxCom2: TCheckBox;
    CheckBoxCom3: TCheckBox;
    CheckBoxCom4: TCheckBox;
    ComboBoxBootrom: TComboBox;
    EditCom1: TEdit;
    EditCom2: TEdit;
    EditCom3: TEdit;
    EditCom4: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    StatusBarLpcDevice: TStatusBar;
    procedure CheckBoxCom1Change(Sender: TObject);
    procedure CheckBoxCom2Change(Sender: TObject);
    procedure CheckBoxCom3Change(Sender: TObject);
    procedure CheckBoxCom4Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    FormAction : String;
    function FormValidate():Boolean;
    procedure LoadDefaultValues(VmName : String);
  end;

var
  FormLpcDevice: TFormLpcDevice;

implementation

{$R *.lfm}

uses
  unit_component, unit_global;

{ TFormLpcDevice }

procedure TFormLpcDevice.CheckBoxCom1Change(Sender: TObject);
begin
  if CheckBoxCom1.Checked then
    EditCom1.Enabled:=True
  else
    EditCom1.Enabled:=False;
end;

procedure TFormLpcDevice.CheckBoxCom2Change(Sender: TObject);
begin
  if CheckBoxCom2.Checked then
    EditCom2.Enabled:=True
  else
    EditCom2.Enabled:=False;
end;

procedure TFormLpcDevice.CheckBoxCom3Change(Sender: TObject);
begin
  if CheckBoxCom3.Checked then
    EditCom3.Enabled:=True
  else
    EditCom3.Enabled:=False;
end;

procedure TFormLpcDevice.CheckBoxCom4Change(Sender: TObject);
begin
  if CheckBoxCom4.Checked then
    EditCom4.Enabled:=True
  else
    EditCom4.Enabled:=False;
end;

procedure TFormLpcDevice.FormShow(Sender: TObject);
begin
  FormLpcDevice.Caption:=FormBhyveManagerLPCDeviceTitle;
end;

function TFormLpcDevice.FormValidate(): Boolean;
begin
  Result:=True;

  if ComboBoxBootrom.ItemIndex=-1 then Result:=False
  else if CheckBoxCom1.Checked and (Trim(EditCom1.Text) = EmptyStr) then Result:=False
  else if CheckBoxCom2.Checked and (Trim(EditCom2.Text) = EmptyStr) then Result:=False
  else if CheckBoxCom3.Checked and (Trim(EditCom3.Text) = EmptyStr) then Result:=False
  else if CheckBoxCom4.Checked and (Trim(EditCom4.Text) = EmptyStr) then Result:=False
end;

procedure TFormLpcDevice.LoadDefaultValues(VmName : String);
begin
  FillComboBootrom(ComboBoxBootrom);

  {$ifdef CPUAMD64}
  ComboBoxBootrom.ItemIndex:=ComboBoxBootrom.Items.IndexOf('BHYVE_UEFI.fd');
  {$endif CPUAMD64}
  {$ifdef CPUAARCH64}
  ComboBoxBootrom.ItemIndex:=ComboBoxBootrom.Items.IndexOf('u-boot.bin');
  {$endif CPUAARCH64}

  EditCom1.Text:='/dev/nmdm-'+VmName+'.1A';
  EditCom2.Text:='/dev/nmdm-'+VmName+'.2A';
  EditCom3.Text:='/dev/nmdm-'+VmName+'.3A';
  EditCom4.Text:='/dev/nmdm-'+VmName+'.4A';
end;

end.

