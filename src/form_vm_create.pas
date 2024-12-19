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

unit form_vm_create;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  Buttons, ComCtrls, LCLType, SpinEx;

type

  { TFormVmCreate }

  TFormVmCreate = class(TForm)
    BitBtnCreateVm: TBitBtn;
    CheckBoxUEFIBootvars: TCheckBox;
    CheckBoxOnlyLocalhost: TCheckBox;
    CheckBoxUseMedia: TCheckBox;
    CheckBoxFramebuffer: TCheckBox;
    CheckBoxWaitVNC: TCheckBox;
    ComboBoxVirtualStorageType: TComboBox;
    ComboBoxSystemType: TComboBox;
    ComboBoxSystemVersion: TComboBox;
    ComboBoxVirtualDeviceType: TComboBox;
    EditVmDescription: TEdit;
    EditVmName: TEdit;
    EditVmFolderPath: TEdit;
    FileNameEditBootMedia: TFileNameEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBoxNewVirtualDisk: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    RadioButtonNotDisk: TRadioButton;
    RadioButtonNewDisk: TRadioButton;
    SpinEditExDiskSize: TSpinEditEx;
    SpinEditExMemory: TSpinEditEx;
    StatusBarVmCreate: TStatusBar;
    procedure CheckBoxFramebufferChange(Sender: TObject);
    procedure CheckBoxOnlyLocalhostChange(Sender: TObject);
    procedure CheckBoxUseMediaChange(Sender: TObject);
    procedure CheckBoxWaitVNCChange(Sender: TObject);
    procedure ComboBoxSystemTypeChange(Sender: TObject);
    procedure EditVmNameEditingDone(Sender: TObject);
    procedure EditVmNameExit(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RadioButtonNewDiskChange(Sender: TObject);
    procedure RadioButtonNotDiskChange(Sender: TObject);
  private

  public
    function FormValidate():Boolean;
  end;

var
  FormVmCreate: TFormVmCreate;

implementation

{$R *.lfm}

uses
  unit_component, unit_global, unit_util;

{ TFormVmCreate }

procedure TFormVmCreate.FormShow(Sender: TObject);
begin
  EditVmName.SetFocus;

  EditVmName.Clear;
  EditVmFolderPath.Clear;

  ComboBoxSystemType.Clear;
  ComboBoxSystemVersion.Clear;

  FillComboSystemType(ComboBoxSystemType);
  EditVmDescription.Clear;

  SpinEditExMemory.Text:='256';

  RadioButtonNotDisk.Checked:=False;
  RadioButtonNewDisk.Checked:=True;

  CheckBoxFramebuffer.Checked:=True;
  CheckBoxWaitVNC.Checked:=True;
  CheckBoxOnlyLocalhost.Checked:=True;
  CheckBoxUEFIBootvars.Checked:=True;

  ComboBoxVirtualDeviceType.Clear;
  FillComboVirtualDeviceType(ComboBoxVirtualDeviceType);
  ComboBoxVirtualDeviceType.Items.Delete(ComboBoxVirtualDeviceType.Items.IndexOf('ahci-cd'));

  ComboBoxVirtualStorageType.Clear;
  FillComboVirtualStorageType(ComboBoxVirtualStorageType);

  SpinEditExDiskSize.Text:='20';

  CheckBoxUseMedia.Checked:=False;
  FileNameEditBootMedia.Clear;

  StatusBarVmCreate.SimpleText:=EmptyStr;

  GroupBoxNewVirtualDisk.Visible:=True;
end;

procedure TFormVmCreate.RadioButtonNewDiskChange(Sender: TObject);
begin
  if RadioButtonNewDisk.Checked then
  begin
    GroupBoxNewVirtualDisk.Enabled:=True;
    GroupBoxNewVirtualDisk.Visible:=True;
  end;
end;

procedure TFormVmCreate.RadioButtonNotDiskChange(Sender: TObject);
begin
  if RadioButtonNotDisk.Checked then
  begin
    GroupBoxNewVirtualDisk.Enabled:=False;
    GroupBoxNewVirtualDisk.Visible:=False;
  end;
end;

function TFormVmCreate.FormValidate(): Boolean;
begin
  Result:=True;

  if (ComboBoxSystemType.ItemIndex=-1) then
    Result:=False;
  if (ComboBoxSystemVersion.ItemIndex=-1) then
    Result:=False;

  if RadioButtonNewDisk.Checked then
  begin
    if (ComboBoxVirtualDeviceType.ItemIndex=-1) then
      Result:=False;
    if (ComboBoxVirtualStorageType.ItemIndex=-1) then
      Result:=False;
  end;

  if CheckBoxUseMedia.Checked then
  begin
    if not FileExists(FileNameEditBootMedia.FileName) then
      Result:=False;
  end;
end;

procedure TFormVmCreate.ComboBoxSystemTypeChange(Sender: TObject);
begin
  ComboBoxSystemVersion.Clear;
  FillComboSystemVersion(ComboBoxSystemVersion, ComboBoxSystemType.Text);
end;

procedure TFormVmCreate.CheckBoxOnlyLocalhostChange(Sender: TObject);
begin
  if CheckBoxOnlyLocalhost.Checked then
  begin
    CheckBoxFramebuffer.Checked:=True;
  end;
end;

procedure TFormVmCreate.CheckBoxUseMediaChange(Sender: TObject);
begin
  if CheckBoxUseMedia.Checked then
    FileNameEditBootMedia.Enabled:=True
  else
    FileNameEditBootMedia.Enabled:=False;
end;

procedure TFormVmCreate.CheckBoxFramebufferChange(Sender: TObject);
begin
  if CheckBoxFramebuffer.Checked then
  begin
    CheckBoxOnlyLocalhost.Checked:=True;
  end
  else
  begin
    CheckBoxOnlyLocalhost.Checked:=False;
    CheckBoxWaitVNC.Checked:=False;
  end;
end;

procedure TFormVmCreate.CheckBoxWaitVNCChange(Sender: TObject);
begin
  if CheckBoxWaitVNC.Checked then
  begin
    CheckBoxFramebuffer.Checked:=True;
  end;
end;

procedure TFormVmCreate.EditVmNameEditingDone(Sender: TObject);
begin
    EditVmFolderPath.Text:= VmPath + '/' + EditVmName.Text
end;

procedure TFormVmCreate.EditVmNameExit(Sender: TObject);
begin
  if not CheckVmName(EditVmName.Text) then
  begin
    EditVmName.SetFocus;
    StatusBarVmCreate.Font.Color:=clRed;
    StatusBarVmCreate.SimpleText:='Virtual machine name is not valid. Valid characters are [a-z][-_][0-9]';
  end;
  if not (GetNewVmName(EditVmName.Text)) then
  begin
    EditVmName.SetFocus;
    StatusBarVmCreate.Font.Color:=clRed;
    StatusBarVmCreate.SimpleText:=EditVmName.Text + ' virtual machine name is not available';
  end
end;

end.

