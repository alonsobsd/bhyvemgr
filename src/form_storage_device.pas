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

unit form_storage_device;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  Buttons, ComCtrls, SpinEx;

type

  { TFormStorageDevice }

  TFormStorageDevice = class(TForm)
    BitBtnSave: TBitBtn;
    CheckBoxNoCache: TCheckBox;
    CheckBoxNoDelete: TCheckBox;
    CheckBoxSync: TCheckBox;
    CheckBoxReadOnly: TCheckBox;
    CheckBoxNvmUseRam: TCheckBox;
    ComboBoxAhciNmrr: TComboBox;
    ComboBoxNvmeDsm: TComboBox;
    ComboBoxStorageDevice: TComboBox;
    ComboBoxStorageType: TComboBox;
    EditSer: TEdit;
    EditAhciRev: TEdit;
    EditAhciModel: TEdit;
    EditNvmeMaxq: TEdit;
    EditNvmeQsz: TEdit;
    EditNvmeIoslots: TEdit;
    EditAhciSectorSize: TEdit;
    EditNvmeSectsz: TEdit;
    EditNvmeEui64: TEdit;
    FileNameEditStoragePath: TFileNameEdit;
    GroupBox1: TGroupBox;
    GroupBoxAhci: TGroupBox;
    GroupBoxNvme: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    SpinEditExDiskSize: TSpinEditEx;
    SpinEditExNvmeRam: TSpinEditEx;
    StatusBarStorageDevice: TStatusBar;
    procedure CheckBoxNvmUseRamChange(Sender: TObject);
    procedure ComboBoxStorageDeviceChange(Sender: TObject);
    procedure ComboBoxStorageTypeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    DiskName : String;
    procedure FillComboAhciNmrr(Combo: TComboBox);
    procedure FillComboNvmeDsm(Combo: TComboBox);
  public
    FormAction : String;
    VmName : String;
    function FormValidate():Boolean;
    procedure LoadDefaultValues();
  end;

var
  FormStorageDevice: TFormStorageDevice;

implementation

{$R *.lfm}

uses
  unit_component, unit_global, unit_util;

{ TFormStorageDevice }

procedure TFormStorageDevice.ComboBoxStorageDeviceChange(Sender: TObject);
begin
  FileNameEditStoragePath.Clear;
  FileNameEditStoragePath.Filter:='';
  FileNameEditStoragePath.Enabled:=True;
  FileNameEditStoragePath.ReadOnly:=True;

  ComboBoxStorageType.Enabled:=True;

  case ComboBoxStorageDevice.Text of
  'ahci-cd':
    begin
      ComboBoxStorageType.ItemIndex:=ComboBoxStorageType.Items.IndexOf('image file');
      ComboBoxStorageType.Enabled:=False;

      GroupBoxAhci.Enabled:=False;
      GroupBoxNvme.Enabled:=False;

      FileNameEditStoragePath.Clear;
      FileNameEditStoragePath.Filter:='*.iso';
      FileNameEditStoragePath.Enabled:=True;
      FileNameEditStoragePath.ReadOnly:=False;
    end;
  'ahci-hd':
    begin
      GroupBoxAhci.Enabled:=True;
      GroupBoxNvme.Enabled:=False;

      ComboBoxStorageTypeChange(Nil);
    end;
  'nvme':
    begin
      GroupBoxAhci.Enabled:=False;
      GroupBoxNvme.Enabled:=True;

      ComboBoxStorageTypeChange(Nil);
    end;
  'virtio-blk':
    begin
      GroupBoxAhci.Enabled:=False;
      GroupBoxNvme.Enabled:=False;

      ComboBoxStorageTypeChange(Nil);
    end;
  end;
end;

procedure TFormStorageDevice.CheckBoxNvmUseRamChange(Sender: TObject);
begin
  if CheckBoxNvmUseRam.Checked then
  begin
    SpinEditExNvmeRam.Enabled:=True;
    FileNameEditStoragePath.Clear;
  end
  else
    SpinEditExNvmeRam.Enabled:=False;
end;

procedure TFormStorageDevice.ComboBoxStorageTypeChange(Sender: TObject);
begin
  case ComboBoxStorageType.Text of
  'image file':
    begin
      if ComboBoxStorageDevice.Text = 'ahci-cd' then
      begin
        FileNameEditStoragePath.Clear;
        FileNameEditStoragePath.Enabled:=True;
        FileNameEditStoragePath.ReadOnly:=False;
      end
      else
      begin
        DiskName:=GetNewStorageName(VmPath +'/'+VmName, False);
        FileNameEditStoragePath.Enabled:=True;
        FileNameEditStoragePath.ReadOnly:=True;
        FileNameEditStoragePath.Filter:='';
        FileNameEditStoragePath.Text:=VmPath+'/'+VmName+'/'+DiskName;
      end;
    end;
  'zfs sparse volume',
  'zfs volume':
    begin
      DiskName:=GetNewStorageName('/dev/zvol'+VmPath+'/'+VmName, True);
      FileNameEditStoragePath.Enabled:=True;
      FileNameEditStoragePath.ReadOnly:=True;
      FileNameEditStoragePath.Filter:='';
      FileNameEditStoragePath.Text:='/dev/zvol'+VmPath+'/'+VmName+'/'+DiskName;
    end;
  end;
end;

procedure TFormStorageDevice.FormShow(Sender: TObject);
begin
  FormStorageDevice.Caption:=FormBhyveManagerStorageDeviceTitle;
end;

function TFormStorageDevice.FormValidate(): Boolean;
begin
  Result:=True;

  if ComboBoxStorageDevice.ItemIndex = -1 then Result:=False
  else if ComboBoxStorageType.ItemIndex = -1 then Result:=False
  else if FileNameEditStoragePath.Text = EmptyStr then Result:=False
  else if SpinEditExDiskSize.Value = 0 then Result:=False
  else if CheckBoxNvmUseRam.Checked and (SpinEditExNvmeRam.Value = 0) then Result:=False;
end;

procedure TFormStorageDevice.FillComboAhciNmrr(Combo: TComboBox);
begin
  Combo.Clear;
  Combo.Items.Add('0');
  Combo.Items.Add('1');
  Combo.Items.Add('4200');
  Combo.Items.Add('5400');
  Combo.Items.Add('7200');
  Combo.Items.Add('10000');
  Combo.Items.Add('15000');
end;

procedure TFormStorageDevice.FillComboNvmeDsm(Combo: TComboBox);
begin
  Combo.Clear;
  Combo.Items.Add('auto');
  Combo.Items.Add('enable');
  Combo.Items.Add('disable');
end;

procedure TFormStorageDevice.LoadDefaultValues();
begin
  ComboBoxStorageDevice.Clear;
  ComboBoxStorageDevice.Enabled:=True;
  FillComboVirtualDeviceType(ComboBoxStorageDevice);

  ComboBoxStorageType.Clear;
  ComboBoxStorageType.Enabled:=True;
  FillComboVirtualStorageType(ComboBoxStorageType);

  FillComboAhciNmrr(ComboBoxAhciNmrr);

  ComboBoxStorageDevice.ItemIndex:=ComboBoxStorageDevice.Items.IndexOf('nvme');
  ComboBoxStorageType.ItemIndex:=ComboBoxStorageType.Items.IndexOf('image file');
  ComboBoxAhciNmrr.ItemIndex:=ComboBoxAhciNmrr.Items.IndexOf('0');
  ComboBoxNvmeDsm.ItemIndex:=ComboBoxNvmeDsm.Items.IndexOf('auto');
  EditSer.Clear;
  EditAhciRev.Text:='001';
  EditAhciSectorSize.Clear;
  EditAhciModel.Clear;
  EditNvmeMaxq.Text:='16';
  EditNvmeQsz.Text:='2058';
  EditNvmeIoslots.Text:='8';
  EditNvmeEui64.Clear;
  EditNvmeSectsz.Clear;
  SpinEditExDiskSize.Value:=20;
  ComboBoxStorageTypeChange(Nil);
end;

end.

