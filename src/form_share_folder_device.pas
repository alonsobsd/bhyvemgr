{ SPDX-License-Identifier: BSD-3-Clause

Copyright (c) 2024-2026, J. Alonso Cárdenas <acardenas@bsd-peru.org>

}

unit form_share_folder_device;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  EditBtn, ComCtrls;

type

  { TFormShareFolderDevice }

  TFormShareFolderDevice = class(TForm)
    BitBtnSave: TBitBtn;
    BitBtnClose: TBitBtn;
    CheckBoxReadOnly: TCheckBox;
    ComboBoxDevice: TComboBox;
    DirectoryEditPath: TDirectoryEdit;
    EditSharename: TEdit;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    StatusBarSharefolderDevice: TStatusBar;
  private
    procedure FillComboDevice(Combo: TComboBox);
  public
    FormAction : String;
    function FormValidate():Boolean;
    procedure LoadDefaultValues();
  end;

var
  FormShareFolderDevice: TFormShareFolderDevice;

implementation

{$R *.lfm}

uses
  unit_global;

{ TFormShareFolderDevice }

procedure TFormShareFolderDevice.FillComboDevice(Combo: TComboBox);
begin
  Combo.Clear;
  Combo.Items.Add('virtio-9p');
end;

function TFormShareFolderDevice.FormValidate(): Boolean;
begin
  Result:=True;

  if ComboBoxDevice.ItemIndex=-1 then Result:=False
  else if (Trim(EditSharename.Text).IsEmpty) then Result:=False
  else if (Trim(DirectoryEditPath.Text).IsEmpty) or not DirectoryExists(DirectoryEditPath.Text) then Result:=False;
end;

procedure TFormShareFolderDevice.LoadDefaultValues();
begin
  FillComboDevice(ComboBoxDevice);
  ComboBoxDevice.ItemIndex:=ComboBoxDevice.Items.IndexOf('virtio-9p');
end;

end.

