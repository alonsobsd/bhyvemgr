{ BSD 3-Clause License

Copyright (c) 2024-2025, Alonso Cárdenas <acardenas@bsd-peru.org>

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
    CheckBoxReadOnly: TCheckBox;
    ComboBoxDevice: TComboBox;
    DirectoryEditPath: TDirectoryEdit;
    EditSharename: TEdit;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    StatusBarSharefolderDevice: TStatusBar;
    procedure FormShow(Sender: TObject);
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

procedure TFormShareFolderDevice.FormShow(Sender: TObject);
begin
  FormShareFolderDevice.Caption:=FormBhyveManagerShareFolderDeviceTitle;
end;

procedure TFormShareFolderDevice.FillComboDevice(Combo: TComboBox);
begin
  Combo.Clear;
  Combo.Items.Add('virtio-9p');
end;

function TFormShareFolderDevice.FormValidate(): Boolean;
begin
  Result:=True;

  if ComboBoxDevice.ItemIndex=-1 then Result:=False
  else if (Trim(EditSharename.Text) = EmptyStr) then Result:=False
  else if (Trim(DirectoryEditPath.Text) = EmptyStr) or not DirectoryExists(DirectoryEditPath.Text) then Result:=False;
end;

procedure TFormShareFolderDevice.LoadDefaultValues();
begin
  FillComboDevice(ComboBoxDevice);
  ComboBoxDevice.ItemIndex:=ComboBoxDevice.Items.IndexOf('virtio-9p');
end;

end.

