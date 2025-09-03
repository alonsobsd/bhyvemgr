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

unit form_passthru_device;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  EditBtn, ComCtrls, LCLTranslator;

type

  { TFormPassthruDevice }

  TFormPassthruDevice = class(TForm)
    BitBtnSave: TBitBtn;
    ComboBoxDevice: TComboBox;
    EditDescripcion: TEdit;
    FileNameEditRom: TFileNameEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    StatusBarPassthruDevice: TStatusBar;
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
  else if (Trim(EditDescripcion.Text) = EmptyStr) then Result:=False
  else if (FileNameEditRom.Text <> EmptyStr) and not (FileExists(FileNameEditRom.FileName)) then Result:=False;
end;

procedure TFormPassthruDevice.LoadDefaultValues();
begin
  FillComboDevice(ComboBoxDevice);

  EditDescripcion.Text:=EmptyStr;

  if ComboBoxDevice.Items.Count = 0 then
  begin
    StatusBarPassthruDevice.Font.Color:=clRed;
    StatusBarPassthruDevice.SimpleText:=ppt_devices_status;
  end;
end;

end.

