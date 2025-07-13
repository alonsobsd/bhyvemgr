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

unit form_vm_info;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ComCtrls;

type

  { TFormVmInfo }

  TFormVmInfo = class(TForm)
    BitBtnSave: TBitBtn;
    CheckBoxIpv6: TCheckBox;
    CheckBoxRDP: TCheckBox;
    ComboBoxVmType: TComboBox;
    ComboBoxVmVersion: TComboBox;
    EditVmName: TEdit;
    EditVmDescription: TEdit;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    StatusBarVmInfo: TStatusBar;
    procedure ComboBoxVmTypeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    function FormValidate():Boolean;
  end;

var
  FormVmInfo: TFormVmInfo;

implementation

{$R *.lfm}

{ TFormVmInfo }

uses
  unit_component;

procedure TFormVmInfo.ComboBoxVmTypeChange(Sender: TObject);
begin
  ComboBoxVmVersion.Clear;
  FillComboSystemVersion(ComboBoxVmVersion, ComboBoxVmType.Text);
end;

procedure TFormVmInfo.FormShow(Sender: TObject);
begin
  ComboBoxVmType.Clear;
  FillComboSystemType(ComboBoxVmType);

  ComboBoxVmType.SetFocus;
end;

function TFormVmInfo.FormValidate(): Boolean;
begin
  Result:=True;

  if (ComboBoxVmType.ItemIndex=-1) then
    Result:=False
  else if (ComboBoxVmVersion.ItemIndex=-1) then
    Result:=False
  else if Trim(EditVmDescription.Text) = EmptyStr then
    Result:=False;
end;

end.

