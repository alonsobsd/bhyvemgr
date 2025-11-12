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

unit form_vm_info;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ComCtrls, LCLTranslator;

type

  { TFormVmInfo }

  TFormVmInfo = class(TForm)
    BitBtnSave: TBitBtn;
    CheckBoxPf: TCheckBox;
    CheckBoxIpv6: TCheckBox;
    CheckBoxNat: TCheckBox;
    CheckBoxRDP: TCheckBox;
    ComboBoxVmType: TComboBox;
    ComboBoxVmVersion: TComboBox;
    EditVmIpv4Address: TEdit;
    EditVmIpv6Address: TEdit;
    EditVmDescription: TEdit;
    EditVmName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    PageControlVmInfo: TPageControl;
    StatusBarVmInfo: TStatusBar;
    TabSheetVmGeneral: TTabSheet;
    TabSheetVmNetworking: TTabSheet;
    procedure ComboBoxVmTypeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    Ip4Address : String;
    function FormValidate():Boolean;
  end;

var
  FormVmInfo: TFormVmInfo;

implementation

{$R *.lfm}

{ TFormVmInfo }

uses
  unit_component, unit_util, unit_language;

procedure TFormVmInfo.ComboBoxVmTypeChange(Sender: TObject);
begin
  ComboBoxVmVersion.Clear;
  FillComboSystemVersion(ComboBoxVmVersion, ComboBoxVmType.Text);
end;

procedure TFormVmInfo.FormShow(Sender: TObject);
begin
  PageControlVmInfo.ActivePage:=TabSheetVmGeneral;

  StatusBarVmInfo.Font.Color:=clTeal;
  StatusBarVmInfo.SimpleText:=EmptyStr;

  ComboBoxVmType.Clear;
  FillComboSystemType(ComboBoxVmType);

  ComboBoxVmType.SetFocus;
end;

function TFormVmInfo.FormValidate(): Boolean;
begin
  Result:=True;

  StatusBarVmInfo.Font.Color:=clTeal;
  StatusBarVmInfo.SimpleText:=EmptyStr;

  if (ComboBoxVmType.ItemIndex=-1) then
  begin
    StatusBarVmInfo.Font.Color:=clRed;
    StatusBarVmInfo.SimpleText:=check_vm_type;

    PageControlVmInfo.ActivePage:=TabSheetVmGeneral;

    Result:=False;
    Exit;
  end
  else if (ComboBoxVmVersion.ItemIndex=-1) then
  begin
    StatusBarVmInfo.Font.Color:=clRed;
    StatusBarVmInfo.SimpleText:=check_vm_version;

    PageControlVmInfo.ActivePage:=TabSheetVmGeneral;

    Result:=False;
    Exit;
  end
  else if (CheckBoxIpv6.Checked) and not (CheckIpv6Address(EditVmIpv6Address.Text)) then
  begin
    StatusBarVmInfo.Font.Color:=clRed;
    StatusBarVmInfo.SimpleText:=check_vm_ipv6;

    PageControlVmInfo.ActivePage:=TabSheetVmNetworking;

    Result:=False;
    Exit;
  end
  else if (CheckBoxNat.Checked) and not (CheckIpvAddress(EditVmIpv4Address.Text)) then
  begin
    StatusBarVmInfo.Font.Color:=clRed;
    StatusBarVmInfo.SimpleText:=check_vm_ipv4;

    PageControlVmInfo.ActivePage:=TabSheetVmNetworking;

    Result:=False;
    Exit;
  end
  else if (CheckBoxPf.Checked) and not (CheckIpvAddress(EditVmIpv4Address.Text)) then
  begin
    StatusBarVmInfo.Font.Color:=clRed;
    StatusBarVmInfo.SimpleText:=check_vm_ipv4;

    PageControlVmInfo.ActivePage:=TabSheetVmNetworking;

    Result:=False;
    Exit;
  end;
end;

end.

