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

unit form_display_device;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ComCtrls;

type

  { TFormDisplayDevice }

  TFormDisplayDevice = class(TForm)
    BitBtnSave: TBitBtn;
    CheckBoxOnlyLocalhost: TCheckBox;
    CheckBoxWaitVnc: TCheckBox;
    CheckBoxUsePassword: TCheckBox;
    ComboBoxResolution: TComboBox;
    ComboBoxVga: TComboBox;
    EditHost: TEdit;
    EditPassword: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    StatusBarDisplayDevice: TStatusBar;
    procedure CheckBoxOnlyLocalhostChange(Sender: TObject);
    procedure CheckBoxUsePasswordChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    HostPort : String;
    procedure FillComboVga(Combo: TComboBox);
  public
    FormAction : String;
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

procedure TFormDisplayDevice.FormShow(Sender: TObject);
begin
  FormDisplayDevice.Caption:=FormBhyveManagerDisplayDeviceTitle;
end;

procedure TFormDisplayDevice.CheckBoxOnlyLocalhostChange(Sender: TObject);
begin
  if CheckBoxOnlyLocalhost.Checked then
    EditHost.Text:='127.0.0.1:'+HostPort
  else
    EditHost.Text:='0.0.0.0:'+HostPort;
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

  if CheckBoxUsePassword.Checked and (Trim(EditPassword.Text)=EmptyStr) then
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
  HostPort:=GetNewVncPortNumber();

  FillComboVga(ComboBoxVga);
  FillComboResolution(ComboBoxResolution);

  ComboBoxVga.ItemIndex:=ComboBoxVga.Items.IndexOf('io');
  ComboBoxResolution.ItemIndex:=ComboBoxResolution.Items.IndexOf('1024x768');
  CheckBoxOnlyLocalhost.Checked:=True;
  EditPassword.Enabled:=False;
  EditHost.Text:='127.0.0.1:'+HostPort;
end;

end.

