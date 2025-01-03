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

unit form_rdp_connection;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ComCtrls;

type

  { TFormRdpConnection }

  TFormRdpConnection = class(TForm)
    BitBtnConnect: TBitBtn;
    ComboBoxResolution: TComboBox;
    EditUsername: TEdit;
    EditPassword: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    StatusBarDisplayDevice: TStatusBar;
    procedure FormDeactivate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    FormAction : String;
    FormUserName : String;
    FormResolution : String;
    function FormValidate():Boolean;
    procedure LoadDefaultValues();
  end;

var
  FormRdpConnection: TFormRdpConnection;

implementation

{$R *.lfm}

uses
  unit_component;

{ TFormRdpConnection }

procedure TFormRdpConnection.FormHide(Sender: TObject);
begin
  FormAction:=EmptyStr;
end;

procedure TFormRdpConnection.FormShow(Sender: TObject);
begin
  ComboBoxResolution.SetFocus;
end;

procedure TFormRdpConnection.FormDeactivate(Sender: TObject);
begin
  FormAction:=EmptyStr;
end;

function TFormRdpConnection.FormValidate(): Boolean;
begin
  Result:=True;

  if ComboBoxResolution.ItemIndex=-1 then  Result:=False
  else if (Trim(EditUsername.Text) = EmptyStr) then  Result:=False
  else if (Trim(EditPassword.Text) = EmptyStr) then  Result:=False;
end;

procedure TFormRdpConnection.LoadDefaultValues();
begin
  EditUsername.Clear;
  EditPassword.Clear;
  ComboBoxResolution.Clear;
  FillComboResolution(ComboBoxResolution);

  if FormUserName = EmptyStr then
  begin
    ComboBoxResolution.ItemIndex:=ComboBoxResolution.Items.IndexOf('1024x768');
  end
  else
  begin
    EditUsername.Text:=FormUserName;
    ComboBoxResolution.ItemIndex:=ComboBoxResolution.Items.IndexOf(FormResolution);
  end;
end;

end.

