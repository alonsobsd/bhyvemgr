{ SPDX-License-Identifier: BSD-3-Clause

Copyright (c) 2024-2026, J. Alonso Cárdenas <acardenas@bsd-peru.org>

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
    BitBtnClose: TBitBtn;
    ComboBoxResolution: TComboBox;
    EditUsername: TEdit;
    EditPassword: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    StatusBarDisplayDevice: TStatusBar;
    procedure BitBtnCloseClick(Sender: TObject);
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

procedure TFormRdpConnection.BitBtnCloseClick(Sender: TObject);
begin
  Close;
end;

function TFormRdpConnection.FormValidate(): Boolean;
begin
  Result:=True;

  if ComboBoxResolution.ItemIndex=-1 then  Result:=False
  else if (Trim(EditUsername.Text).IsEmpty) then  Result:=False
  else if (Trim(EditPassword.Text).IsEmpty) then  Result:=False;
end;

procedure TFormRdpConnection.LoadDefaultValues();
begin
  EditUsername.Clear;
  EditPassword.Clear;
  ComboBoxResolution.Clear;
  FillComboResolution(ComboBoxResolution);

  if Trim(FormUserName).IsEmpty then
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

