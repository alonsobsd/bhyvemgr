{ SPDX-License-Identifier: BSD-3-Clause

Copyright (c) 2024-2026, J. Alonso Cárdenas <acardenas@bsd-peru.org>

}

unit form_console_device;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ComCtrls;

type

  { TFormConsoleDevice }

  TFormConsoleDevice = class(TForm)
    BitBtnSave: TBitBtn;
    BitBtnClose: TBitBtn;
    ComboBoxDevice: TComboBox;
    EditPath: TEdit;
    EditName: TEdit;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    StatusBarConsoleDevice: TStatusBar;
    procedure BitBtnCloseClick(Sender: TObject);
  private
    procedure FillComboDevice(Combo: TComboBox);
  public
    FormAction : String;
    VmName : String;
    VtconName : String;
    function FormValidate():Boolean;
    procedure LoadDefaultValues();
  end;

var
  FormConsoleDevice: TFormConsoleDevice;

implementation

{$R *.lfm}

uses
  unit_global;

{ TFormConsoleDevice }

procedure TFormConsoleDevice.BitBtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormConsoleDevice.FillComboDevice(Combo: TComboBox);
begin
  Combo.Clear;
  Combo.Items.Add('virtio-console');
end;

function TFormConsoleDevice.FormValidate(): Boolean;
begin
  Result:=True;

  if ComboBoxDevice.ItemIndex=-1 then Result:=False
  else if (Trim(EditName.Text).IsEmpty) then Result:=False
  else if (Trim(EditPath.Text).IsEmpty) then Result:=False;
end;

procedure TFormConsoleDevice.LoadDefaultValues();
begin
  FillComboDevice(ComboBoxDevice);
  ComboBoxDevice.ItemIndex:=ComboBoxDevice.Items.IndexOf('virtio-console');
  EditName.Text:=VtconName;
  EditPath.Text:=VmPath +'/'+VmName+'/vtcon/'+VtconName+'.sock';
end;

end.

