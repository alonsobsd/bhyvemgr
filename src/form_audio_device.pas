{ SPDX-License-Identifier: BSD-3-Clause

Copyright (c) 2024-2026, J. Alonso Cárdenas <acardenas@bsd-peru.org>

}

unit form_audio_device;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ComCtrls;

type

  { TFormAudioDevice }

  TFormAudioDevice = class(TForm)
    BitBtnSave: TBitBtn;
    BitBtnClose: TBitBtn;
    EditPlayDevice: TEdit;
    EditRecDevice: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    StatusBarAudioDevice: TStatusBar;
    procedure BitBtnCloseClick(Sender: TObject);
  private
  public
    FormAction : String;
    function FormValidate():Boolean;
    procedure LoadDefaultValues();
  end;

var
  FormAudioDevice: TFormAudioDevice;

implementation

{$R *.lfm}

uses
  unit_global;

{ TFormAudioDevice }

procedure TFormAudioDevice.BitBtnCloseClick(Sender: TObject);
begin
  Close;
end;

function TFormAudioDevice.FormValidate(): Boolean;
begin
  Result:=True;

  if Trim(EditPlayDevice.Text).IsEmpty then Result:=False;
  if Trim(EditRecDevice.Text).IsEmpty then Result:=False;
end;

procedure TFormAudioDevice.LoadDefaultValues;
begin
  EditPlayDevice.Text:='/dev/dsp0';
  EditRecDevice.Text:='/dev/dsp0';
end;

end.

