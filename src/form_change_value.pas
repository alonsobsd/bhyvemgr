{ SPDX-License-Identifier: BSD-3-Clause

Copyright (c) 2024-2026, J. Alonso Cárdenas <acardenas@bsd-peru.org>

}

unit form_change_value;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  SpinEx;

type

  { TFormChangeValue }

  TFormChangeValue = class(TForm)
    BitBtnSave: TBitBtn;
    ComboBoxValue: TComboBox;
    SpinEditExValue: TSpinEditEx;
    Label1: TLabel;
  private
  public
    SettingType : String;
    procedure ShowSpinEx(MinValue : Int64; MaxValue : Int64; Increment : Int64);
    procedure ShowComboBox();
  end;

var
  FormChangeValue: TFormChangeValue;

implementation

{$R *.lfm}

uses
  Math;

{ TFormChangeValue }

procedure TFormChangeValue.ShowSpinEx(MinValue: Int64; MaxValue: Int64;
  Increment: Int64);
begin
  ComboBoxValue.Visible:=False;

  if not Assigned(SpinEditExValue) then
  begin
    SpinEditExValue:=TSpinEditEx.Create(Self);
    SpinEditExValue.Height:=32;
    SpinEditExValue.Width:=174;
    SpinEditExValue.Left:=144;
    SpinEditExValue.Top:=16;
    SpinEditExValue.Enabled:=True;
    SpinEditExValue.Visible:=True;
    SpinEditExValue.Name:='SpinEditExValue';
    SpinEditExValue.MinValue:=MinValue;
    SpinEditExValue.NullValue:=MinValue;
    SpinEditExValue.DirectInput:=True;
    SpinEditExValue.MaxValue:=(MaxValue div (1024**2));
    SpinEditExValue.Increment:=Increment;
    SpinEditExValue.Parent:=Self;
    SpinEditExValue.Show;
  end
  else
    SpinEditExValue.Show;
end;

procedure TFormChangeValue.ShowComboBox();
begin
  if Assigned(SpinEditExValue) then
    SpinEditExValue.Visible:=False;
  ComboBoxValue.Visible:=True;
end;

end.


