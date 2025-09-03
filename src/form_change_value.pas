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

unit form_change_value;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  SpinEx, LCLTranslator;

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


