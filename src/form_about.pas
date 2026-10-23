{ SPDX-License-Identifier: BSD-3-Clause

Copyright (c) 2024-2026, J. Alonso Cárdenas <acardenas@bsd-peru.org>

}

unit form_about;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, LCLIntf, InterfaceBase;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    LabelArch: TLabel;
    LabelVersion: TLabel;
    Memo1: TMemo;
    procedure FormShow(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure Label1MouseEnter(Sender: TObject);
    procedure Label1MouseLeave(Sender: TObject);
  private

  public

  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.lfm}

uses
  unit_global;

{$I version.inc}

{ TFormAbout }

procedure TFormAbout.Label1Click(Sender: TObject);
begin
  OpenURL('https://github.com/alonsobsd/bhyvemgr');
end;

procedure TFormAbout.FormShow(Sender: TObject);
begin
  LabelVersion.Caption:='v'+BhyvemgrVersion;
  LabelArch.Caption:={$I %FPCTARGETCPU%}+'-'+GetLCLWidgetTypeName;
end;

procedure TFormAbout.Label1MouseEnter(Sender: TObject);
begin
  Screen.Cursor:=crHandPoint;
end;

procedure TFormAbout.Label1MouseLeave(Sender: TObject);
begin
  Screen.Cursor:=crDefault;
end;

end.

