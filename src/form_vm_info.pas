{ SPDX-License-Identifier: BSD-3-Clause

Copyright (c) 2024-2026, J. Alonso Cárdenas <acardenas@bsd-peru.org>

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
    BitBtnClose: TBitBtn;
    CheckBoxPf: TCheckBox;
    CheckBoxIpv6: TCheckBox;
    CheckBoxNat: TCheckBox;
    CheckBoxRDP: TCheckBox;
    ComboBoxNatInterface: TComboBox;
    ComboBoxVmType: TComboBox;
    ComboBoxVmVersion: TComboBox;
    EditVmIpv4Address: TEdit;
    EditVmIpv6Address: TEdit;
    EditVmDescription: TEdit;
    EditVmName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label26: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    PageControlVmInfo: TPageControl;
    StatusBarVmInfo: TStatusBar;
    TabSheetVmGeneral: TTabSheet;
    TabSheetVmNetworking: TTabSheet;
    procedure CheckBoxIpv6Change(Sender: TObject);
    procedure ComboBoxVmTypeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    Ip4Address : String;
    MacAddress : String;
    function FormValidate():Boolean;
  end;

var
  FormVmInfo: TFormVmInfo;

implementation

{$R *.lfm}

{ TFormVmInfo }

uses
  unit_component, unit_util, unit_language, unit_global;

procedure TFormVmInfo.ComboBoxVmTypeChange(Sender: TObject);
begin
  ComboBoxVmVersion.Clear;
  FillComboSystemVersion(ComboBoxVmVersion, ComboBoxVmType.Text);
end;

procedure TFormVmInfo.CheckBoxIpv6Change(Sender: TObject);
begin
  if CheckBoxIpv6.Checked then
  begin
    if not (MacAddress.IsEmpty) then
      EditVmIpv6Address.Text:=GetNewIp6Address(Ipv6Prefix, MacAddress);
  end;
end;

procedure TFormVmInfo.FormShow(Sender: TObject);
begin
  PageControlVmInfo.ActivePage:=TabSheetVmGeneral;

  StatusBarVmInfo.Font.Color:=clTeal;
  StatusBarVmInfo.SimpleText:=EmptyStr;

  ComboBoxVmType.Clear;
  ComboBoxNatInterface.Clear;

  FillComboExternalInterfaceList(ComboBoxNatInterface, 'ether');
  FillComboSystemType(ComboBoxVmType);

  ComboBoxNatInterface.ItemIndex:=ComboBoxNatInterface.Items.IndexOf(ExternalInterface);

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

