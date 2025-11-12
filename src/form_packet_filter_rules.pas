unit form_packet_filter_rules;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Buttons,
  Grids, StdCtrls;

type

  { TFormPacketFilterRules }

  TFormPacketFilterRules = class(TForm)
    BitBtnInAddRule: TBitBtn;
    BitBtnInRemoveRule: TBitBtn;
    BitBtnSaveRules: TBitBtn;
    BitBtnOutAddRule: TBitBtn;
    BitBtnOutRemoveRule: TBitBtn;
    BitBtnRdrAddRule: TBitBtn;
    BitBtnRdrRemoveRule: TBitBtn;
    BitBtnClose: TBitBtn;
    ComboBoxInAf: TComboBox;
    ComboBoxInSourceType: TComboBox;
    ComboBoxInInterface: TComboBox;
    ComboBoxOutPort: TComboBox;
    ComboBoxOutAf: TComboBox;
    ComboBoxInPort: TComboBox;
    ComboBoxOutProto: TComboBox;
    ComboBoxOutInterface: TComboBox;
    ComboBoxInProto: TComboBox;
    ComboBoxRdrAf: TComboBox;
    ComboBoxRdrProto: TComboBox;
    ComboBoxRdrVmPort: TComboBox;
    ComboBoxOutHostType: TComboBox;
    ComboBoxRdrFromType: TComboBox;
    EditInSourceAddress: TEdit;
    EditOutPort: TEdit;
    EditOutHostAddress: TEdit;
    EditInPort: TEdit;
    EditRdrFromAddress: TEdit;
    EditRdrHostPort: TEdit;
    EditRdrVmPort: TEdit;
    GroupBox1: TGroupBox;
    GroupBox10: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    GroupBox8: TGroupBox;
    GroupBox9: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    PageControlPacketFilter: TPageControl;
    StatusBarPacketFilterRules: TStatusBar;
    StringGridOutRules: TStringGrid;
    StringGridInRules: TStringGrid;
    StringGridRdrRules: TStringGrid;
    TabSheetInbound: TTabSheet;
    TabSheetOutbound: TTabSheet;
    TabSheetRedirect: TTabSheet;
    procedure BitBtnCloseClick(Sender: TObject);
    procedure BitBtnInAddRuleClick(Sender: TObject);
    procedure BitBtnInRemoveRuleClick(Sender: TObject);
    procedure BitBtnOutAddRuleClick(Sender: TObject);
    procedure BitBtnOutRemoveRuleClick(Sender: TObject);
    procedure BitBtnRdrAddRuleClick(Sender: TObject);
    procedure BitBtnRdrRemoveRuleClick(Sender: TObject);
    procedure BitBtnSaveRulesClick(Sender: TObject);
    procedure ComboBoxInPortChange(Sender: TObject);
    procedure ComboBoxInProtoChange(Sender: TObject);
    procedure ComboBoxInSourceTypeChange(Sender: TObject);
    procedure ComboBoxOutHostTypeChange(Sender: TObject);
    procedure ComboBoxOutPortChange(Sender: TObject);
    procedure ComboBoxOutProtoChange(Sender: TObject);
    procedure ComboBoxRdrFromTypeChange(Sender: TObject);
    procedure ComboBoxRdrProtoChange(Sender: TObject);
    procedure ComboBoxRdrVmPortChange(Sender: TObject);
    procedure StringGridInRulesClick(Sender: TObject);
    procedure StringGridInRulesSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure StringGridOutRulesClick(Sender: TObject);
    procedure StringGridOutRulesSelectCell(Sender: TObject; aCol,
      aRow: Integer; var CanSelect: Boolean);
    procedure StringGridRdrRulesClick(Sender: TObject);
    procedure StringGridRdrRulesSelectCell(Sender: TObject; aCol,
      aRow: Integer; var CanSelect: Boolean);
  private
    RowInNumber : Integer;
    RowOutNumber : Integer;
    RowRdrNumber : Integer;
    procedure LoadInboundRules();
    procedure LoadOutboundRules();
    procedure LoadRedirectRules();
    procedure ClearStringGrid(Rules: TStringGrid);
    function InboundDuplicateCheck():Boolean;
    function OnboundDuplicateCheck():Boolean;
    function RedirectDuplicateCheck():Boolean;
    function FormInboundValidate():Boolean;
    function FormOutboundValidate():Boolean;
    function FormRedirectValidate():Boolean;
  public
    VmName : String;
    VmState : String;
    VmIp4Adress : String;
    VmIp6Adress : String;
    procedure LoadDefaultValues();
  end;

var
  FormPacketFilterRules: TFormPacketFilterRules;

implementation

{$R *.lfm}

uses
  unit_component, unit_global, unit_util, unit_language ,RegExpr, LazLogger;

{ TFormPacketFilterRules }

procedure TFormPacketFilterRules.ComboBoxRdrFromTypeChange(Sender: TObject);
begin
  if ComboBoxRdrFromType.Text = 'custom' then
    EditRdrFromAddress.Enabled:=True
  else
    EditRdrFromAddress.Enabled:=False;
end;

procedure TFormPacketFilterRules.ComboBoxRdrProtoChange(Sender: TObject);
begin
  ComboBoxRdrVmPort.Clear;
  EditRdrVmPort.Clear;

  ComboBoxRdrVmPort.AddItem('custom', TObject(0));

  if ComboBoxRdrProto.Text = 'tcp' then
    FillComboServicePorts(ComboBoxRdrVmPort, 'tcp');
  if ComboBoxRdrProto.Text = 'udp' then
    FillComboServicePorts(ComboBoxRdrVmPort, 'udp');
  if ComboBoxRdrProto.Text = 'sctp' then
    FillComboServicePorts(ComboBoxRdrVmPort, 'sctp');
end;

procedure TFormPacketFilterRules.ComboBoxRdrVmPortChange(Sender: TObject);
begin
  if ComboBoxRdrVmPort.Text = 'custom' then
  begin
    EditRdrVmPort.Clear;
    EditRdrVmPort.ReadOnly:=False;
    EditRdrVmPort.SetFocus;
  end
  else
  begin
    EditRdrVmPort.Text:=PtrInt(ComboBoxRdrVmPort.Items.Objects[ComboBoxRdrVmPort.ItemIndex]).ToString;
    EditRdrVmPort.ReadOnly:=True;
  end;
end;

procedure TFormPacketFilterRules.StringGridInRulesClick(Sender: TObject);
begin
  if RowInNumber > 0 then
    BitBtnInRemoveRule.Enabled:=True;
end;

procedure TFormPacketFilterRules.StringGridInRulesSelectCell(Sender: TObject;
  aCol, aRow: Integer; var CanSelect: Boolean);
begin
  RowInNumber:=aRow;
end;

procedure TFormPacketFilterRules.StringGridOutRulesClick(Sender: TObject);
begin
  if RowOutNumber > 0 then
    BitBtnOutRemoveRule.Enabled:=True;
end;

procedure TFormPacketFilterRules.StringGridOutRulesSelectCell(Sender: TObject;
  aCol, aRow: Integer; var CanSelect: Boolean);
begin
  RowOutNumber:=aRow;
end;

procedure TFormPacketFilterRules.StringGridRdrRulesClick(Sender: TObject);
begin
  if RowRdrNumber > 0 then
    BitBtnRdrRemoveRule.Enabled:=True;
end;

procedure TFormPacketFilterRules.StringGridRdrRulesSelectCell(Sender: TObject;
  aCol, aRow: Integer; var CanSelect: Boolean);
begin
  RowRdrNumber:=aRow;
end;

function TFormPacketFilterRules.RedirectDuplicateCheck(): Boolean;
var
  i : Integer;
  fromType : String;
  vmPort : String;
begin
  Result:=False;

  fromType:=ComboBoxRdrFromType.Text;
  vmPort:=ComboBoxRdrVmPort.Text;

  if fromType = 'custom' then
    fromType:=EditRdrFromAddress.Text;

  if vmPort = 'custom' then
    vmPort:=EditRdrVmPort.Text;

  for i:=1 to StringGridRdrRules.RowCount-1 do
  begin
    if (StringGridRdrRules.Cells[1,i] = ComboBoxRdrAf.Text) and
      (StringGridRdrRules.Cells[2,i] = ComboBoxRdrProto.Text) and
      (StringGridRdrRules.Cells[3,i] = fromType) and
      (StringGridRdrRules.Cells[4,i] = EditRdrHostPort.Text) and
      (StringGridRdrRules.Cells[5,i] = vmPort)
      then
    begin
      Result:=True;
      Break;
    end;
  end;
end;

function TFormPacketFilterRules.InboundDuplicateCheck(): Boolean;
var
  i : Integer;
  sourceType : String;
  customPort : String;
begin
  Result:=False;

  sourceType:=ComboBoxInSourceType.Text;
  customPort:=ComboBoxInPort.Text;

  if sourceType = 'custom' then
    sourceType:=EditInSourceAddress.Text;

  if customPort = 'custom' then
    customPort:=EditInPort.Text;

  for i:=1 to StringGridInRules.RowCount-1 do
  begin
    if (StringGridInRules.Cells[0,i] = ComboBoxInInterface.Text) and
      (StringGridInRules.Cells[1,i] = ComboBoxInAf.Text) and
      (StringGridInRules.Cells[2,i] = ComboBoxInProto.Text) and
      (StringGridInRules.Cells[3,i] = sourceType) and
      (StringGridInRules.Cells[4,i] = customPort) then
    begin
      Result:=True;
      Break;
    end;
  end;
end;

function TFormPacketFilterRules.OnboundDuplicateCheck(): Boolean;
var
  i : Integer;
  hostType : String;
  customPort : String;
begin
  Result:=False;

  hostType:=ComboBoxOutHostType.Text;
  customPort:=ComboBoxOutPort.Text;

  if hostType = 'custom' then
    hostType:=EditOutHostAddress.Text;

  if customPort = 'custom' then
    customPort:=EditOutPort.Text;

  for i:=1 to StringGridOutRules.RowCount-1 do
  begin
    if (StringGridOutRules.Cells[0,i] = ComboBoxOutInterface.Text) and
      (StringGridOutRules.Cells[1,i] = ComboBoxOutAf.Text) and
      (StringGridOutRules.Cells[2,i] = ComboBoxOutProto.Text) and
      (StringGridOutRules.Cells[3,i] = hostType) and
      (StringGridOutRules.Cells[4,i] = customPort) then
    begin
      Result:=True;
      Break;
    end;
  end;
end;

function TFormPacketFilterRules.FormInboundValidate(): Boolean;
begin
  Result:=True;

  StatusBarPacketFilterRules.Font.Color:=clRed;

  if ComboBoxInInterface.ItemIndex=-1 then
  begin
    StatusBarPacketFilterRules.SimpleText:=check_network_device;
    Result:=False;
    Exit;
  end
  else if ComboBoxInAf.ItemIndex=-1 then
  begin
    StatusBarPacketFilterRules.SimpleText:=check_af;
    Result:=False;
    Exit;
  end
  else if ComboBoxInProto.ItemIndex=-1 then
  begin
    StatusBarPacketFilterRules.SimpleText:=check_protocol;
    Result:=False;
    Exit;
  end
  else if ComboBoxInSourceType.ItemIndex=-1 then
  begin
    StatusBarPacketFilterRules.SimpleText:=check_source_info;
    Result:=False;
    Exit;
  end
  else if (ComboBoxInSourceType.Text = 'custom') and (ComboBoxInAf.Text = 'inet')
  and not (CheckIpvAddress(EditInSourceAddress.Text)) then
  begin
    StatusBarPacketFilterRules.SimpleText:=Format(check_custom_ip_source, ['IPv4']);
    Result:=False;
    Exit;
  end
  else if (ComboBoxInSourceType.Text = 'custom') and (ComboBoxInAf.Text = 'inet6')
  and not (CheckIpv6Address(EditInSourceAddress.Text)) then
  begin
    StatusBarPacketFilterRules.SimpleText:=Format(check_custom_ip_source, ['IPv6']);
    Result:=False;
    Exit;
  end
  else if ComboBoxInPort.ItemIndex = -1 then
  begin
    StatusBarPacketFilterRules.SimpleText:=check_port;
    Result:=False;
    Exit;
  end
  else if (ComboBoxInPort.Text = 'custom') and not (CheckNetworkPort(EditInPort.Text)) then
  begin
    StatusBarPacketFilterRules.SimpleText:=check_range_ports;
    Result:=False;
    Exit;
  end
  else if InboundDuplicateCheck() then
  begin
    StatusBarPacketFilterRules.SimpleText:=check_inbound_rule;
    Result:=False;
    Exit;
  end;
end;

function TFormPacketFilterRules.FormOutboundValidate(): Boolean;
begin
  Result:=True;

  StatusBarPacketFilterRules.Font.Color:=clRed;

  if ComboBoxOutInterface.ItemIndex=-1 then
  begin
    StatusBarPacketFilterRules.SimpleText:=check_network_device;
    Result:=False;
    Exit;
  end
  else if ComboBoxOutAf.ItemIndex=-1 then
  begin
    StatusBarPacketFilterRules.SimpleText:=check_af;
    Result:=False;
    Exit;
  end
  else if ComboBoxOutProto.ItemIndex=-1 then
  begin
    StatusBarPacketFilterRules.SimpleText:=check_protocol;
    Result:=False;
    Exit;
  end
  else if ComboBoxOutHostType.ItemIndex=-1 then
  begin
    StatusBarPacketFilterRules.SimpleText:=check_destination;
    Result:=False;
    Exit;
  end
  else if (ComboBoxOutHostType.Text = 'custom') and (ComboBoxOutAf.Text = 'inet')
  and not (CheckIpvAddress(EditOutHostAddress.Text)) then
  begin
    StatusBarPacketFilterRules.SimpleText:=Format(check_custom_ip_destination, ['IPv4']);
    Result:=False;
    Exit;
  end
  else if (ComboBoxOutHostType.Text = 'custom') and (ComboBoxOutAf.Text = 'inet6')
  and not (CheckIpv6Address(EditOutHostAddress.Text)) then
  begin
    StatusBarPacketFilterRules.SimpleText:=Format(check_custom_ip_destination, ['IPv6']);
    Result:=False;
    Exit;
  end
  else if ComboBoxOutPort.ItemIndex = -1 then
  begin
    StatusBarPacketFilterRules.SimpleText:=check_port;
    Result:=False;
    Exit;
  end
  else if (ComboBoxOutPort.Text = 'custom') and not (CheckNetworkPort(EditOutPort.Text)) then
  begin
    StatusBarPacketFilterRules.SimpleText:=check_range_ports;
    Result:=False;
    Exit;
  end
  else if OnboundDuplicateCheck() then
  begin
    StatusBarPacketFilterRules.SimpleText:=check_outbound_rule;
    Result:=False;
    Exit;
  end;
end;

function TFormPacketFilterRules.FormRedirectValidate(): Boolean;
begin
  Result:=True;

  StatusBarPacketFilterRules.Font.Color:=clRed;

  if ComboBoxRdrAf.ItemIndex=-1 then
  begin
    StatusBarPacketFilterRules.SimpleText:=check_af;
    Result:=False;
    Exit;
  end
  else if (ComboBoxRdrAf.Text = 'inet6') then
  begin
    StatusBarPacketFilterRules.SimpleText:=check_redirect_ipv6;
    Result:=False;
    Exit;
  end
  else if ComboBoxRdrProto.ItemIndex=-1 then
  begin
    StatusBarPacketFilterRules.SimpleText:=check_protocol;
    Result:=False;
    Exit;
  end
  else if ComboBoxRdrFromType.ItemIndex=-1 then
  begin
    StatusBarPacketFilterRules.SimpleText:=check_source_info;
    Result:=False;
    Exit;
  end
  else if (ComboBoxRdrFromType.Text = 'custom') and (ComboBoxRdrAf.Text = 'inet')
  and not (CheckIpvAddress(EditRdrFromAddress.Text)) then
  begin
    StatusBarPacketFilterRules.SimpleText:=Format(check_custom_ip_destination, ['IPv4']);
    Result:=False;
    Exit;
  end
  else if ComboBoxRdrVmPort.ItemIndex = -1 then
  begin
    StatusBarPacketFilterRules.SimpleText:=check_vm_port;
    Result:=False;
    Exit;
  end
  else if not (CheckNetworkPort(EditRdrHostPort.Text)) then
  begin
    StatusBarPacketFilterRules.SimpleText:=check_host_range_ports;
    Result:=False;
    Exit;
  end
  else if (ComboBoxRdrVmPort.Text = 'custom') and not (CheckNetworkPort(EditRdrVmPort.Text)) then
  begin
    StatusBarPacketFilterRules.SimpleText:=check_vm_port;
    Result:=False;
    Exit;
  end
  else if RedirectDuplicateCheck() then
  begin
    StatusBarPacketFilterRules.SimpleText:=check_redirect_rule;
    Result:=False;
    Exit;
  end;
end;

procedure TFormPacketFilterRules.ComboBoxOutHostTypeChange(Sender: TObject);
begin
  if ComboBoxOutHostType.Text = 'custom' then
    EditOutHostAddress.Enabled:=True
  else
    EditOutHostAddress.Enabled:=False;
end;

procedure TFormPacketFilterRules.BitBtnOutAddRuleClick(Sender: TObject);
var
  hostType : String;
  customPort : String;
begin
  hostType:=ComboBoxOutHostType.Text;
  customPort:=ComboBoxOutPort.Text;

  StatusBarPacketFilterRules.SimpleText:=EmptyStr;

  if FormOutboundValidate() then
  begin
    if hostType = 'custom' then
      hostType:=EditOutHostAddress.Text;

    if customPort = 'custom' then
      customPort:=EditOutPort.Text;

    StringGridOutRules.InsertRowWithValues(StringGridOutRules.RowCount, [ComboBoxOutInterface.Text, ComboBoxOutAf.Text, ComboBoxOutProto.Text, hostType, customPort]);
    BitBtnSaveRules.Enabled:=True;
  end;
end;

procedure TFormPacketFilterRules.BitBtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormPacketFilterRules.BitBtnInAddRuleClick(Sender: TObject);
var
  sourceType : String;
  customPort : String;
begin
  sourceType:=ComboBoxInSourceType.Text;
  customPort:=ComboBoxInPort.Text;

  StatusBarPacketFilterRules.SimpleText:=EmptyStr;

  if FormInboundValidate() then
  begin
    if sourceType = 'custom' then
      sourceType:=EditInSourceAddress.Text;

    if customPort = 'custom' then
      customPort:=EditInPort.Text;

    StringGridInRules.InsertRowWithValues(StringGridInRules.RowCount, [ComboBoxInInterface.Text, ComboBoxInAf.Text, ComboBoxInProto.Text, sourceType, customPort]);
    BitBtnSaveRules.Enabled:=True;
  end;
end;

procedure TFormPacketFilterRules.BitBtnInRemoveRuleClick(Sender: TObject);
begin
  if RowInNumber > 0 then
  begin
    StringGridInRules.DeleteRow(RowInNumber);
    BitBtnSaveRules.Enabled:=True;
  end;
  BitBtnInRemoveRule.Enabled:=False;
end;

procedure TFormPacketFilterRules.BitBtnOutRemoveRuleClick(Sender: TObject);
begin
  if RowOutNumber > 0 then
  begin
    StringGridOutRules.DeleteRow(RowOutNumber);
    BitBtnSaveRules.Enabled:=True;
  end;
  BitBtnOutRemoveRule.Enabled:=False;
end;

procedure TFormPacketFilterRules.BitBtnRdrAddRuleClick(Sender: TObject);
var
  fromType : String;
  vmPort : String;
begin
  fromType:=ComboBoxRdrFromType.Text;
  vmPort:=ComboBoxRdrVmPort.Text;

  StatusBarPacketFilterRules.SimpleText:=EmptyStr;

  if FormRedirectValidate() then
  begin
    if fromType = 'custom' then
      fromType:=EditOutHostAddress.Text;

    if vmPort = 'custom' then
      vmPort:=EditRdrVmPort.Text;

    StringGridRdrRules.InsertRowWithValues(StringGridRdrRules.RowCount, [ComboBoxRdrAf.Text, ComboBoxRdrProto.Text, fromType, EditRdrHostPort.Text, vmPort]);
    BitBtnSaveRules.Enabled:=True;
  end;
end;

procedure TFormPacketFilterRules.BitBtnRdrRemoveRuleClick(Sender: TObject);
begin
  if RowRdrNumber > 0 then
  begin
    StringGridRdrRules.DeleteRow(RowRdrNumber);
    BitBtnSaveRules.Enabled:=True;
  end;
  BitBtnRdrRemoveRule.Enabled:=False;
end;

procedure TFormPacketFilterRules.BitBtnSaveRulesClick(Sender: TObject);
var
  i : Integer;
  InboundRules : String;
  OutboundRules : String;
  RedirectRules : String;
  flagRule : String;
  IpAddress : String;
  ExternalIpAddress : String;
begin
  InboundRules:=EmptyStr;
  OutboundRules:=EmptyStr;
  RedirectRules:=EmptyStr;

  for i:=1 to StringGridInRules.RowCount-1 do
  begin
    flagRule:=EmptyStr;
    IpAddress:=VmIp4Adress;

    if StringGridInRules.Cells[2,i] = 'tcp' then
      flagRule:=' flags S/SA';

    if StringGridInRules.Cells[1,i] = 'inet6' then
      IpAddress:=VmIp6Adress;

    InboundRules:=InboundRules+'pass out on '+StringGridInRules.Cells[0,i]+' '+StringGridInRules.Cells[1,i]+' proto '+StringGridInRules.Cells[2,i]+' from '+StringGridInRules.Cells[3,i]+' to '+IpAddress+' port '+StringGridInRules.Cells[4,i]+flagRule+sLineBreak;
  end;

  for i:=1 to StringGridOutRules.RowCount-1 do
  begin
    flagRule:=EmptyStr;
    IpAddress:=VmIp4Adress;

    if StringGridOutRules.Cells[2,i] = 'tcp' then
      flagRule:=' flags S/SA';

    if StringGridOutRules.Cells[1,i] = 'inet6' then
      IpAddress:=VmIp6Adress;

    OutboundRules:=OutboundRules+'pass in on '+StringGridOutRules.Cells[0,i]+' '+StringGridOutRules.Cells[1,i]+' proto '+StringGridOutRules.Cells[2,i]+' from '+IpAddress+' to '+StringGridOutRules.Cells[3,i]+' port '+StringGridOutRules.Cells[4,i]+flagRule+sLineBreak;
  end;

  for i:=1 to StringGridRdrRules.RowCount-1 do
  begin
    IpAddress:=VmIp4Adress;
    ExternalIpAddress:=ExternalIpv4;

    RedirectRules:=RedirectRules+'rdr pass on '+ExternalInterface+' '+StringGridRdrRules.Cells[0,i]+' proto '+StringGridRdrRules.Cells[1,i]+' from '+StringGridRdrRules.Cells[2,i]+' to '+ExternalIpAddress+' port '+StringGridRdrRules.Cells[3,i]+' -> '+IpAddress+' port '+StringGridRdrRules.Cells[4,i]+sLineBreak;
  end;

  if PfCreateRules(VmName, InboundRules, 'pass-in') and PfCreateRules(VmName, OutboundRules, 'pass-out') and PfCreateRules(VmName, RedirectRules, 'rdr') then
  begin
    StatusBarPacketFilterRules.Font.Color:=clTeal;
    StatusBarPacketFilterRules.SimpleText:=Format(save_rules, [VmName]);

    MessageDialog(mtInformation, Format(save_rules, [VmName]));

    if CheckVmRunning(VmName) > 0 then
    begin
      if MessageDialog(mtConfirmation, Format(vm_apply_rules_confirmation, [VmName])) = mrYes then
      begin
        PfUnloadRules(VmName, 'pass-in');
        PfUnloadRules(VmName, 'pass-out');
        PfUnloadRules(VmName, 'rdr');

        if PfLoadRules(VmName, 'pass-in') and PfLoadRules(VmName, 'pass-out') and PfLoadRules(VmName, 'rdr') then
        begin
          StatusBarPacketFilterRules.Font.Color:=clTeal;
          StatusBarPacketFilterRules.SimpleText:=Format(save_rules_reload, [VmName]);

          MessageDialog(mtInformation, Format(save_rules_reload, [VmName]));
          DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : '+Format(save_rules_reload ,[VmName]));
        end
        else
        begin
          StatusBarPacketFilterRules.Font.Color:=clRed;
          StatusBarPacketFilterRules.SimpleText:=Format(save_rules_reload_error, [VmName]);
        end;
      end;
    end;

    DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : '+Format(save_rules,[VmName]));
  end;
end;

procedure TFormPacketFilterRules.ComboBoxInPortChange(Sender: TObject);
begin
  if ComboBoxInPort.Text = 'custom' then
  begin
    EditInPort.Clear;
    EditInPort.ReadOnly:=False;
    EditInPort.SetFocus;
  end
  else
  begin
    EditInPort.Text:=PtrInt(ComboBoxInPort.Items.Objects[ComboBoxInPort.ItemIndex]).ToString;
    EditInPort.ReadOnly:=True;
  end;
end;

procedure TFormPacketFilterRules.ComboBoxInProtoChange(Sender: TObject);
begin
  ComboBoxInPort.Clear;
  EditInPort.Clear;

  ComboBoxInPort.AddItem('custom', TObject(0));

  if ComboBoxInProto.Text = 'tcp' then
    FillComboServicePorts(ComboBoxInPort, 'tcp');
  if ComboBoxInProto.Text = 'udp' then
    FillComboServicePorts(ComboBoxInPort, 'udp');
  if ComboBoxInProto.Text = 'sctp' then
    FillComboServicePorts(ComboBoxInPort, 'sctp');
end;

procedure TFormPacketFilterRules.ComboBoxInSourceTypeChange(Sender: TObject);
begin
  if ComboBoxInSourceType.Text = 'custom' then
    EditInSourceAddress.Enabled:=True
  else
    EditInSourceAddress.Enabled:=False;
end;

procedure TFormPacketFilterRules.ComboBoxOutPortChange(Sender: TObject);
begin
  if ComboBoxOutPort.Text = 'custom' then
  begin
    EditOutPort.Clear;
    EditOutPort.ReadOnly:=False;
    EditOutPort.SetFocus;
  end
  else
  begin
    EditOutPort.Text:=PtrInt(ComboBoxOutPort.Items.Objects[ComboBoxOutPort.ItemIndex]).ToString;
    EditOutPort.ReadOnly:=True;
  end;
end;

procedure TFormPacketFilterRules.ComboBoxOutProtoChange(Sender: TObject);
begin
  ComboBoxOutPort.Clear;
  EditOutPort.Clear;

  ComboBoxOutPort.AddItem('custom', TObject(0));

  if ComboBoxOutProto.Text = 'tcp' then
    FillComboServicePorts(ComboBoxOutPort, 'tcp');
  if ComboBoxOutProto.Text = 'udp' then
    FillComboServicePorts(ComboBoxOutPort, 'udp');
  if ComboBoxOutProto.Text = 'sctp' then
    FillComboServicePorts(ComboBoxOutPort, 'sctp');
end;

procedure TFormPacketFilterRules.LoadDefaultValues();
begin
  PageControlPacketFilter.ActivePageIndex:=0;

  ComboBoxOutAf.Clear;
  FillComboPacketFilterAf(ComboBoxOutAf);
  ComboBoxOutProto.Clear;
  FillComboPacketFilterProto(ComboBoxOutProto);
  ComboBoxOutHostType.Clear;
  FillComboPacketFilterHostType(ComboBoxOutHostType);
  ComboBoxOutInterface.Clear;
  FillComboExternalInterfaceList(ComboBoxOutInterface, 'bridge');
  ComboBoxOutInterface.ItemIndex:=ComboBoxOutInterface.Items.IndexOf(BridgeInterface);

  ComboBoxInAf.Clear;
  FillComboPacketFilterAf(ComboBoxInAf);
  ComboBoxInProto.Clear;
  FillComboPacketFilterProto(ComboBoxInProto);
  ComboBoxInSourceType.Clear;
  FillComboPacketFilterHostType(ComboBoxInSourceType);
  ComboBoxInInterface.Clear;
  FillComboExternalInterfaceList(ComboBoxInInterface, 'bridge');
  ComboBoxInInterface.ItemIndex:=ComboBoxInInterface.Items.IndexOf(BridgeInterface);

  ComboBoxRdrAf.Clear;
  FillComboPacketFilterAf(ComboBoxRdrAf);
  ComboBoxRdrProto.Clear;
  FillComboPacketFilterProto(ComboBoxRdrProto);
  ComboBoxRdrFromType.Clear;
  FillComboPacketFilterHostType(ComboBoxRdrFromType);

  ComboBoxOutPort.Clear;
  ComboBoxInPort.Clear;

  EditRdrVmPort.ReadOnly:=True;
  EditOutHostAddress.Enabled:=False;
  EditOutPort.ReadOnly:=True;
  EditInSourceAddress.Enabled:=False;
  EditInPort.ReadOnly:=True;

  BitBtnSaveRules.Enabled:=False;

  BitBtnInRemoveRule.Enabled:=False;
  BitBtnOutRemoveRule.Enabled:=False;
  BitBtnRdrRemoveRule.Enabled:=False;

  StringGridInRules.Cells[0,0]:=in_grid_interface;
  StringGridInRules.Cells[3,0]:=in_grid_source;
  StringGridInRules.Cells[4,0]:=in_grid_port;

  StringGridOutRules.Cells[0,0]:=out_grid_interface;
  StringGridOutRules.Cells[3,0]:=out_grid_destination;
  StringGridOutRules.Cells[4,0]:=out_grid_port;

  StringGridRdrRules.Cells[2,0]:=rdr_grid_source;
  StringGridRdrRules.Cells[3,0]:=rdr_grid_host_port;
  StringGridRdrRules.Cells[4,0]:=rdr_grid_vm_port;

  ClearStringGrid(StringGridInRules);
  ClearStringGrid(StringGridOutRules);
  ClearStringGrid(StringGridRdrRules);

  LoadInboundRules();
  LoadOutboundRules();
  LoadRedirectRules();

  StatusBarPacketFilterRules.Font.Color:=clTeal;
  StatusBarPacketFilterRules.SimpleText:=virtual_machine+' : '+VmName;
end;

procedure TFormPacketFilterRules.LoadInboundRules();
var
  InRules : TStringList;
  RulesPath : String;
  RegexObj: TRegExpr;
begin
  InRules:=TStringList.Create();
  RulesPath:=VmPath+'/'+VmName+'/pf/pass-in.rules';

  if FileExists(RulesPath) then
    InRules.LoadFromFile(RulesPath);

  RegexObj := TRegExpr.Create('pass\s\S+\son\s(\S+)\s(\S+)\s\S+\s+(\S+)\s+from\s+(\S+)\sto\s\S+\sport\s(\S+)');

  if RegexObj.Exec(InRules.Text) then
  begin
    repeat
      StringGridInRules.InsertRowWithValues(StringGridInRules.RowCount, [RegexObj.Match[1], RegexObj.Match[2], RegexObj.Match[3], RegexObj.Match[4], RegexObj.Match[5]]);
    until not RegexObj.ExecNext;
  end;

  RegexObj.Free;
  InRules.Free;
end;

procedure TFormPacketFilterRules.LoadOutboundRules();
var
  OutRules : TStringList;
  RulesPath : String;
  RegexObj: TRegExpr;
begin
  OutRules:=TStringList.Create();
  RulesPath:=VmPath+'/'+VmName+'/pf/pass-out.rules';

  if FileExists(RulesPath) then
    OutRules.LoadFromFile(RulesPath);

  RegexObj := TRegExpr.Create('pass\s\S+\son\s(\S+)\s(\S+)\s\S+\s(\S+)\s\S+\s\S+\s\S+\s(\S+)\sport\s(\S+)');

  if RegexObj.Exec(OutRules.Text) then
  begin
    repeat
      StringGridOutRules.InsertRowWithValues(StringGridOutRules.RowCount, [RegexObj.Match[1], RegexObj.Match[2], RegexObj.Match[3], RegexObj.Match[4], RegexObj.Match[5]]);
    until not RegexObj.ExecNext;
  end;

  RegexObj.Free;
  OutRules.Free;
end;

procedure TFormPacketFilterRules.LoadRedirectRules();
var
  RdrRules : TStringList;
  RulesPath : String;
  RegexObj: TRegExpr;
begin
  RdrRules:=TStringList.Create();
  RulesPath:=VmPath+'/'+VmName+'/pf/rdr.rules';

  if FileExists(RulesPath) then
    RdrRules.LoadFromFile(RulesPath);

  RegexObj := TRegExpr.Create('(\S+)\s+proto\s+(\S+)\s+from\s+(\S+)\s+to\s+\S+\s+port\s+(\S+)\s+->\s+\S+\s+port\s+(\S+)');

  if RegexObj.Exec(RdrRules.Text) then
  begin
    repeat
      StringGridRdrRules.InsertRowWithValues(StringGridRdrRules.RowCount, [RegexObj.Match[1], RegexObj.Match[2], RegexObj.Match[3], RegexObj.Match[4], RegexObj.Match[5]]);
    until not RegexObj.ExecNext;
  end;

  RegexObj.Free;
  RdrRules.Free;
end;

procedure TFormPacketFilterRules.ClearStringGrid(Rules: TStringGrid);
var
  i : Integer;
begin
  for i:=1 to Rules.RowCount-1 do
  begin
    Rules.DeleteRow(Rules.RowCount-1);
  end;
end;

end.

