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

unit form_settings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  EditBtn, Buttons, Clipbrd, LCLTranslator;

type

  { TFormSettings }

  TFormSettings = class(TForm)
    BitBtnCalculateIpv6: TBitBtn;
    BitBtnMacAddress: TBitBtn;
    BitBtnSaveSettings: TBitBtn;
    BitBtnCloseSettings: TBitBtn;
    CheckBoxUsePF: TCheckBox;
    CheckBoxUseIpv6: TCheckBox;
    CheckBoxUseSystray: TCheckBox;
    CheckBoxUseDnsmasq: TCheckBox;
    CheckBoxUseSudo: TCheckBox;
    CheckBoxUseZfs: TCheckBox;
    ComboBoxInterface: TComboBox;
    ComboBoxBridgeInterface: TComboBox;
    ComboBoxIp4: TComboBox;
    ComboBoxIp6: TComboBox;
    ComboBoxZpool: TComboBox;
    DirectoryEditImagesPath: TDirectoryEdit;
    EditBridgeIpv6: TEdit;
    EditBridgeMac: TEdit;
    EditIpv6Prefix: TEdit;
    EditSubnet: TEdit;
    EditVmPathSetting: TEdit;
    EditZfsCreateOptions: TEdit;
    EditRdpArgs: TEdit;
    FileNameEditBhyve: TFileNameEdit;
    FileNameEditQemuImg: TFileNameEdit;
    FileNameEditDoas: TFileNameEdit;
    FileNameEditSudo: TFileNameEdit;
    FileNameEditVncviewer: TFileNameEdit;
    FileNameEditBhyvectl: TFileNameEdit;
    FileNameEditXfreerdp: TFileNameEdit;
    FileNameEditSwtpm: TFileNameEdit;
    FileNameEditSwtpmIoctl: TFileNameEdit;
    FileNameEditBhyveload: TFileNameEdit;
    GroupBoxNatSettings: TGroupBox;
    GroupBoxBhyvePaths: TGroupBox;
    GroupBoxNetworkSettings: TGroupBox;
    GroupBoxRemoteToolPaths: TGroupBox;
    GroupBoxSwtpmToolPaths: TGroupBox;
    GroupBoxExtraToolPaths: TGroupBox;
    GroupBoxUserToolPaths: TGroupBox;
    GroupBoxZfsSettings: TGroupBox;
    GroupBoxRemoteToolSettings: TGroupBox;
    Label1: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label26: TLabel;
    Label30: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label5: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label2: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LabelNetmask: TLabel;
    LabelExternalIp6: TLabel;
    PageControlSettings: TPageControl;
    Prefix: TLabel;
    StatusBarBhyveSettings: TStatusBar;
    TabSheetNetwork: TTabSheet;
    TabSheetGeneral: TTabSheet;
    TabSheetPaths: TTabSheet;
    procedure BitBtnCalculateIpv6Click(Sender: TObject);
    procedure BitBtnCloseSettingsClick(Sender: TObject);
    procedure BitBtnMacAddressClick(Sender: TObject);
    procedure BitBtnSaveSettingsClick(Sender: TObject);
    procedure CheckBoxUsePFChange(Sender: TObject);
    procedure CheckBoxUseDnsmasqChange(Sender: TObject);
    procedure CheckBoxUseIpv6Change(Sender: TObject);
    procedure CheckBoxUseZfsChange(Sender: TObject);
    procedure ComboBoxInterfaceChange(Sender: TObject);
    procedure ComboBoxZpoolChange(Sender: TObject);
    procedure EditSubnetExit(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    function FormValidate():Boolean;
    procedure FillComboZpool();
    procedure LoadDefaultValues();
  public

  end;

var
  FormSettings: TFormSettings;

implementation

{$R *.lfm}

uses
  unit_configuration, unit_component, unit_global, unit_util, unit_language, LazLogger;

{ TFormSettings }

procedure TFormSettings.FormShow(Sender: TObject);
begin
  Self.PageControlSettings.ActivePageIndex:=0;
  FillComboZpool;

  ComboBoxInterface.Clear;
  ComboBoxIp4.Clear;
  ComboBoxIp6.Clear;

  FillComboExternalInterfaceList(ComboBoxInterface, 'ether');
  FillComboExternalInterfaceList(ComboBoxBridgeInterface, 'bridge');

  EditBridgeMac.Clear;
  EditBridgeIpv6.Clear;

  LoadDefaultValues();

  DebugLogger.UseStdOut:= False;
  DebugLogger.CloseLogFileBetweenWrites:= true;
  DebugLogger.LogName:= GetUserDir + '.config/bhyvemgr/bhyvemgr.log';

  DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : '+debugln_bhyve_settings_opened);
end;

function TFormSettings.FormValidate(): Boolean;
begin
  Result:=True;

  StatusBarBhyveSettings.Font.Color:=clRed;

  if CheckBoxUseZfs.Checked then
  begin
    if not CheckZfsSupport() or not FileExists(ZfsCmd) or not FileExists(ZpoolCmd)
       or (ComboBoxZpool.ItemIndex = -1) then
    begin
      StatusBarBhyveSettings.SimpleText:= check_zfs;
      Result:=False;
      Exit;
    end
  end;

  if CheckBoxUseDnsmasq.Checked then
  begin
    if not FileExists(DnsmasqBinPath) then
    begin
      StatusBarBhyveSettings.SimpleText:=check_dnsmasq;
      Result:=False;
      Exit;
    end;
  end;

  if CheckBoxUseIpv6.Checked then
  begin
    if (Trim(EditIpv6Prefix.Text) = EmptyStr) or not (CheckIpv6Address(EditIpv6Prefix.Text)) then
    begin
      StatusBarBhyveSettings.SimpleText:=check_ipv6;
      Result:=False;
      Exit;
    end;
  end;

  if CheckBoxUsePF.Checked then
  begin
    if not (FileExists(PfctlCmd)) then
    begin
      StatusBarBhyveSettings.SimpleText:=check_pf;
      Result:=False;
      Exit;
    end;
  end;

  if CheckBoxUseSudo.Checked then
  begin
    if not FileExists(FileNameEditSudo.FileName) or not (ExtractFileName(FileNameEditSudo.FileName) = 'sudo') then
    begin
      StatusBarBhyveSettings.SimpleText:=check_sudo;
      Result:=False;
      Exit;
    end;
  end
  else
  begin
    if not FileExists(FileNameEditDoas.FileName) or not (ExtractFileName(FileNameEditDoas.FileName) = 'doas') then
    begin
      StatusBarBhyveSettings.SimpleText:=check_doas;
      Result:=False;
      Exit;
    end;
  end;

  if GetOsreldate.ToInt64 >= 1403000 then
  begin
    {$ifdef CPUAMD64}
    if not FileExists(FileNameEditSwtpm.FileName) or not (ExtractFileName(FileNameEditSwtpm.FileName) = 'swtpm') then
    begin
      StatusBarBhyveSettings.SimpleText:=Format(check_base_binary, ['swtpm']);
      Result:=False;
      Exit;
    end
    else if not FileExists(FileNameEditSwtpmIoctl.FileName) or not (ExtractFileName(FileNameEditSwtpmIoctl.FileName) = 'swtpm_ioctl') then
    begin
      StatusBarBhyveSettings.SimpleText:=Format(check_base_binary, ['swtpm_ioctl']);
      Result:=False;
      Exit;
    end;
    {$endif}
  end;

  if ComboBoxBridgeInterface.ItemIndex = -1 then
  begin
    StatusBarBhyveSettings.SimpleText:=check_bridge;
    Result:=False;
    Exit;
  end
  else if (Trim(EditSubnet.Text) = EmptyStr) or not (CheckCidrRange(EditSubnet.Text)) then
  begin
    StatusBarBhyveSettings.SimpleText:=check_subnet;
    Result:=False;
    Exit;
  end
  else if (ComboBoxInterface.ItemIndex = -1) or (ComboBoxIp4.ItemIndex = -1) then
  begin
    StatusBarBhyveSettings.SimpleText:=check_nat;
    Result:=False;
    Exit;
  end
  else if not FileExists(FileNameEditBhyve.FileName) or not (ExtractFileName(FileNameEditBhyve.FileName) = 'bhyve') then
  begin
    StatusBarBhyveSettings.SimpleText:=Format(check_base_binary, ['bhyve']);
    Result:=False;
    Exit;
  end
  else if not FileExists(FileNameEditBhyvectl.FileName) or not (ExtractFileName(FileNameEditBhyvectl.FileName) = 'bhyvectl') then
  begin
    StatusBarBhyveSettings.SimpleText:=Format(check_base_binary, ['bhyvectl']);
    Result:=False;
    Exit;
  end
  {$ifdef CPUAMD64}
  else if not FileExists(FileNameEditBhyveload.FileName) or not (ExtractFileName(FileNameEditBhyveload.FileName) = 'bhyveload') then
  begin
    StatusBarBhyveSettings.SimpleText:=Format(check_base_binary, ['bhyveload']);
    Result:=False;
    Exit;
  end
  {$endif}
  else if not FileExists(FileNameEditVncviewer.FileName) or not (ExtractFileName(FileNameEditVncviewer.FileName) = 'remote-viewer') then
  begin
    StatusBarBhyveSettings.SimpleText:=check_vnc;
    Result:=False;
    Exit;
  end
  else if not FileExists(FileNameEditXfreerdp.FileName) or not (ExtractFileName(FileNameEditXfreerdp.FileName) = 'xfreerdp3') then
  begin
    DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : '+debugln_check_freerdp);
  end
  else if not FileExists(ChownCmd) or not (ExtractFileName(ChownCmd) = 'chown') then
  begin
    StatusBarBhyveSettings.SimpleText:=Format(check_base_binary, ['chown']);
    Result:=False;
    Exit;
  end
  else if not FileExists(ChmodCmd) or not (ExtractFileName(ChmodCmd) = 'chmod') then
  begin
    StatusBarBhyveSettings.SimpleText:=Format(check_base_binary, ['chmod']);
    Result:=False;
    Exit;
  end
  else if not FileExists(CpCmd) or not (ExtractFileName(CpCmd) = 'cp') then
  begin
    StatusBarBhyveSettings.SimpleText:=Format(check_base_binary, ['cp']);
    Result:=False;
    Exit;
  end
  else if not FileExists(IfconfigCmd) or not (ExtractFileName(IfconfigCmd) = 'ifconfig') then
  begin
    StatusBarBhyveSettings.SimpleText:=Format(check_base_binary, ['ifconfig']);
    Result:=False;
    Exit;
  end
  else if not FileExists(InstallCmd) or not (ExtractFileName(InstallCmd) = 'install') then
  begin
    StatusBarBhyveSettings.SimpleText:=Format(check_base_binary, ['install']);
    Result:=False;
    Exit;
  end
  else if not FileExists(FetchCmd) or not (ExtractFileName(FetchCmd) = 'fetch') then
  begin
    StatusBarBhyveSettings.SimpleText:=Format(check_base_binary, ['fetch']);
    Result:=False;
    Exit;
  end
  else if not FileExists(FileCmd) or not (ExtractFileName(FileCmd) = 'file') then
  begin
    StatusBarBhyveSettings.SimpleText:=Format(check_base_binary, ['file']);
    Result:=False;
    Exit;
  end
  else if not FileExists(KillCmd) or not (ExtractFileName(KillCmd) = 'kill') then
  begin
    StatusBarBhyveSettings.SimpleText:=Format(check_base_binary, ['kill']);
    Result:=False;
    Exit;
  end
  else if not FileExists(KldloadCmd) or not (ExtractFileName(KldloadCmd) = 'kldload') then
  begin
    StatusBarBhyveSettings.SimpleText:=Format(check_base_binary, ['kldload']);
    Result:=False;
    Exit;
  end
  else if not FileExists(KldstatCmd) or not (ExtractFileName(KldstatCmd) = 'kldstat') then
  begin
    StatusBarBhyveSettings.SimpleText:=Format(check_base_binary, ['kldstat']);
    Result:=False;
    Exit;
  end
  else if not FileExists(MakefsCmd) or not (ExtractFileName(MakefsCmd) = 'makefs') then
  begin
    StatusBarBhyveSettings.SimpleText:=Format(check_base_binary, ['makefs']);
    Result:=False;
    Exit;
  end
  else if not FileExists(PciconfCmd) or not (ExtractFileName(PciconfCmd) = 'pciconf') then
  begin
    StatusBarBhyveSettings.SimpleText:=Format(check_base_binary, ['pciconf']);
    Result:=False;
    Exit;
  end
  else if not FileExists(PgrepCmd) or not (ExtractFileName(PgrepCmd) = 'pgrep') then
  begin
    StatusBarBhyveSettings.SimpleText:=Format(check_base_binary, ['pgrep']);
    Result:=False;
    Exit;
  end
  else if not FileExists(FileNameEditQemuImg.FileName) or not (ExtractFileName(FileNameEditQemuImg.FileName) = 'qemu-img') then
  begin
    DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : '+debugln_check_qemu);
  end
  else if not FileExists(RmCmd) or not (ExtractFileName(RmCmd) = 'rm') then
  begin
    StatusBarBhyveSettings.SimpleText:=Format(check_base_binary, ['rm']);
    Result:=False;
    Exit;
  end
  else if not FileExists(ServiceCmd) or not (ExtractFileName(ServiceCmd) = 'service') then
  begin
    StatusBarBhyveSettings.SimpleText:=Format(check_base_binary, ['service']);
    Result:=False;
    Exit;
  end
  else if not FileExists(SysctlCmd) or not (ExtractFileName(SysctlCmd) = 'sysctl') then
  begin
    StatusBarBhyveSettings.SimpleText:=Format(check_base_binary, ['sysctl']);
    Result:=False;
    Exit;
  end
  else if not FileExists(TruncateCmd) or not (ExtractFileName(TruncateCmd) = 'truncate') then
  begin
    StatusBarBhyveSettings.SimpleText:=Format(check_base_binary, ['truncate']);
    Result:=False;
    Exit;
  end
  else if not FileExists(XzCmd) or not (ExtractFileName(XzCmd) = 'xz') then
  begin
    StatusBarBhyveSettings.SimpleText:=Format(check_base_binary, ['xz']);
    Result:=False;
    Exit;
  end
end;

procedure TFormSettings.CheckBoxUseZfsChange(Sender: TObject);
begin
  if CheckKernelModule('zfs') then
  begin
    if not CheckBoxUseZfs.Checked then
      EditVmPathSetting.Text:='/usr/local/bhyvemgr'
    else
      EditVmPathSetting.Text:='/'+ZfsZpool+'/bhyvemgr';
  end
  else
  begin
    CheckBoxUseZfs.Checked:=False;
    EditVmPathSetting.Text:='/usr/local/bhyvemgr';
  end;
end;

procedure TFormSettings.ComboBoxInterfaceChange(Sender: TObject);
begin
  ComboBoxIp4.Clear;
  FillComboExternalIp4List(ComboBoxIp4, ComboBoxInterface.Text);
  ComboBoxIp6.Clear;
  FillComboExternalIp6List(ComboBoxIp6, ComboBoxInterface.Text);
end;

procedure TFormSettings.ComboBoxZpoolChange(Sender: TObject);
begin
  if CheckKernelModule('zfs') then
  begin
    if not CheckBoxUseZfs.Checked then
      EditVmPathSetting.Text:='/usr/local/bhyvemgr'
    else
      EditVmPathSetting.Text:='/'+ComboBoxZpool.Text+'/bhyvemgr';
  end
  else
  begin
    CheckBoxUseZfs.Checked:=False;
    EditVmPathSetting.Text:='/usr/local/bhyvemgr';
  end;
end;

procedure TFormSettings.EditSubnetExit(Sender: TObject);
begin
  if not CheckCidrRange(EditSubnet.Text) then
  begin
    PageControlSettings.ActivePageIndex:=0;
    EditSubnet.SetFocus;
    StatusBarBhyveSettings.Font.Color:=clRed;
    StatusBarBhyveSettings.SimpleText:=check_subnet;
  end
  else
  begin
    LabelNetmask.Caption:=ExtractNetMask(ExtractCidr(EditSubnet.Text).ToInteger);

    StatusBarBhyveSettings.Font.Color:=clTeal;
    StatusBarBhyveSettings.SimpleText:=EmptyStr;
  end;
end;

procedure TFormSettings.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : '+debugln_bhyve_settings_closed);
end;

procedure TFormSettings.BitBtnSaveSettingsClick(Sender: TObject);
var
  ConfigFile : ConfigurationClass;
begin
  ConfigFile:=ConfigurationClass.Create(GetUserDir + '.config/bhyvemgr/config.conf');

  if FormValidate() then
  begin
    if CheckBoxUseSudo.Checked then
    begin
      ConfigFile.SetOption('general', 'use_sudo', 'yes');
      SetUseSudo('yes');
    end
    else
    begin
      ConfigFile.SetOption('general', 'use_sudo', 'no');
      SetUseSudo('no');
    end;

    if CheckBoxUseZfs.Checked then
    begin
      ConfigFile.SetOption('general', 'use_zfs', 'yes');
      ConfigFile.SetOption('zfs', 'zfs_zpool', ComboBoxZpool.Text);
      ConfigFile.SetOption('zfs', 'zfs_create_options', EditZfsCreateOptions.Text);

      SetUseZfs('yes');
      SetZfsZpool(ComboBoxZpool.Text);
      SetZfsCreateOptions(EditZfsCreateOptions.Text);
    end
    else
    begin
      ConfigFile.SetOption('general', 'use_zfs', 'no');
      SetUseZfs('no');
    end;

    if CheckBoxUseDnsmasq.Checked then
    begin
      ConfigFile.SetOption('general', 'use_dnsmasq', 'yes');
      ConfigFile.SetOption('network', 'bridge_interface', ComboBoxBridgeInterface.Text);
      ConfigFile.SetOption('network', 'subnet', EditSubnet.Text);

      if not DirectoryExists(DnsmasqDirectory) then
        CreateDirectory(DnsmasqDirectory, 'root');

      SetUseDnsmasq('yes');
      SetBridgeInterface(ComboBoxBridgeInterface.Text);
      SetSubnet(EditSubnet.Text);
    end
    else
    begin
      ConfigFile.SetOption('general', 'use_dnsmasq', 'no');
      ConfigFile.SetOption('network', 'bridge_interface', ComboBoxBridgeInterface.Text);
      ConfigFile.SetOption('network', 'subnet', EditSubnet.Text);

      SetUseDnsmasq('no');
      SetBridgeInterface(ComboBoxBridgeInterface.Text);
      SetSubnet(EditSubnet.Text);
    end;

    if CheckBoxUseSystray.Checked then
    begin
      ConfigFile.SetOption('general', 'use_systray', 'yes');
      SetUseSystray('yes');
    end
    else
    begin
      ConfigFile.SetOption('general', 'use_systray', 'no');
      SetUseSystray('no');
    end;

    if CheckBoxUseIpv6.Checked then
    begin
      ConfigFile.SetOption('general', 'use_ipv6', 'yes');
      ConfigFile.SetOption('network', 'ipv6_prefix', EditIpv6Prefix.Text);
      SetUseIpv6('yes');
      SetIpv6Prefix(EditIpv6Prefix.Text);
    end
    else
    begin
      ConfigFile.SetOption('general', 'use_ipv6', 'no');
      SetUseIpv6('no');
      SetIpv6Prefix(EmptyStr);
    end;

    if CheckBoxUsePF.Checked then
    begin
      ConfigFile.SetOption('general', 'use_pf', 'yes');
      ConfigFile.SetOption('network', 'external_interface', ComboBoxInterface.Text);
      ConfigFile.SetOption('network', 'external_ipv4', ComboBoxIp4.Text);
      ConfigFile.SetOption('network', 'external_ipv6', ComboBoxIp6.Text);
      SetUsePf('yes');
      SetExternalInterface(ComboBoxInterface.Text);
      SetExternalIpv4(ComboBoxIp4.Text);
      SetExternalIpv6(ComboBoxIp6.Text);
    end
    else
    begin
      ConfigFile.SetOption('general', 'use_pf', 'no');
      SetUsePf('yes');
      SetExternalInterface(EmptyStr);
      SetExternalIpv4(EmptyStr);
      SetExternalIpv6(EmptyStr);
    end;

    ConfigFile.SetOption('general','vm_path', EditVmPathSetting.Text);
    ConfigFile.SetOption('general','cloudvm_images_path', DirectoryEditImagesPath.Directory);

    ConfigFile.SetOption('bhyve-tools','bhyve_cmd', FileNameEditBhyve.FileName);
    ConfigFile.SetOption('bhyve-tools','bhyvectl_cmd', FileNameEditBhyvectl.FileName);
    ConfigFile.SetOption('bhyve-tools','bhyveload_cmd', FileNameEditBhyveload.FileName);

    ConfigFile.SetOption('extra-tools','swtpm_cmd', FileNameEditSwtpm.FileName);
    ConfigFile.SetOption('extra-tools','swtpm_ioctl_cmd', FileNameEditSwtpmIoctl.FileName);
    ConfigFile.SetOption('extra-tools','qemu-img_cmd', FileNameEditQemuImg.FileName);

    ConfigFile.SetOption('remote-tools','vncviewer_cmd', FileNameEditVncviewer.FileName);
    ConfigFile.SetOption('remote-tools','xfreerdp_cmd', FileNameEditXfreerdp.FileName);
    ConfigFile.SetOption('remote-tools','xfreerdp_args', EditRdpArgs.Text);

    ConfigFile.SetOption('user-tools','doas_cmd', FileNameEditDoas.FileName);
    ConfigFile.SetOption('user-tools','sudo_cmd', FileNameEditSudo.FileName);

    SetVmPath(EditVmPathSetting.Text);
    SetCloudVmImagesPath(DirectoryEditImagesPath.Directory);

    SetBhyveCmd(FileNameEditBhyve.FileName);
    SetBhyvectlCmd(FileNameEditBhyvectl.FileName);
    SetBhyveloadCmd(FileNameEditBhyveload.FileName);

    SetSwtpmCmd(FileNameEditSwtpm.FileName);
    SetSwtpmIoctlCmd(FileNameEditSwtpmIoctl.FileName);
    SetQemuImgCmd(FileNameEditQemuImg.FileName);

    SetVncviewerCmd(FileNameEditVncviewer.FileName);
    SetXfreerdpCmd(FileNameEditXfreerdp.FileName);
    SetXfreerdpArgs(EditRdpArgs.Text);

    SetSudoCmd(FileNameEditSudo.FileName);
    SetDoasCmd(FileNameEditDoas.FileName);

    if UseZfs = 'yes' then
      ZfsCreateDataset(VmPath.Remove(0,1))
    else
      CreateDirectory(VmPath, GetCurrentUserName());

    StatusBarBhyveSettings.Font.Color:=clTeal;
    StatusBarBhyveSettings.SimpleText:=EmptyStr;

    DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : '+ debugln_bhyve_settings_saved);
    MessageDlg(settings_saved_title, settings_saved, mtInformation, [mbOK], 0);

    SetNewConfig(False);
  end;
  ConfigFile.Free;
end;

procedure TFormSettings.CheckBoxUsePFChange(Sender: TObject);
begin
  if CheckKernelModule('pf') then
  begin
    if CheckBoxUsePF.Checked then
      GroupBoxNatSettings.Enabled:=True
    else
      GroupBoxNatSettings.Enabled:=False;
  end
  else
  begin
    CheckBoxUsePF.Checked:=False;
    GroupBoxNatSettings.Enabled:=False;
  end;
end;

procedure TFormSettings.BitBtnCloseSettingsClick(Sender: TObject);
begin
  Close;
end;

procedure TFormSettings.BitBtnMacAddressClick(Sender: TObject);
begin
  EditBridgeMac.Text:= Clipboard.AsText;
end;

procedure TFormSettings.BitBtnCalculateIpv6Click(Sender: TObject);
begin
  if not (EditIpv6Prefix.Text = EmptyStr) and not (EditBridgeMac.Text = EmptyStr) and
     (CheckMacAddress(EditBridgeMac.Text)) and (CheckIpv6Address(EditIpv6Prefix.Text)) then
  begin
    EditBridgeIpv6.Text:=GetNewIp6Address(EditIpv6Prefix.Text, EditBridgeMac.Text);
    StatusBarBhyveSettings.Font.Color:=clTeal;
    StatusBarBhyveSettings.SimpleText:=Format(calculated_ipv6, [ComboBoxBridgeInterface.Text]);
  end
  else
  begin
    StatusBarBhyveSettings.Font.Color:=clRed;
    StatusBarBhyveSettings.SimpleText:=Format(no_calculated_ipv6, [ComboBoxBridgeInterface.Text]);
  end;
end;

procedure TFormSettings.CheckBoxUseDnsmasqChange(Sender: TObject);
begin
  if CheckBoxUseDnsmasq.Checked then
  begin
    EditSubnet.Enabled:=True;
    CheckBoxUseIpv6.Enabled:=True;
    CheckBoxUsePF.Enabled:=True;
  end
  else
  begin
    EditSubnet.Enabled:=True;
    CheckBoxUseIpv6.Enabled:=False;
    CheckBoxUseIpv6.Checked:=False;
    CheckBoxUsePF.Enabled:=False;
    CheckBoxUsePF.Checked:=False;
  end;
end;

procedure TFormSettings.CheckBoxUseIpv6Change(Sender: TObject);
begin
  if CheckBoxUseIpv6.Checked then
  begin
    BitBtnCalculateIpv6.Enabled:=True;
    EditIpv6Prefix.Enabled:=True;
    EditBridgeMac.Enabled:=True;
    BitBtnMacAddress.Enabled:=True;
    EditBridgeIpv6.Enabled:=True;
    EditBridgeIpv6.Clear;

    if EditIpv6Prefix.Text = EmptyStr then
      EditIpv6Prefix.Text:=GenerateIpv6Preffix();
  end
  else
  begin
    BitBtnCalculateIpv6.Enabled:=False;
    EditIpv6Prefix.Enabled:=False;
    EditBridgeIpv6.Enabled:=False;
    EditBridgeMac.Enabled:=False;
    BitBtnMacAddress.Enabled:=False;
    EditBridgeIpv6.Clear;
  end;
end;

procedure TFormSettings.FillComboZpool();
begin
  ComboBoxZpool.Clear;
  FillComboZpoolList(ComboBoxZpool);
end;

procedure TFormSettings.LoadDefaultValues();
begin
  if UseZfs = 'yes' then
    CheckBoxUseZfs.Checked:=True
  else
    CheckBoxUseZfs.Checked:=False;

  if UseDnsmasq = 'yes' then
  begin
    CheckBoxUseDnsmasq.Checked:=True;
    EditSubnet.Enabled:=True;
    CheckBoxUseIpv6.Enabled:=True;
  end
  else
  begin
    CheckBoxUseDnsmasq.Checked:=False;
    EditSubnet.Enabled:=True;
    CheckBoxUseIpv6.Checked:=False;
    CheckBoxUseIpv6.Enabled:=False;
  end;

  if UsePf = 'yes' then
  begin
    CheckBoxUsePF.Checked:=True;
    ComboBoxInterface.ItemIndex:=ComboBoxInterface.Items.IndexOf(ExternalInterface);
    FillComboExternalIp4List(ComboBoxIp4, ComboBoxInterface.Text);
    ComboBoxIp4.ItemIndex:=ComboBoxIp4.Items.IndexOf(ExternalIpv4);
    FillComboExternalIp6List(ComboBoxIp6, ComboBoxInterface.Text);
    ComboBoxIp6.ItemIndex:=ComboBoxIp6.Items.IndexOf(ExternalIpv6);
    GroupBoxNatSettings.Enabled:=True;
  end
  else
  begin
    CheckBoxUsePF.Checked:=False;
    ComboBoxInterface.ItemIndex:=-1;
    ComboBoxIp4.ItemIndex:=-1;
    ComboBoxIp6.ItemIndex:=-1;
    GroupBoxNatSettings.Enabled:=False;
  end;

  if UseSudo = 'yes' then
    CheckBoxUseSudo.Checked:=True
  else
    CheckBoxUseSudo.Checked:=False;

  if UseSystray = 'yes' then
    CheckBoxUseSystray.Checked:=True
  else
    CheckBoxUseSystray.Checked:=False;

  if (UseIpv6 = 'yes') and (UseDnsmasq = 'yes') then
  begin
    BitBtnCalculateIpv6.Enabled:=True;
    CheckBoxUseIpv6.Checked:=True;
    EditBridgeMac.Enabled:=True;
    BitBtnMacAddress.Enabled:=True;
    EditBridgeIpv6.Enabled:=True;
    EditIpv6Prefix.Enabled:=True;
    EditIpv6Prefix.Text:=Ipv6Prefix;
  end
  else
  begin
    BitBtnCalculateIpv6.Enabled:=False;
    CheckBoxUseIpv6.Checked:=False;
    EditBridgeMac.Enabled:=False;
    BitBtnMacAddress.Enabled:=False;
    EditBridgeIpv6.Enabled:=False;
    EditIpv6Prefix.Enabled:=False;
  end;

  ComboBoxBridgeInterface.ItemIndex:=ComboBoxBridgeInterface.Items.IndexOf(BridgeInterface);

  EditSubnet.Text:=Subnet;

  LabelNetmask.Caption:=ExtractNetMask(ExtractCidr(EditSubnet.Text).ToInteger);

  ComboBoxZpool.ItemIndex:=ComboBoxZpool.Items.IndexOf(ZfsZpool);
  EditZfsCreateOptions.Text:=ZfsCreateOptions;

  FileNameEditBhyve.Text:=BhyveCmd;
  FileNameEditBhyvectl.Text:=BhyvectlCmd;
  FileNameEditBhyveload.Text:=BhyveLoadCmd;

  FileNameEditVncviewer.Text:=VncviewerCmd;
  FileNameEditXfreerdp.Text:=XfreerdpCmd;
  EditRdpArgs.Text:=XfreerdpArgs;

  FileNameEditDoas.Text:=DoasCmd;
  FileNameEditSudo.Text:=SudoCmd;

  FileNameEditSwtpm.Text:=SwtpmCmd;
  FileNameEditSwtpmIoctl.Text:=SwtpmIoctlCmd;

  FileNameEditQemuImg.Text:=QemuImgCmd;

  EditVmPathSetting.Text:=VmPath;

  DirectoryEditImagesPath.RootDir:=GetUserDir;
  DirectoryEditImagesPath.Text:=CloudVmImagesPath;

  StatusBarBhyveSettings.SimpleText:= EmptyStr;
end;

end.

