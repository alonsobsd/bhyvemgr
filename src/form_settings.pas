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

unit form_settings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  EditBtn, Buttons;

type

  { TFormSettings }

  TFormSettings = class(TForm)
    BitBtnSaveSettings: TBitBtn;
    BitBtnCloseSettings: TBitBtn;
    CheckBoxUseSystray: TCheckBox;
    CheckBoxUseDnsmasq: TCheckBox;
    CheckBoxUseSudo: TCheckBox;
    CheckBoxUseZfs: TCheckBox;
    ComboBoxZpool: TComboBox;
    EditVmPathSetting: TEdit;
    EditBridgeInterface: TEdit;
    EditSubnet: TEdit;
    EditZfsCreateOptions: TEdit;
    EditRdpArgs: TEdit;
    FileNameEditBhyve: TFileNameEdit;
    FileNameEditDoas: TFileNameEdit;
    FileNameEditKldstat: TFileNameEdit;
    FileNameEditPgrep: TFileNameEdit;
    FileNameEditPciconf: TFileNameEdit;
    FileNameEditRm: TFileNameEdit;
    FileNameEditService: TFileNameEdit;
    FileNameEditSudo: TFileNameEdit;
    FileNameEditSysctl: TFileNameEdit;
    FileNameEditTruncate: TFileNameEdit;
    FileNameEditZfs: TFileNameEdit;
    FileNameEditZpool: TFileNameEdit;
    FileNameEditVncviewer: TFileNameEdit;
    FileNameEditBhyvectl: TFileNameEdit;
    FileNameEditXfreerdp: TFileNameEdit;
    FileNameEditSwtpm: TFileNameEdit;
    FileNameEditSwtpmIoctl: TFileNameEdit;
    FileNameEditBhyveload: TFileNameEdit;
    FileNameEditChown: TFileNameEdit;
    FileNameEditChmod: TFileNameEdit;
    FileNameEditIfconfig: TFileNameEdit;
    FileNameEditInstall: TFileNameEdit;
    FileNameEditKill: TFileNameEdit;
    FileNameEditKldload: TFileNameEdit;
    GroupBoxBhyvePaths: TGroupBox;
    GroupBoxRemoteToolPaths: TGroupBox;
    GroupBoxSwtpmToolPaths: TGroupBox;
    GroupBoxExtraToolPaths: TGroupBox;
    GroupBoxUserToolPaths: TGroupBox;
    GroupBoxZfsSettings: TGroupBox;
    GroupBoxNetworkSettings: TGroupBox;
    GroupBoxRemoteToolSettings: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    LabelNetmask: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PageControl1: TPageControl;
    StatusBarBhyveSettings: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure BitBtnCloseSettingsClick(Sender: TObject);
    procedure BitBtnSaveSettingsClick(Sender: TObject);
    procedure CheckBoxUseDnsmasqChange(Sender: TObject);
    procedure CheckBoxUseZfsChange(Sender: TObject);
    procedure ComboBoxZpoolChange(Sender: TObject);
    procedure EditSubnetExit(Sender: TObject);
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
  unit_configuration, unit_component, unit_global, unit_util;

{ TFormSettings }

procedure TFormSettings.FormShow(Sender: TObject);
begin
  Self.Caption:=FormBhyveManagerSettingsTitle;
  Self.PageControl1.ActivePageIndex:=0;
  FillComboZpool;
  LoadDefaultValues();
end;

function TFormSettings.FormValidate(): Boolean;
begin
  Result:=True;

  StatusBarBhyveSettings.Font.Color:=clRed;

  if CheckBoxUseZfs.Checked then
  begin
    if not CheckZfsSupport() or not FileExists(FileNameEditZfs.Text) or not FileExists(FileNameEditZpool.Text)
       or (ComboBoxZpool.ItemIndex = -1) then
    begin
      StatusBarBhyveSettings.SimpleText:='Support for zfs/zpool is not available';
      Result:=False;
    end
  end;

  if CheckBoxUseDnsmasq.Checked then
  begin
    if not FileExists('/usr/local/sbin/dnsmasq') then
    begin
      StatusBarBhyveSettings.SimpleText:='dnsmasq was not found. Please install dns/dnsmasq for fix it';
      Result:=False;
    end
    else if (Trim(EditSubnet.Text) = EmptyStr) or not (CheckCidrRange(EditSubnet.Text)) then
    begin
      StatusBarBhyveSettings.SimpleText:='a valid subnet must be defined. It will be used for assign ip address to virtual machines automatically';
      Result:=False;
    end;
  end;

  if CheckBoxUseSudo.Checked then
  begin
    if not FileExists(FileNameEditSudo.Text) or not (ExtractFileName(FileNameEditSudo.Text) = 'sudo') then
    begin
      StatusBarBhyveSettings.SimpleText:='sudo was not found. Please install security/sudo for fix it';
      Result:=False;
    end;
  end
  else
  begin
    if not FileExists(FileNameEditDoas.Text) or not (ExtractFileName(FileNameEditDoas.Text) = 'doas') then
    begin
      StatusBarBhyveSettings.SimpleText:='doas was not found. Please install security/doas for fix it';
      Result:=False;
    end;
  end;

  if GetOsreldate.ToInt64 >= 1500026 then
  begin
    {$ifdef CPUAMD64}
    if not FileExists(FileNameEditSwtpm.Text) or not (ExtractFileName(FileNameEditSwtpm.Text) = 'swtpm') then
    begin
      StatusBarBhyveSettings.SimpleText:='swtpm binary was not found';
      Result:=False;
    end
    else if not FileExists(FileNameEditSwtpmIoctl.Text) or not (ExtractFileName(FileNameEditSwtpmIoctl.Text) = 'swtpm_ioctl') then
    begin
      StatusBarBhyveSettings.SimpleText:='swtpm_iocl binary was not found';
      Result:=False;
    end;
    {$endif}
  end;

  if Trim(EditBridgeInterface.Text) = EmptyStr then
  begin
    StatusBarBhyveSettings.SimpleText:='a bridge name must be defined. It will be used by bhyvemgr for virtual machines network settings';
    Result:=False;
  end
  else if not FileExists(FileNameEditBhyve.Text) or not (ExtractFileName(FileNameEditBhyve.Text) = 'bhyve') then
  begin
    StatusBarBhyveSettings.SimpleText:='bhyve binary was not found';
    Result:=False;
  end
  else if not FileExists(FileNameEditBhyvectl.Text) or not (ExtractFileName(FileNameEditBhyvectl.Text) = 'bhyvectl') then
  begin
    StatusBarBhyveSettings.SimpleText:='bhyvectl binary was not found';
    Result:=False;
  end
  {$ifdef CPUAMD64}
  else if not FileExists(FileNameEditBhyveload.Text) or not (ExtractFileName(FileNameEditBhyveload.Text) = 'bhyveload') then
  begin
    StatusBarBhyveSettings.SimpleText:='bhyveload binary was not found';
    Result:=False;
  end
  {$endif}
  else if not FileExists(FileNameEditVncviewer.Text) or not (ExtractFileName(FileNameEditVncviewer.Text) = 'remote-viewer') then
  begin
    StatusBarBhyveSettings.SimpleText:='vnc support will not available. net-mgmt/virt-viewer is not installed.';
    Result:=False;
  end
  else if not FileExists(FileNameEditXfreerdp.Text) or not (ExtractFileName(FileNameEditXfreerdp.Text) = 'xfreerdp3') then
  begin
    StatusBarBhyveSettings.SimpleText:='freerdp support will not be available. net/freerdp3 is not installed.';
  end
  else if not FileExists(FileNameEditChown.Text) or not (ExtractFileName(FileNameEditChown.Text) = 'chown') then
  begin
    StatusBarBhyveSettings.SimpleText:='chown binary was not found';
    Result:=False;
  end
  else if not FileExists(FileNameEditChmod.Text) or not (ExtractFileName(FileNameEditChmod.Text) = 'chmod') then
  begin
    StatusBarBhyveSettings.SimpleText:='chmod binary was not found';
    Result:=False;
  end
  else if not FileExists(FileNameEditIfconfig.Text) or not (ExtractFileName(FileNameEditIfconfig.Text) = 'ifconfig') then
  begin
    StatusBarBhyveSettings.SimpleText:='ifconfig binary was not found';
    Result:=False;
  end
  else if not FileExists(FileNameEditInstall.Text) or not (ExtractFileName(FileNameEditInstall.Text) = 'install') then
  begin
    StatusBarBhyveSettings.SimpleText:='install binary was not found';
    Result:=False;
  end
  else if not FileExists(FileNameEditKill.Text) or not (ExtractFileName(FileNameEditKill.Text) = 'kill') then
  begin
    StatusBarBhyveSettings.SimpleText:='kill binary was not found';
    Result:=False;
  end
  else if not FileExists(FileNameEditKldload.Text) or not (ExtractFileName(FileNameEditKldload.Text) = 'kldload') then
  begin
    StatusBarBhyveSettings.SimpleText:='kldload binary was not found';
    Result:=False;
  end
  else if not FileExists(FileNameEditKldstat.Text) or not (ExtractFileName(FileNameEditKldstat.Text) = 'kldstat') then
  begin
    StatusBarBhyveSettings.SimpleText:='kldstat binary was not found';
    Result:=False;
  end
  else if not FileExists(FileNameEditPciconf.Text) or not (ExtractFileName(FileNameEditPciconf.Text) = 'pciconf') then
  begin
    StatusBarBhyveSettings.SimpleText:='pciconf binary was not found';
    Result:=False;
  end
  else if not FileExists(FileNameEditPgrep.Text) or not (ExtractFileName(FileNameEditPgrep.Text) = 'pgrep') then
  begin
    StatusBarBhyveSettings.SimpleText:='pgrep binary was not found';
    Result:=False;
  end
  else if not FileExists(FileNameEditRm.Text) or not (ExtractFileName(FileNameEditRm.Text) = 'rm') then
  begin
    StatusBarBhyveSettings.SimpleText:='rm binary was not found';
    Result:=False;
  end
  else if not FileExists(FileNameEditService.Text) or not (ExtractFileName(FileNameEditService.Text) = 'service') then
  begin
    StatusBarBhyveSettings.SimpleText:='service binary was not found';
    Result:=False;
  end
  else if not FileExists(FileNameEditSysctl.Text) or not (ExtractFileName(FileNameEditSysctl.Text) = 'sysctl') then
  begin
    StatusBarBhyveSettings.SimpleText:='sysctl binary was not found';
    Result:=False;
  end
  else if not FileExists(FileNameEditTruncate.Text) or not (ExtractFileName(FileNameEditTruncate.Text) = 'truncate') then
  begin
    StatusBarBhyveSettings.SimpleText:='truncate binary was not found';
    Result:=False;
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
  if CheckBoxUseDnsmasq.Checked then
  begin
    if not CheckCidrRange(EditSubnet.Text) then
    begin
      EditSubnet.SetFocus;
      StatusBarBhyveSettings.Font.Color:=clRed;
      StatusBarBhyveSettings.SimpleText:='a valid subnet must be defined. It will be used for assign ip address to virtual machines automatically';
    end
    else
    begin
      LabelNetmask.Caption:=ExtractNetMask(ExtractCidr(EditSubnet.Text).ToInteger);

      StatusBarBhyveSettings.Font.Color:=clTeal;
      StatusBarBhyveSettings.SimpleText:=EmptyStr;
    end;
  end
  else
  begin
    StatusBarBhyveSettings.SimpleText:=EmptyStr;
  end;
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
      ConfigFile.SetOption('network', 'bridge_interface', EditBridgeInterface.Text);
      ConfigFile.SetOption('network', 'subnet', EditSubnet.Text);

      SetUseDnsmasq('yes');
      SetBridgeInterface(EditBridgeInterface.Text);
      SetSubnet(EditSubnet.Text);
    end
    else
    begin
      ConfigFile.SetOption('general', 'use_dnsmasq', 'no');
      ConfigFile.SetOption('network', 'bridge_interface', EditBridgeInterface.Text);

      SetUseDnsmasq('no');
      SetBridgeInterface(EditBridgeInterface.Text);
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

    ConfigFile.SetOption('general','vm_path', EditVmPathSetting.Text);

    ConfigFile.SetOption('bhyve-tools','bhyve_cmd', FileNameEditBhyve.Text);
    ConfigFile.SetOption('bhyve-tools','bhyvectl_cmd', FileNameEditBhyvectl.Text);
    ConfigFile.SetOption('bhyve-tools','bhyveload_cmd', FileNameEditBhyveload.Text);

    ConfigFile.SetOption('extra-tools','chown_cmd', FileNameEditChown.Text);
    ConfigFile.SetOption('extra-tools','chmod_cmd', FileNameEditChmod.Text);
    ConfigFile.SetOption('extra-tools','ifconfig_cmd', FileNameEditIfconfig.Text);
    ConfigFile.SetOption('extra-tools','install_cmd', FileNameEditInstall.Text);
    ConfigFile.SetOption('extra-tools','kill_cmd', FileNameEditKill.Text);
    ConfigFile.SetOption('extra-tools','kldload_cmd', FileNameEditKldload.Text);
    ConfigFile.SetOption('extra-tools','kldstat_cmd', FileNameEditKldstat.Text);
    ConfigFile.SetOption('extra-tools','pciconf_cmd', FileNameEditPciconf.Text);
    ConfigFile.SetOption('extra-tools','pgrep_cmd', FileNameEditPgrep.Text);
    ConfigFile.SetOption('extra-tools','rm_cmd', FileNameEditRm.Text);
    ConfigFile.SetOption('extra-tools','service_cmd', FileNameEditService.Text);
    ConfigFile.SetOption('extra-tools','swtpm_cmd', FileNameEditSwtpm.Text);
    ConfigFile.SetOption('extra-tools','swtpm_ioctl_cmd', FileNameEditSwtpmIoctl.Text);
    ConfigFile.SetOption('extra-tools','sysctl_cmd', FileNameEditSysctl.Text);
    ConfigFile.SetOption('extra-tools','truncate_cmd', FileNameEditTruncate.Text);
    ConfigFile.SetOption('extra-tools','zfs_cmd', FileNameEditZfs.Text);
    ConfigFile.SetOption('extra-tools','zpool_cmd', FileNameEditZpool.Text);

    ConfigFile.SetOption('remote-tools','vncviewer_cmd', FileNameEditVncviewer.Text);
    ConfigFile.SetOption('remote-tools','xfreerdp_cmd', FileNameEditXfreerdp.Text);
    ConfigFile.SetOption('remote-tools','xfreerdp_args', EditRdpArgs.Text);

    ConfigFile.SetOption('user-tools','doas_cmd', FileNameEditDoas.Text);
    ConfigFile.SetOption('user-tools','sudo_cmd', FileNameEditSudo.Text);

    SetVmPath(EditVmPathSetting.Text);
    SetVncviewerCmd(FileNameEditVncviewer.Text);
    SetXfreerdpCmd(FileNameEditXfreerdp.Text);
    SetXfreerdpArgs(EditRdpArgs.Text);

    SetBhyveCmd(FileNameEditBhyve.Text);
    SetBhyvectlCmd(FileNameEditBhyvectl.Text);
    SetBhyveloadCmd(FileNameEditBhyveload.Text);

    SetSudoCmd(FileNameEditSudo.Text);
    SetDoasCmd(FileNameEditDoas.Text);
    SetChownCmd(FileNameEditChown.Text);
    SetChmodCmd(FileNameEditChmod.Text);
    SetIfconfigCmd(FileNameEditIfconfig.Text);
    SetInstallCmd(FileNameEditInstall.Text);
    SetKillCmd(FileNameEditKill.Text);
    SetKldloadCmd(FileNameEditKldload.Text);
    SetKldstatCmd(FileNameEditKldstat.Text);
    SetPciconfCmd(FileNameEditPciconf.Text);
    SetPgrepCmd(FileNameEditPgrep.Text);
    SetRmCmd(FileNameEditRm.Text);
    SetServiceCmd(FileNameEditService.Text);
    SetSwtpmCmd(FileNameEditSwtpm.Text);
    SetSwtpmIoctlCmd(FileNameEditSwtpmIoctl.Text);
    SetSysctlCmd(FileNameEditSysctl.Text);
    SetTruncateCmd(FileNameEditTruncate.Text);
    SetZfsCmd(FileNameEditZfs.Text);
    SetZpoolCmd(FileNameEditZpool.Text);

    if UseZfs = 'yes' then
      ZfsCreateDataset(VmPath.Remove(0,1))
    else
      CreateDirectory(VmPath, GetCurrentUserName());

    MessageDlg('Settings information', 'Settings were saved successfully', mtInformation, [mbOK], 0);

    SetNewConfig(False);
  end;
  ConfigFile.Free;
end;

procedure TFormSettings.BitBtnCloseSettingsClick(Sender: TObject);
begin
  Close;
end;

procedure TFormSettings.CheckBoxUseDnsmasqChange(Sender: TObject);
begin
  if CheckBoxUseDnsmasq.Checked then
    EditSubnet.Enabled:=True
  else
    EditSubnet.Enabled:=False;
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
  end
  else
  begin
    CheckBoxUseDnsmasq.Checked:=False;
    EditSubnet.Enabled:=False;
  end;

  if UseSudo = 'yes' then
    CheckBoxUseSudo.Checked:=True
  else
    CheckBoxUseSudo.Checked:=False;

  if UseSystray = 'yes' then
    CheckBoxUseSystray.Checked:=True
  else
    CheckBoxUseSystray.Checked:=False;

  EditBridgeInterface.Text:=BridgeInterface;

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

  FileNameEditSwtpmIoctl.Text:=DoasCmd;
  FileNameEditSwtpm.Text:=SudoCmd;

  FileNameEditChown.Text:=ChownCmd;
  FileNameEditChmod.Text:=ChmodCmd;
  FileNameEditIfconfig.Text:=IfconfigCmd;
  FileNameEditInstall.Text:=InstallCmd;
  FileNameEditKill.Text:=KillCmd;
  FileNameEditKldload.Text:=KldloadCmd;
  FileNameEditKldstat.Text:=KldstatCmd;
  FileNameEditPciconf.Text:=PciconfCmd;
  FileNameEditPgrep.Text:=PgrepCmd;
  FileNameEditRm.Text:=RmCmd;
  FileNameEditService.Text:=ServiceCmd;
  FileNameEditSwtpm.Text:=SwtpmCmd;
  FileNameEditSwtpmIoctl.Text:=SwtpmIoctlCmd;
  FileNameEditSysctl.Text:=SysctlCmd;
  FileNameEditTruncate.Text:=TruncateCmd;
  FileNameEditZfs.Text:=ZfsCmd;
  FileNameEditZpool.Text:=ZpoolCmd;

  EditVmPathSetting.Text:=VmPath;

  StatusBarBhyveSettings.SimpleText:= EmptyStr;
end;

end.

