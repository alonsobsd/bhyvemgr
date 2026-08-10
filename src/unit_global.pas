{ BSD 3-Clause License

Copyright (c) 2024-2026, Alonso Cárdenas <acardenas@bsd-peru.org>

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

unit unit_global;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Dialogs;

{ General section }
function GetOsreldate:string;
procedure SetOsreldate(const Value:string);
function GetNewConfig:Boolean;
procedure SetNewConfig(const Value:Boolean);
function GetUseDnsmasq:string;
procedure SetUseDnsmasq(const Value:string);
function GetUseZfs:string;
procedure SetUseZfs(const Value:string);
function GetVmPath:string;
procedure SetVmPath(const Value:string);
function GetCloudVmImagesPath:string;
procedure SetCloudVmImagesPath(const Value:string);
function GetUseSystray:string;
procedure SetUseSystray(const Value:string);
function GetUseIpv6:string;
procedure SetUseIpv6(const Value:string);
function GetUsePf:string;
procedure SetUsePf(const Value:string);
function GetLanguage:string;
procedure SetLanguage(const Value:string);
{ Bhyve section }
function GetBhyveCmd:string;
procedure SetBhyveCmd(const Value:string);
function GetBhyvectlCmd:string;
procedure SetBhyvectlCmd(const Value:string);
function GetBhyveloadCmd:string;
procedure SetBhyveloadCmd(const Value:string);
{ Network section }
function GetBridgeInterface:string;
procedure SetBridgeInterface(const Value:string);
function GetSubnet:string;
procedure SetSubnet(const Value:string);
function GetIpv6Prefix:string;
procedure SetIpv6Prefix(const Value:string);
function GetExternalInterface:string;
procedure SetExternalInterface(const Value:string);
function GetExternalIpv4:string;
procedure SetExternalIpv4(const Value:string);
function GetExternalIpv6:string;
procedure SetExternalIpv6(const Value:string);
{ Remote tools section }
function GetVncviewerCmd:string;
procedure SetVncviewerCmd(const Value:string);
function GetXfreerdpCmd:string;
procedure SetXfreerdpCmd(const Value:string);
function GetXfreerdpArgs:string;
procedure SetXfreerdpArgs(const Value:string);
{ Other tools section }
function GetQemuImgCmd:string;
procedure SetQemuImgCmd(const value:string);
function GetSwtpmCmd:string;
procedure SetSwtpmCmd(const Value:string);
function GetSwtpmIoctlCmd:string;
procedure SetSwtpmIoctlCmd(const Value:string);
{ Zfs section }
function GetZfsZpool:string;
procedure SetZfsZpool(const Value:string);
function GetZfsCreateOptions:string;
procedure SetZfsCreateOptions(const value:string);

{ General section }
property Osreldate:string read GetOsreldate write SetOsreldate;
property NewConfig:boolean read GetNewConfig write SetNewConfig;
property UseDnsmasq:string read GetUseDnsmasq write SetUseDnsmasq;
property UseZfs:string read GetUseZfs write SetUseZfs;
property VmPath:string read GetVmPath write SetVmPath;
property CloudVmImagesPath:string read GetCloudVmImagesPath write SetCloudVmImagesPath;
property UseSystray:string read GetUseSystray write SetUseSystray;
property UseIpv6:string read GetUseIpv6 write SetUseIpv6;
property UsePf:string read GetUsePf write SetUsePf;
property Language:string read GetLanguage write SetLanguage;
{ Bhyve section }
property BhyveCmd:string read GetBhyveCmd write SetBhyveCmd;
property BhyvectlCmd:string read GetBhyvectlCmd write SetBhyvectlCmd;
property BhyveLoadCmd:string read GetBhyveloadCmd write SetBhyveloadCmd;
{ Network section }
property BridgeInterface:string read GetBridgeInterface write SetBridgeInterface;
property Subnet:string read GetSubnet write SetSubnet;
property Ipv6Prefix:string read GetIpv6Prefix write SetIpv6Prefix;
property ExternalInterface:string read GetExternalInterface write SetExternalInterface;
property ExternalIpv4:string read GetExternalIpv4 write SetExternalIpv4;
property ExternalIpv6:string read GetExternalIpv6 write SetExternalIpv6;
{ Remote tools section }
property VncviewerCmd:string read GetVncviewerCmd write SetVncviewerCmd;
property XfreerdpCmd:string read GetXfreerdpCmd write SetXfreerdpCmd;
property XfreerdpArgs:string read GetXfreerdpArgs write SetXfreerdpArgs;
{ Other tools section }
property QemuImgCmd:string read GetQemuImgCmd write SetQemuImgCmd;
property SwtpmCmd:string read GetSwtpmCmd write SetSwtpmCmd;
property SwtpmIoctlCmd:string read GetSwtpmIoctlCmd write SetSwtpmIoctlCmd;
{ Zfs section }
property ZfsZpool:string read GetZfsZpool write SetZfsZpool;
property ZfsCreateOptions:string read GetZfsCreateOptions write SetZfsCreateOptions;

const
  BhyveOui = '58:9c:fc:0';
  {$IFDEF DEBUG}
  DatadirPath = './';
  {$ELSE}
  DatadirPath = '/usr/local/share/bhyvemgr/';
  {$ENDIF}
  { Program paths }
  CHMOD_CMD = '/bin/chmod';
  CHOWN_CMD = '/usr/sbin/chown';
  CP_CMD = '/bin/cp';
  FETCH_CMD = '/usr/bin/fetch';
  FILE_CMD = '/usr/bin/file';
  IFCONFIG_CMD = '/sbin/ifconfig';
  INSTALL_CMD = '/usr/bin/install';
  KILL_CMD = '/bin/kill';
  KLDLOAD_CMD = '/sbin/kldload';
  KLDSTAT_CMD = '/sbin/kldstat';
  MAKEFS_CMD = '/usr/sbin/makefs';
  MDO_CMD = '/usr/bin/mdo';
  PCICONF_CMD = '/usr/sbin/pciconf';
  PFCTL_CMD = '/sbin/pfctl';
  PGREP_CMD = '/usr/bin/pgrep';
  RM_CMD = '/bin/rm';
  SERVICE_CMD = '/usr/sbin/service';
  SYSCTL_CMD = '/sbin/sysctl';
  TRUNCATE_CMD = '/usr/bin/truncate';
  XZ_CMD = '/usr/bin/xz';
  ZFS_CMD = '/sbin/zfs';
  ZPOOL_CMD = '/sbin/zpool';
  { bhyve and bhyvemgrd configuration files }
  BHYVEMGRD_SOCKET = '/var/run/bhyvemgrd/bhyvemgrd.sock';
  BHYVEMGRD_CONFIG_FILE = '/usr/local/etc/bhyvemgrd/daemon.conf';
  BHYVEMGRD_GROUP = 'bhyvemgrd';
  BHYVEMGR_CONFIG_FILE = '.config/bhyvemgr/gui.conf';
  COMMON_CONFIG_FILE = '/usr/local/etc/bhyvemgrd/common.conf';
  { bhyve log file }
  BHYVEMGR_LOG_FILE = '.config/bhyvemgr/bhyvemgr.log';
  { Firmware paths }
  BOOTROMUEFI_PATH = '/usr/local/share/uefi-firmware';
  BOOTROMUBOOT_PATH= '/usr/local/share/u-boot/u-boot-bhyve-arm64';
  { Keyboard layouts path }
  KEYBOARDLAYOUT_PATH = '/usr/share/bhyve/kbdlayout';
  DNSMASQDHCP_PATH = '/usr/local/etc/dnsmasq.d/bhyvemgr-dhcp';
  DNSMASQHOST_PATH = '/usr/local/etc/dnsmasq.d/bhyvemgr-host';
  DNSMASQBIN_CMD = '/usr/local/sbin/dnsmasq';
  SERVICES_FILE = '/etc/services';

  TrayIconNotifytimeout = 3000;
  FirstVncPortNumber = 5900;
  FirstGdbPortNumber = 50000;
  FirstComPortNumber = 60000;

implementation

var
  OsreldateVar: String;
  NewConfigVar: Boolean;
  UseDnsmasqVar: String;
  UseZfsVar: String;
  VmPathVar: String;
  CloudVmImagesPathVar: String;
  UseSystrayVar: String;
  UseIpv6Var: String;
  UsePfVar: String;
  LanguageVar: String;
  BhyveCmdVar: String;
  BhyvectlCmdVar: String;
  BhyveloadCmdVar: String;
  VncviewerCmdVar: String;
  XfreerdpCmdVar: String;
  XfreerdpArgsVar: String;
  BridgeInterfaceVar: String;
  SubnetVar: String;
  Ipv6PrefixVar: String;
  ExternalInterfaceVar: String;
  ExternalIpv4Var: String;
  ExternalIpv6Var: String;
  QemuImgCmdVar: String;
  SwtpmCmdVar: String;
  SwtpmIoctlCmdVar: String;
  ZfsEnableVar: String;
  ZfsZpoolVar: String;
  ZfsCreateOptionsVar: String;

function GetNewConfig: Boolean;
begin
  Result := NewConfigVar;
end;

procedure SetNewConfig(const Value: Boolean);
begin
  NewConfigVar := Value;
end;

function GetUseDnsmasq: string;
begin
  Result := UseDnsmasqVar;
end;

procedure SetUseDnsmasq(const Value: string);
begin
  UseDnsmasqVar := Value;
end;

function GetUseZfs: string;
begin
  Result := UseZfsVar;
end;

procedure SetUseZfs(const Value: string);
begin
  UseZfsVar := Value;
end;

function GetVmPath: string;
begin
  Result := VmPathVar;
end;

procedure SetVmPath(const Value: string);
begin
  VmPathVar := Value;
end;

function GetOsreldate: string;
begin
  Result := OsreldateVar;
end;

procedure SetOsreldate(const Value: string);
begin
  OsreldateVar := Value;
end;

function GetCloudVmImagesPath: string;
begin
  Result := CloudVmImagesPathVar;
end;

procedure SetCloudVmImagesPath(const Value: string);
begin
  CloudVmImagesPathVar := Value;
end;

function GetUseSystray: string;
begin
  Result := UseSystrayVar;
end;

procedure SetUseSystray(const Value: string);
begin
  UseSystrayVar := Value;
end;

function GetUseIpv6: string;
begin
  Result := UseIpv6Var;
end;

procedure SetUseIpv6(const Value: string);
begin
  UseIpv6Var := Value;
end;

function GetUsePf: string;
begin
  Result := UsePfVar;
end;

procedure SetUsePf(const Value: string);
begin
  UsePfVar := Value;
end;

function GetLanguage: string;
begin
  Result := LanguageVar;
end;

procedure SetLanguage(const Value: string);
begin
  LanguageVar := Value;
end;

function GetBhyveCmd: string;
begin
  Result := BhyveCmdVar;
end;

procedure SetBhyveCmd(const Value: string);
begin
  BhyveCmdVar := Value;
end;

function GetBhyvectlCmd: string;
begin
  Result := BhyvectlCmdVar;
end;

procedure SetBhyvectlCmd(const Value: string);
begin
  BhyvectlCmdVar := Value;
end;

function GetBhyveloadCmd: string;
begin
  Result := BhyveloadCmdVar;
end;

procedure SetBhyveloadCmd(const Value: string);
begin
  BhyveloadCmdVar := Value;
end;

function GetBridgeInterface: string;
begin
  Result := BridgeInterfaceVar;
end;

procedure SetBridgeInterface(const Value: string);
begin
  BridgeInterfaceVar := Value;
end;

function GetSubnet: string;
begin
  Result := SubnetVar;
end;

procedure SetSubnet(const Value: string);
begin
  SubnetVar := Value;
end;

function GetIpv6Prefix: string;
begin
  Result := Ipv6PrefixVar;
end;

procedure SetIpv6Prefix(const Value: string);
begin
  Ipv6PrefixVar := Value;
end;

function GetExternalInterface: string;
begin
  Result := ExternalInterfaceVar;
end;

procedure SetExternalInterface(const Value: string);
begin
  ExternalInterfaceVar := Value;
end;

function GetExternalIpv4: string;
begin
  Result := ExternalIpv4Var;
end;

procedure SetExternalIpv4(const Value: string);
begin
  ExternalIpv4Var := Value;
end;

function GetExternalIpv6: string;
begin
  Result := ExternalIpv6Var;
end;

procedure SetExternalIpv6(const Value: string);
begin
  ExternalIpv6Var := Value;
end;

function GetVncviewerCmd: string;
begin
  Result := VncviewerCmdVar;
end;

procedure SetVncviewerCmd(const Value: string);
begin
  VncviewerCmdVar := Value;
end;

function GetXfreerdpCmd: string;
begin
  Result := XfreerdpCmdVar;
end;

procedure SetXfreerdpCmd(const Value: string);
begin
  XfreerdpCmdVar := Value;
end;

function GetXfreerdpArgs: string;
begin
  Result := XfreerdpArgsVar;
end;

procedure SetXfreerdpArgs(const Value: string);
begin
  XfreerdpArgsVar := Value;
end;

function GetQemuImgCmd: string;
begin
  Result := QemuImgCmdVar;
end;

procedure SetQemuImgCmd(const value: string);
begin
  QemuImgCmdVar := Value;
end;

function GetSwtpmCmd: string;
begin
  Result := SwtpmCmdVar;
end;

procedure SetSwtpmCmd(const Value: string);
begin
  SwtpmCmdVar := Value;
end;

function GetSwtpmIoctlCmd: string;
begin
  Result := SwtpmIoctlCmdVar;
end;

procedure SetSwtpmIoctlCmd(const Value: string);
begin
  SwtpmIoctlCmdVar := Value;
end;

function GetZfsEnable: string;
begin
  Result := ZfsEnableVar;
end;

procedure SetZfsEnable(const Value: string);
begin
  ZfsEnableVar := Value;
end;

function GetZfsZpool: string;
begin
  Result := ZfsZpoolVar;
end;

procedure SetZfsZpool(const Value: string);
begin
  ZfsZpoolVar := Value;
end;

function GetZfsCreateOptions: string;
begin
  Result := ZfsCreateOptionsVar;
end;

procedure SetZfsCreateOptions(const value: string);
begin
  ZfsCreateOptionsVar := Value;
end;

end.

