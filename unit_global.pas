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
function GetUseSudo:string;
procedure SetUseSudo(const Value:string);
function GetUseZfs:string;
procedure SetUseZfs(const Value:string);
function GetVmPath:string;
procedure SetVmPath(const Value:string);
{ Bhyve section }
function GetBhyveCmd:string;
procedure SetBhyveCmd(const Value:string);
function GetBhyvectlCmd:string;
procedure SetBhyvectlCmd(const Value:string);
function GetBhyveloadCmd:string;
procedure SetBhyveloadCmd(const Value:string);
{ User tools section }
function GetDoasCmd:string;
procedure SetDoasCmd(const Value:string);
function GetSudoCmd:string;
procedure SetSudoCmd(const Value:string);
{ Network section }
function GetBridgeInterface:string;
procedure SetBridgeInterface(const Value:string);
function GetSubnet:string;
procedure SetSubnet(const Value:string);
{ Remote tools section }
function GetVncviewerCmd:string;
procedure SetVncviewerCmd(const Value:string);
function GetXfreerdpCmd:string;
procedure SetXfreerdpCmd(const Value:string);
function GetXfreerdpArgs:string;
procedure SetXfreerdpArgs(const Value:string);
{ Other tools section }
function GetChownCmd:string;
procedure SetChownCmd(const Value:string);
function GetChmodCmd:string;
procedure SetChmodCmd(const Value:string);
function GetIfconfigCmd:string;
procedure SetIfconfigCmd(const Value:string);
function GetInstallCmd:string;
procedure SetInstallCmd(const Value:string);
function GetKillCmd:string;
procedure SetKillCmd(const Value:string);
function GetKldloadCmd:string;
procedure SetKldloadCmd(const Value:string);
function GetKldstatCmd:string;
procedure SetKldstatCmd(const Value:string);
function GetPciconfCmd:string;
procedure SetPciconfCmd(const value:string);
function GetPgrepCmd:string;
procedure SetPgrepCmd(const value:string);
function GetRmCmd:string;
procedure SetRmCmd(const Value:string);
function GetServiceCmd:string;
procedure SetServiceCmd(const Value:string);
function GetSysctlCmd:string;
procedure SetSysctlCmd(const Value:string);
function GetTruncateCmd:string;
procedure SetTruncateCmd(const Value:string);
function GetZfsCmd:string;
procedure SetZfsCmd(const Value:string);
function GetZpoolCmd:string;
procedure SetZpoolCmd(const Value:string);
{ Zfs section }
function GetZfsZpool:string;
procedure SetZfsZpool(const Value:string);
function GetZfsCreateOptions:string;
procedure SetZfsCreateOptions(const value:string);

{ General section }
property Osreldate:string read GetOsreldate write SetOsreldate;
property NewConfig:boolean read GetNewConfig write SetNewConfig;
property UseDnsmasq:string read GetUseDnsmasq write SetUseDnsmasq;
property UseSudo:string read GetUseSudo write SetUseSudo;
property UseZfs:string read GetUseZfs write SetUseZfs;
property VmPath:string read GetVmPath write SetVmPath;
{ Bhyve section }
property BhyveCmd:string read GetBhyveCmd write SetBhyveCmd;
property BhyvectlCmd:string read GetBhyvectlCmd write SetBhyvectlCmd;
property BhyveLoadCmd:string read GetBhyveloadCmd write SetBhyveloadCmd;
{ Network section }
property BridgeInterface:string read GetBridgeInterface write SetBridgeInterface;
property Subnet:string read GetSubnet write SetSubnet;
{ User tools section }
property DoasCmd:string read GetDoasCmd write SetDoasCmd;
property SudoCmd:string read GetSudoCmd write SetSudoCmd;
{ Remote tools section }
property VncviewerCmd:string read GetVncviewerCmd write SetVncviewerCmd;
property XfreerdpCmd:string read GetXfreerdpCmd write SetXfreerdpCmd;
property XfreerdpArgs:string read GetXfreerdpArgs write SetXfreerdpArgs;
{ Other tools section }
property ChownCmd:string read GetChownCmd write SetChownCmd;
property ChmodCmd:string read GetChmodCmd write SetChmodCmd;
property IfconfigCmd:string read GetIfconfigCmd write SetIfconfigCmd;
property InstallCmd:string read GetInstallCmd write SetInstallCmd;
property KillCmd:string read GetKillCmd write SetKillCmd;
property KldloadCmd:string read GetKldloadCmd write SetKldloadCmd;
property KldstatCmd:string read GetKldstatCmd write SetKldstatCmd;
property PciconfCmd:string read GetPciconfCmd write SetPciconfCmd;
property PgrepCmd:string read GetPgrepCmd write SetPgrepCmd;
property RmCmd:string read GetRmCmd write SetRmCmd;
property ServiceCmd:string read GetServiceCmd write SetServiceCmd;
property SysctlCmd:string read GetSysctlCmd write SetSysctlCmd;
property TruncateCmd:string read GetTruncateCmd write SetTruncateCmd;
property ZfsCmd:string read GetZfsCmd write SetZfsCmd;
property ZpoolCmd:string read GetZpoolCmd write SetZpoolCmd;
{ Zfs section }
property ZfsZpool:string read GetZfsZpool write SetZfsZpool;
property ZfsCreateOptions:string read GetZfsCreateOptions write SetZfsCreateOptions;

const
  BhyveOui = '58:9c:fc:0';
  {$IFDEF DEBUG}
  DatadirPath = '';
  {$ELSE}
  DatadirPath = '/usr/local/share/bhyvemgr/';
  {$ENDIF}

  BootRomUefiPath = '/usr/local/share/uefi-firmware';
  BootRomUbootPath = '/usr/local/share/u-boot/u-boot-bhyve-arm64';
  KeyBoardLayoutPath = '/usr/share/bhyve/kbdlayout';
  DnsmasqDirectory = '/usr/local/etc/dnsmasq.d/bhyvemgr';
  FormBhyveManagerTitle = 'Bhyve Manager - FreeBSD';
  FormBhyveManagerAboutTitle = 'Bhyve Manager - About';
  FormBhyveManagerAudioDeviceTitle = 'Audio device';
  FormBhyveManagerConsoleDeviceTitle = 'Console device';
  FormBhyveManagerDisplayDeviceTitle = 'Display device';
  FormBhyveManagerHostbridgeDeviceTitle = 'Hostbridge device';
  FormBhyveManagerInputDeviceTitle = 'Input device';
  FormBhyveManagerLPCDeviceTitle = 'LPC Device';
  FormBhyveManagerNetworkDeviceTitle = 'Network Device';
  FormBhyveManagerPassthruDeviceTitle = 'Passthru Device';
  FormBhyveManagerSettingsTitle = 'Bhyve Manager - Settings';
  FormBhyveManagerStorageDeviceTitle = 'Storage Device';
  FormBhyveManagerShareFolderDeviceTitle = 'Share folder device';
  FormBhyveManagerCreateVmTitle = 'Bhyve Manager - Create virtual machine';
  FormBhyveManagerEditVmInfoTitle = 'Bhyve Manager - Edit virtual machine info';

var
  OsreldateVar: String;
  NewConfigVar: Boolean;
  UseDnsmasqVar: String;
  UseSudoVar: String;
  UseZfsVar: String;
  VmPathVar: String;
  BhyveCmdVar: String;
  BhyvectlCmdVar: String;
  BhyveloadCmdVar: String;
  DoasCmdVar: String;
  SudoCmdVar: String;
  VncviewerCmdVar: String;
  XfreerdpCmdVar: String;
  XfreerdpArgsVar: String;
  BridgeInterfaceVar: String;
  SubnetVar: String;
  ChownCmdVar: String;
  ChmodCmdVar: String;
  IfconfigCmdVar: String;
  InstallCmdVar: String;
  KillCmdVar: String;
  KldloadCmdVar: String;
  KldstatCmdVar: String;
  PciconfCmdVar: String;
  PgrepCmdVar: String;
  RmCmdVar: String;
  ServiceCmdVar: String;
  SysctlCmdVar: String;
  TruncateCmdVar: String;
  ZfsCmdVar: String;
  ZpoolCmdVar: String;
  ZfsEnableVar: String;
  ZfsZpoolVar: String;
  ZfsCreateOptionsVar: String;

implementation

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

function GetUseSudo: string;
begin
  Result := UseSudoVar;
end;

procedure SetUseSudo(const Value: string);
begin
  UseSudoVar := Value;
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
  if Value = EmptyStr then
    VmPathVar := GetUserDir + 'bhyvemgr'
  else
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

function GetDoasCmd: string;
begin
  Result := DoasCmdVar;
end;

procedure SetDoasCmd(const Value: string);
begin
  DoasCmdVar := Value;
end;

function GetSudoCmd: string;
begin
  Result := SudoCmdVar;
end;

procedure SetSudoCmd(const Value: string);
begin
  SudoCmdVar := Value;
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

function GetChownCmd: string;
begin
  Result := ChownCmdVar;
end;

procedure SetChownCmd(const Value: string);
begin
  ChownCmdVar := Value;
end;

function GetChmodCmd: string;
begin
  Result := ChmodCmdVar;
end;

procedure SetChmodCmd(const Value: string);
begin
  ChmodCmdVar := Value;
end;

function GetIfconfigCmd: string;
begin
  Result := IfconfigCmdVar;
end;

procedure SetIfconfigCmd(const Value: string);
begin
  IfconfigCmdVar := Value;
end;

function GetInstallCmd: string;
begin
  Result := InstallCmdVar;
end;

procedure SetInstallCmd(const Value: string);
begin
  InstallCmdVar := Value;
end;

function GetKillCmd: string;
begin
  Result := KillCmdVar;
end;

procedure SetKillCmd(const Value: string);
begin
  KillCmdVar := Value;
end;

function GetKldloadCmd: string;
begin
  Result := KldloadCmdVar;
end;

procedure SetKldloadCmd(const Value: string);
begin
  KldloadCmdVar := Value;
end;

function GetKldstatCmd: string;
begin
  Result := KldstatCmdVar;
end;

procedure SetKldstatCmd(const Value: string);
begin
  KldstatCmdVar := Value;
end;

function GetPciconfCmd: string;
begin
  Result := PciconfCmdVar;
end;

procedure SetPciconfCmd(const value: string);
begin
  PciconfCmdVar := Value;
end;

function GetPgrepCmd: string;
begin
  Result := PgrepCmdVar;
end;

procedure SetPgrepCmd(const value: string);
begin
  PgrepCmdVar := Value;
end;

function GetRmCmd: string;
begin
  Result := RmCmdVar;
end;

procedure SetRmCmd(const Value: string);
begin
  RmCmdVar := Value;
end;

function GetServiceCmd: string;
begin
  Result := ServiceCmdVar;
end;

procedure SetServiceCmd(const Value: string);
begin
  ServiceCmdVar := Value;
end;

function GetSysctlCmd: string;
begin
  Result := SysctlCmdVar;
end;

procedure SetSysctlCmd(const Value: string);
begin
  SysctlCmdVar := Value;
end;

function GetTruncateCmd: string;
begin
  Result := TruncateCmdVar;
end;

procedure SetTruncateCmd(const Value: string);
begin
  TruncateCmdVar := Value;
end;

function GetZfsCmd: string;
begin
  Result := ZfsCmdVar;
end;

procedure SetZfsCmd(const Value: string);
begin
  ZfsCmdVar := Value;
end;

function GetZpoolCmd: string;
begin
  Result := ZpoolCmdVar;
end;

procedure SetZpoolCmd(const Value: string);
begin
  ZpoolCmdVar := Value;
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

