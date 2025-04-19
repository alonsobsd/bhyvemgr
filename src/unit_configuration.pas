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

unit unit_configuration;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, IniFiles;

Type
   { ConfigurationClass }

   ConfigurationClass = class
      private
        ConfigFile : TIniFile;
        PathFile : String;

      public
        constructor Create(Path : String);
        destructor Destroy; override;
        function SetOption(Section : String; Key : String; Value : String):Boolean;
        function GetOption(Section : String; Key : String; Value : String = ''):String;
        function DelOption(Section : String; Key : String):Boolean;
        function GetSection(Section : String):TStringList;
        function GeneralConfig():Boolean;
   end;

   { BhyveConfigurationClass }

   BhyveConfigurationClass = class
      private
        ConfigList : TStringList;
        PathFile : String;
      public
        constructor Create(Path : String);
        destructor Destroy; override;
        function SetOption(Key : String; Value : String):Boolean;
        function GetOption(Key : String; Value : String = ''):String;
        function DelOption(Key : String):Boolean;
        function LoadFromStringList(ValueList : TStringList):Boolean;
        function LoadFromFile(Path : String):Boolean;
        procedure SaveConfig();
   end;

implementation

uses
  unit_global, unit_util;

{ ConfigurationClass }

constructor ConfigurationClass.Create(Path : String);
begin
  PathFile:=Path;
  ConfigFile:=TIniFile.Create(PathFile);

  if not FileExists(PathFile) then
    SetNewConfig(True)
  else
    SetNewConfig(False);
end;

destructor ConfigurationClass.Destroy();
begin
  ConfigFile.Free;
  inherited;
end;

function ConfigurationClass.SetOption(Section: String; Key: String;
  Value: String): Boolean;
begin
  ConfigFile.WriteString(Section, Key, Value);

  Result:=True;
end;

function ConfigurationClass.GetOption(Section : String; Key : String; Value : String = ''):String;
begin
  Result:=ConfigFile.ReadString(Section, Key, Value);
end;

function ConfigurationClass.DelOption(Section : String; Key : String):Boolean;
begin
  ConfigFile.DeleteKey(Section, Key);

  Result:=True
end;

function ConfigurationClass.GetSection(Section: String): TStringList;
var
  OptionList : TStringList;
begin
  OptionList:=TStringList.Create;
  try
      ConfigFile.ReadSection(Section, OptionList);
      Result:=OptionList;
  finally
      OptionList.Free;
  end;
end;

function ConfigurationClass.GeneralConfig:Boolean;
var
  ZfsPoolList : TStringList;
begin
  ZfsPoolList:=TStringList.Create;

  if ConfigFile.ReadString('general','use_dnsmasq', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('general','use_dnsmasq', 'yes');

  if ConfigFile.ReadString('general','use_sudo', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('general','use_sudo', 'yes');

  if ConfigFile.ReadString('general','use_systray', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('general','use_systray', 'yes');

  if ConfigFile.ReadString('bhyve-tools','bhyve_cmd', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('bhyve-tools','bhyve_cmd', '/usr/sbin/bhyve');

  if ConfigFile.ReadString('bhyve-tools','bhyvectl_cmd', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('bhyve-tools','bhyvectl_cmd', '/usr/sbin/bhyvectl');

  if ConfigFile.ReadString('bhyve-tools','bhyveload_cmd', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('bhyve-tools','bhyveload_cmd', '/usr/sbin/bhyveload');

  if ConfigFile.ReadString('extra-tools','chown_cmd', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('extra-tools','chown_cmd', '/usr/sbin/chown');

  if ConfigFile.ReadString('extra-tools','chmod_cmd', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('extra-tools','chmod_cmd', '/bin/chmod');

  if ConfigFile.ReadString('extra-tools','ifconfig_cmd', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('extra-tools','ifconfig_cmd', '/sbin/ifconfig');

  if ConfigFile.ReadString('extra-tools','install_cmd', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('extra-tools','install_cmd', '/usr/bin/install');

  if ConfigFile.ReadString('extra-tools','kill_cmd', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('extra-tools','kill_cmd', '/bin/kill');

  if ConfigFile.ReadString('extra-tools','kldload_cmd', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('extra-tools','kldload_cmd', '/sbin/kldload');

  if ConfigFile.ReadString('extra-tools','kldstat_cmd', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('extra-tools','kldstat_cmd', '/sbin/kldstat');

  if ConfigFile.ReadString('extra-tools','pciconf_cmd', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('extra-tools','pciconf_cmd', '/usr/sbin/pciconf');

  if ConfigFile.ReadString('extra-tools','pgrep_cmd', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('extra-tools','pgrep_cmd', '/usr/bin/pgrep');

  if ConfigFile.ReadString('extra-tools','rm_cmd', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('extra-tools','rm_cmd', '/bin/rm');

  if ConfigFile.ReadString('extra-tools','service_cmd', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('extra-tools','service_cmd', '/usr/sbin/service');

  if ConfigFile.ReadString('extra-tools','swtpm_cmd', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('extra-tools','swtpm_cmd', '/usr/local/bin/swtpm');

  if ConfigFile.ReadString('extra-tools','swtpm_ioctl_cmd', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('extra-tools','swtpm_ioctl_cmd', '/usr/local/bin/swtpm_ioctl');

  if ConfigFile.ReadString('extra-tools','sysctl_cmd', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('extra-tools','sysctl_cmd', '/sbin/sysctl');

  if ConfigFile.ReadString('extra-tools','truncate_cmd', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('extra-tools','truncate_cmd', '/usr/bin/truncate');

  if ConfigFile.ReadString('extra-tools','zfs_cmd', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('extra-tools','zfs_cmd', '/sbin/zfs');

  if ConfigFile.ReadString('extra-tools','zpool_cmd', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('extra-tools','zpool_cmd', '/sbin/zpool');

  if ConfigFile.ReadString('network','bridge_interface', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('network','bridge_interface', 'bhyve0');

  if ConfigFile.ReadString('network','subnet', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('network','subnet', '10.0.0.0/24');

  if ConfigFile.ReadString('remote-tools','vncviewer_cmd', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('remote-tools','vncviewer_cmd', '/usr/local/bin/remote-viewer');

  if ConfigFile.ReadString('remote-tools','xfreerdp_cmd', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('remote-tools','xfreerdp_cmd', '/usr/local/bin/xfreerdp3');

  if ConfigFile.ReadString('remote-tools','xfreerdp_args', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('remote-tools','xfreerdp_args', '/cert:tofu /sound:sys:oss /network:lan /bpp:32 /gfx:rfx:on /log-level:off');

  if ConfigFile.ReadString('user-tools','doas_cmd', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('user-tools','doas_cmd', '/usr/local/bin/doas');

  if ConfigFile.ReadString('user-tools','sudo_cmd', EmptyStr) = EmptyStr then
    ConfigFile.WriteString('user-tools','sudo_cmd', '/usr/local/bin/sudo');

  SetZpoolCmd('/sbin/zpool');

  ZfsPoolList.Text:=GetZpoolList();

  if ZfsPoolList.Count > 0 then
   begin
     if ConfigFile.ReadString('zfs','zfs_zpool', EmptyStr) = EmptyStr then
       ConfigFile.WriteString('zfs','zfs_zpool', ZfsPoolList[0]);

     if ConfigFile.ReadString('zfs','zfs_create_options', EmptyStr) = EmptyStr then
       ConfigFile.WriteString('zfs','zfs_create_options','-o compress=lz4');

     if ConfigFile.ReadString('general','use_zfs', EmptyStr) = EmptyStr then
       ConfigFile.WriteString('general','use_zfs', 'yes');

     if ConfigFile.ReadString('general','vm_path', EmptyStr) = EmptyStr then
       ConfigFile.WriteString('general','vm_path', '/'+ZfsPoolList[0]+'/bhyvemgr');
   end
   else
   begin
     if ConfigFile.ReadString('zfs','zfs_zpool', EmptyStr) = EmptyStr then
       ConfigFile.WriteString('zfs','zfs_zpool', 'zroot');

     if ConfigFile.ReadString('zfs','zfs_create_options', EmptyStr) = EmptyStr then
       ConfigFile.WriteString('zfs','zfs_create_options','-o compress=lz4');

     if ConfigFile.ReadString('general','use_zfs', EmptyStr) = EmptyStr then
       ConfigFile.WriteString('general','use_zfs' , 'no');

     if ConfigFile.ReadString('general','vm_path', EmptyStr) = EmptyStr then
       ConfigFile.WriteString('general','vm_path', '/usr/local/bhyvemgr');
   end;

   ZfsPoolList.Free;

   Result:=True;
end;

{ BhyveConfigurationClass }

constructor BhyveConfigurationClass.Create(Path: String);
begin
  PathFile:=Path;
  ConfigList:=TStringList.Create();

  if not FileExists(PathFile) then
    FileCreate(PathFile);

end;

destructor BhyveConfigurationClass.Destroy;
begin
  ConfigList.Free;
  inherited Destroy;
end;

function BhyveConfigurationClass.SetOption(Key: String; Value: String): Boolean;
begin
  ConfigList.Values[Key]:=Value;

  Result:=True;
end;

function BhyveConfigurationClass.GetOption(Key: String; Value: String): String;
begin
  Result:=ConfigList.Values[Key];
end;

function BhyveConfigurationClass.DelOption(Key: String): Boolean;
begin
  Result:=True;

  if (ConfigList.IndexOfName(Key) = -1) then
    Result:=False
  else
    ConfigList.Delete(ConfigList.IndexOfName(Key));
end;

function BhyveConfigurationClass.LoadFromStringList(ValueList : TStringList): Boolean;
begin
  ConfigList.Text:=ValueList.Text;

  Result:=True;
end;

function BhyveConfigurationClass.LoadFromFile(Path: String): Boolean;
begin
  ConfigList.LoadFromFile(PathFile);

  Result:=True;
end;

procedure BhyveConfigurationClass.SaveConfig();
begin
  ConfigList.Sorted:=True;
  ConfigList.SaveToFile(PathFile);
end;

end.



