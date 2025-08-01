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

unit unit_util;

{$mode ObjFPC}
{$modeswitch arrayoperators+}
{$H+}

interface

uses
  Classes, SysUtils, Dialogs, FileUtil, Math, md5, process, RegExpr, Users, BaseUnix, StrUtils;

function AttachDeviceToBridge(BridgeName: String; DeviceName: String; VmName : String):Boolean;
function AddDnsmasqDhcpHostEntry(VmName: String; IpAddress: String; MacAddreess : String):Boolean;
function AddDnsmasqHostRecordEntry(VmName: String; Ip6Address: String; MacAddreess : String):Boolean;
function CheckBhyveSupport():Boolean;
function CheckCidrRange(Subnet: String):Boolean;
function CheckKernelModule(Module: String):Boolean;
function CheckIpv6Address(Address: String):Boolean;
function CheckMacAddress(Mac: String):Boolean;
function CheckNetworkDeviceName(Name: String):Boolean;
function CheckSysctl(Name: String):String;
function CheckVmName(Name: String):Boolean;
function CheckVmRunning(Name: String):Integer;
function CheckTpmSocketRunning(Name: String):Integer;
function CheckZfsDataset(Dataset: String): Boolean;
function CheckZfsSupport():Boolean;
function ChmodDir(Path: String):Boolean;
function ChownDir(Path: String):Boolean;
function CreateDirectory(DirectoryName: String; UserName : String; DirMode : String = '700'):Boolean;
function CreateFile(FileName: String; UserName : String; FileMode : String = '600'):Boolean;
function CreateNetworkDevice(DeviceName: String; VmName : String; Mtu : String = '1500'):Boolean;
function CreateTpmSocket(Path: String):Boolean;
function DestroyNetworkInterface(IfName: String):Boolean;
function DestroyVirtualMachine(VmName: String):Boolean;
function ExtractCidr(Network: String): String;
function ExtractIpv6Prefix(prefix : String):String;
function ExtractNetMask(Cidr: Integer): String;
function ExtractNumberValue(TextLine: String; Suffix: String): String;
function ExtractPortValue(TextLine: String): String;
function ExtractVarName(TextLine: String): String;
function ExtractVarValue(TextLine: String): String;
function GenerateIpv6Preffix():String;
function GenerateIpv6Suffix(mac : String):String;
function GenerateMacAddress(): String;
function GenerateUuid(): String;
function GetCurrentUserName(): String;
function GetFileSize(FilePath : String; SizeUnit : String = 'B'): Int64;
function GetEventDeviceList(Path : String; Pattern : String):String;
function GetNewConsoleName(VmName : String): String;
function GetNewIpAddress(Subnet : String): String;
function GetNewIp6Address(prefix : String; mac : String): String;
function GetNewPciSlotNumber(VmName : String): String;
function GetNewPciSlotNumber(StringList : TStringList): String;
function GetNewPciSlotNumber(StringList : TStringList; StartSlot : Integer): String;
function GetNewAhciPortNumber(BusNumber : String; VmName : String): String;
function GetNewComPortNumber(): String;
function GetNewNetworkName(BackendType : String): String;
function GetNewNetworkName(CurrentVmName : String; CurrentVmConfig : TStringList; BackendType : String; StartValue : Integer): String;
function GetNewStorageName(DiskPath : String; IsZvol: Boolean): String;
function GetNewVmName(VmName : String): Boolean;
function GetNewVncPortNumber(): String;
function GetPciDeviceDescripcion(Device : String):String;
function GetPciDeviceList(Device : String):String;
function GetPidValue(Pattern : String): Integer;
function GetStorageSize(StoragePath : String): String;
function GetStorageType(StoragePath : String): String;
function GetZpoolList():String;
function KillPid(Pid : Integer; Signal : String = '-TERM'): Boolean;
function LoadKernelModule(Module : String):Boolean;
function RdpConnect(VmName : String; Username : String; Password : String; Width : String; Height : String):Boolean;
function RemoveDirectory(Directory: String; Recursive : Boolean):Boolean;
function RemoveFile(Path: String):Boolean;
function RemoveDnsmasqEntry(VmName: String):Boolean;
function RestartService(Service : String):Boolean;
function StopVirtualMachine(Pid : Integer):Boolean;
function TruncateImage(ImagePath : String; ImageSize : String):Boolean;
function VncConnect(VmHost : String; VmName : String):Boolean;
function ZfsCreateDataset(ZfsPath : String):Boolean;
function ZfsCreateZvol(ZfsPath : String; ZvolSize : String; ZvolSparse : Boolean = False):Boolean;
function ZfsGetPropertyValue(ZfsPath : String; ZfsProperty : String; ZfsField : String):String;
function ZfsDestroy(ZfsPath : String; Recursive : Boolean = True; Force : Boolean = False):Boolean;

implementation

uses
  unit_configuration, unit_global, unit_thread, LazLogger;

var
  MyAppThread: AppThread;

{ Private IPv4 functions }
function ExtractCidr(Network: String): String;
var
  TmpArray : TStringArray;
begin
  TmpArray:= Network.Split('/');

  Result:=TmpArray[1];
end;

function ExtractIP(Network: String): String;
var
  TmpArray : TStringArray;
begin
  TmpArray:= Network.Split('/');

  Result:=TmpArray[0];
end;

function MaxHosts(Network: String): Integer;
begin
  Result:= 2**(32 - (ExtractCidr(Network).ToInteger))-2;
end;

function IpToDecimal(Network: String; Cidr: Integer): String;
var
  i,j : Integer;
  TmpArray : TStringArray;
  IpArray : TCharArray;
  Bin1, Bin2, Bin3, Bin4 : String;
  DecimalNumber : String;
begin
  j:=0;
  DecimalNumber:=EmptyStr;

  TmpArray:=ExtractIP(Network).Split('.');

  Bin1:=IntToBin(TmpArray[0].ToInteger,8);
  Bin2:=IntToBin(TmpArray[1].ToInteger,8);
  Bin3:=IntToBin(TmpArray[2].ToInteger,8);
  Bin4:=IntToBin(TmpArray[3].ToInteger,8);

  IpArray:=(Bin1+Bin2+Bin3+Bin4).ToCharArray;

  for i:=1 to Length(IpArray) do
  begin
    if i <= Cidr then
      DecimalNumber:=DecimalNumber+IpArray[j]
    else
      DecimalNumber:=DecimalNumber+'0';
    Inc(j);
  end;

  Result:=DecimalNumber;
end;

function BroadcastToDecimal(Subnet: String): String;
var
  i,j : Integer;
  IpArray : TCharArray;
  Cidr : Integer;
  DecimalBroadcast : String;
begin
  j:=0;
  DecimalBroadcast:=EmptyStr;
  IpArray :=IpToDecimal(ExtractIP(Subnet), ExtractCidr(Subnet).ToInteger).ToCharArray;
  Cidr:= ExtractCidr(Subnet).ToInteger;

  for i:=1 to Length(IpArray) do
  begin
    if i <= Cidr then
      DecimalBroadcast:=DecimalBroadcast+IpArray[j]
    else
      DecimalBroadcast:=DecimalBroadcast+'1';
    Inc(j);
  end;

  Result:=DecimalBroadcast;
end;

function BinToDec(Input: String): Integer;
var
  i,j : Integer;
  IpArray : TCharArray;
  DecimalNumber : Integer;
begin
  DecimalNumber:=0;
  if Input.ToInteger > 0 then
  begin
    j:=7;
    IpArray:=Input.ToCharArray;

    for i:=0 to 7 do
    begin
      if (IpArray[i] = '1') then
      begin
        DecimalNumber:=DecimalNumber+(2**j);
      end;
      Dec(j);
    end;
  end;
  Result:=DecimalNumber;
end;

function DecimalToIP(Input: String): String;
var
  i,j : Integer;
  IpArray : TCharArray;
  IpAddress : String;
  DotChar : String;
  DecimalNumber : String;
begin;
  j:=1;
  IpArray:=Input.ToCharArray;
  DecimalNumber:=EmptyStr;
  IpAddress:=EmptyStr;
  DotChar:='.';

  for i:=0 to Length(IpArray)-1 do
  begin
    DecimalNumber:=DecimalNumber+IpArray[i];

    if j < 8 then
    begin
      Inc(j);
    end
    else
    begin
      if i > 24 then
        DotChar:=EmptyStr;

      IpAddress:=IpAddress+IntToStr(BinToDec(DecimalNumber))+DotChar;
      DecimalNumber:=EmptyStr;
      j:=1;
    end;
  end;

  Result:=IpAddress;
end;

function ExtractNetMask(Cidr: Integer): String;
var
  i : Integer;
  Netmask : String;
begin
  Netmask:=EmptyStr;

  for i:=1 to 32 do
  begin
    if i <= Cidr then
      Netmask:=Netmask+'1'
    else
      Netmask:=Netmask+'0';
  end;

  Result:=DecimalToIP(Netmask);
end;

function FirstIpAddress(Network: String): String;
var
  NetworkArray : TStringArray;
  Oct : Integer;
begin
  NetworkArray:=Network.Split('.');

  Oct:=NetworkArray[3].ToInteger+1;

  Result:=NetworkArray[0]+'.'+NetworkArray[1]+'.'+NetworkArray[2]+'.'+Oct.ToString;
end;

function LastIpAddress(Broadcast: String): String;
var
  BroadcastArray : TStringArray;
  Oct : Integer;
begin
  BroadcastArray:=Broadcast.Split('.');

  Oct:=BroadcastArray[3].ToInteger-1;

  Result:=BroadcastArray[0]+'.'+BroadcastArray[1]+'.'+BroadcastArray[2]+'.'+Oct.ToString;
end;

function CheckValidIpAddress(IpAddress: String; Subnet: String): Boolean;
var
  TmpArray : TStringArray;
  FirstIp: TStringArray;
  LasttIp: TStringArray;
  Oct1, Oct2, Oct3, Oct4 : Boolean;
begin
  Result:=False;

  oct1:=False;
  oct2:=False;
  oct3:=False;
  oct4:=False;

  TmpArray:=IpAddress.Split('.');

  FirstIp:=FirstIpAddress(DecimalToIP(IpToDecimal(ExtractIP(Subnet), ExtractCidr(Subnet).ToInteger))).Split('.');
  LasttIp:=LastIpAddress(DecimalToIP(BroadcastToDecimal(Subnet))).Split('.');

  if((TmpArray[0].ToInteger >= FirstIp[0].ToInteger) AND (TmpArray[0].ToInteger <= LasttIp[0].ToInteger)) then
    Oct1:=True;
  if((TmpArray[1].ToInteger >= FirstIp[1].ToInteger) AND (TmpArray[1].ToInteger <= LasttIp[1].ToInteger)) then
    Oct2:=True;
  if((TmpArray[2].ToInteger >= FirstIp[2].ToInteger) AND (TmpArray[2].ToInteger <= LasttIp[2].ToInteger)) then
    Oct3:=True;
  if((TmpArray[3].ToInteger >= FirstIp[3].ToInteger) AND (TmpArray[3].ToInteger <= LasttIp[3].ToInteger)) then
    Oct4:=True;

  if Oct1 AND Oct2 AND Oct3 AND Oct4 then
    Result:=True;
end;

function GetNewIpAddress(Subnet : String): String;
var
  i : Integer;
  ConfigurationFile : ConfigurationClass;
  Directories : TStringList;
  IpArray : TStringArray;
  IpAddress : String;
  IpAddressValueList : TStringList;
  Oct1 : Integer;
  Oct2 : Integer;
  Oct3 : Integer;
  Oct4 : Integer;
begin
  Result:=EmptyStr;
  IpAddress:=EmptyStr;

  IpAddressValueList:=TStringList.Create;

  Directories:=FindAllDirectories(VmPath, False);
  Directories.Sorted:=True;

  for i:=0 to Directories.Count-1 do
  begin
    if FileExists(Directories[i]+'/bhyve_config.conf') and FileExists(Directories[i]+'/'+ExtractFileName(Directories[i])+'.conf') then
      begin
        ConfigurationFile:=ConfigurationClass.Create(Directories[i]+'/'+ExtractFileName(Directories[i])+'.conf');

        IpAddress:=ConfigurationFile.GetOption('general', 'ipaddress', '');

        if (IpAddress <> EmptyStr) and (CheckValidIpAddress(IpAddress, GetSubnet)) then
          IpAddressValueList.Add(IpAddress);

        ConfigurationFile.Free;
      end;
  end;

  IpArray:=FirstIpAddress(DecimalToIP(IpToDecimal(ExtractIP(Subnet), ExtractCidr(Subnet).ToInteger))).Split('.');

  Oct1:=IpArray[0].ToInteger;
  Oct2:=IpArray[1].ToInteger;
  Oct3:=IpArray[2].ToInteger;
  Oct4:=IpArray[3].ToInteger+1;

  for i:=2 to MaxHosts(Subnet) do
  begin
    if Oct4 <= 255 then
    begin
  	IpAddress:=Oct1.ToString+'.'+Oct2.ToString+'.'+Oct3.ToString+'.'+Oct4.ToString;
  	Inc(Oct4);
    end
    else if Oct3 <= 255 then
    begin
  	Oct4:=1;
  	Inc(Oct3);
  	IpAddress:=Oct1.ToString+'.'+Oct2.ToString+'.'+Oct3.ToString+'.'+Oct4.ToString;
    end
    else if Oct2 <= 255 then
    begin
  	Oct3:=1;
  	Inc(Oct2);
  	IpAddress:=Oct1.ToString+'.'+Oct2.ToString+'.'+Oct3.ToString+'.'+Oct4.ToString;
    end
    else if Oct1 <= 255 then
    begin
  	Oct2:=1;
  	Inc(Oct1);
  	IpAddress:=Oct1.ToString+'.'+Oct2.ToString+'.'+Oct3.ToString+'.'+Oct4.ToString;
    end;

    if (IpAddressValueList.IndexOf(IpAddress) = -1) then
    begin
      Result:=IpAddress;
      Break;
    end;
  end;
  IpAddressValueList.Free;
  Directories.Free;
end;

{ Private IPv6 functions }
function ExtractIpv6Prefix(prefix: String): String;
var
  tmpPrefix : TStringArray;
  finalPrefix : TStringArray;
begin
  tmpPrefix:=prefix.Split(':');

  finalPrefix:=[tmpPrefix[0],tmpPrefix[1],tmpPrefix[2],tmpPrefix[3]];

  if finalPrefix[0] = EmptyStr then finalPrefix[0] := '0';
  if finalPrefix[1] = EmptyStr then finalPrefix[1] := '0';
  if finalPrefix[2] = EmptyStr then finalPrefix[2] := '0';
  if finalPrefix[3] = EmptyStr then finalPrefix[3] := '0';

  Result:= String.Join(':', finalPrefix);
end;

function GenerateIpv6Preffix(): String;
var
  tmpPreffix : String;
begin
  tmpPreffix :='fd'+LeftStr(MD5Print(MD5String(RandomRange(1,255).ToString+':'+DateTimeToStr(Now))),10);

  Result:=Copy(tmpPreffix, 1, 4) +':'+ Copy(tmpPreffix, 5, 4)+':'+ Copy(tmpPreffix, 9, 4)+':0001::';
end;

function GenerateIpv6Suffix(mac: String): String;
var
  tmpMac : TStringArray;
  finalMac : TStringArray;
  suffix : String;
begin
  tmpMac:= mac.Split(':');
  finalMac:=[tmpMac[0],tmpMac[1],tmpMac[2],'ff','fe',tmpMac[3],tmpMac[4], tmpMac[5]];

  finalMac[0]:=LowerCase(IntToHex((StrToInt('$'+finalMac[0]) xor $02), 2));

  suffix:= String.Join(':', [finalMac[0]+finalMac[1], finalMac[2]+finalMac[3], finalMac[4]+finalMac[5], finalMac[6]+finalMac[7]]);

  Result:= suffix;
end;

function GetNewIp6Address(prefix : String; mac : String): String;
begin
  Result:= ExtractIpv6Prefix(prefix)+':'+GenerateIpv6Suffix(mac);
end;

function GetPatternValueFromStringList(Pattern: String;  StartValue : Integer; StringList: TStringList
  ): String;
var
  TmpList : TStringList;
  PatternValueList : TStringList;
  RegexObj: TRegExpr;
  PatternValue : String;
  flag : Boolean;
  c,i:Integer;
begin
  TmpList:=TStringList.Create;
  PatternValueList:=TStringList.Create;
  RegexObj := TRegExpr.Create;
  RegexObj.Expression := Pattern;
  PatternValue:=EmptyStr;

  for i:=StringList.Count-1 downto 0 do
  begin
      if (StringList[i].Contains('#'))  then
      begin
        StringList.Delete(i);
      end;
  end;

  TmpList.Text:=StringList.Text;

  if RegexObj.Exec(TmpList.Text) then
  begin
    repeat
      PatternValueList.Add(RegexObj.Match[1]);
    until not RegexObj.ExecNext;
  end;

  PatternValueList.Sorted:=True;

  flag:=True;
  c:=StartValue;

  while(flag) do
  begin
    if (PatternValueList.IndexOf(c.ToString) = -1) then
    begin
      PatternValue:=c.ToString;
      flag:=False;
    end;

    Inc(c);
  end;

  RegexObj.Free;
  TmpList.Free;
  PatternValueList.Free;

  Result:=PatternValue;
end;

function GetPatternValueFromAllConfigFiles(Pattern: String; StartValue : Integer): String;
var
  Directories : TStringList;
  VirtualMachineConfigFile : TStringList;
  PatternValueList : TStringList;
  RegexObj: TRegExpr;
  PatternValue : String;
  flag : Boolean;
  c:Integer;
  i,j : Integer;
begin
  VirtualMachineConfigFile:=TStringList.Create;
  PatternValueList:=TStringList.Create;
  RegexObj := TRegExpr.Create;
  RegexObj.Expression := Pattern;
  PatternValue:=EmptyStr;

  Directories:=FindAllDirectories(VmPath, False);
  Directories.Sorted:=True;

  for i:=0 to Directories.Count-1 do
  begin
    if FileExists(Directories[i]+'/bhyve_config.conf') and FileExists(Directories[i]+'/'+ExtractFileName(Directories[i])+'.conf') then
      begin
        VirtualMachineConfigFile.LoadFromFile(Directories[i]+'/bhyve_config.conf');

        for j:=VirtualMachineConfigFile.Count-1 downto 0 do
        begin
            if (VirtualMachineConfigFile[j].Contains('#'))  then
            begin
              VirtualMachineConfigFile.Delete(j);
            end;
        end;

        if RegexObj.Exec(VirtualMachineConfigFile.Text) then
        begin
            repeat
              PatternValueList.Add(RegexObj.Match[1]);
            until not RegexObj.ExecNext;
        end;
      end;
  end;

  PatternValueList.Sorted:=True;

  flag:=True;
  c:=StartValue;

  while(flag) do
  begin
    if (PatternValueList.IndexOf(c.ToString) = -1) then
    begin
      PatternValue:=c.ToString;
      flag:=False;
    end;

    Inc(c);
  end;

  RegexObj.Free;
  VirtualMachineConfigFile.Free;
  PatternValueList.Free;
  Directories.Free;

  Result:=PatternValue;
end;

function GetPatternValueFromAllConfigFiles(Pattern: String;
  CurrentVmName: String; CurrentVmConfig: TStringList; StartValue: Integer
  ): String;
var
  Directories : TStringList;
  VirtualMachineConfigFile : TStringList;
  PatternValueList : TStringList;
  RegexObj: TRegExpr;
  PatternValue : String;
  flag : Boolean;
  c:Integer;
  i,j : Integer;
begin
  VirtualMachineConfigFile:=TStringList.Create;
  PatternValueList:=TStringList.Create;
  RegexObj := TRegExpr.Create;
  RegexObj.Expression := Pattern;
  PatternValue:=EmptyStr;

  Directories:=FindAllDirectories(VmPath, False);
  Directories.Sorted:=True;

  for i:=0 to Directories.Count-1 do
  begin
    if FileExists(Directories[i]+'/bhyve_config.conf') and FileExists(Directories[i]+'/'+ExtractFileName(Directories[i])+'.conf') and (CurrentVmName <> Directories[i]) then
      begin
        VirtualMachineConfigFile.LoadFromFile(Directories[i]+'/bhyve_config.conf');

        for j:=VirtualMachineConfigFile.Count-1 downto 0 do
        begin
            if (VirtualMachineConfigFile[j].Contains('#'))  then
            begin
              VirtualMachineConfigFile.Delete(j);
            end;
        end;

        if RegexObj.Exec(VirtualMachineConfigFile.Text) then
        begin
            repeat
              PatternValueList.Add(RegexObj.Match[1]);
            until not RegexObj.ExecNext;
        end;
      end;
  end;

  if RegexObj.Exec(CurrentVmConfig.Text) then
  begin
      repeat
        PatternValueList.Add(RegexObj.Match[1]);
      until not RegexObj.ExecNext;
  end;

  PatternValueList.Sorted:=True;

  flag:=True;
  c:=StartValue;

  while(flag) do
  begin
    if (PatternValueList.IndexOf(c.ToString) = -1) then
    begin
      PatternValue:=c.ToString;
      flag:=False;
    end;

    Inc(c);
  end;

  RegexObj.Free;
  VirtualMachineConfigFile.Free;
  PatternValueList.Free;
  Directories.Free;

  Result:=PatternValue;
end;

function GetPatternValueFromConfigFile(Pattern: String; VmName: String
  ): String;
var
  VirtualMachineConfigFile : TStringList;
  PatternValueList : TStringList;
  RegexObj: TRegExpr;
  PatternValue : String;
  flag : Boolean;
  c,i:Integer;
begin
  VirtualMachineConfigFile:=TStringList.Create;
  PatternValueList:=TStringList.Create;
  RegexObj := TRegExpr.Create;
  RegexObj.Expression := Pattern;
  PatternValue:=EmptyStr;

  if FileExists(VmPath+'/'+VmName+'/bhyve_config.conf') then
  begin
    VirtualMachineConfigFile.LoadFromFile(VmPath+'/'+VmName+'/bhyve_config.conf');

    for i:=VirtualMachineConfigFile.Count-1 downto 0 do
    begin
        if (VirtualMachineConfigFile[i].Contains('#'))  then
        begin
          VirtualMachineConfigFile.Delete(i);
        end;
    end;

    if RegexObj.Exec(VirtualMachineConfigFile.Text) then
    begin
      repeat
        PatternValueList.Add(RegexObj.Match[1]);
      until not RegexObj.ExecNext;
    end;
  end;

  PatternValueList.Sorted:=True;

  flag:=True;
  c:=0;

  while(flag) do
  begin
    if (PatternValueList.IndexOf(c.ToString) = -1) then
    begin
      PatternValue:=c.ToString;
      flag:=False;
    end;

    Inc(c);
  end;

  RegexObj.Free;
  VirtualMachineConfigFile.Free;
  PatternValueList.Free;

  Result:=PatternValue;
end;

{ Public functions }
function AttachDeviceToBridge(BridgeName: String; DeviceName: String;
  VmName: String): Boolean;
var
  root_cmd : String;
  ifconfig_cmd : String;
  output : String;
  status : Boolean;
  parameters : TStringArray;
begin
  Result:=False;

  root_cmd:=SudoCmd;
  ifconfig_cmd:=IfconfigCmd;

  if UseSudo = 'no' then
    root_cmd:=DoasCmd;

  parameters:=[ifconfig_cmd, BridgeName, 'addm', DeviceName];

  if FileExists(ifconfig_cmd) and FileExists(root_cmd) then
  begin
    status:=RunCommand(root_cmd, parameters, output, [poStderrToOutPut]);

    if status then
      Result:=status
    else
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : AttachDeviceToBridge : '+ DeviceName+' : '+output);
    end;
  end;
end;

function AddDnsmasqDhcpHostEntry(VmName: String; IpAddress: String; MacAddreess: String
  ): Boolean;
var
  FilePath : TStringList;
  ConfigFile : String;
begin
  Result:=False;

  FilePath:=TStringList.Create;
  ConfigFile:=DnsmasqDirectory+'/'+VmName+'.conf';

  if not FileExists(ConfigFile) then
    CreateFile(ConfigFile, GetCurrentUserName(), '660');

  try
    FilePath.LoadFromFile(ConfigFile);

    if FilePath.IndexOf('dhcp-host='+MacAddreess+','+VmName+','+IpAddress) = -1 then
    begin
      FilePath.Values['dhcp-host']:=MacAddreess+','+VmName+','+IpAddress;
      FilePath.SaveToFile(ConfigFile);
      RestartService('dnsmasq');
    end;
  except
    MessageDlg('Error message', 'Error saving data to '+ConfigFile+' file', mtError, [mbOK], 0);
  end;

  FilePath.Free;
end;

function AddDnsmasqHostRecordEntry(VmName: String; Ip6Address: String;
  MacAddreess: String): Boolean;
var
  FilePath : TStringList;
  ConfigFile : String;
begin
  Result:=False;

  FilePath:=TStringList.Create;
  ConfigFile:=DnsmasqDirectory+'/'+VmName+'.conf';

  if not FileExists(ConfigFile) then
    CreateFile(ConfigFile, GetCurrentUserName(), '660');

  try
    FilePath.LoadFromFile(ConfigFile);

    if FilePath.IndexOf('host-record='+VmName+','+Ip6Address) = -1 then
    begin
      FilePath.Values['host-record']:=VmName+','+Ip6Address;
      FilePath.SaveToFile(ConfigFile);
      RestartService('dnsmasq');
    end;

  except
    MessageDlg('Error message', 'Error saving data to '+ConfigFile+' file', mtError, [mbOK], 0);
  end;

  FilePath.Free;
end;

function CheckBhyveSupport(): Boolean;
begin
  Result:=False;

  if (CheckSysctl('hw.vmm.vmx.initialized') = '1') and (CheckSysctl('hw.vmm.vmx.cap.unrestricted_guest') = '1') then
  begin
    Result:=True;
  end;
end;

function CheckCidrRange(Subnet: String): Boolean;
var
  RegText: TRegExpr;
begin
  Result:=False;

  RegText := TRegExpr.Create('^(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\/([1-3][0-2]$|[0-2][0-9]$|0?[0-9]$)$');

  if RegText.Exec(Subnet) then
  begin
    Result:=True;
  end;

  RegText.Free
end;

function CheckKernelModule(Module: String): Boolean;
var
  kldstat_cmd : String;
  output : String;
  status : Boolean;
begin
  Result:=False;

  kldstat_cmd:=KldstatCmd;

  if FileExists(kldstat_cmd) then
  begin
    status:=RunCommand(kldstat_cmd, ['-q', '-m', module], output, [poStderrToOutPut, poUsePipes]);

    if status then
    begin
      Result:=status
    end
    else
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : CheckKernelModule : '+ Module+' : '+output);
    end;
  end;
end;

function CheckIpv6Address(Address: String): Boolean;
var
  RegText: TRegExpr;
begin
  Result:=False;

  RegText := TRegExpr.Create('^(([0-9a-f]{0,4}:){1,7}[0-9a-f]{0,4})$');

  if RegText.Exec(Address) then
  begin
    Result:=True;
  end;

  RegText.Free
end;

function CheckMacAddress(Mac: String): Boolean;
var
  RegText: TRegExpr;
begin
  Result:=False;

  RegText := TRegExpr.Create('^([0-9a-f]{2}:){5}[0-9a-f]{2}$');

  if RegText.Exec(Mac) then
  begin
    Result:=True;
  end;

  RegText.Free
end;

function CheckNetworkDeviceName(Name: String): Boolean;
var
  RegText: TRegExpr;
begin
  Result:=False;

  RegText := TRegExpr.Create('^tap[0-9]+$|^vmnet[0-9]+$');

  if RegText.Exec(Name) then
  begin
    Result:=True;
  end;

  RegText.Free
end;

function CheckSysctl(Name: String): String;
var
  sysctl_cmd : String;
  output : String;
  status : Boolean;
begin
  Result:=EmptyStr;

  sysctl_cmd:=SysctlCmd;

  if FileExists(sysctl_cmd) then
  begin
    status:=RunCommand(sysctl_cmd, ['-n',Name], output, [poStderrToOutPut]);

    if status then
      Result:=output
    else
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : CheckSysCtl : '+ Name+' : '+output);
  end;
end;

function CheckVmName(Name: String): Boolean;
var
  RegText: TRegExpr;
begin
  Result:=False;

  RegText := TRegExpr.Create('^[a-z0-9_-]{0,229}$');
//  RegText := TRegExpr.Create('^[a-z0-9][.a-z0-9_-]{0,229}[a-z0-9]$');

  if RegText.Exec(Name) then
  begin
    Result:=True;
  end;

  RegText.Free
end;

function CheckVmRunning(Name: String): Integer;
var
  PidNumber : Integer;
begin
  Result:=-1;

  PidNumber:=GetPidValue('^'+BhyveCmd+' -k '+VmPath+'/'+Name+'/bhyve_config.conf');

  if PidNumber > 0 then
    Result:=PidNumber
  else
  begin
    PidNumber:= GetPidValue('^bhyve: '+Name+'$');
    if PidNumber  > 0 then
      Result:=PidNumber;
  end;
end;

function CheckTpmSocketRunning(Name: String): Integer;
var
  PidNumber : Integer;
begin
  Result:=-1;

  PidNumber:=GetPidValue(VmPath+'/'+Name+'/tpm/swtpm.sock');

  if PidNumber > 0 then
    Result:=PidNumber
end;

function CheckZfsDataset(Dataset: String): Boolean;
var
  zfs_cmd : String;
  output : String;
  status : Boolean;
begin
  Result:=False;

  zfs_cmd:=ZfsCmd;

  if FileExists(zfs_cmd) then
  begin
    status:=RunCommand(zfs_cmd, ['list','-H','-o','name', Dataset], output, [poStderrToOutPut, poUsePipes]);

    if status then
      Result:=status
    else
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : CheckZfsDataset : '+ Dataset+' : '+output);
  end;
end;

function CheckZfsSupport(): Boolean;
begin
  Result:=False;

  if (CheckKernelModule('zfs')) and (CheckZfsDataset(ZfsZpool))then
  begin
    Result:=True;
  end;

end;

function ChownDir(Path: String): Boolean;
var
  root_cmd : String;
  chown_cmd : String;
  output : String;
  status : Boolean;
  parameters : TStringArray;
begin
  Result:=False;

  root_cmd:=SudoCmd;
  chown_cmd:=ChownCmd;

  if UseSudo = 'no' then
    root_cmd:=DoasCmd;

  parameters:=[chown_cmd, GetCurrentUserName()+':', Path];

  if FileExists(chown_cmd) and FileExists(root_cmd) then
  begin
    status:=RunCommand(root_cmd, parameters, output, [poStderrToOutPut]);

    if status then
      Result:=status
    else
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : ChownDir : '+ Path+' : '+output);
    end;
  end;
end;

function ChmodDir(Path: String): Boolean;
var
  root_cmd : String;
  chmod_cmd : String;
  output : String;
  status : Boolean;
  parameters : TStringArray;
begin
  Result:=False;

  root_cmd:=SudoCmd;
  chmod_cmd:=ChmodCmd;

  if UseSudo = 'no' then
    root_cmd:=DoasCmd;

  parameters:=[chmod_cmd, '750', Path];

  if FileExists(chmod_cmd) and FileExists(root_cmd) then
  begin
    status:=RunCommand(root_cmd, parameters, output, [poStderrToOutPut]);

    if status then
      Result:=status
    else
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : ChmodDir : '+ Path+' : '+output);
    end;
  end;
end;

function CreateDirectory(DirectoryName: String; UserName: String; DirMode : String = '700'): Boolean;
var
  root_cmd : String;
  install_cmd : String;
  output : String;
  status : Boolean;
  parameters : TStringArray;
begin
  Result:=False;

  root_cmd:=SudoCmd;
  install_cmd:=InstallCmd;

  if UseSudo = 'no' then
    root_cmd:=DoasCmd;

  parameters:=[install_cmd, '-d', '-m', DirMode, '-o', UserName, DirectoryName];

  if FileExists(install_cmd) and FileExists(root_cmd) then
  begin
    status:=RunCommand(root_cmd, parameters, output, [poStderrToOutPut]);

    if status then
      Result:=status
    else
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : CreateDirectory : '+ DirectoryName+' : '+output);
  end;
end;

function CreateFile(FileName: String; UserName: String; FileMode : String = '600'): Boolean;
var
  install_cmd : String;
  output : String;
  status : Boolean;
begin
  Result:=False;

  install_cmd:=InstallCmd;

  if FileExists(install_cmd) then
  begin
    status:=RunCommand(install_cmd, ['-m', FileMode, '-o', UserName, '/dev/null', FileName], output, [poStderrToOutPut]);

    if status then
      Result:=status
    else
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : Createfile : '+ FileName+' : '+output);
  end;
end;

function CreateNetworkDevice(DeviceName: String; VmName: String; Mtu: String
  ): Boolean;
var
  root_cmd : String;
  ifconfig_cmd : String;
  output : String;
  status : Boolean;
  parameters : TStringArray;
begin
  Result:=False;

  root_cmd:=SudoCmd;
  ifconfig_cmd:=IfconfigCmd;

  if UseSudo = 'no' then
    root_cmd:=DoasCmd;

  parameters:=[ifconfig_cmd, DeviceName, 'create', 'descr', '"'+VmName+' VM"'];

  if FileExists(ifconfig_cmd) and FileExists(root_cmd) then
  begin
    status:=RunCommand(root_cmd, parameters, output, [poStderrToOutPut]);

    if status then
      Result:=status
    else
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : '+VmName+' VM : CreateNetworkDevice : '+ DeviceName+' : '+output);
    end;
  end;
end;

function CreateTpmSocket(Path: String): Boolean;
var
  swtpm_cmd : String;
  output : String;
  status : Boolean;
  parameters : TStringArray;
begin
  Result:=False;

  swtpm_cmd:=SwtpmCmd;

  parameters:=['socket', '--tpmstate', 'backend-uri=file:///'+Path+'swtpm.state', '--tpm2'];
  parameters:=parameters+['--server', 'type=unixio,path='+Path+'swtpm.sock', '--log'];
  parameters:=parameters+['file='+Path+'swtpm.log', '--flags', 'not-need-init', '--daemon'];

  if FileExists(swtpm_cmd) and DirectoryExists(Path) then
  begin
    status:=RunCommand(swtpm_cmd, parameters, output, [poStderrToOutPut]);

    if status then
      Result:=status
    else
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : CreateTpmSocket : '+ Path+' : '+output);
    end;
  end;
end;

function DestroyNetworkInterface(IfName: String): Boolean;
var
  root_cmd : String;
  ifconfig_cmd : String;
  output : String;
  status : Boolean;
  parameters : TStringArray;
begin
  Result:=False;

  root_cmd:=SudoCmd;
  ifconfig_cmd:=IfconfigCmd;

  if UseSudo = 'no' then
    root_cmd:=DoasCmd;

  parameters:=[ifconfig_cmd, IfName, 'destroy'];

  if FileExists(ifconfig_cmd) then
  begin
    status:=RunCommand(root_cmd, parameters, output, [poStderrToOutPut]);

    if status then
      Result:=status
    else
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : DestroyNetworkInterface : '+ IfName+' : '+output);
    end;
  end;
end;

function DestroyVirtualMachine(VmName: String): Boolean;
var
  root_cmd : String;
  bhyvectl_cmd : String;
  output : String;
  status : Boolean;
  parameters : TStringArray;
begin
  Result:=False;

  root_cmd:=SudoCmd;
  bhyvectl_cmd:=BhyvectlCmd;

  if UseSudo = 'no' then
    root_cmd:=DoasCmd;

  parameters:=[bhyvectl_cmd, '--vm='+VmName, '--destroy'];

  if FileExists(bhyvectl_cmd) and FileExists(root_cmd) then
  begin
    status:=RunCommand(root_cmd, parameters, output, [poStderrToOutPut]);

    if status then
      Result:=status
    else
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : DestroyVirtualMachne : '+VmName+' : '+output);
    end;
  end;
end;

function ExtractNumberValue(TextLine: String; Suffix: String): String;
var
  RegText: TRegExpr;
begin
  Result:=EmptyStr;
  RegText := TRegExpr.Create('(\d+)'+Suffix);

  if RegText.Exec(TextLine) then
  begin
    Result:=RegText.Match[1];
  end;

  RegText.Free
end;

function ExtractPortValue(TextLine: String): String;
var
  TmpArray : TStringArray;
begin
  TmpArray:= TextLine.Split(':');

  if (TextLine.Contains('[')) and (TextLine.Contains(']:')) then
    Result:=TmpArray[3]
  else
    Result:=TmpArray[1];

end;

function ExtractVarName(TextLine: String): String;
var
  RegText: TRegExpr;
begin
  Result:=EmptyStr;

  RegText := TRegExpr.Create('(.*)\s:');

  if RegText.Exec(TextLine) then
  begin
    Result:=RegText.Match[1];
  end;

  RegText.Free
end;

function ExtractVarValue(TextLine: String): String;
var
  RegText: TRegExpr;
begin
  RegText := TRegExpr.Create('\S*\s:\s(.*)');

  if RegText.Exec(TextLine) then
    Result:=RegText.Match[1]
  else
    Result:=EmptyStr;

  RegText.Free
end;

function GenerateMacAddress(): String;
var
  Md5Hash : String;
  MacAddress : String;
begin
  Md5Hash:=LeftStr(MD5Print(MD5String(RandomRange(1,255).ToString+':'+DateTimeToStr(Now))),5);

  Md5Hash.Insert(1,':');
  Md5Hash.Insert(4,':');

  MacAddress:=BhyveOui+Md5Hash;

  Result:=MacAddress;
end;

function GenerateUuid(): String;
var
  Guid : TGUID;
  Uuid : String;
begin
  CreateGUID(Guid);

  Uuid := Lowercase(GUIDToString(Guid));
  delete(Uuid, 1, 1);
  delete(Uuid, Length(Uuid), 1);

  Result:=Uuid;
end;

function GetCurrentUserName(): String;
begin
  Result:=GetUserName(fpgetuid);
end;

function GetEventDeviceList(Path: String; Pattern: String): String;
var
  TmpDeviceList : TStringList;
begin
  TmpDeviceList:=FindAllFiles(Path, Pattern, False);

  Result:=TmpDeviceList.Text;

  TmpDeviceList.Free;
end;

function GetNewConsoleName(VmName : String): String;
var
  VtconName : String;
begin
  VtconName:=GetPatternValueFromConfigFile('pci.\d+\.\d+\.\d+.\S+.name=vtcon(\d+)', VmName);

  Result := VtconName;
end;

function GetNewComPortNumber(): String;
var
  PortNumber : String;
begin
  PortNumber:=GetPatternValueFromAllConfigFiles('tcp=\S+:(\d+)', FirstComPortNumber);

  Result := PortNumber;
end;

function GetNewNetworkName(BackendType: String): String;
var
  NetworkName : String;
begin
  NetworkName:=BackendType+GetPatternValueFromAllConfigFiles('pci.\d+.\d+.\d+.backend='+BackendType+'(\d+)', 0);

  Result := NetworkName;
end;

function GetNewNetworkName(CurrentVmName : String; CurrentVmConfig: TStringList; BackendType: String;
  StartValue: Integer): String;
var
  NetworkName : String;
begin
  NetworkName:=BackendType+GetPatternValueFromAllConfigFiles('pci.\d+.\d+.\d+.backend='+BackendType+'(\d+)', CurrentVmName, CurrentVmConfig, StartValue);

  Result := NetworkName;
end;

function GetNewStorageName(DiskPath: String; IsZvol: Boolean): String;
var
  c:Integer;
  flag:Boolean;
  DiskList : TStringList;
  DiskName : String;
  DiskExt : String;
begin
  flag:=True;
  DiskName:=EmptyStr;
  c:=0;

  if IsZvol then
    DiskExt:=EmptyStr
  else
    DiskExt:='.img';

  DiskList := FindAllFiles(DiskPath, 'disk*', false);
  DiskList.Sorted:=True;

  while(flag) do
  begin
    if (DiskList.IndexOf(DiskPath+'/disk'+IntToStr(c)+DiskExt) = -1) then
    begin
      DiskName:='disk'+IntToStr(c)+DiskExt;
      flag:=False;
    end;

    Inc(c);
  end;

  DiskList.Free;

  Result:=DiskName;
end;

function GetFileSize(FilePath: String; SizeUnit : String = 'B'): Int64;
var
  FileInfo : stat;
  FileSize : Int64;
begin
  FileInfo.st_size:=0;

  FileSize:=FileInfo.st_size;

  if FpStat(FilePath, FileInfo) = 0 then
    FileSize:=FileInfo.st_size;

  case SizeUnit of
    'B': Result:=FileSize;
    'K': Result:=FileSize div 1024;
    'M': Result:=FileSize div (1024**2);
    'G': Result:=FileSize div (1024**3);
  else Result:=0;
  end;
end;

function GetNewPciSlotNumber(VmName: String): String;
var
  PciSlotNumber : String;
begin
  PciSlotNumber:=GetPatternValueFromConfigFile('pci.\d+.(\d+).\d+.', VmName);

  Result := PciSlotNumber;
end;

function GetNewPciSlotNumber(StringList: TStringList): String;
var
  PciSlotNumber : String;
begin
  PciSlotNumber:=GetPatternValueFromStringList('pci.\d+.(\d+).\d+.', 0, StringList);

  Result := PciSlotNumber;
end;

function GetNewPciSlotNumber(StringList: TStringList; StartSlot: Integer
  ): String;
var
  TmpList : TStringList;
  PatternValueList : TStringList;
  RegexObj: TRegExpr;
  PatternValue : String;
  flag : Boolean;
  c : Integer;
  PciSlotNumber : String;
begin
  PciSlotNumber:='-1';
  PatternValue:=EmptyStr;

  TmpList:=TStringList.Create;
  PatternValueList:=TStringList.Create;
  RegexObj := TRegExpr.Create;

  TmpList.Text:=StringList.Text;

  flag:=True;

  repeat
    RegexObj.Expression := 'pci.\d+.'+StartSlot.ToString+'.(\d+).';
    PatternValueList.Clear;

    if RegexObj.Exec(TmpList.Text) then
    begin
      repeat
        PatternValueList.Add(RegexObj.Match[1]);
      until not RegexObj.ExecNext;
    end;

    PatternValueList.Sorted:=True;

    c:=0;

    repeat
      if (PatternValueList.IndexOf(c.ToString) = -1) then
      begin
        PatternValue:='0.'+StartSlot.ToString+'.'+c.ToString;
        flag:=False;
        Break;
      end;

      Inc(c);

    until (c > 7);

    Inc(StartSlot);

  until ((flag = False) or (StartSlot > 29));

  RegexObj.Free;
  TmpList.Free;
  PatternValueList.Free;

  if StartSlot <=29 then
    PciSlotNumber:=PatternValue;

  Result:=PciSlotNumber;
end;

function GetNewAhciPortNumber(BusNumber : String; VmName: String): String;
var
  PortNumber : String;
begin
  PortNumber:=GetPatternValueFromConfigFile('pci.\d+.'+BusNumber+'.\d+.port.(\d+).path', VmName);

  Result := PortNumber;
end;

function GetNewVmName(VmName: String): Boolean;
var
  i : Integer;
  Directories : TStringList;
begin
  Result:=True;

  Directories:=FindAllDirectories(VmPath, False);
  Directories.Sorted:=True;

  for i:=0 to Directories.Count-1 do
  begin
    if FileExists(Directories[i]+'/bhyve_config.conf') and FileExists(Directories[i]+'/'+ExtractFileName(Directories[i])+'.conf') then
      begin
        if (ExtractFileName(Directories[i]) = VmName) then
          Result:=False;
      end;
  end;

  Directories.Free;
end;

function GetNewVncPortNumber(): String;
var
  PortNumber : String;
begin
  PortNumber:=GetPatternValueFromAllConfigFiles('pci.\d+.\d+.\d+.tcp=\S+:(\d+)', FirstVncPortNumber);

  Result := PortNumber;
end;

function GetPciDeviceDescripcion(Device: String): String;
var
  PciDescripcion : String;
  RegexObj: TRegExpr;
  TmpOutput:String;
  pciconf_cmd : String;
  output : String;
  parameters : TStringArray;
  status : Boolean;
begin
  Result:=EmptyStr;
  TmpOutput:=EmptyStr;
  PciDescripcion:=EmptyStr;

  pciconf_cmd:=PciconfCmd;

  parameters:=['-lv', Device];

  if FileExists(pciconf_cmd) then
  begin
    status:=RunCommand(pciconf_cmd, parameters, output, [poStderrToOutPut, poUsePipes]);

    if status then
      TmpOutput:=output
    else
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : GetPciDeviceDescripcion : '+Device+' : '+output);
    end;
  end;

  RegexObj := TRegExpr.Create;
  RegexObj.Expression := Device+'@.*\n.*\n.*device\s+=\s\D(.*)\D\n\s+class';

  if RegexObj.Exec(TmpOutput) then
  begin
    repeat
      PciDescripcion:=RegexObj.Match[1];
    until not RegexObj.ExecNext;
  end;

  Result:=PciDescripcion;
  RegexObj.Free;
end;

function GetPciDeviceList(Device: String): String;
var
  PciList : TStringList;
  RegexObj: TRegExpr;
  TmpOutput:String;
  pciconf_cmd : String;
  output : String;
  parameters : TStringArray;
  status : Boolean;
begin
  Result:=EmptyStr;
  TmpOutput:=EmptyStr;

  PciList:=TStringList.Create();

  pciconf_cmd:=PciconfCmd;

  parameters:=['-l'];

  if FileExists(pciconf_cmd) then
  begin
    status:=RunCommand(pciconf_cmd, parameters, output, [poStderrToOutPut, poUsePipes]);

    if status then
      TmpOutput:=Trim(output)
    else
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : GetPciDeviceList : '+Device+' : '+output);
    end;
  end;

  RegexObj := TRegExpr.Create;
  RegexObj.Expression := Device+'(\d+)@pci';

  if RegexObj.Exec(TmpOutput) then
  begin
    repeat
      PciList.Add(RegexObj.Match[1]);
    until not RegexObj.ExecNext;
  end;

  PciList.Sorted:=True;

  Result:=PciList.Text;
  RegexObj.Free;
  PciList.free;
end;

function GetPidValue(Pattern: String): Integer;
var
  pgrep_cmd : String;
  root_cmd : String;
  output : String;
  parameters : TStringArray;
  status : Boolean;
begin
  Result:=-1;

  root_cmd:=SudoCmd;
  pgrep_cmd:=PgrepCmd;

  if UseSudo = 'no' then
    root_cmd:=DoasCmd;

  parameters:=[pgrep_cmd, '-fo', Pattern];

  if (FileExists(pgrep_cmd) and FileExists(root_cmd)) then
  begin
    status:=RunCommand(root_cmd, parameters, output, [poStderrToOutPut, poUsePipes]);

    if status then
      Result:=Trim(output).ToInt64
    else
    begin
      if not (output = EmptyStr) then
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : GetPidValue : '+output);
    end;
  end;
end;

function GetStorageSize(StoragePath: String): String;
begin
  Result:='0G';

  if (UseZfs = 'yes') and (StoragePath.Contains('/dev/zvol/'+ZfsZpool)) then
  begin
    if ZfsGetPropertyValue(StoragePath.Remove(0,10), 'refreservation', 'value') = 'none' then
      Result:=ZfsGetPropertyValue(StoragePath.Remove(0,10), 'volsize', 'value')
    else
      Result:=ZfsGetPropertyValue(StoragePath.Remove(0,10), 'refreservation', 'value');
  end
  else if StoragePath.Contains(VmPath) then
  begin
    Result:=GetFileSize(StoragePath, 'G').ToString + 'G';
  end;
end;

function GetStorageType(StoragePath: String): String;
begin
  Result:=EmptyStr;

  if (UseZfs = 'yes') and (StoragePath.Contains('/dev/zvol/'+ZfsZpool)) then
  begin
    if ZfsGetPropertyValue(StoragePath.Remove(0,10), 'refreservation', 'value') = 'none' then
      Result:='zfs sparse volume'
    else
      Result:='zfs volume';
  end
  else if StoragePath.Contains(VmPath) then
  begin
    Result:='image file';
  end;
end;

function GetZpoolList(): String;
var
  zpool_cmd : String;
  output : String;
  status : Boolean;
  parameters : TStringArray;
begin
  Result:=EmptyStr;

  zpool_cmd:=ZpoolCmd;

  parameters:=['list','-H', '-o', 'name'];

  if FileExists(zpool_cmd) then
  begin
    status:=RunCommand(zpool_cmd, parameters, output, [poStderrToOutPut]);

    if status then
      Result:=output
    else
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : GetZpoolList : '+output);
    end;
  end;
end;

function KillPid(Pid: Integer; Signal : String = '-TERM'): Boolean;
var
  root_cmd : String;
  kill_cmd : String;
  output : String;
  status : Boolean;
  parameters : TStringArray;
begin
  Result:=False;

  root_cmd:=SudoCmd;
  kill_cmd:=KillCmd;

  if UseSudo = 'no' then
    root_cmd:=DoasCmd;

  parameters:=[kill_cmd, Signal, Pid.ToString];

  if FileExists(kill_cmd) then
  begin
    status:=RunCommand(root_cmd, parameters, output, [poStderrToOutPut, poUsePipes]);

    if status then
      Result:=status
    else
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : KillPid : '+InttoStr(Pid)+' : '+output);
  end;
end;

function LoadKernelModule(Module: String): Boolean;
var
  kldload_cmd : String;
  root_cmd : String;
  output : String;
  parameters : TStringArray;
  status : Boolean;
begin
  Result:=False;

  root_cmd:=SudoCmd;
  kldload_cmd:=KldloadCmd;

  if UseSudo = 'no' then
    root_cmd:=DoasCmd;

  parameters:=[kldload_cmd, Module];

  if (FileExists(kldload_cmd)) and (FileExists(root_cmd)) and not (CheckKernelModule(Module)) then
  begin
    status:=RunCommand(root_cmd, parameters, output, [poStderrToOutPut, poUsePipes]);

    if status then
      Result:=status
    else
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : LoadKernelModule : '+Module+' : '+output);
    end;
  end;
end;

function RdpConnect(VmName: String; Username: String; Password: String;
  Width: String; Height: String): Boolean;
var
  xfreerdp_cmd : String;
  xfreerdp_args : String;
  xfreerdp_args_file : String;
  xfreerdp_args_list : TStringList;
  options : TStringArray;
  parameters : TStringArray;
  i : Integer;
begin
  Result:=True;

  xfreerdp_args_list:= TStringList.Create();

  xfreerdp_args:=TrimLeft(TrimRight(XfreerdpArgs));
  xfreerdp_args_file:=VmPath+'/'+VmName+'/rdp.args';
  xfreerdp_cmd:=XfreerdpCmd;

  options:=xfreerdp_args.Split(' ');
  options:=['/u:'+Username, '/p:'+Password, '/v:'+VmName, '/w:'+Width, '/h:'+Height, '/t:Bhyve - '+VmName]+options;

  for i:=0 to Length(options)-1 do
  begin
    xfreerdp_args_list.Add(options[i]);
  end;

  if not FileExists(xfreerdp_args_file) then
    CreateFile(xfreerdp_args_file, GetCurrentUserName(), '600');

  xfreerdp_args_list.SaveToFile(xfreerdp_args_file);
  parameters:=['/args-from:'+xfreerdp_args_file];

  if FileExists(xfreerdp_cmd) then
  begin
    MyAppThread := AppThread.Create(xfreerdp_cmd, parameters);
    MyAppThread.Start;
  end
  else
    Result:=False;

  xfreerdp_args_list.Free;
end;

function RemoveDirectory(Directory: String; Recursive: Boolean): Boolean;
var
  rm_cmd : String;
  root_cmd : String;
  output : String;
  parameters : TStringArray;
  status : Boolean;
begin
  Result:=False;

  root_cmd:=SudoCmd;
  rm_cmd:=RmCmd;

  if UseSudo = 'no' then
    root_cmd:=DoasCmd;

  parameters:=[rm_cmd];

  if Recursive then
    parameters:=parameters + ['-R'];

  parameters:=parameters + [VmPath+'/'+Directory];

  if (FileExists(rm_cmd)) and (FileExists(root_cmd) and DirectoryExists(VmPath+'/'+Directory)) then
  begin
    status:=RunCommand(root_cmd, parameters, output, [poStderrToOutPut]);

    if status then
      Result:=status
    else
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : RemoveDirectory : '+Directory+' : '+output);
    end;
  end;
end;

function RemoveFile(Path: String): Boolean;
begin
  Result:=False;

  if FpUnlink(Path) = 0 then
    Result:=True;
end;

function RemoveDnsmasqEntry(VmName: String): Boolean;
var
  Path : String;
begin
  Result:=False;

  Path:=DnsmasqDirectory+'/'+VmName+'.conf';

  if FpUnlink(Path) = 0 then
  begin
    RestartService('dnsmasq');
    Result:=True;
  end;
end;

function RestartService(Service: String): Boolean;
var
  service_cmd : String;
  root_cmd : String;
  output : String;
  parameters : TStringArray;
  status : Boolean;
begin
  Result:=False;

  root_cmd:=SudoCmd;
  service_cmd:=ServiceCmd;

  if UseSudo = 'no' then
    root_cmd:=DoasCmd;

  parameters:=[ServiceCmd, Service, 'restart'];

  if (FileExists(service_cmd)) and (FileExists(root_cmd)) then
  begin
    status:=RunCommand(root_cmd, parameters, output, [poStderrToOutPut]);

    if status then
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : RestartService : '+Service+' : OK');
      Result:=status
    end
    else
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : RestartService : '+Service+' : '+output);
    end;
  end;
end;

function StopVirtualMachine(Pid: Integer): Boolean;
begin
  Result:=KillPid(Pid, '-SIGTERM');
end;

function TruncateImage(ImagePath: String; ImageSize: String): Boolean;
var
  truncate_cmd : String;
  output : String;
  status : Boolean;
  parameters : TStringArray;
begin
  Result:=False;

  truncate_cmd:=TruncateCmd;

  parameters:=['-s', ImageSize];
  parameters:=parameters+[ImagePath];

  if FileExists(truncate_cmd) then
  begin
    status:=RunCommand(truncate_cmd, parameters, output, [poStderrToOutPut]);

    if status then
      Result:=status
    else
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : TruncateImage : '+ImagePath+' : '+output);
    end;
  end;
end;

function VncConnect(VmHost: String; VmName : String): Boolean;
var
  vnc_cmd : String;
begin
  Result:=True;

  vnc_cmd:=VncviewerCmd;

  if FileExists(vnc_cmd) then
  begin
    MyAppThread := AppThread.Create(vnc_cmd, ['-t', 'bhyve - '+VmName, 'vnc://'+VmHost]);
    MyAppThread.Start;
  end
  else
    Result:=False;
end;

function ZfsCreateDataset(ZfsPath: String): Boolean;
var
  root_cmd : String;
  zfs_cmd : String;
  output : String;
  status : Boolean;
  parameters : TStringArray;
  options : TStringArray;
begin
  Result:=False;

  root_cmd:=SudoCmd;
  zfs_cmd:=ZfsCmd;

  if UseSudo = 'no' then
    root_cmd:=DoasCmd;

  options:=ZfsCreateOptions.Split(' ');

  parameters:=[zfs_cmd, 'create']+ options;
  parameters:=parameters+[zfspath];

  if FileExists(root_cmd) and FileExists(zfs_cmd) then
  begin
    status:=RunCommand(root_cmd, parameters, output, [poStderrToOutPut]);

    if status then
    begin
      ChmodDir('/'+ZfsPath);
      ChownDir('/'+ZfsPath);

      Result:=status
    end
    else
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : ZfsCreateDataset : '+ ZfsPath+' : '+output);
    end;
  end;
end;

function ZfsGetPropertyValue(ZfsPath: String; ZfsProperty: String;
  ZfsField: String): String;
var
  zfs_cmd : String;
  output : String;
  status : Boolean;
  parameters : TStringArray;
begin
  Result:=EmptyStr;

  zfs_cmd:=ZfsCmd;

  parameters:=['get','-H', '-o', ZfsField, ZfsProperty, ZfsPath];

  if FileExists(zfs_cmd) then
  begin
    status:=RunCommand(zfs_cmd, parameters, output, [poStderrToOutPut]);

    if status then
      Result:=Trim(output)
    else
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : ZfsGetPropertyValue : '+ ZfsPath+' : '+output);
    end;
  end;
end;

function ZfsDestroy(ZfsPath: String; Recursive: Boolean = True; Force: Boolean = False): Boolean;
var
  root_cmd : String;
  zfs_cmd : String;
  output : String;
  status : Boolean;
  parameters : TStringArray;
begin
  Result:=False;

  root_cmd:=SudoCmd;
  zfs_cmd:=ZfsCmd;

  if UseSudo = 'no' then
    root_cmd:=DoasCmd;

  parameters:=[zfs_cmd, 'destroy'];

  if Recursive then
    parameters:=parameters + ['-r'];

  if Force then
    parameters:=parameters + ['-f'];

  parameters:=parameters + [ZfsPath];

  if FileExists(zfs_cmd) then
  begin
    status:=RunCommand(root_cmd, parameters, output, [poStderrToOutPut]);

    if status then
      Result:=status
    else
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : ZfsDestroy : '+ ZfsPath+' : '+output);
    end;
  end;
end;

function ZfsCreateZvol(ZfsPath: String; ZvolSize : String; ZvolSparse : Boolean = False): Boolean;
var
  zfs_cmd : String;
  root_cmd : String;
  output : String;
  sparse : String;
  status : Boolean;
  parameters : TStringArray;
  options : TStringArray;
begin
  Result:=False;

  root_cmd:=SudoCmd;

  if UseSudo = 'no' then
    root_cmd:=DoasCmd;

  zfs_cmd:=ZfsCmd;

  options:=ZfsCreateOptions.Split(' ');

  if ZvolSparse then
    sparse:='-sV'
  else
    sparse:='-V';

  parameters:=[zfs_cmd,'create', sparse, ZvolSize, '-o','volmode=dev'];
  parameters:=parameters+[ZfsPath];

  if FileExists(root_cmd) and FileExists(zfs_cmd) then
  begin
    status:=RunCommand(root_cmd, parameters, output, [poStderrToOutPut]);

    if status then
      Result:=status
    else
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : ZfsCreateZvol : '+ ZfsPath+' : '+output);
    end;
  end;
end;

end.

