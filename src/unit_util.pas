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

unit unit_util;

{$mode ObjFPC}
{$modeswitch arrayoperators+}
{$H+}

interface

uses
  Classes, SysUtils, Dialogs, FileUtil, Math, md5, process, RegExpr, Users, BaseUnix, StrUtils;

function AddDnsmasqDhcpHostEntry(const VmName: String; const IpAddress: String; const MacAddreess : String):Boolean;
function AddDnsmasqHostRecordEntry(const VmName: String; const Ip6Address: String; const MacAddreess : String):Boolean;
function CheckBhyveSupport():Boolean;
function CheckCidrRange(Subnet: String):Boolean;
function CheckFileExtension(ImageName: String): String;
function CheckFileType(ImageName: String): String;
function CheckFileWriteAccess(FileName: String): Boolean;
function CheckKernelModule(Module: String):Boolean;
function CheckIpv6Address(const Address: String):Boolean;
function CheckIpvAddress(const Address: String):Boolean;
function CheckMacAddress(const Mac: String):Boolean;
function CheckNetworkDeviceName(const Name: String):Boolean;
function CheckNetworkPort(Port: String):Boolean;
function CheckSysctl(const Name: String):String;
function CheckUrl(const Url: String):Boolean;
function CheckUserName(const Name: String):Boolean;
function CheckVmName(const Name: String):Boolean;
function CheckVmRunning(const Name: String):Integer;
function CheckTpmSocketRunning(const Name: String):Integer;
function CheckZfsDataset(const Dataset: String): Boolean;
function CheckZfsSupport():Boolean;
function ConvertFileSize(Size: Int64; SizeUnit: String): Int64;
function CreateFile(const FileName: String; const UserName : String; const GroupName : String = 'bhyvemgrd'; FileMode : String = '600'):Boolean;
function CreateSeedIso(const SourceDirectory: String; const DestinationSeedFile : String):Boolean;
function CreateTpmSocket(const Path: String):Boolean;
function ExtractCidr(const Network: String): String;
function ExtractIpv6Prefix(const prefix : String):String;
function ExtractInterfaceMac(const NetworkInterface : String):String;
function ExtractNetMask(Cidr: Integer): String;
function ExtractNumberValue(TextLine: String; Suffix: String): String;
function ExtractPortValue(TextLine: String): String;
function ExtractVarName(TextLine: String): String;
function ExtractVarValue(TextLine: String): String;
function FirstIpAddress(const Network: String): String;
function GenerateIpv6Preffix():String;
function GenerateIpv6Suffix(const mac : String):String;
function GenerateMacAddress(): String;
function GenerateUuid(): String;
function GetCurrentUserName(): String;
function GetFileSize(const FilePath : String; SizeUnit : String = 'B'): Int64;
function GetEventDeviceList(const Path : String; Pattern : String):String;
function GetExtractSize(const FilePath: String; FileType: String): Int64;
function GetNetworkInterfaceList(NetworkInterfaceType : String): String;
function GetNetworkIp4List(const NetworkInterface : String): String;
function GetNetworkIp6List(const NetworkInterface : String): String;
function GetNewConsoleName(const VmName : String): String;
function GetNewIpAddress(const Subnet : String): String;
function GetNewIp6Address(const prefix : String; mac : String): String;
function GetNewPciSlotNumber(const VmName : String): String;
function GetNewPciSlotNumber(const StringList : TStringList): String;
function GetNewPciSlotNumber(const StringList : TStringList; StartSlot : Integer): String;
function GetNewAhciPortNumber(const BusNumber : String; const VmName : String): String;
function GetNewComPortNumber(): String;
function GetNewNetworkName(BackendType : String): String;
function GetNewNetworkName(const CurrentVmName : String; const CurrentVmConfig : TStringList; BackendType : String; StartValue : Integer): String;
function GetNewStorageName(const DiskPath : String; IsZvol: Boolean): String;
function GetNewVmName(const VmName : String): Boolean;
function GetNewVncPortNumber(): String;
function GetPciDeviceDescripcion(const Device : String):String;
function GetPciDeviceList(const Device : String):String;
function GetRemoteSize(const Url : String): Int64;
function GetServicePortList(Protocol : String):TStringList;
function GetStorageSize(const StoragePath : String): String;
function GetStorageType(const StoragePath : String): String;
function GetVmNetworkInterfaceList(VmName : String): String;
function GetZpoolList():String;
function InstallFile(const SourceFileName: String; const DestinationFileName : String; const UserName : String; FileMode : String = '600'):Boolean;
function PfCreateRules(const VmName : String; const VmRules: String; const RulesType : String):Boolean;
function NetworkAddress(const Subnet : String):String;
function RdpConnect(const VmName : String; const Username : String; const Password : String; Width : String; Height : String):Boolean;
function RemoveFile(const Path: String):Boolean;
function RemoveDnsmasqEntry(const VmName: String):Boolean;
function StopVirtualMachine(Pid : Integer):Boolean;
function TruncateImage(const ImagePath : String; ImageSize : String):Boolean;
function VncConnect(VmHost : String):Boolean;
function ZfsGetPropertyValue(const ZfsPath : String; ZfsProperty : String; ZfsField : String):String;

implementation

uses
  unit_configuration, unit_component ,unit_global, unit_language, unit_thread, unit_helper_client, LazLogger;

var
  MyAppThread: AppThread;

{ Private IPv4 functions }
function ExtractCidr(const Network: String): String;
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

function ExtractInterfaceMac(const NetworkInterface: String): String;
var
  RegexObj: TRegExpr;
  TmpOutput:String;
  output : String;
  parameters : TStringArray;
  status : Boolean;
begin
  Result:=EmptyStr;
  TmpOutput:=EmptyStr;

  parameters:=[NetworkInterface, 'ether'];

  if FileExists(IFCONFIG_CMD) then
  begin
    status:=RunCommand(IFCONFIG_CMD, parameters, output, [poStderrToOutPut, poUsePipes]);

    if status then
    begin
      TmpOutput:=Trim(output);

      RegexObj := TRegExpr.Create;
      RegexObj.Expression := 'ether\s+(\S+)';

      if RegexObj.Exec(TmpOutput) then
      begin
        Result:=RegexObj.Match[1];
      end;

      RegexObj.Free;
    end
    else
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : GetNetworkInterfaceList : '+output);
    end;
  end;
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

function PfCreateRules(const VmName: String; const VmRules: String; const RulesType: String): Boolean;
var
  FilePath : TStringList;
  DirePath : String;
  ConfigFile : String;
begin
  Result:=True;

  FilePath:=TStringList.Create;

  try
    DirePath:=VmPath+'/'+VmName+'/pf';
    ConfigFile:=DirePath+'/'+RulesType+'.rules';

    if not DirectoryExists(VmPath+'/'+DirePath) then
      CreateDirectoryHelper(DirePath, GetCurrentUserName(), BHYVEMGRD_GROUP, '750');

    try
      FilePath.Text:=VmRules;

      if FilePath.Count = 0 then
      begin
        if FileExists(ConfigFile) then
        begin
          RemoveFile(ConfigFile);
        end;
      end
      else
      begin
        if not FileExists(ConfigFile) then
          CreateFile(ConfigFile, GetCurrentUserName(), BHYVEMGRD_GROUP, '640');
        FilePath.SaveToFile(ConfigFile);
      end;
    except
      MessageDialog(mtError, Format(error_saving_file, [ConfigFile]));
      Result:=False;
    end;
  finally
    FilePath.Free;
  end;
end;

function NetworkAddress(const Subnet : String):String;
begin
  Result:=DecimalToIP(IpToDecimal(ExtractIP(Subnet), ExtractCidr(Subnet).ToInteger));
end;

function FirstIpAddress(const Network: String): String;
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

  FirstIp:=FirstIpAddress(NetworkAddress(Subnet)).Split('.');
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

function GetNewIpAddress(const Subnet : String): String;
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

        if not (IpAddress.IsEmpty) and (CheckValidIpAddress(IpAddress, GetSubnet)) then
          IpAddressValueList.Add(IpAddress);

        ConfigurationFile.Free;
      end;
  end;

  IpArray:=FirstIpAddress(NetworkAddress(Subnet)).Split('.');

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
function ExtractIpv6Prefix(const prefix: String): String;
var
  tmpPrefix : TStringArray;
  finalPrefix : TStringArray;
begin
  tmpPrefix:=prefix.Split(':');

  finalPrefix:=[tmpPrefix[0],tmpPrefix[1],tmpPrefix[2],tmpPrefix[3]];

  if finalPrefix[0].IsEmpty then finalPrefix[0] := '0';
  if finalPrefix[1].IsEmpty then finalPrefix[1] := '0';
  if finalPrefix[2].IsEmpty then finalPrefix[2] := '0';
  if finalPrefix[3].IsEmpty then finalPrefix[3] := '0';

  Result:= String.Join(':', finalPrefix);
end;

function GenerateIpv6Preffix(): String;
var
  tmpPreffix : String;
begin
  tmpPreffix :='fd'+LeftStr(MD5Print(MD5String(RandomRange(1,255).ToString+':'+DateTimeToStr(Now))),10);

  Result:=Copy(tmpPreffix, 1, 4) +':'+ Copy(tmpPreffix, 5, 4)+':'+ Copy(tmpPreffix, 9, 4)+':0001::';
end;

function GenerateIpv6Suffix(const mac: String): String;
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

function GetNewIp6Address(const prefix : String; mac : String): String;
begin
  Result:= ExtractIpv6Prefix(prefix)+':'+GenerateIpv6Suffix(mac);
end;

function GetPatternValueFromStringList(const Pattern: String;  StartValue : Integer; const StringList: TStringList
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

function GetPatternValueFromAllConfigFiles(const Pattern: String; StartValue : Integer): String;
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

function GetPatternValueFromAllConfigFiles(const Pattern: String;
  const CurrentVmName: String; const CurrentVmConfig: TStringList; StartValue: Integer
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

function GetPatternValueFromConfigFile(const Pattern: String; const VmName: String
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
function AddDnsmasqDhcpHostEntry(const VmName: String; const IpAddress: String; const MacAddreess: String
  ): Boolean;
var
  FilePath : TStringList;
  ConfigFile : String;
begin
  Result:=False;

  FilePath:=TStringList.Create;
  ConfigFile:=DNSMASQDHCP_PATH+'/'+VmName+'.conf';

  if not FileExists(ConfigFile) then
    CreateFile(ConfigFile, GetCurrentUserName(), BHYVEMGRD_GROUP, '644');

  try
    FilePath.LoadFromFile(ConfigFile);

    if FilePath.IndexOf(MacAddreess+','+VmName+','+IpAddress) = -1 then
    begin
      FilePath.Add(MacAddreess+','+VmName+','+IpAddress);
      FilePath.SaveToFile(ConfigFile);
    end;
  except
    MessageDialog(mtError, Format(error_saving_file, [ConfigFile]));
  end;

  FilePath.Free;
end;

function AddDnsmasqHostRecordEntry(const VmName: String; const Ip6Address: String;
  const MacAddreess: String): Boolean;
var
  FilePath : TStringList;
  ConfigFile : String;
begin
  Result:=False;

  FilePath:=TStringList.Create;
  ConfigFile:=DNSMASQHOST_PATH+'/'+VmName+'.conf';

  if not FileExists(ConfigFile) then
    CreateFile(ConfigFile, GetCurrentUserName(), BHYVEMGRD_GROUP, '644');

  try
    FilePath.LoadFromFile(ConfigFile);

    if FilePath.IndexOf(Ip6Address+' '+VmName) = -1 then
    begin
      FilePath.Add(Ip6Address+' '+VmName);
      FilePath.SaveToFile(ConfigFile);
    end;

  except
    MessageDialog(mtError, Format(error_saving_file, [ConfigFile]));
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

function CheckFileExtension(ImageName: String): String;
var
  extension : String;
begin
  Result:=EmptyStr;

  extension:=ExtractFileExt(ImageName);

  if extension = '.xz' then
    Result:=ExtractFileExt(LeftStr(ImageName, Length(ImageName)-Length(ExtractFileExt(ImageName))))+extension
  else
    Result:=ExtractFileExt(ImageName);
end;

function CheckFileType(ImageName: String): String;
var
  output : String;
  status : Boolean;
  parameters : TStringArray;
begin
  Result:='unknown';

 parameters:=['-b', ImageName];

  if FileExists(FILE_CMD) then
  begin
    status:=RunCommand(FILE_CMD, parameters, output, [poStderrToOutPut]);

    if status then
      Result:=LowerCase(trim(output.Split(' ')[0]))
    else
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : CheckFileType : '+output);
  end;
end;

function CheckFileWriteAccess(FileName: String): Boolean;
var
  F: TextFile;
begin
  try
    AssignFile(F, FileName);
    Rewrite(F);
    CloseFile(F);
    DeleteFile(FileName);
    Result:=True;
  except
    Result:=False;
  end;
end;

function CheckKernelModule(Module: String): Boolean;
var
  output : String;
  status : Boolean;
begin
  Result:=False;

  if FileExists(kldstat_cmd) then
  begin
    status:=RunCommand(KLDSTAT_CMD, ['-q', '-m', module], output, [poStderrToOutPut, poUsePipes]);

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

function CheckIpv6Address(const Address: String): Boolean;
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

function CheckIpvAddress(const Address: String): Boolean;
var
  RegText: TRegExpr;
begin
  Result:=False;

  RegText := TRegExpr.Create('^(?:\b\.?(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){4}$');

  if RegText.Exec(Address) then
  begin
    Result:=True;
  end;

  RegText.Free
end;

function CheckMacAddress(const Mac: String): Boolean;
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

function CheckNetworkDeviceName(const Name: String): Boolean;
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

function CheckNetworkPort(Port: String): Boolean;
var
  RegText: TRegExpr;
begin
  Result:=False;

  RegText := TRegExpr.Create('^(?:6553[0-5]|655[0-2]\d|65[0-4]\d{2}|6[0-4]\d{3}|[1-5]\d{1,4}|[1-9]\d{0,3})(?::(?:6553[0-5]|655[0-2]\d|65[0-4]\d{2}|6[0-4]\d{3}|[1-5]\d{1,4}|[1-9]\d{0,3}))?$');

  if RegText.Exec(Port) then
  begin
    Result:=True;
  end;

  RegText.Free
end;

function CheckSysctl(const Name: String): String;
var
  output : String;
  status : Boolean;
begin
  Result:=EmptyStr;

  if FileExists(SYSCTL_CMD) then
  begin
    status:=RunCommand(SYSCTL_CMD, ['-n',Name], output, [poStderrToOutPut]);

    if status then
      Result:=output
    else
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : CheckSysCtl : '+ Name+' : '+output);
  end;
end;

function CheckUrl(const Url: String): Boolean;
var
  RegText: TRegExpr;
begin
  Result:=False;

  RegText := TRegExpr.Create('^(https?|file):\/\/');

  if RegText.Exec(Url) then
  begin
    Result:=True;
  end;

  RegText.Free
end;

function CheckUserName(const Name: String): Boolean;
var
  RegText: TRegExpr;
begin
  Result:=False;

  RegText := TRegExpr.Create('^[a-z_][a-z0-9_-]{0,30}[a-z0-9_]$');

  if RegText.Exec(Name) then
  begin
    Result:=True;
  end;

  RegText.Free
end;

function CheckVmName(const Name: String): Boolean;
var
  RegText: TRegExpr;
begin
  Result:=False;

  RegText := TRegExpr.Create('^[a-z0-9]{1,64}$');

  if RegText.Exec(Name) then
  begin
    Result:=True;
  end;

  RegText.Free
end;

function CheckVmRunning(const Name: String): Integer;
var
  PidNumber : Integer;
begin
  Result:=-1;

  PidNumber:= GetPIDValueHelper(Format('^bhyve: %s$|^%s -k %s/%s/bhyve_config.conf', [Name, BhyveCmd, VmPath, Name]));

  if PidNumber > 0 then
    Result:=PidNumber
end;

function CheckTpmSocketRunning(const Name: String): Integer;
var
  PidNumber : Integer;
begin
  Result:=-1;

  PidNumber:=GetPIDValueHelper(Format('%s/%s/tpm/swtpm.sock', [VmPath, Name]));

  if PidNumber > 0 then
    Result:=PidNumber
end;

function CheckZfsDataset(const Dataset: String): Boolean;
var
  output : String;
  status : Boolean;
begin
  Result:=False;

  if FileExists(ZFS_CMD) then
  begin
    status:=RunCommand(ZFS_CMD, ['list','-H','-o','name', Dataset], output, [poStderrToOutPut, poUsePipes]);

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

function ConvertFileSize(Size: Int64; SizeUnit: String): Int64;
begin
  case SizeUnit of
    'B': Result:=Size;
    'K': Result:=Size div 1024;
    'M': Result:=Size div (1024**2);
    'G': Result:=Size div (1024**3);
  else Result:=0;
  end;
end;

function CreateFile(const FileName: String; const UserName: String; const GroupName : String = 'bhyvemgrd'; FileMode : String = '600'): Boolean;
var
  output : String;
  status : Boolean;
begin
  Result:=False;

  if FileExists(INSTALL_CMD) then
  begin
    status:=RunCommand(INSTALL_CMD, ['-m', FileMode, '-o', UserName, '-g', GroupName, '/dev/null', FileName], output, [poStderrToOutPut]);

    if status then
      Result:=status
    else
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : Createfile : '+ FileName+' : '+output);
  end;
end;

function CreateSeedIso(const SourceDirectory: String; const DestinationSeedFile: String
  ): Boolean;
var
  output : String;
  status : Boolean;
  parameters : TStringArray;
begin
  Result:=False;

  parameters:=['-t', 'cd9660', '-o', 'R,L=cidata', DestinationSeedFile, SourceDirectory];

  if FileExists(MAKEFS_CMD) then
  begin
    status:=RunCommand(MAKEFS_CMD, parameters, output, [poStderrToOutPut]);

    if status then
      Result:=status
    else
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : CreateSeedIso : '+ DestinationSeedFile+' : '+output);
  end;
end;

function CreateTpmSocket(const Path: String): Boolean;
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

function GetEventDeviceList(const Path: String; Pattern: String): String;
var
  TmpDeviceList : TStringList;
begin
  TmpDeviceList:=FindAllFiles(Path, Pattern, False);

  Result:=TmpDeviceList.Text;

  TmpDeviceList.Free;
end;

function GetExtractSize(const FilePath: String; FileType: String): Int64;
var
  app_cmd : String;
  output : String;
  status : Boolean;
  parameters : TStringArray;
  RegText: TRegExpr;
begin
  app_cmd:=EmptyStr;
  parameters:=[];
  Result:=0;

  RegText:= TRegExpr.Create;

  case FileType of
    'qcow2':
      begin
        app_cmd:=QemuImgCmd;
        parameters:=['info', FilePath];
        RegText.Expression:='virtual\ssize:\s\S+\s\S+\s\((\d+)\sbytes';
      end;
    'raw':
      begin
        app_cmd:=QemuImgCmd;
        parameters:=['info', '--output=json', FilePath];
        RegText.Expression:='raw\S,\s+\Sactual-size\S:\s(\d+),';
      end;
    'xz':
      begin
        app_cmd:=XZ_CMD;
        parameters:=['--robot', '-l', FilePath];
        RegText.Expression:='totals\s\d+\s\d+\s\d+\s+(\d+)\s';
      end;
  end;

  if FileExists(app_cmd) then
  begin
    status:=RunCommand(app_cmd, parameters, output, [poStderrToOutPut]);

    if status then
    begin
      if RegText.Exec(output) then
      begin
        Result:=StrToInt64(RegText.Match[1]);
      end
    end
    else
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : GetExtractSize : '+FilePath+' : '+output);
  end;

  RegText.Free;
end;

function GetNetworkInterfaceList(NetworkInterfaceType : String): String;
var
  NetworkList : TStringList;
  RegexObj: TRegExpr;
  TmpOutput:String;
  output : String;
  parameters : TStringArray;
  status : Boolean;
begin
  Result:=EmptyStr;
  TmpOutput:=EmptyStr;
  parameters:=[];

  NetworkList:=TStringList.Create();

  if NetworkInterfaceType = 'ether' then
    parameters:=['-l', '-u', 'ether']
  else if NetworkInterfaceType = 'bridge' then
    parameters:=['-l', '-u', '-g', 'bridge' ];

  if FileExists(IFCONFIG_CMD) then
  begin
    status:=RunCommand(IFCONFIG_CMD, parameters, output, [poStderrToOutPut, poUsePipes]);

    if status then
      TmpOutput:=Trim(output)
    else
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : GetNetworkInterfaceList : '+output);
    end;
  end;

  RegexObj := TRegExpr.Create;
  RegexObj.Expression := '(\S+)';

  if RegexObj.Exec(TmpOutput) then
  begin
    repeat
      NetworkList.Add(RegexObj.Match[1]);
    until not RegexObj.ExecNext;
  end;

  NetworkList.Sorted:=True;

  Result:=NetworkList.Text;
  RegexObj.Free;
  NetworkList.free;
end;

function GetNetworkIp4List(const NetworkInterface: String): String;
var
  InetList : TStringList;
  RegexObj: TRegExpr;
  TmpOutput:String;
  output : String;
  parameters : TStringArray;
  status : Boolean;
begin
  Result:=EmptyStr;
  TmpOutput:=EmptyStr;

  InetList:=TStringList.Create();

  parameters:=[NetworkInterface];

  if FileExists(IFCONFIG_CMD) then
  begin
    status:=RunCommand(IFCONFIG_CMD, parameters, output, [poStderrToOutPut, poUsePipes]);

    if status then
      TmpOutput:=Trim(output)
    else
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : GetNetworkIp4List : '+NetworkInterface+' : '+output);
    end;
  end;

  RegexObj := TRegExpr.Create;
  RegexObj.Expression := 'inet\s(\S+)\s';

  if RegexObj.Exec(TmpOutput) then
  begin
    repeat
      InetList.Add(RegexObj.Match[1]);
    until not RegexObj.ExecNext;
  end;

  InetList.Sorted:=True;

  Result:=InetList.Text;
  RegexObj.Free;
  InetList.free;
end;

function GetNetworkIp6List(const NetworkInterface: String): String;
var
  InetList : TStringList;
  RegexObj: TRegExpr;
  TmpOutput:String;
  output : String;
  parameters : TStringArray;
  status : Boolean;
begin
  Result:=EmptyStr;
  TmpOutput:=EmptyStr;

  InetList:=TStringList.Create();

  parameters:=[NetworkInterface];

  if FileExists(IFCONFIG_CMD) then
  begin
    status:=RunCommand(IFCONFIG_CMD, parameters, output, [poStderrToOutPut, poUsePipes]);

    if status then
      TmpOutput:=Trim(output)
    else
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : GetNetworkIp6List : '+NetworkInterface+' : '+output);
    end;
  end;

  RegexObj := TRegExpr.Create;
  RegexObj.Expression := 'inet6\s(\S+)\s';

  if RegexObj.Exec(TmpOutput) then
  begin
    repeat
      InetList.Add(RegexObj.Match[1]);
    until not RegexObj.ExecNext;
  end;

  InetList.Sorted:=True;

  Result:=InetList.Text;
  RegexObj.Free;
  InetList.free;
end;

function GetNewConsoleName(const VmName : String): String;
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

function GetNewNetworkName(const CurrentVmName : String; const CurrentVmConfig: TStringList; BackendType: String;
  StartValue: Integer): String;
var
  NetworkName : String;
begin
  NetworkName:=BackendType+GetPatternValueFromAllConfigFiles('pci.\d+.\d+.\d+.backend='+BackendType+'(\d+)', CurrentVmName, CurrentVmConfig, StartValue);

  Result := NetworkName;
end;

function GetNewStorageName(const DiskPath: String; IsZvol: Boolean): String;
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

function GetFileSize(const FilePath: String; SizeUnit : String = 'B'): Int64;
var
  FileInfo : stat;
  FileSize : Int64;
begin
  FileInfo.st_size:=0;

  FileSize:=FileInfo.st_size;

  if FpStat(FilePath, FileInfo) = 0 then
    FileSize:=FileInfo.st_size;

  Result:=ConvertFileSize(FileSize, SizeUnit);
end;

function GetNewPciSlotNumber(const VmName: String): String;
var
  PciSlotNumber : String;
begin
  PciSlotNumber:=GetPatternValueFromConfigFile('pci.\d+.(\d+).\d+.', VmName);

  Result := PciSlotNumber;
end;

function GetNewPciSlotNumber(const StringList: TStringList): String;
var
  PciSlotNumber : String;
begin
  PciSlotNumber:=GetPatternValueFromStringList('pci.\d+.(\d+).\d+.', 0, StringList);

  Result := PciSlotNumber;
end;

function GetNewPciSlotNumber(const StringList: TStringList; StartSlot: Integer
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

function GetNewAhciPortNumber(const BusNumber : String; const VmName: String): String;
var
  PortNumber : String;
begin
  PortNumber:=GetPatternValueFromConfigFile('pci.\d+.'+BusNumber+'.\d+.port.(\d+).path', VmName);

  Result := PortNumber;
end;

function GetNewVmName(const VmName: String): Boolean;
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

function GetPciDeviceDescripcion(const Device: String): String;
var
  PciDescripcion : String;
  RegexObj: TRegExpr;
  TmpOutput:String;
  output : String;
  parameters : TStringArray;
  status : Boolean;
begin
  Result:=EmptyStr;
  TmpOutput:=EmptyStr;
  PciDescripcion:=EmptyStr;

  parameters:=['-lv', Device];

  if FileExists(PCICONF_CMD) then
  begin
    status:=RunCommand(PCICONF_CMD, parameters, output, [poStderrToOutPut, poUsePipes]);

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

function GetPciDeviceList(const Device: String): String;
var
  PciList : TStringList;
  RegexObj: TRegExpr;
  TmpOutput:String;
  output : String;
  parameters : TStringArray;
  status : Boolean;
begin
  Result:=EmptyStr;
  TmpOutput:=EmptyStr;

  PciList:=TStringList.Create();

  parameters:=['-l'];

  if FileExists(PCICONF_CMD) then
  begin
    status:=RunCommand(PCICONF_CMD, parameters, output, [poStderrToOutPut, poUsePipes]);

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

function GetRemoteSize(const Url: String): Int64;
var
  output : String;
  status : Boolean;
  parameters : TStringArray;
begin
  Result:=0;

  parameters:=['-T','3','-s', Url];

  if FileExists(FETCH_CMD) then
  begin
    status:=RunCommand(FETCH_CMD, parameters, output, [poStderrToOutPut]);

    if status then
      Result:=StrToInt64(trim(output))
    else
    begin
      if not (output.IsEmpty) then
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : GetRemoteSize : '+Url+' : '+output);
    end;
  end;
end;

function GetServicePortList(Protocol: String): TStringList;
var
  ServiceList : TStringList;
  RegexObj: TRegExpr;
  TmpOutput:String;
begin
  TmpOutput:=EmptyStr;

  ServiceList:=TStringList.Create();
  ServiceList.LoadFromFile(SERVICES_FILE);

  TmpOutput:=ServiceList.Text;
  ServiceList.Clear;

  RegexObj := TRegExpr.Create;
  RegexObj.Expression := '([-_a-zA-Z0-9\/]+)\s+(\d+)\/'+Protocol;

  if RegexObj.Exec(TmpOutput) then
  begin
    repeat
      ServiceList.Add(RegexObj.Match[1]+'='+RegexObj.Match[2]);
    until not RegexObj.ExecNext;
  end;

  ServiceList.Sorted:=True;
  RegexObj.Free;

  Result:=ServiceList;
end;

function GetStorageSize(const StoragePath: String): String;
begin
  Result:='0G';

  if (UseZfs = 'yes') and (StoragePath.Contains('/dev/zvol/'+ZfsZpool)) then
  begin
    Result:=ZfsGetPropertyValue(StoragePath.Remove(0,10), 'volsize', 'value')
  end
  else if StoragePath.Contains(VmPath) then
  begin
    Result:=GetFileSize(StoragePath, 'G').ToString + 'G';
  end;
end;

function GetStorageType(const StoragePath: String): String;
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

function GetVmNetworkInterfaceList(VmName: String): String;
var
  NetworkList : TStringList;
  RegexObj: TRegExpr;
  TmpOutput:String;
begin
  TmpOutput:=EmptyStr;

  NetworkList:=TStringList.Create();
  NetworkList.LoadFromFile(VmPath+'/'+VmName+'/bhyve_config.conf');

  TmpOutput:=NetworkList.Text;
  NetworkList.Clear;

  RegexObj := TRegExpr.Create;
  RegexObj.Expression := 'pci\W\d+\W\d+\S\d+\Sbackend=(tap\d+|vmnet\d+)';

  if RegexObj.Exec(TmpOutput) then
  begin
    repeat
      NetworkList.Add(RegexObj.Match[1]);
    until not RegexObj.ExecNext;
  end;

  RegexObj.Free;

  Result:=NetworkList.Text;

  NetworkList.Free;
end;

function GetZpoolList(): String;
var
  output : String;
  status : Boolean;
  parameters : TStringArray;
begin
  Result:=EmptyStr;

  parameters:=['list','-H', '-o', 'name'];

  if FileExists(ZPOOL_CMD) then
  begin
    status:=RunCommand(ZPOOL_CMD, parameters, output, [poStderrToOutPut]);

    if status then
      Result:=output
    else
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : GetZpoolList : '+output);
    end;
  end;
end;

function InstallFile(const SourceFileName: String; const DestinationFileName: String;
  const UserName: String; FileMode: String = '600'): Boolean;
var
  output : String;
  status : Boolean;
begin
  Result:=False;

  if FileExists(INSTALL_CMD) then
  begin
    status:=RunCommand(INSTALL_CMD, ['-m', FileMode, '-o', UserName, SourceFileName, DestinationFileName], output, [poStderrToOutPut]);

    if status then
      Result:=status
    else
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : Installfile : '+ SourceFileName+' : '+output);
  end;
end;

function RdpConnect(const VmName: String; const Username: String; const Password: String;
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
    CreateFile(xfreerdp_args_file, GetCurrentUserName());

  xfreerdp_args_list.SaveToFile(xfreerdp_args_file);
  parameters:=['/args-from:file:'+xfreerdp_args_file];

  if FileExists(xfreerdp_cmd) then
  begin
    MyAppThread := AppThread.Create(xfreerdp_cmd, parameters);
    MyAppThread.Start;
  end
  else
    Result:=False;

  xfreerdp_args_list.Free;
end;

function RemoveFile(const Path: String): Boolean;
begin
  Result:=False;

  if FpUnlink(Path) = 0 then
    Result:=True;
end;

function RemoveDnsmasqEntry(const VmName: String): Boolean;
var
  DhcpPath : String;
  HostPath : String;
begin
  Result:=False;

  DhcpPath:=DNSMASQDHCP_PATH+'/'+VmName+'.conf';
  HostPath:=DNSMASQHOST_PATH+'/'+VmName+'.conf';

  if FpUnlink(DhcpPath) = 0 then
  begin
    if FileExists(HostPath) then
      FpUnlink(HostPath);

    RestartServiceHelper('dnsmasq');
    Result:=True;
  end;
end;

function StopVirtualMachine(Pid: Integer): Boolean;
begin
  Result:=KillPidHelper(Pid, '-SIGTERM');
end;

function TruncateImage(const ImagePath: String; ImageSize: String): Boolean;
var
  output : String;
  status : Boolean;
  parameters : TStringArray;
begin
  Result:=False;

  parameters:=['-s', ImageSize];
  parameters:=parameters+[ImagePath];

  if FileExists(TRUNCATE_CMD) then
  begin
    status:=RunCommand(TRUNCATE_CMD, parameters, output, [poStderrToOutPut]);

    if status then
      Result:=status
    else
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : TruncateImage : '+ImagePath+' : '+output);
    end;
  end;
end;

function VncConnect(VmHost: String): Boolean;
var
  vnc_cmd : String;
begin
  Result:=True;

  vnc_cmd:=VncviewerCmd;

  if FileExists(vnc_cmd) then
  begin
    if VmHost.StartsWith('unix:') then
      VmHost:=StringReplace(VmHost, 'unix:', EmptyStr, [rfReplaceAll]);

    MyAppThread := AppThread.Create(vnc_cmd, [VmHost]);
    MyAppThread.Start;
  end
  else
    Result:=False;
end;

function ZfsGetPropertyValue(const ZfsPath: String; ZfsProperty: String;
  ZfsField: String): String;
var
  output : String;
  status : Boolean;
  parameters : TStringArray;
begin
  Result:=EmptyStr;

  parameters:=['get','-H', '-o', ZfsField, ZfsProperty, ZfsPath];

  if FileExists(ZFS_CMD) then
  begin
    status:=RunCommand(ZFS_CMD, parameters, output, [poStderrToOutPut]);

    if status then
      Result:=Trim(output)
    else
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : ZfsGetPropertyValue : '+ ZfsField+' : '+ ZfsProperty+' : '+ ZfsPath+' : '+output);
    end;
  end;
end;

end.

