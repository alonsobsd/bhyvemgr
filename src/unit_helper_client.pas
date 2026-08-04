unit unit_helper_client;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, fpjson, jsonparser, unit_socket, unit_request, unit_global;
var
  HelperSocketThread: TSocketThread;
  HelperRequestManager: TRequestManager;

function AttachDeviceToBridgeHelper(const BridgeName: String; const DeviceName: String):Boolean;
function ChmodHelper(const Path: String; Mode : String = '750'):Boolean;
function ChownHelper(const Path: String; const Username : String):Boolean;
function CreateDirectoryHelper(const DirectoryName: String; const UserName : String; DirMode : String = '700'):Boolean;
function CreateNetworkDeviceHelper(const DeviceName: String; const VmName : String; Mtu : String = '1500'):Boolean;
function DestroyNetworkInterfaceHelper(const IfName: String): Boolean;
function DestroyVirtualMachineHelper(const VmName: String): Boolean;
function GetPIDValueHelper(const Pattern: String): Integer;
function KillPidHelper(Pid: Integer; Signal : String = '-TERM'): Boolean;
function PfLoadRulesHelper(const VmName : String; const RulesType : String):Boolean;
function PfUnloadRulesHelper(const VmName : String; const RulesType : String):Boolean;
function RestartServiceHelper(const Service: String): Boolean;
function RemoveDirectoryHelper(const VmName : String; const DireType: String; Recursive: Boolean): Boolean;
function ZfsCreateDatasetHelper(const ZfsType: String; const VmName : String; ZfsOptions : String; const WithMountpoint : Boolean = False): Boolean;
function ZfsCreateZvolHelper(const VmName: String; const DiskName: String; ZvolSize : String; ZvolSparse : Boolean = False):Boolean;
function ZfsDestroyHelper(const VmName : String; const ZfsType: String; const ZfsDevice : String; Recursive : Boolean = True; Force : Boolean = False):Boolean;
function ZfsSetPropertyValueHelper(const ZfsPath : String; ZfsProperty : String; ZfsValue : String):String;

implementation

function ExecuteHelper(const Method: String; Params: TJSONObject; Timeout: Cardinal = 1000): TJSONObject;
var
  Pending: TPendingRequest;
  Request: TJSONObject;
  Start: QWord;
begin
  Result := Nil;

  if (HelperSocketThread = nil) or (HelperRequestManager = nil) then
    raise Exception.Create('HelperClient no inicializado');

  Pending := HelperRequestManager.RegisterRequest;

  try
    Request := TJSONObject.Create;
    try
      Request.Add('type', 'task');
      Request.Add('id', Pending.ID);
      Request.Add('method', Method);
      Request.Add('params', Params);

      HelperSocketThread.SendJSON(Request.AsJSON);
    finally
      Request.Free;
    end;

    Start := GetTickCount64;

    repeat
      if Pending.Event.WaitFor(10) = wrSignaled then
        Break;

      CheckSynchronize(0);
    until GetTickCount64 - Start >= Timeout;

    Result := GetJSON(Pending.Response) as TJSONObject;
  finally
    HelperRequestManager.Remove(Pending.ID);
  end;
end;

function AttachDeviceToBridgeHelper(const BridgeName: String;
  const DeviceName: String): Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.Add('bridge', BridgeName);
  Params.Add('device', DeviceName);

  Resp := ExecuteHelper('network.attach_bridge', Params, 1000);
  try
    Result := Resp.Get('success', False);
  finally
    Resp.Free;
  end;
end;

function ChmodHelper(const Path: String; Mode: String): Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.Add('path', Path);
  Params.Add('mode', Mode);

  Resp := ExecuteHelper('fs.chmod', Params, 1000);
  try
    Result := Resp.Get('success', False);
  finally
    Resp.Free;
  end;
end;

function ChownHelper(const Path: String; const Username : String):Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.Add('path', Path);
  Params.Add('username', Username);

  Resp := ExecuteHelper('fs.chown', Params, 1000);
  try
    Result := Resp.Get('success', False);
  finally
    Resp.Free;
  end;
end;

function CreateDirectoryHelper(const DirectoryName: String; const UserName : String; DirMode : String = '700'):Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.Add('mode', DirMode);
  Params.Add('username', Username);
  Params.Add('directory', DirectoryName);

  Resp := ExecuteHelper('fs.mkdir', Params, 1000);
  try
    Result := Resp.Get('success', False);
  finally
    Resp.Free;
  end;
end;

function CreateNetworkDeviceHelper(const DeviceName: String; const VmName : String; Mtu : String = '1500'):Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.Add('device', DeviceName);
  Params.Add('vmname', VmName);

  Resp := ExecuteHelper('network.create_device', Params, 1000);
  try
    Result := Resp.Get('success', False);
  finally
    Resp.Free;
  end;
end;

function DestroyNetworkInterfaceHelper(const IfName: String): Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.Add('ifname', IfName);

  Resp := ExecuteHelper('network.destroy_device', Params, 1000);
  try
    Result := Resp.Get('success', False);
  finally
    Resp.Free;
  end;
end;

function DestroyVirtualMachineHelper(const VmName: String): Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.Add('vmname', VmName);

  Resp := ExecuteHelper('vm.destroy', Params, 1000);
  try
    Result := Resp.Get('success', False);
  finally
    Resp.Free;
  end;
end;

function GetPIDValueHelper(const Pattern: String): Integer;
var
  Params: TJSONObject;
  Resp: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.Add('pattern', Pattern);

  Resp := ExecuteHelper('process.get_pid', Params, 1000);
  try
    Result:=StrToInt64(Resp.Get('pid', '-1'));
  finally
    Resp.Free;
  end;
end;

function KillPidHelper(Pid: Integer; Signal : String = '-TERM'): Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.Add('signal', Signal);
  Params.Add('pid', Pid.ToString);

  Resp := ExecuteHelper('process.kill_pid', Params, 1000);
  try
    Result := Resp.Get('success', False);
  finally
    Resp.Free;
  end;
end;

function PfLoadRulesHelper(const VmName : String; const RulesType : String):Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.Add('vmname', VmName);
  Params.Add('ruletype', RulesType);

  Resp := ExecuteHelper('pf.load_rules', Params, 1000);
  try
    Result := Resp.Get('success', False);
  finally
    Resp.Free;
  end;
end;

function PfUnloadRulesHelper(const VmName : String; const RulesType : String):Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.Add('vmname', VmName);
  Params.Add('ruletype', RulesType);

  Resp := ExecuteHelper('pf.unload_rules', Params, 1000);
  try
    Result := Resp.Get('success', False);
  finally
    Resp.Free;
  end;
end;

function RestartServiceHelper(const Service: String): Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.Add('service', Service);

  Resp := ExecuteHelper('service.restart', Params, 1000);
  try
    Result := Resp.Get('success', False);
  finally
    Resp.Free;
  end;
end;

function RemoveDirectoryHelper(const VmName : String; const DireType: String; Recursive: Boolean): Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.Add('vmname', VmName);
  Params.Add('diretype', DireType);
  Params.Add('recursive', BoolToStr(Recursive, 'true', 'false'));

  Resp := ExecuteHelper('fs.rmdir', Params, 1000);
  try
    Result := Resp.Get('success', False);
  finally
    Resp.Free;
  end;
end;

function ZfsCreateDatasetHelper(const ZfsType: String; const VmName : String; ZfsOptions : String; const WithMountpoint : Boolean = False): Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.Add('vmname', VmName);
  Params.Add('zfstype', ZfsType);
  Params.Add('options', ZfsOptions);
  Params.Add('mountpoint', BoolToStr(WithMountpoint, 'true', 'false'));

  Resp := ExecuteHelper('zfs.create_dataset', Params, 1000);
  try
    Result := Resp.Get('success', False);
  finally
    Resp.Free;
  end;
end;

function ZfsCreateZvolHelper(const VmName: String; const DiskName: String; ZvolSize : String; ZvolSparse : Boolean = False):Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.Add('vmname', VmName);
  Params.Add('diskname', DiskName);
  Params.Add('volsize', ZvolSize);
  Params.Add('sparse', BoolToStr(ZvolSparse, 'true', 'false'));

  Resp := ExecuteHelper('zfs.create_zvol', Params, 1000);
  try
    Result := Resp.Get('success', False);
  finally
    Resp.Free;
  end;
end;

function ZfsDestroyHelper(const VmName : String; const ZfsType: String; const ZfsDevice : String; Recursive : Boolean = True; Force : Boolean = False):Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.Add('vmname', VmName);
  Params.Add('zfstype', ZfsType);
  Params.Add('zfsdevice', ZfsDevice);
  Params.Add('recursive', BoolToStr(Recursive, 'true', 'false'));
  Params.Add('force', BoolToStr(Force, 'true', 'false'));

  Resp := ExecuteHelper('zfs.destroy', Params, 1000);
  try
    Result := Resp.Get('success', False);
  finally
    Resp.Free;
  end;
end;

function ZfsSetPropertyValueHelper(const ZfsPath : String; ZfsProperty : String; ZfsValue : String):String;
var
  Params: TJSONObject;
  Resp: TJSONObject;
begin
  Params := TJSONObject.Create;
  Params.Add('path', ZfsPath);
  Params.Add('property', ZfsProperty);
  Params.Add('value', ZfsValue);

  Resp := ExecuteHelper('zfs.destroy', Params, 1000);
  try
    Result := Resp.Get('output', EmptyStr);
  finally
    Resp.Free;
  end;
end;

end.

