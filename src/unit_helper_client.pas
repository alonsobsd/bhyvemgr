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
function ChownHelper(const Path: String; const Username : String; const Groupname : String = 'bhyvemgrd'):Boolean;
function CreateDirectoryHelper(const DirectoryName: String; const UserName : String; Groupname : String = 'bhyvemgrd'; DirMode : String = '700'):Boolean;
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

uses
  LazLogger;

function ExecuteHelper(const Method: String; Params: TJSONObject; Timeout: Cardinal; out IsTimedout: Boolean): TJSONObject;
var
  Pending: TPendingRequest;
  Request: TJSONObject;
  Start: QWord;
begin
  Result := Nil;
  IsTimedout:=False;

  if (HelperSocketThread = nil) or (HelperRequestManager = nil) then
    raise Exception.Create('HelperClient not started');

  Pending := HelperRequestManager.RegisterRequest;

  try
    Request := TJSONObject.Create;
    try
      Request.Add('type', 'task');
      Request.Add('id', Pending.ID);
      Request.Add('method', Method);
      Request.Add('params', Params.Clone);

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

    if Pending.Response = EmptyStr then
    begin
       IsTimedout:=True;
       Exit(nil);
    end;

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
  IsTimedout: Boolean;
begin
  Params := TJSONObject.Create;

  try
    Params.Add('bridge', BridgeName);
    Params.Add('device', DeviceName);

    Resp := ExecuteHelper('network.attach_bridge', Params, 2000, IsTimedout);
    try
      if IsTimedout then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : AttachDeviceToBridgeHelper: timeout : '+DeviceName);
        Exit(False);
      end;

      if not Assigned(Resp) then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : AttachDeviceToBridgeHelper: without respond : '+DeviceName);
        Exit(False);
      end;

      Result := Resp.Get('success', False);
    finally
      Resp.Free;
    end;
  finally
    Params.Free;
  end;
end;

function ChmodHelper(const Path: String; Mode: String): Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
  IsTimedout : Boolean;
begin
  Params := TJSONObject.Create;

  try
    Params.Add('path', Path);
    Params.Add('mode', Mode);

    Resp := ExecuteHelper('fs.chmod', Params, 1000, IsTimedout);
    try
      if IsTimedout then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : ChmodHelper: timeout : '+Path);
        Exit(False);
      end;

      if not Assigned(Resp) then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : ChmodHelper: without respond : '+Path);
        Exit(False);
      end;

      Result := Resp.Get('success', False);
    finally
      Resp.Free;
    end;
  finally
    Params.Free;
  end;
end;

function ChownHelper(const Path: String; const Username : String; const Groupname : String):Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
  IsTimedout : Boolean;
begin
  Params := TJSONObject.Create;
  try
    Params.Add('path', Path);
    Params.Add('username', Username);
    Params.Add('groupname', Groupname);

    Resp := ExecuteHelper('fs.chown', Params, 1000, IsTimedout);
    try
      if IsTimedout then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : ChownHelper: timeout : '+Path);
        Exit(False);
      end;

      if not Assigned(Resp) then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : ChownHelper: without respond : '+Path);
        Exit(False);
      end;

      Result := Resp.Get('success', False);
    finally
      Resp.Free;
    end;
  finally
    Params.Free;
  end;
end;

function CreateDirectoryHelper(const DirectoryName: String; const UserName : String; Groupname : String = 'bhyvemgrd'; DirMode : String = '700'):Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
  IsTimedout: Boolean;
begin
  Params := TJSONObject.Create;

  try
    Params.Add('mode', DirMode);
    Params.Add('username', Username);
    Params.Add('groupname', Groupname);
    Params.Add('directory', DirectoryName);

    Resp := ExecuteHelper('fs.mkdir', Params, 1000, IsTimedout);
    try
      if IsTimedout then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : CreateDirectoryHelper: timeout : '+DirectoryName);
        Exit(False);
      end;

      if not Assigned(Resp) then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : CreateDirectoryHelper: without respond : '+DirectoryName);
        Exit(False);
      end;

      Result := Resp.Get('success', False);
    finally
      Resp.Free;
    end;
  finally
    Params.Free;
  end;
end;

function CreateNetworkDeviceHelper(const DeviceName: String; const VmName : String; Mtu : String = '1500'):Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
  IsTimedout: Boolean;
begin
  Params := TJSONObject.Create;

  try
    Params.Add('device', DeviceName);
    Params.Add('vmname', VmName);
    Params.Add('mtu', Mtu);

    Resp := ExecuteHelper('network.create_device', Params, 1000, IsTimedout);
    try
      if IsTimedout then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : CreateNetworkDeviceHelper: timeout : '+VmName);
        Exit(False);
      end;

      if not Assigned(Resp) then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : CreateNetworkDeviceHelper: without respond : '+VmName);
        Exit(False);
      end;

      Result := Resp.Get('success', False);
    finally
      Resp.Free;
    end;
  finally
    Params.Free;
  end;
end;

function DestroyNetworkInterfaceHelper(const IfName: String): Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
  IsTimedout: Boolean;
begin
  Params := TJSONObject.Create;

  try
    Params.Add('ifname', IfName);

    Resp := ExecuteHelper('network.destroy_device', Params, 1000, IsTimedout);
    try
      if IsTimedout then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : DestroyNetworkInterfaceHelper: timeout : '+IfName);
        Exit(False);
      end;

      if not Assigned(Resp) then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : DestroyNetworkInterfaceHelper: without respond : '+IfName);
        Exit(False);
      end;

      Result := Resp.Get('success', False);
    finally
      Resp.Free;
    end;
  finally
    Params.Free;
  end;
end;

function DestroyVirtualMachineHelper(const VmName: String): Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
  IsTimedout : Boolean;
begin
  Params := TJSONObject.Create;

  try
    Params.Add('vmname', VmName);

    Resp := ExecuteHelper('vm.destroy', Params, 5000, IsTimedout);
    try
      if IsTimedout then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : DestroyVirtualMachineHelper: timeout : '+VmName);
        Exit(False);
      end;

      if not Assigned(Resp) then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : DestroyVirtualMachineHelper: without respond : '+VmName);
        Exit(False);
      end;

      Result := Resp.Get('success', False);
    finally
      Resp.Free;
    end;
  finally
    Params.Free;
  end;
end;

function GetPIDValueHelper(const Pattern: String): Integer;
var
  Params: TJSONObject;
  Resp: TJSONObject;
  IsTimedout : Boolean;
begin
  Params := TJSONObject.Create;

  try
    Params.Add('pattern', Pattern);

    Resp := ExecuteHelper('process.get_pid', Params, 1000, IsTimedout);
    try
      if IsTimedout then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : GetPIDValueHelper: timeout : '+Pattern);
        Exit(-1);
      end;

      if not Assigned(Resp) then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : GetPIDValueHelper: without respond : '+Pattern);
        Exit(-1);
      end;

      Result:=StrToInt64(Resp.Get('pid', '-1'));
    finally
      Resp.Free;
    end;
  finally
    Params.Free;
  end;
end;

function KillPidHelper(Pid: Integer; Signal : String = '-TERM'): Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
  IsTimedout : Boolean;
begin
  Params := TJSONObject.Create;

  try
    Params.Add('signal', Signal);
    Params.Add('pid', Pid.ToString);

    Resp := ExecuteHelper('process.kill_pid', Params, 1000, IsTimedout);
    try
      if IsTimedout then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : KillPidHelper: timeout : '+Pid.ToString);
        Exit(False);
      end;

      if not Assigned(Resp) then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : KillPidHelper: without respond : '+Pid.ToString);
        Exit(False);
      end;

      Result := Resp.Get('success', False);
    finally
      Resp.Free;
    end;
  finally
    Params.Free;
  end;
end;

function PfLoadRulesHelper(const VmName : String; const RulesType : String):Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
  IsTimedout : Boolean;
begin
  Params := TJSONObject.Create;

  try
    Params.Add('vmname', VmName);
    Params.Add('ruletype', RulesType);

    Resp := ExecuteHelper('pf.load_rules', Params, 2000, IsTimedout);
    try
      if IsTimedout then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : PfLoadRulesHelper: timeout : '+VmName);
        Exit(False);
      end;

      if not Assigned(Resp) then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : PfLoadRulesHelper: without respond : '+VmName);
        Exit(False);
      end;

      Result := Resp.Get('success', False);
    finally
      Resp.Free;
    end;
  finally
    Params.Free;
  end;
end;

function PfUnloadRulesHelper(const VmName : String; const RulesType : String):Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
  IsTimedout : Boolean;
begin
  Params := TJSONObject.Create;

  try
    Params.Add('vmname', VmName);
    Params.Add('ruletype', RulesType);

    Resp := ExecuteHelper('pf.unload_rules', Params, 2000, IsTimedout);
    try
      if IsTimedout then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : PfUnloadRulesHelper: timeout : '+VmName);
        Exit(False);
      end;

      if not Assigned(Resp) then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : PfUnloadRulesHelper: without respond : '+VmName);
        Exit(False);
      end;

      Result := Resp.Get('success', False);
    finally
      Resp.Free;
    end;
  finally
    Params.Free;
  end;
end;

function RestartServiceHelper(const Service: String): Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
  IsTimedout : Boolean;
begin
  Params := TJSONObject.Create;

  try
    Params.Add('service', Service);

    Resp := ExecuteHelper('service.restart', Params, 2000, IsTimedout);

    try
      if IsTimedout then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : RestartServiceHelper: timeout : '+Service);
        Exit(False);
      end;

      if not Assigned(Resp) then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : RestartServiceHelper: without respond : '+Service);
        Exit(False);
      end;

      Result := Resp.Get('success', False);
    finally
      Resp.Free;
    end;
  finally
    Params.Free;
  end;
end;

function RemoveDirectoryHelper(const VmName : String; const DireType: String; Recursive: Boolean): Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
  IsTimedout : Boolean;
begin
  Params := TJSONObject.Create;

  try
    Params.Add('vmname', VmName);
    Params.Add('diretype', DireType);
    Params.Add('recursive', BoolToStr(Recursive, 'true', 'false'));

    Resp := ExecuteHelper('fs.rmdir', Params, 3000, IsTimedout);
    try
      if IsTimedout then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : RemoveDirectoryHelper: timeout : '+VmName);
        Exit(False);
      end;

      if not Assigned(Resp) then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : RemoveDirectoryHelper: without respond : '+VmName);
        Exit(False);
      end;

      Result := Resp.Get('success', False);
    finally
      Resp.Free;
    end;
  finally
    Params.Free;
  end;
end;

function ZfsCreateDatasetHelper(const ZfsType: String; const VmName : String; ZfsOptions : String; const WithMountpoint : Boolean = False): Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
  IsTimedout : Boolean;
begin
  Params := TJSONObject.Create;

  try
    Params.Add('vmname', VmName);
    Params.Add('zfstype', ZfsType);
    Params.Add('options', ZfsOptions);
    Params.Add('mountpoint', BoolToStr(WithMountpoint, 'true', 'false'));

    Resp := ExecuteHelper('zfs.create_dataset', Params, 50000, IsTimedout);
    try
      if IsTimedout then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : ZfsCreateDatasetHelper: timeout : '+VmName);
        Exit(False);
      end;

      if not Assigned(Resp) then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : ZfsCreateDatasetHelper: without respond : '+VmName);
        Exit(False);
      end;

      Result := Resp.Get('success', False);
    finally
      Resp.Free;
    end;
  finally
    Params.Free;
  end;
end;

function ZfsCreateZvolHelper(const VmName: String; const DiskName: String; ZvolSize : String; ZvolSparse : Boolean = False):Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
  IsTimedout : Boolean;
begin
  Params := TJSONObject.Create;

  try
    Params.Add('vmname', VmName);
    Params.Add('diskname', DiskName);
    Params.Add('volsize', ZvolSize);
    Params.Add('sparse', BoolToStr(ZvolSparse, 'true', 'false'));

    Resp := ExecuteHelper('zfs.create_zvol', Params, 5000, IsTimedout);
    try
      if IsTimedout then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : ZfsCreateZvolHelper: timeout : '+VmName);
        Exit(False);
      end;

      if not Assigned(Resp) then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : ZfsCreateZvolHelper: without respond : '+VmName);
        Exit(False);
      end;

      Result := Resp.Get('success', False);
    finally
      Resp.Free;
    end;
  finally
    Params.Free;
  end;
end;

function ZfsDestroyHelper(const VmName : String; const ZfsType: String; const ZfsDevice : String; Recursive : Boolean = True; Force : Boolean = False):Boolean;
var
  Params: TJSONObject;
  Resp: TJSONObject;
  IsTimedout : Boolean;
begin
  Params := TJSONObject.Create;

  try
    Params.Add('vmname', VmName);
    Params.Add('zfstype', ZfsType);
    Params.Add('zfsdevice', ZfsDevice);
    Params.Add('recursive', BoolToStr(Recursive, 'true', 'false'));
    Params.Add('force', BoolToStr(Force, 'true', 'false'));

    Resp := ExecuteHelper('zfs.destroy', Params, 5000, IsTimedout);
    try
      if IsTimedout then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : ZfsDestroyHelper: timeout : '+VmName);
        Exit(False);
      end;

      if not Assigned(Resp) then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : ZfsDestroyHelper: without respond : '+VmName);
        Exit(False);
      end;

      Result := Resp.Get('success', False);
    finally
      Resp.Free;
    end;
  finally
    Params.Free;
  end;
end;

function ZfsSetPropertyValueHelper(const ZfsPath : String; ZfsProperty : String; ZfsValue : String):String;
var
  Params: TJSONObject;
  Resp: TJSONObject;
  IsTimedout : Boolean;
begin
  Params := TJSONObject.Create;

  try
    Params.Add('path', ZfsPath);
    Params.Add('property', ZfsProperty);
    Params.Add('value', ZfsValue);

    Resp := ExecuteHelper('zfs.destroy', Params, 1000, IsTimedout);
    try
      if IsTimedout then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : ZfsSetPropertyValueHelperHelper: timeout : '+ZfsPath);
        Exit(EmptyStr);
      end;

      if not Assigned(Resp) then
      begin
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : ZfsSetPropertyValueHelperHelper: without respond : '+ZfsPath);
        Exit(EmptyStr);
      end;

      Result := Resp.Get('output', EmptyStr);
    finally
      Resp.Free;
    end;
  finally
    Params.Free;
  end;
end;

end.

