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

unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, FileUtil, Graphics, Dialogs, StdCtrls, Menus,
  ComCtrls, Buttons, RegExpr, StrUtils, unit_component, unit_device, unit_thread;

type

  { TFormBhyveManager }

  TFormBhyveManager = class(TForm)
    SpeedButtonAddVm: TSpeedButton;
    EditSystemType: TEdit;
    EditSystemVersion: TEdit;
    EditDescription: TEdit;
    SpeedButtonReloadVmConfig: TSpeedButton;
    SpeedButtonRemoveVm: TSpeedButton;
    SpeedButtonStartVm: TSpeedButton;
    SpeedButtonVncVm: TSpeedButton;
    SpeedButtonStopVm: TSpeedButton;
    VirtualMachinesGroupBox: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    MainMenu1: TMainMenu;
    MenuItemExit: TMenuItem;
    MenuItemSettings: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    SettingsPageControl: TPageControl;
    StatusBarBhyveManager: TStatusBar;
    GlobalSettingsTabSheet: TTabSheet;
    DeviceSettingsTabSheet: TTabSheet;
    DeviceSettingsTreeView: TTreeView;
    GlobalSettingsTreeView: TTreeView;
    VirtualMachinesTreeView: TTreeView;
    procedure DeviceSettingsTreeViewDeletion(Sender: TObject; Node: TTreeNode);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemSettingsClick(Sender: TObject);
    procedure SpeedButtonAddVmClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure DeviceSettingsTreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GlobalSettingsTreeViewClick(Sender: TObject);
    procedure GlobalSettingsTreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GlobalSettingsTreeViewSelectionChanged(Sender: TObject);
    procedure OpenFormGlobalChangeValue(Sender: TObject);
    procedure GlobalChangeValue(Sender: TObject);
    procedure AddDevice(Sender: TObject);
    procedure EditDevice(Sender: TObject);
    procedure EditVirtualMachineInfo(Sender: TObject);
    procedure RemoveDevice(Sender: TObject);
    procedure RemoteDesktopProtocolVm(Sender: TObject);
    procedure SaveAudioDevice(Sender: TObject);
    procedure SaveConsoleDevice(Sender: TObject);
    procedure SaveDisplayDevice(Sender: TObject);
    procedure SaveHostbridgeDevice(Sender: TObject);
    procedure SaveInputDevice(Sender: TObject);
    procedure SaveLpcDevice(Sender: TObject);
    procedure SaveNetworkDevice(Sender: TObject);
    procedure SavePassthruDevice(Sender: TObject);
    procedure SaveShareFolderDevice(Sender: TObject);
    procedure SaveStorageDevice(Sender: TObject);
    procedure CreateVmClick(Sender: TObject);
    procedure SaveVirtualMachineInfoClick(Sender: TObject);
    procedure SpeedButtonReloadVmConfigClick(Sender: TObject);
    procedure SpeedButtonRemoveVmClick(Sender: TObject);
    procedure SpeedButtonStartVmClick(Sender: TObject);
    procedure SpeedButtonStopVmClick(Sender: TObject);
    procedure SpeedButtonVncVmClick(Sender: TObject);
    procedure VirtualMachinesTreeViewDblClick(Sender: TObject);
    procedure VirtualMachinesTreeViewDeletion(Sender: TObject; Node: TTreeNode);
    procedure VirtualMachinesTreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure VirtualMachinesTreeViewSelectionChanged(Sender: TObject);
  private
    DevicesPopup : TDevicesPopupMenu;
    VirtualMachinesPopup : TVirtualMachinesPopupMenu;
    GlobalSettingsPopup : TGlobalSettingsPopupMenu;
    DeviceImageList : TDeviceImageList;
    SystemImageList : TSystemImageList;
    ActionImageList : TActionImageList;
    MyVmThread: VmThread;
    function FillDetailAudioDevice(Details : String; pci : String; device : String):TAudioDeviceClass;
    function FillDetailConsoleDevice(Details : String; pci : String; device : String; port : Integer):TSerialVirtioConsoleDeviceClass;
    function FillDetailDisplayDevice(Details : String; pci : String; device : String):TDisplayDeviceClass;
    function FillDetailHostbridgeDevice(Details : String; pci : String; device : String):THostbridgeDeviceClass;
    function FillDetailInputDevice(Details : String; pci : String; device : String):TVirtioInputDeviceClass;
    function FillDetailLpcDevice(Details : String; pci : String; device : String):TLPCDeviceClass;
    function FillDetailNetworkDevice(Details : String; pci : String; device : String):TNetworkDeviceClass;
    function FillDetailPassthruDevice(Details : String; pci : String; device : String):TPassthruDeviceClass;
    function FillDetailRngDevice(Details : String; pci : String; device : String):TRNGDeviceClass;
    function FillDetailShareFolderDevice(Details : String; pci : String; device : String):TShareFolderDeviceClass;
    function FillDetailStorageAhciDevice(Details : String; pci : String; device : String; port : Integer):TStorageAhciDeviceClass;
    function FillDetailStorageNvmeDevice(Details : String; pci : String; device : String):TStorageNvmeDeviceClass;
    function FillDetailStorageVirtioBlkDevice(Details : String; pci : String; device : String):TStorageVirtioBlkDeviceClass;
    function FillDetailUsbXhciDevice(Details : String; pci : String; device : String; slot : Integer):TUsbXhciDeviceClass;
    procedure FillGlobalCategoryList();
    procedure FillGlobalCategoryDetailList();
    procedure FillDeviceCategoryList();
    procedure FillDeviceDetailList();
    procedure FillVirtualMachineList();
    function LoadDeviceSettingsValues(VmName: String):Boolean;
    function LoadGlobalSettingsValues(VmName: String):Boolean;
    function LoadVirtualMachineData(ConfigPath: String):TVirtualMachineClass;
    function LoadNetworkDevice():TStringArray;
    procedure ResetTreeView(TreeView : TTreeView);
    function SaveVirtualMachineConfig():Boolean;
    procedure VirtualMachineShowStatus(Status: Integer; Message : String; VmName : String);
  public

  end;

var
  FormBhyveManager: TFormBhyveManager;
  AudioDevice: TAudioDeviceClass;
  HostBridgeDevice: THostbridgeDeviceClass;
  InputDevice: TVirtioInputDeviceClass;
  DisplayDevice: TDisplayDeviceClass;
  LPCDevice : TLPCDeviceClass;
  NetworkDevice : TNetworkDeviceClass;
  PassthruDevice : TPassthruDeviceClass;
  RNGDevice : TRNGDeviceClass;
  SerialVirtioConsoleDevice : TSerialVirtioConsoleDeviceClass;
  ShareFolderDevice : TShareFolderDeviceClass;
  StorageAhciDevice : TStorageAhciDeviceClass;
  StorageVirtioBlkDevice : TStorageVirtioBlkDeviceClass;
  StorageNvmeDevice : TStorageNvmeDeviceClass;
  UsbXhciDevice : TUsbXhciDeviceClass;
  VirtualMachine : TVirtualMachineClass;
  GlobalSettingTypeList : TStringList;
  GlobalSettingDefaultValueList : TStringList;
  GlobalSettingCategoryList : TStringList;
  NetworkDeviceList : TStringList;
  VirtualMachineList : TStringList;
  DevicesList : TStringList;
  TmpDevicesStringList : TStringList;
  NodeIndex : Integer;
  GlobalNode : TTreeNode;

implementation

{$R *.lfm}

uses
  form_about, form_audio_device, form_change_value, form_console_device, form_display_device,
  form_hostbridge_device, form_input_device, form_lpc_device, form_network_device, form_passthru_device,
  form_rdp_connection, form_share_folder_device, form_storage_device, form_settings, form_vm_create,
  form_vm_info, unit_configuration, unit_global, unit_util;

{ TFormBhyveManager }

procedure TFormBhyveManager.FormCreate(Sender: TObject);
begin
  FormSettings:= TFormSettings.Create(FormBhyveManager);
  FormAbout:= TFormAbout.Create(FormBhyveManager);
  FormChangeValue:= TFormChangeValue.Create(FormBhyveManager);
  FormVMCreate:=TFormVmCreate.Create(FormBhyveManager);
  FormVmCreate.Caption:=FormBhyveManagerCreateVmTitle;
  FormVmInfo:=TFormVmInfo.Create(FormBhyveManager);
  FormVmInfo.Caption:=FormBhyveManagerEditVmInfoTitle;
  FormRdpConnection:=TFormRdpConnection.Create(FormBhyveManager);

  FormBhyveManager.Caption:=FormBhyveManagerTitle;
  SettingsPageControl.TabIndex:=0;

  SystemImageList:=TSystemImageList.Create(FormBhyveManager);
  DeviceImageList:=TDeviceImageList.Create(FormBhyveManager);
  ActionImageList:=TActionImageList.Create(FormBhyveManager);

  // Virtual Machine Popup Menu
  VirtualMachinesPopup:= TVirtualMachinesPopupMenu.Create(FormBhyveManager);
  VirtualMachinesPopup.PopupMenu.Images:=ActionImageList.ActionList;

  // Virtual Machine Popup Menu - Add, Modify and Remove
  VirtualMachinesPopup.PopupMenu.Items[0].ImageIndex:=0;
  VirtualMachinesPopup.PopupMenu.Items[1].ImageIndex:=1;
  VirtualMachinesPopup.PopupMenu.Items[2].ImageIndex:=2;
  VirtualMachinesPopup.PopupMenu.Items[3].ImageIndex:=3;

  VirtualMachinesPopup.PopupMenu.Items[0].OnClick:=@SpeedButtonAddVmClick;
  VirtualMachinesPopup.PopupMenu.Items[1].OnClick:=@EditVirtualMachineInfo;
  VirtualMachinesPopup.PopupMenu.Items[2].OnClick:=@SpeedButtonRemoveVmClick;
  VirtualMachinesPopup.PopupMenu.Items[3].OnClick:=@RemoteDesktopProtocolVm;

  // Devices Popup Menu
  DevicesPopup:= TDevicesPopupMenu.Create(FormBhyveManager);
  DevicesPopup.PopupMenu.Images:=ActionImageList.ActionList;

  // Devices Popup Menu - Add, Modify and Remove
  DevicesPopup.PopupMenu.Items[0].ImageIndex:=0;
  DevicesPopup.PopupMenu.Items[1].ImageIndex:=1;
  DevicesPopup.PopupMenu.Items[2].ImageIndex:=2;

  DevicesPopup.PopupMenu.Items[0].OnClick:=@AddDevice;
  DevicesPopup.PopupMenu.Items[1].OnClick:=@EditDevice;
  DevicesPopup.PopupMenu.Items[2].OnClick:=@RemoveDevice;

  // Global Settings Popup Menu
  GlobalSettingsPopup:= TGlobalSettingsPopupMenu.Create(FormBhyveManager);
  GlobalSettingsPopup.PopupMenu.Images:=ActionImageList.ActionList;

  GlobalSettingsPopup.PopupMenu.Items[0].ImageIndex:=1;
  GlobalSettingsPopup.PopupMenu.Items[0].OnClick:=@OpenFormGlobalChangeValue;

  // Assign Devices Popup Menu to TreeView
  VirtualMachinesTreeView.PopupMenu:=DevicesPopup.PopupMenu;
  DeviceSettingsTreeView.PopupMenu:=DevicesPopup.PopupMenu;

  // Assign Image Lists to TreeViews
  VirtualMachinesTreeView.Images:=SystemImageList.SystemList;
  GlobalSettingsTreeView.Images:=DeviceImageList.DeviceList;
  DeviceSettingsTreeView.Images:=DeviceImageList.DeviceList;

  GlobalSettingDefaultValueList := TStringList.Create;
  GlobalSettingTypeList := TStringList.Create;
  GlobalSettingCategoryList := TStringList.Create;
  DevicesList := TStringList.Create;
  NetworkDeviceList := TStringList.Create;
  VirtualMachineList := TStringList.Create;

  TmpDevicesStringList := TStringList.Create;

  FillVirtualMachineList();
  FillDeviceCategoryList();
  FillGlobalCategoryList();
  FillDeviceDetailList();
end;

procedure TFormBhyveManager.DeviceSettingsTreeViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbRight) and (Assigned(DeviceSettingsTreeView.Selected)) and (Assigned(VirtualMachinesTreeView.Selected)) and (VirtualMachinesTreeView.Selected.Level <> 0) then
   begin
     if (DeviceSettingsTreeView.Selected.Level = 0) then
      begin
        DevicesPopup.PopupMenu.Items.Items[1].Enabled:=False;
        DevicesPopup.PopupMenu.Items.Items[2].Enabled:=False;
        DevicesPopup.PopupMenu.PopUp;
      end
      else if (DeviceSettingsTreeView.Selected.Level = 1) then
      begin
        DevicesPopup.PopupMenu.Items.Items[1].Enabled:=True;
        DevicesPopup.PopupMenu.Items.Items[2].Enabled:=True;
        DevicesPopup.PopupMenu.PopUp;
      end;
   end;
end;

procedure TFormBhyveManager.GlobalSettingsTreeViewClick(Sender: TObject);
begin
  if (Assigned(GlobalSettingsTreeView.Selected)) and (GlobalSettingsTreeView.Selected.Level <> 0) then
    StatusBarBhyveManager.SimpleText:=GlobalSettingsTreeView.Selected.Text
  else
    StatusBarBhyveManager.SimpleText:=EmptyStr;
end;

procedure TFormBhyveManager.GlobalSettingsTreeViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbRight) and (Assigned(GlobalSettingsTreeView.Selected)) and (GlobalSettingsTreeView.Selected.Level <> 0) then
   begin
      GlobalSettingsPopup.PopupMenu.PopUp;
   end;
end;

procedure TFormBhyveManager.GlobalSettingsTreeViewSelectionChanged(Sender: TObject);
begin
  if Assigned(GlobalSettingsTreeView.Selected) and (GlobalSettingsTreeView.Selected.Level = 1) then
    StatusBarBhyveManager.SimpleText:=GlobalSettingsTreeView.Selected.Text
  else
    StatusBarBhyveManager.SimpleText:=EmptyStr;
end;

procedure TFormBhyveManager.FillGlobalCategoryList();
var
  i: Integer;
  GlobalCategoryList: TStringList;
begin
  GlobalCategoryList := TStringList.Create;

  GlobalCategoryList.Add('System');
  GlobalCategoryList.Add('Processor');
  GlobalCategoryList.Add('Memory');
  GlobalCategoryList.Add('ACPI');
  GlobalCategoryList.Add('Debugging');
  GlobalCategoryList.Add('TPM');
  GlobalCategoryList.Add('x86');

  GlobalSettingsTreeView.Items.Clear;

  for i:=0 to GlobalCategoryList.Count-1 do
  begin
    GlobalNode:=GlobalSettingsTreeView.Items.Add(Nil, GlobalCategoryList.ValueFromIndex[i]);
    GlobalNode.ImageIndex:=0;
    GlobalNode.SelectedIndex:=0;
  end;

  GlobalCategoryList.Free;
end;

procedure TFormBhyveManager.FillGlobalCategoryDetailList();
begin
  { Remove when bhyve will updated on FreeBSD 13.x and 14.x }
  if GetOsreldate.ToInt64 >= 1500023 then
  begin
    GlobalSettingDefaultValueList.Values['bootrom'] := EmptyStr;
    GlobalSettingDefaultValueList.Values['bootvars'] := EmptyStr;
  end;
  GlobalSettingDefaultValueList.Values['cpus'] := '1';
  GlobalSettingDefaultValueList.Values['cores'] := '1';
  GlobalSettingDefaultValueList.Values['threads'] := '1';
  GlobalSettingDefaultValueList.Values['sockets'] := '1';
  GlobalSettingDefaultValueList.Values['memory.guest_in_core'] := 'false';
  GlobalSettingDefaultValueList.Values['memory.size'] := '256M';
  GlobalSettingDefaultValueList.Values['memory.wired'] := 'false';
  GlobalSettingDefaultValueList.Values['acpi_tables'] := 'true';
  GlobalSettingDefaultValueList.Values['acpi_tables_in_memory'] := 'true';
  GlobalSettingDefaultValueList.Values['destroy_on_poweroff'] := 'false';
  GlobalSettingDefaultValueList.Values['gdb.address'] := 'localhost';
  GlobalSettingDefaultValueList.Values['gdb.port'] := '0';
  GlobalSettingDefaultValueList.Values['gdb.wait'] := 'false';
  GlobalSettingDefaultValueList.Values['keyboard.layout'] := EmptyStr;
  GlobalSettingDefaultValueList.Values['tpm.path'] := EmptyStr;
  GlobalSettingDefaultValueList.Values['tpm.type'] := EmptyStr;
  GlobalSettingDefaultValueList.Values['tpm.version'] := EmptyStr;
  GlobalSettingDefaultValueList.Values['rtc.use_localtime'] := 'true';
  GlobalSettingDefaultValueList.Values['uuid'] := EmptyStr;
  GlobalSettingDefaultValueList.Values['virtio_msix'] := 'true';
  GlobalSettingDefaultValueList.Values['x86.mptable'] := 'true';
  GlobalSettingDefaultValueList.Values['x86.x2apic'] := 'false';
  GlobalSettingDefaultValueList.Values['x86.strictio'] := 'false';
  GlobalSettingDefaultValueList.Values['x86.strictmsr'] := 'true';
  GlobalSettingDefaultValueList.Values['x86.vmexit_on_hlt'] := 'false';
  GlobalSettingDefaultValueList.Values['x86.vmexit_on_pause'] := 'false';

  { Remove when bhyve will updated on FreeBSD 13.x and 14.x }
  if GetOsreldate.ToInt64 >= 1500023 then
  begin
    GlobalSettingTypeList.Values['bootrom'] := 'String';
    GlobalSettingTypeList.Values['bootvars'] := 'String';
  end;
  GlobalSettingTypeList.Values['cpus'] := 'Integer';
  GlobalSettingTypeList.Values['cores'] := 'Integer';
  GlobalSettingTypeList.Values['threads'] := 'Integer';
  GlobalSettingTypeList.Values['sockets'] := 'Integer';
  GlobalSettingTypeList.Values['memory.guest_in_core'] := 'Boolean';
  GlobalSettingTypeList.Values['memory.size'] := 'String';
  GlobalSettingTypeList.Values['memory.wired'] := 'Boolean';
  GlobalSettingTypeList.Values['acpi_tables'] := 'Boolean';
  GlobalSettingTypeList.Values['acpi_tables_in_memory'] := 'Boolean';
  GlobalSettingTypeList.Values['destroy_on_poweroff'] := 'Boolean';
  GlobalSettingTypeList.Values['gdb.address'] := 'String';
  GlobalSettingTypeList.Values['gdb.port'] := 'Integer';
  GlobalSettingTypeList.Values['gdb.wait'] := 'Boolean';
  GlobalSettingTypeList.Values['keyboard.layout'] := 'String';
  GlobalSettingTypeList.Values['tpm.path'] := 'String';
  GlobalSettingTypeList.Values['tpm.type'] := 'String';
  GlobalSettingTypeList.Values['tpm.version'] := 'String';
  GlobalSettingTypeList.Values['rtc.use_localtime'] := 'Boolean';
  GlobalSettingTypeList.Values['uuid'] := 'String';
  GlobalSettingTypeList.Values['virtio_msix'] := 'Boolean';
  GlobalSettingTypeList.Values['x86.mptable'] := 'Boolean';
  GlobalSettingTypeList.Values['x86.x2apic'] := 'Boolean';
  GlobalSettingTypeList.Values['x86.strictio'] := 'Boolean';
  GlobalSettingTypeList.Values['x86.strictmsr'] := 'Boolean';
  GlobalSettingTypeList.Values['x86.vmexit_on_hlt'] := 'Boolean';
  GlobalSettingTypeList.Values['x86.vmexit_on_pause'] := 'Boolean';

  { Remove when bhyve will updated on FreeBSD 13.x and 14.x }
  if GetOsreldate.ToInt64 >= 1500023 then
  begin
    GlobalSettingCategoryList.Values['bootrom'] := 'System';
    GlobalSettingCategoryList.Values['bootvars'] := 'System';
  end;
  GlobalSettingCategoryList.Values['destroy_on_poweroff'] := 'System';
  GlobalSettingCategoryList.Values['keyboard.layout'] := 'System';
  GlobalSettingCategoryList.Values['rtc.use_localtime'] := 'System';
  GlobalSettingCategoryList.Values['virtio_msix'] := 'System';
  GlobalSettingCategoryList.Values['uuid'] := 'System';
  GlobalSettingCategoryList.Values['cpus'] := 'Processor';
  GlobalSettingCategoryList.Values['cores'] := 'Processor';
  GlobalSettingCategoryList.Values['threads'] := 'Processor';
  GlobalSettingCategoryList.Values['sockets'] := 'Processor';
  GlobalSettingCategoryList.Values['memory.guest_in_core'] := 'Memory';
  GlobalSettingCategoryList.Values['memory.size'] := 'Memory';
  GlobalSettingCategoryList.Values['memory.wired'] := 'Memory';
  GlobalSettingCategoryList.Values['acpi_tables'] := 'ACPI';
  GlobalSettingCategoryList.Values['acpi_tables_in_memory'] := 'ACPI';
  GlobalSettingCategoryList.Values['gdb.address'] := 'Debugging';
  GlobalSettingCategoryList.Values['gdb.port'] := 'Debugging';
  GlobalSettingCategoryList.Values['gdb.wait'] := 'Debugging';
  GlobalSettingCategoryList.Values['tpm.path'] := 'TPM';
  GlobalSettingCategoryList.Values['tpm.type'] := 'TPM';
  GlobalSettingCategoryList.Values['tpm.version'] := 'TPM';
  GlobalSettingCategoryList.Values['x86.mptable'] := 'x86';
  GlobalSettingCategoryList.Values['x86.x2apic'] := 'x86';
  GlobalSettingCategoryList.Values['x86.strictio'] := 'x86';
  GlobalSettingCategoryList.Values['x86.strictmsr'] := 'x86';
  GlobalSettingCategoryList.Values['x86.vmexit_on_hlt'] := 'x86';
  GlobalSettingCategoryList.Values['x86.vmexit_on_pause'] := 'x86';
end;

procedure TFormBhyveManager.FillDeviceCategoryList();
var
  i: Integer;
  DevicesCategoryList: TStringList;
begin
  DevicesCategoryList := TStringList.Create;

  DevicesCategoryList.Add('Audio');
  DevicesCategoryList.Add('Console');
  DevicesCategoryList.Add('Display');
  DevicesCategoryList.Add('Hostbridge');
  DevicesCategoryList.Add('Input');
  DevicesCategoryList.Add('LPC');
  DevicesCategoryList.Add('Network');
  DevicesCategoryList.Add('Passthru');
  DevicesCategoryList.Add('RNG');
  DevicesCategoryList.Add('Shared folders');
  DevicesCategoryList.Add('Storage');
  DevicesCategoryList.Add('USB');

  DeviceSettingsTreeView.Items.Clear;

  for i:=0 to DevicesCategoryList.Count-1 do
  begin
    GlobalNode:=DeviceSettingsTreeView.Items.Add(Nil, DevicesCategoryList.ValueFromIndex[i]);
    GlobalNode.ImageIndex:=0;
    GlobalNode.SelectedIndex:=0;
  end;

  DevicesCategoryList.Free;
end;

procedure TFormBhyveManager.FillDeviceDetailList();
begin
  DevicesList.Values['hda'] := 'Audio';
  DevicesList.Values['fbuf'] := 'Display';
  DevicesList.Values['amdhostbridge'] := 'Hostbridge';
  DevicesList.Values['hostbridge'] := 'Hostbridge';
  DevicesList.Values['virtio-input'] := 'Input';
  DevicesList.Values['lpc'] := 'LPC';
  DevicesList.Values['virtio-net'] := 'Network';
  DevicesList.Values['e1000'] := 'Network';
  DevicesList.Values['passthru'] := 'Passthru';
  DevicesList.Values['virtio-rnd'] := 'RNG';
  DevicesList.Values['uart'] := 'Serial ports';
  DevicesList.Values['virtio-console'] := 'Console';
  DevicesList.Values['virtio-9p'] := 'Shared folders';
  DevicesList.Values['ahci'] := 'Storage';
  DevicesList.Values['nvme'] := 'Storage';
  DevicesList.Values['virtio-blk'] := 'Storage';
  DevicesList.Values['virtual-scsi'] := 'Storage';
  DevicesList.Values['xhci'] := 'USB';
end;

procedure TFormBhyveManager.FillVirtualMachineList();
var
  i : integer;
  Directories : TStringList;
  Nodo : TTreeNode;
begin
  Directories:=FindAllDirectories(VmPath, False);
  Directories.Sorted:=True;

  for i:=0 to Directories.Count-1 do
  begin
    if FileExists(Directories[i]+'/bhyve_config.conf') and FileExists(Directories[i]+'/'+ExtractFileName(Directories[i])+'.conf') then
      begin
        VirtualMachine:=LoadVirtualMachineData(Directories[i]+'/'+ExtractFileName(Directories[i])+'.conf');

        if VirtualMachinesTreeView.Items.FindNodeWithText(VirtualMachine.system_type) = Nil then
          begin
            Nodo := VirtualMachinesTreeView.Items.Add(Nil, VirtualMachine.system_type);
            Nodo.ImageIndex:=0;
            Nodo.SelectedIndex:=0;
          end;

        if CheckVmRunning(VirtualMachine.name) > 0 then
          GlobalNode:=VirtualMachinesTreeView.Items.AddChild(VirtualMachinesTreeView.Items.FindNodeWithText(VirtualMachine.system_type), VirtualMachine.name + ' : Running')
        else
          GlobalNode:=VirtualMachinesTreeView.Items.AddChild(VirtualMachinesTreeView.Items.FindNodeWithText(VirtualMachine.system_type), VirtualMachine.name);

        GlobalNode.ImageIndex:=VirtualMachine.image;
        GlobalNode.SelectedIndex:=VirtualMachine.image;
        GlobalNode.Data:=VirtualMachine;
      end;
  end;

  Directories.Free;
end;

procedure TFormBhyveManager.ResetTreeView(TreeView: TTreeView);
var
  i : Integer;
  j : Integer;
begin
  for i:=0 to TreeView.Items.TopLvlCount-1 do
  begin
    if (TreeView.Items.TopLvlItems[i].Count > 0) then
    begin
      for j:=TreeView.Items.TopLvlItems[i].Count-1 downto 0 do
      begin
        TObject(TreeView.Items.TopLvlItems[i].Items[j].Data).Free;
        TreeView.Items.TopLvlItems[i].Items[j].Data:=Nil;
        TreeView.Items.TopLvlItems[i].Items[j].Delete;
      end;
    end;
  end;
end;

function TFormBhyveManager.SaveVirtualMachineConfig(): Boolean;
var
  TmpGlobalSettingList : TStringList;
  i : Integer;
  j : Integer;
begin
  Result:=False;

  TmpGlobalSettingList:= TStringList.Create;

  TmpGlobalSettingList.Values['name']:=TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).name;

  for i:=0 to GlobalSettingsTreeView.Items.TopLvlCount-1 do
  begin
    for j:=0 to GlobalSettingsTreeView.Items.TopLvlItems[i].Count-1 do
    begin
      if GlobalSettingDefaultValueList.Values[ExtractVarName(GlobalSettingsTreeView.Items.TopLvlItems[i].Items[j].Text)] <> ExtractVarValue(GlobalSettingsTreeView.Items.TopLvlItems[i].Items[j].Text) then
      begin
        TmpGlobalSettingList.Values[ExtractVarName(GlobalSettingsTreeView.Items.TopLvlItems[i].Items[j].Text)] := ExtractVarValue(GlobalSettingsTreeView.Items.TopLvlItems[i].Items[j].Text);
      end;
    end;
  end;

 TmpGlobalSettingList.Sorted:=True;
 TmpDevicesStringList.Sorted:=True;


 GlobalNode:=VirtualMachinesTreeView.Selected;
 VirtualMachine := TVirtualMachineClass(GlobalNode.Data);

 TmpGlobalSettingList.Text:=TmpGlobalSettingList.Text + TmpDevicesStringList.Text;

 CreateFile(VmPath+'/'+VirtualMachine.name+'/bhyve_config.conf', GetCurrentUserName());

 try
   TmpGlobalSettingList.SaveToFile(VmPath+'/'+VirtualMachine.name+'/bhyve_config.conf');
   Result:=True;
 finally
   TmpDevicesStringList.Sorted:=False;
   TmpGlobalSettingList.Free;
 end;
end;

procedure TFormBhyveManager.VirtualMachineShowStatus(Status: Integer;
  Message: String; VmName : String);
var
  i : Integer;
begin
  if Status = 0 then
  begin
    StatusBarBhyveManager.Font.Color:=clTeal;
    StatusBarBhyveManager.SimpleText := Message;

    MyVmThread := VmThread.Create(VmName);
    MyVmThread.OnExitStatus := @VirtualMachineShowStatus;
    MyVmThread.Start;
  end
  else if Status = 1 then
  begin
    for i:=NetworkDeviceList.Count-1 downto 0 do
    begin
      if NetworkDeviceList.ValueFromIndex[i] = VmName then
      begin
          DestroyNetworkInterface(NetworkDeviceList.Names[i]);
          NetworkDeviceList.Delete(i);
      end;
    end;

    SpeedButtonVncVm.Enabled:=False;
    SpeedButtonRemoveVm.Enabled:=True;
    SpeedButtonStartVm.Enabled:=True;
    SpeedButtonStopVm.Enabled:=False;
    SpeedButtonReloadVmConfig.Enabled:=True;

    if Assigned(VirtualMachinesTreeView.Items.FindNodeWithText(VmName+' : Running')) then
      VirtualMachinesTreeView.Items.FindNodeWithText(VmName+' : Running').Text:=VmName;

    DestroyVirtualMachine(VmName);
    RemoveDirectory(VmName+'/vtcon', True);

    StatusBarBhyveManager.Font.Color:=clTeal;
    StatusBarBhyveManager.SimpleText := Message;
  end
  else if Status = 2 then
  begin
    StatusBarBhyveManager.Font.Color:=clTeal;
    StatusBarBhyveManager.SimpleText := Message;
  end
  else if Status > 2 then
  begin
    SpeedButtonVncVm.Enabled:=False;
    SpeedButtonStopVm.Enabled:=False;
    SpeedButtonStartVm.Enabled:=True;
    SpeedButtonRemoveVm.Enabled:=True;

    if Assigned(VirtualMachinesTreeView.Items.FindNodeWithText(VmName+' : Running')) then
      VirtualMachinesTreeView.Items.FindNodeWithText(VmName+' : Running').Text:=VmName;

    if (Status = 4) or (Status = 6) then
    begin
      for i:=NetworkDeviceList.Count-1 downto 0 do
      begin
        if NetworkDeviceList.ValueFromIndex[i] = VmName then
        begin
            DestroyNetworkInterface(NetworkDeviceList.Names[i]);
            NetworkDeviceList.Delete(i);
        end;
      end;
      DestroyVirtualMachine(VmName);
      RemoveDirectory(VmName+'/vtcon', True);
    end;

    StatusBarBhyveManager.SimpleText := EmptyStr;
    MessageDlg('Error message', Message, mtError, [mbOK], 0);
    MyVmThread.Terminate;
  end;
end;

procedure TFormBhyveManager.GlobalChangeValue(Sender: TObject);
begin
  if (FormChangeValue.SettingType = 'memory.size') then
    StatusBarBhyveManager.SimpleText:='Value has been changed to '+FormChangeValue.SpinEditExValue.Text+'M'
  else
    StatusBarBhyveManager.SimpleText:='Value has been changed to '+FormChangeValue.ComboBoxValue.Text;

  if (FormChangeValue.SettingType = 'bootrom') or (FormChangeValue.SettingType = 'bootvars') then
    GlobalSettingsTreeView.Items.Item[NodeIndex].Text:=ExtractVarName(GlobalSettingsTreeView.Selected.Text)+' : '+BootRomUefiPath+'/'+FormChangeValue.ComboBoxValue.Text
  else if (FormChangeValue.SettingType = 'memory.size') then
    GlobalSettingsTreeView.Items.Item[NodeIndex].Text:=ExtractVarName(GlobalSettingsTreeView.Selected.Text)+' : '+FormChangeValue.SpinEditExValue.Text+'M'
  else
    GlobalSettingsTreeView.Items.Item[NodeIndex].Text:=ExtractVarName(GlobalSettingsTreeView.Selected.Text)+' : '+FormChangeValue.ComboBoxValue.Text;

  SaveVirtualMachineConfig();

  FormChangeValue.Close;
end;

procedure TFormBhyveManager.OpenFormGlobalChangeValue(Sender: TObject);
var
  SettingName : String;
begin
  if GlobalSettingTypeList.Values[extractVarName(GlobalSettingsTreeView.Selected.Text)] = 'Boolean' then
  begin
    SettingName:=extractVarName(GlobalSettingsTreeView.Selected.Text);
    NodeIndex:=GlobalSettingsTreeView.Selected.AbsoluteIndex;
    FormChangeValue.ShowComboBox();
    FormChangeValue.ComboBoxValue.Clear;
    FormChangeValue.SettingType:=SettingName;
    FillComboBooleanType(FormChangeValue.ComboBoxValue);
    FormChangeValue.ComboBoxValue.ItemIndex:=FormChangeValue.ComboBoxValue.Items.IndexOf(extractVarValue(GlobalSettingsTreeView.Selected.Text));
    FormChangeValue.Caption:='Editing '+extractVarName(GlobalSettingsTreeView.Selected.Text);
    FormChangeValue.BitBtnSave.OnClick:=@GlobalChangeValue;
    FormChangeValue.Visible:=True;
  end;

  if GlobalSettingTypeList.Values[extractVarName(GlobalSettingsTreeView.Selected.Text)] = 'Integer' then
  begin
    SettingName:=extractVarName(GlobalSettingsTreeView.Selected.Text);

    if (SettingName = 'cpus') or (SettingName = 'cores') or (SettingName = 'threads') or (SettingName = 'sockets')  then
    begin
      NodeIndex:=GlobalSettingsTreeView.Selected.AbsoluteIndex;

      FormChangeValue.ShowComboBox();
      FormChangeValue.ComboBoxValue.Clear;
      FormChangeValue.SettingType:=SettingName;
      FillComboIntegerType(FormChangeValue.ComboBoxValue, 1, Trim(CheckSysctl('hw.vmm.maxcpu')).ToInteger, 1);
      FormChangeValue.ComboBoxValue.ItemIndex:=FormChangeValue.ComboBoxValue.Items.IndexOf(extractVarValue(GlobalSettingsTreeView.Selected.Text));
      FormChangeValue.Caption:='Editing '+extractVarName(GlobalSettingsTreeView.Selected.Text);
      FormChangeValue.BitBtnSave.OnClick:=@GlobalChangeValue;
      FormChangeValue.Visible:=True;
    end;

    if (SettingName = 'gdb.port')then
    begin
      NodeIndex:=GlobalSettingsTreeView.Selected.AbsoluteIndex;

      FormChangeValue.ShowComboBox();
      FormChangeValue.ComboBoxValue.Clear;
      FormChangeValue.SettingType:=SettingName;
      FormChangeValue.ComboBoxValue.Items.Add('0');
      FillComboIntegerType(FormChangeValue.ComboBoxValue, 60000, 60100, 1);
      FormChangeValue.ComboBoxValue.ItemIndex:=FormChangeValue.ComboBoxValue.Items.IndexOf(extractVarValue(GlobalSettingsTreeView.Selected.Text));
      FormChangeValue.Caption:='Editing '+extractVarName(GlobalSettingsTreeView.Selected.Text);
      FormChangeValue.BitBtnSave.OnClick:=@GlobalChangeValue;
      FormChangeValue.Visible:=True;
    end;
  end;

  if GlobalSettingTypeList.Values[extractVarName(GlobalSettingsTreeView.Selected.Text)] = 'String' then
  begin
    SettingName:=extractVarName(GlobalSettingsTreeView.Selected.Text);

    if SettingName = 'bootrom' then
    begin
      NodeIndex:=GlobalSettingsTreeView.Selected.AbsoluteIndex;

      FormChangeValue.ShowComboBox();
      FormChangeValue.ComboBoxValue.Clear;
      FormChangeValue.SettingType:=SettingName;
      FillComboBootrom(FormChangeValue.ComboBoxValue);
      FormChangeValue.ComboBoxValue.ItemIndex:=FormChangeValue.ComboBoxValue.Items.IndexOf(ExtractFileName(extractVarValue(GlobalSettingsTreeView.Selected.Text)));
      FormChangeValue.Caption:='Editing '+extractVarName(GlobalSettingsTreeView.Selected.Text);
      FormChangeValue.BitBtnSave.OnClick:=@GlobalChangeValue;
      FormChangeValue.Visible:=True;
    end;

    if SettingName = 'keyboard.layout' then
    begin
      NodeIndex:=GlobalSettingsTreeView.Selected.AbsoluteIndex;

      FormChangeValue.ShowComboBox();
      FormChangeValue.ComboBoxValue.Clear;
      FormChangeValue.SettingType:=SettingName;
      FillComboKeyboardLayout(FormChangeValue.ComboBoxValue);
      FormChangeValue.ComboBoxValue.ItemIndex:=FormChangeValue.ComboBoxValue.Items.IndexOf(extractVarValue(GlobalSettingsTreeView.Selected.Text));
      FormChangeValue.Caption:='Editing '+extractVarName(GlobalSettingsTreeView.Selected.Text);
      FormChangeValue.BitBtnSave.OnClick:=@GlobalChangeValue;
      FormChangeValue.Visible:=True;
    end;

    if SettingName = 'tpm.path' then
    begin
      NodeIndex:=GlobalSettingsTreeView.Selected.AbsoluteIndex;

      FormChangeValue.ShowComboBox();
      FormChangeValue.ComboBoxValue.Clear;
      FormChangeValue.SettingType:=SettingName;
      FillComboTpmDevice(FormChangeValue.ComboBoxValue);
      FormChangeValue.ComboBoxValue.ItemIndex:=FormChangeValue.ComboBoxValue.Items.IndexOf(extractVarValue(GlobalSettingsTreeView.Selected.Text));
      FormChangeValue.Caption:='Editing '+extractVarName(GlobalSettingsTreeView.Selected.Text);
      FormChangeValue.BitBtnSave.OnClick:=@GlobalChangeValue;
      FormChangeValue.Visible:=True;
    end;

    if SettingName = 'tpm.type' then
    begin
      NodeIndex:=GlobalSettingsTreeView.Selected.AbsoluteIndex;

      FormChangeValue.ShowComboBox();
      FormChangeValue.ComboBoxValue.Clear;
      FormChangeValue.SettingType:=SettingName;
      FillComboTpmType(FormChangeValue.ComboBoxValue);
      FormChangeValue.ComboBoxValue.ItemIndex:=FormChangeValue.ComboBoxValue.Items.IndexOf(extractVarValue(GlobalSettingsTreeView.Selected.Text));
      FormChangeValue.Caption:='Editing '+extractVarName(GlobalSettingsTreeView.Selected.Text);
      FormChangeValue.BitBtnSave.OnClick:=@GlobalChangeValue;
      FormChangeValue.Visible:=True;
    end;

    if SettingName = 'tpm.version' then
    begin
      NodeIndex:=GlobalSettingsTreeView.Selected.AbsoluteIndex;

      FormChangeValue.ShowComboBox();
      FormChangeValue.ComboBoxValue.Clear;
      FormChangeValue.SettingType:=SettingName;
      FillComboTpmVersion(FormChangeValue.ComboBoxValue);
      FormChangeValue.ComboBoxValue.ItemIndex:=FormChangeValue.ComboBoxValue.Items.IndexOf(extractVarValue(GlobalSettingsTreeView.Selected.Text));
      FormChangeValue.Caption:='Editing '+extractVarName(GlobalSettingsTreeView.Selected.Text);
      FormChangeValue.BitBtnSave.OnClick:=@GlobalChangeValue;
      FormChangeValue.Visible:=True;
    end;

    if SettingName = 'memory.size' then
    begin
      NodeIndex:=GlobalSettingsTreeView.Selected.AbsoluteIndex;

      FormChangeValue.ComboBoxValue.Clear;
      FormChangeValue.SettingType:=SettingName;
      FormChangeValue.ShowSpinEx(256, Trim(CheckSysctl('hw.usermem')).ToInt64, 256);
      FormChangeValue.SpinEditExValue.Value:=extractVarValue(GlobalSettingsTreeView.Selected.Text).Replace('M', EmptyStr).ToInt64;
      FormChangeValue.Caption:='Editing '+extractVarName(GlobalSettingsTreeView.Selected.Text)+' in MB';
      FormChangeValue.BitBtnSave.OnClick:=@GlobalChangeValue;
      FormChangeValue.Visible:=True;
    end;
  end;
end;

procedure TFormBhyveManager.SpeedButtonAddVmClick(Sender: TObject);
begin
  FormVmCreate.BitBtnCreateVm.OnClick:=@CreateVmClick;
  FormVmCreate.Visible:=True;
end;

procedure TFormBhyveManager.DeviceSettingsTreeViewDeletion(Sender: TObject;
  Node: TTreeNode);
begin
  if assigned(TObject(Node.Data)) then
  begin
    TObject(Node.Data).Free;
  end;
end;

procedure TFormBhyveManager.FormActivate(Sender: TObject);
begin
  if NewConfig then
  begin
    MessageDlg('A config file was generated, but the config file itself may not follow your own needs. A settings form will be open. Review it a press Save settings button if everything is ok.', mtInformation, [mbOk], 0);
    FormSettings.Show;
  end;
end;

procedure TFormBhyveManager.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  i, j : Integer;
  flag : Boolean;
  CloseMessage : String;
  Node : TTreeNode;
begin
  CloseMessage := 'bhyvemgr detects VMs running. You must stop them before of close this app.'+sLineBreak+sLineBreak+'Do you really want to close?';

  flag:=False;

  for i:=0 to VirtualMachinesTreeView.Items.TopLvlCount-1 do
  begin
    if flag then
      Break;

    for j:=0 to VirtualMachinesTreeView.Items.TopLvlItems[i].Count-1 do
    begin
      Node:=VirtualMachinesTreeView.Items.TopLvlItems[i].Items[j];
{      VirtualMachine := TVirtualMachineClass(node.Data);
      if CheckVmRunning(VirtualMachine.name) > 0 then }
      if ExtractVarValue(Node.Text) = 'Running' then
      begin
        flag:=True;
        Break;
      end
    end;
  end;

  if flag then
  begin
    if MessageDlg(CloseMessage, mtConfirmation, [mbOk, mbCancel], 0) = mrCancel then
      CanClose := false;
  end;
end;

procedure TFormBhyveManager.MenuItemAboutClick(Sender: TObject);
begin
  FormAbout.Visible:=True;
end;

procedure TFormBhyveManager.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormBhyveManager.MenuItemSettingsClick(Sender: TObject);
begin
  FormSettings.Visible:=True;
end;

procedure TFormBhyveManager.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  GlobalNode.Free;
  GlobalSettingTypeList.Free;
  GlobalSettingDefaultValueList.Free;
  GlobalSettingCategoryList.Free;
  DevicesList.Free;
  ActionImageList.Free;
  DeviceImageList.Free;
  SystemImageList.Free;
  DevicesPopup.Free;
  GlobalSettingsPopup.Free;
  VirtualMachinesPopup.Free;
  NetworkDeviceList.Free;
  VirtualMachineList.Free;

  TmpDevicesStringList.Free;
end;

procedure TFormBhyveManager.AddDevice(Sender: TObject);
var
  Node : TTreeNode;
  PciSlot : String;
begin
  Node := DeviceSettingsTreeView.Selected;

  if Assigned(VirtualMachinesTreeView.Selected) then
  begin
    case Node.Text of
        'Audio':
          begin
            FormAudioDevice:=TFormAudioDevice.Create(FormBhyveManager);
            FormAudioDevice.BitBtnSave.OnClick:=@SaveAudioDevice;
            FormAudioDevice.FormStyle:=fsSystemStayOnTop;
            FormAudioDevice.LoadDefaultValues();
            FormAudioDevice.FormAction:='Add';
            FormAudioDevice.Show;
          end;
        'Console':
          begin
            FormConsoleDevice:=TFormConsoleDevice.Create(FormBhyveManager);
            FormConsoleDevice.BitBtnSave.OnClick:=@SaveConsoleDevice;
            FormConsoleDevice.FormStyle:=fsSystemStayOnTop;
            FormConsoleDevice.VtconName:='vtcon'+GetNewConsoleName(TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).name);
            FormConsoleDevice.VmName:=TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).name;
            FormConsoleDevice.LoadDefaultValues();
            FormConsoleDevice.FormAction:='Add';
            FormConsoleDevice.Show;
          end;
        'Display':
          begin
            if (Node.Count = 0) then
            begin
              FormDisplayDevice:=TFormDisplayDevice.Create(FormBhyveManager);
              FormDisplayDevice.BitBtnSave.OnClick:=@SaveDisplayDevice;
              FormDisplayDevice.FormStyle:=fsSystemStayOnTop;
              FormDisplayDevice.LoadDefaultValues();
              FormDisplayDevice.FormAction:='Add';
              FormDisplayDevice.Show;
            end;
          end;
        'Hostbridge':
          begin
            if (Node.Count = 0) then
            begin
              FormHostbridgeDevice:=TFormHostbridgeDevice.Create(FormBhyveManager);
              FormHostbridgeDevice.BitBtnSave.OnClick:=@SaveHostbridgeDevice;
              FormHostbridgeDevice.FormStyle:=fsSystemStayOnTop;
              FormHostbridgeDevice.LoadDefaultValues();
              FormHostbridgeDevice.FormAction:='Add';
              FormHostbridgeDevice.Show;
            end;
          end;
        'Input':
          begin
            FormInputDevice:=TFormInputDevice.Create(FormBhyveManager);
            FormInputDevice.BitBtnSave.OnClick:=@SaveInputDevice;
            FormInputDevice.FormStyle:=fsSystemStayOnTop;
            FormInputDevice.LoadDefaultValues();
            FormInputDevice.FormAction:='Add';
            FormInputDevice.Show;
          end;
        'LPC':
          begin
            if (Node.Count = 0) then
            begin
              FormLpcDevice:=TFormLpcDevice.Create(FormBhyveManager);
              FormLpcDevice.BitBtnSave.OnClick:=@SaveLpcDevice;
              FormLpcDevice.FormStyle:=fsSystemStayOnTop;
              FormLpcDevice.LoadDefaultValues(TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).name);
              FormLpcDevice.FormAction:='Add';
              FormLpcDevice.Show;
            end;
          end;
        'Network':
          begin
            FormNetworkDevice:=TFormNetworkDevice.Create(FormBhyveManager);
            FormNetworkDevice.BitBtnSave.OnClick:=@SaveNetworkDevice;
            FormNetworkDevice.FormStyle:=fsSystemStayOnTop;
            FormNetworkDevice.BackendDevice:=GetNewNetworkName(TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).name, TmpDevicesStringList, 'tap', 0);
            FormNetworkDevice.MacAddress:=GenerateMacAddress();
            FormNetworkDevice.LoadDefaultValues();
            FormNetworkDevice.FormAction:='Add';
            FormNetworkDevice.Show;
          end;
        'Passthru':
          begin
            FormPassthruDevice:=TFormPassthruDevice.Create(FormBhyveManager);
            FormPassthruDevice.BitBtnSave.OnClick:=@SavePassthruDevice;
            FormPassthruDevice.FormStyle:=fsSystemStayOnTop;
            FormPassthruDevice.LoadDefaultValues();
            FormPassthruDevice.FormAction:='Add';
            FormPassthruDevice.Show;
          end;
        'RNG':
          begin
            if (Node.Count = 0) then
            begin
              PciSlot:=GetNewPciSlotNumber(TmpDevicesStringList, 20);

              TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:='virtio-rnd';

              RNGDevice:=FillDetailRngDevice(TmpDevicesStringList.Text, PciSlot, 'virtio-rnd');

              GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('RNG'), 'device : '+RNGDevice.device);
              GlobalNode.Data:=RNGDevice;
              GlobalNode.ImageIndex:=12;
              GlobalNode.SelectedIndex:=12;

              DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+RNGDevice.pci);

              SaveVirtualMachineConfig();
            end;
          end;
        'Shared folders':
          begin
            FormShareFolderDevice:=TFormShareFolderDevice.Create(FormBhyveManager);
            FormShareFolderDevice.BitBtnSave.OnClick:=@SaveShareFolderDevice;
            FormShareFolderDevice.FormStyle:=fsSystemStayOnTop;
            FormShareFolderDevice.LoadDefaultValues();
            FormShareFolderDevice.FormAction:='Add';
            FormShareFolderDevice.Show;
          end;
        'Storage':
          begin
            FormStorageDevice:=TFormStorageDevice.Create(FormBhyveManager);
            FormStorageDevice.BitBtnSave.OnClick:=@SaveStorageDevice;
            FormStorageDevice.FormStyle:=fsSystemStayOnTop;
            FormStorageDevice.VmName:=TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).name;
            FormStorageDevice.FormAction:='Add';
            FormStorageDevice.LoadDefaultValues();
            FormStorageDevice.Show;
          end;
        'USB':
          begin
            if (Node.Count = 0) then
            begin
              PciSlot:='0.31.0';

              TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:='xhci';
              TmpDevicesStringList.Values['pci.'+PciSlot+'.slot.1.device']:='tablet';

              UsbXhciDevice:=FillDetailUsbXhciDevice(TmpDevicesStringList.Text, PciSlot, 'xhci', 1);

              GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('USB'), 'device : '+UsbXhciDevice.device+'-'+UsbXhciDevice.slot_device);
              GlobalNode.Data:=UsbXhciDevice;
              GlobalNode.ImageIndex:=14;
              GlobalNode.SelectedIndex:=14;

              DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+UsbXhciDevice.pci);
              DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'slot : '+UsbXhciDevice.slot.ToString);

              SaveVirtualMachineConfig();
            end;
          end;
    end;
  end;
end;

procedure TFormBhyveManager.EditDevice(Sender: TObject);
var
  Node : TTreeNode;
begin
  Node := DeviceSettingsTreeView.Selected;

  if Assigned(VirtualMachinesTreeView.Selected) then
  begin
    case Node.Parent.Text of
        'Audio':
          begin
            AudioDevice := TAudioDeviceClass(Node.Data);

            FormAudioDevice:=TFormAudioDevice.Create(FormBhyveManager);
            FormAudioDevice.BitBtnSave.OnClick:=@SaveAudioDevice;
            FormAudioDevice.FormStyle:=fsSystemStayOnTop;
            FormAudioDevice.LoadDefaultValues();
            FormAudioDevice.EditPlayDevice.Text:=AudioDevice.play;
            FormAudioDevice.EditRecDevice.Text:=AudioDevice.rec;
            FormAudioDevice.FormAction:='Update';
            FormAudioDevice.Show;
          end;
        'Console':
          begin
            SerialVirtioConsoleDevice := TSerialVirtioConsoleDeviceClass(Node.Data);

            FormConsoleDevice:=TFormConsoleDevice.Create(FormBhyveManager);
            FormConsoleDevice.BitBtnSave.OnClick:=@SaveConsoleDevice;
            FormConsoleDevice.FormStyle:=fsSystemStayOnTop;
            FormConsoleDevice.VtconName:=SerialVirtioConsoleDevice.name;
            FormConsoleDevice.VmName:=TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).name;
            FormConsoleDevice.ComboBoxDevice.ItemIndex:=FormConsoleDevice.ComboBoxDevice.Items.IndexOf(SerialVirtioConsoleDevice.device);
            FormConsoleDevice.LoadDefaultValues();
            FormConsoleDevice.FormAction:='Update';

            FormConsoleDevice.Show;
          end;
        'Display':
          begin
            DisplayDevice := TDisplayDeviceClass(Node.Data);

            FormDisplayDevice:=TFormDisplayDevice.Create(FormBhyveManager);
            FormDisplayDevice.BitBtnSave.OnClick:=@SaveDisplayDevice;
            FormDisplayDevice.FormStyle:=fsSystemStayOnTop;
            FormDisplayDevice.LoadDefaultValues();
            FormDisplayDevice.FormAction:='Update';
            FormDisplayDevice.CheckBoxWaitVnc.Checked:=StrToBool(DisplayDevice.wait);
            FormDisplayDevice.EditHost.Text:=DisplayDevice.tcp;

            if DisplayDevice.vga <> EmptyStr then FormDisplayDevice.ComboBoxVga.ItemIndex:=FormDisplayDevice.ComboBoxVga.Items.IndexOf(DisplayDevice.vga);
            if (DisplayDevice.w > 0) and (DisplayDevice.h > 0) then FormDisplayDevice.ComboBoxResolution.ItemIndex:=FormDisplayDevice.ComboBoxResolution.Items.IndexOf(DisplayDevice.w.ToString+'x'+DisplayDevice.h.ToString);
            if trim(DisplayDevice.pass) <> EmptyStr then
            begin
              FormDisplayDevice.CheckBoxUsePassword.Checked:=True;
              FormDisplayDevice.EditPassword.Text:=DisplayDevice.pass;
            end;

            FormDisplayDevice.Show;
          end;
        'Hostbridge':
          begin
            HostBridgeDevice := THostbridgeDeviceClass(Node.Data);

            FormHostbridgeDevice:=TFormHostbridgeDevice.Create(FormBhyveManager);
            FormHostbridgeDevice.BitBtnSave.OnClick:=@SaveHostbridgeDevice;
            FormHostbridgeDevice.FormStyle:=fsSystemStayOnTop;
            FormHostbridgeDevice.LoadDefaultValues();
            FormHostbridgeDevice.FormAction:='Update';
            FormHostbridgeDevice.ComboBoxHostbridgeDevice.ItemIndex:=FormHostbridgeDevice.ComboBoxHostbridgeDevice.Items.IndexOf(HostBridgeDevice.device);
            FormHostbridgeDevice.Show;
          end;
        'Input':
          begin
            InputDevice := TVirtioInputDeviceClass(Node.Data);

            FormInputDevice:=TFormInputDevice.Create(FormBhyveManager);
            FormInputDevice.BitBtnSave.OnClick:=@SaveInputDevice;
            FormInputDevice.FormStyle:=fsSystemStayOnTop;
            FormInputDevice.LoadDefaultValues();
            FormInputDevice.FormAction:='Update';
            FormInputDevice.ComboBoxInputDevice.ItemIndex:=FormInputDevice.ComboBoxInputDevice.Items.IndexOf(InputDevice.path);
            FormInputDevice.Show;
          end;
        'LPC':
          begin
            LPCDevice := TLPCDeviceClass(Node.Data);

            FormLpcDevice:=TFormLpcDevice.Create(FormBhyveManager);
            FormLpcDevice.BitBtnSave.OnClick:=@SaveLpcDevice;
            FormLpcDevice.FormStyle:=fsSystemStayOnTop;
            FormLpcDevice.LoadDefaultValues(TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).name);
            FormLpcDevice.FormAction:='Update';

            FormLpcDevice.ComboBoxBootrom.ItemIndex:=FormLpcDevice.ComboBoxBootrom.Items.IndexOf(ExtractFileName(LPCDevice.bootrom));
            if LPCDevice.com1 <> EmptyStr then FormLpcDevice.CheckBoxCom1.Checked:=True;
            if LPCDevice.com2 <> EmptyStr then FormLpcDevice.CheckBoxCom2.Checked:=True;
            if LPCDevice.com3 <> EmptyStr then FormLpcDevice.CheckBoxCom3.Checked:=True;
            if LPCDevice.com4 <> EmptyStr then FormLpcDevice.CheckBoxCom4.Checked:=True;

            FormLpcDevice.Show;
          end;
        'Network':
          begin
            NetworkDevice := TNetworkDeviceClass(Node.Data);

            FormNetworkDevice:=TFormNetworkDevice.Create(FormBhyveManager);
            FormNetworkDevice.BitBtnSave.OnClick:=@SaveNetworkDevice;
            FormNetworkDevice.FormStyle:=fsSystemStayOnTop;
            FormNetworkDevice.BackendDevice:=NetworkDevice.backend;
            FormNetworkDevice.MacAddress:=NetworkDevice.mac;
            FormNetworkDevice.LoadDefaultValues();
            FormNetworkDevice.ComboBoxDevice.ItemIndex:=FormNetworkDevice.ComboBoxDevice.Items.IndexOf(NetworkDevice.device);
            FormNetworkDevice.FormAction:='Update';

            if ((NetworkDevice.mtu <> 1500) and (NetworkDevice.mtu <> 0)) then FormNetworkDevice.SpinEditExMtu.Value:=NetworkDevice.mtu else FormNetworkDevice.SpinEditExMtu.Value:=1500;

            FormNetworkDevice.Show;
          end;
        'Passthru':
          begin
            PassthruDevice := TPassthruDeviceClass(Node.Data);

            FormPassthruDevice:=TFormPassthruDevice.Create(FormBhyveManager);
            FormPassthruDevice.BitBtnSave.OnClick:=@SavePassthruDevice;
            FormPassthruDevice.FormStyle:=fsSystemStayOnTop;
            FormPassthruDevice.LoadDefaultValues();
            FormPassthruDevice.ComboBoxDevice.ItemIndex:=FormPassthruDevice.ComboBoxDevice.Items.IndexOf(PassthruDevice.pptdev);
            FormPassthruDevice.EditDescripcion.Text:=GetPciDeviceDescripcion(PassthruDevice.pptdev);
            FormPassthruDevice.FormAction:='Update';

            if (PassthruDevice.rom <> EmptyStr) then FormPassthruDevice.FileNameEditRom.Text:=PassthruDevice.rom;

            FormPassthruDevice.Show;
          end;
        'RNG':
          begin
            { nothing to do because rng device must be added or deleted }
          end;
        'Shared folders':
          begin
            ShareFolderDevice := TShareFolderDeviceClass(Node.Data);

            FormShareFolderDevice:=TFormShareFolderDevice.Create(FormBhyveManager);
            FormShareFolderDevice.BitBtnSave.OnClick:=@SaveShareFolderDevice;
            FormShareFolderDevice.FormStyle:=fsSystemStayOnTop;
            if ShareFolderDevice.ro then FormShareFolderDevice.CheckBoxReadOnly.Checked:=True else FormShareFolderDevice.CheckBoxReadOnly.Checked:=False;
            FormShareFolderDevice.ComboBoxDevice.ItemIndex:=FormShareFolderDevice.ComboBoxDevice.Items.IndexOf(ShareFolderDevice.device);
            FormShareFolderDevice.EditSharename.Text:=ShareFolderDevice.sharename;
            FormShareFolderDevice.DirectoryEditPath.Text:=ShareFolderDevice.path;
            FormShareFolderDevice.LoadDefaultValues();
            FormShareFolderDevice.FormAction:='Update';

            FormShareFolderDevice.Show;
          end;
        'Storage':
          begin
            FormStorageDevice:=TFormStorageDevice.Create(FormBhyveManager);
            FormStorageDevice.BitBtnSave.OnClick:=@SaveStorageDevice;
            FormStorageDevice.FormStyle:=fsSystemStayOnTop;
            FormStorageDevice.VmName:=TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).name;

            case ExtractVarValue(Node.Text) of
                'ahci-cd',
                'ahci-hd':
                  begin
                    StorageAhciDevice := TStorageAhciDeviceClass(Node.Data);
                    FormStorageDevice.LoadDefaultValues();
                    FormStorageDevice.FormAction:='Update';
                    FormStorageDevice.ComboBoxStorageDevice.ItemIndex:=FormStorageDevice.ComboBoxStorageDevice.Items.IndexOf(StorageAhciDevice.device+'-'+StorageAhciDevice.device_type);
                    FormStorageDevice.ComboBoxStorageType.ItemIndex:=FormStorageDevice.ComboBoxStorageType.Items.IndexOf(StorageAhciDevice.storage_type);
                    FormStorageDevice.ComboBoxStorageDeviceChange(Nil);
                    FormStorageDevice.ComboBoxStorageDevice.Enabled:=False;
                    FormStorageDevice.ComboBoxStorageType.Enabled:=False;
                    FormStorageDevice.FileNameEditStoragePath.Text:=StorageAhciDevice.path;
                    FormStorageDevice.SpinEditExDiskSize.Value:=StorageAhciDevice.storage_size.Replace('G','').ToInt64;
                    FormStorageDevice.SpinEditExDiskSize.Enabled:=False;
                    FormStorageDevice.EditSer.Text:=StorageAhciDevice.ser;
                    FormStorageDevice.CheckBoxNoCache.Checked:=StorageAhciDevice.nocache;
                    FormStorageDevice.CheckBoxNoDelete.Checked:=StorageAhciDevice.nodelete;
                    FormStorageDevice.CheckBoxSync.Checked:=StorageAhciDevice.sync;
                    FormStorageDevice.CheckBoxReadOnly.Checked:=StorageAhciDevice.ro;
                  end;
                'nvme':
                  begin
                    StorageNvmeDevice := TStorageNvmeDeviceClass(Node.Data);
                    FormStorageDevice.LoadDefaultValues();
                    FormStorageDevice.FormAction:='Update';
                    FormStorageDevice.ComboBoxStorageDevice.ItemIndex:=FormStorageDevice.ComboBoxStorageDevice.Items.IndexOf(StorageNvmeDevice.device);
                    FormStorageDevice.ComboBoxStorageType.ItemIndex:=FormStorageDevice.ComboBoxStorageType.Items.IndexOf(StorageNvmeDevice.storage_type);
                    FormStorageDevice.ComboBoxStorageDeviceChange(Nil);
                    FormStorageDevice.ComboBoxStorageDevice.Enabled:=False;
                    FormStorageDevice.ComboBoxStorageType.Enabled:=False;
                    FormStorageDevice.FileNameEditStoragePath.Text:=StorageNvmeDevice.devpath;
                    FormStorageDevice.SpinEditExDiskSize.Value:=StorageNvmeDevice.storage_size.Replace('G','').ToInt64;
                    FormStorageDevice.SpinEditExDiskSize.Enabled:=False;
                    FormStorageDevice.EditSer.Text:=StorageNvmeDevice.ser;
                    FormStorageDevice.CheckBoxNoCache.Checked:=StorageNvmeDevice.nocache;
                    FormStorageDevice.CheckBoxNoDelete.Checked:=StorageNvmeDevice.nodelete;
                    FormStorageDevice.CheckBoxSync.Checked:=StorageNvmeDevice.sync;
                    FormStorageDevice.CheckBoxReadOnly.Checked:=StorageNvmeDevice.ro;

                    if (StorageNvmeDevice.maxq <> 16) and (StorageNvmeDevice.maxq <> 0) then FormStorageDevice.EditNvmeMaxq.Text:=StorageNvmeDevice.maxq.ToString;
                    if (StorageNvmeDevice.qsz <> 2058) and (StorageNvmeDevice.qsz <> 0) then FormStorageDevice.EditNvmeQsz.Text:=StorageNvmeDevice.qsz.ToString;
                    if (StorageNvmeDevice.ioslots <> 8) and (StorageNvmeDevice.ioslots <> 0) then FormStorageDevice.EditNvmeIoslots.Text:=StorageNvmeDevice.ioslots.ToString;
                    FormStorageDevice.EditNvmeSectsz.Text:=StorageNvmeDevice.sectsz.ToString;
                    FormStorageDevice.EditNvmeEui64.Text:=StorageNvmeDevice.eui64.ToString;
                    if (StorageNvmeDevice.dsm <> 'auto') and (StorageNvmeDevice.dsm <> EmptyStr) then FormStorageDevice.ComboBoxNvmeDsm.ItemIndex:=FormStorageDevice.ComboBoxNvmeDsm.Items.IndexOf(StorageNvmeDevice.dsm);
                    if StorageNvmeDevice.ram > 0 then
                    begin
                      FormStorageDevice.CheckBoxNvmUseRam.Checked:=True;
                      FormStorageDevice.SpinEditExNvmeRam.Enabled:=True;
                      FormStorageDevice.SpinEditExDiskSize.Enabled:=False;
                    end
                    else
                    begin
                      FormStorageDevice.CheckBoxNvmUseRam.Enabled:=False;
                      FormStorageDevice.SpinEditExNvmeRam.Enabled:=False;
                    end;
                  end;
                'virtio-blk':
                  begin
                    StorageVirtioBlkDevice := TStorageVirtioBlkDeviceClass(Node.Data);
                    FormStorageDevice.LoadDefaultValues();
                    FormStorageDevice.FormAction:='Update';
                    FormStorageDevice.ComboBoxStorageDevice.ItemIndex:=FormStorageDevice.ComboBoxStorageDevice.Items.IndexOf(StorageVirtioBlkDevice.device);
                    FormStorageDevice.ComboBoxStorageType.ItemIndex:=FormStorageDevice.ComboBoxStorageType.Items.IndexOf(StorageVirtioBlkDevice.storage_type);
                    FormStorageDevice.ComboBoxStorageDeviceChange(Nil);
                    FormStorageDevice.ComboBoxStorageDevice.Enabled:=False;
                    FormStorageDevice.ComboBoxStorageType.Enabled:=False;
                    FormStorageDevice.SpinEditExDiskSize.Value:=StorageVirtioBlkDevice.storage_size.Replace('G','').ToInt64;
                    FormStorageDevice.SpinEditExDiskSize.Enabled:=False;
                    FormStorageDevice.FileNameEditStoragePath.Text:=StorageVirtioBlkDevice.path;
                    FormStorageDevice.EditSer.Text:=StorageVirtioBlkDevice.ser;
                    FormStorageDevice.CheckBoxNoCache.Checked:=StorageVirtioBlkDevice.nocache;
                    FormStorageDevice.CheckBoxNoDelete.Checked:=StorageVirtioBlkDevice.nodelete;
                    FormStorageDevice.CheckBoxSync.Checked:=StorageVirtioBlkDevice.sync;
                    FormStorageDevice.CheckBoxReadOnly.Checked:=StorageVirtioBlkDevice.ro;
                  end;
            end;

            FormStorageDevice.Show;
          end;
        'USB':
          begin
            { nothing to do because usb device must be added or deleted }
          end;
    end;
  end;
end;

procedure TFormBhyveManager.EditVirtualMachineInfo(Sender: TObject);
begin
  if (Assigned(VirtualMachinesTreeView.Selected)) and (VirtualMachinesTreeView.Selected.Level = 1) then
  begin
    GlobalNode:=VirtualMachinesTreeView.Selected;
    VirtualMachine := TVirtualMachineClass(GlobalNode.Data);

    FormVmInfo.BitBtnSave.OnClick:=@SaveVirtualMachineInfoClick;
    FormVmInfo.Visible:=True;
    FormVmInfo.ComboBoxVmType.Clear;
    FillComboSystemType(FormVmInfo.ComboBoxVmType);
    FormVmInfo.ComboBoxVmType.ItemIndex:=FormVmInfo.ComboBoxVmType.Items.IndexOf(VirtualMachine.system_type);
    FillComboSystemVersion(FormVmInfo.ComboBoxVmVersion, FormVmInfo.ComboBoxVmType.Text);
    FormVmInfo.ComboBoxVmVersion.ItemIndex:=FormVmInfo.ComboBoxVmVersion.Items.IndexOf(VirtualMachine.system_version);
    FormVmInfo.EditVmName.Text:=VirtualMachine.name;
    FormVmInfo.EditVmDescription.Text:=VirtualMachine.description;
    if VirtualMachine.rdp = StrToBool('True') then
      FormVmInfo.CheckBoxRDP.Checked:=True
    else
      FormVmInfo.CheckBoxRDP.Checked:=False;
  end;
end;

procedure TFormBhyveManager.RemoveDevice(Sender: TObject);
var
  i : Integer;
  PciSlot : String;
begin
  if (Assigned(DeviceSettingsTreeView.Selected)) and (DeviceSettingsTreeView.Selected.Level = 1) then
  begin
    if (MessageDlg('Virtual machine devices', 'This action will remove all files/resources created by this device. Do you want remove '+ExtractVarValue(DeviceSettingsTreeView.Selected.Text)+' device?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin

      case DeviceSettingsTreeView.Selected.Parent.Text of
          'Audio':
            begin
              AudioDevice := TAudioDeviceClass(DeviceSettingsTreeView.Selected.Data);
              PciSlot:=AudioDevice.pci;
            end;
          'Console':
            begin
              SerialVirtioConsoleDevice := TSerialVirtioConsoleDeviceClass(DeviceSettingsTreeView.Selected.Data);
              PciSlot:=SerialVirtioConsoleDevice.pci;
            end;
          'Display':
            begin
              DisplayDevice := TDisplayDeviceClass(DeviceSettingsTreeView.Selected.Data);
              PciSlot:=DisplayDevice.pci;
            end;
          'Hostbridge':
            begin
              HostBridgeDevice := THostbridgeDeviceClass(DeviceSettingsTreeView.Selected.Data);
              PciSlot:=HostBridgeDevice.pci;
            end;
          'Input':
            begin
              InputDevice := TVirtioInputDeviceClass(DeviceSettingsTreeView.Selected.Data);
              PciSlot:=InputDevice.pci;
            end;
          'LPC':
            begin
              LPCDevice := TLPCDeviceClass(DeviceSettingsTreeView.Selected.Data);
              PciSlot:=LPCDevice.pci;
            end;
          'Network':
            begin
              NetworkDevice := TNetworkDeviceClass(DeviceSettingsTreeView.Selected.Data);
              PciSlot:=NetworkDevice.pci;
            end;
          'Passthru':
            begin
              PassthruDevice := TPassthruDeviceClass(DeviceSettingsTreeView.Selected.Data);
              PciSlot:=PassthruDevice.pci;
            end;
          'RNG':
            begin
              RNGDevice := TRNGDeviceClass(DeviceSettingsTreeView.Selected.Data);
              PciSlot:=RNGDevice.pci;
            end;
          'Shared folders':
            begin
              ShareFolderDevice := TShareFolderDeviceClass(DeviceSettingsTreeView.Selected.Data);
              PciSlot:=ShareFolderDevice.pci;
            end;
          'Storage':
            begin
              case ExtractVarValue(DeviceSettingsTreeView.Selected.Text) of
                  'ahci-cd',
                  'ahci-hd':
                    begin
                      StorageAhciDevice := TStorageAhciDeviceClass(DeviceSettingsTreeView.Selected.Data);
                      PciSlot:=StorageAhciDevice.pci;

                      case StorageAhciDevice.storage_type of
                          'image file': if StorageAhciDevice.device_type = 'hd' then RemoveFile(StorageAhciDevice.path);
                          'zfs sparse volume': ZfsDestroy(StorageAhciDevice.path.Remove(0,10), False);
                          'zfs volume': ZfsDestroy(StorageAhciDevice.path.Remove(0,10), False);
                      end;
                    end;
                  'nvme':
                    begin
                      StorageNvmeDevice := TStorageNvmeDeviceClass(DeviceSettingsTreeView.Selected.Data);
                      PciSlot:=StorageNvmeDevice.pci;

                      case StorageNvmeDevice.storage_type of
                          'image file': RemoveFile(StorageNvmeDevice.devpath);
                          'zfs sparse volume': ZfsDestroy(StorageNvmeDevice.devpath.Remove(0,10), False);
                          'zfs volume': ZfsDestroy(StorageNvmeDevice.devpath.Remove(0,10), False);
                      end;
                    end;
                  'virtio-blk':
                    begin
                      StorageVirtioBlkDevice := TStorageVirtioBlkDeviceClass(DeviceSettingsTreeView.Selected.Data);
                      PciSlot:=StorageVirtioBlkDevice.pci;

                      case StorageVirtioBlkDevice.storage_type of
                          'image file': RemoveFile(StorageVirtioBlkDevice.path);
                          'zfs sparse volume': ZfsDestroy(StorageVirtioBlkDevice.path.Remove(0,10), False);
                          'zfs volume': ZfsDestroy(StorageVirtioBlkDevice.path.Remove(0,10), False);
                      end;
                    end;
              end;
            end;
          'USB':
            begin
              UsbXhciDevice := TUsbXhciDeviceClass(DeviceSettingsTreeView.Selected.Data);
              PciSlot:=UsbXhciDevice.pci;
            end;
      end;

      for i:=TmpDevicesStringList.Count-1 downto 0 do
      begin
          if (TmpDevicesStringList[i].Contains('pci.'+PciSlot))  then
          begin
            TmpDevicesStringList.Delete(i);
          end;

          if (DeviceSettingsTreeView.Selected.Parent.Text = 'LPC') and
          (TmpDevicesStringList[i].Contains('lpc.')) then
          begin
            TmpDevicesStringList.Delete(i);
          end;
      end;

      TObject(DeviceSettingsTreeView.Selected.Data).Free;
      DeviceSettingsTreeView.Selected.Data:=Nil;
      DeviceSettingsTreeView.Selected.Delete;

      SaveVirtualMachineConfig();
    end;
  end;
end;

procedure TFormBhyveManager.RemoteDesktopProtocolVm(Sender: TObject);
begin
  if (Assigned(VirtualMachinesTreeView.Selected)) and (VirtualMachinesTreeView.Selected.Level = 1)
     and (TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).rdp = True) then
  begin
    if (FormRdpConnection.FormAction = EmptyStr) then
    begin
      FormRdpConnection.Caption:=TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).name+' VM';
      FormRdpConnection.BitBtnConnect.OnClick:=@RemoteDesktopProtocolVm;
      FormRdpConnection.FormStyle:=fsSystemStayOnTop;
      FormRdpConnection.FormAction:='Connect';
      FormRdpConnection.LoadDefaultValues();
      FormRdpConnection.Show;
    end
    else if (FormRdpConnection.FormAction='Connect') then
    begin
      RdpConnect(TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).name, FormRdpConnection.EditUsername.Text, FormRdpConnection.EditPassword.Text, ExtractDelimited(1,FormRdpConnection.ComboBoxResolution.Text,['x']), ExtractDelimited(2,FormRdpConnection.ComboBoxResolution.Text,['x']));

      FormRdpConnection.FormAction:=EmptyStr;
      FormRdpConnection.Hide;
    end;
  end;
end;

procedure TFormBhyveManager.SaveAudioDevice(Sender: TObject);
var
  PciSlot : String;
begin
 if (FormAudioDevice.FormAction = 'Add') and FormAudioDevice.FormValidate() then
 begin
   PciSlot:=GetNewPciSlotNumber(TmpDevicesStringList, 18);

   TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:='hda';
   TmpDevicesStringList.Values['pci.'+PciSlot+'.play']:=FormAudioDevice.EditPlayDevice.Text;
   TmpDevicesStringList.Values['pci.'+PciSlot+'.rec']:=FormAudioDevice.EditRecDevice.Text;

   AudioDevice:=FillDetailAudioDevice(TmpDevicesStringList.Text, PciSlot, 'hda');

   GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('Audio'), 'device : '+AudioDevice.device);
   GlobalNode.Data:=AudioDevice;
   GlobalNode.ImageIndex:=1;
   GlobalNode.SelectedIndex:=1;

   DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+AudioDevice.pci);

   FormAudioDevice.Destroy;

   SaveVirtualMachineConfig();
 end
 else if (FormAudioDevice.FormAction = 'Update') and FormAudioDevice.FormValidate() then
 begin
   PciSlot:=AudioDevice.pci;

   TmpDevicesStringList.Values['pci.'+PciSlot+'.play']:=FormAudioDevice.EditPlayDevice.Text;
   TmpDevicesStringList.Values['pci.'+PciSlot+'.rec']:=FormAudioDevice.EditRecDevice.Text;

   AudioDevice.play:=FormAudioDevice.EditPlayDevice.Text;
   AudioDevice.rec:=FormAudioDevice.EditRecDevice.Text;

   FormAudioDevice.Destroy;

   SaveVirtualMachineConfig();
 end
 else
 begin
   FormAudioDevice.StatusBarAudioDevice.Font.Color:=clRed;
   FormAudioDevice.StatusBarAudioDevice.SimpleText:='Audio device can not added/updated.';
 end;
end;

procedure TFormBhyveManager.SaveConsoleDevice(Sender: TObject);
var
  i : Integer;
  PciSlot : String;
begin
 if (FormConsoleDevice.FormAction = 'Add') and FormConsoleDevice.FormValidate() then
 begin
   PciSlot:=GetNewPciSlotNumber(TmpDevicesStringList, 21);

   TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:=FormConsoleDevice.ComboBoxDevice.Text;
   TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.name']:=FormConsoleDevice.EditName.Text;
   TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.path']:=FormConsoleDevice.EditPath.Text;

   SerialVirtioConsoleDevice:=FillDetailConsoleDevice(TmpDevicesStringList.Text, PciSlot, FormConsoleDevice.ComboBoxDevice.Text, 0);

   GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('Console'), 'device : '+SerialVirtioConsoleDevice.device);
   GlobalNode.Data:=SerialVirtioConsoleDevice;
   GlobalNode.ImageIndex:=3;
   GlobalNode.SelectedIndex:=3;

   DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+SerialVirtioConsoleDevice.pci);
   DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'port : '+IntToStr(SerialVirtioConsoleDevice.port));

   FormConsoleDevice.Destroy;

   SaveVirtualMachineConfig();
 end
 else if (FormConsoleDevice.FormAction = 'Update') and FormConsoleDevice.FormValidate() then
 begin
   PciSlot:=SerialVirtioConsoleDevice.pci;

   for i:=TmpDevicesStringList.Count-1 downto 0 do
   begin
       if (TmpDevicesStringList[i].Contains('pci.'+PciSlot)) then
       begin
         TmpDevicesStringList.Delete(i);
       end;
   end;

   TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:=FormConsoleDevice.ComboBoxDevice.Text;
   TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.name']:=FormConsoleDevice.EditName.Text;
   TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.path']:=FormConsoleDevice.EditPath.Text;

   DeviceSettingsTreeView.Items.FindNodeWithText('device : '+SerialVirtioConsoleDevice.device).Text:='device : '+FormConsoleDevice.ComboBoxDevice.Text;

   SerialVirtioConsoleDevice.device:=FormConsoleDevice.ComboBoxDevice.Text;

   FormConsoleDevice.Destroy;

   SaveVirtualMachineConfig();
 end
 else
 begin
   FormConsoleDevice.StatusBarConsoleDevice.Font.Color:=clRed;
   FormConsoleDevice.StatusBarConsoleDevice.SimpleText:='Console device can not added/updated.';
 end;
end;

procedure TFormBhyveManager.SaveDisplayDevice(Sender: TObject);
var
  PciSlot : String;
begin
 if (FormDisplayDevice.FormAction = 'Add') and FormDisplayDevice.FormValidate() then
 begin
   PciSlot:='0.30.0';

   TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:='fbuf';
   TmpDevicesStringList.Values['pci.'+PciSlot+'.tcp']:=FormDisplayDevice.EditHost.Text;
   if FormDisplayDevice.CheckBoxWaitVnc.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.wait']:='true';
   TmpDevicesStringList.Values['pci.'+PciSlot+'.w']:=ExtractDelimited(1,FormDisplayDevice.ComboBoxResolution.Text,['x']);
   TmpDevicesStringList.Values['pci.'+PciSlot+'.h']:=ExtractDelimited(2,FormDisplayDevice.ComboBoxResolution.Text,['x']);
   TmpDevicesStringList.Values['pci.'+PciSlot+'.vga']:=FormDisplayDevice.ComboBoxVga.Text;
   if (FormDisplayDevice.CheckBoxUsePassword.Checked and (trim(FormDisplayDevice.EditPassword.Text) <> EmptyStr)) then TmpDevicesStringList.Values['pci.'+PciSlot+'.password']:=FormDisplayDevice.EditPassword.Text;

   DisplayDevice:=FillDetailDisplayDevice(TmpDevicesStringList.Text, '0.'+PciSlot, 'fbuf');

   GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('Display'), 'device : '+DisplayDevice.device);
   GlobalNode.Data:=DisplayDevice;
   GlobalNode.ImageIndex:=4;
   GlobalNode.SelectedIndex:=4;

   DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+DisplayDevice.pci);

   FormDisplayDevice.Destroy;

   SaveVirtualMachineConfig();
 end
 else if (FormDisplayDevice.FormAction = 'Update') and FormDisplayDevice.FormValidate() then
 begin
   PciSlot:=DisplayDevice.pci;

   TmpDevicesStringList.Values['pci.'+PciSlot+'.tcp']:=FormDisplayDevice.EditHost.Text;
   TmpDevicesStringList.Values['pci.'+PciSlot+'.wait']:=BoolToStr(FormDisplayDevice.CheckBoxWaitVnc.Checked, 'true', 'false');
   TmpDevicesStringList.Values['pci.'+PciSlot+'.w']:=ExtractDelimited(1,FormDisplayDevice.ComboBoxResolution.Text,['x']);
   TmpDevicesStringList.Values['pci.'+PciSlot+'.h']:=ExtractDelimited(2,FormDisplayDevice.ComboBoxResolution.Text,['x']);
   TmpDevicesStringList.Values['pci.'+PciSlot+'.vga']:=FormDisplayDevice.ComboBoxVga.Text;

   if FormDisplayDevice.EditPassword.Text = EmptyStr then
   begin
     if TmpDevicesStringList.IndexOfName('pci.'+PciSlot+'.password') <> -1 then
       TmpDevicesStringList.Delete(TmpDevicesStringList.IndexOfName('pci.'+PciSlot+'.password'));
   end
   else
   begin
     TmpDevicesStringList.Values['pci.'+PciSlot+'.password']:=FormDisplayDevice.EditPassword.Text;
   end;

   DisplayDevice.tcp:=FormDisplayDevice.EditHost.Text;
   DisplayDevice.port:=StrToInt(ExtractDelimited(2,FormDisplayDevice.EditHost.Text,[':']));
   DisplayDevice.wait:=BoolToStr(FormDisplayDevice.CheckBoxWaitVnc.Checked, 'true', 'false');
   DisplayDevice.w:=ExtractDelimited(1,FormDisplayDevice.ComboBoxResolution.Text,['x']).ToInteger;
   DisplayDevice.h:=ExtractDelimited(2,FormDisplayDevice.ComboBoxResolution.Text,['x']).ToInteger;
   DisplayDevice.vga:=FormDisplayDevice.ComboBoxVga.Text;
   DisplayDevice.pass:=Trim(FormDisplayDevice.EditPassword.Text);

   FormDisplayDevice.Destroy;

   SaveVirtualMachineConfig();
 end
 else
 begin
   FormDisplayDevice.StatusBarDisplayDevice.Font.Color:=clRed;
   FormDisplayDevice.StatusBarDisplayDevice.SimpleText:='Display device can not added/updated.';
 end;
end;

procedure TFormBhyveManager.SaveHostbridgeDevice(Sender: TObject);
var
  PciSlot : String;
begin
 if (FormHostbridgeDevice.FormAction = 'Add') and FormHostbridgeDevice.FormValidate() then
 begin
   PciSlot:='0.0.0';

   TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:=FormHostbridgeDevice.ComboBoxHostbridgeDevice.Text;

   HostBridgeDevice:=FillDetailHostbridgeDevice(TmpDevicesStringList.Text, PciSlot, FormHostbridgeDevice.ComboBoxHostbridgeDevice.Text);

   GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('Hostbridge'), 'device : '+HostBridgeDevice.device);
   GlobalNode.Data:=HostBridgeDevice;
   GlobalNode.ImageIndex:=6;
   GlobalNode.SelectedIndex:=6;

   DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+HostBridgeDevice.pci);

   FormHostbridgeDevice.Destroy;

   SaveVirtualMachineConfig();
 end
 else if (FormHostbridgeDevice.FormAction = 'Update') and FormHostbridgeDevice.FormValidate() then
 begin
   PciSlot:=HostBridgeDevice.pci;

   TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:=FormHostbridgeDevice.ComboBoxHostbridgeDevice.Text;

   DeviceSettingsTreeView.Items.FindNodeWithText('device : '+HostBridgeDevice.device).Text:='device : '+FormHostbridgeDevice.ComboBoxHostbridgeDevice.Text;

   HostBridgeDevice.device:=FormHostbridgeDevice.ComboBoxHostbridgeDevice.Text;

   FormHostbridgeDevice.Destroy;

   SaveVirtualMachineConfig();
 end
 else
 begin
   FormHostbridgeDevice.StatusBarHostbridgeDevice.Font.Color:=clRed;
   FormHostbridgeDevice.StatusBarHostbridgeDevice.SimpleText:='Hostbridge device can not added/updated.';
 end;
end;

procedure TFormBhyveManager.SaveInputDevice(Sender: TObject);
var
  PciSlot : String;
begin
 if (FormInputDevice.FormAction = 'Add') and FormInputDevice.FormValidate() then
 begin
   PciSlot:=GetNewPciSlotNumber(TmpDevicesStringList, 19);

   TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:='virtio-input';
   TmpDevicesStringList.Values['pci.'+PciSlot+'.path']:=FormInputDevice.ComboBoxInputDevice.Text;

   InputDevice:=FillDetailInputDevice(TmpDevicesStringList.Text, PciSlot, 'virtio-input');

   GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('Input'), 'device : '+InputDevice.device);
   GlobalNode.Data:=InputDevice;
   GlobalNode.ImageIndex:=7;
   GlobalNode.SelectedIndex:=7;

   DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+InputDevice.pci);

   FormInputDevice.Destroy;

   SaveVirtualMachineConfig();
 end
 else if (FormInputDevice.FormAction = 'Update') and FormInputDevice.FormValidate() then
 begin
   PciSlot:=InputDevice.pci;

   TmpDevicesStringList.Values['pci.'+PciSlot+'.path']:=FormInputDevice.ComboBoxInputDevice.Text;

   InputDevice.path:=FormInputDevice.ComboBoxInputDevice.Text;

   FormInputDevice.Destroy;

   SaveVirtualMachineConfig();
 end
 else
 begin
   FormInputDevice.StatusBarInputDevice.Font.Color:=clRed;
   FormInputDevice.StatusBarInputDevice.SimpleText:='Input device can not added/updated.';
 end;
end;

procedure TFormBhyveManager.SaveLpcDevice(Sender: TObject);
var
  PciSlot : String;
begin
 if (FormLpcDevice.FormAction = 'Add') and FormLpcDevice.FormValidate() then
 begin
   PciSlot:='0.1.0';

   TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:='lpc';

   { Remove when bhyve will updated on FreeBSD 13.x and 14.x }
   if GetOsreldate.ToInt64 >= 1500023 then
   begin
     {$ifdef CPUAMD64}
     TmpDevicesStringList.Values['bootrom']:=BootRomUefiPath+'/'+FormLpcDevice.ComboBoxBootrom.Text;
     {$endif CPUAMD64}
     {$ifdef CPUAARCH64}
     TmpDevicesStringList.Values['bootrom']:=BootRomUbootPath+'/'+FormLpcDevice.ComboBoxBootrom.Text;
     {$endif CPUAARCH64}
   end
   else
   begin
     {$ifdef CPUAMD64}
     TmpDevicesStringList.Values['lpc.bootrom']:=BootRomUefiPath+'/'+FormLpcDevice.ComboBoxBootrom.Text;
     {$endif CPUAMD64}
     {$ifdef CPUAARCH64}
     TmpDevicesStringList.Values['lpc.bootrom']:=BootRomUbootPath+'/'+FormLpcDevice.ComboBoxBootrom.Text;
     {$endif CPUAARCH64}
   end;

   TmpDevicesStringList.Values['lpc.fwcfg']:='bhyve';

   if FormLpcDevice.CheckBoxCom1.Checked then TmpDevicesStringList.Values['lpc.com1.path']:=FormLpcDevice.EditCom1.Text;
   if FormLpcDevice.CheckBoxCom2.Checked then TmpDevicesStringList.Values['lpc.com2.path']:=FormLpcDevice.EditCom2.Text;
   if FormLpcDevice.CheckBoxCom3.Checked then TmpDevicesStringList.Values['lpc.com3.path']:=FormLpcDevice.EditCom3.Text;
   if FormLpcDevice.CheckBoxCom4.Checked then TmpDevicesStringList.Values['lpc.com4.path']:=FormLpcDevice.EditCom4.Text;

   LPCDevice:=FillDetailLpcDevice(TmpDevicesStringList.Text, PciSlot, 'lpc');

   GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('LPC'), 'device : '+LPCDevice.device);
   GlobalNode.Data:=LPCDevice;
   GlobalNode.ImageIndex:=8;
   GlobalNode.SelectedIndex:=8;

   DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+LPCDevice.pci);

   FormLpcDevice.Destroy;

   SaveVirtualMachineConfig();
 end
 else if (FormLpcDevice.FormAction = 'Update') and FormLpcDevice.FormValidate() then
 begin
   PciSlot:=LPCDevice.pci;

   if FormLpcDevice.CheckBoxCom1.Checked then
   begin
     TmpDevicesStringList.Values['lpc.com1.path']:=FormLpcDevice.EditCom1.Text;
     LPCDevice.com1:=FormLpcDevice.EditCom1.Text;
   end
   else
   begin
     if TmpDevicesStringList.IndexOfName('lpc.com1.path') <> -1 then
       TmpDevicesStringList.Delete(TmpDevicesStringList.IndexOfName('lpc.com1.path'));
   end;

   if FormLpcDevice.CheckBoxCom2.Checked then
   begin
     TmpDevicesStringList.Values['lpc.com2.path']:=FormLpcDevice.EditCom2.Text;
     LPCDevice.com2:=FormLpcDevice.EditCom2.Text;
   end
   else
   begin
     if TmpDevicesStringList.IndexOfName('lpc.com2.path') <> -1 then
       TmpDevicesStringList.Delete(TmpDevicesStringList.IndexOfName('lpc.com2.path'));
   end;

   if FormLpcDevice.CheckBoxCom3.Checked then
   begin
     TmpDevicesStringList.Values['lpc.com3.path']:=FormLpcDevice.EditCom2.Text;
     LPCDevice.com3:=FormLpcDevice.EditCom3.Text;
   end
   else
   begin
     if TmpDevicesStringList.IndexOfName('lpc.com3.path') <> -1 then
       TmpDevicesStringList.Delete(TmpDevicesStringList.IndexOfName('lpc.com3.path'));
   end;

   if FormLpcDevice.CheckBoxCom4.Checked then
   begin
     TmpDevicesStringList.Values['lpc.com4.path']:=FormLpcDevice.EditCom2.Text;
     LPCDevice.com4:=FormLpcDevice.EditCom4.Text;
   end
   else
   begin
     if TmpDevicesStringList.IndexOfName('lpc.com4.path') <> -1 then
       TmpDevicesStringList.Delete(TmpDevicesStringList.IndexOfName('lpc.com4.path'));
   end;

   FormLpcDevice.Destroy;

   SaveVirtualMachineConfig();
 end
 else
 begin
   FormLpcDevice.StatusBarLpcDevice.Font.Color:=clRed;
   FormLpcDevice.StatusBarLpcDevice.SimpleText:='LPC device can not added/updated.';
 end;
end;

procedure TFormBhyveManager.SaveNetworkDevice(Sender: TObject);
var
  PciSlot : String;
begin
 if (FormNetworkDevice.FormAction = 'Add') and FormNetworkDevice.FormValidate() then
 begin
   PciSlot:=GetNewPciSlotNumber(TmpDevicesStringList, 10);

   TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:=FormNetworkDevice.ComboBoxDevice.Text;
   TmpDevicesStringList.Values['pci.'+PciSlot+'.backend']:=FormNetworkDevice.EditBackend.Text;
   TmpDevicesStringList.Values['pci.'+PciSlot+'.mac']:=FormNetworkDevice.EditMac.Text;
   if FormNetworkDevice.SpinEditExMtu.Text <> '1500' then TmpDevicesStringList.Values['pci.'+PciSlot+'.mtu']:=FormNetworkDevice.SpinEditExMtu.Text;

   NetworkDevice:=FillDetailNetworkDevice(TmpDevicesStringList.Text, PciSlot, FormNetworkDevice.ComboBoxDevice.Text);

   GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('Network'), 'device : '+NetworkDevice.device);
   GlobalNode.Data:=NetworkDevice;
   GlobalNode.ImageIndex:=9;
   GlobalNode.SelectedIndex:=9;

   DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+NetworkDevice.pci);

   FormNetworkDevice.Destroy;

   SaveVirtualMachineConfig();
 end
 else if (FormNetworkDevice.FormAction = 'Update') and FormNetworkDevice.FormValidate() then
 begin
   PciSlot:=NetworkDevice.pci;

   TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:=FormNetworkDevice.ComboBoxDevice.Text;

   if FormNetworkDevice.SpinEditExMtu.Value = 1500 then
   begin
     if TmpDevicesStringList.IndexOfName('pci.'+PciSlot+'.mtu') <> -1 then
       TmpDevicesStringList.Delete(TmpDevicesStringList.IndexOfName('pci.'+PciSlot+'.mtu'));
   end
   else
   begin
     TmpDevicesStringList.Values['pci.'+PciSlot+'.mtu']:=FormNetworkDevice.SpinEditExMtu.Text;
   end;

   DeviceSettingsTreeView.Items.FindNodeWithText('device : '+NetworkDevice.device).Text:='device : '+FormNetworkDevice.ComboBoxDevice.Text;

   NetworkDevice.device:=FormNetworkDevice.ComboBoxDevice.Text;
   NetworkDevice.mtu:=StrToInt(FormNetworkDevice.SpinEditExMtu.Text);

   FormNetworkDevice.Destroy;

   SaveVirtualMachineConfig();
 end
 else
 begin
   FormNetworkDevice.StatusBarNetworkDevice.Font.Color:=clRed;
   FormNetworkDevice.StatusBarNetworkDevice.SimpleText:='Network device can not added/updated.';
 end;
end;

procedure TFormBhyveManager.SavePassthruDevice(Sender: TObject);
var
  PciSlot : String;
begin
 if (FormPassthruDevice.FormAction = 'Add') and FormPassthruDevice.FormValidate() then
 begin
   PciSlot:=GetNewPciSlotNumber(TmpDevicesStringList, 14);

   TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:='passthru';
   TmpDevicesStringList.Values['pci.'+PciSlot+'.pptdev']:=FormPassthruDevice.ComboBoxDevice.Text;
   if FormPassthruDevice.FileNameEditRom.Text <> EmptyStr then TmpDevicesStringList.Values['pci.'+PciSlot+'.rom']:=FormPassthruDevice.FileNameEditRom.Text;

   PassthruDevice:=FillDetailPassthruDevice(TmpDevicesStringList.Text, PciSlot, 'passthru');

   GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('Passthru'), 'device : '+PassthruDevice.device+'-'+PassthruDevice.pptdev);
   GlobalNode.Data:=PassthruDevice;
   GlobalNode.ImageIndex:=11;
   GlobalNode.SelectedIndex:=11;

   DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+PassthruDevice.pci);

   FormPassthruDevice.Destroy;

   SaveVirtualMachineConfig();
 end
 else if (FormPassthruDevice.FormAction = 'Update') and FormPassthruDevice.FormValidate() then
 begin
   PciSlot:=PassthruDevice.pci;

   TmpDevicesStringList.Values['pci.'+PciSlot+'.pptdev']:=FormPassthruDevice.ComboBoxDevice.Text;

   if FormPassthruDevice.FileNameEditRom.Text = EmptyStr then
   begin
     if TmpDevicesStringList.IndexOfName('pci.'+PciSlot+'.rom') <> -1 then
       TmpDevicesStringList.Delete(TmpDevicesStringList.IndexOfName('pci.'+PciSlot+'.rom'));
   end
   else
   begin
     TmpDevicesStringList.Values['pci.'+PciSlot+'.rom']:=FormPassthruDevice.FileNameEditRom.Text;
   end;

   DeviceSettingsTreeView.Items.FindNodeWithText('device : '+PassthruDevice.device).Text:='device : '+FormPassthruDevice.ComboBoxDevice.Text;

   PassthruDevice.pptdev:=FormPassthruDevice.ComboBoxDevice.Text;

   FormPassthruDevice.Destroy;

   SaveVirtualMachineConfig();
 end
 else
 begin
    FormPassthruDevice.StatusBarPassthruDevice.Font.Color:=clRed;
    FormPassthruDevice.StatusBarPassthruDevice.SimpleText:='Passthru device can not added/updated.';
 end;
end;

procedure TFormBhyveManager.SaveShareFolderDevice(Sender: TObject);
var
  PciSlot : String;
begin
 if (FormShareFolderDevice.FormAction = 'Add') and FormShareFolderDevice.FormValidate() then
 begin
   PciSlot:=GetNewPciSlotNumber(TmpDevicesStringList, 22);

   TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:=FormShareFolderDevice.ComboBoxDevice.Text;
   TmpDevicesStringList.Values['pci.'+PciSlot+'.sharename']:=FormShareFolderDevice.EditSharename.Text;
   TmpDevicesStringList.Values['pci.'+PciSlot+'.path']:=FormShareFolderDevice.DirectoryEditPath.Text;
   if FormShareFolderDevice.CheckBoxReadOnly.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.ro']:=BoolToStr(FormShareFolderDevice.CheckBoxReadOnly.Checked, 'true', 'false');

   ShareFolderDevice:=FillDetailShareFolderDevice(TmpDevicesStringList.Text, PciSlot, FormShareFolderDevice.ComboBoxDevice.Text);

   GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('Shared folders'), 'device : '+ShareFolderDevice.device);
   GlobalNode.Data:=ShareFolderDevice;
   GlobalNode.ImageIndex:=13;
   GlobalNode.SelectedIndex:=13;

   DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+ShareFolderDevice.pci);

   FormShareFolderDevice.Destroy;

   SaveVirtualMachineConfig();
 end
 else if (FormShareFolderDevice.FormAction = 'Update') and FormShareFolderDevice.FormValidate() then
 begin
   PciSlot:=ShareFolderDevice.pci;

   TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:=FormShareFolderDevice.ComboBoxDevice.Text;
   TmpDevicesStringList.Values['pci.'+PciSlot+'.sharename']:=FormShareFolderDevice.EditSharename.Text;
   TmpDevicesStringList.Values['pci.'+PciSlot+'.path']:=FormShareFolderDevice.DirectoryEditPath.Text;

   if FormShareFolderDevice.CheckBoxReadOnly.Checked then
     TmpDevicesStringList.Values['pci.'+PciSlot+'.ro']:=BoolToStr(FormShareFolderDevice.CheckBoxReadOnly.Checked, 'true', 'false')
   else
   begin
     if TmpDevicesStringList.IndexOfName('pci.'+PciSlot+'.ro') <> -1 then
       TmpDevicesStringList.Delete(TmpDevicesStringList.IndexOfName('pci.'+PciSlot+'.ro'));
   end;


   DeviceSettingsTreeView.Items.FindNodeWithText('device : '+ShareFolderDevice.device).Text:='device : '+FormShareFolderDevice.ComboBoxDevice.Text;

   ShareFolderDevice.device:=FormShareFolderDevice.ComboBoxDevice.Text;

   FormShareFolderDevice.Destroy;

   SaveVirtualMachineConfig();
 end
 else
 begin
   FormShareFolderDevice.StatusBarSharefolderDevice.Font.Color:=clRed;
   FormShareFolderDevice.StatusBarSharefolderDevice.SimpleText:='Sharefolder device can not added/updated.';
 end;
end;

procedure TFormBhyveManager.SaveStorageDevice(Sender: TObject);
var
  PciSlot : String;
  StoragePath : String;
  StorageSize : String;
  i : Integer;
begin
  StoragePath:=FormStorageDevice.FileNameEditStoragePath.Text;
  StorageSize:=FormStorageDevice.SpinEditExDiskSize.Text;

  if (FormStorageDevice.FormAction = 'Add') and FormStorageDevice.FormValidate() then
  begin
    case FormStorageDevice.ComboBoxStorageDevice.Text of
        'ahci-cd':
          begin
            PciSlot:=GetNewPciSlotNumber(TmpDevicesStringList, 2);

            TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:='ahci';
            TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.path']:=FormStorageDevice.FileNameEditStoragePath.Text;
            TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.type']:='cd';

            if FormStorageDevice.EditSer.Text <> EmptyStr then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.ser']:=FormStorageDevice.EditSer.Text;
            if FormStorageDevice.CheckBoxNoCache.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.nocache']:='true';
            if FormStorageDevice.CheckBoxNoDelete.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.nodelete']:='true';
            if FormStorageDevice.CheckBoxSync.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.sync']:='true';
            if FormStorageDevice.CheckBoxReadOnly.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.ro']:='true';

            StorageAhciDevice:=FillDetailStorageAhciDevice(TmpDevicesStringList.Text, PciSlot, 'ahci', 0);

            StorageAhciDevice.storage_size:='0G';
            StorageAhciDevice.storage_type:='image file';

            GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('Storage'), 'device : '+StorageAhciDevice.device+'-'+StorageAhciDevice.device_type);
            GlobalNode.Data:=StorageAhciDevice;
            GlobalNode.ImageIndex:=2;
            GlobalNode.SelectedIndex:=2;

            DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+StorageAhciDevice.pci);
            DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'port : '+IntToStr(StorageAhciDevice.port));

            FormStorageDevice.Destroy;

            SaveVirtualMachineConfig();
          end;
        'ahci-hd':
          begin
            PciSlot:=GetNewPciSlotNumber(TmpDevicesStringList, 3);

            TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:='ahci';
            TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.path']:=StoragePath;
            TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.type']:='hd';

            if FormStorageDevice.EditSer.Text <> EmptyStr then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.ser']:=FormStorageDevice.EditSer.Text;
            if FormStorageDevice.CheckBoxNoCache.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.nocache']:='true';
            if FormStorageDevice.CheckBoxNoDelete.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.nodelete']:='true';
            if FormStorageDevice.CheckBoxSync.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.sync']:='true';
            if FormStorageDevice.CheckBoxReadOnly.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.ro']:='true';
            if FormStorageDevice.EditAhciSectorSize.Text <> EmptyStr then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.sectorsize']:=FormStorageDevice.EditAhciSectorSize.Text;
            if FormStorageDevice.EditAhciRev.Text <> '001' then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.rev']:=FormStorageDevice.EditAhciRev.Text;
            if FormStorageDevice.EditAhciModel.Text <> EmptyStr then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.model']:=FormStorageDevice.EditAhciModel.Text;
            if FormStorageDevice.ComboBoxAhciNmrr.Text <> '0' then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.nmrr']:=FormStorageDevice.ComboBoxAhciNmrr.Text;

            StorageAhciDevice:=FillDetailStorageAhciDevice(TmpDevicesStringList.Text, PciSlot, 'ahci', 0);

            StorageAhciDevice.storage_size:=StorageSize+'G';
            StorageAhciDevice.storage_type:=FormStorageDevice.ComboBoxStorageType.Text;

            case StorageAhciDevice.storage_type of
                'image file':
                  begin
                    CreateFile(StoragePath, GetCurrentUserName());
                    TruncateImage(StoragePath, StorageSize+'G');
                  end;
                'zfs sparse volume': ZfsCreateZvol(StoragePath.Remove(0,10), StorageSize+'G' , True);
                'zfs volume': ZfsCreateZvol(StoragePath.Remove(0,10), StorageSize+'G' , False);
            end;

            GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('Storage'), 'device : '+StorageAhciDevice.device+'-'+StorageAhciDevice.device_type);
            GlobalNode.Data:=StorageAhciDevice;
            GlobalNode.ImageIndex:=5;
            GlobalNode.SelectedIndex:=5;

            DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+StorageAhciDevice.pci);
            DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'port : '+IntToStr(StorageAhciDevice.port));

            FormStorageDevice.Destroy;

            SaveVirtualMachineConfig();
          end;
        'nvme':
          begin
            PciSlot:=GetNewPciSlotNumber(TmpDevicesStringList, 4);

            TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:=FormStorageDevice.ComboBoxStorageDevice.Text;
            TmpDevicesStringList.Values['pci.'+PciSlot+'.path']:=FormStorageDevice.FileNameEditStoragePath.Text;

            if FormStorageDevice.EditSer.Text <> EmptyStr then TmpDevicesStringList.Values['pci.'+PciSlot+'.ser']:=FormStorageDevice.EditSer.Text;
            if FormStorageDevice.CheckBoxNoCache.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.nocache']:='true';
            if FormStorageDevice.CheckBoxNoDelete.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.nodelete']:='true';
            if FormStorageDevice.CheckBoxSync.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.sync']:='true';
            if FormStorageDevice.CheckBoxReadOnly.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.ro']:='true';
            if FormStorageDevice.EditNvmeMaxq.Text <> '16' then TmpDevicesStringList.Values['pci.'+PciSlot+'.maxq']:=FormStorageDevice.EditNvmeMaxq.Text;
            if FormStorageDevice.EditNvmeEui64.Text <> EmptyStr then TmpDevicesStringList.Values['pci.'+PciSlot+'.eui64']:=FormStorageDevice.EditNvmeEui64.Text;
            if FormStorageDevice.EditNvmeIoslots.Text <> '8' then TmpDevicesStringList.Values['pci.'+PciSlot+'.ioslots']:=FormStorageDevice.EditNvmeIoslots.Text;
            if FormStorageDevice.EditNvmeQsz.Text <> '2058' then TmpDevicesStringList.Values['pci.'+PciSlot+'.qsz']:=FormStorageDevice.EditNvmeQsz.Text;
            if FormStorageDevice.EditNvmeSectsz.Text <> EmptyStr then TmpDevicesStringList.Values['pci.'+PciSlot+'.sectsz']:=FormStorageDevice.EditNvmeSectsz.Text;
            if FormStorageDevice.ComboBoxNvmeDsm.Text <> 'auto' then TmpDevicesStringList.Values['pci.'+PciSlot+'.dsm']:=FormStorageDevice.ComboBoxNvmeDsm.Text;

            StorageNvmeDevice:=FillDetailStorageNvmeDevice(TmpDevicesStringList.Text, PciSlot, FormStorageDevice.ComboBoxStorageDevice.Text);

            if FormStorageDevice.CheckBoxNvmUseRam.Checked then
            begin
              TmpDevicesStringList.Values['pci.'+PciSlot+'.ram']:=FormStorageDevice.SpinEditExNvmeRam.Text;
              StorageNvmeDevice.storage_size:='0G';
            end
            else
            begin
              StorageNvmeDevice.storage_size:=FormStorageDevice.SpinEditExDiskSize.Text+'G';
            end;

            StorageNvmeDevice.storage_type:=FormStorageDevice.ComboBoxStorageType.Text;

            case StorageNvmeDevice.storage_type of
                'image file':
                  begin
                    CreateFile(StoragePath, GetCurrentUserName());
                    TruncateImage(StoragePath, StorageSize+'G');
                  end;
                'zfs sparse volume': ZfsCreateZvol(StoragePath.Remove(0,10), StorageSize+'G' , True);
                'zfs volume': ZfsCreateZvol(StoragePath.Remove(0,10), StorageSize+'G' , False);
            end;

            GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('Storage'), 'device : '+StorageNvmeDevice.device);
            GlobalNode.Data:=StorageNvmeDevice;
            GlobalNode.ImageIndex:=10;
            GlobalNode.SelectedIndex:=10;

            DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+StorageNvmeDevice.pci);

            FormStorageDevice.Destroy;

            SaveVirtualMachineConfig();
          end;
        'virtio-blk':
          begin
            PciSlot:=GetNewPciSlotNumber(TmpDevicesStringList, 6);

            TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:=FormStorageDevice.ComboBoxStorageDevice.Text;
            TmpDevicesStringList.Values['pci.'+PciSlot+'.path']:=FormStorageDevice.FileNameEditStoragePath.Text;

            if FormStorageDevice.EditSer.Text <> EmptyStr then TmpDevicesStringList.Values['pci.'+PciSlot+'.ser']:=FormStorageDevice.EditSer.Text;
            if FormStorageDevice.CheckBoxNoCache.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.nocache']:='true';
            if FormStorageDevice.CheckBoxNoDelete.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.nodelete']:='true';
            if FormStorageDevice.CheckBoxSync.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.sync']:='true';
            if FormStorageDevice.CheckBoxReadOnly.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.ro']:='true';

            StorageVirtioBlkDevice:=FillDetailStorageVirtioBlkDevice(TmpDevicesStringList.Text, PciSlot, FormStorageDevice.ComboBoxStorageDevice.Text);

            StorageNvmeDevice.storage_size:=FormStorageDevice.SpinEditExDiskSize.Text+'G';
            StorageNvmeDevice.storage_type:=FormStorageDevice.ComboBoxStorageType.Text;

            case StorageVirtioBlkDevice.storage_type of
                'image file':
                  begin
                    CreateFile(StoragePath, GetCurrentUserName());
                    TruncateImage(StoragePath, StorageSize+'G');
                  end;
                'zfs sparse volume': ZfsCreateZvol(StoragePath.Remove(0,10), StorageSize+'G' , True);
                'zfs volume': ZfsCreateZvol(StoragePath.Remove(0,10), StorageSize+'G' , False);
            end;

            GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('Storage'), 'device : '+StorageVirtioBlkDevice.device);
            GlobalNode.Data:=StorageVirtioBlkDevice;
            GlobalNode.ImageIndex:=5;
            GlobalNode.SelectedIndex:=5;

            DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+StorageVirtioBlkDevice.pci);

            FormStorageDevice.Destroy;

            SaveVirtualMachineConfig();
          end;
    end;
  end
  else if (FormStorageDevice.FormAction = 'Update') and FormStorageDevice.FormValidate() then
  begin
        case FormStorageDevice.ComboBoxStorageDevice.Text of
            'ahci-cd':
              begin
                PciSlot:=StorageAhciDevice.pci;

                for i:=TmpDevicesStringList.Count-1 downto 0 do
                begin
                    if (TmpDevicesStringList[i].Contains('pci.'+PciSlot)) then
                    begin
                      TmpDevicesStringList.Delete(i);
                    end;
                end;

                TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:=StorageAhciDevice.device;
                TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.path']:=FormStorageDevice.FileNameEditStoragePath.Text;
                TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.type']:=StorageAhciDevice.device_type;

                StorageAhciDevice.path:=FormStorageDevice.FileNameEditStoragePath.Text;

                if FormStorageDevice.EditSer.Text <> EmptyStr then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.ser']:=FormStorageDevice.EditSer.Text;
                if FormStorageDevice.CheckBoxNoCache.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.nocache']:='true';
                if FormStorageDevice.CheckBoxNoDelete.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.nodelete']:='true';
                if FormStorageDevice.CheckBoxSync.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.sync']:='true';
                if FormStorageDevice.CheckBoxReadOnly.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.ro']:='true';

                if StorageAhciDevice.ser <> FormStorageDevice.EditSer.Text then StorageAhciDevice.ser:=FormStorageDevice.EditSer.Text;
                if StorageAhciDevice.nocache <> FormStorageDevice.CheckBoxNoCache.Checked then StorageAhciDevice.nocache:=FormStorageDevice.CheckBoxNoCache.Checked;
                if StorageAhciDevice.nodelete <> FormStorageDevice.CheckBoxNoDelete.Checked then StorageAhciDevice.nodelete:=FormStorageDevice.CheckBoxNoDelete.Checked;
                if StorageAhciDevice.sync <> FormStorageDevice.CheckBoxSync.Checked then StorageAhciDevice.sync:=FormStorageDevice.CheckBoxSync.Checked;
                if StorageAhciDevice.ro <> FormStorageDevice.CheckBoxReadOnly.Checked then StorageAhciDevice.ro:=FormStorageDevice.CheckBoxReadOnly.Checked;

                FormStorageDevice.Destroy;

                SaveVirtualMachineConfig();
              end;
            'ahci-hd':
              begin
                PciSlot:=StorageAhciDevice.pci;

                for i:=TmpDevicesStringList.Count-1 downto 0 do
                begin
                    if (TmpDevicesStringList[i].Contains('pci.'+PciSlot)) then
                    begin
                      TmpDevicesStringList.Delete(i);
                    end;
                end;

                TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:=StorageAhciDevice.device;
                TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.path']:=StorageAhciDevice.path;
                TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.type']:=StorageAhciDevice.device_type;

                if FormStorageDevice.EditSer.Text <> EmptyStr then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.ser']:=FormStorageDevice.EditSer.Text;
                if FormStorageDevice.CheckBoxNoCache.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.nocache']:='true';
                if FormStorageDevice.CheckBoxNoDelete.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.nodelete']:='true';
                if FormStorageDevice.CheckBoxSync.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.sync']:='true';
                if FormStorageDevice.CheckBoxReadOnly.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.ro']:='true';
                if FormStorageDevice.EditAhciSectorSize.Text <> '16' then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.sectorsize']:=FormStorageDevice.EditAhciSectorSize.Text;
                if FormStorageDevice.EditAhciRev.Text <> '001' then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.rev']:=FormStorageDevice.EditAhciRev.Text;
                if FormStorageDevice.EditAhciModel.Text <> EmptyStr then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.model']:=FormStorageDevice.EditAhciModel.Text;
                if FormStorageDevice.ComboBoxAhciNmrr.Text <> '0' then TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.nmrr']:=FormStorageDevice.ComboBoxAhciNmrr.Text;

                if StorageAhciDevice.ser <> FormStorageDevice.EditSer.Text then StorageAhciDevice.ser:=FormStorageDevice.EditSer.Text;
                if StorageAhciDevice.nocache <> FormStorageDevice.CheckBoxNoCache.Checked then StorageAhciDevice.nocache:=FormStorageDevice.CheckBoxNoCache.Checked;
                if StorageAhciDevice.nodelete <> FormStorageDevice.CheckBoxNoDelete.Checked then StorageAhciDevice.nodelete:=FormStorageDevice.CheckBoxNoDelete.Checked;
                if StorageAhciDevice.sync <> FormStorageDevice.CheckBoxSync.Checked then StorageAhciDevice.sync:=FormStorageDevice.CheckBoxSync.Checked;
                if StorageAhciDevice.ro <> FormStorageDevice.CheckBoxReadOnly.Checked then StorageAhciDevice.ro:=FormStorageDevice.CheckBoxReadOnly.Checked;
                if StorageAhciDevice.sectorsize <> FormStorageDevice.EditAhciSectorSize.Text then StorageAhciDevice.sectorsize:=FormStorageDevice.EditAhciSectorSize.Text;
                if StorageAhciDevice.rev <> FormStorageDevice.EditAhciRev.Text then StorageAhciDevice.rev:=FormStorageDevice.EditAhciRev.Text;
                if StorageAhciDevice.model <> FormStorageDevice.EditAhciModel.Text then StorageAhciDevice.model:=FormStorageDevice.EditAhciModel.Text;
                if StorageAhciDevice.nmrr.ToString <> FormStorageDevice.ComboBoxAhciNmrr.Text then StorageAhciDevice.nmrr:=StrToInt(FormStorageDevice.ComboBoxAhciNmrr.Text);

                FormStorageDevice.Destroy;

                SaveVirtualMachineConfig();
              end;
            'nvme':
              begin
                PciSlot:=StorageNvmeDevice.pci;

                for i:=TmpDevicesStringList.Count-1 downto 0 do
                begin
                    if (TmpDevicesStringList[i].Contains('pci.'+PciSlot)) then
                    begin
                      TmpDevicesStringList.Delete(i);
                    end;
                end;

                TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:=StorageNvmeDevice.device;
                TmpDevicesStringList.Values['pci.'+PciSlot+'.path']:=StorageNvmeDevice.devpath;

                if FormStorageDevice.EditSer.Text <> EmptyStr then TmpDevicesStringList.Values['pci.'+PciSlot+'.ser']:=FormStorageDevice.EditSer.Text;
                if FormStorageDevice.CheckBoxNoCache.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.nocache']:='true';
                if FormStorageDevice.CheckBoxNoDelete.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.nodelete']:='true';
                if FormStorageDevice.CheckBoxSync.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.sync']:='true';
                if FormStorageDevice.CheckBoxReadOnly.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.ro']:='true';
                if FormStorageDevice.EditNvmeMaxq.Text <> '16' then TmpDevicesStringList.Values['pci.'+PciSlot+'.maxq']:=FormStorageDevice.EditNvmeMaxq.Text;
                if FormStorageDevice.EditNvmeEui64.Text <> EmptyStr then TmpDevicesStringList.Values['pci.'+PciSlot+'.eui64']:=FormStorageDevice.EditNvmeEui64.Text;
                if FormStorageDevice.EditNvmeIoslots.Text <> '8' then TmpDevicesStringList.Values['pci.'+PciSlot+'.ioslots']:=FormStorageDevice.EditNvmeIoslots.Text;
                if FormStorageDevice.EditNvmeQsz.Text <> '2058' then TmpDevicesStringList.Values['pci.'+PciSlot+'.qsz']:=FormStorageDevice.EditNvmeQsz.Text;
                if FormStorageDevice.EditNvmeSectsz.Text <> EmptyStr then TmpDevicesStringList.Values['pci.'+PciSlot+'.sectsz']:=FormStorageDevice.EditNvmeSectsz.Text;
                if FormStorageDevice.ComboBoxNvmeDsm.Text <> 'auto' then TmpDevicesStringList.Values['pci.'+PciSlot+'.dsm']:=FormStorageDevice.ComboBoxNvmeDsm.Text;

                if StorageNvmeDevice.ser <> FormStorageDevice.EditSer.Text then StorageNvmeDevice.ser:=FormStorageDevice.EditSer.Text;
                if StorageNvmeDevice.nocache <> FormStorageDevice.CheckBoxNoCache.Checked then StorageNvmeDevice.nocache:=FormStorageDevice.CheckBoxNoCache.Checked;
                if StorageNvmeDevice.nodelete <> FormStorageDevice.CheckBoxNoDelete.Checked then StorageNvmeDevice.nodelete:=FormStorageDevice.CheckBoxNoDelete.Checked;
                if StorageNvmeDevice.sync <> FormStorageDevice.CheckBoxSync.Checked then StorageNvmeDevice.sync:=FormStorageDevice.CheckBoxSync.Checked;
                if StorageNvmeDevice.ro <> FormStorageDevice.CheckBoxReadOnly.Checked then StorageNvmeDevice.ro:=FormStorageDevice.CheckBoxReadOnly.Checked;
                if StorageNvmeDevice.maxq.ToString <> FormStorageDevice.EditNvmeMaxq.Text then StorageNvmeDevice.maxq:=StrToInt(FormStorageDevice.EditNvmeMaxq.Text);
                if StorageNvmeDevice.eui64.ToString <> FormStorageDevice.EditNvmeEui64.Text then StorageNvmeDevice.eui64:=StrToInt(FormStorageDevice.EditNvmeEui64.Text);
                if StorageNvmeDevice.ioslots.ToString <> FormStorageDevice.EditNvmeIoslots.Text then StorageNvmeDevice.ioslots:=StrToInt(FormStorageDevice.EditNvmeIoslots.Text);
                if StorageNvmeDevice.qsz.ToString <> FormStorageDevice.EditNvmeQsz.Text then StorageNvmeDevice.qsz:=StrToInt(FormStorageDevice.EditNvmeQsz.Text);
                if StorageNvmeDevice.sectsz.ToString <> FormStorageDevice.EditNvmeSectsz.Text then StorageNvmeDevice.sectsz:=StrToInt(FormStorageDevice.EditNvmeSectsz.Text);
                if StorageNvmeDevice.dsm <> FormStorageDevice.ComboBoxNvmeDsm.Text then StorageNvmeDevice.dsm:=FormStorageDevice.ComboBoxNvmeDsm.Text;

                FormStorageDevice.Destroy;

                SaveVirtualMachineConfig();
              end;
            'virtio-blk':
              begin
                PciSlot:=StorageVirtioBlkDevice.pci;

                for i:=TmpDevicesStringList.Count-1 downto 0 do
                begin
                    if (TmpDevicesStringList[i].Contains('pci.'+PciSlot)) then
                    begin
                      TmpDevicesStringList.Delete(i);
                    end;
                end;

                TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:=StorageVirtioBlkDevice.device;
                TmpDevicesStringList.Values['pci.'+PciSlot+'.path']:=StorageVirtioBlkDevice.path;

                if FormStorageDevice.EditSer.Text <> EmptyStr then TmpDevicesStringList.Values['pci.'+PciSlot+'.ser']:=FormStorageDevice.EditSer.Text;
                if FormStorageDevice.CheckBoxNoCache.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.nocache']:='true';
                if FormStorageDevice.CheckBoxNoDelete.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.nodelete']:='true';
                if FormStorageDevice.CheckBoxSync.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.sync']:='true';
                if FormStorageDevice.CheckBoxReadOnly.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.ro']:='true';

                if StorageVirtioBlkDevice.ser <> FormStorageDevice.EditSer.Text then StorageVirtioBlkDevice.ser:=FormStorageDevice.EditSer.Text;
                if StorageVirtioBlkDevice.nocache <> FormStorageDevice.CheckBoxNoCache.Checked then StorageVirtioBlkDevice.nocache:=FormStorageDevice.CheckBoxNoCache.Checked;
                if StorageVirtioBlkDevice.nodelete <> FormStorageDevice.CheckBoxNoDelete.Checked then StorageVirtioBlkDevice.nodelete:=FormStorageDevice.CheckBoxNoDelete.Checked;
                if StorageVirtioBlkDevice.sync <> FormStorageDevice.CheckBoxSync.Checked then StorageVirtioBlkDevice.sync:=FormStorageDevice.CheckBoxSync.Checked;
                if StorageVirtioBlkDevice.ro <> FormStorageDevice.CheckBoxReadOnly.Checked then StorageVirtioBlkDevice.ro:=FormStorageDevice.CheckBoxReadOnly.Checked;

                FormStorageDevice.Destroy;

                SaveVirtualMachineConfig();
              end;
        end;
  end
  else
  begin
    FormStorageDevice.StatusBarStorageDevice.Font.Color:=clRed;
    FormStorageDevice.StatusBarStorageDevice.SimpleText:='Storage device can not added/updated.';
  end;
end;

procedure TFormBhyveManager.CreateVmClick(Sender: TObject);
var
  NewVMConfig : ConfigurationClass;
  NewBhyveConfig : TStringList;
  Uuid : String;
  DiskName : String;
  IpAddress : String;
  MacAddress : String;
  Path : String = '.path';
  PciSlot : String;
begin
  if FormVmCreate.FormValidate() then
  begin
    NewBhyveConfig:=TStringList.Create;

    if UseZfs = 'yes' then
      ZfsCreateDataset(VmPath.Remove(0,1)+'/'+FormVmCreate.EditVmName.Text)
    else
      CreateDirectory(VmPath+'/'+FormVmCreate.EditVmName.Text, GetCurrentUserName());


    CreateFile(FormVmCreate.EditVmFolderPath.Text+'/'+FormVmCreate.EditVmName.Text+'.conf', GetCurrentUserName());
    NewVMConfig:=ConfigurationClass.Create(FormVmCreate.EditVmFolderPath.Text+'/'+FormVmCreate.EditVmName.Text+'.conf');

    Uuid:=GenerateUuid();

    NewBhyveConfig.Values['uuid']:=Uuid;
    NewBhyveConfig.Values['name']:=FormVmCreate.EditVmName.Text;
    NewBhyveConfig.Values['memory.size']:=FormVmCreate.SpinEditExMemory.Text+'M';
    NewBhyveConfig.Values['cpus']:='1';
    NewBhyveConfig.Values['sockets']:='1';
    NewBhyveConfig.Values['cores']:='1';
    NewBhyveConfig.Values['rtc.use_localtime']:='true';

    NewBhyveConfig.Values['acpi_tables']:='false';
    NewBhyveConfig.Values['x86.vmexit_on_hlt']:='true';
    NewBhyveConfig.Values['x86.strictmsr']:='false';

    { Remove when bhyve will updated on FreeBSD 13.x and 14.x }
    if GetOsreldate.ToInt64 >= 1500023 then
    begin
      {$ifdef CPUAMD64}
      NewBhyveConfig.Values['bootrom'] := BootRomUefiPath+ '/' +'BHYVE_UEFI.fd'
      {$endif CPUAMD64}
      {$ifdef CPUAARCH64}
      NewBhyveConfig.Values['bootrom'] := BootRomUbootPath+ '/' +'u-boot.bin';
      {$endif CPUAARCH64}
    end
    else
    begin
      {$ifdef CPUAMD64}
      NewBhyveConfig.Values['lpc.bootrom']:=BootRomUefiPath+ '/' +'BHYVE_UEFI.fd';
      {$endif CPUAMD64}
      {$ifdef CPUAARCH64}
      NewBhyveConfig.Values['lpc.bootrom']:=BootRomUbootPath+ '/' +'u-boot.bin';
      {$endif CPUAARCH64}
    end;

    NewBhyveConfig.Values['lpc.fwcfg']:='bhyve';
    NewBhyveConfig.Values['lpc.com1.path']:='/dev/nmdm-'+FormVmCreate.EditVmName.Text+'.1A';
    NewBhyveConfig.Values['pci.0.0.0.device']:='hostbridge';
    NewBhyveConfig.Values['pci.0.1.0.device']:='lpc';

    if FormVmCreate.CheckBoxUseMedia.Checked then
    begin
      NewBhyveConfig.Values['pci.0.2.0.device']:='ahci';
      NewBhyveConfig.Values['pci.0.2.0.port.0.type']:='cd';
      NewBhyveConfig.Values['pci.0.2.0.port.0.path']:=FormVmCreate.FileNameEditBootMedia.Text;
    end;

    if not FormVmCreate.RadioButtonNotDisk.Checked then
    begin
      case FormVmCreate.ComboBoxVirtualDeviceType.Text of
        'ahci-hd':
          begin
            PciSlot:='3';
            NewBhyveConfig.Values['pci.0.'+PciSlot+'.0.device']:='ahci';
            NewBhyveConfig.Values['pci.0.'+PciSlot+'.0.port.0.type']:='hd';
            Path := '.port.0.path';
          end;
        'nvme':
          begin
            PciSlot:='4';
            NewBhyveConfig.Values['pci.0.'+PciSlot+'.0.device']:=FormVmCreate.ComboBoxVirtualDeviceType.Text;
          end;
        'virtio-blk':
          begin
            PciSlot:='6';
            NewBhyveConfig.Values['pci.0.'+PciSlot+'.0.device']:=FormVmCreate.ComboBoxVirtualDeviceType.Text;
          end;
        'virtio-scsi':
          begin
            PciSlot:='8';
            NewBhyveConfig.Values['pci.0.'+PciSlot+'.0.device']:=FormVmCreate.ComboBoxVirtualDeviceType.Text;
          end;
      end;

      case FormVmCreate.ComboBoxVirtualStorageType.Text of
        'image file':
          begin
            DiskName:=GetNewStorageName(FormVmCreate.EditVmFolderPath.Text+'/', False);
            CreateFile(FormVmCreate.EditVmFolderPath.Text+'/'+DiskName, GetCurrentUserName());
            TruncateImage(FormVmCreate.EditVmFolderPath.Text+'/'+DiskName, FormVmCreate.SpinEditExDiskSize.Text+'G');
            NewBhyveConfig.Values['pci.0.'+PciSlot+'.0'+Path]:=FormVmCreate.EditVmFolderPath.Text+'/'+DiskName;
          end;
        'zfs sparse volume':
          begin
            if UseZfs = 'yes' then
            begin
              DiskName:=GetNewStorageName('/dev/zvol'+VmPath+'/'+FormVmCreate.EditVmName.Text, True);
              ZfsCreateZvol(VmPath.Remove(0,1)+'/'+FormVmCreate.EditVmName.Text+'/'+DiskName, FormVmCreate.SpinEditExDiskSize.Text+'G' , True);
              NewBhyveConfig.Values['pci.0.'+PciSlot+'.0'+Path]:='/dev/zvol'+VmPath+'/'+FormVmCreate.EditVmName.Text+'/'+DiskName;
            end;
          end;
        'zfs volume':
          begin
            if UseZfs = 'yes' then
            begin
              DiskName:=GetNewStorageName('/dev/zvol'+VmPath+'/'+FormVmCreate.EditVmName.Text, True);
              ZfsCreateZvol(VmPath.Remove(0,1)+'/'+FormVmCreate.EditVmName.Text+'/'+DiskName, FormVmCreate.SpinEditExDiskSize.Text+'G' , False);
              NewBhyveConfig.Values['pci.0.'+PciSlot+'.0'+Path]:='/dev/zvol'+VmPath+'/'+FormVmCreate.EditVmName.Text+'/'+DiskName;
            end;
          end;
      end;
    end;

    MacAddress:=GenerateMacAddress();

    NewBhyveConfig.Values['pci.0.10.0.device']:='virtio-net';
    NewBhyveConfig.Values['pci.0.10.0.backend']:=GetNewNetworkName('tap');
    NewBhyveConfig.Values['pci.0.10.0.mac']:=MacAddress;

    if FormVmCreate.CheckBoxFramebuffer.Checked then
    begin
      NewBhyveConfig.Values['pci.0.30.0.device']:='fbuf';
      if FormVmCreate.CheckBoxOnlyLocalhost.Checked then
        NewBhyveConfig.Values['pci.0.30.0.tcp']:='127.0.0.1:'+GetNewVncPortNumber()
      else
        NewBhyveConfig.Values['pci.0.30.0.tcp']:='0.0.0.0:'+GetNewVncPortNumber();

      if FormVmCreate.CheckBoxWaitVNC.Checked then
        NewBhyveConfig.Values['pci.0.30.0.wait']:='true';
    end;

    NewBhyveConfig.Values['pci.0.31.0.device']:='xhci';
    NewBhyveConfig.Values['pci.0.31.0.slot.1.device']:='tablet';

    NewBhyveConfig.Sorted:=True;

    CreateFile(FormVmCreate.EditVmFolderPath.Text+'/bhyve_config.conf', GetCurrentUserName());
    NewBhyveConfig.SaveToFile(FormVmCreate.EditVmFolderPath.Text+'/bhyve_config.conf');

    NewVMConfig.SetOption('general','name', FormVmCreate.EditVmName.Text);
    NewVMConfig.SetOption('general','description', FormVmCreate.EditVmDescription.Text);
    NewVMConfig.SetOption('general','uuid', Uuid);
    NewVMConfig.SetOption('general','config_path', FormVmCreate.EditVmFolderPath.Text+'/bhyve_config.conf');
    NewVMConfig.SetOption('general','type' , FormVmCreate.ComboBoxSystemType.Text);
    NewVMConfig.SetOption('general','version', FormVmCreate.ComboBoxSystemVersion.Text);
    NewVMConfig.SetOption('general','image', PtrInt(FormVmCreate.ComboBoxSystemVersion.Items.Objects[FormVmCreate.ComboBoxSystemVersion.ItemIndex]).ToString);
    NewVMConfig.SetOption('general','rdp', 'False');

    if UseDnsmasq = 'yes' then
    begin
      IpAddress:=GetNewIpAddress(GetSubnet);
      NewVMConfig.SetOption('general','ipaddress', IpAddress );
      AddDnsmasqEntry(FormVmCreate.EditVmName.Text, IpAddress, MacAddress);
    end;

    NewVMConfig. Free;
    NewBhyveConfig.Free;

    ResetTreeView(VirtualMachinesTreeView);
    VirtualMachinesTreeView.Items.Clear;
    FillVirtualMachineList();

    FormVmCreate.Hide;

    MessageDlg('Create virtual machine information', 'A new '+FormVmCreate.EditVmName.Text+' VM was created', mtInformation, [mbOK], 0);

    StatusBarBhyveManager.Font.Color:=clTeal;
    StatusBarBhyveManager.SimpleText:='A new '+FormVmCreate.EditVmName.Text+' VM was created';
  end
  else
  begin
    FormVmCreate.StatusBarVmCreate.Font.Color:=clRed;
    FormVmCreate.StatusBarVmCreate.SimpleText:='You must fill all requeriments';
  end;
end;

procedure TFormBhyveManager.SaveVirtualMachineInfoClick(Sender: TObject);
var
  Configuration : ConfigurationClass;
begin
  if FormVmInfo.FormValidate() then
  begin
    Configuration:= ConfigurationClass.Create(VmPath+ '/'+FormVmInfo.EditVmName.Text+'/'+FormVmInfo.EditVmName.Text+'.conf');

    Configuration.SetOption('general','type' , FormVmInfo.ComboBoxVmType.Text);
    Configuration.SetOption('general','version', FormVmInfo.ComboBoxVmVersion.Text);
    Configuration.SetOption('general','image', PtrInt(FormVmInfo.ComboBoxVmVersion.Items.Objects[FormVmInfo.ComboBoxVmVersion.ItemIndex]).ToString);
    Configuration.SetOption('general','description', FormVmInfo.EditVmDescription.Text);
    Configuration.SetOption('general','rdp', BoolToStr(FormVmInfo.CheckBoxRDP.Checked, 'True', 'False'));

    Configuration.Free;

    ResetTreeView(VirtualMachinesTreeView);
    VirtualMachinesTreeView.Items.Clear;
    FillVirtualMachineList();

    FormVmInfo.Hide;
  end
  else
  begin
    FormVmInfo.StatusBarVmInfo.Font.Color:=clRed;
    FormVmInfo.StatusBarVmInfo.SimpleText:='You must complete all form fields';
  end;
end;

procedure TFormBhyveManager.SpeedButtonReloadVmConfigClick(Sender: TObject);
begin
  ResetTreeView(DeviceSettingsTreeView);
  LoadDeviceSettingsValues(VirtualMachine.name);
end;

procedure TFormBhyveManager.SpeedButtonRemoveVmClick(Sender: TObject);
var
  Status : Boolean;
  VmName : String;
begin
  Status:=False;

  if (Assigned(VirtualMachinesTreeView.Selected)) and (VirtualMachinesTreeView.Selected.Level = 1) and not (CheckVmRunning(VirtualMachine.name) > 0) then
  begin
    GlobalNode:=VirtualMachinesTreeView.Selected;
    VirtualMachine := TVirtualMachineClass(GlobalNode.Data);

    VmName:=VirtualMachine.name;

    if (MessageDlg('Remove VM', 'Do you want remove '+VmName+' VM data?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      if UseZfs = 'yes' then
      begin
        if ZfsDestroy(VmPath.Remove(0,1)+'/'+VirtualMachine.name) then
          Status:=True
        else
        begin
          if (MessageDlg('Remove VM', VmName+' VM data cannot be removed.'+sLineBreak+sLineBreak+'Do you want force it?', mtWarning, [mbYes, mbNo], 0) = mrYes) then
          begin
            if ZfsDestroy(VmPath.Remove(0,1)+'/'+VirtualMachine.name, True, True) then
              Status:=True;
          end;
        end;
      end
      else
      begin
        if RemoveDirectory(VmName, True) then
          Status:=True;
      end;
    end;

    if Status then
    begin
      if UseDnsmasq = 'yes' then
        RemoveDnsmasqEntry(VmName);

      TObject(VirtualMachinesTreeView.Selected.Data).Free;
      VirtualMachinesTreeView.Selected.Data:=Nil;
      VirtualMachinesTreeView.Selected.Delete;

      VirtualMachinesTreeView.Items.Clear;
      FillVirtualMachineList();

      MessageDlg('Remove VM', VmName+ ' VM data has been removed', mtInformation, [mbOK], 0);

      StatusBarBhyveManager.Font.Color:=clTeal;
      StatusBarBhyveManager.SimpleText:=VmName+ ' VM data has been removed';
    end
    else
    begin
      MessageDlg('Remove VM', VmName+ ' VM data was not removed', mtInformation, [mbOK], 0);

      StatusBarBhyveManager.Font.Color:=clRed;
      StatusBarBhyveManager.SimpleText:=VmName+ ' VM data was not removed';
    end;


  end;
end;

procedure TFormBhyveManager.SpeedButtonStartVmClick(Sender: TObject);
var
  i : Integer;
  Node : TTreeNode;
  IpAddress : String;
  VmConfig : ConfigurationClass;
begin
  GlobalNode:=VirtualMachinesTreeView.Selected;

  VirtualMachine:=TVirtualMachineClass(GlobalNode.Data);

  if DeviceSettingsTreeView.Items.TopLvlItems[1].Count > 0 then
  begin
    CreateDirectory(VmPath+'/'+VirtualMachine.name+'/vtcon', GetCurrentUserName());
  end;

  MyVmThread := VmThread.Create(VirtualMachine.name);
  MyVmThread.OnExitStatus := @VirtualMachineShowStatus;
  MyVmThread.Start;

  Sleep(100);

  if CheckVmRunning(VirtualMachine.name) > 0 then
  begin
    if DeviceSettingsTreeView.Items.TopLvlItems[2].Count = 1 then
      SpeedButtonVncVm.Enabled:=True
    else
      SpeedButtonVncVm.Enabled:=False;

    if Assigned(DeviceSettingsTreeView.Items.FindNodeWithText('Network')) then
    begin
      for i:=0 to DeviceSettingsTreeView.Items.FindNodeWithText('Network').Count-1 do
      begin
        Node:=DeviceSettingsTreeView.Items.FindNodeWithText('Network').Items[i];
        NetworkDevice:=TNetworkDeviceClass(Node.Data);

        CreateNetworkDevice(NetworkDevice.backend, VirtualMachine.name, '');

        if i=0 then
        begin
          AttachDeviceToBridge(BridgeInterface, NetworkDevice.backend, VirtualMachine.name);
          if UseDnsmasq = 'yes' then
          begin
            VmConfig:=ConfigurationClass.Create(VmPath+'/'+VirtualMachine.name+'/'+VirtualMachine.name+'.conf');

            IpAddress:=VmConfig.GetOption('general', 'ipaddress', '');

            if IpAddress = EmptyStr then
            begin
              IpAddress:=GetNewIpAddress(GetSubnet);
              VmConfig.SetOption('general','ipaddress', IpAddress );
            end;

            AddDnsmasqEntry(VirtualMachine.name, IpAddress, NetworkDevice.mac);

            VmConfig.Free;
          end;
        end;

        NetworkDeviceList.Values[NetworkDevice.backend]:=VirtualMachine.name;
        VirtualMachineList.Values[VirtualMachine.name]:='Running';
      end;
    end;

    SpeedButtonRemoveVm.Enabled:=False;
    SpeedButtonStartVm.Enabled:=False;
    SpeedButtonStopVm.Enabled:=True;

    SpeedButtonReloadVmConfig.Enabled:=False;

    VirtualMachinesTreeView.Selected.Text:=VirtualMachine.name+' : Running';

    StatusBarBhyveManager.Font.Color := clTeal;
    StatusBarBhyveManager.SimpleText := VirtualMachine.name+' VM has been started';
  end
  else
  begin
    SpeedButtonRemoveVm.Enabled:=True;
    SpeedButtonVncVm.Enabled:=False;
    SpeedButtonStartVm.Enabled:=True;
    SpeedButtonStopVm.Enabled:=False;

    SpeedButtonReloadVmConfig.Enabled:=True;
  end;
end;

procedure TFormBhyveManager.SpeedButtonStopVmClick(Sender: TObject);
var
  PidNumber : Integer;
begin
  GlobalNode:=VirtualMachinesTreeView.Selected;
  VirtualMachine:=TVirtualMachineClass(GlobalNode.Data);

  PidNumber:=CheckVmRunning(VirtualMachine.name);

  if PidNumber > 0 then
  begin
    KillPid(PidNumber, '-SIGTERM');
  end
end;

procedure TFormBhyveManager.SpeedButtonVncVmClick(Sender: TObject);
var
  DisplayNode : TTreeNode;
begin
  if DeviceSettingsTreeView.Items.TopLvlItems[2].Count = 1 then
  begin
    DisplayNode:=DeviceSettingsTreeView.Items.TopLvlItems[2].Items[0];
    DisplayDevice:=TDisplayDeviceClass(DisplayNode.Data);
    VncConnect(DisplayDevice.tcp, TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).name);
  end;
end;

procedure TFormBhyveManager.VirtualMachinesTreeViewDblClick(Sender: TObject);
begin
  if (Assigned(VirtualMachinesTreeView.Selected)) and (VirtualMachinesTreeView.Selected.Level = 1) then
  begin
    GlobalSettingsTreeView.Items.Clear;
    ResetTreeView(DeviceSettingsTreeView);

    FillGlobalCategoryList();
    FillGlobalCategoryDetailList();
    LoadGlobalSettingsValues(VirtualMachinesTreeView.Selected.Text);
    LoadDeviceSettingsValues(VirtualMachinesTreeView.Selected.Text);

    GlobalNode:=VirtualMachinesTreeView.Selected;
    VirtualMachine := TVirtualMachineClass(GlobalNode.Data);
    EditDescription.Text:=VirtualMachine.description;
    EditSystemType.Text:=VirtualMachine.system_type;
    EditSystemVersion.Text:=VirtualMachine.system_version;
  end;
end;

procedure TFormBhyveManager.VirtualMachinesTreeViewDeletion(Sender: TObject;
  Node: TTreeNode);
begin
  if assigned(TObject(Node.Data)) then
  begin
    TObject(Node.Data).Free;
  end;
end;

procedure TFormBhyveManager.VirtualMachinesTreeViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbRight) and (Assigned(VirtualMachinesTreeView.Selected))then
   begin
     if (VirtualMachinesTreeView.Selected.Level = 0) then
      begin
        VirtualMachinesPopup.PopupMenu.Items.Items[0].Enabled:=True;
        VirtualMachinesPopup.PopupMenu.Items.Items[1].Enabled:=False;
        VirtualMachinesPopup.PopupMenu.Items.Items[2].Enabled:=False;
        VirtualMachinesPopup.PopupMenu.Items.Items[3].Enabled:=False;
        VirtualMachinesPopup.PopupMenu.PopUp;
      end
      else if (VirtualMachinesTreeView.Selected.Level = 1) then
      begin
        VirtualMachinesPopup.PopupMenu.Items.Items[0].Enabled:=False;
        VirtualMachinesPopup.PopupMenu.Items.Items[1].Enabled:=True;
        VirtualMachinesPopup.PopupMenu.Items.Items[2].Enabled:=True;

        if (ExtractVarValue(VirtualMachinesTreeView.Selected.Text) = 'Running') and
           (TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).rdp = True) and
           FileExists(XfreerdpCmd) then
          VirtualMachinesPopup.PopupMenu.Items.Items[3].Enabled:=True
        else
          VirtualMachinesPopup.PopupMenu.Items.Items[3].Enabled:=False;

        VirtualMachinesPopup.PopupMenu.PopUp;
      end;
   end;
end;

procedure TFormBhyveManager.VirtualMachinesTreeViewSelectionChanged(
  Sender: TObject);
begin
  if (Assigned(VirtualMachinesTreeView.Selected)) and (VirtualMachinesTreeView.Selected.Level = 1) then
  begin
    GlobalSettingsTreeView.Items.Clear;
    ResetTreeView(DeviceSettingsTreeView);

    FillGlobalCategoryList();
    FillGlobalCategoryDetailList();

    GlobalNode:=VirtualMachinesTreeView.Selected;
    VirtualMachine := TVirtualMachineClass(GlobalNode.Data);

    LoadGlobalSettingsValues(VirtualMachine.name);
    LoadDeviceSettingsValues(VirtualMachine.name);

    EditDescription.Text:=VirtualMachine.description;
    EditSystemType.Text:=VirtualMachine.system_type;
    EditSystemVersion.Text:=VirtualMachine.system_version;

    StatusBarBhyveManager.Font.Color:=clTeal;
    StatusBarBhyveManager.SimpleText:='virtual machine : '+VirtualMachine.name;

    SpeedButtonStartVm.Enabled:=True;
    SpeedButtonRemoveVm.Enabled:=True;
    SpeedButtonVncVm.Enabled:=False;
    SpeedButtonStopVm.Enabled:=False;
    SpeedButtonReloadVmConfig.Enabled:=True;

    if CheckVmRunning(VirtualMachine.name) > 0 then
    begin
      SpeedButtonStartVm.Enabled:=False;
      SpeedButtonStopVm.Enabled:=True;
      SpeedButtonRemoveVm.Enabled:=False;
      SpeedButtonReloadVmConfig.Enabled:=False;

      if DeviceSettingsTreeView.Items.TopLvlItems[2].Count = 1 then
        SpeedButtonVncVm.Enabled:=True
      else
        SpeedButtonVncVm.Enabled:=False;
    end;
  end
  else
  begin
    GlobalSettingsTreeView.Items.Clear;
    ResetTreeView(DeviceSettingsTreeView);

    FillGlobalCategoryList();
    FillGlobalCategoryDetailList();

    EditDescription.Clear;
    EditSystemType.Clear;
    EditSystemVersion.Clear;

    StatusBarBhyveManager.SimpleText:=EmptyStr;

    SpeedButtonStartVm.Enabled:=False;
    SpeedButtonRemoveVm.Enabled:=False;
    SpeedButtonVncVm.Enabled:=False;
    SpeedButtonStopVm.Enabled:=False;
    SpeedButtonReloadVmConfig.Enabled:=False;
  end;
end;

function TFormBhyveManager.FillDetailAudioDevice(Details: String; pci: String;
  device: String): TAudioDeviceClass;
var
  RegexObj: TRegExpr;
begin
  AudioDevice := TAudioDeviceClass.Create;

  RegexObj := TRegExpr.Create;
  RegexObj.Expression := 'pci.'+pci+'.(\S+)=(\S+)';

  AudioDevice.pci:=pci;
  AudioDevice.device:=device;

  if RegexObj.Exec(Details) then
  begin
    repeat
      case RegexObj.Match[1] of
          'device': AudioDevice.device := RegexObj.Match[2];
          'play': AudioDevice.play := RegexObj.Match[2];
          'rec': AudioDevice.rec := RegexObj.Match[2];
      end;
    until not RegexObj.ExecNext;
  end;

  RegexObj.Free;
  Result:=AudioDevice;
end;

function TFormBhyveManager.FillDetailConsoleDevice(Details: String;
  pci: String; device: String; port : Integer): TSerialVirtioConsoleDeviceClass;
var
  RegexObj: TRegExpr;
begin
  SerialVirtioConsoleDevice := TSerialVirtioConsoleDeviceClass.Create;

  RegexObj := TRegExpr.Create;
  RegexObj.Expression := 'pci.'+pci+'.port.'+IntToStr(port)+'.(\S+)=(\S+)';

  SerialVirtioConsoleDevice.device:=device;
  SerialVirtioConsoleDevice.pci:=pci;
  SerialVirtioConsoleDevice.port := port;

  if RegexObj.Exec(Details) then
  begin
    repeat
      case RegexObj.Match[1] of
          'name': SerialVirtioConsoleDevice.name := RegexObj.Match[2];
          'path': SerialVirtioConsoleDevice.path := RegexObj.Match[2];
      end;
    until not RegexObj.ExecNext;
  end;

  RegexObj.Free;
  Result:=SerialVirtioConsoleDevice;
end;

function TFormBhyveManager.FillDetailDisplayDevice(Details: String;
  pci: String; device: String): TDisplayDeviceClass;
var
  RegexObj: TRegExpr;
begin
  DisplayDevice := TDisplayDeviceClass.Create;

  RegexObj := TRegExpr.Create;
  RegexObj.Expression := 'pci.'+pci+'.(\S+)=(\S+)';

  DisplayDevice.pci:=pci;
  DisplayDevice.device:=device;

  if RegexObj.Exec(Details) then
  begin
    repeat
      case RegexObj.Match[1] of
          'device': DisplayDevice.device := RegexObj.Match[2];
          'tcp':
            begin
              DisplayDevice.tcp := RegexObj.Match[2];
              DisplayDevice.port := StrToInt(ExtractDelimited(2,RegexObj.Match[2],[':']));
            end;
          'w': DisplayDevice.w := StrToInt(RegexObj.Match[2]);
          'h': DisplayDevice.h := StrToInt(RegexObj.Match[2]);
          'vga': DisplayDevice.vga := RegexObj.Match[2];
          'wait': DisplayDevice.wait := RegexObj.Match[2];
          'password': DisplayDevice.pass := RegexObj.Match[2];
      end;
    until not RegexObj.ExecNext;
  end;

  RegexObj.Free;
  Result:=DisplayDevice;
end;

function TFormBhyveManager.FillDetailHostbridgeDevice(Details: String;
  pci: String; device: String): THostbridgeDeviceClass;
var
  RegexObj: TRegExpr;
begin
  HostbridgeDevice := THostbridgeDeviceClass.Create;

  RegexObj := TRegExpr.Create;
  RegexObj.Expression := 'pci.'+pci+'.(\S+)=(\S+)';

  HostBridgeDevice.pci:=pci;
  HostBridgeDevice.device:=device;

  if RegexObj.Exec(Details) then
  begin
    repeat
      case RegexObj.Match[1] of
          'device': HostBridgeDevice.device := RegexObj.Match[2];
      end;
    until not RegexObj.ExecNext;
  end;

  RegexObj.Free;
  Result:=HostBridgeDevice;
end;

function TFormBhyveManager.FillDetailInputDevice(Details: String; pci: String;
  device: String): TVirtioInputDeviceClass;
var
  RegexObj: TRegExpr;
begin
  InputDevice := TVirtioInputDeviceClass.Create;

  RegexObj := TRegExpr.Create;
  RegexObj.Expression := 'pci.'+pci+'.(\S+)=(\S+)';

  InputDevice.device:=device;
  InputDevice.pci:=pci;

  if RegexObj.Exec(Details) then
  begin
    repeat
      case RegexObj.Match[1] of
          'path': InputDevice.path := RegexObj.Match[2];
      end;
    until not RegexObj.ExecNext;
  end;

  RegexObj.Free;
  Result:=InputDevice;
end;

function TFormBhyveManager.FillDetailLpcDevice(Details: String; pci: String;
  device: String): TLPCDeviceClass;
var
  RegexObj: TRegExpr;
begin
  LPCDevice := TLPCDeviceClass.Create;

  RegexObj := TRegExpr.Create;
  RegexObj.Expression := 'lpc.(\S+)=(\S+)';

  LPCDevice.pci:=pci;
  LPCDevice.device := device;

  if RegexObj.Exec(Details) then
  begin
    repeat
      case RegexObj.Match[1] of
          { Remove bootrom and bootvars from LPC when bhyve will updated on FreeBSD 13.x and 14.x }
          'bootrom': if GetOsreldate.ToInt64 < 1500023 then LPCDevice.bootrom := RegexObj.Match[2];
          'bootvars': if GetOsreldate.ToInt64 < 1500023 then LPCDevice.bootvars := RegexObj.Match[2];
          'com1.path': LPCDevice.com1 := RegexObj.Match[2];
          'com2.path': LPCDevice.com2 := RegexObj.Match[2];
          'com3.path': LPCDevice.com3 := RegexObj.Match[2];
          'com4.path': LPCDevice.com4 := RegexObj.Match[2];
          'fwcfg': LPCDevice.fwcfg := RegexObj.Match[2];
          'pc-testdev': LPCDevice.pctestdev := RegexObj.Match[2];
      end;
    until not RegexObj.ExecNext;
  end;

  RegexObj.Free;
  Result:=LPCDevice;
end;

function TFormBhyveManager.FillDetailNetworkDevice(Details: String;
  pci: String; device: String): TNetworkDeviceClass;
var
  RegexObj: TRegExpr;
begin
  NetworkDevice := TNetworkDeviceClass.Create;

  RegexObj := TRegExpr.Create;
  RegexObj.Expression := 'pci.'+pci+'.(\S+)=(\S+)';

  NetworkDevice.pci:=pci;
  NetworkDevice.device:=device;

  if RegexObj.Exec(Details) then
  begin
    repeat
      case RegexObj.Match[1] of
          'device': NetworkDevice.device := RegexObj.Match[2];
          'backend': NetworkDevice.backend := RegexObj.Match[2];
          'mac': NetworkDevice.mac := RegexObj.Match[2];
          'mtu': NetworkDevice.mtu := StrToInt(RegexObj.Match[2]);
          'path': NetworkDevice.path := RegexObj.Match[2];
          'peerhook': NetworkDevice.peerhook := RegexObj.Match[2];
          'socket': NetworkDevice.socket := RegexObj.Match[2];
          'hook': NetworkDevice.hook := RegexObj.Match[2];
          'hostfwd': NetworkDevice.hook := RegexObj.Match[2];
      end;
    until not RegexObj.ExecNext;
  end;

  RegexObj.Free;
  Result:=NetworkDevice;
end;

function TFormBhyveManager.FillDetailPassthruDevice(Details: String;
  pci: String; device: String): TPassthruDeviceClass;
var
  RegexObj: TRegExpr;
begin
  PassthruDevice := TPassthruDeviceClass.Create;

  RegexObj := TRegExpr.Create;
  RegexObj.Expression := 'pci.'+pci+'.(\S+)=(\S+)';

  PassthruDevice.pci:=pci;
  PassthruDevice.device:=device;

  if RegexObj.Exec(Details) then
  begin
    repeat
      case RegexObj.Match[1] of
          'bus': PassthruDevice.bus := RegexObj.Match[2].ToInteger;
          'slot': PassthruDevice.slot := RegexObj.Match[2].ToInteger;
          'func': PassthruDevice.func := RegexObj.Match[2].ToInteger;
          'pptdev': PassthruDevice.pptdev := RegexObj.Match[2];
          'rom': PassthruDevice.rom := RegexObj.Match[2];
      end;
    until not RegexObj.ExecNext;
  end;

  RegexObj.Free;
  Result:=PassthruDevice;
end;

function TFormBhyveManager.FillDetailRngDevice(Details: String; pci: String;
  device: String): TRNGDeviceClass;
var
  RegexObj: TRegExpr;
begin
  RNGDevice := TRNGDeviceClass.Create;

  RegexObj := TRegExpr.Create;
  RegexObj.Expression := 'pci.'+pci+'.(\S+)=(\S+)';

  RNGDevice.pci:=pci;
  RNGDevice.device:=device;

  if RegexObj.Exec(Details) then
  begin
    repeat
      case RegexObj.Match[1] of
          'device': RNGDevice.device := RegexObj.Match[2];
      end;
    until not RegexObj.ExecNext;
  end;

  RegexObj.Free;
  Result:=RNGDevice;
end;

function TFormBhyveManager.FillDetailShareFolderDevice(Details: String;
  pci: String; device: String): TShareFolderDeviceClass;
var
  RegexObj: TRegExpr;
begin
  ShareFolderDevice := TShareFolderDeviceClass.Create;

  RegexObj := TRegExpr.Create;
  RegexObj.Expression := 'pci.'+pci+'.(\S+)=(\S+)';

  ShareFolderDevice.pci:=pci;
  ShareFolderDevice.device:=device;

  if RegexObj.Exec(Details) then
  begin
    repeat
      case RegexObj.Match[1] of
          'device': ShareFolderDevice.device := RegexObj.Match[2];
          'sharename': ShareFolderDevice.sharename := RegexObj.Match[2];
          'path': ShareFolderDevice.path := RegexObj.Match[2];
          'ro': ShareFolderDevice.ro := StrToBool(RegexObj.Match[2]);
      end;
    until not RegexObj.ExecNext;
  end;

  RegexObj.Free;
  Result:=ShareFolderDevice;
end;

function TFormBhyveManager.FillDetailStorageAhciDevice(Details: String;
  pci: String; device: String; port: Integer): TStorageAhciDeviceClass;
var
  RegexObj: TRegExpr;
begin
  StorageAhciDevice := TStorageAhciDeviceClass.Create;

  RegexObj := TRegExpr.Create;
  RegexObj.Expression := 'pci.'+pci+'.port.'+IntToStr(port)+'.(\S+)=(\S+)';

  StorageAhciDevice.pci:=pci;
  StorageAhciDevice.device:=device;
  StorageAhciDevice.port := port;

  if RegexObj.Exec(Details) then
  begin
    repeat
      case RegexObj.Match[1] of
          'device': StorageAhciDevice.device := RegexObj.Match[2];
          'type': StorageAhciDevice.device_type := RegexObj.Match[2];
          'path': StorageAhciDevice.path := RegexObj.Match[2];
          'nocache': StorageAhciDevice.nocache := StrToBool(RegexObj.Match[2]);
          'nodelete': StorageAhciDevice.nodelete := StrToBool(RegexObj.Match[2]);
          'sync': StorageAhciDevice.sync := StrToBool(RegexObj.Match[2]);
          'direct': StorageAhciDevice.direct := StrToBool(RegexObj.Match[2]);
          'ro': StorageAhciDevice.ro := StrToBool(RegexObj.Match[2]);
          'sectorsize': StorageAhciDevice.sectorsize := RegexObj.Match[2];
          'ser': StorageAhciDevice.ser := RegexObj.Match[2];
          'nmrr': StorageAhciDevice.nmrr := StrToInt(RegexObj.Match[2]);
          'rev': StorageAhciDevice.rev := RegexObj.Match[2];
          'model': StorageAhciDevice.model := RegexObj.Match[2];
      end;
    until not RegexObj.ExecNext;
  end;

  RegexObj.Free;
  Result:=StorageAhciDevice;
end;

function TFormBhyveManager.FillDetailStorageNvmeDevice(Details: String;
  pci: String; device: String): TStorageNvmeDeviceClass;
var
  RegexObj: TRegExpr;
begin
  StorageNvmeDevice := TStorageNvmeDeviceClass.Create;

  RegexObj := TRegExpr.Create;
  RegexObj.Expression := 'pci.'+pci+'.(\S+)=(\S+)';

  StorageNvmeDevice.pci:=pci;
  StorageNvmeDevice.device:=device;

  if RegexObj.Exec(Details) then
  begin
    repeat
      case RegexObj.Match[1] of
          'device': StorageNvmeDevice.device := RegexObj.Match[2];
          'path': StorageNvmeDevice.devpath := RegexObj.Match[2];
          'nocache': StorageNvmeDevice.nocache := StrToBool(RegexObj.Match[2]);
          'nodelete': StorageNvmeDevice.nodelete := StrToBool(RegexObj.Match[2]);
          'sync': StorageNvmeDevice.sync := StrToBool(RegexObj.Match[2]);
          'direct': StorageNvmeDevice.direct := StrToBool(RegexObj.Match[2]);
          'ro': StorageNvmeDevice.ro := StrToBool(RegexObj.Match[2]);
          'maxq': StorageNvmeDevice.maxq := StrToInt(RegexObj.Match[2]);
          'qsz': StorageNvmeDevice.qsz := StrToInt(RegexObj.Match[2]);
          'ioslots': StorageNvmeDevice.ioslots := StrToInt(RegexObj.Match[2]);
          'sectsz': StorageNvmeDevice.sectsz := StrToInt(RegexObj.Match[2]);
          'ser': StorageNvmeDevice.ser := RegexObj.Match[2];
          'eui64': StorageNvmeDevice.eui64 := StrToInt(RegexObj.Match[2]);
          'dsm': StorageNvmeDevice.dsm := RegexObj.Match[2];
          'ram': StorageNvmeDevice.ram := StrToInt(RegexObj.Match[2]);
      end;
    until not RegexObj.ExecNext;
  end;

  RegexObj.Free;
  Result:=StorageNvmeDevice;
end;

function TFormBhyveManager.FillDetailStorageVirtioBlkDevice(Details: String;
  pci: String; device: String): TStorageVirtioBlkDeviceClass;
var
  RegexObj: TRegExpr;
begin
  StorageVirtioBlkDevice := TStorageVirtioBlkDeviceClass.Create;

  RegexObj := TRegExpr.Create;
  RegexObj.Expression := 'pci.'+pci+'.(\S+)=(\S+)';

  StorageVirtioBlkDevice.pci:=pci;
  StorageVirtioBlkDevice.device:=device;

  if RegexObj.Exec(Details) then
  begin
    repeat
      case RegexObj.Match[1] of
          'device': StorageVirtioBlkDevice.device := RegexObj.Match[2];
          'path': StorageVirtioBlkDevice.path := RegexObj.Match[2];
          'nocache': StorageVirtioBlkDevice.nocache := StrToBool(RegexObj.Match[2]);
          'nodelete': StorageVirtioBlkDevice.nodelete := StrToBool(RegexObj.Match[2]);
          'sync': StorageVirtioBlkDevice.sync := StrToBool(RegexObj.Match[2]);
          'direct': StorageVirtioBlkDevice.direct := StrToBool(RegexObj.Match[2]);
          'ro': StorageVirtioBlkDevice.ro := StrToBool(RegexObj.Match[2]);
          'sectorsize': StorageVirtioBlkDevice.sectorsize := RegexObj.Match[2];
          'ser': StorageVirtioBlkDevice.ser := RegexObj.Match[2];
      end;
    until not RegexObj.ExecNext;
  end;

  RegexObj.Free;
  Result:=StorageVirtioBlkDevice;
end;

function TFormBhyveManager.FillDetailUsbXhciDevice(Details: String;
  pci: String; device: String; slot: Integer): TUsbXhciDeviceClass;
var
  RegexObj: TRegExpr;
begin
  UsbXhciDevice := TUsbXhciDeviceClass.Create;

  RegexObj := TRegExpr.Create;
  RegexObj.Expression := 'pci.'+pci+'.slot.'+IntToStr(slot)+'.(\S+)=(\S+)';

  UsbXhciDevice.pci:=pci;
  UsbXhciDevice.device:=device;
  UsbXhciDevice.slot := slot;

  if RegexObj.Exec(Details) then
  begin
    repeat
      case RegexObj.Match[1] of
          'device': UsbXhciDevice.slot_device := RegexObj.Match[2];
      end;
    until not RegexObj.ExecNext;
  end;

  RegexObj.Free;
  Result:=UsbXhciDevice;
end;

{ Load default global settings from Virtual Machine bhyve_config.conf file }
function TFormBhyveManager.LoadGlobalSettingsValues(VmName: String): Boolean;
var
  i: Integer;
  RegexObj: TRegExpr;
  TmpGlobalSettingsList : TStringList;
  ConfigSettingsFile: TStringList;
begin
  TmpGlobalSettingsList:=TStringList.Create;
  TmpGlobalSettingsList.Text := GlobalSettingDefaultValueList.Text;

  RegexObj := TRegExpr.Create;
  RegexObj.Expression := '(\S+)=(\S+)';

  ConfigSettingsFile:=TStringList.Create;
  ConfigSettingsFile.LoadFromFile(VmPath + '/' + VmName + '/bhyve_config.conf');

  for i:=ConfigSettingsFile.Count-1 downto 0 do
  begin
      if (ConfigSettingsFile[i].Contains('pci.')) or (ConfigSettingsFile[i].Contains('lpc.'))  then
      begin
        ConfigSettingsFile.Delete(i);
      end;
  end;

  if RegexObj.Exec(ConfigSettingsFile.Text) then
  begin
    repeat
      TmpGlobalSettingsList.Values[RegexObj.Match[1]]:= RegexObj.Match[2];
    until not RegexObj.ExecNext;
  end;

  RegexObj.Free;
  ConfigSettingsFile.Free;

  for i:=0 to GlobalSettingCategoryList.Count-1 do
  begin
    GlobalSettingsTreeView.Items.AddChild(GlobalSettingsTreeView.Items.FindNodeWithText(GlobalSettingCategoryList.ValueFromIndex[i]), GlobalSettingCategoryList.Names[i]+' : '+TmpGlobalSettingsList.Values[GlobalSettingCategoryList.Names[i]]);
  end;

  TmpGlobalSettingsList.Free;
  Result:=True;
end;

{ Load default device settings from Virtual Machine bhyve_config.conf file }
function TFormBhyveManager.LoadDeviceSettingsValues(VmName: String): Boolean;
var
  sl: TStringList;
  slf : TStringList;
  RegexObj: TRegExpr;
  RegexObj1: TRegExpr;
  ImageIndex : Integer;
  i : Integer;
begin
  sl:=TStringList.Create;
  slf:=TStringList.Create;

  try
    sl.LoadFromFile(VmPath + '/' + VmName + '/bhyve_config.conf');

    RegexObj := TRegExpr.Create;

    for i:=0 to sl.Count-1 do
    begin
      RegexObj.Expression := '^lpc.*|^pci.*';
      if RegexObj.Exec(sl[i]) then
      begin
        { Remove when bhyve will updated on FreeBSD 13.x and 14.x }
        if (GetOsreldate.ToInt64 >= 1500023) then
        begin
          if not (sl[i].Contains('lpc.bootrom')) then
            slf.Add(sl[i]);
        end
        else
        begin
          slf.Add(sl[i]);
        end;
      end;
    end;

    TmpDevicesStringList.Text:=slf.Text;

    RegexObj.Expression := 'pci\.(\d+\.\d+\.\d+).device=(\S+)';
    if RegexObj.Exec(slf.Text) then
    begin
      repeat
        case DevicesList.Values[RegexObj.Match[2]] of
            'Audio':
              begin
                AudioDevice:=FillDetailAudioDevice(slf.Text, RegexObj.Match[1], RegexObj.Match[2]);

                GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('Audio'), 'device : '+AudioDevice.device);
                GlobalNode.Data:=AudioDevice;
                GlobalNode.ImageIndex:=1;
                GlobalNode.SelectedIndex:=1;

                DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+AudioDevice.pci);
              end;
            'Console':
              begin
                RegexObj1 := TRegExpr.Create;
                RegexObj1.Expression := 'pci.'+RegexObj.Match[1]+'.port.(\d+).name=(\S+)';

                if RegexObj1.Exec(slf.Text) then
                begin
                  repeat
                   SerialVirtioConsoleDevice:=FillDetailConsoleDevice(slf.Text, RegexObj.Match[1], RegexObj.Match[2], StrToInt(RegexObj1.Match[1]));
                   GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('Console'), 'device : '+SerialVirtioConsoleDevice.device);
                   GlobalNode.Data:=SerialVirtioConsoleDevice;
                   GlobalNode.ImageIndex:=3;
                   GlobalNode.SelectedIndex:=3;

                   DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+SerialVirtioConsoleDevice.pci);
                   DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'port : '+IntToStr(SerialVirtioConsoleDevice.port));
                  until not RegexObj1.ExecNext;
                end;
                RegexObj1.Free;
              end;
            'Hostbridge':
              begin
                HostBridgeDevice:=FillDetailHostbridgeDevice(slf.Text, RegexObj.Match[1], RegexObj.Match[2]);

                GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('Hostbridge'), 'device : '+HostBridgeDevice.device);
                GlobalNode.Data:=HostBridgeDevice;
                GlobalNode.ImageIndex:=6;
                GlobalNode.SelectedIndex:=6;

                DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+HostBridgeDevice.pci);
              end;
            'Input':
              begin
                InputDevice:=FillDetailInputDevice(slf.Text, RegexObj.Match[1], RegexObj.Match[2]);

                GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('Input'), 'device : '+InputDevice.device);
                GlobalNode.Data:=InputDevice;
                GlobalNode.ImageIndex:=7;
                GlobalNode.SelectedIndex:=7;

                DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+InputDevice.pci);
              end;
            'Display':
              begin
                DisplayDevice:=FillDetailDisplayDevice(slf.Text, RegexObj.Match[1], RegexObj.Match[2]);

                GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('Display'), 'device : '+DisplayDevice.device);
                GlobalNode.Data:=DisplayDevice;
                GlobalNode.ImageIndex:=4;
                GlobalNode.SelectedIndex:=4;

                DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+DisplayDevice.pci);
              end;
            'LPC':
              begin
                LPCDevice:=FillDetailLpcDevice(slf.Text, RegexObj.Match[1], RegexObj.Match[2]);

                GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('LPC'), 'device : '+LPCDevice.device);
                GlobalNode.Data:=LPCDevice;
                GlobalNode.ImageIndex:=8;
                GlobalNode.SelectedIndex:=8;

                DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+LPCDevice.pci);
              end;
            'Network':
              begin
                NetworkDevice:=FillDetailNetworkDevice(slf.Text, RegexObj.Match[1], RegexObj.Match[2]);

                GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('Network'), 'device : '+NetworkDevice.device);
                GlobalNode.Data:=NetworkDevice;
                GlobalNode.ImageIndex:=9;
                GlobalNode.SelectedIndex:=9;

                DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+NetworkDevice.pci);
              end;
            'Passthru':
              begin
                PassthruDevice:=FillDetailPassthruDevice(slf.Text, RegexObj.Match[1], RegexObj.Match[2]);

                GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('Passthru'), 'device : '+PassthruDevice.device+'-'+PassthruDevice.pptdev);
                GlobalNode.Data:=PassthruDevice;
                GlobalNode.ImageIndex:=11;
                GlobalNode.SelectedIndex:=11;

                DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+PassthruDevice.pci);
              end;
            'RNG':
              begin
                RNGDevice:=FillDetailRngDevice(slf.Text, RegexObj.Match[1], RegexObj.Match[2]);

                GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('RNG'), 'device : '+RNGDevice.device);
                GlobalNode.Data:=RNGDevice;
                GlobalNode.ImageIndex:=12;
                GlobalNode.SelectedIndex:=12;

                DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+RNGDevice.pci);
              end;
            'Shared folders':
              begin
                ShareFolderDevice:=FillDetailShareFolderDevice(slf.Text, RegexObj.Match[1], RegexObj.Match[2]);

                GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('Shared folders'), 'device : '+ShareFolderDevice.device);
                GlobalNode.Data:=ShareFolderDevice;
                GlobalNode.ImageIndex:=13;
                GlobalNode.SelectedIndex:=13;

                DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+ShareFolderDevice.pci);
              end;
            'Storage':
              begin
                case RegexObj.Match[2] of
                    'ahci':
                      begin
                        RegexObj1 := TRegExpr.Create;
                        RegexObj1.Expression := 'pci.'+RegexObj.Match[1]+'.port.(\d+).type=(\S+)';

                        if RegexObj1.Exec(slf.Text) then
                        begin
                          repeat
                           StorageAhciDevice:=FillDetailStorageAhciDevice(slf.Text, RegexObj.Match[1], RegexObj.Match[2], StrToInt(RegexObj1.Match[1]));

                           if StorageAhciDevice.device_type = 'hd' then
                           begin
                             StorageAhciDevice.storage_size:=GetStorageSize(StorageAhciDevice.path);
                             StorageAhciDevice.storage_type:=GetStorageType(StorageAhciDevice.path);
                             ImageIndex:=5;
                           end
                           else
                           begin
                             StorageAhciDevice.storage_size:='0G';
                             StorageAhciDevice.storage_type:='image file';
                             ImageIndex:=2;
                           end;

                           GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('Storage'), 'device : '+StorageAhciDevice.device+'-'+StorageAhciDevice.device_type);
                           GlobalNode.Data:=StorageAhciDevice;
                           GlobalNode.ImageIndex:=ImageIndex;
                           GlobalNode.SelectedIndex:=ImageIndex;

                           DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+StorageAhciDevice.pci);
                           DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'port : '+IntToStr(StorageAhciDevice.port));
                          until not RegexObj1.ExecNext;
                        end;
                        RegexObj1.Free;
                      end;
                    'nvme':
                      begin
                        StorageNvmeDevice:=FillDetailStorageNvmeDevice(slf.Text, RegexObj.Match[1], RegexObj.Match[2]);
                        StorageNvmeDevice.storage_size:=GetStorageSize(StorageNvmeDevice.devpath);
                        StorageNvmeDevice.storage_type:=GetStorageType(StorageNvmeDevice.devpath);

                        GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('Storage'), 'device : '+StorageNvmeDevice.device);
                        GlobalNode.Data:=StorageNvmeDevice;
                        GlobalNode.ImageIndex:=10;
                        GlobalNode.SelectedIndex:=10;

                        DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+StorageNvmeDevice.pci);
                      end;
                    'virtio-blk':
                      begin
                        StorageVirtioBlkDevice:=FillDetailStorageVirtioBlkDevice(slf.Text, RegexObj.Match[1], RegexObj.Match[2]);
                        StorageVirtioBlkDevice.storage_size:=GetStorageSize(StorageVirtioBlkDevice.path);
                        StorageVirtioBlkDevice.storage_type:=GetStorageType(StorageVirtioBlkDevice.path);;

                        GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('Storage'), 'device : '+StorageVirtioBlkDevice.device);
                        GlobalNode.Data:=StorageVirtioBlkDevice;
                        GlobalNode.ImageIndex:=5;
                        GlobalNode.SelectedIndex:=5;

                        DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+StorageVirtioBlkDevice.pci);
                      end;
                end;
              end;
            'USB':
              begin
                RegexObj1 := TRegExpr.Create;
                RegexObj1.Expression := 'pci.'+RegexObj.Match[1]+'.slot.(\d+).device=(\S+)';

                if RegexObj1.Exec(slf.Text) then
                begin
                  repeat
                   UsbXhciDevice:=FillDetailUsbXhciDevice(slf.Text, RegexObj.Match[1], RegexObj.Match[2], StrToInt(RegexObj1.Match[1]));
                   GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('USB'), 'device : '+UsbXhciDevice.device+'-'+UsbXhciDevice.slot_device);
                   GlobalNode.Data:=UsbXhciDevice;
                   GlobalNode.ImageIndex:=14;
                   GlobalNode.SelectedIndex:=14;

                   DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+UsbXhciDevice.pci);
                   DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'slot : '+IntToStr(UsbXhciDevice.slot));
                  until not RegexObj1.ExecNext;
                end;
                RegexObj1.Free;
              end;
        end;
      until not RegexObj.ExecNext;
    end;
  finally
    Result:=True;
    RegexObj.Free;
    sl.Free;
    slf.Free;
  end;
end;

function TFormBhyveManager.LoadNetworkDevice():TStringArray;
var
  i : Integer;
  DevicesArray : TStringArray;
  node : TTreeNode;
begin
  DevicesArray:=[];

  if Assigned(DeviceSettingsTreeView.Items.FindNodeWithText('Network')) then
  begin
    for i:=0 to DeviceSettingsTreeView.Items.FindNodeWithText('Network').Count-1 do
    begin
      node:=DeviceSettingsTreeView.Items.FindNodeWithText('Network').Items[i];
      NetworkDevice:=TNetworkDeviceClass(node.Data);
      Insert(NetworkDevice.backend, DevicesArray, i);
    end;
  end;

  Result:=DevicesArray;
end;

function TFormBhyveManager.LoadVirtualMachineData(ConfigPath: String
  ): TVirtualMachineClass;
var
  Configuration : ConfigurationClass;
begin
  Configuration:= ConfigurationClass.Create(ConfigPath);

  VirtualMachine := TVirtualMachineClass.Create;

  VirtualMachine.name:=Configuration.GetOption('general','name');
  VirtualMachine.description:=Configuration.GetOption('general','description');
  VirtualMachine.uuid:=Configuration.GetOption('general','uuid');
  VirtualMachine.config_path:=Configuration.GetOption('general','config_path');
  VirtualMachine.system_type:=Configuration.GetOption('general','type');
  VirtualMachine.system_version:=Configuration.GetOption('general','version');
  VirtualMachine.image:=StrToInt(Configuration.GetOption('general','image'));

  if Configuration.GetOption('general','rdp') = 'True' then
    VirtualMachine.rdp:=True
  else
    VirtualMachine.rdp:=False;

  if UseDnsmasq = 'yes' then
    VirtualMachine.ipaddress:=Configuration.GetOption('general','ipaddress');

  Configuration.Free;

  Result:=VirtualMachine;
end;

end.

