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

unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, FileUtil, Graphics, Dialogs, StdCtrls, Menus, ExtCtrls,
  ComCtrls, Buttons, RegExpr, StrUtils, unit_component, unit_device, unit_thread, LCLTranslator,
  Translations;

type

  { TFormBhyveManager }

  TFormBhyveManager = class(TForm)
    ComboBoxLanguage: TComboBox;
    ImageLanguage: TImage;
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
    procedure ComboBoxLanguageChange(Sender: TObject);
    procedure ShowHideClick(Sender: TObject);
    procedure DeviceSettingsTreeViewDeletion(Sender: TObject; Node: TTreeNode);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
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
    procedure CopyVmNameClick(Sender: TObject);
    procedure CopyComCommandClick(Sender: TObject);
    procedure CopyIpv4AddressClick(Sender: TObject);
    procedure CopyIpv6AddressClick(Sender: TObject);
    procedure CreateVmClick(Sender: TObject);
    procedure EditDevice(Sender: TObject);
    procedure EditVirtualMachineInfo(Sender: TObject);
    procedure PacketFilterRulesVm(Sender: TObject);
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
    TrayIconPopup : TTrayIconPopupMenu;
    TrayIcon : TSystemTrayIcon;
    DeviceImageList : TDeviceImageList;
    SystemImageList : TSystemImageList;
    ActionImageList : TActionImageList;
    MyVmThread: VmThread;
    ProcessPid : Integer;
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
    procedure FillVirtualMachine(VmName : String);
    function LoadDeviceSettingsValues(VmName: String):Boolean;
    function LoadGlobalSettingsValues(VmName: String):Boolean;
    function LoadVirtualMachineData(ConfigPath: String):TVirtualMachineClass;
    function LoadNetworkDevice():TStringArray;
    procedure ResetTreeView(TreeView : TTreeView);
    function SaveVirtualMachineConfig():Boolean;
    procedure VirtualMachineShowStatus(Status: Integer; Message : String; VmName : String; ErrorMessage : String);
    procedure AppShowStatus(Status: Integer; AppPid: Integer);
    procedure AppEndStatus(Status: Integer; AppName : String; AppPid: Integer);
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
  DiskFile : String;
  TotalSize : Int64;

implementation

{$R *.lfm}

uses
  form_about, form_audio_device, form_change_value, form_console_device, form_display_device,
  form_hostbridge_device, form_input_device, form_lpc_device, form_network_device, form_passthru_device,
  form_rdp_connection, form_share_folder_device, form_storage_device, form_settings, form_vm_create,
  form_vm_info, form_packet_filter_rules, unit_configuration, unit_global, unit_util, unit_language,
  Clipbrd, LazLogger;

{ TFormBhyveManager }

procedure TFormBhyveManager.FormCreate(Sender: TObject);
begin
  { Temporary workaround when LCLGTK2 is used with latest version of Lazarus }
  {$ifdef LCLGTK2}
  FormBhyveManager.BorderStyle:=bsSizeable;
  {$endif}

  DebugLogger.UseStdOut:= False;
  DebugLogger.CloseLogFileBetweenWrites:= true;
  DebugLogger.LogName:= GetUserDir + '.config/bhyvemgr/bhyvemgr.log';

  DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : '+debugln_bhyve_started);

  FormSettings:= TFormSettings.Create(FormBhyveManager);
  FormAbout:= TFormAbout.Create(FormBhyveManager);
  FormChangeValue:= TFormChangeValue.Create(FormBhyveManager);
  FormVMCreate:=TFormVmCreate.Create(FormBhyveManager);
  FormVmInfo:=TFormVmInfo.Create(FormBhyveManager);
  FormRdpConnection:=TFormRdpConnection.Create(FormBhyveManager);
  FormPacketFilterRules:=TFormPacketFilterRules.Create(FormBhyveManager);

  SettingsPageControl.TabIndex:=0;

  SystemImageList:=TSystemImageList.Create(FormBhyveManager);
  DeviceImageList:=TDeviceImageList.Create(FormBhyveManager);
  ActionImageList:=TActionImageList.Create(FormBhyveManager);

  // System TrayIconPopup and TrayIcon
  TrayIconPopup:=TTrayIconPopupMenu.Create(FormBhyveManager);
  TrayIconPopup.PopupMenu.Images:=ActionImageList.ActionList;

  TrayIconPopup.PopupMenu.Items[0].ImageIndex:=4;
  TrayIconPopup.PopupMenu.Items[1].ImageIndex:=5;

  TrayIconPopup.PopupMenu.Items[0].OnClick:=@ShowHideClick;
  TrayIconPopup.PopupMenu.Items[1].OnClick:=@MenuItemExitClick;

  TrayIcon:=TSystemTrayIcon.Create(FormBhyveManager);
  TrayIcon.TrayIcon.OnClick:=@ShowHideClick;
  TrayIcon.TrayIcon.PopUpMenu:=TrayIconPopup.PopupMenu;

  if UseSystray = 'yes' then
    TrayIcon.TrayIcon.Show;

  // Virtual Machine Popup Menu
  VirtualMachinesPopup:= TVirtualMachinesPopupMenu.Create(FormBhyveManager);
  VirtualMachinesPopup.PopupMenu.Images:=ActionImageList.ActionList;

  // Virtual Machine Popup Menu - Add, Modify and Remove
  VirtualMachinesPopup.PopupMenu.Items[0].ImageIndex:=0;
  VirtualMachinesPopup.PopupMenu.Items[1].ImageIndex:=1;
  VirtualMachinesPopup.PopupMenu.Items[2].ImageIndex:=2;
  VirtualMachinesPopup.PopupMenu.Items[3].ImageIndex:=3;
  VirtualMachinesPopup.PopupMenu.Items[4].ImageIndex:=10;
  VirtualMachinesPopup.PopupMenu.Items[6].ImageIndex:=8;
  VirtualMachinesPopup.PopupMenu.Items[7].ImageIndex:=7;
  VirtualMachinesPopup.PopupMenu.Items[8].ImageIndex:=9;
  VirtualMachinesPopup.PopupMenu.Items[9].ImageIndex:=9;

  VirtualMachinesPopup.PopupMenu.Items[0].OnClick:=@SpeedButtonAddVmClick;
  VirtualMachinesPopup.PopupMenu.Items[1].OnClick:=@EditVirtualMachineInfo;
  VirtualMachinesPopup.PopupMenu.Items[2].OnClick:=@SpeedButtonRemoveVmClick;
  VirtualMachinesPopup.PopupMenu.Items[3].OnClick:=@RemoteDesktopProtocolVm;
  VirtualMachinesPopup.PopupMenu.Items[4].OnClick:=@PacketFilterRulesVm;
  VirtualMachinesPopup.PopupMenu.Items[6].OnClick:=@CopyVmNameClick;
  VirtualMachinesPopup.PopupMenu.Items[7].OnClick:=@CopyComCommandClick;
  VirtualMachinesPopup.PopupMenu.Items[8].OnClick:=@CopyIpv4AddressClick;
  VirtualMachinesPopup.PopupMenu.Items[9].OnClick:=@CopyIpv6AddressClick;

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
  ProcessPid:=-1;

  FillComboLanguage(ComboBoxLanguage);
  ComboBoxLanguage.ItemIndex:=ComboBoxLanguage.Items.IndexOf(Language);

  FillVirtualMachineList();
  FillDeviceCategoryList();
  FillGlobalCategoryList();
  FillDeviceDetailList();
end;

{
  This procedure is used to show a device menu when a group device or a device
  is selected and mouse right button is pressed over it at Device settings page.
  The menu includes three options: Add device, Edit device, and Delete device
}
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

{
  This procedure is used to show a global menu when an item is selected and
  mouse right button is pressed over it at Global settings page. The menu
  includes an option: Edit global setting
}
procedure TFormBhyveManager.GlobalSettingsTreeViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbRight) and (Assigned(GlobalSettingsTreeView.Selected)) and (GlobalSettingsTreeView.Selected.Level <> 0) then
   begin
      GlobalSettingsPopup.PopupMenu.PopUp;
   end;
end;

{
  This procedure is used to show an "option : value" string on status bar when a
  item is clicked at Global settings page.
}
procedure TFormBhyveManager.GlobalSettingsTreeViewClick(Sender: TObject);
begin
  if (Assigned(GlobalSettingsTreeView.Selected)) and (GlobalSettingsTreeView.Selected.Level <> 0) then
    StatusBarBhyveManager.SimpleText:=GlobalSettingsTreeView.Selected.Text
  else
    StatusBarBhyveManager.SimpleText:=EmptyStr;
end;

{
  This procedure is used to show an "option : value" string on status bar when a
  item is selected at Global settings page.
}
procedure TFormBhyveManager.GlobalSettingsTreeViewSelectionChanged(Sender: TObject);
begin
  if Assigned(GlobalSettingsTreeView.Selected) and (GlobalSettingsTreeView.Selected.Level = 1) then
    StatusBarBhyveManager.SimpleText:=GlobalSettingsTreeView.Selected.Text
  else
    StatusBarBhyveManager.SimpleText:=EmptyStr;
end;

{
  This procedure is used to fill Global settings page with main category names.
}
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
  GlobalCategoryList.Add('BIOS');
  {$ifdef CPUAMD64}
  GlobalCategoryList.Add('x86');
  {$endif}

  GlobalSettingsTreeView.Items.Clear;

  for i:=0 to GlobalCategoryList.Count-1 do
  begin
    GlobalNode:=GlobalSettingsTreeView.Items.Add(Nil, GlobalCategoryList.ValueFromIndex[i]);
    GlobalNode.ImageIndex:=0;
    GlobalNode.SelectedIndex:=0;
  end;

  GlobalCategoryList.Free;
end;

{
  This procedure is used to load "Global setting" default keys, values, and types
  for each main categories.
}
procedure TFormBhyveManager.FillGlobalCategoryDetailList();
begin
  // Remove when bhyve will updated on FreeBSD 13.x and 14.x
  { System }
  if GetOsreldate.ToInt64 >= 1500023 then
  begin
    GlobalSettingDefaultValueList.Values['bootrom'] := EmptyStr;
    {$ifdef CPUAMD64}
    GlobalSettingDefaultValueList.Values['bootvars'] := EmptyStr;
    {$endif}
  end;
  {$ifdef CPUAARCH64}
  GlobalSettingDefaultValueList.Values['console'] := 'stdio';
  {$endif}
  GlobalSettingDefaultValueList.Values['destroy_on_poweroff'] := 'false';
  GlobalSettingDefaultValueList.Values['rtc.use_localtime'] := 'true';
  GlobalSettingDefaultValueList.Values['uuid'] := EmptyStr;
  GlobalSettingDefaultValueList.Values['virtio_msix'] := 'true';
  GlobalSettingDefaultValueList.Values['keyboard.layout'] := EmptyStr;
  { Processor }
  GlobalSettingDefaultValueList.Values['cpus'] := '1';
  GlobalSettingDefaultValueList.Values['cores'] := '1';
  GlobalSettingDefaultValueList.Values['threads'] := '1';
  GlobalSettingDefaultValueList.Values['sockets'] := '1';
  { Memory  }
  GlobalSettingDefaultValueList.Values['memory.guest_in_core'] := 'false';
  GlobalSettingDefaultValueList.Values['memory.size'] := '256M';
  GlobalSettingDefaultValueList.Values['memory.wired'] := 'false';
  { ACPI }
  if GetOsreldate.ToInt64 < 1500000 then
    GlobalSettingDefaultValueList.Values['acpi_tables'] := 'false'
  else
    GlobalSettingDefaultValueList.Values['acpi_tables'] := 'true';

  GlobalSettingDefaultValueList.Values['acpi_tables_in_memory'] := 'true';
  { Debugging }
  GlobalSettingDefaultValueList.Values['gdb.address'] := 'localhost';
  GlobalSettingDefaultValueList.Values['gdb.port'] := '0';
  GlobalSettingDefaultValueList.Values['gdb.wait'] := 'false';
  { TPM }
  GlobalSettingDefaultValueList.Values['tpm.path'] := EmptyStr;
  GlobalSettingDefaultValueList.Values['tpm.type'] := EmptyStr;
  GlobalSettingDefaultValueList.Values['tpm.version'] := EmptyStr;
  { x86 }
  {$ifdef CPUAMD64}
  GlobalSettingDefaultValueList.Values['x86.mptable'] := 'true';
  GlobalSettingDefaultValueList.Values['x86.x2apic'] := 'false';
  GlobalSettingDefaultValueList.Values['x86.strictio'] := 'false';
  GlobalSettingDefaultValueList.Values['x86.strictmsr'] := 'true';
  if GetOsreldate.ToInt64 >= 1500023 then
  begin
    GlobalSettingDefaultValueList.Values['x86.verbosemsr'] := 'false';
  end;
  GlobalSettingDefaultValueList.Values['x86.vmexit_on_hlt'] := 'false';
  GlobalSettingDefaultValueList.Values['x86.vmexit_on_pause'] := 'false';
  {$endif}
  { BIOS }
  GlobalSettingDefaultValueList.Values['bios.vendor'] := 'BHYVE';
  GlobalSettingDefaultValueList.Values['bios.version'] := '14.0';
  GlobalSettingDefaultValueList.Values['bios.release_date'] := '10/17/2021';
  GlobalSettingDefaultValueList.Values['system.family_name'] := 'Virtual Machine';
  GlobalSettingDefaultValueList.Values['system.manufacturer'] := 'FreeBSD';
  GlobalSettingDefaultValueList.Values['system.product_name'] := 'BHYVE';
  GlobalSettingDefaultValueList.Values['system.serial_number'] := 'None';
  GlobalSettingDefaultValueList.Values['system.sku'] := 'None';
  GlobalSettingDefaultValueList.Values['system.version'] := '1.0';
  GlobalSettingDefaultValueList.Values['board.manufacturer'] := 'FreeBSD';
  GlobalSettingDefaultValueList.Values['board.product_name'] := 'BHYVE';
  GlobalSettingDefaultValueList.Values['board.version'] := '1.0';
  GlobalSettingDefaultValueList.Values['board.serial_number'] := 'None';
  GlobalSettingDefaultValueList.Values['board.asset_tag'] := 'None';
  GlobalSettingDefaultValueList.Values['board.location'] := 'None';
  GlobalSettingDefaultValueList.Values['chassis.manufacturer'] := 'FreeBSD';
  GlobalSettingDefaultValueList.Values['chassis.version'] := '1.0';
  GlobalSettingDefaultValueList.Values['chassis.serial_number'] := 'None';
  GlobalSettingDefaultValueList.Values['chassis.asset_tag'] := 'None';
  GlobalSettingDefaultValueList.Values['chassis.sku'] := 'None';

  // Remove when bhyve will updated on FreeBSD 13.x and 14.x
  { System }
  if GetOsreldate.ToInt64 >= 1500023 then
  begin
    GlobalSettingTypeList.Values['bootrom'] := 'String';
    {$ifdef CPUAMD64}
    GlobalSettingTypeList.Values['bootvars'] := 'String';
    {$endif}
  end;

  {$ifdef CPUAARCH64}
  GlobalSettingTypeList.Values['console'] := 'String';
  {$endif}
  GlobalSettingTypeList.Values['keyboard.layout'] := 'String';
  GlobalSettingTypeList.Values['rtc.use_localtime'] := 'Boolean';
  GlobalSettingTypeList.Values['uuid'] := 'String';
  GlobalSettingTypeList.Values['virtio_msix'] := 'Boolean';
  GlobalSettingTypeList.Values['destroy_on_poweroff'] := 'Boolean';
  { Processor }
  GlobalSettingTypeList.Values['cpus'] := 'Integer';
  GlobalSettingTypeList.Values['cores'] := 'Integer';
  GlobalSettingTypeList.Values['threads'] := 'Integer';
  GlobalSettingTypeList.Values['sockets'] := 'Integer';
  { Memory }
  GlobalSettingTypeList.Values['memory.guest_in_core'] := 'Boolean';
  GlobalSettingTypeList.Values['memory.size'] := 'String';
  GlobalSettingTypeList.Values['memory.wired'] := 'Boolean';
  { ACPI }
  GlobalSettingTypeList.Values['acpi_tables'] := 'Boolean';
  GlobalSettingTypeList.Values['acpi_tables_in_memory'] := 'Boolean';
  { Debugging }
  GlobalSettingTypeList.Values['gdb.address'] := 'String';
  GlobalSettingTypeList.Values['gdb.port'] := 'Integer';
  GlobalSettingTypeList.Values['gdb.wait'] := 'Boolean';
  { TPM }
  GlobalSettingTypeList.Values['tpm.path'] := 'String';
  GlobalSettingTypeList.Values['tpm.type'] := 'String';
  GlobalSettingTypeList.Values['tpm.version'] := 'String';
  { Bios }
  GlobalSettingTypeList.Values['bios.vendor'] := 'String';
  GlobalSettingTypeList.Values['bios.version'] := 'String';
  GlobalSettingTypeList.Values['bios.release_date'] := 'String';
  GlobalSettingTypeList.Values['system.family_name'] := 'String';
  GlobalSettingTypeList.Values['system.manufacturer'] := 'String';
  GlobalSettingTypeList.Values['system.product_name'] := 'String';
  GlobalSettingTypeList.Values['system.serial_number'] := 'String';
  GlobalSettingTypeList.Values['system.sku'] := 'String';
  GlobalSettingTypeList.Values['system.version'] := 'String';
  GlobalSettingTypeList.Values['board.manufacturer'] := 'String';
  GlobalSettingTypeList.Values['board.product_name'] := 'String';
  GlobalSettingTypeList.Values['board.version'] := 'String';
  GlobalSettingTypeList.Values['board.serial_number'] := 'String';
  GlobalSettingTypeList.Values['board.asset_tag'] := 'String';
  GlobalSettingTypeList.Values['board.location'] := 'String';
  GlobalSettingTypeList.Values['chassis.manufacturer'] := 'String';
  GlobalSettingTypeList.Values['chassis.version'] := 'String';
  GlobalSettingTypeList.Values['chassis.serial_number'] := 'String';
  GlobalSettingTypeList.Values['chassis.asset_tag'] := 'String';
  GlobalSettingTypeList.Values['chassis.sku'] := 'String';
  { x86 }
  {$ifdef CPUAMD64}
  GlobalSettingTypeList.Values['x86.mptable'] := 'Boolean';
  GlobalSettingTypeList.Values['x86.x2apic'] := 'Boolean';
  GlobalSettingTypeList.Values['x86.strictio'] := 'Boolean';
  GlobalSettingTypeList.Values['x86.strictmsr'] := 'Boolean';
  if GetOsreldate.ToInt64 >= 1500023 then
  begin
    GlobalSettingTypeList.Values['x86.verbosemsr'] := 'Boolean';
  end;
  GlobalSettingTypeList.Values['x86.vmexit_on_hlt'] := 'Boolean';
  GlobalSettingTypeList.Values['x86.vmexit_on_pause'] := 'Boolean';
  {$endif}

  // Remove when bhyve will updated on FreeBSD 13.x and 14.x
  { System }
  if GetOsreldate.ToInt64 >= 1500023 then
  begin
    GlobalSettingCategoryList.Values['bootrom'] := 'System';
    {$ifdef CPUAMD64}
    GlobalSettingCategoryList.Values['bootvars'] := 'System';
    {$endif}
  end;

  {$ifdef CPUAARCH64}
  GlobalSettingCategoryList.Values['console'] := 'System';
  {$endif}
  GlobalSettingCategoryList.Values['destroy_on_poweroff'] := 'System';
  GlobalSettingCategoryList.Values['keyboard.layout'] := 'System';
  GlobalSettingCategoryList.Values['rtc.use_localtime'] := 'System';
  GlobalSettingCategoryList.Values['virtio_msix'] := 'System';
  GlobalSettingCategoryList.Values['uuid'] := 'System';
  { Processor }
  GlobalSettingCategoryList.Values['cpus'] := 'Processor';
  GlobalSettingCategoryList.Values['cores'] := 'Processor';
  GlobalSettingCategoryList.Values['threads'] := 'Processor';
  GlobalSettingCategoryList.Values['sockets'] := 'Processor';
  { Memory }
  GlobalSettingCategoryList.Values['memory.guest_in_core'] := 'Memory';
  GlobalSettingCategoryList.Values['memory.size'] := 'Memory';
  GlobalSettingCategoryList.Values['memory.wired'] := 'Memory';
  { ACPI }
  GlobalSettingCategoryList.Values['acpi_tables'] := 'ACPI';
  GlobalSettingCategoryList.Values['acpi_tables_in_memory'] := 'ACPI';
  { Debugging }
  GlobalSettingCategoryList.Values['gdb.address'] := 'Debugging';
  GlobalSettingCategoryList.Values['gdb.port'] := 'Debugging';
  GlobalSettingCategoryList.Values['gdb.wait'] := 'Debugging';
  { TPM }
  GlobalSettingCategoryList.Values['tpm.path'] := 'TPM';
  GlobalSettingCategoryList.Values['tpm.type'] := 'TPM';
  GlobalSettingCategoryList.Values['tpm.version'] := 'TPM';
  { Bios }
  GlobalSettingCategoryList.Values['bios.vendor'] := 'BIOS';
  GlobalSettingCategoryList.Values['bios.version'] := 'BIOS';
  GlobalSettingCategoryList.Values['bios.release_date'] := 'BIOS';
  GlobalSettingCategoryList.Values['system.family_name'] := 'BIOS';
  GlobalSettingCategoryList.Values['system.manufacturer'] := 'BIOS';
  GlobalSettingCategoryList.Values['system.product_name'] := 'BIOS';
  GlobalSettingCategoryList.Values['system.serial_number'] := 'BIOS';
  GlobalSettingCategoryList.Values['system.sku'] := 'BIOS';
  GlobalSettingCategoryList.Values['system.version'] := 'BIOS';
  GlobalSettingCategoryList.Values['board.manufacturer'] := 'BIOS';
  GlobalSettingCategoryList.Values['board.product_name'] := 'BIOS';
  GlobalSettingCategoryList.Values['board.version'] := 'BIOS';
  GlobalSettingCategoryList.Values['board.serial_number'] := 'BIOS';
  GlobalSettingCategoryList.Values['board.asset_tag'] := 'BIOS';
  GlobalSettingCategoryList.Values['board.location'] := 'BIOS';
  GlobalSettingCategoryList.Values['chassis.manufacturer'] := 'BIOS';
  GlobalSettingCategoryList.Values['chassis.version'] := 'BIOS';
  GlobalSettingCategoryList.Values['chassis.serial_number'] := 'BIOS';
  GlobalSettingCategoryList.Values['chassis.asset_tag'] := 'BIOS';
  GlobalSettingCategoryList.Values['chassis.sku'] := 'BIOS';
  { x86 }
  {$ifdef CPUAMD64}
  GlobalSettingCategoryList.Values['x86.mptable'] := 'x86';
  GlobalSettingCategoryList.Values['x86.x2apic'] := 'x86';
  GlobalSettingCategoryList.Values['x86.strictio'] := 'x86';
  GlobalSettingCategoryList.Values['x86.strictmsr'] := 'x86';
  if GetOsreldate.ToInt64 >= 1500023 then
  begin
    GlobalSettingCategoryList.Values['x86.verbosemsr'] := 'x86';
  end;
  GlobalSettingCategoryList.Values['x86.vmexit_on_hlt'] := 'x86';
  GlobalSettingCategoryList.Values['x86.vmexit_on_pause'] := 'x86';
  {$endif}
end;

{
  This procedure is used to fill "Device settings page" with main category
  names.
}
procedure TFormBhyveManager.FillDeviceCategoryList();
var
  i: Integer;
  DevicesCategoryList: TStringList;
begin
  DevicesCategoryList := TStringList.Create;

  DevicesCategoryList.Add('Audio');
  DevicesCategoryList.Add('Console');
  {$ifdef CPUAMD64}
  DevicesCategoryList.Add('Display');
  {$endif}
  DevicesCategoryList.Add('Hostbridge');
  DevicesCategoryList.Add('Input');
  {$ifdef CPUAMD64}
  DevicesCategoryList.Add('LPC');
  {$endif}
  DevicesCategoryList.Add('Network');
  {$ifdef CPUAMD64}
  DevicesCategoryList.Add('Passthru');
  {$endif}
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

{
  This procedure is used to load "Device setting" default keys, values, and
  types for each main categories.
}
procedure TFormBhyveManager.FillDeviceDetailList();
begin
  DevicesList.Values['hda'] := 'Audio';
  {$ifdef CPUAMD64}
  DevicesList.Values['fbuf'] := 'Display';
  {$endif}
  DevicesList.Values['amdhostbridge'] := 'Hostbridge';
  DevicesList.Values['hostbridge'] := 'Hostbridge';
  DevicesList.Values['virtio-input'] := 'Input';
  {$ifdef CPUAMD64}
  DevicesList.Values['lpc'] := 'LPC';
  {$endif}
  DevicesList.Values['virtio-net'] := 'Network';
  DevicesList.Values['e1000'] := 'Network';
  {$ifdef CPUAMD64}
  DevicesList.Values['passthru'] := 'Passthru';
  {$endif}
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

{
  This procedure is used to fill "Virtual machines" treeview with virtual
  machine names and current state. Only virtual machines with bhyve_config.conf
  and vmname.conf configuration files at VmPath are loaded.
}
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

  VirtualMachinesTreeView.AlphaSort;
end;

{
  This procedure is used to fill "Virtual machines" treeview with a new virtual
  machine name and data when a new one is added.
  Only virtual machine with bhyve_config.conf and vmname.conf configuration
  files is loaded.
}
procedure TFormBhyveManager.FillVirtualMachine(VmName: String);
var
  Nodo : TTreeNode;
begin
  if FileExists(VmPath+'/'+VmName+'/bhyve_config.conf') and FileExists(VmPath+'/'+VmName+'/'+VmName+'.conf') then
    begin
      VirtualMachine:=LoadVirtualMachineData(VmPath+'/'+VmName+'/'+VmName+'.conf');

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

{
  This procedure is used to delete all content of a TreeView passed as
   parameter.
}
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

{
  This function is used to save bhyve_config.conf changes to virtual machine
  directory.
}
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

{
  This procedure runs rutines depending of a virtual machine status code when
  MyVmThread thread calls an OnExitStatus event. The MyVmThread thread is used
  to run a virtual machine from bhyvemgr.
}
procedure TFormBhyveManager.VirtualMachineShowStatus(Status: Integer;
  Message: String; VmName : String; ErrorMessage : String);
var
  i : Integer;
  PidNumber : Integer;
begin
  { Rebooting }
  if Status = 0 then
  begin
    if DirectoryExists(VmPath+'/'+VmName+'/vtcon') then
    begin
      RemoveDirectory(VmName+'/vtcon', True);
      CreateDirectory(VmPath+'/'+VmName+'/vtcon', GetCurrentUserName());
    end;

    if TVirtualMachineClass(VirtualMachinesTreeView.Items.FindNodeWithText(VmName+' : Running').Data).nat then
    begin
      PfUnloadRules(VmName, 'nat');
    end;

    if TVirtualMachineClass(VirtualMachinesTreeView.Items.FindNodeWithText(VmName+' : Running').Data).pf then
    begin
      PfUnloadRules(VmName, 'rdr');
      PfUnloadRules(VmName, 'pass-in');
      PfUnloadRules(VmName, 'pass-out');
    end;

    StatusBarBhyveManager.Font.Color:=clTeal;
    StatusBarBhyveManager.SimpleText := Message;

    if UseSystray = 'yes' then
    begin
      TrayIcon.TrayIcon.BalloonHint:=Message;
      TrayIcon.TrayIcon.BalloonTimeout:=TrayIconNotifytimeout;
      TrayIcon.TrayIcon.BalloonFlags:=bfInfo;
      TrayIcon.TrayIcon.ShowBalloonHint;
    end;

    DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : '+Message);

    MyVmThread := VmThread.Create(VmName);
    MyVmThread.OnExitStatus := @VirtualMachineShowStatus;
    MyVmThread.Start;

    if TVirtualMachineClass(VirtualMachinesTreeView.Items.FindNodeWithText(VmName+' : Running').Data).nat then
    begin
      PfloadRules(VmName, 'nat');
    end;

    if TVirtualMachineClass(VirtualMachinesTreeView.Items.FindNodeWithText(VmName+' : Running').Data).pf then
    begin
      PfloadRules(VmName, 'rdr');
      PfloadRules(VmName, 'pass-in');
      PfloadRules(VmName, 'pass-out');
    end;
  end
  { Powered off and Halted}
  else if (Status = 1) OR (Status = 2) then
  begin
    for i:=NetworkDeviceList.Count-1 downto 0 do
    begin
      if NetworkDeviceList.ValueFromIndex[i] = VmName then
      begin
          DestroyNetworkInterface(NetworkDeviceList.Names[i]);
          NetworkDeviceList.Delete(i);
      end;
    end;

    if Assigned(VirtualMachinesTreeView.Items.FindNodeWithText(VmName+' : Running')) then
      VirtualMachinesTreeView.Items.FindNodeWithText(VmName+' : Running').Text:=VmName;

    if VirtualMachinesTreeView.Selected.Text = VmName then
    begin
      SpeedButtonVncVm.Enabled:=False;
      SpeedButtonRemoveVm.Enabled:=True;
      SpeedButtonStartVm.Enabled:=True;
      SpeedButtonStopVm.Enabled:=False;
      SpeedButtonReloadVmConfig.Enabled:=True;
    end;

    DestroyVirtualMachine(VmName);
    RemoveDirectory(VmName+'/vtcon', True);

    if GetOsreldate.ToInt64 >= 1403000 then
    begin
      PidNumber:=CheckTpmSocketRunning(VmName);
      if PidNumber > 0 then
        KillPid(PidNumber);
    end;

    StatusBarBhyveManager.Font.Color:=clTeal;
    StatusBarBhyveManager.SimpleText := Message;

    if UseSystray = 'yes' then
    begin
      TrayIcon.TrayIcon.BalloonHint:=Message;
      TrayIcon.TrayIcon.BalloonTimeout:=TrayIconNotifytimeout;
      TrayIcon.TrayIcon.BalloonFlags:=bfInfo;
      TrayIcon.TrayIcon.ShowBalloonHint;
    end;

    if TVirtualMachineClass(VirtualMachinesTreeView.Items.FindNodeWithText(VmName).Data).nat then
    begin
      PfUnloadRules(VmName, 'nat');
    end;

    if TVirtualMachineClass(VirtualMachinesTreeView.Items.FindNodeWithText(VmName).Data).pf then
    begin
      PfUnloadRules(VmName, 'rdr');
      PfUnloadRules(VmName, 'pass-in');
      PfUnloadRules(VmName, 'pass-out');
    end;

    DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : '+Message);
  end
  { Other exit status }
  else if Status > 2 then
  begin
    if Assigned(VirtualMachinesTreeView.Items.FindNodeWithText(VmName+' : Running')) then
      VirtualMachinesTreeView.Items.FindNodeWithText(VmName+' : Running').Text:=VmName;

    if VirtualMachinesTreeView.Selected.Text = VmName then
    begin
      SpeedButtonVncVm.Enabled:=False;
      SpeedButtonStopVm.Enabled:=False;
      SpeedButtonStartVm.Enabled:=True;
      SpeedButtonRemoveVm.Enabled:=True;
    end;

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

      if GetOsreldate.ToInt64 >= 1403000 then
      begin
        PidNumber:=CheckTpmSocketRunning(VmName);
        if PidNumber > 0 then
          KillPid(PidNumber);
      end;
    end;

    if TVirtualMachineClass(VirtualMachinesTreeView.Items.FindNodeWithText(VmName).Data).nat then
    begin
      PfUnloadRules(VmName, 'nat');
    end;

    if TVirtualMachineClass(VirtualMachinesTreeView.Items.FindNodeWithText(VmName).Data).pf then
    begin
      PfUnloadRules(VmName, 'rdr');
      PfUnloadRules(VmName, 'pass-in');
      PfUnloadRules(VmName, 'pass-out');
    end;

    StatusBarBhyveManager.SimpleText := EmptyStr;
    MessageDialog(mtError, Message);

    DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : '+Message);

    if ErrorMessage <> EmptyStr then
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : '+VmName+' VM : '+ErrorMessage);

    MyVmThread.Terminate;
  end;
end;

{
  This procedure is called to show us the copy status of image files while it
  is not finishedd.
}
procedure TFormBhyveManager.AppShowStatus(Status: Integer; AppPid: Integer);
var
  total : Int64;
begin
  ProcessPid:=AppPid;

  total:=ConvertFileSize(GetFileSize(DiskFile), 'M');

  StatusBarBhyveManager.Font.Color:=clTeal;
  StatusBarBhyveManager.SimpleText:=Format(copy_status, [total.ToString]);
end;

{
  This procedure is called when copy of image file is finished.
}
procedure TFormBhyveManager.AppEndStatus(Status: Integer; AppName: String; AppPid: Integer);
var
  total : Int64;
begin
  if (Status = 0) then
  begin
    ProcessPid:=-1;

    total:=ConvertFileSize(GetFileSize(RemoteFile), 'M');

    StatusBarBhyveManager.Font.Color:=clTeal;
    StatusBarBhyveManager.SimpleText:=Format(copy_status, [total.ToString]);

    FillVirtualMachine(FormVmCreate.EditVmName.Text);
    VirtualMachinesTreeView.AlphaSort;

    StatusBarBhyveManager.Font.Color:=clTeal;
    StatusBarBhyveManager.SimpleText:=Format(vm_create_status, [FormVmCreate.EditVmName.Text]);

    Sleep(100);
  end
  else if (Status > 0) then
  begin
    FormVmCreate.StatusBarVmCreate.Font.Color:=clRed;
    FormVmCreate.StatusBarVmCreate.SimpleText:=Format(app_error_status, [FormVmCreate.EditVmName.Text, AppName, Status.ToString]);
  end;
end;

{
  This procedure is used to generate a global setting form when this needs
  changed. Also, it evaluates some rules before of generate some forms.
}
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
    FormChangeValue.Caption:=extractVarName(GlobalSettingsTreeView.Selected.Text);
    FormChangeValue.BitBtnSave.OnClick:=@GlobalChangeValue;
    FormChangeValue.Visible:=True;
  end;

  if GlobalSettingTypeList.Values[extractVarName(GlobalSettingsTreeView.Selected.Text)] = 'Integer' then
  begin
    SettingName:=extractVarName(GlobalSettingsTreeView.Selected.Text);

    case SettingName of
        'cpus',
        'cores',
        'threads',
        'sockets':
        begin
          NodeIndex:=GlobalSettingsTreeView.Selected.AbsoluteIndex;

          FormChangeValue.ShowComboBox();
          FormChangeValue.ComboBoxValue.Clear;
          FormChangeValue.SettingType:=SettingName;
          FillComboIntegerType(FormChangeValue.ComboBoxValue, 1, Trim(CheckSysctl('hw.vmm.maxcpu')).ToInteger, 1);
          FormChangeValue.ComboBoxValue.ItemIndex:=FormChangeValue.ComboBoxValue.Items.IndexOf(extractVarValue(GlobalSettingsTreeView.Selected.Text));
          FormChangeValue.Caption:=extractVarName(GlobalSettingsTreeView.Selected.Text);
          FormChangeValue.BitBtnSave.OnClick:=@GlobalChangeValue;
          FormChangeValue.Visible:=True;
        end;
        'gdb.port':
        begin
          NodeIndex:=GlobalSettingsTreeView.Selected.AbsoluteIndex;

          FormChangeValue.ShowComboBox();
          FormChangeValue.ComboBoxValue.Clear;
          FormChangeValue.SettingType:=SettingName;
          FormChangeValue.ComboBoxValue.Items.Add('0');
          FillComboIntegerType(FormChangeValue.ComboBoxValue, FirstGdbPortNumber, FirstGdbPortNumber + 100, 1);
          FormChangeValue.ComboBoxValue.ItemIndex:=FormChangeValue.ComboBoxValue.Items.IndexOf(extractVarValue(GlobalSettingsTreeView.Selected.Text));
          FormChangeValue.Caption:=extractVarName(GlobalSettingsTreeView.Selected.Text);
          FormChangeValue.BitBtnSave.OnClick:=@GlobalChangeValue;
          FormChangeValue.Visible:=True;
        end;
    end;
  end;

  if GlobalSettingTypeList.Values[extractVarName(GlobalSettingsTreeView.Selected.Text)] = 'String' then
  begin
    SettingName:=extractVarName(GlobalSettingsTreeView.Selected.Text);

    case SettingName of
        'bootrom':
        begin
          NodeIndex:=GlobalSettingsTreeView.Selected.AbsoluteIndex;

          FormChangeValue.ShowComboBox();
          FormChangeValue.ComboBoxValue.Clear;
          FormChangeValue.SettingType:=SettingName;
          FillComboBootrom(FormChangeValue.ComboBoxValue);
          FormChangeValue.ComboBoxValue.ItemIndex:=FormChangeValue.ComboBoxValue.Items.IndexOf(ExtractFileName(extractVarValue(GlobalSettingsTreeView.Selected.Text)));
          FormChangeValue.Caption:=extractVarName(GlobalSettingsTreeView.Selected.Text);
          FormChangeValue.BitBtnSave.OnClick:=@GlobalChangeValue;
          FormChangeValue.Visible:=True;
        end;
        {$ifdef CPUAMD64}
        'bootvars':
        begin
          NodeIndex:=GlobalSettingsTreeView.Selected.AbsoluteIndex;

          FormChangeValue.ShowComboBox();
          FormChangeValue.ComboBoxValue.Clear;
          FormChangeValue.SettingType:=SettingName;
          FillComboBootvars(FormChangeValue.ComboBoxValue);
          FormChangeValue.ComboBoxValue.ItemIndex:=FormChangeValue.ComboBoxValue.Items.IndexOf(ExtractFileName(extractVarValue(GlobalSettingsTreeView.Selected.Text)));
          FormChangeValue.Caption:=extractVarName(GlobalSettingsTreeView.Selected.Text);
          FormChangeValue.BitBtnSave.OnClick:=@GlobalChangeValue;
          FormChangeValue.Visible:=True;
        end;
        {$endif}
        {$ifdef CPUAARCH64}
        'console':
        begin
          NodeIndex:=GlobalSettingsTreeView.Selected.AbsoluteIndex;
          VirtualMachine:=TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data);

          FormChangeValue.ShowComboBox();
          FormChangeValue.ComboBoxValue.Clear;
          FormChangeValue.SettingType:=SettingName;
          FormChangeValue.ComboBoxValue.Items.Add('/dev/nmdm-'+VirtualMachine.name+'.1A');
          FormChangeValue.ComboBoxValue.Items.Add('tcp=0.0.0.0:'+GetNewComPortNumber());
          FormChangeValue.ComboBoxValue.Items.Add('tcp=127.0.0.1:'+GetNewComPortNumber());
          if UseIpv6 = 'yes' then
          begin
            FormChangeValue.ComboBoxValue.Items.Add('tcp=[::]:'+GetNewComPortNumber());
            FormChangeValue.ComboBoxValue.Items.Add('tcp=[::1]:'+GetNewComPortNumber());
          end;
          FormChangeValue.ComboBoxValue.ItemIndex:=FormChangeValue.ComboBoxValue.Items.IndexOf(ExtractFileName(extractVarValue(GlobalSettingsTreeView.Selected.Text)));
          FormChangeValue.Caption:=extractVarName(GlobalSettingsTreeView.Selected.Text);
          FormChangeValue.BitBtnSave.OnClick:=@GlobalChangeValue;
          FormChangeValue.Visible:=True;
        end;
        {$endif}
        'keyboard.layout':
        begin
          NodeIndex:=GlobalSettingsTreeView.Selected.AbsoluteIndex;

          FormChangeValue.ShowComboBox();
          FormChangeValue.ComboBoxValue.Clear;
          FormChangeValue.SettingType:=SettingName;
          FillComboKeyboardLayout(FormChangeValue.ComboBoxValue);
          FormChangeValue.ComboBoxValue.ItemIndex:=FormChangeValue.ComboBoxValue.Items.IndexOf(extractVarValue(GlobalSettingsTreeView.Selected.Text));
          FormChangeValue.Caption:=extractVarName(GlobalSettingsTreeView.Selected.Text);
          FormChangeValue.BitBtnSave.OnClick:=@GlobalChangeValue;
          FormChangeValue.Visible:=True;
        end;
        'tpm.path':
        begin
          NodeIndex:=GlobalSettingsTreeView.Selected.AbsoluteIndex;

          FormChangeValue.ShowComboBox();
          FormChangeValue.ComboBoxValue.Clear;
          FormChangeValue.SettingType:=SettingName;

          case ExtractVarValue(GlobalSettingsTreeView.Items.FindTopLvlNode('TPM').Items[1].Text) of
              'passthru':
              begin
                FillComboTpmDevice(FormChangeValue.ComboBoxValue);
                FormChangeValue.ComboBoxValue.ItemIndex:=FormChangeValue.ComboBoxValue.Items.IndexOf(extractVarValue(GlobalSettingsTreeView.Selected.Text));
              end;
              'swtpm':
              begin
                if GetOsreldate.ToInt64 >= 1403000 then
                begin
                  FormChangeValue.ComboBoxValue.Clear;
                  FormChangeValue.ComboBoxValue.Items.Add(VmPath+'/'+TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).name+'/tpm/swtpm.sock');
                end;
              end;
              else
              begin
                FormChangeValue.ComboBoxValue.Clear;
              end;
          end;

          FormChangeValue.Caption:=extractVarName(GlobalSettingsTreeView.Selected.Text);
          FormChangeValue.BitBtnSave.OnClick:=@GlobalChangeValue;
          FormChangeValue.Visible:=True;
        end;
        'tpm.type':
        begin
          NodeIndex:=GlobalSettingsTreeView.Selected.AbsoluteIndex;

          FormChangeValue.ShowComboBox();
          FormChangeValue.ComboBoxValue.Clear;
          FormChangeValue.SettingType:=SettingName;
          FillComboTpmType(FormChangeValue.ComboBoxValue);
          FormChangeValue.ComboBoxValue.ItemIndex:=FormChangeValue.ComboBoxValue.Items.IndexOf(extractVarValue(GlobalSettingsTreeView.Selected.Text));
          FormChangeValue.Caption:=extractVarName(GlobalSettingsTreeView.Selected.Text);
          FormChangeValue.BitBtnSave.OnClick:=@GlobalChangeValue;
          FormChangeValue.Visible:=True;
        end;
        'tpm.version':
        begin
          NodeIndex:=GlobalSettingsTreeView.Selected.AbsoluteIndex;

          FormChangeValue.ShowComboBox();
          FormChangeValue.ComboBoxValue.Clear;
          FormChangeValue.SettingType:=SettingName;
          FillComboTpmVersion(FormChangeValue.ComboBoxValue);
          FormChangeValue.ComboBoxValue.ItemIndex:=FormChangeValue.ComboBoxValue.Items.IndexOf(extractVarValue(GlobalSettingsTreeView.Selected.Text));
          FormChangeValue.Caption:=extractVarName(GlobalSettingsTreeView.Selected.Text);
          FormChangeValue.BitBtnSave.OnClick:=@GlobalChangeValue;
          FormChangeValue.Visible:=True;
        end;
        'memory.size':
        begin
          NodeIndex:=GlobalSettingsTreeView.Selected.AbsoluteIndex;

          FormChangeValue.ComboBoxValue.Clear;
          FormChangeValue.SettingType:=SettingName;
          FormChangeValue.ShowSpinEx(256, Trim(CheckSysctl('hw.usermem')).ToInt64, 256);
          FormChangeValue.SpinEditExValue.Value:=extractVarValue(GlobalSettingsTreeView.Selected.Text).Replace('M', EmptyStr).ToInt64;
          FormChangeValue.Caption:=extractVarName(GlobalSettingsTreeView.Selected.Text)+' (MB)';
          FormChangeValue.BitBtnSave.OnClick:=@GlobalChangeValue;
          FormChangeValue.Visible:=True;
        end;
        'bios.vendor',
        'bios.version',
        'bios.release_date',
        'system.family_name',
        'system.manufacturer',
        'system.product_name',
        'system.serial_number',
        'system.sku',
        'system.version',
        'board.manufacturer',
        'board.product_name',
        'board.version',
        'board.serial_number',
        'board.asset_tag',
        'board.location',
        'chassis.manufacturer',
        'chassis.version',
        'chassis.serial_number',
        'chassis.asset_tag',
        'chassis.sku':
        begin
          NodeIndex:=GlobalSettingsTreeView.Selected.AbsoluteIndex;

          FormChangeValue.ShowComboBox();
          FormChangeValue.ComboBoxValue.Clear;
          FormChangeValue.ComboBoxValue.ReadOnly:=False;
          FormChangeValue.ComboBoxValue.Items.Add(ExtractVarValue(GlobalSettingsTreeView.Selected.Text));

          if extractVarValue(GlobalSettingsTreeView.Selected.Text) <> GlobalSettingDefaultValueList.Values[SettingName] then
            FormChangeValue.ComboBoxValue.Items.Add(GlobalSettingDefaultValueList.Values[SettingName]);

          FormChangeValue.SettingType:=SettingName;
          FormChangeValue.ComboBoxValue.ItemIndex:=FormChangeValue.ComboBoxValue.Items.IndexOf(extractVarValue(GlobalSettingsTreeView.Selected.Text));
          FormChangeValue.Caption:=extractVarName(GlobalSettingsTreeView.Selected.Text);
          FormChangeValue.BitBtnSave.OnClick:=@GlobalChangeValue;
          FormChangeValue.Visible:=True;
        end;
    end;
  end;
end;

{
  This procedure is used to save global setting changes to virtual machine
  directory. Also, it evaluates some rules before of save the changes.
}
procedure TFormBhyveManager.GlobalChangeValue(Sender: TObject);
begin
  if (FormChangeValue.SettingType = 'memory.size') then
    StatusBarBhyveManager.SimpleText:=Format(edit_global_status+'M', [FormChangeValue.SettingType, FormChangeValue.SpinEditExValue.Text])
  else
    StatusBarBhyveManager.SimpleText:=Format(edit_global_status, [FormChangeValue.SettingType, FormChangeValue.ComboBoxValue.Text]);

  if (FormChangeValue.SettingType = 'bootrom') then
  begin
    {$ifdef CPUAMD64}
    GlobalSettingsTreeView.Items.Item[NodeIndex].Text:=ExtractVarName(GlobalSettingsTreeView.Selected.Text)+' : '+BootRomUefiPath+'/'+FormChangeValue.ComboBoxValue.Text;
    {$endif CPUAMD64}
    {$ifdef CPUAARCH64}
    GlobalSettingsTreeView.Items.Item[NodeIndex].Text:=ExtractVarName(GlobalSettingsTreeView.Selected.Text)+' : '+BootRomUbootPath+'/'+FormChangeValue.ComboBoxValue.Text;
    {$endif CPUAARCH64}
  end
  else if (FormChangeValue.SettingType = 'bootvars') then
  begin
    {$ifdef CPUAMD64}
    if not (FormChangeValue.ComboBoxValue.Text = EmptyStr) then
    begin
      GlobalSettingsTreeView.Items.Item[NodeIndex].Text:=ExtractVarName(GlobalSettingsTreeView.Selected.Text)+' : '+VmPath+'/'+TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).name+'/'+FormChangeValue.ComboBoxValue.Text;
      if not FileExists(VmPath+'/'+TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).name+'/'+FormChangeValue.ComboBoxValue.Text) then
      begin
        CreateFile(VmPath+'/'+TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).name+'/'+FormChangeValue.ComboBoxValue.Text, GetCurrentUserName());
        CopyFile(BootRomUefiPath+'/BHYVE_UEFI_VARS.fd', VmPath+'/'+TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).name+'/'+FormChangeValue.ComboBoxValue.Text);
      end;
    end
    else
      GlobalSettingsTreeView.Items.Item[NodeIndex].Text:=ExtractVarName(GlobalSettingsTreeView.Selected.Text)+' : ';
    {$endif CPUAMD64}
  end
  else if (FormChangeValue.SettingType = 'memory.size') then
    GlobalSettingsTreeView.Items.Item[NodeIndex].Text:=ExtractVarName(GlobalSettingsTreeView.Selected.Text)+' : '+FormChangeValue.SpinEditExValue.Text+'M'
  else if (FormChangeValue.SettingType = 'tpm.type') then
  begin
    GlobalSettingsTreeView.Items.Item[NodeIndex].Text:=ExtractVarName(GlobalSettingsTreeView.Selected.Text)+' : '+FormChangeValue.ComboBoxValue.Text;

    case ExtractVarValue(GlobalSettingsTreeView.Selected.Text) of
        'passthru':
         begin
           GlobalSettingsTreeView.Items.FindTopLvlNode('TPM').Items[0].Text:='tpm.path : ';
           GlobalSettingsTreeView.Items.FindTopLvlNode('TPM').Items[2].Text:='tpm.version : 2.0';
         end;
        'swtpm':
         begin
           if GetOsreldate.ToInt64 >= 1403000 then
           begin
             GlobalSettingsTreeView.Items.FindTopLvlNode('TPM').Items[0].Text:='tpm.path : '+VmPath+'/'+TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).name+'/tpm/swtpm.sock';
             GlobalSettingsTreeView.Items.FindTopLvlNode('TPM').Items[2].Text:='tpm.version : 2.0';
           end;
         end;
         else
         begin
           GlobalSettingsTreeView.Items.FindTopLvlNode('TPM').Items[0].Text:='tpm.path : ';
           GlobalSettingsTreeView.Items.FindTopLvlNode('TPM').Items[2].Text:='tpm.version : ';
         end;
    end;
  end
  else
    GlobalSettingsTreeView.Items.Item[NodeIndex].Text:=ExtractVarName(GlobalSettingsTreeView.Selected.Text)+' : '+FormChangeValue.ComboBoxValue.Text;

  SaveVirtualMachineConfig();

  FormChangeValue.Close;
end;

{
  This procedure is used to call Settings form if not exists a bhyvemgr
  configuration file.
}
procedure TFormBhyveManager.FormActivate(Sender: TObject);
begin
  if NewConfig then
  begin
    MessageDialog(mtInformation, configuration_notice);
    SetNewConfig(False);
    FormSettings.Show;
  end;
end;

{
  This procedure is used to avoid close bhyvemgr if there are virtual machines
  on running state.
}
procedure TFormBhyveManager.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  i, j : Integer;
  flag : Boolean;
  Node : TTreeNode;
begin
  flag:=False;
  CanClose:=True;

  if FormVmCreate.IsVisible and (FormVmCreate.ProcessPid > 0) then
  begin
    MessageDialog(mtInformation, check_create_task);
    CanClose:=False;
    Exit;
  end;

  if ProcessPid > 0 then
  begin
    if MessageDialog(mtConfirmation, check_create_task_confirmation) = mrYes then
    begin
      KillPid(ProcessPid);
      ProcessPid:=-1;
      StatusBarBhyveManager.SimpleText:=EmptyStr;
      Exit;
    end;
  end;

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
    if MessageDialog(mtConfirmation, check_vm_running) = mrNo then
      CanClose := False;
  end;
end;

{
  This procedure is used like a temporary workaround to show bhyvemgr main
  window correctly when LCLGTK2 is used with latest version of Lazarus.
}
procedure TFormBhyveManager.FormShow(Sender: TObject);
begin
  {$ifdef LCLGTK2}
  FormBhyveManager.BorderStyle:=bsSingle;
  FormBhyveManager.Height:=619;
  {$endif}
end;

{
  This procedure is used to show a Create Virtual Machine form and assign an
  OnClick event.
}
procedure TFormBhyveManager.SpeedButtonAddVmClick(Sender: TObject);
begin
  FormVmCreate.BitBtnCreateVm.OnClick:=@CreateVmClick;
  FormVmCreate.Visible:=True;
end;

{
  This procedure is used to show or hide a systray icon if it is enable by default.
}
procedure TFormBhyveManager.ShowHideClick(Sender: TObject);
begin
 if not FormBhyveManager.IsVisible then
   FormBhyveManager.Show
 else
   FormBhyveManager.Hide;
end;

procedure TFormBhyveManager.ComboBoxLanguageChange(Sender: TObject);
var
  ConfigFile : ConfigurationClass;
begin
  ConfigFile:=ConfigurationClass.Create(GetUserDir + '.config/bhyvemgr/config.conf');

  SetDefaultLang(ComboBoxLanguage.Text, DatadirPath+'languages');
  Translations.TranslateUnitResourceStrings('LCLStrConsts', DatadirPath+'languages/lcl/lclstrconsts.'+ComboBoxLanguage.Text+'.po');

  TrayIcon.TrayIcon.PopUpMenu.Items[0].Caption:=popup_tray_show_hide;
  TrayIcon.TrayIcon.PopUpMenu.Items[1].Caption:=popup_tray_quit;

  GlobalSettingsPopup.PopupMenu.Items[0].Caption:=popup_edit_global_setting;

  VirtualMachinesPopup.PopupMenu.Items[0].Caption:=popup_add_vm;
  VirtualMachinesPopup.PopupMenu.Items[1].Caption:=popup_modify_vm;
  VirtualMachinesPopup.PopupMenu.Items[2].Caption:=popup_remove_vm;
  VirtualMachinesPopup.PopupMenu.Items[3].Caption:=popup_rdp_vm;
  VirtualMachinesPopup.PopupMenu.Items[4].Caption:=popup_pf_rules_vm;
  VirtualMachinesPopup.PopupMenu.Items[6].Caption:=popup_copy_vm_name;
  VirtualMachinesPopup.PopupMenu.Items[7].Caption:=popup_copy_com1_command;
  VirtualMachinesPopup.PopupMenu.Items[8].Caption:=popup_copy_ipv4;
  VirtualMachinesPopup.PopupMenu.Items[9].Caption:=popup_copy_ipv6;

  DevicesPopup.PopupMenu.Items[0].Caption:=popup_add_device;
  DevicesPopup.PopupMenu.Items[1].Caption:=popup_edit_device;
  DevicesPopup.PopupMenu.Items[2].Caption:=popup_delete_device;

  ConfigFile.SetOption('general', 'language', ComboBoxLanguage.Text);
  SetLanguage(ComboBoxLanguage.Text);

  StatusBarBhyveManager.SimpleText:=EmptyStr;

  ConfigFile.Free;
end;

{
  This procedure is used to show "about" form when it is called from main menu.
}
procedure TFormBhyveManager.MenuItemAboutClick(Sender: TObject);
begin
  FormAbout.Visible:=True;
end;

{
  This procedure is used to close bhyvemgr when it is called from main menu or
  systray icon.
}
procedure TFormBhyveManager.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;

{
  This procedure is used to show "settings" form when it is called from main
  menu.
}
procedure TFormBhyveManager.MenuItemSettingsClick(Sender: TObject);
begin
  FormSettings.Visible:=True;
end;

{
  This procedure is used to free memory from some components.
}
procedure TFormBhyveManager.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : '+debugln_bhyve_finished);

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
  TrayIconPopup.Free;
  TrayIcon.Free;
  NetworkDeviceList.Free;
  VirtualMachineList.Free;

  TmpDevicesStringList.Free;
  DebugLogger.Free;
end;

{
  This procedure is called when "Add device" option is selected from Device
  settings treeview. A device form will be opened accord of main category
  selected: Audio, Console, Display, etc.
}
procedure TFormBhyveManager.AddDevice(Sender: TObject);
var
  i : Integer;
  Node : TTreeNode;
  PciSlot : String;
begin
  Node := DeviceSettingsTreeView.Selected;

  if Assigned(VirtualMachinesTreeView.Selected) then
  begin
    case Node.Text of
        'Audio':
          begin
            if not Assigned(FormAudioDevice) then
              FormAudioDevice:=TFormAudioDevice.Create(FormBhyveManager);

            FormAudioDevice.BitBtnSave.OnClick:=@SaveAudioDevice;
            FormAudioDevice.FormStyle:=fsSystemStayOnTop;
            FormAudioDevice.FormAction:='Add';
            FormAudioDevice.LoadDefaultValues();
            FormAudioDevice.Show;
          end;
        'Console':
          begin
            if not Assigned(FormConsoleDevice) then
              FormConsoleDevice:=TFormConsoleDevice.Create(FormBhyveManager);

            FormConsoleDevice.BitBtnSave.OnClick:=@SaveConsoleDevice;
            FormConsoleDevice.FormStyle:=fsSystemStayOnTop;
            FormConsoleDevice.VtconName:='vtcon'+GetNewConsoleName(TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).name);
            FormConsoleDevice.VmName:=TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).name;
            FormConsoleDevice.FormAction:='Add';
            FormConsoleDevice.LoadDefaultValues();
            FormConsoleDevice.Show;
          end;
        'Display':
          begin
            if (Node.Count = 0) then
            begin
              if not Assigned(FormDisplayDevice) then
                FormDisplayDevice:=TFormDisplayDevice.Create(FormBhyveManager);

              FormDisplayDevice.BitBtnSave.OnClick:=@SaveDisplayDevice;
              FormDisplayDevice.FormStyle:=fsSystemStayOnTop;
              FormDisplayDevice.FormAction:='Add';
              FormDisplayDevice.LoadDefaultValues();
              FormDisplayDevice.Show;
            end;
          end;
        'Hostbridge':
          begin
            if (Node.Count = 0) then
            begin
              if not Assigned(FormHostbridgeDevice) then
                FormHostbridgeDevice:=TFormHostbridgeDevice.Create(FormBhyveManager);

              FormHostbridgeDevice.BitBtnSave.OnClick:=@SaveHostbridgeDevice;
              FormHostbridgeDevice.FormStyle:=fsSystemStayOnTop;
              FormHostbridgeDevice.FormAction:='Add';
              FormHostbridgeDevice.LoadDefaultValues();
              FormHostbridgeDevice.Show;
            end;
          end;
        'Input':
          begin
            if not Assigned(FormInputDevice) then
              FormInputDevice:=TFormInputDevice.Create(FormBhyveManager);

            FormInputDevice.BitBtnSave.OnClick:=@SaveInputDevice;
            FormInputDevice.FormStyle:=fsSystemStayOnTop;
            FormInputDevice.LoadDefaultValues();

            for i:=0 to DeviceSettingsTreeView.Items.FindTopLvlNode('Input').Count-1 do
            begin
              FormInputDevice.ComboBoxInputDevice.Items.Delete(FormInputDevice.ComboBoxInputDevice.Items.IndexOf(TVirtioInputDeviceClass(DeviceSettingsTreeView.Items.FindTopLvlNode('Input').Items[i].Data).path));
            end;

            FormInputDevice.FormAction:='Add';
            FormInputDevice.Show;
          end;
        'LPC':
          begin
            if (Node.Count = 0) then
            begin
              if not Assigned(FormLpcDevice) then
                FormLpcDevice:=TFormLpcDevice.Create(FormBhyveManager);

              FormLpcDevice.BitBtnSave.OnClick:=@SaveLpcDevice;
              FormLpcDevice.FormStyle:=fsSystemStayOnTop;
              FormLpcDevice.FormVmName:=TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).name;
              FormLpcDevice.FormAction:='Add';
              FormLpcDevice.LoadDefaultValues();
              FormLpcDevice.Show;
            end;
          end;
        'Network':
          begin
            if not Assigned(FormNetworkDevice) then
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
            if not Assigned(FormPassthruDevice) then
              FormPassthruDevice:=TFormPassthruDevice.Create(FormBhyveManager);

            FormPassthruDevice.BitBtnSave.OnClick:=@SavePassthruDevice;
            FormPassthruDevice.FormStyle:=fsSystemStayOnTop;
            FormPassthruDevice.LoadDefaultValues();

            for i:=0 to DeviceSettingsTreeView.Items.FindTopLvlNode('Passthru').Count-1 do
            begin
              if (FormPassthruDevice.ComboBoxDevice.Items.IndexOf(TPassthruDeviceClass(DeviceSettingsTreeView.Items.FindTopLvlNode('Passthru').Items[i].Data).pptdev) <> -1) then
                FormPassthruDevice.ComboBoxDevice.Items.Delete(FormPassthruDevice.ComboBoxDevice.Items.IndexOf(TPassthruDeviceClass(DeviceSettingsTreeView.Items.FindTopLvlNode('Passthru').Items[i].Data).pptdev));
            end;

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

              GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindTopLvlNode('RNG'), 'device : '+RNGDevice.device);
              GlobalNode.Data:=RNGDevice;
              GlobalNode.ImageIndex:=12;
              GlobalNode.SelectedIndex:=12;

              DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+RNGDevice.pci);

              SaveVirtualMachineConfig();
            end;
          end;
        'Shared folders':
          begin
            if not Assigned(FormShareFolderDevice) then
              FormShareFolderDevice:=TFormShareFolderDevice.Create(FormBhyveManager);

            FormShareFolderDevice.BitBtnSave.OnClick:=@SaveShareFolderDevice;
            FormShareFolderDevice.FormStyle:=fsSystemStayOnTop;
            FormShareFolderDevice.LoadDefaultValues();
            FormShareFolderDevice.FormAction:='Add';
            FormShareFolderDevice.Show;
          end;
        'Storage':
          begin
            if not Assigned(FormStorageDevice) then
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
              PciSlot:='0.30.0';

              TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:='xhci';
              TmpDevicesStringList.Values['pci.'+PciSlot+'.slot.1.device']:='tablet';

              UsbXhciDevice:=FillDetailUsbXhciDevice(TmpDevicesStringList.Text, PciSlot, 'xhci', 1);

              GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindTopLvlNode('USB'), 'device : '+UsbXhciDevice.device+'-'+UsbXhciDevice.slot_device);
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

{
  This procedure is used to copy virtual machine name to desktop clipboard.
}
procedure TFormBhyveManager.CopyVmNameClick(Sender: TObject);
begin
  if Assigned(VirtualMachinesTreeView.Selected) and (VirtualMachinesTreeView.Selected.Level <> 0) then
  begin
    if VirtualMachinesTreeView.Selected.Text.Contains(':') then
      Clipboard.AsText:=ExtractVarName(VirtualMachinesTreeView.Selected.Text)
    else
      Clipboard.AsText:=VirtualMachinesTreeView.Selected.Text;
  end;
end;

{
  This procedure is used to copy com connection command to desktop clipboard.
}
procedure TFormBhyveManager.CopyComCommandClick(Sender: TObject);
var
  ComDevice : String;
begin
  if Assigned(VirtualMachinesTreeView.Selected) and (VirtualMachinesTreeView.Selected.Level <> 0) then
  begin
    VirtualMachine:=TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data);

    {$ifdef CPUAARCH64}
    ComDevice:=GlobalSettingsTreeView.Items.FindTopLvlNode('System').Items[1].Text;

    if ComDevice.Contains('/dev/nmdm') then
      Clipboard.AsText:='cu -l /dev/nmdm-'+VirtualMachine.name+'.1B';

    if ComDevice.Contains('tcp=') and not ComDevice.Contains('[') then
      Clipboard.AsText:='netcat 127.0.0.1 '+ExtractPortValue(ComDevice)
    else if ComDevice.Contains('tcp=') and ComDevice.Contains('[') then
      Clipboard.AsText:='netcat -6 ::1 '+ExtractPortValue(ComDevice);
    {$endif}
    {$ifdef CPUAMD64}
    LPCDevice:=TLPCDeviceClass(DeviceSettingsTreeView.Items.FindTopLvlNode('LPC').Items[0].Data);
    ComDevice:=LPCDevice.com1;

    if ComDevice.Contains('/dev/nmdm') then
      Clipboard.AsText:='cu -l /dev/nmdm-'+VirtualMachine.name+'.1B';

    if ComDevice.Contains('tcp=') and not ComDevice.Contains('[') then
      Clipboard.AsText:='netcat 127.0.0.1 '+ExtractPortValue(ComDevice)
    else if ComDevice.Contains('tcp=') and ComDevice.Contains('[') then
      Clipboard.AsText:='netcat -6 ::1 '+ExtractPortValue(ComDevice);
    {$endif}
  end;
end;

{
  This procedure is used to copy virtual machiine IPv4 address to desktop clipboard.
}
procedure TFormBhyveManager.CopyIpv4AddressClick(Sender: TObject);
begin
  if Assigned(VirtualMachinesTreeView.Selected) and (VirtualMachinesTreeView.Selected.Level <> 0) then
  begin
    Clipboard.AsText:=TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).ipaddress;
  end;
end;

{
  This procedure is used to copy virtual machiine IPv6 address to desktop clipboard.
}
procedure TFormBhyveManager.CopyIpv6AddressClick(Sender: TObject);
begin
  if Assigned(VirtualMachinesTreeView.Selected) and (VirtualMachinesTreeView.Selected.Level <> 0) then
  begin
    Clipboard.AsText:=TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).ip6address;
  end;
end;

{
  This procedure is called when "Edit device" option is selected from "Device
  settings" treeview. A device form will be opened accord of device selected:
  device : hda, device : virtio-console, device : fbuf, etc.
}
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

            if not Assigned(FormAudioDevice) then
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

            if not Assigned(FormConsoleDevice) then
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

            if not Assigned(FormDisplayDevice) then
              FormDisplayDevice:=TFormDisplayDevice.Create(FormBhyveManager);

            FormDisplayDevice.BitBtnSave.OnClick:=@SaveDisplayDevice;
            FormDisplayDevice.FormStyle:=fsSystemStayOnTop;
            FormDisplayDevice.HostPort:=ExtractPortValue(DisplayDevice.tcp);
            FormDisplayDevice.FormAction:='Update';
            FormDisplayDevice.LoadDefaultValues();
            FormDisplayDevice.ComboBoxHost.ItemIndex:=FormDisplayDevice.ComboBoxHost.Items.IndexOf(DisplayDevice.tcp);

            if DisplayDevice.wait <> EmptyStr then
              FormDisplayDevice.CheckBoxWaitVnc.Checked:=StrToBool(DisplayDevice.wait)
            else
              FormDisplayDevice.CheckBoxWaitVnc.Checked:=False;

            if DisplayDevice.tcp.Contains('0.0.0.0') or DisplayDevice.tcp.Contains('[::]')  then
              FormDisplayDevice.CheckBoxOnlyLocalhost.Checked:=False;

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

            if not Assigned(FormHostbridgeDevice) then
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

            if not Assigned(FormInputDevice) then
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

            if not Assigned(FormLpcDevice) then
              FormLpcDevice:=TFormLpcDevice.Create(FormBhyveManager);

            FormLpcDevice.BitBtnSave.OnClick:=@SaveLpcDevice;
            FormLpcDevice.FormStyle:=fsSystemStayOnTop;
            FormLpcDevice.FormVmName:=TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).name;
            FormLpcDevice.FormAction:='Update';
            FormLpcDevice.LoadDefaultValues();
            FormLpcDevice.CheckBoxCom1.Checked:=False;


            { Remove when bhyve will updated on FreeBSD 13.x and 14.x }
            if GetOsreldate.ToInt64 < 1500023 then
            begin
              FormLpcDevice.ComboBoxBootrom.ItemIndex:=FormLpcDevice.ComboBoxBootrom.Items.IndexOf(ExtractFileName(LPCDevice.bootrom));
              FormLpcDevice.ComboBoxBootvars.ItemIndex:=FormLpcDevice.ComboBoxBootvars.Items.IndexOf(ExtractFileName(LPCDevice.bootvars));
            end
            else
            begin
              { GlobalSettingsTreeView = Global Settings / TopLvlItems[0] = System / Items[0] = bootroom }
              FormLpcDevice.ComboBoxBootrom.ItemIndex:=FormLpcDevice.ComboBoxBootrom.Items.IndexOf(ExtractFileName(GlobalSettingsTreeView.Items.TopLvlItems[0].Items[0].Text));
              { GlobalSettingsTreeView = Global Settings / TopLvlItems[0] = System / Items[1] = bootvars }
              FormLpcDevice.ComboBoxBootvars.ItemIndex:=FormLpcDevice.ComboBoxBootvars.Items.IndexOf(ExtractFileName(GlobalSettingsTreeView.Items.TopLvlItems[0].Items[1].Text));
            end;

            if LPCDevice.com1 <> EmptyStr then
            begin
              if (GetOsreldate.ToInt64 > 1500023) then
              begin
                if LPCDevice.com1.Contains('tcp=') then
                begin
                   FormLpcDevice.ComboBoxCom1.Items.Add('tcp=127.0.0.1:'+ExtractPortValue(LPCDevice.com1));
                   FormLpcDevice.ComboBoxCom1.Items.Add('tcp=0.0.0.0:'+ExtractPortValue(LPCDevice.com1));

                   if UseIpv6 = 'yes' then
                   begin
                     FormLpcDevice.ComboBoxCom1.Items.Add('tcp=[::1]:'+ExtractPortValue(LPCDevice.com1));
                     FormLpcDevice.ComboBoxCom1.Items.Add('tcp=[::]:'+ExtractPortValue(LPCDevice.com1));
                   end;
                end
                else
                begin
                  FormLpcDevice.ComboBoxCom1.Items.Add('tcp=127.0.0.1:'+GetNewComPortNumber());
                  FormLpcDevice.ComboBoxCom1.Items.Add('tcp=0.0.0.0:'+GetNewComPortNumber());

                  if UseIpv6 = 'yes' then
                  begin
                    FormLpcDevice.ComboBoxCom1.Items.Add('tcp=[::1]:'+GetNewComPortNumber());
                    FormLpcDevice.ComboBoxCom1.Items.Add('tcp=[::]:'+GetNewComPortNumber());
                  end;
                end;
              end;

              FormLpcDevice.ComboBoxCom1.ItemIndex:=FormLpcDevice.ComboBoxCom1.Items.IndexOf(LPCDevice.com1);
              FormLpcDevice.CheckBoxCom1.Checked:=True;
            end
            else
            begin
              FormLpcDevice.ComboBoxCom1.Items.Add('tcp=127.0.0.1:'+GetNewComPortNumber());
              FormLpcDevice.ComboBoxCom1.Items.Add('tcp=0.0.0.0:'+GetNewComPortNumber());

              if UseIpv6 = 'yes' then
              begin
                FormLpcDevice.ComboBoxCom1.Items.Add('tcp=[::1]:'+GetNewComPortNumber());
                FormLpcDevice.ComboBoxCom1.Items.Add('tcp=[::]:'+GetNewComPortNumber());
              end;
            end;

            if LPCDevice.com2 <> EmptyStr then
            begin
              FormLpcDevice.ComboBoxCom2.ItemIndex:=FormLpcDevice.ComboBoxCom2.Items.IndexOf(LPCDevice.com2);
              FormLpcDevice.CheckBoxCom2.Checked:=True;
            end;

            if LPCDevice.com3 <> EmptyStr then
            begin
              FormLpcDevice.ComboBoxCom3.ItemIndex:=FormLpcDevice.ComboBoxCom3.Items.IndexOf(LPCDevice.com3);
              FormLpcDevice.CheckBoxCom3.Checked:=True;
            end;

            if LPCDevice.com4 <> EmptyStr then
            begin
              FormLpcDevice.ComboBoxCom4.ItemIndex:=FormLpcDevice.ComboBoxCom4.Items.IndexOf(LPCDevice.com4);
              FormLpcDevice.CheckBoxCom4.Checked:=True;
            end;

            FormLpcDevice.ComboBoxFwcfg.ItemIndex:=FormLpcDevice.ComboBoxFwcfg.Items.IndexOf(LPCDevice.fwcfg);

            FormLpcDevice.Show;
          end;
        'Network':
          begin
            NetworkDevice := TNetworkDeviceClass(Node.Data);

            if not Assigned(FormNetworkDevice) then
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

            if not Assigned(FormPassthruDevice) then
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

            if not Assigned(FormShareFolderDevice) then
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
            if not Assigned(FormStorageDevice) then
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
                    FormStorageDevice.SpinEditExDiskSize.Enabled:=True;
                    FormStorageDevice.EditSer.Text:=StorageAhciDevice.ser;
                    FormStorageDevice.CheckBoxNoCache.Checked:=StorageAhciDevice.nocache;
                    FormStorageDevice.CheckBoxNoDelete.Checked:=StorageAhciDevice.nodelete;
                    FormStorageDevice.CheckBoxSync.Checked:=StorageAhciDevice.sync;
                    FormStorageDevice.CheckBoxReadOnly.Checked:=StorageAhciDevice.ro;

                    if StorageAhciDevice.storage_type = 'image file' then
                      FormStorageDevice.DiskSize:=GetFileSize(StorageAhciDevice.path, 'G')
                    else
                      FormStorageDevice.DiskSize:=ZfsGetPropertyValue(StorageAhciDevice.path.Replace('/dev/zvol/', EmptyStr), 'volsize', 'value').Replace('G', EmptyStr).ToInt64;
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
                    FormStorageDevice.SpinEditExDiskSize.Value:=Round(StorageNvmeDevice.storage_size.Replace('G',EmptyStr).ToInt64);
                    FormStorageDevice.SpinEditExDiskSize.Enabled:=True;
                    FormStorageDevice.EditSer.Text:=StorageNvmeDevice.ser;
                    FormStorageDevice.CheckBoxNoCache.Checked:=StorageNvmeDevice.nocache;
                    FormStorageDevice.CheckBoxNoDelete.Checked:=StorageNvmeDevice.nodelete;
                    FormStorageDevice.CheckBoxSync.Checked:=StorageNvmeDevice.sync;
                    FormStorageDevice.CheckBoxReadOnly.Checked:=StorageNvmeDevice.ro;

                    if StorageNvmeDevice.storage_type = 'image file' then
                      FormStorageDevice.DiskSize:=GetFileSize(StorageNvmeDevice.devpath, 'G')
                    else
                      FormStorageDevice.DiskSize:=ZfsGetPropertyValue(StorageNvmeDevice.devpath.Replace('/dev/zvol/', EmptyStr), 'volsize', 'value').Replace('G', EmptyStr).ToInt64;

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
                    FormStorageDevice.SpinEditExDiskSize.Enabled:=True;
                    FormStorageDevice.FileNameEditStoragePath.Text:=StorageVirtioBlkDevice.path;
                    FormStorageDevice.EditSer.Text:=StorageVirtioBlkDevice.ser;
                    FormStorageDevice.CheckBoxNoCache.Checked:=StorageVirtioBlkDevice.nocache;
                    FormStorageDevice.CheckBoxNoDelete.Checked:=StorageVirtioBlkDevice.nodelete;
                    FormStorageDevice.CheckBoxSync.Checked:=StorageVirtioBlkDevice.sync;
                    FormStorageDevice.CheckBoxReadOnly.Checked:=StorageVirtioBlkDevice.ro;

                    if StorageVirtioBlkDevice.storage_type = 'image file' then
                      FormStorageDevice.DiskSize:=GetFileSize(StorageVirtioBlkDevice.path, 'G')
                    else
                      FormStorageDevice.DiskSize:=ZfsGetPropertyValue(StorageVirtioBlkDevice.path.Replace('/dev/zvol/', EmptyStr), 'volsize', 'value').Replace('G', EmptyStr).ToInt64;
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

{
  This procedure is called when "Delete device" option is selected from "Device
  settings" treeview. The device node will be deleted.
}
procedure TFormBhyveManager.RemoveDevice(Sender: TObject);
var
  i : Integer;
  PciSlot : String;
begin
  if (Assigned(DeviceSettingsTreeView.Selected)) and (DeviceSettingsTreeView.Selected.Level = 1) then
  begin
    if (MessageDialog(mtConfirmation, Format(device_remove_notice, [ExtractVarValue(DeviceSettingsTreeView.Selected.Text)])) = mrYes) then
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

{
  This procedure is called when "RDP to virtual machine" option is
  selected from "Virtual machines" treeview. An "RDP Login credentials"
  form is opened to start a RDP connection to a virtual machine selected
}
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
      FormRdpConnection.FormUserName:=FormRdpConnection.EditUsername.Text;
      FormRdpConnection.FormResolution:=FormRdpConnection.ComboBoxResolution.Text;
      FormRdpConnection.Hide;
    end;
  end;
end;

{
  This procedure is used to save audio device settings
}
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

   FormAudioDevice.Hide;

   SaveVirtualMachineConfig();
 end
 else if (FormAudioDevice.FormAction = 'Update') and FormAudioDevice.FormValidate() then
 begin
   PciSlot:=AudioDevice.pci;

   TmpDevicesStringList.Values['pci.'+PciSlot+'.play']:=FormAudioDevice.EditPlayDevice.Text;
   TmpDevicesStringList.Values['pci.'+PciSlot+'.rec']:=FormAudioDevice.EditRecDevice.Text;

   AudioDevice.play:=FormAudioDevice.EditPlayDevice.Text;
   AudioDevice.rec:=FormAudioDevice.EditRecDevice.Text;

   FormAudioDevice.Hide;

   SaveVirtualMachineConfig();
 end
 else
 begin
   FormAudioDevice.StatusBarAudioDevice.Font.Color:=clRed;
   FormAudioDevice.StatusBarAudioDevice.SimpleText:=Format(device_status, ['Audio']);
 end;
end;

{
  This procedure is used to save console device settings
}
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

   FormConsoleDevice.Hide;

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

   FormConsoleDevice.Hide;

   SaveVirtualMachineConfig();
 end
 else
 begin
   FormConsoleDevice.StatusBarConsoleDevice.Font.Color:=clRed;
   FormConsoleDevice.StatusBarConsoleDevice.SimpleText:=Format(device_status, ['Console']);
 end;
end;

{
  This procedure is used to save audio display settings
}
procedure TFormBhyveManager.SaveDisplayDevice(Sender: TObject);
var
  PciSlot : String;
begin
 if (FormDisplayDevice.FormAction = 'Add') and FormDisplayDevice.FormValidate() then
 begin
   PciSlot:='0.29.0';

   TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:='fbuf';
   TmpDevicesStringList.Values['pci.'+PciSlot+'.tcp']:=FormDisplayDevice.ComboBoxHost.Text;
   if FormDisplayDevice.CheckBoxWaitVnc.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.wait']:='true';
   TmpDevicesStringList.Values['pci.'+PciSlot+'.w']:=ExtractDelimited(1,FormDisplayDevice.ComboBoxResolution.Text,['x']);
   TmpDevicesStringList.Values['pci.'+PciSlot+'.h']:=ExtractDelimited(2,FormDisplayDevice.ComboBoxResolution.Text,['x']);
   TmpDevicesStringList.Values['pci.'+PciSlot+'.vga']:=FormDisplayDevice.ComboBoxVga.Text;
   if (FormDisplayDevice.CheckBoxUsePassword.Checked and (trim(FormDisplayDevice.EditPassword.Text) <> EmptyStr)) then TmpDevicesStringList.Values['pci.'+PciSlot+'.password']:=FormDisplayDevice.EditPassword.Text;

   DisplayDevice:=FillDetailDisplayDevice(TmpDevicesStringList.Text, PciSlot, 'fbuf');

   GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('Display'), 'device : '+DisplayDevice.device);
   GlobalNode.Data:=DisplayDevice;
   GlobalNode.ImageIndex:=4;
   GlobalNode.SelectedIndex:=4;

   DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+DisplayDevice.pci);

   FormDisplayDevice.Hide;

   SaveVirtualMachineConfig();
 end
 else if (FormDisplayDevice.FormAction = 'Update') and FormDisplayDevice.FormValidate() then
 begin
   PciSlot:=DisplayDevice.pci;

   TmpDevicesStringList.Values['pci.'+PciSlot+'.tcp']:=FormDisplayDevice.ComboBoxHost.Text;
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

   DisplayDevice.tcp:=FormDisplayDevice.ComboBoxHost.Text;
   DisplayDevice.port:=StrToInt(ExtractPortValue(FormDisplayDevice.ComboBoxHost.Text));
   DisplayDevice.wait:=BoolToStr(FormDisplayDevice.CheckBoxWaitVnc.Checked, 'true', 'false');
   DisplayDevice.w:=ExtractDelimited(1,FormDisplayDevice.ComboBoxResolution.Text,['x']).ToInteger;
   DisplayDevice.h:=ExtractDelimited(2,FormDisplayDevice.ComboBoxResolution.Text,['x']).ToInteger;
   DisplayDevice.vga:=FormDisplayDevice.ComboBoxVga.Text;
   DisplayDevice.pass:=Trim(FormDisplayDevice.EditPassword.Text);

   FormDisplayDevice.Hide;

   SaveVirtualMachineConfig();
 end
 else
 begin
   FormDisplayDevice.StatusBarDisplayDevice.Font.Color:=clRed;
   FormDisplayDevice.StatusBarDisplayDevice.SimpleText:=Format(device_status, ['Display']);
 end;
end;

{
  This procedure is used to save hostbridge device settings.
}
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

   FormHostbridgeDevice.Hide;

   SaveVirtualMachineConfig();
 end
 else if (FormHostbridgeDevice.FormAction = 'Update') and FormHostbridgeDevice.FormValidate() then
 begin
   PciSlot:=HostBridgeDevice.pci;

   TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:=FormHostbridgeDevice.ComboBoxHostbridgeDevice.Text;

   DeviceSettingsTreeView.Items.FindNodeWithText('device : '+HostBridgeDevice.device).Text:='device : '+FormHostbridgeDevice.ComboBoxHostbridgeDevice.Text;

   HostBridgeDevice.device:=FormHostbridgeDevice.ComboBoxHostbridgeDevice.Text;

   FormHostbridgeDevice.Hide;

   SaveVirtualMachineConfig();
 end
 else
 begin
   FormHostbridgeDevice.StatusBarHostbridgeDevice.Font.Color:=clRed;
   FormHostbridgeDevice.StatusBarHostbridgeDevice.SimpleText:=Format(device_status, ['Hostbridge']);
 end;
end;

{
  This procedure is used to save input device settings
}
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

   FormInputDevice.Hide;

   SaveVirtualMachineConfig();
 end
 else if (FormInputDevice.FormAction = 'Update') and FormInputDevice.FormValidate() then
 begin
   PciSlot:=InputDevice.pci;

   TmpDevicesStringList.Values['pci.'+PciSlot+'.path']:=FormInputDevice.ComboBoxInputDevice.Text;

   InputDevice.path:=FormInputDevice.ComboBoxInputDevice.Text;

   FormInputDevice.Hide;

   SaveVirtualMachineConfig();
 end
 else
 begin
   FormInputDevice.StatusBarInputDevice.Font.Color:=clRed;
   FormInputDevice.StatusBarInputDevice.SimpleText:=Format(device_status, ['Input']);
 end;
end;

{
  This procedure is used to save lpc device settings
}
procedure TFormBhyveManager.SaveLpcDevice(Sender: TObject);
var
  PciSlot : String;
begin
 if (FormLpcDevice.FormAction = 'Add') and FormLpcDevice.FormValidate() then
 begin
   PciSlot:='0.31.0';

   TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:='lpc';

   { Remove when bhyve will updated on FreeBSD 13.x and 14.x }
   if GetOsreldate.ToInt64 < 1500023 then
   begin
     {$ifdef CPUAMD64}
     TmpDevicesStringList.Values['lpc.bootrom']:= BootRomUefiPath+'/'+FormLpcDevice.ComboBoxBootrom.Text;
     if FormLpcDevice.ComboBoxBootvars.Text <> EmptyStr then
     begin
       TmpDevicesStringList.Values['lpc.bootvars']:= VmPath+'/'+FormLpcDevice.FormVmName+'/'+FormLpcDevice.ComboBoxBootvars.Text;

       if not FileExists(VmPath+'/'+FormLpcDevice.FormVmName+'/'+FormLpcDevice.ComboBoxBootvars.Text) then
       begin
         CreateFile(VmPath+'/'+FormLpcDevice.FormVmName+'/'+FormLpcDevice.ComboBoxBootvars.Text, GetCurrentUserName());
         CopyFile(BootRomUefiPath+'/BHYVE_UEFI_VARS.fd', VmPath+'/'+FormLpcDevice.FormVmName+'/'+FormLpcDevice.ComboBoxBootvars.Text);
       end;
     end;
     {$endif CPUAMD64}
     {$ifdef CPUAARCH64}
     TmpDevicesStringList.Values['lpc.bootrom']:=BootRomUbootPath+'/'+FormLpcDevice.ComboBoxBootrom.Text;
     {$endif CPUAARCH64}
   end;

   if FormLpcDevice.CheckBoxCom1.Checked then TmpDevicesStringList.Values['lpc.com1.path']:=FormLpcDevice.ComboBoxCom1.Text;
   if FormLpcDevice.CheckBoxCom2.Checked then TmpDevicesStringList.Values['lpc.com2.path']:=FormLpcDevice.ComboBoxCom2.Text;
   if FormLpcDevice.CheckBoxCom3.Checked then TmpDevicesStringList.Values['lpc.com3.path']:=FormLpcDevice.ComboBoxCom3.Text;
   if FormLpcDevice.CheckBoxCom4.Checked then TmpDevicesStringList.Values['lpc.com4.path']:=FormLpcDevice.ComboBoxCom4.Text;

   TmpDevicesStringList.Values['lpc.fwcfg']:=FormLpcDevice.ComboBoxFwcfg.Text;

   LPCDevice:=FillDetailLpcDevice(TmpDevicesStringList.Text, PciSlot, 'lpc');

   GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('LPC'), 'device : '+LPCDevice.device);
   GlobalNode.Data:=LPCDevice;
   GlobalNode.ImageIndex:=8;
   GlobalNode.SelectedIndex:=8;

   DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+LPCDevice.pci);

   FormLpcDevice.Hide;

   SaveVirtualMachineConfig();
 end
 else if (FormLpcDevice.FormAction = 'Update') and FormLpcDevice.FormValidate() then
 begin
   PciSlot:=LPCDevice.pci;

   if GetOsreldate.ToInt64 < 1500023 then
   begin
     {$ifdef CPUAMD64}
     if FormLpcDevice.ComboBoxBootvars.Text = EmptyStr then
     begin
       if TmpDevicesStringList.IndexOfName('lpc.bootvars') <> -1 then
         TmpDevicesStringList.Delete(TmpDevicesStringList.IndexOfName('lpc.bootvars'));
       LPCDevice.bootvars:=EmptyStr;
     end
     else
     begin
       if not FileExists(VmPath+'/'+FormLpcDevice.FormVmName+'/'+FormLpcDevice.ComboBoxBootvars.Text) then
       begin
         CreateFile(VmPath+'/'+FormLpcDevice.FormVmName+'/'+FormLpcDevice.ComboBoxBootvars.Text, GetCurrentUserName());
         CopyFile(BootRomUefiPath+'/BHYVE_UEFI_VARS.fd', VmPath+'/'+FormLpcDevice.FormVmName+'/'+FormLpcDevice.ComboBoxBootvars.Text);
       end;

       TmpDevicesStringList.Values['lpc.bootvars']:= VmPath+'/'+FormLpcDevice.FormVmName+'/'+ FormLpcDevice.ComboBoxBootvars.Text;
       LPCDevice.bootvars:= VmPath+'/'+FormLpcDevice.FormVmName+'/'+ FormLpcDevice.ComboBoxBootvars.Text;
     end;
     {$endif CPUAMD64}
   end;

   LPCDevice.com1:=EmptyStr;
   LPCDevice.com2:=EmptyStr;
   LPCDevice.com3:=EmptyStr;
   LPCDevice.com4:=EmptyStr;

   if FormLpcDevice.CheckBoxCom1.Checked then
   begin
     TmpDevicesStringList.Values['lpc.com1.path']:=FormLpcDevice.ComboBoxCom1.Text;
     LPCDevice.com1:=FormLpcDevice.ComboBoxCom1.Text;
   end
   else
   begin
     if TmpDevicesStringList.IndexOfName('lpc.com1.path') <> -1 then
       TmpDevicesStringList.Delete(TmpDevicesStringList.IndexOfName('lpc.com1.path'));
   end;

   if FormLpcDevice.CheckBoxCom2.Checked then
   begin
     TmpDevicesStringList.Values['lpc.com2.path']:=FormLpcDevice.ComboBoxCom2.Text;
     LPCDevice.com2:=FormLpcDevice.ComboBoxCom2.Text;
   end
   else
   begin
     if TmpDevicesStringList.IndexOfName('lpc.com2.path') <> -1 then
       TmpDevicesStringList.Delete(TmpDevicesStringList.IndexOfName('lpc.com2.path'));
   end;

   if FormLpcDevice.CheckBoxCom3.Checked then
   begin
     TmpDevicesStringList.Values['lpc.com3.path']:=FormLpcDevice.ComboBoxCom3.Text;
     LPCDevice.com3:=FormLpcDevice.ComboBoxCom3.Text;
   end
   else
   begin
     if TmpDevicesStringList.IndexOfName('lpc.com3.path') <> -1 then
       TmpDevicesStringList.Delete(TmpDevicesStringList.IndexOfName('lpc.com3.path'));
   end;

   if FormLpcDevice.CheckBoxCom4.Checked then
   begin
     TmpDevicesStringList.Values['lpc.com4.path']:=FormLpcDevice.ComboBoxCom4.Text;
     LPCDevice.com4:=FormLpcDevice.ComboBoxCom4.Text;
   end
   else
   begin
     if TmpDevicesStringList.IndexOfName('lpc.com4.path') <> -1 then
       TmpDevicesStringList.Delete(TmpDevicesStringList.IndexOfName('lpc.com4.path'));
   end;

   TmpDevicesStringList.Values['lpc.fwcfg']:=FormLpcDevice.ComboBoxFwcfg.Text;

   FormLpcDevice.Hide;

   SaveVirtualMachineConfig();
 end
 else
 begin
   FormLpcDevice.StatusBarLpcDevice.Font.Color:=clRed;
   FormLpcDevice.StatusBarLpcDevice.SimpleText:=Format(device_status, ['LPC']);;
 end;
end;

{
  This procedure is used to save network device settings
}
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

   FormNetworkDevice.Hide;

   SaveVirtualMachineConfig();
 end
 else if (FormNetworkDevice.FormAction = 'Update') and FormNetworkDevice.FormValidate() then
 begin
   PciSlot:=NetworkDevice.pci;

   TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:=FormNetworkDevice.ComboBoxDevice.Text;
   TmpDevicesStringList.Values['pci.'+PciSlot+'.backend']:=FormNetworkDevice.EditBackend.Text;

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
   NetworkDevice.backend:=FormNetworkDevice.EditBackend.Text;
   NetworkDevice.mtu:=StrToInt(FormNetworkDevice.SpinEditExMtu.Text);

   FormNetworkDevice.Hide;

   SaveVirtualMachineConfig();
 end
 else
 begin
   FormNetworkDevice.StatusBarNetworkDevice.Font.Color:=clRed;
   FormNetworkDevice.StatusBarNetworkDevice.SimpleText:=Format(device_status, ['Network']);
 end;
end;

{
  This procedure is used to save passthru device settings
}
procedure TFormBhyveManager.SavePassthruDevice(Sender: TObject);
var
  PciSlot : String;
begin
 if (FormPassthruDevice.FormAction = 'Add') and FormPassthruDevice.FormValidate() then
 begin
   PciSlot:=GetNewPciSlotNumber(TmpDevicesStringList, 14);

   TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:='passthru';
   TmpDevicesStringList.Values['pci.'+PciSlot+'.pptdev']:=FormPassthruDevice.ComboBoxDevice.Text;
   if FormPassthruDevice.FileNameEditRom.Text <> EmptyStr then TmpDevicesStringList.Values['pci.'+PciSlot+'.rom']:=FormPassthruDevice.FileNameEditRom.FileName;

   PassthruDevice:=FillDetailPassthruDevice(TmpDevicesStringList.Text, PciSlot, 'passthru');

   GlobalNode:=DeviceSettingsTreeView.Items.AddChild(DeviceSettingsTreeView.Items.FindNodeWithText('Passthru'), 'device : '+PassthruDevice.device+'-'+PassthruDevice.pptdev);
   GlobalNode.Data:=PassthruDevice;
   GlobalNode.ImageIndex:=11;
   GlobalNode.SelectedIndex:=11;

   DeviceSettingsTreeView.Items.AddChild(GlobalNode, 'pci : '+PassthruDevice.pci);

   FormPassthruDevice.Hide;

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
     TmpDevicesStringList.Values['pci.'+PciSlot+'.rom']:=FormPassthruDevice.FileNameEditRom.FileName;
   end;

   DeviceSettingsTreeView.Items.FindNodeWithText('device : '+PassthruDevice.device+'-'+PassthruDevice.pptdev).Text:='device : '+PassthruDevice.device+'-'+FormPassthruDevice.ComboBoxDevice.Text;

   PassthruDevice.pptdev:=FormPassthruDevice.ComboBoxDevice.Text;

   FormPassthruDevice.Hide;

   SaveVirtualMachineConfig();
 end
 else
 begin
    FormPassthruDevice.StatusBarPassthruDevice.Font.Color:=clRed;
    FormPassthruDevice.StatusBarPassthruDevice.SimpleText:=Format(device_status, ['Passthru']);
 end;
end;

{
  This procedure is used to save share folder device settings
}
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

   FormShareFolderDevice.Hide;

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

   FormShareFolderDevice.Hide;

   SaveVirtualMachineConfig();
 end
 else
 begin
   FormShareFolderDevice.StatusBarSharefolderDevice.Font.Color:=clRed;
   FormShareFolderDevice.StatusBarSharefolderDevice.SimpleText:=Format(device_status, ['Sharefolder']);
 end;
end;

{
  This procedure is used to save storage device settings
}
procedure TFormBhyveManager.SaveStorageDevice(Sender: TObject);
var
  PciSlot : String;
  StoragePath : String;
  StorageSize : String;
  i : Integer;
begin
  StoragePath:=FormStorageDevice.FileNameEditStoragePath.FileName;
  StorageSize:=FormStorageDevice.SpinEditExDiskSize.Text;

  if (FormStorageDevice.FormAction = 'Add') and FormStorageDevice.FormValidate() then
  begin
    case FormStorageDevice.ComboBoxStorageDevice.Text of
        'ahci-cd':
          begin
            PciSlot:=GetNewPciSlotNumber(TmpDevicesStringList, 1);

            TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:='ahci';
            TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.path']:=FormStorageDevice.FileNameEditStoragePath.FileName;
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

            FormStorageDevice.Hide;

            SaveVirtualMachineConfig();
          end;
        'ahci-hd':
          begin
            PciSlot:=GetNewPciSlotNumber(TmpDevicesStringList, 2);

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

            FormStorageDevice.Hide;

            SaveVirtualMachineConfig();
          end;
        'nvme':
          begin
            PciSlot:=GetNewPciSlotNumber(TmpDevicesStringList, 3);

            TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:=FormStorageDevice.ComboBoxStorageDevice.Text;
            TmpDevicesStringList.Values['pci.'+PciSlot+'.path']:=FormStorageDevice.FileNameEditStoragePath.FileName;

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

            FormStorageDevice.Hide;

            SaveVirtualMachineConfig();
          end;
        'virtio-blk':
          begin
            PciSlot:=GetNewPciSlotNumber(TmpDevicesStringList, 5);

            TmpDevicesStringList.Values['pci.'+PciSlot+'.device']:=FormStorageDevice.ComboBoxStorageDevice.Text;
            TmpDevicesStringList.Values['pci.'+PciSlot+'.path']:=FormStorageDevice.FileNameEditStoragePath.FileName;

            if FormStorageDevice.EditSer.Text <> EmptyStr then TmpDevicesStringList.Values['pci.'+PciSlot+'.ser']:=FormStorageDevice.EditSer.Text;
            if FormStorageDevice.CheckBoxNoCache.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.nocache']:='true';
            if FormStorageDevice.CheckBoxNoDelete.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.nodelete']:='true';
            if FormStorageDevice.CheckBoxSync.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.sync']:='true';
            if FormStorageDevice.CheckBoxReadOnly.Checked then TmpDevicesStringList.Values['pci.'+PciSlot+'.ro']:='true';

            StorageVirtioBlkDevice:=FillDetailStorageVirtioBlkDevice(TmpDevicesStringList.Text, PciSlot, FormStorageDevice.ComboBoxStorageDevice.Text);

            StorageVirtioBlkDevice.storage_size:=FormStorageDevice.SpinEditExDiskSize.Text+'G';
            StorageVirtioBlkDevice.storage_type:=FormStorageDevice.ComboBoxStorageType.Text;

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

            FormStorageDevice.Hide;

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
                TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.path']:=FormStorageDevice.FileNameEditStoragePath.FileName;
                TmpDevicesStringList.Values['pci.'+PciSlot+'.port.0.type']:=StorageAhciDevice.device_type;

                StorageAhciDevice.path:=FormStorageDevice.FileNameEditStoragePath.FileName;

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

                FormStorageDevice.Hide;

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

                if FormStorageDevice.SpinEditExDiskSize.Value > StorageAhciDevice.storage_size.Replace('G', EmptyStr).ToInt64 then
                begin
                  if StorageAhciDevice.storage_type = 'image file' then
                    TruncateImage(StorageAhciDevice.path, FormStorageDevice.SpinEditExDiskSize.Value.ToString+'G')
                  else
                    ZfsSetPropertyValue(StorageAhciDevice.path.Replace('/dev/zvol/', EmptyStr), 'volsize', FormStorageDevice.SpinEditExDiskSize.Value.ToString+'G');

                  StorageAhciDevice.storage_size:=FormStorageDevice.SpinEditExDiskSize.Value.ToString+'G';
                end;

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

                FormStorageDevice.Hide;

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

                if FormStorageDevice.SpinEditExDiskSize.Value > StorageNvmeDevice.storage_size.Replace('G', EmptyStr).ToInt64 then
                begin
                  if StorageNvmeDevice.storage_type = 'image file' then
                    TruncateImage(StorageNvmeDevice.devpath, FormStorageDevice.SpinEditExDiskSize.Value.ToString+'G')
                  else
                    ZfsSetPropertyValue(StorageNvmeDevice.devpath.Replace('/dev/zvol/', EmptyStr), 'volsize', FormStorageDevice.SpinEditExDiskSize.Value.ToString+'G');

                  StorageNvmeDevice.storage_size:=FormStorageDevice.SpinEditExDiskSize.Value.ToString+'G';
                end;

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

                FormStorageDevice.Hide;

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

                if FormStorageDevice.SpinEditExDiskSize.Value > StorageVirtioBlkDevice.storage_size.Replace('G', EmptyStr).ToInt64 then
                begin
                  if StorageVirtioBlkDevice.storage_type = 'image file' then
                    TruncateImage(StorageVirtioBlkDevice.path, FormStorageDevice.SpinEditExDiskSize.Value.ToString+'G')
                  else
                    ZfsSetPropertyValue(StorageVirtioBlkDevice.path.Replace('/dev/zvol/', EmptyStr), 'volsize', FormStorageDevice.SpinEditExDiskSize.Value.ToString+'G');

                  StorageVirtioBlkDevice.storage_size:=FormStorageDevice.SpinEditExDiskSize.Value.ToString+'G';
                end;

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

                FormStorageDevice.Hide;

                SaveVirtualMachineConfig();
              end;
        end;
  end
  else
  begin
    FormStorageDevice.StatusBarStorageDevice.Font.Color:=clRed;
    FormStorageDevice.StatusBarStorageDevice.SimpleText:=Format(device_status, ['Storage']);
  end;
end;

{
  This procedure is used to save settings of a new virtual machine created from
  "Create Virtual Machine" form.
}
procedure TFormBhyveManager.CreateVmClick(Sender: TObject);
var
  NewVMConfig : ConfigurationClass;
  NewBhyveConfig : TStringList;
  SeedImageConfig : TStringList;
  Uuid : String;
  DiskName : String;
  IpAddress : String;
  Ip6Address : String;
  MacAddress : String;
  Path : String = '.path';
  PciSlot : String;
  SeedRunCmd : String;
  MyAppThread : AppProgressBarThread;
begin
  if FormVmCreate.FormValidate() then
  begin
    try
      FormVmCreate.StatusBarVmCreate.Font.Color:=clTeal;
      FormVmCreate.StatusBarVmCreate.SimpleText:=Format(vm_try_status, [FormVmCreate.EditVmName.Text]);
      FormVmCreate.BitBtnCreateVm.Enabled:=False;

      Application.ProcessMessages;

      NewBhyveConfig:=TStringList.Create;
      SeedImageConfig:=TStringList.Create;

      IpAddress:=EmptyStr;
      Ip6Address:=EmptyStr;
      SeedRunCmd:=EmptyStr;

      if UseZfs = 'yes' then
      begin
        if not DirectoryExists(VmPath) then
        begin
          if not (CreateDirectory(VmPath, GetCurrentUserName())) or not (ZfsCreateDataset(VmPath.Remove(0,1))) then
          begin
            DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : '+Format(debugln_dataset_status, [FormVmCreate.EditVmName.Text, VmPath]));
            Exit;
          end;
        end;
        if not ZfsCreateDataset(VmPath.Remove(0,1)+'/'+FormVmCreate.EditVmName.Text) then
        begin
          DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : '+Format(debugln_dataset_status, [FormVmCreate.EditVmName.Text, VmPath.Remove(0,1)+'/'+FormVmCreate.EditVmName.Text]));
          Exit;
        end;
      end
      else
      begin
        if not CreateDirectory(VmPath+'/'+FormVmCreate.EditVmName.Text, GetCurrentUserName()) then
        begin
          DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : '+Format(debugln_directory_status, [FormVmCreate.EditVmName.Text, VmPath+'/'+FormVmCreate.EditVmName.Text]));
          Exit;
        end;
      end;

      CreateFile(FormVmCreate.EditVmFolderPath.Text+'/'+FormVmCreate.EditVmName.Text+'.conf', GetCurrentUserName());
      NewVMConfig:=ConfigurationClass.Create(FormVmCreate.EditVmFolderPath.Text+'/'+FormVmCreate.EditVmName.Text+'.conf');

      Uuid:=GenerateUuid();

      if FormVmCreate.CheckBoxUseStaticIpv6.Checked then
        MacAddress:=FormVmCreate.MacAddress
      else
        MacAddress:=GenerateMacAddress();

      NewBhyveConfig.Values['uuid']:=Uuid;
      NewBhyveConfig.Values['name']:=FormVmCreate.EditVmName.Text;
      NewBhyveConfig.Values['memory.size']:=FormVmCreate.SpinEditExMemory.Text+'M';
      NewBhyveConfig.Values['cpus']:='1';
      NewBhyveConfig.Values['sockets']:='1';
      NewBhyveConfig.Values['cores']:='1';
      NewBhyveConfig.Values['rtc.use_localtime']:='true';

      {$ifdef CPUAMD64}
      NewBhyveConfig.Values['x86.vmexit_on_hlt']:='true';
      NewBhyveConfig.Values['x86.strictmsr']:='false';
      {$endif CPUAMD64}

      { Remove when bhyve will updated on FreeBSD 13.x and 14.x }
      if GetOsreldate.ToInt64 >= 1500023 then
      begin
        {$ifdef CPUAMD64}
        NewBhyveConfig.Values['bootrom'] := BootRomUefiPath+ '/' +'BHYVE_UEFI.fd';
        if FormVmCreate.CheckBoxUEFIBootvars.Checked then
        begin
          NewBhyveConfig.Values['bootvars'] := VmPath+'/'+FormVmCreate.EditVmName.Text+ '/' +'uefi-vars.fd';

          CreateFile(VmPath+'/'+FormVmCreate.EditVmName.Text+'/uefi-vars.fd', GetCurrentUserName());
          CopyFile(BootRomUefiPath+'/BHYVE_UEFI_VARS.fd', VmPath+'/'+FormVmCreate.EditVmName.Text+ '/' +'uefi-vars.fd');
        end;
        {$endif CPUAMD64}
        {$ifdef CPUAARCH64}
        NewBhyveConfig.Values['bootrom'] := BootRomUbootPath+ '/' +'u-boot.bin';
        {$endif CPUAARCH64}
      end
      else
      begin
        {$ifdef CPUAMD64}
        NewBhyveConfig.Values['lpc.bootrom']:=BootRomUefiPath+ '/' +'BHYVE_UEFI.fd';
        if FormVmCreate.CheckBoxUEFIBootvars.Checked then
        begin
          NewBhyveConfig.Values['lpc.bootvars'] := VmPath+'/'+FormVmCreate.EditVmName.Text+ '/' +'uefi-vars.fd';

          CreateFile(VmPath+'/'+FormVmCreate.EditVmName.Text+'/uefi-vars.fd', GetCurrentUserName());
          CopyFile(BootRomUefiPath+'/BHYVE_UEFI_VARS.fd', VmPath+'/'+FormVmCreate.EditVmName.Text+ '/' +'uefi-vars.fd');
        end;
        {$endif CPUAMD64}
      end;

      {$ifdef CPUAMD64}
      NewBhyveConfig.Values['lpc.fwcfg']:='bhyve';
      NewBhyveConfig.Values['lpc.com1.path']:='/dev/nmdm-'+FormVmCreate.EditVmName.Text+'.1A';
      {$endif CPUAMD64}
      {$ifdef CPUAARCH64}
      NewBhyveConfig.Values['console']:='/dev/nmdm-'+FormVmCreate.EditVmName.Text+'.1A';
      {$endif CPUAARCH64}

      NewBhyveConfig.Values['pci.0.0.0.device']:='hostbridge';

      {$ifdef CPUAMD64}
      NewBhyveConfig.Values['pci.0.31.0.device']:='lpc';
      {$endif}

      if not FormVmCreate.RadioButtonNotDisk.Checked then
      begin
        case FormVmCreate.ComboBoxVirtualDeviceType.Text of
          'ahci-hd':
            begin
              PciSlot:='2';
              NewBhyveConfig.Values['pci.0.'+PciSlot+'.0.device']:='ahci';
              NewBhyveConfig.Values['pci.0.'+PciSlot+'.0.port.0.type']:='hd';
              Path := '.port.0.path';
            end;
          'nvme':
            begin
              PciSlot:='3';
              NewBhyveConfig.Values['pci.0.'+PciSlot+'.0.device']:=FormVmCreate.ComboBoxVirtualDeviceType.Text;
            end;
          'virtio-blk':
            begin
              PciSlot:='5';
              NewBhyveConfig.Values['pci.0.'+PciSlot+'.0.device']:=FormVmCreate.ComboBoxVirtualDeviceType.Text;
            end;
          'virtio-scsi':
            begin
              PciSlot:='7';
              NewBhyveConfig.Values['pci.0.'+PciSlot+'.0.device']:=FormVmCreate.ComboBoxVirtualDeviceType.Text;
            end;
        end;

        case FormVmCreate.ComboBoxVirtualStorageType.Text of
          'image file':
            begin
              DiskName:=GetNewStorageName(FormVmCreate.EditVmFolderPath.Text+'/', False);
              CreateFile(FormVmCreate.EditVmFolderPath.Text+'/'+DiskName, GetCurrentUserName());

              if FormVmCreate.RadioButtonDiskFromImage.Checked then
              begin
                DiskFile:=FormVmCreate.EditVmFolderPath.Text+'/'+DiskName;

                TotalSize:=ConvertFileSize(GetRemoteSize(FormVmCreate.FileNameEditImageFile.FileName), 'M');

                MyAppThread := AppProgressBarThread.Create(CpCmd, [FormVmCreate.FileNameEditImageFile.FileName, FormVmCreate.EditVmFolderPath.Text+'/'+DiskName]);
                MyAppThread.OnShowStatus := @AppShowStatus;
                MyAppThread.OnEndStatus:= @AppEndStatus;
                MyAppThread.Start;
              end
              else
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

        if FormVmCreate.CheckBoxImageMinimal.Checked then
        begin
          CreateDirectory(FormVmCreate.EditVmFolderPath.Text+'/cloud-data', GetCurrentUserName());
          SeedImageConfig.LoadFromFile(DatadirPath+'templates/user-data');

          if FormVmCreate.CheckBoxImageUseSudo.Checked or FormVmCreate.CheckBoxImageUseDoas.Checked then
          begin
            SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%PACKAGES%%', 'packages:', [rfReplaceAll]);

            if FormVmCreate.CheckBoxImageUseSudo.Checked then
            begin
              SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%SUDOAS%%', ',sudo'+sLineBreak+'    sudo: ALL=(ALL) NOPASSWD:ALL', [rfReplaceAll]);
              SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%SUDOASPACKAGE%%', '  - sudo', [rfReplaceAll]);
            end;
            if FormVmCreate.CheckBoxImageUseDoas.Checked then
            begin
              SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%SUDOAS%%', sLineBreak+'    doas: permit nopass '+FormVmCreate.EditUsername.Text+' as root', [rfReplaceAll]);
              SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%SUDOASPACKAGE%%', '  - doas', [rfReplaceAll]);
            end;
          end
          else
          begin
            SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%PACKAGES%%', EmptyStr, [rfReplaceAll]);
            SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%SUDOAS%%', EmptyStr, [rfReplaceAll]);
            SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%SUDOASPACKAGE%%', EmptyStr, [rfReplaceAll]);
          end;

          if FormVmCreate.CheckBoxIpv6Address.Checked then
          begin
            SeedRunCmd := 'runcmd:'+sLineBreak;
            SeedRunCmd := SeedRunCmd + '  - |'+sLineBreak;
            SeedRunCmd := SeedRunCmd + '    if [ "$(uname -s)" = "Linux" ]; then'+sLineBreak;
            SeedRunCmd := SeedRunCmd + '      IFACE=$(nmcli -t -f DEVICE,TYPE device | grep '':ethernet'' | cut -d: -f1 | head -n1)'+sLineBreak;
            SeedRunCmd := SeedRunCmd + '      nmcli connection modify "$IFACE" ipv6.addr-gen-mode eui64'+sLineBreak;
            SeedRunCmd := SeedRunCmd + '      nmcli connection up "$IFACE"'+sLineBreak;
            SeedRunCmd := SeedRunCmd + '    fi';

            SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%RUNCMD%%', SeedRunCmd, [rfReplaceAll]);
          end
          else
            SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%RUNCMD%%', SeedRunCmd, [rfReplaceAll]);

          SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%HOSTNAME%%', FormVmCreate.EditVmName.Text, [rfReplaceAll]);
          SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%USERNAME%%', FormVmCreate.EditUsername.Text, [rfReplaceAll]);
          SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%SSH-KEY%%', FormVmCreate.EditSshPubKey.Text, [rfReplaceAll]);

          CreateFile(FormVmCreate.EditVmFolderPath.Text+'/cloud-data/user-data', GetCurrentUserName());
          SeedImageConfig.SaveToFile(FormVmCreate.EditVmFolderPath.Text+'/cloud-data/user-data');

          SeedImageConfig.LoadFromFile(DatadirPath+'templates/meta-data');
          SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%UUID%%', Uuid, [rfReplaceAll]);
          SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%HOSTNAME%%', FormVmCreate.EditVmName.Text, [rfReplaceAll]);

          CreateFile(FormVmCreate.EditVmFolderPath.Text+'/cloud-data/meta-data', GetCurrentUserName());
          SeedImageConfig.SaveToFile(FormVmCreate.EditVmFolderPath.Text+'/cloud-data/meta-data');

          if FormVmCreate.CheckBoxUseStaticIpv4.Checked then
          begin
            SeedImageConfig.LoadFromFile(DatadirPath+'templates/network-config');

            SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%MACADDRESS%%', MacAddress, [rfReplaceAll]);
            SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%IP4ADDRESS%%', FormVmCreate.EditIpv4Address.Text , [rfReplaceAll]);
            SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%GATEWAY4%%', FormVmCreate.EditGateway.Text , [rfReplaceAll]);
            SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%DNS4SERVERS%%', FormVmCreate.EditDNS.Text , [rfReplaceAll]);

            if FormVmCreate.CheckBoxUseStaticIpv6.Checked then
            begin
              SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%DHCP6%%', 'dhcp6: false', [rfReplaceAll]);
              SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%IP6ADDRESS%%', '- '+FormVmCreate.EditIpv6Address.Text+'/64' , [rfReplaceAll]);
              SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%GATEWAY6%%', 'gateway6: '+FormVmCreate.EditGatewayIpv6.Text , [rfReplaceAll]);
              SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%DNS6SERVERS%%', '- '+FormVmCreate.EditDnsIpv6.Text , [rfReplaceAll]);
            end
            else
            begin
              SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%DHCP6%%', EmptyStr, [rfReplaceAll]);
              SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%IP6ADDRESS%%', EmptyStr, [rfReplaceAll]);
              SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%GATEWAY6%%', EmptyStr, [rfReplaceAll]);
              SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%DNS6SERVERS%%', EmptyStr, [rfReplaceAll]);
            end;

            CreateFile(FormVmCreate.EditVmFolderPath.Text+'/cloud-data/network-config', GetCurrentUserName());
            SeedImageConfig.SaveToFile(FormVmCreate.EditVmFolderPath.Text+'/cloud-data/network-config');
          end;

          CreateFile(FormVmCreate.EditVmFolderPath.Text+'/seed.iso', GetCurrentUserName());
          CreateSeedIso(FormVmCreate.EditVmFolderPath.Text+'/cloud-data/', FormVmCreate.EditVmFolderPath.Text+'/seed.iso');

          NewBhyveConfig.Values['pci.0.1.0.device']:='ahci';
          NewBhyveConfig.Values['pci.0.1.0.port.0.type']:='cd';
          NewBhyveConfig.Values['pci.0.1.0.port.0.path']:=FormVmCreate.EditVmFolderPath.Text+'/seed.iso';
        end;

        if FormVmCreate.CheckBoxImageFiles.Checked then
        begin
          CreateDirectory(FormVmCreate.EditVmFolderPath.Text+'/cloud-data', GetCurrentUserName());

          if FileExists(FormVmCreate.FileNameEditMetaData.FileName) then
          begin
            SeedImageConfig.LoadFromFile(FormVmCreate.FileNameEditMetaData.FileName);
            SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%UUID%%', Uuid, [rfReplaceAll]);
            SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%HOSTNAME%%', FormVmCreate.EditVmName.Text, [rfReplaceAll]);

            CreateFile(FormVmCreate.EditVmFolderPath.Text+'/cloud-data/meta-data', GetCurrentUserName());
            SeedImageConfig.SaveToFile(FormVmCreate.EditVmFolderPath.Text+'/cloud-data/meta-data');
          end;

          if FileExists(FormVmCreate.FileNameEditUserData.FileName) then
          begin
            SeedImageConfig.LoadFromFile(FormVmCreate.FileNameEditUserData.FileName);
            SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%HOSTNAME%%', FormVmCreate.EditVmName.Text, [rfReplaceAll]);
            SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%MACADDRESS%%', MacAddress, [rfReplaceAll]);

            CreateFile(FormVmCreate.EditVmFolderPath.Text+'/cloud-data/user-data', GetCurrentUserName());
            SeedImageConfig.SaveToFile(FormVmCreate.EditVmFolderPath.Text+'/cloud-data/user-data');
          end;

          if FileExists(FormVmCreate.FileNameEditNetworkConfig.FileName) then
          begin
            SeedImageConfig.LoadFromFile(FormVmCreate.FileNameEditNetworkConfig.FileName);
            SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%MACADDRESS%%', MacAddress, [rfReplaceAll]);

            if FormVmCreate.CheckBoxUseStaticIpv4.Checked then
            begin
              SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%IP4ADDRESS%%', FormVmCreate.EditIpv4Address.Text , [rfReplaceAll]);
              SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%GATEWAY4%%', FormVmCreate.EditGateway.Text , [rfReplaceAll]);
              SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%DNS4SERVERS%%', FormVmCreate.EditDNS.Text , [rfReplaceAll]);
            end;

            if FormVmCreate.CheckBoxUseStaticIpv6.Checked then
            begin
              SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%IP6ADDRESS%%', FormVmCreate.EditIpv6Address.Text, [rfReplaceAll]);
              SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%GATEWAY6%%', FormVmCreate.EditGatewayIpv6.Text , [rfReplaceAll]);
              SeedImageConfig.Text:=StringReplace(SeedImageConfig.Text, '%%DNS6SERVERS%%', FormVmCreate.EditDnsIpv6.Text , [rfReplaceAll]);
            end;

            CreateFile(FormVmCreate.EditVmFolderPath.Text+'/cloud-data/network-config', GetCurrentUserName());
            SeedImageConfig.SaveToFile(FormVmCreate.EditVmFolderPath.Text+'/cloud-data/network-config');
          end;

          CreateFile(FormVmCreate.EditVmFolderPath.Text+'/seed.iso', GetCurrentUserName());
          CreateSeedIso(FormVmCreate.EditVmFolderPath.Text+'/cloud-data/', FormVmCreate.EditVmFolderPath.Text+'/seed.iso');

          NewBhyveConfig.Values['pci.0.1.0.device']:='ahci';
          NewBhyveConfig.Values['pci.0.1.0.port.0.type']:='cd';
          NewBhyveConfig.Values['pci.0.1.0.port.0.path']:=FormVmCreate.EditVmFolderPath.Text+'/seed.iso';
        end;
      end;

      if FormVmCreate.CheckBoxUseMedia.Checked and
         not FormVmCreate.CheckBoxImageMinimal.Checked and
         not FormVmCreate.CheckBoxImageFiles.Checked then
      begin
        NewBhyveConfig.Values['pci.0.1.0.device']:='ahci';
        NewBhyveConfig.Values['pci.0.1.0.port.0.type']:='cd';
        NewBhyveConfig.Values['pci.0.1.0.port.0.path']:=FormVmCreate.FileNameEditBootMedia.FileName;
      end;

      NewBhyveConfig.Values['pci.0.10.0.device']:='virtio-net';
      NewBhyveConfig.Values['pci.0.10.0.backend']:=GetNewNetworkName('tap');
      NewBhyveConfig.Values['pci.0.10.0.mac']:=MacAddress;

      {$ifdef CPUAMD64}
      if FormVmCreate.CheckBoxFramebuffer.Checked then
      begin
        NewBhyveConfig.Values['pci.0.29.0.device']:='fbuf';
        if FormVmCreate.CheckBoxOnlyLocalhost.Checked then
          NewBhyveConfig.Values['pci.0.29.0.tcp']:='127.0.0.1:'+GetNewVncPortNumber()
        else
          NewBhyveConfig.Values['pci.0.29.0.tcp']:='0.0.0.0:'+GetNewVncPortNumber();

        if FormVmCreate.CheckBoxWaitVNC.Checked then
          NewBhyveConfig.Values['pci.0.29.0.wait']:='true';
      end;

      NewBhyveConfig.Values['pci.0.30.0.device']:='xhci';
      NewBhyveConfig.Values['pci.0.30.0.slot.1.device']:='tablet';
      {$endif CPUAMD64}

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

      if FormVmCreate.CheckBoxUseStaticIpv4.Checked then
        IpAddress:=FormVmCreate.EditIpv4Address.Text;

      if (UseIpv6 = 'yes') and FormVmCreate.CheckBoxIpv6Address.Checked and
         (FormVmCreate.CheckBoxUseStaticIpv6.Checked) then
        Ip6Address:=FormVmCreate.EditIpv6Address.Text;

      if UseDnsmasq = 'yes' then
      begin
        if FormVmCreate.CheckBoxUseStaticIpv4.Checked then
          AddDnsmasqHostRecordEntry(FormVmCreate.EditVmName.Text, IpAddress, MacAddress)
        else
        begin
          IpAddress:=GetNewIpAddress(GetSubnet);
          AddDnsmasqDhcpHostEntry(FormVmCreate.EditVmName.Text, IpAddress, MacAddress);
        end;

        if (UseIpv6 = 'yes') and (FormVmCreate.CheckBoxIpv6Address.Checked) then
        begin
          Ip6Address:=GetNewIp6Address(GetIpv6Prefix, MacAddress);
          AddDnsmasqHostRecordEntry(FormVmCreate.EditVmName.Text, Ip6Address, MacAddress);
        end;

        RestartService('dnsmasq');
      end;

      if not (IpAddress = EmptyStr) then
        NewVMConfig.SetOption('general','ipaddress', IpAddress );

      if not (Ip6Address = EmptyStr) then
      begin
        NewVMConfig.SetOption('general','ip6address', Ip6Address );
        NewVMConfig.SetOption('general','ipv6', 'True');
      end;

      FormVmCreate.Hide;

      if not FormVmCreate.RadioButtonDiskFromImage.Checked then
      begin
        FillVirtualMachine(FormVmCreate.EditVmName.Text);
        VirtualMachinesTreeView.AlphaSort;

        StatusBarBhyveManager.Font.Color:=clTeal;
        StatusBarBhyveManager.SimpleText:=Format(vm_create_status, [FormVmCreate.EditVmName.Text]);

        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : '+Format(vm_create_status, [FormVmCreate.EditVmName.Text]));
      end;
    finally
      if Assigned(NewVMConfig) then
        NewVMConfig. Free;
      NewBhyveConfig.Free;
      SeedImageConfig.Free;
    end;
  end
end;

{
  This procedure is called when "Modify virtual machine info" option is
  selected from "Virtual machines" treeview. An "Edit virtual machine info"
  form is opened to do some changes.
}
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
    FormVmInfo.ComboBoxVmVersion.Clear;
    FillComboSystemVersion(FormVmInfo.ComboBoxVmVersion, FormVmInfo.ComboBoxVmType.Text);
    FormVmInfo.ComboBoxVmVersion.ItemIndex:=FormVmInfo.ComboBoxVmVersion.Items.IndexOf(VirtualMachine.system_version);
    FormVmInfo.EditVmName.Text:=VirtualMachine.name;
    FormVmInfo.EditVmDescription.Text:=VirtualMachine.description;
    FormVmInfo.EditVmIpv4Address.Text:=VirtualMachine.ipaddress;
    FormVmInfo.EditVmIpv6Address.Text:=VirtualMachine.ip6address;
    FormVmInfo.Ip4Address:=VirtualMachine.ipaddress;

    if Assigned(DeviceSettingsTreeView.Items.FindTopLvlNode('Network').Items[0].Data) then
      FormVmInfo.MacAddress:=TNetworkDeviceClass(DeviceSettingsTreeView.Items.FindTopLvlNode('Network').Items[0].Data).mac
    else
      FormVmInfo.MacAddress:=EmptyStr;

    if VirtualMachine.rdp = StrToBool('True') then
      FormVmInfo.CheckBoxRDP.Checked:=True
    else
      FormVmInfo.CheckBoxRDP.Checked:=False;

    if UseIpv6 = 'yes' then
    begin
      FormVmInfo.CheckBoxIpv6.Enabled:=True;

      if VirtualMachine.ipv6 = StrToBool('True') then
        FormVmInfo.CheckBoxIpv6.Checked:=True
      else
        FormVmInfo.CheckBoxIpv6.Checked:=False;
    end
    else
    begin
      FormVmInfo.CheckBoxIpv6.Enabled:=False;
      FormVmInfo.CheckBoxIpv6.Checked:=False;
    end;

    if (UsePf = 'yes') then
    begin
      FormVmInfo.CheckBoxNat.Enabled:=True;
      FormVmInfo.CheckBoxPf.Enabled:=True;

      if VirtualMachine.nat = StrToBool('True') then
        FormVmInfo.CheckBoxNat.Checked:=True
      else
        FormVmInfo.CheckBoxNat.Checked:=False;

      if VirtualMachine.pf = StrToBool('True') then
        FormVmInfo.CheckBoxPf.Checked:=True
      else
        FormVmInfo.CheckBoxPf.Checked:=False;
    end
    else
    begin
      FormVmInfo.CheckBoxNat.Enabled:=False;
      FormVmInfo.CheckBoxPf.Enabled:=False;
    end;
  end;
end;

{
  This procedure is called when "PF Rules" option is
  selected from "Virtual machines" treeview. An "Packet Filter rules"
  form is opened to do some changes.
}
procedure TFormBhyveManager.PacketFilterRulesVm(Sender: TObject);
begin
  if (Assigned(VirtualMachinesTreeView.Selected)) and (VirtualMachinesTreeView.Selected.Level = 1) then
  begin
    GlobalNode:=VirtualMachinesTreeView.Selected;
    VirtualMachine := TVirtualMachineClass(GlobalNode.Data);

    FormPacketFilterRules.Visible:=True;
    FormPacketFilterRules.VmIp4Adress:=VirtualMachine.ipaddress;
    FormPacketFilterRules.VmIp6Adress:=VirtualMachine.ip6address;
    FormPacketFilterRules.VmName:=VirtualMachine.name;
    FormPacketFilterRules.LoadDefaultValues();
  end;
end;

{
  This procedure is used to save virtual machine info when it is modified.
}
procedure TFormBhyveManager.SaveVirtualMachineInfoClick(Sender: TObject);
var
  Configuration : ConfigurationClass;
  NodoName : String;
begin
  NodoName:=EmptyStr;

  if FormVmInfo.FormValidate() then
  begin
    Configuration:= ConfigurationClass.Create(VmPath+ '/'+FormVmInfo.EditVmName.Text+'/'+FormVmInfo.EditVmName.Text+'.conf');

    Configuration.SetOption('general','type' , FormVmInfo.ComboBoxVmType.Text);
    Configuration.SetOption('general','version', FormVmInfo.ComboBoxVmVersion.Text);
    Configuration.SetOption('general','image', PtrInt(FormVmInfo.ComboBoxVmVersion.Items.Objects[FormVmInfo.ComboBoxVmVersion.ItemIndex]).ToString);
    Configuration.SetOption('general','description', FormVmInfo.EditVmDescription.Text);
    Configuration.SetOption('general','rdp', BoolToStr(FormVmInfo.CheckBoxRDP.Checked, 'True', 'False'));
    Configuration.SetOption('general','ipv6', BoolToStr(FormVmInfo.CheckBoxIpv6.Checked, 'True', 'False'));
    Configuration.SetOption('general','ipaddress', FormVmInfo.EditVmIpv4Address.Text);

    if not FormVmInfo.CheckBoxIpv6.Checked then
      Configuration.SetOption('general','ip6address', EmptyStr);

    if UseDnsmasq = 'yes' then
    begin
      if Assigned(DeviceSettingsTreeView.Items.FindTopLvlNode('Network').Items[0].Data) and CheckIpvAddress(FormVmInfo.EditVmIpv4Address.Text)  then
        AddDnsmasqDhcpHostEntry(FormVmInfo.EditVmName.Text, FormVmInfo.EditVmIpv4Address.Text, TNetworkDeviceClass(DeviceSettingsTreeView.Items.FindTopLvlNode('Network').Items[0].Data).mac);

      if Assigned(DeviceSettingsTreeView.Items.FindTopLvlNode('Network').Items[0].Data) and CheckIpv6Address(FormVmInfo.EditVmIpv6Address.Text) then
        AddDnsmasqHostRecordEntry(FormVmInfo.EditVmName.Text, FormVmInfo.EditVmIpv6Address.Text, TNetworkDeviceClass(DeviceSettingsTreeView.Items.FindTopLvlNode('Network').Items[0].Data).mac);
    end;

    if FormVmInfo.CheckBoxNat.Checked then
    begin
      Configuration.SetOption('general','nat', 'True');

      if PfCreateRules(FormVmInfo.EditVmName.Text, 'nat on '+ExternalInterface+' from '+FormVmInfo.EditVmIpv4Address.Text +' to any -> '+ExternalIpv4, 'nat') then
      begin
        if CheckVmRunning(FormVmInfo.EditVmName.Text) > 0 then
        begin
          if MessageDialog(mtConfirmation, Format(vm_apply_nat_confirmation, [FormVmInfo.EditVmName.Text])) = mrYes then
            PfLoadRules(FormVmInfo.EditVmName.Text, 'nat');
        end;
      end;
    end
    else
    begin
      Configuration.SetOption('general','nat', 'False');
    end;

    if FormVmInfo.CheckBoxPf.Checked then
    begin
      Configuration.SetOption('general','pf', 'True');

      if (CheckVmRunning(FormVmInfo.EditVmName.Text) > 0) and
         ((FileExists(VmPath+'/'+FormVmInfo.EditVmName.Text+'/pf/rdr.rules')) or
         (FileExists(VmPath+'/'+FormVmInfo.EditVmName.Text+'/pf/pass-in.rules')) or
         (FileExists(VmPath+'/'+FormVmInfo.EditVmName.Text+'/pf/pass-out.rules'))) then
      begin
        if MessageDialog(mtConfirmation, Format(vm_apply_rules_confirmation, [FormVmInfo.EditVmName.Text])) = mrYes then
        begin
          PfLoadRules(FormVmInfo.EditVmName.Text, 'rdr');
          PfLoadRules(FormVmInfo.EditVmName.Text, 'pass-in');
          PfLoadRules(FormVmInfo.EditVmName.Text, 'pass-out');
        end;
      end;
    end
    else
      Configuration.SetOption('general','pf', 'False');

    Configuration.Free;

    if Assigned(VirtualMachinesTreeView.Items.FindNodeWithText(FormVmInfo.EditVmName.Text)) then
      NodoName:=FormVmInfo.EditVmName.Text
    else if Assigned(VirtualMachinesTreeView.Items.FindNodeWithText(FormVmInfo.EditVmName.Text+' : Running')) then
      NodoName:=FormVmInfo.EditVmName.Text+' : Running';

    if Assigned(VirtualMachinesTreeView.Items.FindNodeWithText(NodoName)) and (FormVmInfo.ComboBoxVmType.Text = TVirtualMachineClass(VirtualMachinesTreeView.Items.FindNodeWithText(NodoName).Data).system_type)  then
    begin
      TVirtualMachineClass(VirtualMachinesTreeView.Items.FindNodeWithText(NodoName).Data).system_version:=FormVmInfo.ComboBoxVmVersion.Text;
      TVirtualMachineClass(VirtualMachinesTreeView.Items.FindNodeWithText(NodoName).Data).system_type:=FormVmInfo.ComboBoxVmType.Text;
      TVirtualMachineClass(VirtualMachinesTreeView.Items.FindNodeWithText(NodoName).Data).description:=FormVmInfo.EditVmDescription.Text;
      TVirtualMachineClass(VirtualMachinesTreeView.Items.FindNodeWithText(NodoName).Data).image:=PtrInt(FormVmInfo.ComboBoxVmVersion.Items.Objects[FormVmInfo.ComboBoxVmVersion.ItemIndex]);
      TVirtualMachineClass(VirtualMachinesTreeView.Items.FindNodeWithText(NodoName).Data).rdp:=FormVmInfo.CheckBoxRDP.Checked;
      TVirtualMachineClass(VirtualMachinesTreeView.Items.FindNodeWithText(NodoName).Data).ipv6:=FormVmInfo.CheckBoxIpv6.Checked;
      TVirtualMachineClass(VirtualMachinesTreeView.Items.FindNodeWithText(NodoName).Data).ipaddress:=FormVmInfo.EditVmIpv4Address.Text;
      TVirtualMachineClass(VirtualMachinesTreeView.Items.FindNodeWithText(NodoName).Data).ip6address:=FormVmInfo.EditVmIpv6Address.Text;
      TVirtualMachineClass(VirtualMachinesTreeView.Items.FindNodeWithText(NodoName).Data).nat:=FormVmInfo.CheckBoxNat.Checked;
      TVirtualMachineClass(VirtualMachinesTreeView.Items.FindNodeWithText(NodoName).Data).pf:=FormVmInfo.CheckBoxPf.Checked;

      VirtualMachinesTreeView.Items.FindNodeWithText(NodoName).ImageIndex:=PtrInt(FormVmInfo.ComboBoxVmVersion.Items.Objects[FormVmInfo.ComboBoxVmVersion.ItemIndex]);
      VirtualMachinesTreeView.Items.FindNodeWithText(NodoName).SelectedIndex:=PtrInt(FormVmInfo.ComboBoxVmVersion.Items.Objects[FormVmInfo.ComboBoxVmVersion.ItemIndex]);
    end
    else
    begin
      ResetTreeView(VirtualMachinesTreeView);
      VirtualMachinesTreeView.Items.Clear;
      FillVirtualMachineList();
    end;

    FormVmInfo.Hide;
  end;
end;

{
  This procedure is used to call ResetTreeView and LoadDeviceSettingsValues
  procedures. They will clean and load DeviceSettingsTreeView from
  bhyve_config.conf file data.
}
procedure TFormBhyveManager.SpeedButtonReloadVmConfigClick(Sender: TObject);
begin
  ResetTreeView(DeviceSettingsTreeView);
  LoadDeviceSettingsValues(VirtualMachine.name);
end;

{
  This procedure is used to delete virtual machine from Virtual Machines
  treeview and remove all data generated by a virtual machine: directories,
  configuration files, disk images, sockets, etc.
}
procedure TFormBhyveManager.SpeedButtonRemoveVmClick(Sender: TObject);
var
  Status : Boolean;
  VmName : String;
  ParentNode : String;
begin
  Status:=False;

  if (Assigned(VirtualMachinesTreeView.Selected)) and (VirtualMachinesTreeView.Selected.Level = 1) and not (CheckVmRunning(VirtualMachine.name) > 0) then
  begin
    GlobalNode:=VirtualMachinesTreeView.Selected;
    VirtualMachine := TVirtualMachineClass(GlobalNode.Data);

    VmName:=VirtualMachine.name;

    if (MessageDialog(mtConfirmation, Format(vm_remove_notice, [VmName])) = mrYes) then
    begin
      if UseZfs = 'yes' then
      begin
        if ZfsDestroy(VmPath.Remove(0,1)+'/'+VirtualMachine.name) then
          Status:=True
        else
        begin
          if (MessageDialog(mtWarning, Format(vm_remove_force, [VmName])) = mrYes) then
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

      ParentNode:=VirtualMachinesTreeView.Selected.Parent.Text;

      TObject(VirtualMachinesTreeView.Selected.Data).Free;
      VirtualMachinesTreeView.Selected.Data:=Nil;
      VirtualMachinesTreeView.Selected.Delete;

      if VirtualMachinesTreeView.Items.FindTopLvlNode(ParentNode).Count = 0 then
      begin
        VirtualMachinesTreeView.Items.FindTopLvlNode(ParentNode).Data:=Nil;
        VirtualMachinesTreeView.Items.FindTopLvlNode(ParentNode).Delete;
      end;

      StatusBarBhyveManager.Font.Color:=clTeal;
      StatusBarBhyveManager.SimpleText:=Format(vm_remove_status, [VmName]);

      MessageDialog(mtInformation, Format(vm_remove_status, [VmName]));

      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : '+Format(vm_remove_status, [VmName]));
    end
    else
    begin
      StatusBarBhyveManager.Font.Color:=clRed;
      StatusBarBhyveManager.SimpleText:=Format(vm_notremove_status, [VmName]);
    end;
  end;
end;

{
  This procedure is used to run a virtual machine thread. Also this modify
  configuration files, create network devices, etc required by the virtual
  machine.
}
procedure TFormBhyveManager.SpeedButtonStartVmClick(Sender: TObject);
var
  i : Integer;
  Node : TTreeNode;
  IpAddress : String;
  Ip6Address : String;
  VmConfig : ConfigurationClass;
begin
  GlobalNode:=VirtualMachinesTreeView.Selected;

  VirtualMachine:=TVirtualMachineClass(GlobalNode.Data);

  if DeviceSettingsTreeView.Items.TopLvlItems[1].Count > 0 then
  begin
    CreateDirectory(VmPath+'/'+VirtualMachine.name+'/vtcon', GetCurrentUserName());
  end;

  { Remove this condition when bhyve will be updated on FreeBSD 13.x and 14.x }
  if GetOsreldate.ToInt64 >= 1403000 then
  begin
    if (CheckTpmSocketRunning(VirtualMachine.name) = -1) and (Assigned(GlobalSettingsTreeView.Items.FindTopLvlNode('TPM')))
       and (ExtractVarValue(GlobalSettingsTreeView.Items.FindTopLvlNode('TPM').Items[1].Text) = 'swtpm') then
    begin
      if not (DirectoryExists(ExtractFilePath(ExtractVarValue(GlobalSettingsTreeView.Items.FindTopLvlNode('TPM').Items[0].Text)))) then
        CreateDirectory(ExtractFilePath(ExtractVarValue(GlobalSettingsTreeView.Items.FindTopLvlNode('TPM').Items[0].Text)), GetCurrentUserName(), '750');

      CreateTpmSocket(ExtractFilePath(ExtractVarValue(GlobalSettingsTreeView.Items.FindTopLvlNode('TPM').Items[0].Text)));
    end;
  end;

  MyVmThread := VmThread.Create(VirtualMachine.name);
  MyVmThread.OnExitStatus := @VirtualMachineShowStatus;
  MyVmThread.Start;

  Sleep(100);

  DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : '+Format(vm_start_status, [VirtualMachine.name]));

  if CheckVmRunning(VirtualMachine.name) > 0 then
  begin
    if (Assigned(DeviceSettingsTreeView.Items.FindTopLvlNode('Display')))
       and (DeviceSettingsTreeView.Items.FindTopLvlNode('Display').Count = 1) then
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

          if VirtualMachine.nat then
          begin
            PfLoadRules(VirtualMachine.name, 'nat');
          end;

          if VirtualMachine.pf then
          begin
            PfLoadRules(VirtualMachine.name, 'rdr');
            PfLoadRules(VirtualMachine.name, 'pass-in');
            PfLoadRules(VirtualMachine.name, 'pass-out');
          end;

          if UseDnsmasq = 'yes' then
          begin
            VmConfig:=ConfigurationClass.Create(VmPath+'/'+VirtualMachine.name+'/'+VirtualMachine.name+'.conf');

            IpAddress:=VmConfig.GetOption('general', 'ipaddress', '');
            Ip6Address:=VmConfig.GetOption('general', 'ip6address', '');

            if IpAddress = EmptyStr then
            begin
              IpAddress:=GetNewIpAddress(GetSubnet);
              VmConfig.SetOption('general','ipaddress', IpAddress );
              AddDnsmasqDhcpHostEntry(VirtualMachine.name, IpAddress, NetworkDevice.mac);
              RestartService('dnsmasq');
            end;

            if (UseIpv6 = 'yes') and (Virtualmachine.ipv6 = True) and
               ((Ip6Address = EmptyStr) or not (Ip6Address = GetNewIp6Address(GetIpv6Prefix, NetworkDevice.mac))) then
            begin
              Ip6Address:=GetNewIp6Address(GetIpv6Prefix, NetworkDevice.mac );
              VmConfig.SetOption('general','ip6address', Ip6Address );
              AddDnsmasqHostRecordEntry(VirtualMachine.name, Ip6Address, NetworkDevice.mac);
              RestartService('dnsmasq');
            end;

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
    StatusBarBhyveManager.SimpleText := Format(vm_start_status, [VirtualMachine.name]);
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

{
  This procedure is used to stop a virtual machine. It sends a -SIGTERM to
  virtual machine main process.
}
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

{
  This procedure is used to open a vnc connection if the virtual machine
  supports it.
}
procedure TFormBhyveManager.SpeedButtonVncVmClick(Sender: TObject);
var
  DisplayNode : TTreeNode;
begin
  if (Assigned(DeviceSettingsTreeView.Items.FindTopLvlNode('Display')))
     and (DeviceSettingsTreeView.Items.FindTopLvlNode('Display').Count = 1) then
  begin
    DisplayNode:=DeviceSettingsTreeView.Items.FindTopLvlNode('Display').Items[0];
    DisplayDevice:=TDisplayDeviceClass(DisplayNode.Data);
    VncConnect(DisplayDevice.tcp, TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).name);
  end;
end;

{
  This procedure is used to load Global settings treeview, Device Settings
  treeview, and information of a virtual machine selected when a double click
  event is generated.
}
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

{
  This procedure is used to free all nodes from DeviceSettings TreeView to avoid
  memory leaks.
}
procedure TFormBhyveManager.DeviceSettingsTreeViewDeletion(Sender: TObject;
  Node: TTreeNode);
begin
  if assigned(TObject(Node.Data)) then
  begin
    TObject(Node.Data).Free;
  end;
end;

{
  This procedure is used to free all nodes from VirtualMachines TreeView to
  avoid memory leaks.
}
procedure TFormBhyveManager.VirtualMachinesTreeViewDeletion(Sender: TObject;
  Node: TTreeNode);
begin
  if assigned(TObject(Node.Data)) then
  begin
    TObject(Node.Data).Free;
  end;
end;

{
  This procedure is used to display a menu when a mouse right button is pressed
  over a virtual machine name or a category.
}
procedure TFormBhyveManager.VirtualMachinesTreeViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbRight) and (Assigned(VirtualMachinesTreeView.Selected))then
   begin
     if (VirtualMachinesTreeView.Selected.Level = 0) then
      begin
        // Add virtual machine
        VirtualMachinesPopup.PopupMenu.Items.Items[0].Enabled:=True;
        // Modify virtual machine info
        VirtualMachinesPopup.PopupMenu.Items.Items[1].Enabled:=False;
        // Remove virtual machine
        VirtualMachinesPopup.PopupMenu.Items.Items[2].Enabled:=False;
        // RDP to virtual machine
        VirtualMachinesPopup.PopupMenu.Items.Items[3].Enabled:=False;
        // PF Rules to virtual machine
        VirtualMachinesPopup.PopupMenu.Items.Items[4].Enabled:=False;
        // Copy virtual machine name
        VirtualMachinesPopup.PopupMenu.Items.Items[6].Enabled:=False;
        // Copy cu/netcat command to virtual machine
        VirtualMachinesPopup.PopupMenu.Items.Items[7].Enabled:=False;
        // Copy IPv4 address
        VirtualMachinesPopup.PopupMenu.Items.Items[8].Enabled:=False;
        // Copy IPv6 address
        VirtualMachinesPopup.PopupMenu.Items.Items[9].Enabled:=False;

        VirtualMachinesPopup.PopupMenu.PopUp;
      end
      else if (VirtualMachinesTreeView.Selected.Level = 1) then
      begin
        VirtualMachinesPopup.PopupMenu.Items.Items[0].Enabled:=False;
        VirtualMachinesPopup.PopupMenu.Items.Items[1].Enabled:=True;
        VirtualMachinesPopup.PopupMenu.Items.Items[2].Enabled:=True;
        VirtualMachinesPopup.PopupMenu.Items.Items[6].Enabled:=True;
        VirtualMachinesPopup.PopupMenu.Items.Items[7].Enabled:=True;

        if (ExtractVarValue(VirtualMachinesTreeView.Selected.Text) = 'Running') and
           (TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).rdp = True) and
           FileExists(XfreerdpCmd) then
          VirtualMachinesPopup.PopupMenu.Items.Items[3].Enabled:=True
        else
          VirtualMachinesPopup.PopupMenu.Items.Items[3].Enabled:=False;

        if TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).pf then
          VirtualMachinesPopup.PopupMenu.Items.Items[4].Enabled:=True
        else
          VirtualMachinesPopup.PopupMenu.Items.Items[4].Enabled:=False;

        if TVirtualMachineClass(VirtualMachinesTreeView.Selected.Data).ipv6 then
          VirtualMachinesPopup.PopupMenu.Items.Items[9].Enabled:=True
        else
          VirtualMachinesPopup.PopupMenu.Items.Items[9].Enabled:=False;

        VirtualMachinesPopup.PopupMenu.PopUp;
      end;
   end;
end;

{
  This procedure is used to load Global settings treeview, Device Settings
  treeview, and information of a virtual machine selected when a click
  event is generated.
}
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
    StatusBarBhyveManager.SimpleText:=virtual_machine+' : '+VirtualMachine.name;

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

      if (Assigned(DeviceSettingsTreeView.Items.FindTopLvlNode('Display')))
            and (DeviceSettingsTreeView.Items.FindTopLvlNode('Display').Count = 1) then
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

{
  This procedure is used to extract audio device data from configuration data
  and put them to AudioDevice class.
}
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

{
  This procedure is used to extract console device data from configuration data
  and put them to SerialVirtioConsoleDevice class.
}
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

{
  This procedure is used to extract display device data from configuration data
  and put them to DisplayDevice class.
}
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
              DisplayDevice.port := StrToInt(ExtractPortValue(RegexObj.Match[2]));
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

{
  This procedure is used to extract hostbridge device data from configuration data
  and put them to HostbridgeDevice class.
}
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

{
  This procedure is used to extract input device data from configuration data
  and put them to InputDevice class.
}
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

{
  This procedure is used to extract LPC device data from configuration data
  and put them to LPCDevice class.
}
function TFormBhyveManager.FillDetailLpcDevice(Details: String; pci: String;
  device: String): TLPCDeviceClass;
var
  RegexObj: TRegExpr;
begin
  LPCDevice := TLPCDeviceClass.Create;

  RegexObj := TRegExpr.Create;
  RegexObj.Expression := 'lpc\.(\S+?)=(\S+)';
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

{
  This procedure is used to extract network device data from configuration data
  and put them to NetworkDevice class.
}
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

{
  This procedure is used to extract passthru device data from configuration data
  and put them to PassthruDevice class.
}
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

{
  This procedure is used to extract rng device data from configuration data
  and put them to RNGDevice class.
}
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

{
  This procedure is used to extract share folder device data from configuration
  data and put them to ShareFolderDevice class.
}
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

{
  This procedure is used to extract ahci storage device data from configuration
  data and put them to StorageAhciDevice class.
}
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

{
  This procedure is used to extract nvme storage device data from configuration
  data and put them to StorageNvmeDevice class.
}
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

{
  This procedure is used to extract virtio-blk storage device data from
  configuration data and put them to StorageVirtioBlkDevice class.
}
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

{
  This procedure is used to extract usb xhci device data from configuration
  data and put them to UsbXhciDevice class.
}
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

{
  Load default global settings from Virtual Machine bhyve_config.conf file
}
function TFormBhyveManager.LoadGlobalSettingsValues(VmName: String): Boolean;
var
  i: Integer;
  TmpGlobalSettingsList : TStringList;
  ConfigSettingsFile: TStringList;
begin
  TmpGlobalSettingsList:=TStringList.Create;
  TmpGlobalSettingsList.Text := GlobalSettingDefaultValueList.Text;

  ConfigSettingsFile:=TStringList.Create;
  ConfigSettingsFile.LoadFromFile(VmPath + '/' + VmName + '/bhyve_config.conf');

  for i:=ConfigSettingsFile.Count-1 downto 0 do
  begin
      if (ConfigSettingsFile[i].Contains('pci.')) or (ConfigSettingsFile[i].Contains('lpc.'))  then
        ConfigSettingsFile.Delete(i)
      else
        TmpGlobalSettingsList.Values[ConfigSettingsFile.Names[i]]:= ConfigSettingsFile.ValueFromIndex[i];
  end;

  ConfigSettingsFile.Free;

  for i:=0 to GlobalSettingCategoryList.Count-1 do
  begin
    GlobalSettingsTreeView.Items.AddChild(GlobalSettingsTreeView.Items.FindNodeWithText(GlobalSettingCategoryList.ValueFromIndex[i]), GlobalSettingCategoryList.Names[i]+' : '+TmpGlobalSettingsList.Values[GlobalSettingCategoryList.Names[i]]);
  end;

  TmpGlobalSettingsList.Free;
  Result:=True;
end;

{
  Load default device settings from Virtual Machine bhyve_config.conf file
}
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

{
  Extract all network device settings from a Virtual Machine bhyve_config.conf
  file.
}
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

{
  Load Virtual Machine data from vmname.conf file.
}
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
  VirtualMachine.ipaddress:=Configuration.GetOption('general','ipaddress');

  if Configuration.GetOption('general','rdp') = 'True' then
    VirtualMachine.rdp:=True
  else
    VirtualMachine.rdp:=False;

  if (UsePf = 'yes') and (Configuration.GetOption('general','nat') = 'True') then
    VirtualMachine.nat:=True
  else
    VirtualMachine.nat:=False;

  if (UsePf = 'yes') and (Configuration.GetOption('general','pf') = 'True') then
    VirtualMachine.pf:=True
  else
    VirtualMachine.pf:=False;

  if (UseIpv6 = 'yes') and (Configuration.GetOption('general','ipv6') = 'True') then
  begin
    VirtualMachine.ip6address:=Configuration.GetOption('general','ip6address');
    VirtualMachine.ipv6:=True;
  end
  else
  begin
    VirtualMachine.ipv6:=False;
    VirtualMachine.ip6address:=EmptyStr;
  end;

  Configuration.Free;

  Result:=VirtualMachine;
end;

end.

