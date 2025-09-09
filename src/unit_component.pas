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

unit unit_component;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Menus, Dialogs, Controls, StdCtrls, ExtCtrls, FileUtil, LCLTranslator;

type

  { TPopupMenuDevices }

  TPopupMenuDevices = Class(TObject)
    PopupMenu : TPopupMenu;
    PopupMenuItem : TMenuItem;
    public
      constructor Create(component : TComponent); overload;
    private
  end;

  { TDevicesPopupMenu }

  TDevicesPopupMenu = Class(TObject)
    PopupMenu : TPopupMenu;
    PopupMenuItem : TMenuItem;
    public
      constructor Create(component : TComponent); overload;
    private
  end;

  { TGlobalSettingsPopupMenu }

  TGlobalSettingsPopupMenu = Class(TObject)
    PopupMenu : TPopupMenu;
    PopupMenuItem : TMenuItem;
    public
      constructor Create(component : TComponent); overload;
    private
  end;

  { TVirtualMachinesPopupMenu }

  TVirtualMachinesPopupMenu = Class(TObject)
    PopupMenu : TPopupMenu;
    PopupMenuItem : TMenuItem;
    public
      constructor Create(component : TComponent); overload;
    private
  end;

  { TDeviceImageList }

  TDeviceImageList = Class(TObject)
    DeviceList : TImageList;
    public
      constructor Create(component : TComponent); overload;
    private
    end;

  { TActionImageList }

  TActionImageList = Class(TObject)
    ActionList : TImageList;
    public
      constructor Create(component : TComponent); overload;
    private
    end;

  { TSystemImageList }

  TSystemImageList = Class(TObject)
    SystemList : TImageList;
    public
      constructor Create(component : TComponent); overload;
    private
    end;

  { TTrayIconPopupMenu }

  TTrayIconPopupMenu = Class(TObject)
    PopupMenu : TPopupMenu;
    PopupMenuItem : TMenuItem;
    public
      constructor Create(component : TComponent); overload;
    private
  end;

  { TSystemTrayIcon }

  TSystemTrayIcon = Class(TObject)
    TrayIcon : TTrayIcon;
    public
      constructor Create(component : TComponent); overload;
    private
    end;

  function FillComboIntegerType(Combo: TComboBox; StartNumber : Integer; EndNumber : Integer; Increment : Integer):Boolean;
  function FillComboBooleanType(Combo: TComboBox):Boolean;
  function FillComboKeyboardLayout(Combo: TComboBox):Boolean;
  function FillComboBootrom(Combo: TComboBox):Boolean;
  function FillComboBootvars(Combo: TComboBox):Boolean;
  procedure FillComboLanguage(Combo: TComboBox);
  procedure FillComboTpmDevice(Combo: TComboBox);
  procedure FillComboTpmType(Combo: TComboBox);
  procedure FillComboTpmVersion(Combo: TComboBox);
  procedure FillComboResolution(Combo: TComboBox);
  function FillComboSystemType(Combo: TComboBox):Boolean;
  function FillComboSystemVersion(Combo: TComboBox; System: String ):Boolean;
  function FillComboVirtualDeviceType(Combo: TComboBox):Boolean;
  function FillComboVirtualStorageType(Combo: TComboBox):Boolean;
  function FillComboZpoolList(Combo: TComboBox):Boolean;

implementation

uses
  unit_global, unit_util, unit_language;

constructor TPopupMenuDevices.Create(component : TComponent);
begin
  PopupMenu:=TPopupMenu.Create(component);
  PopupMenu.AutoPopup:=False;

  PopupMenuItem:=TMenuItem.Create(PopupMenu);
  PopupMenuItem.Caption:=popup_add_device;

  PopupMenu.Items.Add(PopupMenuItem);

  PopupMenuItem:=TMenuItem.Create(PopupMenu);
  PopupMenuItem.Caption:=popup_edit_device;

  PopupMenu.Items.Add(PopupMenuItem);

  PopupMenuItem:=TMenuItem.Create(PopupMenu);
  PopupMenuItem.Caption:=popup_delete_device;

  PopupMenu.Items.Add(PopupMenuItem);
end;

{ TDevicesPopupMenu }

constructor TDevicesPopupMenu.Create(component: TComponent);
begin
  PopupMenu:=TPopupMenu.Create(component);
  PopupMenu.AutoPopup:=False;

  PopupMenuItem:=TMenuItem.Create(PopupMenu);
  PopupMenuItem.Caption:=popup_add_device;
  PopupMenu.Items.Add(PopupMenuItem);

  PopupMenuItem:=TMenuItem.Create(PopupMenu);
  PopupMenuItem.Caption:=popup_edit_device;
  PopupMenu.Items.Add(PopupMenuItem);

  PopupMenuItem:=TMenuItem.Create(PopupMenu);
  PopupMenuItem.Caption:=popup_delete_device;
  PopupMenu.Items.Add(PopupMenuItem);
end;

{ TGlobalSettingsPopupMenu }

constructor TGlobalSettingsPopupMenu.Create(component: TComponent);
begin
  PopupMenu:=TPopupMenu.Create(component);
  PopupMenu.AutoPopup:=False;

  PopupMenuItem:=TMenuItem.Create(PopupMenu);
  PopupMenuItem.Caption:=popup_edit_global_setting;
  PopupMenu.Items.Add(PopupMenuItem);
end;

{ TVirtualMachinesPopupMenu }

constructor TVirtualMachinesPopupMenu.Create(component: TComponent);
begin
  PopupMenu:=TPopupMenu.Create(component);
  PopupMenu.AutoPopup:=False;

  PopupMenuItem:=TMenuItem.Create(PopupMenu);
  PopupMenuItem.Caption:=popup_add_vm;
  PopupMenu.Items.Add(PopupMenuItem);

  PopupMenuItem:=TMenuItem.Create(PopupMenu);
  PopupMenuItem.Caption:=popup_modify_vm;
  PopupMenu.Items.Add(PopupMenuItem);

  PopupMenuItem:=TMenuItem.Create(PopupMenu);
  PopupMenuItem.Caption:=popup_remove_vm;
  PopupMenu.Items.Add(PopupMenuItem);

  PopupMenuItem:=TMenuItem.Create(PopupMenu);
  PopupMenuItem.Caption:=popup_rdp_vm;
  PopupMenu.Items.Add(PopupMenuItem);

  PopupMenuItem:=TMenuItem.Create(PopupMenu);
  PopupMenuItem.Caption:='-';
  PopupMenu.Items.Add(PopupMenuItem);

  PopupMenuItem:=TMenuItem.Create(PopupMenu);
  PopupMenuItem.Caption:=popup_copy_vm_name;
  PopupMenu.Items.Add(PopupMenuItem);

  PopupMenuItem:=TMenuItem.Create(PopupMenu);
  PopupMenuItem.Caption:=popup_copy_com1_command;
  PopupMenu.Items.Add(PopupMenuItem);

  PopupMenuItem:=TMenuItem.Create(PopupMenu);
  PopupMenuItem.Caption:=popup_copy_ipv4;
  PopupMenu.Items.Add(PopupMenuItem);

  PopupMenuItem:=TMenuItem.Create(PopupMenu);
  PopupMenuItem.Caption:=popup_copy_ipv6;
  PopupMenu.Items.Add(PopupMenuItem);
end;

constructor TDeviceImageList.Create(component: TComponent);
var
  Image : TImage;
begin
  DeviceList:=TImageList.Create(component);

  Image := TImage.Create(Nil);
  Image.Width:=16;
  Image.Height:=16;

  { Index 0 }
  Image.Picture.LoadFromFile(DatadirPath+'images/devices/category.png');
  DeviceList.Add(Image.Picture.Bitmap, nil);
  { Index 1 }
  Image.Picture.LoadFromFile(DatadirPath+'images/devices/audio.png');
  DeviceList.Add(Image.Picture.Bitmap, nil);
  { Index 2 }
  Image.Picture.LoadFromFile(DatadirPath+'images/devices/cd.png');
  DeviceList.Add(Image.Picture.Bitmap, nil);
  { Index 3 }
  Image.Picture.LoadFromFile(DatadirPath+'images/devices/console.png');
  DeviceList.Add(Image.Picture.Bitmap, nil);
  { Index 4 }
  Image.Picture.LoadFromFile(DatadirPath+'images/devices/display.png');
  DeviceList.Add(Image.Picture.Bitmap, nil);
  { Index 5 }
  Image.Picture.LoadFromFile(DatadirPath+'images/devices/hd.png');
  DeviceList.Add(Image.Picture.Bitmap, nil);
  { Index 6 }
  Image.Picture.LoadFromFile(DatadirPath+'images/devices/hostbridge.png');
  DeviceList.Add(Image.Picture.Bitmap, nil);
  { Index 7 }
  Image.Picture.LoadFromFile(DatadirPath+'images/devices/input.png');
  DeviceList.Add(Image.Picture.Bitmap, nil);
  { Index 8 }
  Image.Picture.LoadFromFile(DatadirPath+'images/devices/lpc.png');
  DeviceList.Add(Image.Picture.Bitmap, nil);
  { Index 9 }
  Image.Picture.LoadFromFile(DatadirPath+'images/devices/network.png');
  DeviceList.Add(Image.Picture.Bitmap, nil);
  { Index 10 }
  Image.Picture.LoadFromFile(DatadirPath+'images/devices/nvme.png');
  DeviceList.Add(Image.Picture.Bitmap, nil);
  { Index 11 }
  Image.Picture.LoadFromFile(DatadirPath+'images/devices/passthru.png');
  DeviceList.Add(Image.Picture.Bitmap, nil);
  { Index 12 }
  Image.Picture.LoadFromFile(DatadirPath+'images/devices/rnd.png');
  DeviceList.Add(Image.Picture.Bitmap, nil);
  { Index 13 }
  Image.Picture.LoadFromFile(DatadirPath+'images/devices/sharefolder.png');
  DeviceList.Add(Image.Picture.Bitmap, nil);
  { Index 14 }
  Image.Picture.LoadFromFile(DatadirPath+'images/devices/usb.png');
  DeviceList.Add(Image.Picture.Bitmap, nil);

  Image.Free;
end;

{ TActionImageList }

constructor TActionImageList.Create(component: TComponent);
var
  Image : TImage;
begin
  ActionList:=TImageList.Create(component);

  Image := TImage.Create(Nil);
  Image.Width:=16;
  Image.Height:=16;

  { Index 0 }
  Image.Picture.LoadFromFile(DatadirPath+'images/menu/add.png');
  ActionList.Add(Image.Picture.Bitmap, nil);
  { Index 1 }
  Image.Picture.LoadFromFile(DatadirPath+'images/menu/edit.png');
  ActionList.Add(Image.Picture.Bitmap, nil);
  { Index 2 }
  Image.Picture.LoadFromFile(DatadirPath+'images/menu/remove.png');
  ActionList.Add(Image.Picture.Bitmap, nil);
  { Index 3 }
  Image.Picture.LoadFromFile(DatadirPath+'images/menu/rdp.png');
  ActionList.Add(Image.Picture.Bitmap, nil);
  { Index 4 }
  Image.Picture.LoadFromFile(DatadirPath+'images/menu/hide.png');
  ActionList.Add(Image.Picture.Bitmap, nil);
  { Index 5 }
  Image.Picture.LoadFromFile(DatadirPath+'images/menu/quit.png');
  ActionList.Add(Image.Picture.Bitmap, nil);
  { Index 6 }
  Image.Picture.LoadFromFile(DatadirPath+'images/menu/ssh.png');
  ActionList.Add(Image.Picture.Bitmap, nil);
  { Index 7 }
  Image.Picture.LoadFromFile(DatadirPath+'images/menu/com.png');
  ActionList.Add(Image.Picture.Bitmap, nil);
  { Index 8 }
  Image.Picture.LoadFromFile(DatadirPath+'images/menu/vm.png');
  ActionList.Add(Image.Picture.Bitmap, nil);
  { Index 9 }
  Image.Picture.LoadFromFile(DatadirPath+'images/menu/ip.png');
  ActionList.Add(Image.Picture.Bitmap, nil);

  Image.Free;
end;

{ TSystemImageList }

constructor TSystemImageList.Create(component: TComponent);
var
  Image : TImage;
begin
  SystemList:=TImageList.Create(component);

  Image := TImage.Create(Nil);
  Image.Width:=16;
  Image.Height:=16;

  { Index 0 }
  Image.Picture.LoadFromFile(DatadirPath+'images/systems/category.png');
  SystemList.Add(Image.Picture.Bitmap, nil);
  { Index 1 }
  Image.Picture.LoadFromFile(DatadirPath+'images/systems/archlinux.png');
  SystemList.Add(Image.Picture.Bitmap, nil);
  { Index 2 }
  Image.Picture.LoadFromFile(DatadirPath+'images/systems/debian.png');
  SystemList.Add(Image.Picture.Bitmap, nil);
  { Index 3 }
  Image.Picture.LoadFromFile(DatadirPath+'images/systems/dragonflybsd.png');
  SystemList.Add(Image.Picture.Bitmap, nil);
  { Index 4 }
  Image.Picture.LoadFromFile(DatadirPath+'images/systems/fedora.png');
  SystemList.Add(Image.Picture.Bitmap, nil);
  { Index 5 }
  Image.Picture.LoadFromFile(DatadirPath+'images/systems/linux.png');
  SystemList.Add(Image.Picture.Bitmap, nil);
  { Index 6 }
  Image.Picture.LoadFromFile(DatadirPath+'images/systems/freebsd.png');
  SystemList.Add(Image.Picture.Bitmap, nil);
  { Index 7 }
  Image.Picture.LoadFromFile(DatadirPath+'images/systems/netbsd.png');
  SystemList.Add(Image.Picture.Bitmap, nil);
  { Index 8 }
  Image.Picture.LoadFromFile(DatadirPath+'images/systems/openbsd.png');
  SystemList.Add(Image.Picture.Bitmap, nil);
  { Index 9 }
  Image.Picture.LoadFromFile(DatadirPath+'images/systems/other.png');
  SystemList.Add(Image.Picture.Bitmap, nil);
  { Index 10 }
  Image.Picture.LoadFromFile(DatadirPath+'images/systems/rockylinux.png');
  SystemList.Add(Image.Picture.Bitmap, nil);
  { Index 11 }
  Image.Picture.LoadFromFile(DatadirPath+'images/systems/ubuntu.png');
  SystemList.Add(Image.Picture.Bitmap, nil);
  { Index 12 }
  Image.Picture.LoadFromFile(DatadirPath+'images/systems/windows.png');
  SystemList.Add(Image.Picture.Bitmap, nil);
  { Index 13 }
  Image.Picture.LoadFromFile(DatadirPath+'images/systems/ghostbsd.png');
  SystemList.Add(Image.Picture.Bitmap, nil);
  { Index 14 }
  Image.Picture.LoadFromFile(DatadirPath+'images/systems/nomadbsd.png');
  SystemList.Add(Image.Picture.Bitmap, nil);
  { Index 15 }
  Image.Picture.LoadFromFile(DatadirPath+'images/systems/almalinux.png');
  SystemList.Add(Image.Picture.Bitmap, nil);
  { Index 16 }
  Image.Picture.LoadFromFile(DatadirPath+'images/systems/alpinelinux.png');
  SystemList.Add(Image.Picture.Bitmap, nil);
  { Index 17 }
  Image.Picture.LoadFromFile(DatadirPath+'images/systems/endeavouros.png');
  SystemList.Add(Image.Picture.Bitmap, nil);
  { Index 18 }
  Image.Picture.LoadFromFile(DatadirPath+'images/systems/kalilinux.png');
  SystemList.Add(Image.Picture.Bitmap, nil);
  { Index 19 }
  Image.Picture.LoadFromFile(DatadirPath+'images/systems/manjaro.png');
  SystemList.Add(Image.Picture.Bitmap, nil);
  { Index 20 }
  Image.Picture.LoadFromFile(DatadirPath+'images/systems/parrot.png');
  SystemList.Add(Image.Picture.Bitmap, nil);
  { Index 21 }
  Image.Picture.LoadFromFile(DatadirPath+'images/systems/popos.png');
  SystemList.Add(Image.Picture.Bitmap, nil);
  { Index 22 }
  Image.Picture.LoadFromFile(DatadirPath+'images/systems/linuxmint.png');
  SystemList.Add(Image.Picture.Bitmap, nil);
  { Index 23 }
  Image.Picture.LoadFromFile(DatadirPath+'images/systems/cachyos.png');
  SystemList.Add(Image.Picture.Bitmap, nil);
  { Index 24 }
  Image.Picture.LoadFromFile(DatadirPath+'images/systems/zorinos.png');
  SystemList.Add(Image.Picture.Bitmap, nil);

  Image.Free;
end;

{ TTrayIconPopupMenu }

constructor TTrayIconPopupMenu.Create(component: TComponent);
begin
  PopupMenu := TPopupMenu.Create(component);

  PopupMenuItem := TMenuItem.Create(PopupMenu);
  PopupMenuItem.Caption := popup_tray_show_hide;
  PopupMenu.Items.Add(PopupMenuItem);

  PopupMenuItem := TMenuItem.Create(PopupMenu);
  PopupMenuItem.Caption := popup_tray_quit;
  PopupMenu.Items.Add(PopupMenuItem);
end;

{ TSystemTrayIcon }

constructor TSystemTrayIcon.Create(component: TComponent);
begin
  TrayIcon:=TTrayIcon.Create(component);
  TrayIcon.Hint:='Bhyve Management GUI';
  TrayIcon.Icon.LoadFromFile(DatadirPath+'images/additional/bhyvemgr.ico');
end;

function FillComboIntegerType(Combo: TComboBox; StartNumber: Integer;
  EndNumber: Integer; Increment: Integer): Boolean;
var
  i : Integer;
begin
  i:=StartNumber;

  repeat
    Combo.Items.Add(i.ToString);
    Inc(i, Increment);
  until i > EndNumber;

  Result:=True;
end;

function FillComboBooleanType(Combo: TComboBox): Boolean;
begin
  combo.AddItem('false', TObject(1));
  combo.AddItem('true', TObject(2));

  Result:= True;
end;

function FillComboKeyboardLayout(Combo: TComboBox): Boolean;
var
  i : Integer;
  keyboard_layouts : TStringList;
begin
  keyboard_layouts:=FindAllFiles(KeyBoardLayoutPath, '', False);
  keyboard_layouts.Sorted:=True;

  for i:=0 to keyboard_layouts.Count-1 do
  begin
    combo.Items.Add(ExtractFileName(keyboard_layouts[i]));
  end;

  keyboard_layouts.Free;

  Result:=True;
end;

function FillComboBootrom(Combo: TComboBox): Boolean;
begin
{$ifdef CPUAARCH64}
  Combo.Items.Add('u-boot.bin');
{$endif CPUAARCH64}
{$ifdef CPUAMD64}
  Combo.Items.Add('BHYVE_UEFI.fd');
{$endif CPUAMD64}
  Result:=True;
end;

function FillComboBootvars(Combo: TComboBox): Boolean;
begin
  Combo.Items.Add(EmptyStr);
{$ifdef CPUAMD64}
  Combo.Items.Add('uefi-vars.fd');
{$endif CPUAMD64}
  Result:=True;
end;

procedure FillComboLanguage(Combo: TComboBox);
begin
  Combo.Items.Add('en');
  Combo.Items.Add('es');
  Combo.Items.Add('zh_CN');
end;

procedure FillComboTpmDevice(Combo: TComboBox);
var
  i : Integer;
  TmpList : TStringList;
begin
  TmpList:=TStringList.Create();
  TmpList.Text:=GetEventDeviceList('/dev', 'tpm*');
  TmpList.Sorted:=True;

  Combo.Clear;
  Combo.items.Add(EmptyStr);

  for i:=0 to TmpList.Count-1 do
  begin
    Combo.Items.Add(TmpList[i]);
  end;

  TmpList.Free;
end;

procedure FillComboTpmType(Combo: TComboBox);
begin
  Combo.items.Add(EmptyStr);
  Combo.Items.Add('passthru');
  if GetOsreldate.ToInt64 >= 1403000 then
    Combo.Items.Add('swtpm');
end;

procedure FillComboTpmVersion(Combo: TComboBox);
begin
  Combo.items.Add(EmptyStr);
  Combo.Items.Add('2.0');
end;

procedure FillComboResolution(Combo: TComboBox);
begin
  Combo.Items.Add('640x480');
  Combo.Items.Add('720x480');
  Combo.Items.Add('800x600');
  Combo.Items.Add('1024x768');
  Combo.Items.Add('1280x720');
  Combo.Items.Add('1440x960');
  Combo.Items.Add('1600x900');
  Combo.Items.Add('1920x1080');
  Combo.Items.Add('2048x1080');
  Combo.Items.Add('2560x1440');
  Combo.Items.Add('3840x2160');
end;

function FillComboSystemType(Combo: TComboBox): Boolean;
begin
  combo.AddItem('BSD', TObject(PtrUint(0)));
  combo.AddItem('Linux', TObject(PtrUint(1)));
  combo.AddItem('Windows', TObject(PtrUint(2)));
  combo.AddItem('Other', TObject(PtrUint(3)));
  Result:= True;
end;

function FillComboSystemVersion(Combo: TComboBox; System: String): Boolean;
begin
  case System of
  'BSD':
    begin
      combo.AddItem('DragonFlyBSD 6.x', TObject(3));
      combo.AddItem('FreeBSD 13.x', TObject(6));
      combo.AddItem('FreeBSD 14.x', TObject(6));
      combo.AddItem('FreeBSD 15.x', TObject(6));
      combo.AddItem('GhostBSD 25.x', TObject(13));
      combo.AddItem('NomadBSD 14.x', TObject(14));
      combo.AddItem('NetBSD 9.x', TObject(7));
      combo.AddItem('NetBSD 10.x', TObject(7));
      combo.AddItem('OpenBSD 7.x', TObject(8));
      combo.AddItem('Other', TObject(9));
    end;
  'Linux':
    begin
      combo.AddItem('Almalinux 8.x', TObject(15));
      combo.AddItem('Almalinux 9.x', TObject(15));
      combo.AddItem('Almalinux 10.x', TObject(15));
      combo.AddItem('Alpinelinux 3.x', TObject(16));
      combo.AddItem('Arch Linux 2025.x', TObject(1));
      combo.AddItem('CachyOS 25x', TObject(23));
      combo.AddItem('Debian 13.x', TObject(2));
      combo.AddItem('EndeavourOS 2025.x', TObject(17));
      combo.AddItem('Fedora 42.x', TObject(4));
      combo.AddItem('Kali Linux 2025.x', TObject(18));
      combo.AddItem('Linux Mint 22.x', TObject(22));
      combo.AddItem('Manjaro 25.x', TObject(19));
      combo.AddItem('Parrot 6.x', TObject(20));
      combo.AddItem('Pop! OS 22.x', TObject(21));
      combo.AddItem('Rockylinux 8.x', TObject(10));
      combo.AddItem('Rockylinux 9.x', TObject(10));
      combo.AddItem('Rockylinux 10.x', TObject(10));
      combo.AddItem('Ubuntu 24.x', TObject(11));
      combo.AddItem('Ubuntu 25.x', TObject(11));
      combo.AddItem('Zorin OS 17.x', TObject(24));
      combo.AddItem('Other', TObject(5));
    end;
  'Windows':
    begin
      combo.AddItem('Windows 10.x', TObject(12));
      combo.AddItem('Windows 11.x', TObject(12));
      combo.AddItem('Windows Server 2019', TObject(12));
      combo.AddItem('Windows Server 2022', TObject(12));
      combo.AddItem('Other', TObject(9));
    end;
  'Other':
    begin
      combo.AddItem('Other', TObject(9));
    end;
  end;

  Result:= True;
end;

function FillComboVirtualStorageType(Combo: TComboBox): Boolean;
begin
  combo.Items.Add('image file');
  if UseZfs = 'yes' then
  begin
    combo.Items.Add('zfs sparse volume');
    combo.Items.Add('zfs volume');
  end;

  Result:= True;
end;

function FillComboVirtualDeviceType(Combo: TComboBox): Boolean;
begin
  combo.Items.Add('ahci-cd');
  combo.Items.Add('ahci-hd');
  combo.Items.Add('nvme');
  combo.Items.Add('virtio-blk');
  { combo.Items.Add('virtio-scsi'); }
  Result:= True;
end;

function FillComboZpoolList(Combo: TComboBox): Boolean;
var
  i : Integer;
  zpool_list : TStringList;
begin
  zpool_list:=TStringList.Create;
  zpool_list.Text:=GetZpoolList;

  for i:=0 to zpool_list.Count-1 do
  begin
    combo.Items.Add(zpool_list[i]);
  end;

  zpool_list.Free;

  Result:=True;
end;

end.

