{ SPDX-License-Identifier: BSD-3-Clause

Copyright (c) 2024-2026, J. Alonso Cárdenas <acardenas@bsd-peru.org>

}

unit unit_device;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TAudioDeviceClass=class
    device:String;
    pci:String;
    play:String;
    rec:String;
  end;

  TDisplayDeviceClass=class
    device : String;
    pci:String;
    tcp:String;
    port:String;
    w:Integer;
    h:Integer;
    vga:String;
    wait:String;
    pass:String;
  end;

  THostbridgeDeviceClass=class
    device : String;
    pci:String;
  end;

  TLPCDeviceClass=class
    device : String;
    pci:String;
    bootrom:String;
    bootvars:String;
    com1:String;
    com2:String;
    com3:String;
    com4:String;
    fwcfg:String;
    pctestdev:String;
  end;

  TNetworkDeviceClass=class
    device : String;
    pci:String;
    backend:String;
    mac:String;
    mtu:Integer;
    path:String;
    peerhook:String;
    socket:String;
    hook:String;
    hostfwd:String;
  end;

  TPassthruDeviceClass=class
    device : String;
    pci:String;
    bus:Integer;
    slot:Integer;
    func:Integer;
    pptdev:String;
    rom:String;
  end;

  TRNGDeviceClass=class
    device : String;
    pci:String;
  end;

  TSerialVirtioConsoleDeviceClass=class
    device : String;
    pci:String;
    name:String;
    path:String;
    port:Integer;
  end;

  TVirtioInputDeviceClass=class
    device : String;
    path : String;
    pci:String;
  end;

  TShareFolderDeviceClass=class
    device : String;
    pci:String;
    sharename:String;
    path:String;
    ro:Boolean;
  end;

  TStorageAhciDeviceClass=class
    device : String;
    device_type : String;
    pci:String;
    port:Integer;
    path:String;
    nocache:Boolean;
    nodelete:Boolean;
    sync:Boolean;
    direct:Boolean;
    ro:Boolean;
    sectorsize:String;
    ser:String;
    nmrr:Integer;
    rev:String;
    model:String;
    storage_size: String;
    storage_type: String;
  end;

  TStorageNvmeDeviceClass=class
    device : String;
    pci:String;
    devpath:String;
    nocache:Boolean;
    nodelete:Boolean;
    sync:Boolean;
    direct:Boolean;
    ro:Boolean;
    maxq:Integer;
    qsz:Integer;
    ioslots:Integer;
    sectsz:Integer;
    ser:String;
    eui64:Integer;
    dsm:String;
    ram:Integer;
    storage_size: String;
    storage_type: String;
  end;

  TStorageVirtioBlkDeviceClass=class
    device : String;
    pci:String;
    path:String;
    nocache:Boolean;
    nodelete:Boolean;
    sync:Boolean;
    direct:Boolean;
    ro:Boolean;
    sectorsize:string;
    ser:String;
    storage_size: String;
    storage_type: String;
  end;

  TStorageVirtioScsiDeviceClass=class
    device : String;
    pci:String;
    dev:String;
    iid:Integer;
  end;

  TUsbXhciDeviceClass=class
    device:String;
    pci:String;
    slot:Integer;
    slot_device:String;
  end;

  TVirtualMachineClass=class
    name:String;
    description:String;
    uuid:String;
    config_path:String;
    system_type:String;
    system_version:String;
    image:Integer;
    ipaddress:String;
    ip6address:String;
    rdp:Boolean;
    ipv6:Boolean;
    nat:Boolean;
    pf:Boolean;
  end;

implementation

end.

