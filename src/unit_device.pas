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
    port:Integer;
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
  end;

implementation

end.

