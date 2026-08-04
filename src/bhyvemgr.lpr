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

program bhyvemgr;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  DefaultTranslator,
  LCLTranslator,
  Translations,
  Classes, Forms, UniqueInstanceRaw, lazcontrols, SysUtils, LazLogger, Dialogs,
  { you can add units after this }
  unit_configuration, unit_device, unit_global,
  unit_component, unit_util, unit_thread, unit_language,
  form_main, form_vm_create, form_change_value, form_audio_device,
  form_display_device, form_hostbridge_device, form_lpc_device,
  form_network_device, form_storage_device, form_about, form_settings,
  form_share_folder_device, form_console_device, form_passthru_device,
  form_input_device, form_rdp_connection, form_vm_info,
  form_packet_filter_rules, unit_socket;

{$R *.res}
var
  Configuration : ConfigurationClass;
  BhyveConfiguration : BhyvemgrdConfigurationClass;
  ZfsPoolList : TStringList;

begin
  if not InstanceRunning('bhyvemgr') then
  begin
    {$IFDEF DEBUG}
      if FileExists('heap.trc') then
        DeleteFile('heap.trc');
      SetHeapTraceOutput('heap.trc');
    {$ENDIF DEBUG}

    ZfsPoolList:= TStringList.Create;

    DebugLogger.UseStdOut:= False;
    DebugLogger.CloseLogFileBetweenWrites:= true;
    DebugLogger.LogName:= GetUserDir + BHYVEMGR_LOG_FILE;

    DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : '+debugln_bhyve_started);

    if FileExists(GetUserDir + BHYVEMGR_CONFIG_FILE) and FileExists(COMMON_CONFIG_FILE) then
    begin
      Configuration:= ConfigurationClass.Create(GetUserDir + BHYVEMGR_CONFIG_FILE);
      Configuration.GeneralConfig();

      BhyveConfiguration:= BhyvemgrdConfigurationClass.Create(COMMON_CONFIG_FILE);

      SetUseDnsmasq(Configuration.GetOption('general','use_dnsmasq'));
      SetUseZfs(Configuration.GetOption('general','use_zfs'));
      SetCloudVmImagesPath(Configuration.GetOption('general','cloudvm_images_path'));
      SetUseSystray(Configuration.GetOption('general','use_systray'));
      SetUseIpv6(Configuration.GetOption('general','use_ipv6'));
      SetUsePf(Configuration.GetOption('general','use_pf'));
      SetLanguage(Configuration.GetOption('general','language'));

      SetBhyveCmd(Configuration.GetOption('bhyve-tools','bhyve_cmd'));
      SetBhyvectlCmd(Configuration.GetOption('bhyve-tools','bhyvectl_cmd'));
      SetBhyveloadCmd(Configuration.GetOption('bhyve-tools','bhyveload_cmd'));

      SetBridgeInterface(Configuration.GetOption('network','bridge_interface'));
      SetSubnet(Configuration.GetOption('network','subnet'));
      SetIpv6Prefix(Configuration.GetOption('network','ipv6_prefix'));
      SetExternalInterface(Configuration.GetOption('network','external_interface'));
      SetExternalIpv4(Configuration.GetOption('network','external_ipv4'));
      SetExternalIpv6(Configuration.GetOption('network','external_ipv6'));

      SetVncviewerCmd(Configuration.GetOption('remote-tools','vncviewer_cmd'));
      SetXfreerdpCmd(Configuration.GetOption('remote-tools','xfreerdp_cmd'));
      SetXfreerdpArgs(Configuration.GetOption('remote-tools','xfreerdp_args'));

      SetQemuImgCmd(Configuration.GetOption('extra-tools','qemu-img_cmd'));
      SetSwtpmCmd(Configuration.GetOption('extra-tools','swtpm_cmd'));
      SetSwtpmIoctlCmd(Configuration.GetOption('extra-tools','swtpm_ioctl_cmd'));

      SetZfsZpool(Configuration.getOption('zfs','zfs_zpool'));
      SetZfsCreateOptions(Configuration.getOption('zfs','zfs_create_options'));

      SetVmPath(BhyveConfiguration.GetOption('common','vm_path'));

      Configuration.Free;
      BhyveConfiguration.Free;
    end
    else
    begin
      SetNewConfig(True);

      SetUseDnsmasq('yes');
      SetUseSystray('yes');
      SetUseIpv6('no');
      SetUsePf('no');
      SetBridgeInterface('bhyve0');
      SetSubnet('10.0.0.0/24');
      SetIpv6Prefix('fd92:5e7a:bd00:1::');
      SetExternalInterface('');
      SetExternalIpv4(EmptyStr);
      SetExternalIpv6(EmptyStr);
      SetBhyveCmd('/usr/sbin/bhyve');
      SetBhyvectlCmd('/usr/sbin/bhyvectl');
      SetBhyveloadCmd('/usr/sbin/bhyveload');
      SetQemuImgCmd('/usr/local/bin/qemu-img');
      SetSwtpmCmd('/usr/local/bin/swtpm');
      SetSwtpmIoctlCmd('/usr/local/bin/swtpm_ioctl');
      SetVncviewerCmd('/usr/local/bin/vncviewer');
      SetXfreerdpCmd('/usr/local/bin/xfreerdp3');
      SetXfreerdpArgs('/cert:tofu /sound:sys:oss /network:lan /bpp:32 /gfx:rfx:on /log-level:ERROR');

      ZfsPoolList.Text:=GetZpoolList();

      if ZfsPoolList.Count > 0 then
       begin
         SetZfsZpool(ZfsPoolList[0]);
         SetZfsCreateOptions('-o compress=lz4 -o atime=off');
         SetUseZfs('yes');
         SetVmPath('/'+ZfsPoolList[0]+'/bhyvemgr');
       end
       else
       begin
         SetZfsZpool('zroot');
         SetZfsCreateOptions('-o compress=lz4 -o atime=off');
         SetUseZfs('no');
         SetVmPath('/usr/local/bhyvemgr');
       end;

      SetCloudVmImagesPath(GetUserDir+'.bhyvemgr');
      SetLanguage('en');
    end;

    ZfsPoolList.Free;

    SetDefaultLang(Language, DatadirPath+'languages');
    Translations.TranslateUnitResourceStrings('LCLStrConsts', DatadirPath+'languages/lcl/lclstrconsts.'+Language+'.po');

    if not CheckKernelModule('vmm') or not CheckKernelModule('nmdm') or not CheckKernelModule('mac_do') then
    begin
      DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : '+error_kernel_modules);
      Exit;
    end;

    SetOsreldate(Trim(CheckSysctl('kern.osreldate')));

    RequireDerivedFormResource:=True;
  Application.Scaled:=True;
    Application.Initialize;
    Application.CreateForm(TFormBhyveManager, FormBhyveManager);
    Application.Run;
  end;
end.

