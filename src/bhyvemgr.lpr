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

program bhyvemgr;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  DefaultTranslator,
  LCLTranslator,
  Translations,
  Forms, UniqueInstanceRaw, lazcontrols, form_main, unit_configuration, unit_device
  { you can add units after this }
  ,SysUtils, form_vm_create, form_change_value, unit_global, unit_component,
  unit_util, unit_thread, form_audio_device, form_display_device,
  form_hostbridge_device, form_lpc_device, form_network_device,
  form_storage_device, form_about, form_settings, Dialogs,
  form_share_folder_device, form_console_device, form_passthru_device,
  form_input_device, form_rdp_connection, form_vm_info, unit_language;

{$R *.res}
var
  Configuration : ConfigurationClass;

begin
  if not InstanceRunning('bhyvemgr') then
  begin
    Configuration:= ConfigurationClass.Create(GetUserDir + '.config/bhyvemgr/config.conf');
    Configuration.GeneralConfig();

    SetUseDnsmasq(Configuration.GetOption('general','use_dnsmasq'));
    SetUseSudo(Configuration.GetOption('general','use_sudo'));
    SetUseZfs(Configuration.GetOption('general','use_zfs'));
    SetVmPath(Configuration.GetOption('general','vm_path'));
    SetCloudVmImagesPath(Configuration.GetOption('general','cloudvm_images_path'));
    SetUseSystray(Configuration.GetOption('general','use_systray'));
    SetUseIpv6(Configuration.GetOption('general','use_ipv6'));
    SetLanguage(Configuration.GetOption('general','language'));

    SetBhyveCmd(Configuration.GetOption('bhyve-tools','bhyve_cmd'));
    SetBhyvectlCmd(Configuration.GetOption('bhyve-tools','bhyvectl_cmd'));
    SetBhyveloadCmd(Configuration.GetOption('bhyve-tools','bhyveload_cmd'));

    SetBridgeInterface(Configuration.GetOption('network','bridge_interface'));
    SetSubnet(Configuration.GetOption('network','subnet'));
    SetIpv6Prefix(Configuration.GetOption('network','ipv6_prefix'));

    SetDoasCmd(Configuration.GetOption('user-tools','doas_cmd'));
    SetSudoCmd(Configuration.GetOption('user-tools','sudo_cmd'));

    SetVncviewerCmd(Configuration.GetOption('remote-tools','vncviewer_cmd'));
    SetXfreerdpCmd(Configuration.GetOption('remote-tools','xfreerdp_cmd'));
    SetXfreerdpArgs(Configuration.GetOption('remote-tools','xfreerdp_args'));

    SetChownCmd(Configuration.GetOption('extra-tools','chown_cmd'));
    SetChmodCmd(Configuration.GetOption('extra-tools','chmod_cmd'));
    SetCpCmd(Configuration.GetOption('extra-tools','cp_cmd'));
    SetFetchCmd(Configuration.GetOption('extra-tools','fetch_cmd'));
    SetFileCmd(Configuration.GetOption('extra-tools','file_cmd'));
    SetIfconfigCmd(Configuration.GetOption('extra-tools','ifconfig_cmd'));
    SetInstallCmd(Configuration.GetOption('extra-tools','install_cmd'));
    SetKillCmd(Configuration.GetOption('extra-tools','kill_cmd'));
    SetKldloadCmd(Configuration.GetOption('extra-tools','kldload_cmd'));
    SetKldstatCmd(Configuration.GetOption('extra-tools','kldstat_cmd'));
    SetMakefsCmd(Configuration.GetOption('extra-tools','makefs_cmd'));
    SetPciconfCmd(Configuration.GetOption('extra-tools','pciconf_cmd'));
    SetPgrepCmd(Configuration.GetOption('extra-tools','pgrep_cmd'));
    SetQemuImgCmd(Configuration.GetOption('extra-tools','qemu-img_cmd'));
    SetRmCmd(Configuration.GetOption('extra-tools','rm_cmd'));
    SetServiceCmd(Configuration.GetOption('extra-tools','service_cmd'));
    SetSwtpmCmd(Configuration.GetOption('extra-tools','swtpm_cmd'));
    SetSwtpmIoctlCmd(Configuration.GetOption('extra-tools','swtpm_ioctl_cmd'));
    SetSysctlCmd(Configuration.GetOption('extra-tools','sysctl_cmd'));
    SetTruncateCmd(Configuration.GetOption('extra-tools','truncate_cmd'));
    SetXzCmd(Configuration.GetOption('extra-tools','xz_cmd'));
    SetZfsCmd(Configuration.GetOption('extra-tools','zfs_cmd'));
    SetZpoolCmd(Configuration.GetOption('extra-tools','zpool_cmd'));

    SetZfsZpool(Configuration.getOption('zfs','zfs_zpool'));
    SetZfsCreateOptions(Configuration.getOption('zfs','zfs_create_options'));

  {  if (UseZfs = 'yes') and (CheckKernelModule('zfs')) then
      SetVmPath('/'+ZfsZpool+'/'+configuration.getOption('general','vm_path'))
    else
      SetVmPath('/usr/local/'+configuration.getOption('general','vm_path'));}

    Configuration.Free;

    SetDefaultLang(Language, DatadirPath+'languages');
    Translations.TranslateUnitResourceStrings('LCLStrConsts', DatadirPath+'languages/lcl/lclstrconsts.'+Language+'.po');

    LoadKernelModule('vmm');
    LoadKernelModule('nmdm');

    SetOsreldate(Trim(CheckSysctl('kern.osreldate')));

    {$IFDEF DEBUG}
      if FileExists('heap.trc') then
        DeleteFile('heap.trc');
      SetHeapTraceOutput('heap.trc');
    {$ENDIF DEBUG}

    RequireDerivedFormResource:=True;
  Application.Scaled:=True;
    Application.Initialize;
    Application.CreateForm(TFormBhyveManager, FormBhyveManager);
    Application.Run;
  end;
end.

