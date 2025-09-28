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

unit unit_language;

{$mode ObjFPC}{$H+}

interface

uses
  LResources;

resourcestring
  { popup strings }
  popup_add_device = 'Add device';
  popup_edit_device = 'Edit device';
  popup_delete_device = 'Delete device';
  popup_edit_global_setting = 'Edit global setting';
  popup_add_vm = 'Add virtual machine';
  popup_modify_vm = 'Modify virtual machine info';
  popup_remove_vm = 'Remove virtual machine';
  popup_rdp_vm = 'RDP to virtual machine';
  popup_copy_vm_name = 'Copy VM name';
  popup_copy_com1_command = 'Copy COM1 command';
  popup_copy_ipv4 = 'Copy IPv4 address';
  popup_copy_ipv6 = 'Copy IPv6 address';
  popup_tray_show_hide = 'Show/Hide Bhyvemgr';
  popup_tray_quit = 'Quit Bhyvemgr';
  { form_main strings }
  debugln_bhyve_started = 'Bhyvemgr was started';
  debugln_bhyve_finished = 'Bhyvemgr was finished';
  debugln_dataset_status = '%0:s: : Can not create %1:s: dataset';
  debugln_directory_status = '%0:s: : Can not create %1:s: directory';
  copy_status = 'Copying %s MB from image...';
  edit_global_status = '%0:s: value has been changed to %1:s';
  configuration_notice = 'A configuration file was generated. A settings form will be open to review / modify bhyvemgr options. Press "Save settings button" if everything is ok.';
  check_vm_running = 'bhyvemgr detects VMs running. You must stop them before of close this app. '+sLineBreak+sLineBreak+'Do you really want to close?';
  device_remove_title = 'Virtual machine devices';
  device_remove_notice = 'This action will remove all files/resources created by this device. Do you want remove %s device?';
  device_status = '%s device can not added/updated.';
  vm_start_status = '%s VM have been started';
  vm_try_status = 'Trying create %s virtual machine...';
  vm_create_status = 'A new %s virtual machine was created';
  vm_fields_status = 'You must complete all form fields';
  vm_reboot_status = '%s VM is rebooting';
  vm_poweroff_status = '%s VM has been powered off';
  vm_halt_status = '%s VM is halted';
  vm_triplefault_status = '%s VM is triple fault';
  vm_exiterror_status = '%s VM exited due to an error';
  vm_exit_status = '%s VM exited';
  exception_status = 'An exception was raised: %s';
  error_title = 'Error message';
  vm_remove_title = 'Remove VM';
  vm_remove_notice = 'Do you want remove %s VM data?';
  vm_remove_force = '%s VM data cannot be removed.'+sLineBreak+sLineBreak+'Do you want force it?';
  vm_remove_status = '%s VM data has been removed';
  vm_notremove_status = '%s VM data was not removed';
  app_error_status = '%0:s: VM : %1:s: process generated an error: %2:s:';
  virtual_machine = 'virtual machine';
  { form_settings strings }
  debugln_bhyve_settings_opened = 'Bhyve Settings : settings form was opened.';
  debugln_check_freerdp = 'Bhyve Settings : freerdp support will not be available. net/freerdp3 is not installed.';
  debugln_check_qemu = 'Bhyve Settings : qcow2 convert support will not be available. qemu-tools is not installed.';
  debugln_bhyve_settings_closed = 'Bhyve Settings : settings form was closed.';
  debugln_bhyve_settings_saved = 'Bhyve Settings : settings were saved successfully.';
  check_zfs = 'Support for zfs/zpool is not available';
  check_dnsmasq = 'dnsmasq was not found. Please install dns/dnsmasq for fix it';
  check_ipv6 = 'A valid IPv6 prefix must be defined. It will be used to assign virtual machine ipv6 addresses.';
  check_sudo = 'sudo was not found. Please install security/sudo for fix it';
  check_doas = 'doas was not found. Please install security/doas for fix it';
  check_bridge = 'A bridge name must be defined. It will be used by bhyvemgr for virtual machines network settings.';
  check_subnet = 'A valid subnet must be defined. It will be used for assign/generate ip address.';
  check_vnc = 'vnc support will not available. net-mgmt/virt-viewer is not installed.';
  check_base_binary = '%s binary was not found';
  calculated_ipv6 = 'Now, assign this IPv6 address to %s interface. Do not forget add accept_rtadv and auto_linklocal options to it too.';
  no_calculated_ipv6 = '%s IPv6 address can not calculated. IPv6 prefix or Mac address are not valid.';
  { form_vm_create strings }
  convert_status = 'Converting...';
  download_status = 'Downloading...';
  done_status = 'Done';
  nothing_status = 'Nothing to do';
  error_status = '%0:s process generated an error: %1:s';
  extract_status = 'Extracting...';
  retrieve_size = '%0:s can not retrieve %1:s file size.';
  compatible_image = '%s could not be compatible with bhyve.';
  check_support_file = '%s file is not supported.';
  check_system_type = 'You must select an operating system type. e.g, BSD, Linux, etc.';
  check_system_version = 'You must select an operating system version. e.g, FreeBSD 15.x, etc.';
  check_device_type = 'You must select a virtual device type. e.g, ahci-hd, nvme, etc.';
  check_storage_type = 'You must select a virtual storage type. e.g, image file, etc.';
  check_disk_size = 'The virtual disk size can not be less than %s';
  check_image_path = 'The Cloud/VM image does not exist.';
  check_username = 'Username is not valid.';
  check_ssh_key = 'SSH key can not be empty.';
  check_template_files = 'user-data, meta-data, or network-config are not available at templates directory.';
  check_userdata_file = 'user-data file does not exist.';
  check_metadata_file = 'meta-data file does not exist.';
  check_valid_url = 'Enter a valid URL: only http://, https://, or file:// are supported.';
  check_ipv4 = 'IPv4 address is not valid.';
  check_gateway = 'Gateway is not valid.';
  check_dns = 'DNS Servers is not valid.';
  check_boot_media = 'CD/DVD media does not exist.';
  check_vm_name = 'Virtual machine name is not valid. Valid characters are [a-z][-_][0-9]';
  check_vm = '%s virtual machine name is not available';
  check_create_task = 'The Create Virtual Machine form is open and some tasks are not finished. Please, close it before trying to close this app.';
  check_create_task_confirmation = 'The virtual machine creation task has not finished. '+sLineBreak+sLineBreak+'Do you really want to stop it and close form?';
  { form_passthru_device strings }
  ppt_devices_status = 'ppt devices were not detected';

implementation

end.

