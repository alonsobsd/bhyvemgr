# Bhyvemgr
Bhyvemgr is a bhyve management GUI written in Freepascal/Lazarus on FreeBSD. It needs a bunch of tools mostly installed on base system and some installed from ports/packages. Currently it supports amd64 and aarch64. The main goal is to be a desktop user application to easily and quickly setup and run virtual machines on the FreeBSD host.

<img width="832" height="698" alt="image" src="https://github.com/user-attachments/assets/08a90ae3-23b6-4519-87c9-0d5bb3e1aad1" />

# Features
- virtual machines management
- create virtual machines from cloud/vm images
- cloud init and naugeinit initialization support
- devices management (support almost all bhyve pci devices with some exceptions)
- dnsmasq support
- vnc and xfreerdp client support
- zfs/ufs support
- raw/zfs volume support
- bridge/tap support
- bhyve_config configuration variables support
- basic sudo/doas support
- uefi/uboot support only
- initial swtpm support on FreeBSD >= 1403000
- ipv4/ipv6 support
- aarch64 and amd64 support
- i18n support
- PF/NAT support
- and more

# TODO
- Allow change VM (zfs/ufs) directory to custom ones

# Bhyvemgr dependencies
## From base system
bhyve, bhyvectl, bhyveload, chown, chmod, fetch, file, ifconfig, install, kill, kldload, kldstat, makefs, pciconf, pfctl, pgrep, rm, service, sysctl, truncate, xz, zfs and zpool
## From ports/packages
bhyve-firmware (sysutils/bhyve-firmware), doas (security/doas), qemu-tools (emulatorsd/qemu@tools), remote-viewer (net-mgmt/virt-viewer), swtpm (sysutils/swtpm), sudo (security/sudo), and xfreerdp3 (net/freerdp3)

### Network configuration
bhyvemgr can use two kind of network settings: *Quick network configuration* or *Optimal network configuration*. Choose one of them accord to your own needs. I recommend second one because it permits a complete network management of virtual machines. Take a look at [network configuration guide](https://github.com/alonsobsd/bhyvemgr/wiki/network_config) for details about how use/configure them.

### sudo / doas configuration

bhyve requires root privileges on FreeBSD. To handle these tasks, bhyvemgr uses sudo or doas to mitigate certain security risks. The easiest - but not recommended - way to configure sudo or doas is as follows:

For sudo, if the user is part of the wheel group. Alternatively, a specific user can be defined instead of the group - replace :wheel with a username, such as acm, for example.
```sh
%wheel ALL=(ALL:ALL) NOPASSWD: ALL
```
For doas, if the user is part of the wheel group. Alternatively, a specific user can be defined instead of the group - replace :wheel with a username, such as acm, for example.
```sh
permit nopass :wheel
```
Otherwise, if you panic, use the following:

For sudo, if the user is part of the wheel group
```sh
%wheel ALL=(ALL) NOPASSWD: /usr/sbin/bhyve -k *, /usr/sbin/bhyvectl --vm=* destroy,
/usr/sbin/chmod 750 /zroot/bhyvemgr, /bin/chown acm: /zroot/bhyvemgr, /sbin/ifconfig bhyve0 addm *,
/usr/sbin/install -d *, /bin/kill -SIGTERM *, /sbin/kldload, /sbin/pfctl, /usr/bin/pgrep, /bin/rm -R /zroot/bhyvemgr/*,
/usr/sbin/service dnsmasq restart, /sbin/zfs create * zroot/bhyvemgr/*, /sbin/zfs destroy * zroot/bhyvemgr/*,
/sbin/zfs set volsize=* zroot/bhyvemgr/*
```
For doas, if the user is part of the wheel group
```sh
permit nopass :wheel as root cmd bhyve
permit nopass :wheel as root cmd bhyvectl
permit nopass :wheel as root cmd chmod
permit nopass :wheel as root cmd chown
permit nopass :wheel as root cmd ifconfig
permit nopass :wheel as root cmd install
permit nopass :wheel as root cmd kill
permit nopass :wheel as root cmd kldload
permit nopass :wheel as root cmd pfctl
permit nopass :wheel as root cmd pgrep
permit nopass :wheel as root cmd rm
permit nopass :wheel as root cmd service
permit nopass :wheel as root cmd zfs
```

# Run bhyvemgr for the first time
When bhyvemgr starts in the first time, this will create a initial config file. It is mandatory to review, modify (if it is necessary) and press **Save settings** button from of **Settings form** the first time

<img width="512" height="189" alt="image" src="https://github.com/user-attachments/assets/f8c526bf-1036-4a7f-ae98-52cfa95ae10b" />

<img width="811" height="564" alt="image" src="https://github.com/user-attachments/assets/beac6634-d779-4177-8689-773e076ea1e3" />

# Demo

### On FreeBSD aarch64

[![bhyvemgr aarch64](https://img.youtube.com/vi/PagnKjWE_Uw/0.jpg)](https://www.youtube.com/watch?v=PagnKjWE_Uw)

### On FreeBSD amd64

[![bhyvemgr amd64](https://img.youtube.com/vi/B-GPRHfnZsc/0.jpg)](https://www.youtube.com/watch?v=B-GPRHfnZsc)

[Bhyvemgr Wiki](https://github.com/alonsobsd/bhyvemgr/wiki) contains guides about how use bhyvemgr in some use cases. Enjoy creating and testing your virtual machines on FreeBSD


