# Bhyvemgr
Bhyvemgr is a bhyve management GUI written in Freepascal/Lazarus on FreeBSD. It needs a bunch of tools mostly installed on base system and some installed from ports/packages. Currently it supports amd64 and aarch64. The main goal is to be a desktop user application to easily and quickly setup and run virtual machines on the FreeBSD host. Since v2.0.0 version, bhyvemgr was migrated to client/server architecture. For this reason a server component was developed. This new component is named [bhyvemgrd](https://github.com/alonsobsd/bhyvemgrd).

<img width="822" height="688" alt="image" src="https://github.com/user-attachments/assets/95876676-83ce-4bba-90a6-2b09da19095d" />

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
- mac_do/mdo support
- mac_do/setcred/exec_paths support (FreeBSD >= 1501501)
- uefi/uboot support only
- swtpm support
- ipv4/ipv6 support
- aarch64 and amd64 support
- i18n support
- PF/NAT support
- Storage ZFS snapshots support
- and more

# Bhyvemgr dependencies
## From base system
bhyve, bhyvectl, bhyveload, chown, chmod, fetch, file, ifconfig, install, kill, kldload, kldstat, makefs, mdo/mac_do, pciconf, pfctl, pgrep, rm, service, sysctl, truncate, xz, zfs and zpool

## From ports/packages
bhyvemgrd (sysutils/bhyvemgrd), bhyve-firmware (sysutils/bhyve-firmware), qemu-tools (emulators/qemu@tools), swtpm (sysutils/swtpm), vncviewer (net/tigervnc-viewer) and xfreerdp3 (net/freerdp3)

# Network configuration
bhyvemgr can use two kind of network settings: *Quick network configuration* or *Optimal network configuration*. Choose one of them accord to your own needs. I recommend second one because it permits a complete network management of virtual machines. Take a look at [network configuration guide](https://github.com/alonsobsd/bhyvemgr/wiki/network_config) for details about how use/configure them.

# Privilege tasks
The new version of bhyvemgr doesn't require root privileges via sudo or doas on FreeBSD. Privileged operations are handled by [bhyvemgrd](https://github.com/alonsobsd/bhyvemgrd), [mac_do/mdo](https://man.freebsd.org/cgi/man.cgi?query=mdo&apropos=0&sektion=0&manpath=FreeBSD+14.4-RELEASE&format=html) or [mac_do/setcred/exec_paths](https://man.freebsd.org/cgi/man.cgi?query=setcred&apropos=0&sektion=0&manpath=FreeBSD+14.4-RELEASE&format=html) instead , helping to mitigate security risks. See the [bhyvemgrd README](https://github.com/alonsobsd/bhyvemgrd/blob/main/README.md) for details about how to configure and run it.

# Run bhyvemgr for the first time
bhyvemgr will create two configuration files: **gui.conf** and **common.conf**. The **common.conf** file is required for both bhyvemgr and bhyvemgrd. Take a look at [migration guide](https://github.com/alonsobsd/bhyvemgr/wiki/new_version_migration) if you are planning move from 1.15.0 to 2.0.0 or later. Don't forget add your user to bhyvemgrd group and do a re-login or restart your system before of run bhyvemgr on first time.

# Demo

### On FreeBSD aarch64

[![bhyvemgr aarch64](https://img.youtube.com/vi/PagnKjWE_Uw/0.jpg)](https://www.youtube.com/watch?v=PagnKjWE_Uw)

### On FreeBSD amd64

[![bhyvemgr amd64](https://img.youtube.com/vi/B-GPRHfnZsc/0.jpg)](https://www.youtube.com/watch?v=B-GPRHfnZsc)

[Bhyvemgr Wiki](https://github.com/alonsobsd/bhyvemgr/wiki) contains guides about how use bhyvemgr in some use cases. Enjoy creating and testing your virtual machines on FreeBSD


