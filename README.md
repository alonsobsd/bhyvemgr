# Bhyvemgr
Bhyvemgr is a bhyve management GUI written in Freepascal/Lazarus on FreeBSD. It needs a bunch of tools mostly installed on base system and some installed from ports/packages. Currently it supports only amd64. The main goal is be an desktop user useful app for configure and run virtual machines easily and quickly on FreeBSD host.

![image](https://github.com/user-attachments/assets/6e084335-2a11-451e-9848-d4bb9775ad19)

# Features
- virtual machines management
- devices management
- dnsmasq support
- vnc and xfreerdp client support
- zfs/ufs support
- raw/zfs volume support
- bridge/tap support
- bhyve_config configuration variables support
- sudo/doas support
- ipv4 support
- and more

# TODO
- Complete global settings edition
- aarch64 support (depends of fix some Freepascal/Lazarus linking issues on FreeBSD/aarch64)
- Log message

# Bhyvemgr dependencies
## From base system
bhyve, bhyvectl, bhyveload, chown, chmod, ifconfig, install, kill, kldload, kldstat, pciconf, pgrep, rm, service, sysctl, truncate, zfs and zpool
## From ports/packages
doas (security/doas), remote-viewer (net-mgmt/virt-viewer), sudo (security/sudo) and xfreerdp3 (net/freerdp3)

# Bhyvemgr configuration
## Network configuration

### Quick config
```sh
cloned_interfaces="bridge0"
ifconfig_bridge0_name="bhyve0"
ifconfig_bhyve0="addm em0 up"
ifconfig_bhyve0_descr="bhyve manager bridge"
```
### Best config

```sh
gateway_enable="YES"
cloned_interfaces="bridge0"
ifconfig_bridge0_name="bhyve0"
ifconfig_bhyve0="inet 10.0.0.1 netmask 255.255.255.0"
ifconfig_bhyve0_descr="bhyve manager bridge"
pf_enable="YES
```

#### Dnsmasq

```sh
# sysrc dnsmasq_enable="YES"
# install -d -m 770 -o root -g wheel /usr/local/etc/dnsmasq.d
# install -d -m 770 -o acm /usr/local/etc/dnsmasq.d/bhyvemgr
```
```sh
# ee /usr/local/etc/dnsmasq.conf
```
```sh
port=53
domain-needed
no-resolv
except-interface=lo0
bind-interfaces
local-service
dhcp-authoritative
domain=bsd.lan
server=1.1.1.1
server=1.0.0.1
interface=bhyve0
dhcp-range=10.0.0.0,static,255.255.255.0,60m
dhcp-option=option:router,10.0.0.1
dhcp-option=option:dns-server,10.0.0.1
conf-dir=/usr/local/etc/dnsmasq.d/bhyvemgr/,*.conf
```

```sh
# ee /etc/resolv.conf
```
```sh
domain 10.0.0.1
```

#### sudo / doas configuration

```sh
%wheel ALL=(ALL:ALL) NOPASSWD: ALL
```

```sh
permit nopass :wheel
```

```sh
%wheel ALL=(ALL) NOPASSWD: /usr/sbin/bhyve -k *, /usr/sbin/bhyvectl --vm=* destroy,
/usr/sbin/chmod 750 /zroot/bhyvemgr, /bin/chown acm: /zroot/bhyvemgr, /sbin/ifconfig bhyve0 addm *,
/usr/sbin/install -d *, /bin/kill -SIGTERM *, /sbin/kldload, /usr/bin/pgrep, /bin/rm -R /zroot/bhyvemgr/*,
/usr/sbin/service dnsmasq restart, /sbin/zfs create * zroot/bhyvemgr/*, /sbin/zfs destroy * zroot/bhyvemgr/*
```
```sh
permit nopass :wheel as root cmd bhyve
permit nopass :wheel as root cmd bhyvectl
permit nopass :wheel as root cmd chmod
permit nopass :wheel as root cmd chown
permit nopass :wheel as root cmd ifconfig
permit nopass :wheel as root cmd install
permit nopass :wheel as root cmd kill
permit nopass :wheel as root cmd kldload
permit nopass :wheel as root cmd pgrep
permit nopass :wheel as root cmd rm
permit nopass :wheel as root cmd service
permit nopass :wheel as root cmd zfs
```
