# Bhyvemgr
Bhyvemgr is a bhyve management GUI written in Freepascal/Lazarus on FreeBSD. It needs a bunch of tools mostly installed on base system and some installed from ports/packages. Currently it supports only amd64. The main goal is be a desktop user app for configure and run virtual machines easily and quickly on FreeBSD host.

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
- global settings entries
- aarch64 support (depends of fix some Freepascal/Lazarus linking issues on FreeBSD/aarch64)
- log message

# Bhyvemgr dependencies
## From base system
bhyve, bhyvectl, bhyveload, chown, chmod, ifconfig, install, kill, kldload, kldstat, pciconf, pgrep, rm, service, sysctl, truncate, zfs and zpool
## From ports/packages
doas (security/doas), remote-viewer (net-mgmt/virt-viewer), sudo (security/sudo) and xfreerdp3 (net/freerdp3)

# Bhyvemgr configuration
## Network configuration

### Quick config
If you want use bhyve without much network configuration, you can create a bridge and add your ethernet interface to it. Take on mind you will need a dhcp server in your network enviorenment if you want virtual machine network configuration will be assigned automatically. Otherwise you must set network configuration manually for each virtual machine.

Add the following lines to your **/etc/rc.conf** file

```sh
cloned_interfaces="bridge0"
ifconfig_bridge0_name="bhyve0"
ifconfig_bhyve0="addm em0 up"
ifconfig_bhyve0_descr="bhyve manager bridge"
```
bhyvemgr add each tap interface to this bridge when a virtual machine is started. The same way it deletes and removes tap interface when a virtual machine is stopped.

### Best config
On another hand, if you want use bhyve with a much complete network configuration (dhcpd and dns features) including NAT support, you need configure some additional services like dnsmasq and packet filter. Create a bridge and assign one ip address to it. This will be used like a gateway ip address for each virtual machine. A subnet 10.0.0.0/24 will be used in this guide.

```sh
gateway_enable="YES"
cloned_interfaces="bridge0"
ifconfig_bridge0_name="bhyve0"
ifconfig_bhyve0="inet 10.0.0.1 netmask 255.255.255.0"
ifconfig_bhyve0_descr="bhyve manager bridge"
pf_enable="YES
```
#### Dnsmasq

Dnsmasq is used for bring dhcp and dns features to our virtual machine network environment. bhyvemgr will add a entry to dnsmasq config (ip address, mac and vm name) when a virtual machine is created. Add a dnsmasq entry to **/etc/rc.conf** file

```sh
# sysrc dnsmasq_enable="YES"
```
Create some dnsmasq directories for store virtual machine config files. In this sample, I am using **acm** like my bhyvemgr user

```sh
# install -d -m 770 -o root -g wheel /usr/local/etc/dnsmasq.d
# install -d -m 770 -o acm /usr/local/etc/dnsmasq.d/bhyvemgr
```
The following is a minimal dnsmasq configuration needed

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
It is necessary add **10.0.0.1 ip** address to **/etc/resolv.conf** file for resolv each virtual machine from our FreeBSD host

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
