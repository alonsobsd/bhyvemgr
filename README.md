# Bhyvemgr
Bhyvemgr is a bhyve management GUI written in Freepascal/Lazarus on FreeBSD. It needs a bunch of tools mostly installed on base system and some installed from ports/packages. Currently it supports amd64 and aarch64. The main goal is to be a desktop user application to easily and quickly setup and run virtual machines on the FreeBSD host.

![image](https://github.com/user-attachments/assets/82563dca-0bee-4fcb-9cdb-d1cb797b1997)

# Features
- virtual machines management
- devices management (support almost all bhyve pci devices with some exceptions)
- dnsmasq support
- vnc and xfreerdp client support
- zfs/ufs support
- raw/zfs volume support
- bridge/tap support
- bhyve_config configuration variables support
- basic sudo/doas support
- uefi/uboot support only
- initial swtpm support on CURRENT (1500026)
- ipv4 support
- aarch64 and amd64 support
- and more

# TODO
- Allow change VM (zfs/ufs) directory to custom ones
- Add uart device support
- log message

# Bhyvemgr dependencies
## From base system
bhyve, bhyvectl, bhyveload, chown, chmod, ifconfig, install, kill, kldload, kldstat, pciconf, pgrep, rm, service, sysctl, truncate, zfs and zpool
## From ports/packages
bhyve-firmware (sysutils/bhyve-firmware), doas (security/doas), remote-viewer (net-mgmt/virt-viewer), swtpm (sysutils/swtpm), sudo (security/sudo) and xfreerdp3 (net/freerdp3)

# Network configuration
bhyvemgr can use two kind of network settings: *Quick network configuration* or *Best network configuration*. Choose one of them accord to your own needs. I recommend second one because it permits a complete network management of virtual machines.

## Quick network configuration
If you want use bhyve without many network features, you can create a bridge and add your ethernet interface to it. Take on mind you will need a dhcp server in your network environment if you want that virtual machine network configuration will be assigned automatically. Otherwise you must set network configuration manually for each virtual machine.

Add the following lines to your **/etc/rc.conf** file

```sh
cloned_interfaces="bridge0"
ifconfig_bridge0_name="bhyve0"
ifconfig_bhyve0="addm em0 up"
ifconfig_bhyve0_descr="bhyve manager bridge"
```
bhyvemgr add each tap interface to **bhyve0** bridge when a virtual machine is started. The same way it deletes and removes tap interface when a virtual machine is stopped.

## Best network configuration
On another hand, if you want use bhyve with a better network features (dhcpd and dns features) including NAT support, you need configure some additional services like dnsmasq and packet filter. Create a bridge and assign an ip address to it. This will be used like a gateway by each virtual machine. A subnet **10.0.0.0/24** will be used in this guide.

```sh
gateway_enable="YES"
cloned_interfaces="bridge0"
ifconfig_bridge0_name="bhyve0"
ifconfig_bhyve0="inet 10.0.0.1 netmask 255.255.255.0"
ifconfig_bhyve0_descr="bhyve manager bridge"
pf_enable="YES"
dnsmasq_enable="YES"
```
### Dnsmasq

Dnsmasq is used for bring dhcp and dns features to our virtual machine network environment. bhyvemgr will add a entry to dnsmasq config (ip address, mac and vm name) when a virtual machine is created.

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
It is necessary add **10.0.0.1** ip address into **/etc/resolv.conf** file if you want that each virtual machine name or subdomain **(fbsd15x634 or fbsd15x64.bsd.lan)** will be resolved from FreeBSD host.

```sh
# ee /etc/resolv.conf
```
```sh
nameserver 10.0.0.1
```
### sudo / doas configuration

bhyve needs root privileges on FreeBSD. For these tasks, bhyvemgr uses sudo or doas to mitigate some security issues. The laziest way to configure sudo or doas (not recommended) is the following:

For sudo if user is part of wheel group. Also, an user can be defined there instead of wheel group (replace %wheel by acm for example).
```sh
%wheel ALL=(ALL:ALL) NOPASSWD: ALL
```
For doas if user is part of wheel group. Also, an user can be defined there instead of wheel group (replace :wheel by acm for example).
```sh
permit nopass :wheel
```
Otherwise, if you panic, use the following

For sudo if user is part of wheel group
```sh
%wheel ALL=(ALL) NOPASSWD: /usr/sbin/bhyve -k *, /usr/sbin/bhyvectl --vm=* destroy,
/usr/sbin/chmod 750 /zroot/bhyvemgr, /bin/chown acm: /zroot/bhyvemgr, /sbin/ifconfig bhyve0 addm *,
/usr/sbin/install -d *, /bin/kill -SIGTERM *, /sbin/kldload, /usr/bin/pgrep, /bin/rm -R /zroot/bhyvemgr/*,
/usr/sbin/service dnsmasq restart, /sbin/zfs create * zroot/bhyvemgr/*, /sbin/zfs destroy * zroot/bhyvemgr/*
```
For doas if user is part of wheel group
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
### PF configuration

PF is used to bring packets filter and NAT features to our local environment. Take a look at the following PF configuration sample. In this guide **em0** is used like a local interface and **10.0.0.0/24** subnet for virtual machines. **10.0.0.1** is the ip address used by **bhyve0** bridge and this is where dnsmasq is listening. Change it according your own needs.

```sh
ext_if="em0"

tcp_services={ ssh, http, https, ntp, domain}
udp_services={ ntp, domain }
ksm_service="1688"

set block-policy return
scrub in on $ext_if all fragment reassemble
set skip on lo

nat on $ext_if from 10.0.0.0/24 to any -> ($ext_if:0)

block in all
pass out quick keep state
antispoof for $ext_if inet

pass in inet proto icmp all icmp-type echoreq
pass out inet proto icmp all icmp-type echoreq

pass in inet proto tcp from 10.0.0.0/24 to any port $tcp_services flags S/SA keep state
pass in inet proto tcp from 10.0.0.0/24 to any port $udp_services flags S/SA keep state
pass in inet proto tcp from 10.0.0.0/24 to any port $ksm_service flags S/SA keep state
pass in inet proto tcp from 10.0.0.1 to any port bootps flags S/SA keep state
pass in inet proto tcp from 10.0.0.1 to any port bootpc flags S/SA keep state

pass in quick on bhyve0 proto udp from port bootpc to port bootps keep state
pass out quick on bhyve0 proto udp from port bootps to port bootps keep state

pass out quick on $ext_if inet proto { tcp udp } from any to any
```
With this best configuration, bhyvemgr will add an entry for each virtual machine to dnsmasq, dnsmasq will use this data for assign network configuration automatically, this will provide a dns service to resolv each virtual machine name or subdmomain. Virtual machines traffic will be management by packet filter rules.

# Run bhyvemgr for the first time
When bhyvemgr starts in the first time, this will create a initial config file. It is mandatory to review, modify (if it is necessary) and press **Save settings** button from of **Settings form** the first time

![image](https://github.com/user-attachments/assets/35b0ea78-8449-4c08-bc13-c09977e0c30a)

![image](https://github.com/user-attachments/assets/b57fd1af-0a19-49c9-a0a5-547ff6cc7033)

# Demo

https://github.com/user-attachments/assets/4e761b97-926c-4bc6-8941-3897359e07e6

Enjoy creating and testing your virtual machines on FreeBSD


