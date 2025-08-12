# Bhyvemgr
Bhyvemgr is a bhyve management GUI written in Freepascal/Lazarus on FreeBSD. It needs a bunch of tools mostly installed on base system and some installed from ports/packages. Currently it supports amd64 and aarch64. The main goal is to be a desktop user application to easily and quickly setup and run virtual machines on the FreeBSD host.

<img width="832" height="698" alt="image" src="https://github.com/user-attachments/assets/f5b2bf8e-4b16-401b-a914-94cea2a4bd72" />

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
- and more

# TODO
- Allow change VM (zfs/ufs) directory to custom ones

# Bhyvemgr dependencies
## From base system
bhyve, bhyvectl, bhyveload, chown, chmod, fetch, file, ifconfig, install, kill, kldload, kldstat, makefs, pciconf, pgrep, rm, service, sysctl, truncate, xz, zfs and zpool
## From ports/packages
bhyve-firmware (sysutils/bhyve-firmware), doas (security/doas), qemu-tools (emulatorsd/qemu@tools), remote-viewer (net-mgmt/virt-viewer), swtpm (sysutils/swtpm), sudo (security/sudo), and xfreerdp3 (net/freerdp3)

# Network configuration
bhyvemgr can use two kind of network settings: *Quick network configuration* or *Optimal network configuration*. Choose one of them accord to your own needs. I recommend second one because it permits a complete network management of virtual machines.

## Quick network configuration
If you want use bhyve without many network features, you can create a bridge and add your ethernet interface to it. Keep in mind that you will need a DHCP/DHCPv6 server, SLAAC or a router with prefix delegation activated in your network environment if you want the virtual machine's network configuration to be assigned automatically. Otherwise, you'll need to manually configure the network settings for each virtual machine.

Add the following lines to your **/etc/rc.conf** file:

```sh
cloned_interfaces="bridge0"
ifconfig_bridge0_name="bhyve0"
ifconfig_bhyve0="addm em0 up"
ifconfig_bhyve0_descr="bhyve manager bridge"
```

If you want activate IPv6 support, add the following line:

```sh
ifconfig_em0_ipv6="inet6 accept_rtadv auto_linklocal"
```

Bhyvemgr add each tap/vtnet interface to **bhyve0** bridge when a virtual machine is started. The same way it deletes and removes tap interface when a virtual machine is stopped.

## Optimal network configuration
On another hand, if you want use bhyve with a better network features (DHCPD and DNS features) including NAT support, you need configure some additional services like dnsmasq and packet filter. Create a bridge and assign an IPv4 address to it. This will be used like a gateway by each virtual machine. A subnet **10.0.0.0/24** will be used in this guide.

```sh
# ee /etc/rc.conf
```
```sh
gateway_enable="YES"
cloned_interfaces="bridge0"
ifconfig_bridge0_name="bhyve0"
ifconfig_bhyve0="inet 10.0.0.1 netmask 255.255.255.0"
ifconfig_bhyve0_descr="bhyve manager bridge"
```

If you want include IPv6 support we need to a IPv6 address (Unique Local Address) and some other configuration to your bridge interface. The best way to calculate a bhyve0 IPv6 address is from **Bhyve Manager Settings** window.

<img width="789" height="293" alt="image" src="https://github.com/user-attachments/assets/73e6b683-ad88-4753-8b24-6d713f08d73b" />

You need two things to calcule bhyve0 IPv6 Address: **bhyve0 MAC Adresss** and an **IPv6 prefix**. Keep in mind the IPv6 prefix will also be used in your Dnsmasq configuration later. 

**IPv6 prefix** can be generated or calculated from **Bhyve Manager Settings** and this value must be saved to bhyvemgr configuration file because it is used to calculate virtual machines IPv6 address of each first network interface.

**bhyve0 MAC Address** can be obtained from **ifconfig bhyve0** output.

```sh
# ifconfig bhyve0 | grep ether
	ether 38:7c:fc:00:c6:11
```

In this guide, I will use **fd4d:39f0:0d6b:0001::** as IPv6 prefix and **38:7c:fc:00:c6:11** as bhyve0 MAC Address. Bhyvemgr will calculate **fd4d:39f0:0d6b:0001:3a7c:fcff:fe00:c611** (EUI-64 format) with these two values and it must be used as **bhyve0 IPv6 Address** into **/etc/rc.conf** file.

Add the following lines to your **/etc/rc.conf** file:

```sh
ifconfig_bhyve0_ipv6="inet6 fd4d:39f0:0d6b:0001:3a7c:fcff:fe00:c611 prefixlen 64 autoconf accept_rtadv auto_linklocal"
```

The bhyve0 IPv6 address **fd4d:39f0:0d6b:0001:3a7c:fcff:fe00:c611** will also be used in your Dnsmasq configuration file.

### Dnsmasq

Dnsmasq is used for bring DHCP, Router Advertisements and DNS features to our virtual machine network environment. bhyvemgr will add *dhcp-host* and *host-record* entries to dnsmasq configuration (IPv4 address, IPv6 address resolution, MAC and VM name) when a virtual machine is created.

Create some dnsmasq directories for store virtual machine config files. In this sample, I am using **acm** like my bhyvemgr user

```sh
# install -d -m 770 -o root -g wheel /usr/local/etc/dnsmasq.d
# install -d -m 770 -o acm /usr/local/etc/dnsmasq.d/bhyvemgr
```
The following is the minimal dnsmasq configuration required.

```sh
# ee /usr/local/etc/dnsmasq.conf
```
```sh

# General configuration

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

# IPv4 configuration

dhcp-range=10.0.0.0,static,255.255.255.0,5m
dhcp-option=option:router,10.0.0.1
dhcp-option=option:dns-server,10.0.0.1

# IPv6 configuration

enable-ra
dhcp-range=fd4d:39f0:0d6b:0001::,slaac,64,5m
dhcp-option=option6:dns-server,[fd4d:39f0:0d6b:0001:3a7c:fcff:fe00:c611]

# Load configuration files from bhyvemgr directory

conf-dir=/usr/local/etc/dnsmasq.d/bhyvemgr/,*.conf
```
Add the IP address **10.0.0.1** to the **/etc/resolv.conf** file if you want each virtual machine name or subdomain (e.g., **fbsd15x64** or **fbsd15x64.bsd.lan**) to be resolved from the FreeBSD host.

```sh
# ee /etc/resolv.conf
```
```sh
nameserver 10.0.0.1
```
Don't forget to add the following line to your **/etc/rc.conf** file:

```sh
dnsmasq_enable="YES"
```

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
/usr/sbin/install -d *, /bin/kill -SIGTERM *, /sbin/kldload, /usr/bin/pgrep, /bin/rm -R /zroot/bhyvemgr/*,
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
permit nopass :wheel as root cmd pgrep
permit nopass :wheel as root cmd rm
permit nopass :wheel as root cmd service
permit nopass :wheel as root cmd zfs
```
### PF configuration

PF is used to bring packets filter and NAT features to our local environment. In this guide **em0** is used like a local interface and **10.0.0.0/24** subnet for virtual machines. **10.0.0.1** is the ip address used by **bhyve0** bridge and this is where dnsmasq is listening. Change it according your own needs. Take a look at the following PF configuration sample:

```sh
# ee /etc/pf.conf
```
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

pass in quick on bhyve0 proto icmp6
pass in quick on bhyve0 proto udp from any port dhcpv6-client to any port dhcpv6-server
pass out quick on bhyve0 proto udp from any port dhcpv6-server to any port dhcpv6-client

pass out quick on $ext_if inet proto { tcp udp } from any to any
```

Don't forget to add the following line to your **/etc/rc.conf** file:

```sh
pf_enable="YES"
```

With this optimal configuration, bhyvemgr will add an entry for each virtual machine to dnsmasq. dnsmasq will then use this data to automatically assign network configuration. It will also provide DNS service to resolve each virtual machine's name or subdomain. Virtual machine traffic will be managed by packet filter (PF) rules.

# Run bhyvemgr for the first time
When bhyvemgr starts in the first time, this will create a initial config file. It is mandatory to review, modify (if it is necessary) and press **Save settings** button from of **Settings form** the first time

![image](https://github.com/user-attachments/assets/35b0ea78-8449-4c08-bc13-c09977e0c30a)

<img width="811" height="720" alt="image" src="https://github.com/user-attachments/assets/d7e70bec-df74-413a-928d-6b9a0215f897" />

# Demo

https://github.com/user-attachments/assets/4e761b97-926c-4bc6-8941-3897359e07e6

Enjoy creating and testing your virtual machines on FreeBSD


