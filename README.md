<div align="center">

# Pythia

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/pythia?include_prereleases&sort=semver)](https://github.com/tbidne/pythia/releases/)
[![ci](http://img.shields.io/github/actions/workflow/status/tbidne/pythia/ci.yaml?branch=main)](https://github.com/tbidne/pythia/actions/workflows/ci.yaml)
[![MIT](https://img.shields.io/github/license/tbidne/pythia?color=blue)](https://opensource.org/licenses/MIT)

</div>

---

### Table of Contents

* [Introduction](#introduction)
* [Services](#services)
  * [Battery](#battery)
  * [Global IP](#global-ip)
  * [Memory](#memory)
  * [Network Connection](#network-connection)
  * [Network Interfaces](#network-interfaces)
  * [Time](#time)

# Introduction

Pythia is a tool for retrieving system information. It is both a library and an executable. Pythia's functionality is based on the concept of using existing tools one has on their system. For example, to read battery information, you can tell `pythia` to use the `upower` utility. Pythia will then execute a `upower` command, parse its output, then return that data to the user.

## Configuration

Pythia is configured via CLI args or a possible TOML file. We automatically look in the XDG config directory e.g. `~/.config/pythia/config.toml` and use it if it exists. The path to the TOML file can also be specified manually.

CLI Args override TOML in case the same option is specified in both.

# Services

This section describes services from the perspective of the executable. The library API is close, but there are some discrepancies, usually where the executable has extra functionality.

## Battery

This service is for reading battery information.

### Usage

```
Usage: pythia battery [-a|--app (acpi | sysfs | upower)]
                      [-f|--field (default | percentage | status)]

  Queries the battery state.


Available options:
  -a,--app (acpi | sysfs | upower)
                           Specifies the app to use.

  -f,--field (default | percentage | status)
                           If specified, prints only the given field.

  -h,--help                Show this help text
```

### Supported Apps

The following applications are supported:

* `acpi`
* `sysfs`: Reading `/sys/class/power_supply/BAT[N]` directly.
* `upower`

By default, pythia returns both the charging status and percentage. The `--field` option allows one to further refine this.

### Examples

```
$ pythia battery
Discharging: 76%

$ pythia battery --app acpi
Charging: 42%

$ pythia battery --app acpi --field status
Discharging

$ pythia battery --app acpi --field percentage
75%
```

## Global IP

This service is for reading our global IP address.

### Usage

```
Usage: pythia global-ip [-a|--app (curl | dig)]
                        [-f|--field (ipv4 | ipv6 | both)] [--ipv4-src URL]
                        [--ipv6-src URL]

  Queries the global IP addresses.


Available options:
  -a,--app (curl | dig)    Specifies the app to use.

  -f,--field (ipv4 | ipv6 | both)
                           Whether to retrieve IPv4 or IPv6 address. Defaults to
                           ipv4.

  --ipv4-src URL           Custom server URL for retrieving the IPv4 address
                           e.g. http://whatismyip.akamai.com/. Can be specified
                           multiple times and overrides the defaults. These
                           sources are only used if we query for IPv4 per
                           --field.

  --ipv6-src URL           Custom server URL for retrieving the IPv6 address.
                           Can be specified multiple times and overrides the
                           defaults. These sources are only used if we query for
                           IPv6 per --field.

  -h,--help                Show this help text
```

### Supported Apps

The following applications are supported:

* `curl`
* `dig`

The `--field` flag can be used to specify which IP addresses we want to retrieve. It defaults to `ipv4` only.

Because we have to reach out to an external server to get this IP address, pythia provides a list of default sources to use. These can be overridden by using the `--ipv4-src` and `--ipv6-src` option. Note: there are currently no default `ipv6` sources. Thus if you want to retrieve your global ipv6 address (i.e. `--field` with either `--ipv6` or `--both`), you must supply them yourself with `--ipv6-src`.

### Examples

```
$ pythia global-ip --app curl
165.52.200.7

$ pythia global-ip --app curl --ipv4-src http://whatismyip.akamai.com/
165.52.200.7

$ pythia global-ip --app dig --ipv4-src "@resolver1.opendns.com myip.opendns.com"
165.52.200.7
```

## Memory

This service is for reading memory usage.

### Usage

```
Usage: pythia memory [-a|--app (free)] [-f|--field (free | total | used)]
                     [-u|--units (bytes | percentage)]

  Queries memory usage.


Available options:
  -a,--app (free)          Specifies the app to use.

  -f,--field (free | total | used)
                           If specified, prints only the given field. Default to
                           total.

  -u,--units (bytes | percentage)
                           Units to use.

  -h,--help                Show this help text
```

### Supported Apps

The following applications are supported:

* `free`

By default, we return the total and used memory. This can be refined with `--field`.

### Examples

```
$ pythia memory
6.75 G / 16.57 G

$ pythia memory --app free --percentage
39 / 100%

$ pythia memory --app free --field total
16.57 G

$ pythia memory --app free --field used
6.71 G

$ pythia memory --app free --field used --percentage
40%

$ pythia memory --app free --field free
9.87 G

$ pythia memory --app free --field free --percentage
60%
```

## Network Connection

This service is for finding a network interface that represents a live connection. As such it closely resembles [network interfaces](#network-interfaces), except we filter on an active connection.

### Usage

```
Usage: pythia net-conn [-a|--app (ip | nmcli)]
                       [-f|--field (device | type | name | ipv4 | ipv6)]

  Queries network interfaces for a live connection.


Available options:
  -a,--app (ip | nmcli)    The app to use.

  -f,--field (device | type | name | ipv4 | ipv6)
                           If specified, prints only the given field.

  -h,--help                Show this help text
```

### Supported Apps

The following applications are supported:

* `ip`
* `nmcli`

`pythia` will return the first interface that has state "up". The return fields can be refined via `--field`.

### Examples

```
$ pythia net-conn --app nmcli
Device: wlp0s20f3
Type: Wifi
State: Up
Name: MySSID
IPv4: 192.168.1.2
IPv6: fe80::a328:482:5263:10b8

$ pythia net-conn --app nmcli --field name
MySSID

$ pythia net-conn --app nmcli --field ipv4
192.168.1.2
```

## Network Interfaces

This service is for reading information about the network interfaces found on this system.

### Usage

```
Usage: pythia net-if [-a|--app (ip | nmcli)] [-d|--device (none | NAME)]
                     [-f|--field (name | ipv4 | ipv6)]

  Queries network interfaces.


Available options:
  -a,--app (ip | nmcli)    The app to use.

  -d,--device (none | NAME)
                           The name of the network device to filter on e.g.
                           wlp0s20f3. The string 'none' explicitly opts out of
                           filtering (the default).

  -f,--field (name | ipv4 | ipv6)
                           If specified, prints only the given field.

  -h,--help                Show this help text
```

### Supported Apps

The following applications are supported:

* `ip`
* `nmcli`

`nmcli` should be preferred, as it returns more information. By default, `pythia` returns a list of all devices found on the system. This can be restricted to a single device by using the `--device` option. Furthermore, the return fields can be refined via `--field`.

### Examples

```
$ pythia net-if --app nmcli

Device: wlp0s20f3
Type: Wifi
State: Up
Name: MySSID
IPv4: 192.168.1.2
IPv6: fe80::fe44:82ff:fede:f814, fe80::a328:482:5263:10b8

Device: enp0s31f6
Type: Ethernet
State: Down
Name:
IPv4:
IPv6:

Device: lo
Type: Loopback
State: UnknownState "unmanaged"
Name:
IPv4: 127.0.0.1
IPv6: ::1

Device: vpn
Type: Tun
State: UnknownState "unmanaged"
Name:
IPv4:
IPv6: fe80::a63f:791a:3eaa:9d86

$ pythia net-if  --app nmcli --device wlp0s20f3
Type: Wifi
State: Up
Name: MySSID
IPv4: 192.168.1.2
IPv6: fe80::fe44:82ff:fede:f814, fe80::a328:482:5263:10b8

$ pythia net-if  --app nmcli --device wlp0s20f3 --field name
MySSID

$ pythia net-if --app nmcli --device wlp0s20f3 --field ipv4
192.168.1.2

$ pythia net-if --app nmcli --field ipv6
192.168.1.2fe80::fe44:82ff:fede:f814, fe80::a328:482:5263:10b8


::1
fe80::a63f:791a:3eaa:9d86
```

## Time

This service is for reading the current system time.

### Usage

```
Usage: pythia time [-d|--dest (local | utc | TZ)]
                   [-f|--format (default | FMT_STR)]

  Queries the system time.


Available options:
  -d,--dest (local | utc | TZ)
                           Determines what timezone we return. If none is given
                           we assume local time. If given, must be one of [utc |
                           TZ] where TZ is a tz database label e.g.
                           America/New_York. See
                           https://en.wikipedia.org/wiki/Tz_database.

  -f,--format (default | FMT_STR)
                           Glibc-style format string e.g. %Y-%m-%d for
                           yyyy-mm-dd. Defaults to RFC822. See
                           https://hackage.haskell.org/package/time-1.13/docs/Data-Time-Format.html#v:formatTime.

  -h,--help                Show this help text
```

### Examples

```
# get current local time
$ pythia time
Tue, 12 Jul 2022 23:19:00 NZST

# get current time and convert
$ pythia time -d America/New_York
Tue, 12 Jul 2022 07:19:28 EDT

# use glibc-style format
$ pythia time -f %H:%M
23:20
```
