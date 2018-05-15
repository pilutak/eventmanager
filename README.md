# eventmanager
[![Build Status](https://travis-ci.org/timezone4/eventmanager.svg)](https://travis-ci.org/timezone4/eventmanager)

A Broadworks OCI-R Client application that keep connections to one or more Broadsoft Broadworks application servers
, handles recieves OCI-R events, and provisions the Ericsson IMS core (HSS-FE and ENUM-FE).

This is work in progress. I need to write documentation before it us
usable, but as such the eventmanager should support provisioning of:

  - Users
  - Huntgroups
  - Auto attendants
  - Call centers
  - Meet-Me conferences
  - Voiceportals
  - Voicemails

I would love to get some feedback and help building this thing.

## Installation:
The eventmanager runs as a release
```
make rel run

```

Configuration is done in the sys.config file
```
[
    {em,
        [
        {role, "master"},
        {service_profiles, [
            {virtual, "Profile1"},
            {user, "Profile2"},
            {trunk_pilot, "Profile3"},
            {trunk_ddi, "Profile4"}
        ]},
        {em_db, [
            {hostname, "127.0.0.1"},
            {database, "alice"},
            {username, "alice"},
            {password, "alice"}
        ]},
        {em_srd, [
            {hostname, "127.0.0.1"},
            {database, "bob"},
            {username, "bob"},
            {password, "bob"}
        ]},
        {em_ema, [
            {primary, [
                {hostname, "127.0.0.1"},
                {port, "8080"},
                {url, "/CAI3G1.2/services/CAI3G1.2"},
                {username, "alice"},
                {password, "alice"}
            ]},
            {secondary, [
                {hostname, "127.0.0.1"},
                {port, "8080"},
                {url, "/CAI3G1.2/services/CAI3G1.2"},
                {username, "alice"},
                {password, "alice"}
            ]}
        ]},
        {em_surgemail, [
            {domain_password, "alice"},
            {primary, [
                {hostname, "127.0.0.1"},
                {port, "7026"},
                {username, "alce"},
                {password, "alice"}
            ]},
            {secondary, [
                {hostname, "127.0.0.1"},
                {port, "7026"},
                {username, "alice"},
                {password, "alice"}
            ]}
        ]},        
        {bw_hosts,["127.0.0.1"]}
        ]
    }
].

```
