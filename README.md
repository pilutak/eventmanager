# eventmanager
[![Build Status](https://travis-ci.com/telsgaard/eventmanager.svg)](https://travis-ci.com/telsgaard/eventmanager)

A Broadworks OCI-R Client application that keep connections to one or more Broadsoft Broadworks application servers
where it recieves OCI-R events. Relevant business logic is executed, and provisioning request is send to the Ericsson IMS core (HSS-FE and ENUM-FE) via Ericsson EMA/PG. Some requestes is sent directly to the voicemail application. 

The EMA handles following Broadworks data objects
 
  - Users
  - Huntgroups
  - Auto attendants
  - Call centers (standard)
  - Meet-Me conferences
  - Voiceportals
  - Voicemails
  - BLF (Busy Lamp Field) 

## Installation:
Installation path
```
/opt/em

```

Configuration file:
```
/etc/em/em.ini
```

The eventmanager runs as a release
```
make rel run

```

Configuration is done in the /etc/em/em.ini file
```
[em_db]
em_db_host=
em_db_database=
em_db_username=
em_db_password=

[srd_db]
srd_db_host=
srd_db_database=
srd_db_username=
srd_db_password=

[ema]
primary_host=
secondary_host=
username=
password=

[surgemail]
domain_password=
primary_host=
secondary_host=
port=
username=
password=

[broadworks]
host=

[service_profiles]
virtual=
user=
pilot=
trunk=

[em]
loglevel=notice

```
## Stop / Start:
Systemdd config
```
[Unit]
After=network.target remote-fs.target
Requires=network.target remote-fs.target

[Service]
Type=simple
TimeoutStartSec=0
Restart=always
RestartSec=5
Environment=HOME=/opt/em
ExecStart=/opt/em/bin/em foreground
ExecStop=/opt/em/bin/em stop

[Install]
WantedBy=multi-user.target

````

Then EM can be stopped / started as:
```
systemctl stop em.service
systemctl start em.service

```

## EM Update / Upgrade:
```
systemctl stop em.service
cd /opt/em/
rm -rf *
cp /root/em-2.0.x.tgz .
tar -zxvf em-2.0.x.tgz 
systemctl start em.service

```

## logging:
The EM logfile:
```
/opt/em/log/erlang.log
```

Loglevel can be changed via the config file:
```
loglevel=notice
```

After updating the loglevel, EM must be restartet in order to activate,
Default loglevel is notice, for debugging use: debug


## Monitoring:
The EM instances can be monitored via the alivecheck:

```
curl http://{EM hostname / IP}:8080/alivecheck
```

Which returns 200 OK and: {"is_alive":true}

## Event Monitor:
The Event Monitor is a small java UI application which monitors the events. Event processing is classified into following catagories:

- Green   (Successfull)
- Yellow  (Pending)
- Red     (Failed)
- Grey    (Ignored) 

Green:  The event has been analyzed by EM, and processing was successfull. In most cases there will have been sent commands southbound to the network.

Yellow: The event is currently being processed. If an event stays in status pending, something is wrong.

Red:    The event processing has failed! This request should be handled manually.

Grey:   The event is ignored by EM (most likely because the event is not implemented). During normal operations, grey events are NOT shown in the Event Monitor!


## Scheduled operations tasks:
Daily: Look in the event monitor if any events has failed the last 24 hours.

Weekly: Look in the EM logfile if there is logged any error messages or crashes (on both EM nodes).

Monthly: Verify diskspace on application and DB servers, if needed, clean events from DB. Verify DB backups (make sure to have SRD backup). check health of both DB servers.

## Resolving Errors:
An event has been marked as failed in event monitor: Investigate why it failed, and resolve. In data is fixed manually in the core, the SRD data must be updated accordingly! 

EM does not receive any events from Broadworks: Restart EM, events has been queued in Broadworks, and should be processed when EM is restarted. Verify that EM is processing events.

Primary DB is DOWN: Update the IP / hostname of the DB server in the EM config file, restart EM. Resolve lost events manually!!

# System Architecture

EM (Eventmanager) receives OCI-R events from the BW AS (Broadworks application server). OCI-R events are queued in the BW AS, and EM processes events synchronously one by one. 

The reader module extracts the commandtypem and performs a lookup to find a matching "processor". If no processor is found, the event is marked as "IGNORED", if the commandtype is found in the processor map, the event is initially marked as "PENDING".

A new process is spawned, and the event is processed by the events module. Based on the commandtype, following processors can be called:

- em_domain
- em_user
- em_service
- em_voicemail

The event module extracts parameters from the event needed in order to apply business logic, and inserts the parameters into an event record, which depending on the processor type, is processed by an processor module

Processor modules are reposinble for applying the event specific business logic. After applying business logic, the EM might send one or multiple request to the EMA/PG or surgemail nodes. If the event prcessing is successfull, the event status in the EM DB is updated to either: completed or failed.


# Processing Logic

em_domain:
    Following OCI-R commands is calling the domain processor:

    SystemDomainAddRequest -> Creating domain in surgemail
    SystemDomainDeleteRequest -> Deleting domain in surgemail


em_user:
    Following OCI-R commands is calling the user processor:
    
    UserAddRequest17sp4 -> Add user in SRD, HSS
    UserModifyRequest17sp4 -> Modify user in SRD. ENUM, HSS
    UserDeleteRequest -> Delete user from SRD, ENUM, HSS
    UserAuthenticationModifyRequest -> Update SIP password in HSS
    GroupTrunkGroupAddInstanceRequest21 -> Add user as pilot user in SRD and HSS
    GroupDeleteRequest -> List all users from SRD (belonging to BW group), and delete from SRD, ENUM, HSS, SURGEMAIL
    GroupVoiceMessagingGroupModifyVoicePortalRequest -> Create virtual user in SRD, HSS, ENUM


em_service:
    Following OCI-R commands is calling the service processor:
    
    GroupAutoAttendantAddInstanceRequest20 -> Add user in SRD, HSS
    GroupAutoAttendantModifyInstanceRequest20 -> Modify user in SRD. ENUM, HSS
    GroupAutoAttendantDeleteInstanceRequest -> Delete user from SRD, ENUM, HSS
    GroupHuntGroupAddInstanceRequest20 -> Add user in SRD, HSS
    GroupHuntGroupModifyInstanceRequest -> Modify user in SRD. ENUM, HSS
    GroupHuntGroupDeleteInstanceRequest -> Delete user from SRD, ENUM, HSS
    GroupCallCenterAddInstanceRequest19 -> Add user in SRD, HSS
    GroupCallCenterModifyInstanceRequest19 -> Modify user in SRD. ENUM, HSS
    GroupCallCenterDeleteInstanceRequest -> Delete user from SRD, ENUM, HSS
    GroupMeetMeConferencingAddInstanceRequest19 -> Add user in SRD, HSS
    GroupMeetMeConferencingModifyInstanceRequest -> Modify user in SRD. ENUM, HSS
    GroupMeetMeConferencingDeleteInstanceRequest -> Delete user from SRD, ENUM, HSS
    

em_service:
    Following OCI-R commands is calling the vocemail processor:
    
    UserVoiceMessagingUserModifyAdvancedVoiceManagementRequest -> Modify user in SRD and surgemail (create, modify, delete) 
    
# Status
EM currently support a limited subset of BW Rel 21 sp3 OCI-R events, and the Ericsson layered architecture.

# Tests

Mosts test is integration tests via CT

Running all tests:

```
make ct
````

Run specific test:

```
make ct-groupAutoAttendantModifyInstanceRequest20
```

## License

Copyright 2018 Thomas Elsgaard

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
