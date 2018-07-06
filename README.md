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
trunk_pilot=
trunk_ddi=

```

# System Architecture

EM (Eventmanager) receives OCI-R events from the BW AS (Broadworks application server). OCI-R events are queued in the BW AS, and EM processes events synchronously one by one. 

The reader module extracts the commandtypem and performs a lookup to find a matching "processor". If no processor is found, the event is marked as "IGNORED", if the commandtype is found in the processor map, the event is initially marked as "PENDING".

A new process is spawned, and the event is processed by the events module. Based on the commandtype, following processors can be called:

- create_service
- modify_service
- delete_service
- create_user
- modify_user
- delete_user
- set_password
- create_trunk
- modify_group_vp
- modify_user_vm
- delete_group
- create_domain
- delete_domain

The events module extracts parameters from the event needed in order to apply business logic, and inserts the parameters into an event record, which depending on the processor type, is processed by an em_manager_* module:

- em_manager_hss
- em_manager_surgemail

Manager modules are reposinble for applying the event specific business logic. After applying business logic, the EM might send one or multiple request to the EMA/PG or surgemail nodes. If the event prcessing is successfull, the event status in the EM DB is updated to either: completed or failed.

# Status
EM currently support a limited subset of BW Rel 21 sp3 OCI-R events, and the Ericcson layered architecture.

# Tests

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
