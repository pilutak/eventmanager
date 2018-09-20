# eventmanager


## Installation:
Installation path
```
/opt/em

```

Configuration file:
```
/etc/em/em.ini
```

## Stop / Start:
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
