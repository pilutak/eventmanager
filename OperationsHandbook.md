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
