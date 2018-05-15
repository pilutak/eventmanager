# eventmanager
[![Build Status](https://travis-ci.org/timezone4/eventmanager.svg)](https://travis-ci.org/timezone4/eventmanager)

### How to run:

```
erl -pa ebin -mnesia dir '"/var/lib/eventmanager"'
application:start(inets).
application:start(em).
```
