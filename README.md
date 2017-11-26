# eventmanager

### How to run:

```
erl -pa ebin -mnesia dir '"/var/lib/eventmanager"'
application:start(inets).
application:start(em).
```