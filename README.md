# eventmanager

###How to run:

```
erl -pa ebin -mnesia dir '"/var/lib/eventmanager"'
application:start(sasl).
application:start(mnesia).
application:start(em).
```