PROJECT = em
PROJECT_DESCRIPTION = Middleware for BW/IMS provisioning
PROJECT_VERSION = 0.1.0

PROJECT_ENV = [{bw_hosts,["any.host.com"]},{ema,{"http://10.8.10.132:8998","sogadm","sogadm"}},{vmail,{"http://10.8.10.8:7026","admin","ics5ics5"}}]

DEPS = jsx epgsql elli

DEPS = elli
DEPS = jsx
DEPS = epgsql

include erlang.mk
