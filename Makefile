PROJECT = em
PROJECT_DESCRIPTION = Middleware for BW/IMS provisioning
PROJECT_VERSION = 0.1.0

PROJECT_ENV = [{pg_host,"xxxx"},{bw_hosts,["any.host.com"]},{ema,{"http://xxxx:8080/CAI3G1.2/services/CAI3G1.2","xxxx","xxxx"}},{vmail,{"xxxx:7026","xxxx","xxxx"}}]

DEPS = jsx epgsql elli

include erlang.mk
