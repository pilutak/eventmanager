PROJECT = em
PROJECT_DESCRIPTION = Middleware for BW/IMS provisioning
PROJECT_VERSION = 0.1.0

PROJECT_ENV = [{pg_host,"xxxx"},{bw_hosts,["xxxx"]},{ema,{"http://xxxx:8080/CAI3G1.2/services/CAI3G1.2","xxxx","xxxx"}},{vmail,{"http://xxxx:7026","username","password"}}]

DEPS = jsx epgsql elli

include erlang.mk
