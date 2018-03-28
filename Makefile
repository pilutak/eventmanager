PROJECT = em
PROJECT_DESCRIPTION = Middleware for BW/IMS provisioning
PROJECT_VERSION = 0.1.0

PROJECT_ENV = [{pg_host,"172.16.159.151"},{bw_hosts,["any.host.com"]},{ema,{"http://10.44.64.40:8080/CAI3G1.2/services/CAI3G1.2","provuser","User@123"}},{vmail,{"http://10.8.10.8:7026","username","password"}}]

DEPS = jsx epgsql elli

include erlang.mk
