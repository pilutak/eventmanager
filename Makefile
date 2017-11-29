PROJECT = em
PROJECT_DESCRIPTION = Middleware for BW/IMS provisioning
PROJECT_VERSION = 0.1.0

PROJECT_ENV = [{bw_hosts,["any.host.com"]},{ema,{"http://10.8.10.132:8998","sogadm","sogadm"}}]

include erlang.mk
