PROJECT = em
PROJECT_DESCRIPTION = Middleware for BW/IMS provisioning
PROJECT_VERSION = 2.0.22

DEPS = jsx epgsql elli econfig
dep_jsx = git https://github.com/talentdeficit/jsx.git v2.9.0

include erlang.mk

