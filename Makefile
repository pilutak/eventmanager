PROJECT = em
PROJECT_DESCRIPTION = Middleware for BW/IMS provisioning
PROJECT_VERSION = 0.2.26

DEPS = jsx epgsql elli cai3g
dep_cai3g = git https://github.com/telsgaard/cai3g.git

include erlang.mk
