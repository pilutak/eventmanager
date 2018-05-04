PROJECT = em
PROJECT_DESCRIPTION = Middleware for BW/IMS provisioning
PROJECT_VERSION = 0.2.7


PROJECT_ENV = [{role, "master"},{service_profiles, [{virtual, "IMT_VIRTUAL"},{user, "IMS_CENTREX"},{trunk_pilot, "BusinessTrunk"},{trunk_ddi, "BusinessTrunk_wild"}]},{em_db, [{hostname, "127.0.0.1"},{database, "db1"},{username, "user"},{password, "pass"}]},{em_srd, [{hostname, "127.0.0.1"},{database, "db1"},{username, "user"},{password, "pass"}]},{em_ema, [{primary, [{hostname, "127.0.0.1"},{port, "8080"},{url, "/CAI3G1.2/services/CAI3G1.2"},{username, "user"},{password, "pass"}]},{secondary, [{hostname, "127.0.0.1"},{port, "8080"},{url, "/CAI3G1.2/services/CAI3G1.2"},{username, "user"},{password, "pass"}]}]},{em_surgemail, [{domain_password, "pass"},{primary, [{hostname, "127.0.0.1"},{port, "7026"},{username, "user"},{password, "pass"}]},{secondary, [{hostname, "127.0.0.1"},{port, "7026"},{username, "user"},{password, "pass"}]}]},{bw_hosts,["127.0.0.1"]}]

DEPS = jsx epgsql elli
dep_epgsql = git https://github.com/epgsql/epgsql.git 3.4.0

include erlang.mk
