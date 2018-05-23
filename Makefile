PROJECT = em
PROJECT_DESCRIPTION = Middleware for BW/IMS provisioning
PROJECT_VERSION = 0.2.10


PROJECT_ENV = [{role, "master"},{service_profiles, [{virtual, "IMT_VIRTUAL"},{user, "IMS_CENTREX"},{trunk_pilot, "BusinessTrunk"},{trunk_ddi, "BusinessTrunk_wild"}]},{em_db, [{hostname, "172.16.159.151"},{database, "srd"},{username, "srd"},{password, "srd"}]},{em_srd, [{hostname, "172.16.159.151"},{database, "srd"},{username, "srd"},{password, "srd"}]},{em_ema, [{primary, [{hostname, "127.0.0.1"},{port, "8080"},{url, "/CAI3G1.2/services/CAI3G1.2"},{username, "user"},{password, "pass"}]},{secondary, [{hostname, "127.0.0.1"},{port, "8080"},{url, "/CAI3G1.2/services/CAI3G1.2"},{username, "user"},{password, "pass"}]}]},{em_surgemail, [{domain_password, "pass"},{primary, [{hostname, "127.0.0.1"},{port, "7026"},{username, "user"},{password, "pass"}]},{secondary, [{hostname, "127.0.0.1"},{port, "7026"},{username, "user"},{password, "pass"}]}]},{bw_hosts,["127.0.0.1"]}]

DEPS = jsx epgsql elli

include erlang.mk
