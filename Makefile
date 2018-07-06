PROJECT = em
PROJECT_DESCRIPTION = Middleware for BW/IMS provisioning
PROJECT_VERSION = 0.2.30

DEPS = lager jsx epgsql elli econfig
PROJECT_ENV = [{lager, [{log_root, "../log"},{handlers, [{lager_console_backend, info},{lager_file_backend, [{file, "error.log"}, {level, error}]},{lager_file_backend, [{file, "console.log"}, {level, info}]}]}]}]
ERLC_OPTS += +'{parse_transform, lager_transform}'

include erlang.mk

