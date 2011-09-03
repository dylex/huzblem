default: huzblem all pguri.sql

huzblem:
	ghc --make -threaded -O -Wall $@.hs -i -i. -o $@
.PHONY: huzblem

EXTRA_CLEAN = *.o *.hi huzblem

MODULES = pguri
PG_CONFIG = pg_config
PGXS := $(shell pg_config --pgxs)
include $(PGXS)
%.sql: %.sql.in
	sed 's,MODULE_PATHNAME,$(CURDIR)/$*,g' $< >$@
