default: huzblem init.min.js all pguri.sql

huzblem:
	ghc --make -dynamic -threaded -O -Wall -fno-warn-tabs $@.hs -i -i. -o $@
%.min.js: %.js
	uglifyjs -o $@ $<

hlint:
	hlint -c .

.PHONY: huzblem hlint
EXTRA_CLEAN = *.o *.hi huzblem

MODULES = pguri
PG_CONFIG = pg_config
PGXS := $(shell pg_config --pgxs)
include $(PGXS)
%.sql: %.sql.in
	sed 's,MODULE_PATHNAME,$(CURDIR)/$*,g' $< >$@
