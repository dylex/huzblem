default: huzblem init.jss all pguri.sql

huzblem:
	ghc --make -dynamic -threaded -O -Wall $@.hs -i -i. -o $@
.PHONY: huzblem
jss: jss.hs
	ghc --make -O -Wall $< -i. -o $@
%.jss: %.js jss
	./jss < $< > $@

EXTRA_CLEAN = *.o *.hi huzblem

MODULES = pguri
PG_CONFIG = pg_config
PGXS := $(shell pg_config --pgxs)
include $(PGXS)
%.sql: %.sql.in
	sed 's,MODULE_PATHNAME,$(CURDIR)/$*,g' $< >$@
