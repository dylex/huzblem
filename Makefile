default: huzblem init.jss all pguri.sql

huzblem:
	ghc --make -dynamic -threaded -O -Wall -fno-warn-tabs $@.hs -i -i. -o $@
%.min.js: %.js
	curl -Ss --data-urlencode "js_code@$<" -o $@ "http://marijnhaverbeke.nl/uglifyjs"

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
