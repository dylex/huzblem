CREATE TABLE browse (
	id	serial PRIMARY KEY,
	uri	uri UNIQUE NOT NULL,
	title	text,
	last	timestamp (0) NOT NULL DEFAULT now(),
	visits	integer NOT NULL DEFAULT 1
);
CREATE INDEX browse_domain_idx ON browse (((uri).domain));

CREATE TABLE mark (
	id	serial PRIMARY KEY,
	uri	uri UNIQUE NOT NULL,
	follow	boolean NOT NULL,
	browse	integer REFERENCES browse
);
CREATE INDEX mark_domain_idx ON mark (((uri).domain));

CREATE OR REPLACE FUNCTION browse_add(uri, text) RETURNS integer LANGUAGE plpgsql STRICT AS
$$
DECLARE
	u ALIAS FOR $1;
	t ALIAS FOR $2;
	i INTEGER;
BEGIN
	LOOP
		UPDATE browse SET visits = visits + 1, title = t, last = now() WHERE uri = u RETURNING id INTO i;
		IF NOT found THEN
			BEGIN
				INSERT INTO browse (uri, title) VALUES (u, t) RETURNING id INTO i;
			EXCEPTION WHEN unique_violation THEN
				CONTINUE;
			END;
		END IF;
		UPDATE mark SET browse = i WHERE uri = u OR follow AND uri @> u;
		RETURN i;
	END LOOP;
END;
$$
;

CREATE OR REPLACE FUNCTION mark_add(uri, boolean) RETURNS integer LANGUAGE plpgsql STRICT AS
$$
DECLARE
	u ALIAS FOR $1;
	f ALIAS FOR $2;
	i INTEGER;
BEGIN
	SELECT id INTO i FROM browse WHERE uri = u OR f AND uri <@ u ORDER BY last DESC;
	INSERT INTO mark (uri, follow, browse) VALUES (u, f, i) RETURNING id INTO i;
	RETURN i;
END;
$$
;
