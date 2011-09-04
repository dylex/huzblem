CREATE TABLE browse (
	id	serial PRIMARY KEY,
	uri	text UNIQUE NOT NULL,
	last	timestamp (0) NOT NULL DEFAULT now(),
	visits	integer NOT NULL DEFAULT 1
);

CREATE OR REPLACE FUNCTION browse_add(text) RETURNS integer LANGUAGE plpgsql STRICT AS
$$
DECLARE
	u ALIAS FOR $1;
	i INTEGER;
BEGIN
	LOOP
		UPDATE browse SET visits = visits + 1, last = now() WHERE uri = u RETURNING id INTO i;
		IF NOT found THEN
			BEGIN
				INSERT INTO browse (uri) VALUES (u) RETURNING id INTO i;
			EXCEPTION WHEN unique_violation THEN
				CONTINUE;
			END;
		END IF;
		RETURN i;
	END LOOP;
END;
$$
;

CREATE TABLE block (
	host	domainname PRIMARY KEY,
	trust	bool
);
