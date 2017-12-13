CREATE OR REPLACE FUNCTION reset_idseq(t varchar) RETURNS integer AS $$
DECLARE
	max_id integer DEFAULT 0;
	next_id integer DEFAULT 0;
	seq_name varchar := '';
BEGIN
	RAISE NOTICE 'table is %', t;

	EXECUTE 'SELECT max(id) FROM '
	|| t::regclass
	INTO max_id 
	USING t;

	RAISE NOTICE 'max_id is %', max_id;

	seq_name = t::regclass || '_id_seq';

	EXECUTE 'ALTER SEQUENCE ' 
	|| seq_name 
	|| ' RESTART WITH '
	|| max_id;

	SELECT nextval(seq_name) INTO next_id;

	RETURN next_id;
END;
$$ LANGUAGE plpgsql;

SELECT reset_idseq('article_content');
