db_start:
	./scripts/pg_start.sh

api:
	make db_start && cabal run calendar-api

migrations:
	cabal run calendar-api-migrations

