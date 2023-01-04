#!/usr/bin/env bash
export $(cat .env | xargs)
export PGDIR=".tmp/calendardb"

[ ! -d $PGDIR ] && pg_ctl initdb -D $PGDIR

if pg_ctl -D $PGDIR status >/dev/null
then
  echo "Already started"
else
  # start db
  pg_ctl -D $PGDIR -l logs/psql -o "-p $DB_PORT --unix_socket_directories='$PWD'" start
  # create user if not exists
  if [ -z $(psql -p $DB_PORT -h $DB_HOST -U $POSTGRES_USER postgres -tXAc "SELECT 1 FROM pg_roles WHERE rolname='$POSTGRES_USER'")]
  then
    createuser -p $DB_PORT -h $DB_HOST --createdb $POSTGRES_USER
    echo "'$POSTGRES_USER' user has been created"
  fi
  # create db if not exists
  if [ -z $(psql -p $DB_PORT -h $DB_HOST -U $POSTGRES_USER -lqt | cut -d \| -f 1 | grep -qw $POSTGRES_DB) ]
    then
      createdb -p $DB_PORT -h $DB_HOST -U $POSTGRES_USER $POSTGRES_DB
      echo "Database '$POSTGRES_DB' has been created"
    fi
fi

