#!/usr/bin/env bash

PORT=$(cat $PWD/config.json | jq '.dbPort')

if pg_ctl -D $PGDIR status >/dev/null
then
  echo "Already started"
else
  pg_ctl -D $PGDIR -l logs/psql -o "-p $PORT --unix_socket_directories='$PWD'" start
fi


