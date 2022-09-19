#!/usr/bin/env bash

if pg_ctl -D $PGDIR status >/dev/null
then
  echo "Already started"
else
  pg_ctl -D $PGDIR -l logs/psql -o "-p 5555 --unix_socket_directories='$PWD'" start
fi


