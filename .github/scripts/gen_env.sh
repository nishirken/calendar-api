#! /usr/bin/env bash

ssh -i ~/.ssh/gandi_key $USERNAME@$HOST "
  cd /var/calendar-api
  touch .env
  echo POSTGRES_USER=postgres > .env
  echo POSTGRES_DB=calendar >> .env
  echo POSTGRES_PASSWORD=$PASS >> .env
  echo AUTH_KEY=$KEY >> .env
  echo DB_PORT=5555 >> .env
  echo DB_HOST=postgres >> .env
  echo APP_PORT=8081 >> .env
  echo APP_ORIGIN=https://46.226.104.150 >> .env
"

