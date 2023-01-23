#! /bin/bash

scp -i ~/.ssh/gandi_key ci/$COMPOSE_FILE $USERNAME@$HOST:/var/calendar-api/$COMPOSE_FILE

ssh -i ~/.ssh/gandi_key $USERNAME@$HOST "
  export TAG_APP=$TAG_APP
  export TAG_MIGRATIONS=$TAG_MIGRATIONS
 
  echo 'Logging in'
  podman login registry.gitlab.com/nishirken/calendar-api --username nishirken --password N 2>\$1  | grep Error && exit 1
  echo 'Loggin succeeded'

  podman-compose -f /var/calendar-api/$COMPOSE_FILE up -d --force-recreate 2>\$1 | egrep 'Exit code..[1-9]' && exit 1
  echo 'Running containers succeeded'
"
