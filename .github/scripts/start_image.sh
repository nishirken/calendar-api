#! /bin/bash

scp -i ~/.ssh/gandi_key ci/$COMPOSE_FILE $USERNAME@$HOST:/var/calendar-api/$COMPOSE_FILE

ssh -i ~/.ssh/gandi_key $USERNAME@$HOST "
  podman login registry.gitlab.com/nishirken/calendar-api --username nishirken --password $TOKEN
  TAG_APP=$TAG_APP TAG_MIGRATIONS=$TAG_MIGRATIONS podman-compose -f /var/calendar-api/$COMPOSE_FILE up -d --force-recreate
"
