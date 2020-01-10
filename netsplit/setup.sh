#!/usr/bin/env bash

set -ex

docker network create ab
docker network create ac
docker network create bc

# Node a should be in both ab & ac network
docker run -d --network ab --name nodea \
  -p "5001:5672" -p "15001:15672" \
  -e RABBITMQ_NODENAME="rabbit@nodea" \
  -e RABBITMQ_ERLANG_COOKIE="netsplit" \
  rabbitmq:alpine

docker network connect ac nodea

# Node b should be in both ab & bc network
docker run -d --network ab --name nodeb \
  -p "5002:5672" -p "15002:15672" \
  -e RABBITMQ_NODENAME="rabbit@nodeb" \
  -e RABBITMQ_ERLANG_COOKIE="netsplit" \
  rabbitmq:alpine

docker network connect bc nodeb

# Node c should be in both ac & bc network
docker run -d --network ac --name nodec \
  -p "5003:5672" -p "15003:15672" \
  -e RABBITMQ_NODENAME="rabbit@nodec" \
  -e RABBITMQ_ERLANG_COOKIE="netsplit" \
  rabbitmq:alpine

docker network connect bc nodec

# RabbitMQ needs a bit of time to start up
sleep 10

docker exec nodea rabbitmq-plugins enable rabbitmq_management
docker exec nodea rabbitmqctl stop_app
docker exec nodea rabbitmqctl reset
docker exec nodea rabbitmqctl start_app
docker exec nodea rabbitmqctl set_log_level error
docker exec nodea rabbitmqctl add_vhost netsplit
docker exec nodea rabbitmqctl set_permissions -p netsplit guest ".*" ".*" ".*"

docker exec nodeb rabbitmq-plugins enable rabbitmq_management
docker exec nodeb rabbitmqctl stop_app
docker exec nodeb rabbitmqctl reset
docker exec nodeb rabbitmqctl start_app
docker exec nodeb rabbitmqctl stop_app
docker exec nodeb rabbitmqctl join_cluster rabbit@nodea
docker exec nodeb rabbitmqctl start_app
docker exec nodeb rabbitmqctl set_log_level error

docker exec nodec rabbitmq-plugins enable rabbitmq_management
docker exec nodec rabbitmqctl stop_app
docker exec nodec rabbitmqctl reset
docker exec nodec rabbitmqctl start_app
docker exec nodec rabbitmqctl stop_app
docker exec nodec rabbitmqctl join_cluster rabbit@nodea
docker exec nodec rabbitmqctl start_app
docker exec nodec rabbitmqctl set_log_level error