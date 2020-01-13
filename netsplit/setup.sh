#!/usr/bin/env bash

image=rabbitmq:3.8-alpine

function wait () {
  local node=$1

  while ! docker exec $node rabbitmqctl status &>/dev/null
  do
    echo "Waiting for $node"
    sleep 5s
  done
  echo "$node is running"
}

set -ex

# Create bridge networks
docker network create ab
docker network create ac
docker network create bc

# Node a should be in both ab & ac network
docker run -d --name nodea \
  -p "5001:5672" -p "15001:15672" \
  -e RABBITMQ_NODENAME="rabbit@nodea" \
  -e RABBITMQ_ERLANG_COOKIE="netsplit" \
  -v $PWD/rabbitmq.conf:/etc/rabbitmq/rabbitmq.conf \
  $image

docker network connect ab nodea
docker network connect ac nodea

# Node b should be in both ab & bc network
docker run -d --name nodeb \
  -p "5002:5672" -p "15002:15672" \
  -e RABBITMQ_NODENAME="rabbit@nodeb" \
  -e RABBITMQ_ERLANG_COOKIE="netsplit" \
  -v $PWD/rabbitmq.conf:/etc/rabbitmq/rabbitmq.conf \
  $image

docker network connect ab nodeb
docker network connect bc nodeb

# Node c should be in both ac & bc network
docker run -d --name nodec \
  -p "5003:5672" -p "15003:15672" \
  -e RABBITMQ_NODENAME="rabbit@nodec" \
  -e RABBITMQ_ERLANG_COOKIE="netsplit" \
  -v $PWD/rabbitmq.conf:/etc/rabbitmq/rabbitmq.conf \
  $image

docker network connect ac nodec
docker network connect bc nodec

# Start a load balancer for all amqp ports
docker run -d --name lb \
 -p 5000:5000 \
 --link nodea \
 --link nodeb \
 --link nodec \
 -v $PWD/gobetween.toml:/etc/gobetween/conf/gobetween.toml \
 yyyar/gobetween

# RabbitMQ need some time to startup
wait nodea
wait nodeb
wait nodec

docker exec nodea rabbitmq-plugins enable rabbitmq_management
docker exec nodea rabbitmqctl stop_app
docker exec nodea rabbitmqctl reset
docker exec nodea rabbitmqctl start_app
docker exec nodea rabbitmqctl add_vhost netsplit
docker exec nodea rabbitmqctl set_permissions -p netsplit guest ".*" ".*" ".*"

docker exec nodeb rabbitmq-plugins enable rabbitmq_management
docker exec nodeb rabbitmqctl stop_app
docker exec nodeb rabbitmqctl reset
docker exec nodeb rabbitmqctl start_app
docker exec nodeb rabbitmqctl stop_app
docker exec nodeb rabbitmqctl join_cluster rabbit@nodea
docker exec nodeb rabbitmqctl start_app

docker exec nodec rabbitmq-plugins enable rabbitmq_management
docker exec nodec rabbitmqctl stop_app
docker exec nodec rabbitmqctl reset
docker exec nodec rabbitmqctl start_app
docker exec nodec rabbitmqctl stop_app
docker exec nodec rabbitmqctl join_cluster rabbit@nodea
docker exec nodec rabbitmqctl start_app
