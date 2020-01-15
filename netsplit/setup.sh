#!/usr/bin/env bash

function start () {
  local image=rabbitmq:3.8-alpine
  local node=$1
  local amqp_port=$2
  local ui_port=$3

  docker run -d --name "$node" \
    -p "$amqp_port:5672" -p "$ui_port:15672" \
    -e RABBITMQ_NODENAME="rabbit@$node" \
    -e RABBITMQ_ERLANG_COOKIE="netsplit" \
    -v "$PWD"/rabbitmq.conf:/etc/rabbitmq/rabbitmq.conf \
    $image
}

function wait () {
  local node=$1

  while ! docker exec "$node" rabbitmqctl status &>/dev/null
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
start nodea 5001 15001
docker network connect ab nodea
docker network connect ac nodea

# Node b should be in both ab & bc network
start nodeb 5002 15002
docker network connect ab nodeb
docker network connect bc nodeb

# Node c should be in both ac & bc network
start nodec 5003 15003
docker network connect ac nodec
docker network connect bc nodec

# Start a load balancer for all amqp ports
docker run -d --name lb \
 -p 5000:5000 \
 --link nodea \
 --link nodeb \
 --link nodec \
 -v "$PWD"/gobetween.toml:/etc/gobetween/conf/gobetween.toml \
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
