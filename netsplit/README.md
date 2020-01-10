# Simulating NetSplit

This creates a guaranteed netsplit.

The only dependency is `docker`. Make sure all these resources are available for use:
- Container names: `nodea`, `nodeb` and `nodec`
- Networks names: `ab`, `ac` and `bc`
- Ports: 5001 - 5003, 15001 - 15003

## Setup

Run `setup.sh` to start a 3 node cluster, each pair of nodes are connected through a separate [bridge network](https://docs.docker.com/network/bridge/). Currently no [partition handling strategy](https://www.rabbitmq.com/partitions.html#automatic-handling) is defined.

The cluster sets up following ports on localhost:

| Node | AMQP | Management UI |
| --- | --- | --- |
| nodea | 5001 | 15001 |
| nodeb | 5002 | 15002 |
| nodec | 5003 | 15003 |

## Create NetSplit

Run `split.sh` to disconnect the network between `nodeb` and `nodec`, which shall create a netsplit

After the netsplit, you should be able to see through `docker logs nodeb` or (`nodec`), including following lines:

```
2020-01-10 15:18:56.184 [error] <0.2383.0> ** Node rabbit@nodec not responding **
** Removing (timedout) connection **
2020-01-10 15:18:56.197 [error] <0.2072.0> Partial partition detected:
 * We saw DOWN from rabbit@nodec
 * We can still see rabbit@nodea which can see rabbit@nodec
We will therefore intentionally disconnect from rabbit@nodea
2020-01-10 15:18:58.225 [error] <0.1880.0> Mnesia(rabbit@nodeb): ** ERROR ** mnesia_event got {inconsistent_database, running_partitioned_network, rabbit@nodea}
```

The other node in the disconnected network would probably just see one error:

```
2020-01-10 15:18:57.479 [error] <0.1526.0> ** Node rabbit@nodeb not responding **
** Removing (timedout) connection **
```

Suppose you see exactly those (minus the timestamp), then `nodea` and `nodec` should still be clustered. But `nodeb` will be running in a separate partition.

## Cleanup

If something goes wrong, run `cleanup.sh` at any stage to cleanup all containers and networks created.

# Notes

This script is inspired by a recent incident in which we even experienced a complete netsplit (each node in its own partition). Though it might have been worsened by unlucky timing or due to even number of nodes. I.e. both side decided to intentionally disconnect at the same time. We unfortunately didn't really follow through. My theory is that if `nodeb` successfully disconnected then from `nodec` it won't see `nodeb` anymore, hence won't trigger the intentional disconnection.
