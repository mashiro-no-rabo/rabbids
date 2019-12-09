A toolset for [Chaos Engineering](https://www.wikiwand.com/en/Chaos_engineering) _on_ RabbitMQ. Which means it's not built for testing RabbitMQ per say, instead it focuses on helping to verify program and/or system behaviour which depends on RabbitMQ, by simulating various failure scenarios.

The name is inspired by [a game series about crazy rabbits](https://www.wikiwand.com/en/Raving_Rabbids).

Some planned functionalities:

- Trigger alarm state
- Set queue to `blocking` / `blocked`
- HA queues test (node up/down)
- Partitioned cluster

## Attacks

For now the workflow of performing attacks are very rudimentary, often requires direct (remote) Erlang shell access inside the cluster.

Hopefully this will change in near future.

### Memory Attack

The goal can be to trigger high memory alarm (which affects on the whole RabbitMQ cluster) or just examine system behaviour under high memory load.

#### Setup

1. Connect to one of the nodes in the target RabbitMQ cluster, make sure you're in an environment that it's possible to [remote shell](http://erlang.org/doc/man/shell.html#jcl-mode) to the running Erlang node.
  - Start another node with `erl -hidden` so it does not disrupt other nodes, [see documentation](https://erlang.org/doc/reference_manual/distributed.html#hidden-nodes)
  - Use `-sname` or `-name` depends on how the RabbitMQ node is started, they need to match otherwise they would refuse to cluster
  - Use `-setcookie` to [ensure cookies match](https://erlang.org/doc/reference_manual/distributed.html#security)
1. Drop the content of `rabbids_mem_attack.erl` to a good location - i.e. a location that is easy to reference/access in the Erlang shell
  - Use `pwd().` to find out the shell's working directory
1. Open a remote shell into the running node (typically `Ctrl-G`, then `r 'rabbit@...'` then `c`)
1. Load the attack module with `c("rabbids_mem_attack").`
1. Prepare the attack with `rabbids_mem_attack:prepare().`

#### Attack

Launch the attack with `rabbids_mem_attack:attack(500).` The only argument determines the scale of the memory attack in MiB unit, so substitute it with the amount desired.

Use `rabbids_mem_attack:info().` to check the running attack.

The attack can be tweaked by further calls to `rabbids_mem_attack:attack(...).` The new number will _replace_ (not accumulate) the previous attack value.

#### Withdraw

The attack can be stopped at any time by `rabbids_mem_attack:withdraw().` The memory in use should be quickly reclaimed.

#### Cleanup

It is best to cleanup after everything is done by `rabbid_mem_attack:teardown().` This will also release used memory if any attack is running.

#### Limitations

Currently the attack is limited by one process on the local node. I.e there will only be at most 1 attack running, and it can't be run on other nodes in the cluster.
