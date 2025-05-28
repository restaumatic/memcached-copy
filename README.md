# memcached-copy

`memcached-copy` is a small program to run alongside a `memcached` instance.

It discovers other instances (under a given DNS name) and copies all data from them to the local instance.

It is part of a replication scheme where all keys are replicated to all instances. 

It assumes some things about your architecture:

1. No sharding - all keys are replicated to all instances.
2. DNS-based service discovery - other memcached nodes are discovered via DNS entries.

**Important**: Using this system will severely relax the consistency guarantees you would get from a single-node system. In particular:

- `DELETE`s are unreliable. In fact, `memcached-replicator` doesn't replicate DELETEs at all. We discourage using them for invalidation in this architecture
- `SET`s may also fail to overwrite an older value. (Note that `SET`s are always unreliable in some sense in memcached, since any key can be evicted; but single-node memcached doesn't allow a key to return to a previous value).

In short, when using this system, if you read from memcached, you can get not only the latest value, but any value previously written to a given key (although most of the time it should be the latest value). In particular it could happen that a server with older value replicates it to other servers (including ones that have latest value).

This works well if your objects are immutable - this is the case e.g. in Rails' key-based expiration scheme (new version of an object is written under a new key).

## How it works

`memcached-copy` should be deployed alongside each of the memcached instances (e.g. in the same container, or in a sidecar container).

Usage: `memcached-copy <discovery address> <self address>`

Where:

- `discovery address` is the service discovery DNS name. Querying it should return all the memcached nodes (possibly including self).
- `self address` is the address to connect to the local instance. Most of the time should be `localhost`.

Note: port is hardcoded to `11211`. If you want to change it, you need to patch this software.

On startup, `replicator` discovers other servers and unceremoniously copies all data from them to the local server.

## Application usage

The application may read from any of the servers, and should write to all servers.

## Discovering self

Since the result from service discovery may also include the local server, we have to exclude it. Since determining the IP address of the local container may get tricky, we use a different method: to determine if two addresses refer to the same server, we write a random key (in a way that doesn't trigger replication) to one and read it from another. If it's present, it is the same server.

There is a small chance the key will be evicted during this operation, but it's probably vanishingly small.
