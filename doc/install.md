# Codekoan installation instructions

This document outlines how to build and run the Codekoan search pipeline. While
the Codekoan pipeline can be used with arbitrary indexed code fragments, this
document solely outlines how to run it with an index of Stackoverflow Documents.

## Building the Codekoan pipeline

### Download the stack build tool

The Codekoan search pipeline is exclusively implemented using
the [Haskell](http://haskell.org) programming language. To build the complete
Codekoan project you will need Haskell's [stack](https://haskellstack.org/)
build tool.

The stack build tool is available as a package for most major linux
distributions. For Ubuntu/Debian you can use the `haskell-stack` package.  On OS
X you can install the `haskell-stack` package. Installers for Windows are also
available on the stack homepage.

### Build the Codekoan pipeline

Once you have the stack build tool installed go the the Codekoan repository and
run `stack build`. This should be sufficient to build all of the necessary
binaries. The binaries can be found in `.stack-work/install/<your
architecture>/lts-8.0/8.0.2/bin/`.

## Set up RabbitMQ

### Installation

The [RabbitMQ](https://www.rabbitmq.com) messaging system is used to coordinate
the distributed parts of the Codekoan pipeline. RabbitMQ must be installed on
one single machine in your distributed cluster. 

In order to simplify the subsequent RabbitMQ configuration, it is recommended to
use RabbitMQ's [management plugin](https://www.rabbitmq.com/management.html)
which is included in the default RabbitMQ installation (although not enabled by
default)

### RabbitMQ Configuration

This section assumes that you are familiar with
the [basics](https://www.rabbitmq.com/getstarted.html) of RabbitMQ. If you
aren't you should probably browse through the linked documentation.

Naming of queues and exchanges does not follow a fixed scheme and you can change
the names used below to anything you like, however we recommend using the
provided names for clarity and will follow this naming scheme in other parts of
this documentation.

**Query routing**

The first thing you need to set up is a topic exchange called
`queries`. Furthermore you need to configure a fanout exchange
`queries-<language>` for each programming language you intend on using. You then
need to bind every one of the `queries-<language>` exchanges to the `queries`
exchange using `<language>` as a routing key. So for example you would bind the
the exchange `queries-java` to `queries` using the routing key `java`.

You then need to make a decision about your cluster size for each
language. Assuming you want to use a cluster of four worker processes for `java`
you need to create four queues `queries-java-[1..n]` that are bound to the
`queries-java` exchange.

**Response collection**

The returned replies need to be collected by an exchange named `replies` which
is bound to a queue named `replies`.

## Set up PostgreSQL


