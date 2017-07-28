(LMU specific config)[uni.md]
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

*TODO:* On LMU worker instances the current project root is
`/home/kryo/programming/ma-project`

Once you have the stack build tool installed go the the Codekoan repository and
run `stack build`. This should be sufficient to build all of the necessary
binaries. The binaries can be found in `.stack-work/install/<your
architecture>/lts-8.0/8.0.2/bin/`.

### Set up RabbitMQ

#### Installation

The [RabbitMQ](https://www.rabbitmq.com) messaging system is used to coordinate
the distributed parts of the Codekoan pipeline. RabbitMQ must be installed on
one single machine in your distributed cluster. 

In order to simplify the subsequent RabbitMQ configuration, it is recommended to
use RabbitMQ's [management plugin](https://www.rabbitmq.com/management.html)
which is included in the default RabbitMQ installation (although not enabled by
default)

#### RabbitMQ Configuration

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
you need to create four queues `queries-java-[1..4]` that are bound to the
`queries-java` exchange.

**Response collection**

The returned replies need to be collected by an exchange named `replies` which
is bound to a queue named `replies`.

### Set up PostgreSQL

Codekoan currently uses the [PostgreSQL](https://www.postgresql.org/) database,
which needs to be set up on your system.

The data to index is contained in
the
[Stack Overflow data dump](https://archive.org/details/stackexchange). Specifically
you will need the xml file in the `stackoverflow.com-Posts` archive.

To index this data into the database in a Format, that Codekoan can understand,
use the `ma-postgres-indexer` binary like this:

```
stack exec ma-postgres-indexer <dump-xml "stackoverflow.com-Posts" file>
                               <postgres host>
							   <postgres port>
							   <postgres user>
							   <postgres password>
							   <postgres database>
```

## Set up a search-cluster

Some of the services in the following require open ports. The recommended way to
set up the Codekoan search engine is to only open ports for https/http access on
the web-application and not allow network traffic to other services from outside.

The git repository comes with sample configuration files which are usually named
`settings.yaml`. The ports that will be used in the following documentation are
just examples and can be configured freely.

### Start the reply cache

The reply cache is a small webservice that consumes search results from rabbitmq
and stores them for later consumption.

Launch with: `stack exec ma-reply-cache <settings.yaml>`

This service requires an open port, which is **6367** by default.

### Start the rabbitmq injector

This service accepts, validates and submits queries. It replies to the submitter
with a json-wrapped search query id, by which submitters can track their search
in the reply cache mentioned above.

Launch with: `stack exec ma-rmq-injector <settings.yaml>`

This service requires an open port which is **6368** by default.

### Configure and start the semantic service

In most configuration settings search will analyze the similarity of identifier
words in code. This is done by a configurable web service. Such a service is
included in the codekoan distribution.

It can be started with `stack exec ma-semantic-service <settings.yaml>`

This service requires an open port which is **6366** by default.

### Start the webservice

For users to interact with a search cluster, the codekoan web-application is
very useful.

Launch it with `stack exec ma-site-light <settings.yaml>`.

The web-application routinely operates on port **6365** which should be proxied
by e.g. apache or nginx in order to enable ssl and integrate into an existing
website.

### Set up a search service cluster

The actual work of CodeKoan search engine is performed in worker processes that
are started with the `ma-search-service` process. Each of these worker processes
can hold either part or all of the given stackoverflow patterns for a given
programming language.

The following will discuss how these worker instances are configured using
modifications of the below sample configuration file:

```yaml
search-language: "java"
search-exchange: "queries-java-1"
search-question-tag: "java"
search-cluster-size: 5
search-answer-digits: [0, 1]
search-rabbitmq-settings:
  rabbitmq-user: "guest"
  rabbitmq-pwd: "guest"
  rabbitmq-host: "localhost"
  rabbitmq-virtual-host: "/"
search-postgres-database:
  db-user: "<user>"
  db-pwd: "<passwor>"
  db-name: "testdb"
  db-port: 5432
  db-host: "localhost"
search-semantic-url: "http://localhost:3666/submit"
```


#### Controlling Index Distribution In The Config

All answers in Stack Overflow are assigned inter IDs. Which answers are indexed
in which worker process is determined by using a **modulus 10** of each answer's
id. This is done in the `search-answer-digits` parameter. This is a list of
single digits. For example the sample config above would lead to an index that
roughly contains one fifth of all of Stack Overflow, namely the answers with IDs
that end in either 0 or 1.

**IMPORTANT:** take care, that indices of distributed workers don't overlap,
i.e. don't create a situation where you have one worker with
`search-answer-digits: [0,1,2]` and another worker with `search-answer-digits: [1,2,3]`. If you do this, you will get *duplicated search results*!

#### Run The Search Service

Copy the `ma-search-service` from the `.stack-work/...` directory mentioned
above into a new directory. Copy a `settings.yaml` file as in the example above
or in `ma-search-service/settings.yaml` into the new directory (e.g.
`/home/user/instance-1/`). Start the service using 

```bash
./ma-search-service > service.log
```
