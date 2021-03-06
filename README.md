# The Codekoan search engine

## Overview

## Installation instructions

The Codekoan search engine is easy to set up. Please see the [installation instructions](doc/install.md)

## Overview

This project is the complete implementation of my code pattern recognition
engine including a web-application to acces it.

It is structured as a multi-package [stack][] project.

# Packages:
## codekoan-search-backend

This package contains a programming language-agnostic library that provides:

* Access to stackoverflow data through PostgreSQL
* Access to the stackoverflow REST-API
* Data structures for Stackoverflow data
* Levenshtein search
* Token Bloom filters
* Syntactic filtering steps
  * Alignment
  * Aggregation
* Static code analysis
  * Blocks analysis
  * Word similiarity analysis

## codekoan-messaging

This package contains types that are used for communicating between
microservices.

## Language Implementations
### codekoan-language-java
### codekoan-language-python

## codekoan-postgres-indexer

This package provides an executable that writes data from a Stackoverflow
posts-dump into a PostgreSQL database

## codekoan-rmq-injector

This package provides a webservice that takes JSON queries and puts them into
RabbitMQ.

## codekoan-search-service (Search Worker)

This package contains an executable that takes data from RabbitMQ, performs
searches and returns the results to RabbitMQ

### RabbitMQ Configuration for every single Worker Instance

**The following Code are yaml syntax, the space indent are crucial.**
<pre>
search-language: "java" // one of the three language "java", "python". "haskell", input is checked in source codes
search-exchange: "queries-java-3" // unique queue name in RabbitMQ, the current work polls its task message from this unique queue
search-question-tag: "java" // filter tag from StackOverflow to prevent search through irrelevant stackoverflow answers.
search-answer-digits: [0,3,4,5] // the answer id from StackOverflow are chunked into 10 portions with modulo, the nummer presented here are the chunked index. We assume the post topic are uniformly distributed amount the chunks with this seperation method, which can be systematically proved with in a future work. 
search-cluster-size: 4 // work arround for failing configuration service. This number is crucial, which should be identical for all works for one language, so that the reply cache serivce know how many response are pending. This number reflex on the number of workers for a dedicated language. 
search-rabbitmq-settings: // this section describes the credential for rabbitmq (aka: Message Queueing Service)
 rabbitmq-user: "user"
 rabbitmq-pwd: "password"
 rabbitmq-host: "localhost"
 rabbitmq-virtual-host: "/"
search-postgres-database: // this section describes the credential for PostgreSQL DB 
 db-user: "user"
 db-pwd: "password"
 db-name: "testdb"
 db-port: 5432
 db-host: "10.155.208.4"
search-semantic-url: "http://localhost:3666/submit" // the end-point of semantic service for work to fetch identifier similarity scores
</pre>


## Installation instructions

First get a working [stack][] installation. Then ```git clone``` the project
into a directory, change into the ```codekoan``` directory and use
```stack build && stack exec-ma-site``` to run the webapplication. The default
port to reach the webapplication is ```3000```.

You will probably also have to adapt the config in
```ma-project/ma-site/config``` and get the necessary data from a [publicly
available](https://archive.org/details/stackexchange) stackoverflow data-dump.

[stack]: https://docs.haskellstack.org/en/stable/README/

## Starting the system
### Start RabbitMQ Injector Service
Yaml config file
<pre>
# Settings pertaining to the RabbitMQ connection
rabbitmq-settings:
 rabbitmq-user: "user"
 rabbitmq-pwd : "password"
 rabbitmq-host: "localhost"
 rabbitmq-virtual-host: "/"

# Settings pertaining to logging
log-settings:
 log-level: debug

# The port that the application is run on
application-port: 6368
</pre>
* starting with `stack exec codekoan-rmq-injector`

### Start Reply Cache Service
Yaml config file
<pre>
# Settings pertaining to the RabbitMQ connection
rabbitmq-settings:
 rabbitmq-user: "user"
 rabbitmq-pwd : "password"
 rabbitmq-host: "localhost"
 rabbitmq-virtual-host: "/"

# The RabbitMQ queue that we observe for replies
reply-queue: "replies-1"

# Settings pertaining to logging
log-settings:
 log-level: debug

# The port that the application is run on
application-port: 6367
</pre>
* starting with `stack exec codekoan-reply-cache`

# Start Semantic Service
Yaml config file
<pre>
#Directory that is recursively searched for code files
#(ending is hardcoded into language type) 
corpus-directory: /home/analytics/temp/javacorpus/elasticsearch

# The language to use (one of ["python", "java", "haskell"])
corpus-language: java

# Settings pertaining to logging
log-settings:
 log-level: debug

# The port that the application is run on
application-port: 6366
</pre>
* starting with `start exec codekoan-semantic-service`

# Config RabbitMQ
## prerequisition 
* Confiq topic exchange named "queries"

* Config 3 fanout exchanges for the 3 supported languages, 1 fanout for "java", 1 fanout for "python", 1 fanout for "haskell"

* Config binding for routing with routing key (Key), fanout exchanges "queries-java" -> java, fanout exchanges "queries-python" -> python, fanout exchanges "queries-haskell" -> haskell

* Using RabbitMQ Management Interface to create new queues for each worker, and bin the fontout exchange to each of this worker queue. (the messages passing though the fanout exchange will be redundantly copyed to each queue binded to the fanout exchange.)  

* the search-exchange variable in the config Yaml file of each work, should be identical to the name of the queue binded to the approperate fanout exchange.

* Config reply exchange for receiving response messages and bind it to a queue.

## Starting Work instance
* copy the codekoan-project folder to all the workers
* `cd codekoan-project`
* `stack build` to build the binary for the worker on each worker node
* `stack exec codekoan-search-service <path-to-yaml>`, <path-to-yaml> is the path to the worker yaml config file (after starting the work, the surfix tree index will be build automatically)

## Starting Web Application
* `stack exec codekoan-site-light`, pwd out is codekoan-project



