# Code Pattern Search Engine Implementation in Haskell
## System Components Communication
* StackOverflow Persistency: PostgreSQL DB on damar (PMS On-Premises) Port 5432, this port should be accessible from MWN to PMS DMZ

* Message Queuing: RabbitMQ on damar Port 5672, this port should be accessible from MWN to PMS 

* Message Monitor: RabbitMQ Web Monitor on damar Port 15672, this port should be accessible from MWN to PMS.

* Semantic Service: Haskel RestService on damar Port 6366, this port should be accessible from MWN to PMS

* TCP Service: Haskel TCP Backend Service on damar Port 6365 on damar, an appache proxy from 443 will be routed to 6365 for handling TCP request from the client. Port 6365 is local port, which should not be opened for MWN and Public Internet. The apache proxy port 443 should be open for MWN in the first stage of the project, and in the later stage, port 443 should be open for public internet.

* ReplyCache Service: Haskel RestService on damar Port 6367, this port should be accessible from MWN to PMS. (debugging purpose, can be closed at production stage)

* Message Query Injector Service: Haskel RestService for posting messages into RabbitMQ on damar Port 6368, this port should be accessible from MWN to PMS. (debugging purpose, can be closed at production stage)


## Overview

This project is the complete implementation of my code pattern recognition
engine including a web-application to acces it.

It is structured as a multi-package [stack][] project.

# Packages:
## ma-search-backend

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

## ma-messaging

This package contains types that are used for communicating between
microservices.

## Language Implementations
### ma-language-java
### ma-language-python

## ma-postgres-indexer

This package provides an executable that writes data from a Stackoverflow
posts-dump into a PostgreSQL database

## ma-rmq-injector

This package provides a webservice that takes JSON queries and puts them into
RabbitMQ.

## ma-search-service (Search Worker)

This package contains an executable that takes data from RabbitMQ, performs
searches and returns the results to RabbitMQ

### RabbitMQ Configuration for every single Worker Instance
<pre>
search-language: "java"
search-exchange: "queries-java-3"
search-question-tag: "java"
search-answer-digits: [0,3,4,5]
search-cluster-size: 4
search-rabbitmq-settings:
 rabbitmq-user: "kryo"
 rabbitmq-pwd: "mnl07xs"
 rabbitmq-host: "localhost"
 rabbitmq-virtual-host: "/"
search-postgres-database:
 db-user: "kryo"
 db-pwd: "mnl07xs"
 db-name: "testdb"
 db-port: 5432
 db-host: "10.155.208.4"
search-semantic-url: "http://localhost:3666/submit"
</pre>



## ma-site

A legacy webinterface, that will soon be removed / reimplemented

## Installation instructions

First get a working [stack][] installation. Then ```git clone``` the project
into a directory and change into the ```ma-project/ma-site``` directory and use
```stack build && stack exec-ma-site``` to run the webapplication. The default
port to reach the webapplication is ```3000```.

You will probably also have to adapt the config in
```ma-project/ma-site/config``` and get the necessary data from a [publicly
available](https://archive.org/details/stackexchange) stackoverflow data-dump.

[stack]: https://docs.haskellstack.org/en/stable/README/
