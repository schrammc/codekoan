# Code Pattern Search Engine Implementation in Haskell

## Overview

This project is the complete implementation of my code pattern recognition
engine including a web-application to acces it.

It is structured as a multi-package [stack][] project.

## Packages:
### ma-search-backend

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

### ma-messaging

This package contains types that are used for communicating between
microservices.

### Language Implementations
#### ma-language-java
#### ma-language-python

## ma-postgres-indexer

This package provides an executable that writes data from a Stackoverflow
posts-dump into a PostgreSQL database

## ma-rmq-injector

This package provides a webservice that takes JSON queries and puts them into
RabbitMQ.

## ma-search-service

This package contains an executable that takes data from RabbitMQ, performs
searches and returns the results to RabbitMQ

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
