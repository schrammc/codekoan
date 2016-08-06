# Code Pattern Search Engine Implementation in Haskell

## Overview

This project is the complete implementation of my code pattern recognition
engine including a web-application to acces it.

It is structured as a multi-package
[stack][] project.

## Installation instructions

First get a working [stack][] installation. Then ```git clone``` the project
into a directory and change into the ```ma-project/ma-site``` directory and use
```stack build && stack exec-ma-site``` to run the webapplication. The default
port to reach the webapplication is ```3000```.

You will probably also have to adapt the config in
```ma-project/ma-site/config``` and get the necessary data from a [publicly
available](https://archive.org/details/stackexchange) stackoverflow data-dump.

[stack]: https://docs.haskellstack.org/en/stable/README/