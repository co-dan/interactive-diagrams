# Restricted Workers Library

This library provides an abstract interface for running various kinds
of workers under resource restrictions. It was originally developed as
part of the
interactive-diagrams (<http://github.com/co-dan/interactive-diagrams>)
project. You can read more about security restrictions in the wiki: <https://github.com/co-dan/interactive-diagrams/wiki/Restricted-Workers>

The library provides a convenient way of running worker processes,
saving data obtained by the workers at start-up, a simple pool
abstraction and a configurable security and resource limitations.

Right now there are several kinds of security restrictions that could
be applied to the worker process:

- RLimits
- chroot jail
- custom process euid
- cgroups
- process niceness
- SELinux security context

# Documentation

The easiest way to get a grip of the restricted-workers library is to
look at the examples below showing off the basic concepts of the
library. Another good idea would be to read haddock documentations
which feature comments for each exported function and type in the
library. Do not hesitate to bug me if you think that the documentation
in some places can be improved.

## Examples

The following examples will walk you through creating basic kinds of
workers (IOWorker), handling a pool of workers, communicating with
workers using 'System.Restricted.Workers.Protocol' and creating your
own types of workers.

- [EchoWorker.lhs](examples/EchoWorker.lhs) - basic usage of
  `IOWorker`
- [EchoPool.lhs](examples/EchoPool.lhs) - basic usage of
  `Workers.Pool`
- [CommandEvalProtocol.lhs](examples/CommandEvalProtocol.lhs) -
  rewriting our Echo worker to use the provided Protocol module
- [NewWorkerType.lhs](examples/NewWorkerType.lhs) - rolling out your
  own worker types

## Wiki page

<https://github.com/co-dan/interactive-diagrams/wiki/Restricted-Workers>

# External configurations 

Some restrictions require external configuration, below we provide
some example files for them that we use in interactive-diagrams:

- SELinux configuration:
  https://github.com/co-dan/interactive-diagrams/tree/master/selinux
  
  Run `build.sh` to build the policy module, then `load.sh` to load
  it. Read the
  [blog post](http://parenz.wordpress.com/2013/07/15/interactive-diagrams-gsoc-progress-report/)
  which explains the policy.
  
- CGroups:
  https://github.com/co-dan/interactive-diagrams/blob/master/cgconfig.conf
  
  CGroups configuration is pretty straightforward. You can load the
  configuration with
  
  ```
  cgconfigparser -l cgconfig.conf
  ```
