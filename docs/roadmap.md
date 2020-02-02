# Roadmap for scheme-for-max
2020-02-02

## Beta Release 1:

### Working:
* evaluating scheme sent in to inlet 0 as eval-string "(my-funct 1 2)"  
* evaluating scheme code sent to inlet 0 as max messages: my-funct 1 2
* sending messages from outlets
* attaching listeners to inlets
* table i/o (ints in tables)
* dict io for basic values (no arrays or nested dicts)
* file loading and reloading
* console posting (with arbitrary values)

### Todo - Release 1:
* add loading of initial file from name in the scm4max object box
* way of resetting environment
* dict i/o for array values
* buff i/o for handling floats in tables
* compile as max package that can be used easily
* ensure multiple scm4max objects can coexist

## Backlog (unordered)
* nested dictionary handling for dict values
* i/o for colls
* jitter matrix i/o
* network repl from spacemacs for hot reloading partial buffers
* attach listeners to ITM transport for deferred handling
* enable multiple scm4max objects to share same environment
* threading macros (clojure style) for code in max messages
* some benchmarks to establish cost of the interpreter vs JS, Max, Gen, C
* documentation of API
* tutorials (text)
* tutorials (video)

