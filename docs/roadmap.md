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
* add loading of initial file from name in the scm4max object box
* buff i/o for handling floats in tables
* ensure multiple scm4max objects can coexist
* i/o for colls (done by implementing general message sending)

### Todo - Release 1:
* way of resetting environment
* compile as max package that can be used easily
* release as a package someone can install
* documentation
* souped up repl (multi-line)
* jitter matrix support??

## Backlog (unordered)
* nested dictionary handling for dict values
* dict i/o for array values
* jitter matrix i/o
* network repl from spacemacs for hot reloading partial buffers
* attach listeners to ITM transport for deferred handling
* enable multiple scm4max objects to share same environment
* threading macros (clojure style) for code in max messages
* some benchmarks to establish cost of the interpreter vs JS, Max, Gen, C
* documentation of API
* tutorials (text)
* tutorials (video)

