# Scheme-For-Max Release Log

## 0.3.0 2021-11-27
- delay possible from low thread
- sending code to s4m no longer needs tosymbol and eval-string
- new gc functions: gc-enable, gc-disable, gc-run, gc-try
- set heap size with attr @heap
- threading macro 
- function send\* flattens list input
- function out\* spreads lists over outlets 
- sample Live API code to use

## 0.2.0 2021-01-13
- thread lock to high or low thread
- attributes for log-nulls and log-repl
- table i/o
- buffer i/o
- dict i/o
- transport integration
- delay and clock functions

## 0.1.3 2020-04-30
- bug fixes in list output
- adding loop.scm and utilities.scm from Common Music by default

## 0.1.2 2020-04-14
- bug fixes in s4m.repl

## 0.1.1 2020-04-14
- bug fixes in s4m-dispatch and post

## 0.1.0 2020-04-13

### Features
* file input and output via "read"
* main file input via file name in object box
* double clicking object box to open built in editor
* "reset" resets interpreter
* set number of inlets and outlets from @args in object box
* s4m.repl object can send code as strings
* listener registration for inlets 1+
* help tab with demos of all the above

### Issues
* setting inlets/outlets from inspector broken

