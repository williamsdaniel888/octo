# VisualInterface

This is an F# interface to [VisUAL](https://salmanarif.bitbucket.io/visual/downloads.html) emulator. The basic functionality is a function that has input an assembler source file and output a trace of execution, similar to what would happen in the VisUAL GUI.

A built-in cache, if configured, means that repeated execution of the same program will be near instantaneous. Otherwise every program takes 0.5s or so to execute due to Java startup time.

For information on VisUAL assembler instructions see the VisUAL documentation, or ask on Slack.

## Files

Directories:

* `.\VisualTest` - F# project `VisualTest` with the VisUAL interface and its test code
* `.\visualapp` - the VisUAL app itself, with its portable Java distribution (change for os-x or linux)
* `.\ visualWork` - directory used by VisUAL I/O files, and the interface itself, for temporary files

F# Program files under VisualTest:

* `VCommon.fs` Base types used throughout
* `VData.fs` Auto-create assembler fragments for interfacing
* `VLog.fs` Primitive serialisation interface for cache. Written without dependencies.
* `Visual.fs` Drive the Visual command-line program, implementing parallel opration and cacheing
* `VTest.fs` Tests are the framework itself, and some sample assember tests
* `VProgram.fs` Top-level code


## Code

You can use VisualTest by incorporating the VisualTest source code directly in your project (in which case you must change the namespace to be the same as yours), or reconfigure this code as a library. 

For convenience it is possible to put multiple framework modules in one file. Just cut and paste the module definitions, keeping the same sequential module order.

## Dependencies

VisualInterface requires VisUAL to be [downloaded](https://salmanarif.bitbucket.io/visual/downloads.html) and placed in a directory which is input as `paras.VisualPath`. Note that Visual is packaged with a portable version of Java which runs it.

You are recommended to keep the directory structure in the example code.

In theory the code will work on all platforms, but thus far it has only been tested on Windows.

## Operation


Normal operation is with cache on `paras.cached=true` because the cache mechanism is very robust. If anything goes wrong the cache will be recreated, which slows down operation, but does not affect results. Especially when testing tests, the cache saves considerable time.

NB - property-based tests are random and therefore by default don't get cached. You can
specify the same random set each time by setting a seed in fsConfig, see the `VTest.fs` file.

## Status

Basic operation as needed for data procesisng instructions (the main need for VisUAL is this) is tested. The memory read and write functionality in the testbench is experimental and not guaranteed to work. ask on Slack for more advanced usage.





