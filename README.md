This is the mechanization of the semantics presented in *"Decoding Lua: Formal semantics for the developer and the semanticist"* and *"Understanding Lua’s Garbage Collection - Towards a Formalized Static Analyzer"*.

1. **Installation of DrRacket**

To install Racket's IDE DrRacket, simply download the installer that corresponds to your system from https://racket-lang.org/download/. The present version of the mechanization has been tested on DrRacket 8.1.

2. **Structure of the mechanization**

The modules' distribution mimics the structure of the semantics presented in the accompanying paper. Its main components are:
* **grammar.rkt**: grammar of the language, together with the definition of evaluation contexts. For clarity purposes, the grammar is divided into:

    * **coreGrammar.rkt**, core subset of grammar constructions.    
    * **extGrammar.rkt**, extended core to include semantics elements (note: in Redex we are dealing with a reduction semantics, hence we have semantics elements codified as syntactic 
objects)    
    * **wellFormedGrammar.rkt**, well-formed subset of programs, needed for randomized testing purposes (see below).
    
* **executionEnvironment.rkt**: where bindings to the services available to every program are defined. The execution environment itself is defined as an evaluation context. 
* **interpreter.rkt**: simple module that provides 3 procedures, execute, luagctrace and luatrace, that executes a given Lua program or call the stepper to show the execution under the semantics with or without garbage collection, respectively. As an example, to run the Lua program `print("hello, world!")`, type:

```racket
(execute "print(\"hello, world!\")")
```

* Desugar directory: the implementation of our desugaring function. 
* Meta-functions directory:
	* **delta.rkt**: the implementation of the delta interpretation function. It is divided into:   
	
	   * **deltaBasics.rkt**, mechanization of Lua's basic functions.   
	   * **deltaMath.rkt**, **deltaString.rkt** and **deltaTable.rkt**, mechanization of Lua's math, string and table library services, respectively.
	    
	* **errorMessagesMetafunctions.rkt**: implementation of the meta-function #errmessage (mentioned in the paper)
	* **grammarMetaFunctions.rkt**: a couple of predicates over grammar's symbols (that ease the definition of several rules from the model) and some minor tasks related with the manipulation of phrases from that grammar (they are mostly implementation related).
	* **gc.rkt**: mechanization of garbage collection in Lua 5.2. It is divided into:   
	    
	    * **finalization.rkt**, **./weakTables.rkt**, mechanization of concepts related to finalization and weak tables, respectively.
	     
	* **objStoreMetafunctions.rkt**: meta-function for the manipulation of the objects' store.
	* **substitution.rkt**: our implementation of a substitution function, that suites our needs (in particular, we don't need it to be capture-avoiding).
	* **tablesMetafunctions.rkt**: several meta-functions that manipulate tables. This is the module where the meta-function addkeys (mentioned in the paper) is defined.
	* **valStoreMetafunctions.rkt**: meta-function for the manipulation of the values' store.
	* Relations directory: each module corresponds to one relation from the ones composing our model.
	* Tests directory: each meta-function and relation has its own test suite. They are placed in this directory.
		* LuaTests: contains versions of the files from Lua 5.2's test suite that can be executed with our model. 

3. **Tests**

* The module **Tests/runAllTests.rkt**, when executed, calls every test suite specifically for the mechanization (that is, it doesn't call the Lua 5.2's compliance test suite). You should be able to open the file within DrRacket, execute it and see how all the tests passed. 

* This test suite exploits the modular structure of the semantics, by testing each relation and meta-function in isolation, while also doing some "integration tests" (running some complete programs and testing snippets of code plugged into the execution environment). 

* **Executing Lua's own test suite**:    
     
     * For performance purposes, we divided each .lua file of the test suite into several fragments. A .rkt file is provided which implements several tests that compile each of these fragments.
     * For a given module M.lua of Lua's test suite, a single test test-M_n from M.rkt will compile all the content of the file M_n.lua, plugging the result into an execution environment that only contains the required library services. Then, it will execute the resulting program and test whether the execution was successful or not.
     * For easiness of use, we provide a Makefile that allows for the execution of the whole test suite, with or without testing of gc:         
        ```make [with-gc] [-jn]```.            
     * ```with-gc``` enables testing of the whole test suite, gc included, and ```n``` is the number of parallel threads of execution to be used. It is advisable to make use of parallel threads of execution, if available.
* **Randomized testing of properties**
    * The module **Tests/RandomTesting/soundness/wfc.rkt** contains the definitions of the formal system that captures well-formedness of configurations and terms, while **Tests/RandomTesting/soundness/soundness_rand_test.rkt** contains procedures to perform random testing of the soundness of said formal system. 
    * ```(soundness_wfc_full_coverage attempts debug)``` (defined in  **Tests/RandomTesting/soundness/soundness_rand_test.rkt**) generates `attempts` terms, and checks if the statement of soundness holds for each term. ```debug``` is a boolean flag that indicates if we want to inspect every configuration generated that is not well-formed (useful for debugging purposes).  

4. **Now, where to go?**

If you want to test by your own some Lua code, and you are not familiar with PLT Redex's syntax, nor with Lisp dialects' syntax, reading the file grammar.rkt won't be very useful. But don't worry. We can still make use of the mechanized Lua's compiler, to execute programs using the procedures provided by the module interpreter.rkt, already described: just type in your Lua program and let the marvelous syntactic machine of PLT Redex to do the rest: it will begin by populating the execution environment with all the services that we have actually mechanized, and then it will proceed by executing the given code. 
	
The result of the program will appears somewhat cryptic at the beginning: it will consist of a syntactic representation of what the program had left in the value and object stores, together with the skip statement (;) or a return statement, if there were no problems with the execution. 

Remember that a Lua program works by making successive changes to a state. They are truly state modifiers and that is what the semantics is explaining. That's why we obtain as a result just that: the final state of the memory, together with the final term. If you want to actually observe the result of some computation, you can use the print service, which uses Racket's own print service, and also saves its received arguments in a special position in the values store, for latter inspection.
