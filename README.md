# Readme

Heist is a programming language for Bigraphs implented in a interpreter in ```Heist/bin``` with dependent backend library ```Heist/lib``` which implements the languages backend evaulation constructs independently. Also provided in ```Heist/test/lib``` is a second miniature library with simple functionality allowing users to unit test their own code written in Heist, sample programs written in Heist (```Heist/test/Heat```) and equivalent *BigraphER* representations (```Heist/test/BigrapherTemplates```) are provided including **JSON** implementations of all the necessary Bigraphs and reaction rules in (```Heist/test/Bigrapherfiles```).


## Build instructions

##### Fundamental setup
Beginning from the Heist directory it is recommoned that a new opam switch is created to prevent package version conflicts, the switch can have any desired name for example *my_switch*.

```
$ opam switch create my_switch  ocaml.4.13.1
$ opam switch my_switch
```

Next the dune build system should be installed in the switch to enable automated gathering of other dependencies.

```
$ opam install dune
```

##### External dependencies - BigraphER

dependencies *BigrapheER* and the associated *Bigraph* librabry cannot be installed directly using dune or Opam and must be acquired from [uofg-bigraph-tools](https://bitbucket.org/uog-bigraph/bigraph-tools/src/yojson/). 

After cloning the yojson branch of the Bigraph tools repository outside of the Heist directory dune is used to build and install the bigraph-tools packages.

```
$ dune build --profile=release
$ dune install --profile=release
```
 - Note this external dependecy may require other packages which aren't automatically installed by it's build system including ```menhir```,```progress```,```dune-configurator``` and ```ppx_deriving_yojson``` which will raise errors if missing, these can be installed independently with ``` opam install ``` if necessary. 

To verify installation ``` opam list ``` can be used to check whether the correct packages have been installed and should look like:

##### building Heist from source

finally returning to the Heist directory any remaining dependencies can be installed using the .opam file generated by dune.

```
$ opam install . --deps-only --with-test --with-doc
```

Finally, dune can be used to build the Heist interpreter.

```
$ dune build
```

Once the build has been successfull the ```heist.exe``` interpreter should exist within the Heist directory and developer documentation can be accessed using ```open _build/default/_doc/_html/index.html```

### Requirements

* Ocaml : >=  4.13.1
* dune : >= 3.13.0
* Packages:  
  * automatically installable : listed in `Heist.opam`
  * manual instalation : [bigraph](https://bitbucket.org/uog-bigraph/bigraph-tools/src/yojson/)
* Tested on Ubuntu and WSL2

### Test steps

##### Automated unit testing 

Two suites of unit tests can be run to ensure Heist is working correctly which should all pass and can be triggered using:

```
$ dune runtest --force
```

##### Manual program execution 

Heist comes provided with a number of sample programs to demonstrate how typical programs written in the language should look. One of these programs stored in "testrun.heat" can be evaluated using the interpreter as follows:

```
 $ ./heist.exe test/Heat/testrun.heat test/Bigraphfiles/comms.json
```

* Note the *heist.exe* interpreter is by default created in the ```Heist``` directory so any filepaths when working with examples should be relative to ```Heist``` unless the executable has been moved
