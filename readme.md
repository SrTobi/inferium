# Inferium

A program to generate TypeScript type definitions for JavaScript packages.

## How to build

The following software has to be present to build Inferium:

- sbt
- scala
- nodejs + npm

In the directory `extras/ts-reader/`, the following command has to be executed once to install Node.js dependencies.

```
npm install
```

The `run` script automatically uses `sbt` to build Inferium.


## How to run

Simply run the `run` script with with the target's package name as argument.

~~~
./run <package-name>
~~~

The resulting type definition  will be printed to the terminal.
For example:

~~~
./run left-pad
~~~

## Benchmarks

To evaluate Inferium, use the `run_all` script with a file as argument that contains one package name per line.
The script expects [DefinitelyTyped](https://github.com/DefinitelyTyped/DefinitelyTyped) to be installed next to the Inferium directory.
The result will be put into `result/<list-file-name>`.

~~~
./run_all <list-file-name>
~~~

