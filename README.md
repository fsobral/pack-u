# ![][logo]

### A packing system for online stores.


Contact us if you are interest in a packing solution!

Authors:

  - Francisco N. C. Sobral - fncsobral at uem dot br

  - Emerson V. Castelani
  
  - Wesley V. I. Shirabayashi
  
  - Rodrigo Schulz 

Developed at State University of Maringá, Paraná, Brazil.

## Compiling

Pack-U uses a combination of heuristics and non-linear programming to
solve packing problems. Up to now Pack-U only deals with *rectangular
containers* and *square items*. The items are not allowed to
rotate. In order to solve non-linear programming problems, Pack-U uses
[ALGENCAN 3.0.0][algencan].

To build Pack-U you need to

  1. Download [ALGENCAN 3.0.0][algencan] and generate its library file
  `libalgencan.a`

  1. Download Pack-U

  1. Modify the path to ALGENCAN's library in the `Makefile`. The
  default is

          TANGOLIB = /opt/tango/algencan-3.0.0/lib

  1. Type
  
          make packu

The generated file is located in the path given by `BUILDDIR`
variable, which is `build/` by default.

## Setting up your problem

Pack-U needs 3 files to solve a packing problem:

  - `containers.txt`: the type of containers that can be used. The
    first line contains the **number** of different containers. Each
    remaining line contains the **width** and **height** of each
    container. The following example illustrates the case of 2
    containers:

		2
		40.0 30.0
		28.5 17.5

  - `items.txt`: the type of items that can be packed. The first line
    contains the **number** of different items. Each remaining line
    contains a triple: **width**, **height** and **weight** (not used
    yet). The following example illustrates the case of 4 items:

            4
	    4 4 50
	    5.7 5.7 200
	    5.7 5.7 100
	    7.5 7.5 500   

  - `data.txt`: the **number items of each type** to be packed. It has
    the same number of lines as the number of type of items in the
    problem. In this example, a possible data file is:

            300
	    20
	    0
	    70

Usually, files `containers.txt` and `items.txt` do not change very
often, since they represent all the available items and
containers. Each packing problem consists of a different `data.txt`
file.

## Running

After setting up your problem, to run Pack-U, simply type

    ./build/packu

## Solver configuration

ALGENCAN provides many configuration parameters by the use of the
configuration file `algencan.dat`. The number of iterations can be
controlled by

    INNER-ITERATIONS-LIMIT XXXX

where XXXX is an integer number. The bigger this number the best
packing solutions it finds. However it takes more time to find the
result. The default is `99999999` (i.e. *unlimited*), but, for online
services and practical problems, it is reasonable to use small
numbers, such as 10, 100.

In order to avoid excessive output information, it is possible to
create an empty file `.silent` in the same directory where the
executable is run:

    touch .silent

[logo]: docs/images/packu-logo.png

[algencan]: http://www.ime.usp.br/~egbirgin/tango
