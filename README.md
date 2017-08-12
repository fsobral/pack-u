# Pack-U ![][logo]

### A packing system for online stores.


Contact us if you are interest in a packing solution!

Authors:

  - Francisco N. C. Sobral - fncsobral at uem dot br

  - Emerson V. Castelani
  
  - Wesley V. I. Shirabayashi
  
  - Rodrigo Schulz 

Developed at State University of Maringá, Paraná, Brazil.

## Compiling

Pack-U uses a combination of heuristics and non-linear
programming to solve packing problems Up to now Pack-U only deals with
*rectangular containers* and *square items*. The items are not allowed
to rotate. In order to solving non-linear programming problems, Pack-U
uses [ALGENCAN 3.0.0][algencan].

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

  - `containers.txt`: the type of containers that can be used
  - `items.txt`: the type of items that can be packed
  - `data.txt`: the number of each type of item to be packed

Usually, files `containers.txt` and `items.txt` do not change very
often. Each packing problem consists of a different `data.txt` file.

## Running

After setting up your problem, to run Pack-U, simply type

    ./build/packu

ALGENCAN provides many configuration parameters by the use of the
configuration file `algencan.dat`. The number of iterations can be
controlled by

    INNER-ITERATIONS-LIMIT XXXX

where XXXX is an integer number. The bigger this number the best
packing solutions it finds. However it takes more time to find the
result. For online services, it is reasonable to use small numbers.

[logo]: docs/images/packu-small.png

[algencan]: http://www.ime.usp.br/~egbirgin/tango
