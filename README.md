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

## Running

[logo]: docs/packu-small.png

[algencan]: http://www.ime.usp.br/~egbirgin/tango
