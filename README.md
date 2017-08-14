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
    remaining line contains the **length** and **width** of each
    container, from the largest area to the smallest one. The
    following example illustrates the case of 2 containers:

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

**Important**: All the unit measures must be the same.

## Running

After setting up your problem, to run Pack-U, simply go to the `build`
directory, copy the files that describe the problem and run it:

    cd build/
    cp ../examples/toyprob/*.txt .
    ./packu

Pack-U produces several files for output information:

  - `solution.csv`: a CSV file having the number of lines as the
    number of used containers. Each line follows the pattern:

        Container Type, #Item 1, #Item 2, ...,
	
    where *Container Type* is a number associated with the line in
    `containers.txt` and *#Item 1* is the number of items of type 1
    that will be packed into this container. The same for *#Item 2*,
    *#Item 3*, etc.

  - `solXXX.asy`: Asymptote files for drawing solutions. Those files
    show the packing configuration for container *XXX*. In order to
    display each file, it is necessary to copy `packilib.asy` file to
    the current directory and call `asy`

        cp ../packlib.asy
        asy solXXX.asy

    A PDF file `solXXX.pdf` will be generated. If a PDF file with
    **all** solutions is wanted, then the user can run
    `makepdf.sh` script, located at `scripts/` directory:

        cp ../packlib.asy
        ../scripts/makepdf.sh

    Be carefull, since this script removes all ASY files. File
    `solution.pdf` will be generated.

  - `stats.csv`: CSV file containing statistical information about the
    problem. It follows the pattern:

        NC, TCA, TIA, WR, CPU

    where `NC` is the *number of containers*, `TCA` is the *total area
    of the containers*, `TIA` is the *total area of the items*, `WR` is
    the *waste ratio* in percents (100 * (TCA / TIA - 1)) and `CPU`
    is the *CPU time* in miliseconds.

The solution obtained for the above problem is

    1,   70,    0,    0,    0,
    1,   70,    0,    0,    0,
    1,   70,    0,    0,    0,
    1,   70,    0,    0,    0,
    1,    0,    0,    0,   20,
    1,    0,    0,    0,   20,
    1,    0,    0,    0,   20,
    1,    0,   17,    0,   10,
    2,   20,    3,    0,    0,

and the images generated are

![][sol0] ![][sol1] ![][sol2]
![][sol3] ![][sol4] ![][sol5]
![][sol6] ![][sol7] ![][sol8]

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

In order to avoid excessive output information of ALGENCAN, it is possible to
create an empty file `.silent` in the same directory where the
executable is run:

    touch .silent

[logo]: docs/images/packu-logo.png

[sol0]: docs/images/sol0.png
[sol1]: docs/images/sol1.png
[sol2]: docs/images/sol2.png
[sol3]: docs/images/sol3.png
[sol4]: docs/images/sol4.png
[sol5]: docs/images/sol5.png
[sol6]: docs/images/sol6.png
[sol7]: docs/images/sol7.png
[sol8]: docs/images/sol8.png

[algencan]: http://www.ime.usp.br/~egbirgin/tango
