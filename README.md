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

  1. Create a file `user_variables.mk` and define the path to
     ALGENCAN's library. The name of the variable is `TANGOLIB` and a
     example is given by

         TANGOLIB = /opt/tango/algencan-3.0.0/lib

  1. Type
  
         make packu

The generated file is located in the path given by `BUILDDIR`
variable, which is `build/` by default.

## Setting up your problem <A ID="setting"></A>

Pack-U needs 3 files to solve a packing problem:

  - `containers.txt`: the types of containers that can be used. The
    first line contains the **number** of different containers. Each
    remaining line contains the **length**, **width** and **id** of
    each container. The following example illustrates the case of 2
    containers with the same *id* (the use of *id*s is explained
    [here](#ID)):

		2
		40.0 30.0 0
		28.5 17.5 0

  - `items.txt`: the types of items that can be packed. The first line
    contains the **number** of different types of items. Each
    remaining line contains a triple: **width**, **height** and
    **id**. The following example illustrates the case of 4 items with
    the same *id* (the use of *id*s is explained [here](#ID)):

        4
        4 4 0
        5.7 5.7 0
        5.7 5.7 0
        7.5 7.5 0

  - `data.txt`: the **number items of each type** to be packed. It has
    the same number of lines as the number of types of items in the
    problem. In this example, a possible data file is:

        300
        20
        0
        70

Usually, files `containers.txt` and `items.txt` do not change very
often, since they represent all the available items and
containers. Each packing problem consists of a different `data.txt`
file.

**Important**: All the unit measures for sizes must be the same.

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

    Be careful, since this script removes all ASY files. File
    `solution.pdf` will be generated.

  - `stats.csv`: CSV file containing statistical information about the
    problem. It follows the pattern:

        NC, NI, TCA, TIA, WR, CPU

    where `NC` is the *number of containers*, `NI` is the *number of
    items*, `TCA` is the *total area of the containers*, `TIA` is the
    *total area of the items*, `WR` is the *waste ratio* in percents
    `(100 * (TCA / TIA - 1))` and `CPU` is the *CPU time* in
    milliseconds.

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

## The *id* parameter <A ID="ID"></A>

The *id* parameter was introduced to describe which items can be
placed in which containers. Thus, the *id*s of item types and
containers must be related. The idea is that an item of type with *id*
`X` can be packed into containers of *id* `Y` if and only if `Y >=
X`. In addition, if a container of type `Y` was used to pack items,
then it *must have* a least one item of type with id `X = Y`.

> For example, items with *id* `1` can be packed into containers of
> *id* `1` and `2`, but **cannot** be packed into container with *id*
> `0`! Conversely, if a container with *id* `2` was selected, it must
> have at least one item of type with *id* `2`!

Let's recall the [full example](#setting) above and insert some
*id*s. In this new example, items of type 1 can only be packed into
containers of type 2 (smallest containers).

  - `containers.txt`:

        2
       	40.0 30.0 0
        28.5 17.5 1

  - `items.txt`:

        4
        4 4 1
        5.7 5.7 0
        5.7 5.7 0
        7.5 7.5 0

  - `data.txt`:

        300
        20
        0
        70

The solution obtained for the above problem is

    2,   28,    0,    0,    0,
    2,   28,    0,    0,    0,
    2,   28,    0,    0,    0,
    2,   28,    0,    0,    0,
    2,   28,    0,    0,    0,
    2,   28,    0,    0,    0,
    2,   28,    0,    0,    0,
    2,   28,    0,    0,    0,
    2,   28,    0,    0,    0,
    2,   28,    0,    0,    0,
    1,    0,    0,    0,   20,
    1,    0,    0,    0,   20,
    1,    0,    0,    0,   20,
    2,   20,    0,    0,    2,
    1,    0,   20,    0,    8,

and the images generated are

![][id_sol01] ![][id_sol02] ![][id_sol03]
![][id_sol04] ![][id_sol05] ![][id_sol06]
![][id_sol07] ![][id_sol08] ![][id_sol09]
![][id_sol10] ![][id_sol11] ![][id_sol12]
![][id_sol13] ![][id_sol14] ![][id_sol15]

We can see that the red items are not packed into the larger
containers. On the other hand, the other items (for example, the
purple ones) were packed into both containers. We also can see that
there is no small container (which has *id* 1) with only purple
and blue items (which have *id* 0).

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

## Caching results <A ID='cache'></A>

Given a set of type of items and containers, if many packing problems
are thought to be solved, where only the number of items changes, then
it may be interesting to cache each result. Pack-U allows the storage
of the obtained results, which means that, if the solution of an
already cached instance is asked, the computation time is almost 0.

The current implementation of cache in Pack-U uses the programming
language *Python 3*, the non-relational database *couchdb* and its
interface to Python 3. In Linux systems you can easily install the
necessary packages with commands:

    sudo aptitude install python3 couchdb pip3
    sudo -H pip3 install CouchDB

Then, build Pack-U executable

    make packu

go to the `build` directory, create all the necessary [configuration
files](#settings) and, instead of running the executable, call the
**Python executable**

    ../src/python/packu.py

In order to see the cache working, simply try to solve a time
consuming instance twice.

**Important**: It makes no sense to solve instances related to
  different sets of items and containers using the same cache. It is
  possible to receive totally wrong results! When changing the number
  of types of items, it is necessary to delete the previous
  cache. When changing only the number of types of containers it is
  possible to keep the cache, but one has to solve each stored problem
  again.

## A full example: website

In order to illustrate the possibilities of Pack-U, we have developed
a simple, full functional example, of a website. The example uses
Python 3 package *web.py* (version 0.40.dev0) in addition to the
packages used for [caching results](#cache)

    sudo aptitude install python3 couchdb python3-pip
    sudo -H pip3 install CouchDB
    sudo -H pip3 install web.py==0.40.dev0

Then, it is necessary to add directory `pack-u/src/python` to the list
of Python's library directories, what can be done in Linux with command

    export PYTHONPATH='path_to_pack-u'/pack-u/src/python

where `path_to_pack-u` is the location of Pack-U directory in your
computer. Build Pack-U, go to `build` directory and copy all files and
directories from `examples/site/`

    make packu
    cd build/
    cp ../examples/site/* . -r

Finally, the server is started with command

    python3 site.py

Open any browser and go to `http://127.0.0.1:8080/` . It is possible to
see the cache working with this full example. Also, it is possible to
change the number of types of items and the web page will automatically
follow the new configuration. Do not forget to clean the cache when
changing the problem's configuration.

## Improvements
  - **January, 2018**: Cache, site example

  - **November, 2017**: Items and containers have priority
      identification.
  - **July, 2017**: First version

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

[id_sol01]: docs/images/id_sol001.png
[id_sol02]: docs/images/id_sol002.png
[id_sol03]: docs/images/id_sol003.png
[id_sol04]: docs/images/id_sol004.png
[id_sol05]: docs/images/id_sol005.png
[id_sol06]: docs/images/id_sol006.png
[id_sol07]: docs/images/id_sol007.png
[id_sol08]: docs/images/id_sol008.png
[id_sol09]: docs/images/id_sol009.png
[id_sol10]: docs/images/id_sol010.png
[id_sol11]: docs/images/id_sol011.png
[id_sol12]: docs/images/id_sol012.png
[id_sol13]: docs/images/id_sol013.png
[id_sol14]: docs/images/id_sol014.png
[id_sol15]: docs/images/id_sol015.png

[algencan]: http://www.ime.usp.br/~egbirgin/tango
