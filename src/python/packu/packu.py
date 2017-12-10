#!/usr/bin/python3


import packureduce as pr
import packucache as pc
import os
import subprocess


# Run Packu

if __name__ == "__main__":

    # Parameters

    ITEMS = 'items.txt'

    CONTAINERS = 'containers.txt'

    DATA = 'data.txt'

    DATATMP = 'data.txt.tmp'

    SOLFILE = 'solution.csv'

    STATSFILE = 'stats.txt'

    # Load data

    items_list = pr.parse_items_files(ITEMS)

    containers_list = pr.parse_containers_files(CONTAINERS)

    items_to_place = pr.load_items_to_place(DATA, len(items_list))

    # First cache: check if the whole problem has already been
    # solved

    key = pc.createKey(items_to_place)

    sol = pc.getSolution(key)

    if sol is not None:

        print('INFO: Solution retrieved from cache.')

        pc.toFile(sol, SOLFILE, STATSFILE)

        exit

    # Reduce the problem

    itmap = pr.calculate_maximum(items_list, containers_list)

    number_containers, remaining_items = \
        pr.reduce(items_to_place, itmap, items_to_place)

    # Second cache: check if the reduced problem has already been
    # solved

    key = pc.createKey(remaining_items)

    sol = pc.getSolution(key)

    if sol is not None:

        print('INFO: Solution partially retrieved from cache.')

        pc.updateSol(sol, number_containers, items_list, itmap)

        sol['_id'] = key

        pc.saveSolution(key, sol)

        pc.toFile(sol, SOLFILE, STATSFILE)

        exit

    # Save original data and create reduced problem

    try:

        os.remove(DATATMP)

        os.rename(DATA, DATATMP)

    except OSError as io:

        print('ERROR: Problems saving original data. ' +
              'Possible loss of the file.')

        exit

    pr.save_remaining_items(DATA, remaining_items)

    # Call Fortran
    # TODO: if it takes too long, apply recursive heuristic.

    print('INFO: Solution not found in cache. Will run Pack-U.')

    try:

        subprocess.run(['./packu'], stdout=subprocess.PIPE)

    except KeyboardInterrupt:

        os.remove(DATA)

        os.rename(DATATMP, DATA)

        exit

    sol = pc.fromFile(SOLFILE, STATSFILE)

    if pc.saveSolution(key, sol):

        print('INFO: New solution saved to cache.')

    else:

        print('INFO: Solution retrieved from cache.')

        pc.toFile(sol, SOLFILE, STATSFILE)
