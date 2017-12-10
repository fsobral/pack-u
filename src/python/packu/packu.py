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

    STATSFILE = 'stats.csv'

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

        # TODO: save new solution to DB

        pc.updateSol(sol, number_containers, items_list, itmap)

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

    # Cache

    key = pc.createKey(items_to_place)

    sol = pc.getSolution(key)

    if sol is None:

        print('Solution not found in cache. Will run Pack-U.')

        # Call Fortran, if necessary

        try:

            subprocess.run(['./packu'], stdout=subprocess.PIPE)

        except KeyboardInterrupt:

            os.remove(DATA)

            os.rename(DATATMP, DATA)

            exit

        if pc.saveSolution(key, 'solution.csv', 'stats.csv'):

            print('New solution saved to cache.')

    else:

        print('Solution retrieved from cache.')

        pc.toFile(sol, 'solution.csv', 'stats.csv')
