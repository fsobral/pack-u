#!/usr/bin/python3


import packureduce as pr
import packucache as pc
import os
import subprocess
import logging


# Parameters

ITEMS = 'items.txt'

CONTAINERS = 'containers.txt'

DATA = 'data.txt'

DATATMP = 'data.txt.tmp'

SOLFILE = 'solution.csv'

STATSFILE = 'stats.csv'

# Run Packu

if __name__ == "__main__":

    # Set up logging

    logger = logging.getLogger('packu')

    logger.addHandler(logging.StreamHandler())

    logger.setLevel(logging.DEBUG)

    # Load data

    items_list = pr.parse_items_files(ITEMS)

    containers_list = pr.parse_containers_files(CONTAINERS)

    items_to_place = pr.load_items_to_place(DATA, len(items_list))

    # First cache: check if the whole problem has already been
    # solved

    key = pc.createKey(items_to_place)

    sol = pc.getSolution(key)

    if sol is not None:

        logger.debug('Solution retrieved from cache. Key: ' + key)

        pc.toFile(sol, SOLFILE, STATSFILE)

        exit()

    # Reduce the problem

    itmap = pr.calculate_maximum(items_list, containers_list)

    number_containers, remaining_items = \
        pr.reduce(items_list, itmap, items_to_place)

    # Second cache: check if the reduced problem has already been
    # solved

    redkey = pc.createKey(remaining_items)

    sol = pc.getSolution(redkey)

    if sol is not None:

        logger.debug('Solution partially retrieved from cache. Key: ' + redkey)

        pc.updateSol(sol, number_containers, items_list, itmap)

        sol['_id'] = key

        pc.saveSolution(key, sol)

        pc.toFile(sol, SOLFILE, STATSFILE)

        exit()

    # Save original data and create reduced problem

    try:

        os.rename(DATA, DATATMP)

    except OSError as io:

        try:

            os.remove(DATATMP)

            os.rename(DATA, DATATMP)

        except OSError:

            logger.error('Problems saving original data. ' +
                         'Possible loss of the file.')

            exit()

    pr.save_remaining_items(DATA, remaining_items)

    # Call Fortran
    # TODO: if it takes too long, apply recursive heuristic.

    logger.debug('Solution not found in cache. Will run Pack-U.')

    try:

        subprocess.run(['./packu'], stdout=subprocess.PIPE)

    except KeyboardInterrupt:

        os.remove(DATA)

        os.rename(DATATMP, DATA)

        exit()

    sol = pc.fromFile(key, SOLFILE, STATSFILE)

    if pc.saveSolution(key, sol):

        logger.debug('New solution saved to cache. Key: ' + key)

    else:

        logger.debug('Solution retrieved from cache.')

        pc.toFile(sol, SOLFILE, STATSFILE)
