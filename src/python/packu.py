#!/usr/bin/python3


import packureduce as pr
import os


# Run

if __name__ == "__main__":

    ITEMS = 'items.txt'

    CONTAINERS = 'containers.txt'

    DATA = 'data.txt'

    DATATMP = 'data.txt.tmp'

    # Load data

    items_list = pr.parse_items_files(ITEMS)

    containers_list = pr.parse_containers_files(CONTAINERS)

    items_to_place = pr.load_items_to_place(DATA, len(items_list))

    itmap = pr.calculate_maximum(items_list, containers_list)

    remaining_items = pr.reduce(items_to_place, itmap, items_to_place)

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

    # Run Fortran, if necessary
