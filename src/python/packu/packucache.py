#!/usr/bin/python3

""""

This file contains functions for using cache in Pack-U.
It uses the Apache CouchDB database and Python.

"""

import couchdb

DBNAME = 'packu_cache'


def getOrCreateDB(dbname):

    """

    Gets Pack-U database or creates one, if necessary.

    """

    couch = couchdb.Server()

    if dbname not in couch:

        couch.create(dbname)

    return couch[dbname]


def createKey(problemData):

    """

    Creates the key for a given 'problemData' list of number
    of item types.

    """

    key = ''

    for itData in problemData:

        key += str(itData) + ','

    # Remove the last comma

    return key[:-1]


def getSolution(key):

    """

    Returns a dictionary

    {'id_': key, 'solution': list, 'stats': string}

    associated with the given solution, or None if no key
    has been found.

    """

    if key is None:

        return None

    db = getOrCreateDB(DBNAME)

    if key in db:

        return db[key]

    else:

        return None


def formatList(container, items_to_place):

    """

    Receives a container and a list with the number of items to
    be placed on it. Returns a string representing one line of
    the solution file.

    """

    s = '{0:3d},'.format(container.getUid())

    for nit in items_to_place:

        s += '{0:5d},'.format(nit)

    s += '\n'

    return s


def updateSol(sol, number_containers, items_list, itcont_map):

    """

    This function updates the solution 'sol' retrived by the database.
    This solution is a dictionary, and the field used is sol['solution']
    and is given by a list of strings representing the lines of the solution
    file.

    """

    numberOfItems = len(items_list)

    items_to_place = [0 for i in range(0, numberOfItems)]

    for i in range(0, numberOfItems):

        container, nit = itcont_map[items_list[i]]

        items_to_place[i] = nit

        formattedList = formatList(container, items_to_place)

        for j in range(0, number_containers[i]):

            sol['solution'].append(formattedList)

        items_to_place[i] = 0


def toFile(sol, solfilename, statsfilename):

    with open(solfilename, 'w') as fp:

        for s in sol['solution']:

            fp.write(s)

    with open(statsfilename, 'w') as fp:

        fp.write(sol['stats'].rstrip() + ' *\n')


def fromFile(key, solfilename, statsfilename):

    """

    Extracts a solution from output files generated
    by the Fortran program.

    """

    s = []

    with open(solfilename, 'r') as fp:

        for line in fp:

            s.append(line)

    stats = ''

    with open(statsfilename, 'r') as fp:

        stats = fp.readline()

    sol = {'_id': key,
           'solution': s,
           'stats': stats}

    return sol


def saveSolution(key, sol):

    if key is None:

        return None

    db = getOrCreateDB(DBNAME)

    if key not in db:

        print('INFO: Added key ' + key)

        db.save(sol)

        return True

    return False
