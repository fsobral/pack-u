#!/usr/bin/python3

""""

This file contains functions for using cache in Pack-U.
It uses the Apache CouchDB database and Python.

"""

import couchdb

DBNAME = 'packu_cache'

# This format should follow the same Fortran format
STATSFMT = '{0:7d},{1:7d},{2:10.2f},{3:10.2f},{4:20.4f},{5:20.6f}'


def getOrCreateDB(dbname):

    """

    Gets Pack-U database or creates one, if necessary.

    """

    couch = couchdb.Server()

    if not couch.__nonzero__():

        return None

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

    if db is not None and key in db:

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
    This solution is a dictionary, and the field used are sol['solution'],
    given by a list of strings representing the lines of the solution
    file, and sol['stats'].

    """

    totItArea = 0.0

    totCoArea = 0.0

    nTotIt = 0

    nTotCo = 0

    numberOfItems = len(items_list)

    items_to_place = [0 for i in range(0, numberOfItems)]

    for i in range(0, numberOfItems):

        item = items_list[i]

        container, nit = itcont_map[item]

        items_to_place[i] = nit

        formattedList = formatList(container, items_to_place)

        nc = number_containers[i]

        for j in range(0, nc):

            sol['solution'].append(formattedList)

        items_to_place[i] = 0

        # Update stats information

        totItArea += nc * nit * item.getArea()

        totCoArea += nc * container.getArea()

        nTotIt += nit * nc

        nTotCo += nc

    # Update stats

    if 'stats' in sol:

        stoken = sol['stats'].split(',')

        nTotCo += int(stoken[0])

        nTotIt += int(stoken[1])

        totCoArea += float(stoken[2])

        totItArea += float(stoken[3])

        time = float(stoken[5])

        sol['stats'] = STATSFMT.format(
            nTotCo, nTotIt, totCoArea, totItArea,
            ((totCoArea / totItArea) - 1.0) * 100.0, time
        )

    else:

        print("WARNING: No 'stats' in sol!")


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

    if db is None:

        return False

    if key not in db:

        print('INFO: Added key ' + key)

        db.save(sol)

        return True

    return False
