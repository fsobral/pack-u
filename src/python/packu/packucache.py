#!/usr/bin/python3

""""

This file contains functions for using cache in Pack-U.
It uses the Apache CouchDB database and Python.

"""

import pycouchdb as couchdb

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


def toFile(sol, solfilename, statsfilename):

    with open(solfilename, 'w') as fp:

        for s in sol['solution']:

            fp.write(s)

    with open(statsfilename, 'w') as fp:

        fp.write(sol['stats'].rstrip() + ' *\n')


def saveSolution(key, solfilename, statsfilename):

    if key is None:

        return None

    db = getOrCreateDB(DBNAME)

    if key not in db:

        s = []

        with open(solfilename, 'r') as fp:

            for line in fp:

                s.append(line)

        stats = ''

        with open(statsfilename, 'r') as fp:

            stats = fp.readline()

        # print('Added key ' + key)

        db.save({'_id': key, 'solution': s, 'stats': stats})

        return True

    return False
