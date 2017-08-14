#!/usr/bin/python3

""""

This file is an example of how to use cache to make Pack-U runs
faster. It uses the Apache CouchDB database and Python.

"""

import couchdb
import os

DBNAME = 'packu_cache'


def getOrCreateDB(dbname):

    couch = couchdb.Server()

    if dbname not in couch:

        couch.create(dbname)

    return couch[dbname]


def getSolution(key):

    if key is None:

        return None

    db = getOrCreateDB(DBNAME)

    if key in db:

        return db[key]

    else:

        return None

def toFile(sol, filename):

    with open(filename, 'w') as fp:

        for s in sol['solution']:

            fp.write(s)

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


if __name__ == '__main__':

    key = ''

    with open('data.txt', 'r') as fp:

        for line in fp:

            item = int(line)

            key = key + str(item) + ','

    # Remove the last comma
    key = key[:-1]

    sol = getSolution(key)

    if sol is None:

        print('Solution not found in cache. Will run Pack-U.')

        os.system('./packu')

        if saveSolution(key, 'solution.csv', 'stats.csv'):

            print('New solution saved to cache.')

    else:

        print('Solution retrieved from cache.')

        toFile(sol, 'solution.csv')
