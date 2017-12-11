#!/usr/bin/python3

import unittest
# Packu package
import packu.packureduce as pr
from packu.packucache import createKey, updateSol, \
    formatList, fromFile, STATSFMT


class TestCache(unittest.TestCase):

    def test_extract_from_file(self):

        l = [20, 30, 0, 100, 10]

        sol = {'_id': 12345,
               'solution': [],
               'stats': 'XXXXXXXXXXXX'}

        with open("solution.csv", "w") as fp:

            line = formatList(pr.Container(1, 1, 0, 1), l)

            for i in range(0, 5):

                sol['solution'].append(line)

                fp.write(line)

        with open("stats.csv", "w") as fp:

            fp.write(sol['stats'])

        retrievedSol = fromFile(sol['_id'],
                                'solution.csv',
                                'stats.csv')

        self.assertEqual(sol, retrievedSol)

    def test_create_key(self):

        noi_list = [0, 10, 20, 30, 0]

        self.assertEqual("0,10,20,30,0",
                         createKey(noi_list))

    def test_update_sol(self):

        sol = {}

        sol['solution'] = []

        sol['stats'] = STATSFMT.format(0, 0, 0.0, 0.0, 0.0, 0.0)

        items = [pr.Item(1, 1, 0, 1),
                 pr.Item(3, 3, 0, 2)]

        container = pr.Container(15, 15, 0, 1)

        itmap = dict()

        itmap[items[0]] = (container, 15 * 15)
        itmap[items[1]] = (container, 5 * 5)

        updateSol(sol, [2, 1], items, itmap)

        solFormat = '{0:3d},{1:5d},{2:5d},\n'

        self.assertEqual(3, len(sol['solution']))

        for i in range(0, 2):

            self.assertEqual(
                solFormat.format(container.getUid(),
                                 15 * 15, 0),
                sol['solution'][i])

        for i in range(2, 3):

            self.assertEqual(
                solFormat.format(container.getUid(),
                                 0, 5 * 5),
                sol['solution'][i])


if __name__ == '__main__':

    unittest.main()
