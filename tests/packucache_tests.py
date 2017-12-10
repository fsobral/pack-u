#!/usr/bin/python3

import unittest
# Packu package
from packu.packucache import createKey


class TestCache(unittest.TestCase):

    def test_create_key(self):

        noi_list = [0, 10, 20, 30, 0]

        self.assertEqual("0,10,20,30,0",
                         createKey(noi_list))


if __name__ == '__main__':

    unittest.main()
