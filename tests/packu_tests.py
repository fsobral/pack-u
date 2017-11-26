#!/usr/bin/python3

import unittest

from packu import Item, parse_items_files


class TestItem(unittest.TestCase):

    def create_items_files(self, item_list):

        with open("items.txt", "w") as fp:

            fp.write(str(len(item_list)) + "\n")

            for item in item_list:

                fp.write(str(item) + "\n")

    def test_print_item(self):

        item = Item(2, 3, 1)

        s = str(item).split()

        self.assertEqual("2.00", s[0])

        self.assertEqual("3.00", s[1])

        self.assertEqual("1", s[2])

    def test_equals(self):

        item1 = Item(1, 1, 1)

        item2 = Item(1, 1, 1)

        self.assertEqual(item1, item1)

        self.assertEqual(item1, item2)

    def test_not_equals(self):

        item1 = Item(1, 2, 1)

        item2 = Item(1, 2, 0)

        self.assertNotEqual(item1, item2)

    def test_load_file(self):

        item1 = Item(2, 3, 1)

        item_list = [item1]

        self.create_items_files(item_list)

        result_list = parse_items_files("items.txt")

        self.assertEqual(1, len(result_list))

        self.assertEqual(item_list, result_list)


if __name__ == '__main__':

    unittest.main()
