#!/usr/bin/python3

import unittest
import re
# Working with files and directories
import os

from packu import Item, Container, parse_items_files, parse_containers_files


class IntegrationTests(unittest.TestCase):

    def __init__(self, kwargs):

        self.TESTPATH = './'

        super().__init__(kwargs)

    def test_problems(self):

        tstPattern = re.compile("^t[0-9]+$")

        test_dirs = os.listdir(self.TESTPATH)

        for t in [td for td in test_dirs if tstPattern.match(td) is not None]:

            os.system('cp {0:s}/{1:s}/*.txt . -v'.format(
                self.TESTPATH, t))

            os.system('../build/packu')

            with open('solution.csv', 'r') as pfp:

                with open('true_sol.txt', 'r') as tfp:

                    for packu_lines in pfp:

                        a = packu_lines.split(',')

                        b = tfp.readline().split(',')

                        self.assertEqual(
                            len(b), len(a),
                            msg='{0:s}: Different files.'.format(t))

                        for i in range(0, len(a)):

                            self.assertEqual(
                                b[i].strip(), a[i].strip(),
                                '{0:s}: Different files.'.format(t))

                    self.assertEqual('', tfp.readline())


class TestFileLoader(unittest.TestCase):

    def create_file(self, filename, obj_list):

        with open(filename, "w") as fp:

            fp.write(str(len(obj_list)) + "\n")

            for item in obj_list:

                fp.write(str(item) + "\n")

    def test_load_item_file(self):

        item1 = Item(2, 3, 1)

        item_list = [item1]

        self.create_file("items.txt", item_list)

        result_list = parse_items_files("items.txt")

        self.assertEqual(1, len(result_list))

        self.assertEqual(item_list, result_list)

    def test_load_container_file(self):

        cont1 = Container(5, 5, 0)

        cont_list = [cont1]

        self.create_file("containers.txt", cont_list)

        result_list = parse_containers_files("containers.txt")

        self.assertEqual(1, len(result_list))

        self.assertEqual(cont_list, result_list)

    def test_load_file_comments(self):

        item = Item(5, 5, 0)

        item_list = [item]

        with open("items.txt", "w") as fp:

            fp.write("# This is a comment\n1\n")

            fp.write("#This is another comment\n")

            fp.write(str(item) + " # Another comment")

        self.create_file("items.txt", item_list)

        result_list = parse_items_files("items.txt")

        self.assertEqual(1, len(result_list))

        self.assertEqual(item_list, result_list)


class TestContainer(unittest.TestCase):

    def test_print_item(self):

        container = Container(2, 3, 1)

        s = str(container).split()

        self.assertEqual("2.00", s[0])

        self.assertEqual("3.00", s[1])

        self.assertEqual("1", s[2])

    def test_equals(self):

        container1 = Container(1, 1, 1)

        container2 = Container(1, 1, 1)

        self.assertEqual(container1, container1)

        self.assertEqual(container1, container2)

    def test_not_equals(self):

        container1 = Container(1, 2, 1)

        container2 = Container(1, 2, 0)

        self.assertNotEqual(container1, container2)

    def test_number_of_items(self):

        item1 = Item(2, 2, 0)

        item2 = Item(1, 1, 1)

        container1 = Container(8, 8, 0)

        container2 = Container(6, 6, 1)

        self.assertEqual(16, container1.how_many_items(item1))

        self.assertEqual(9, container2.how_many_items(item1))

        self.assertEqual(0, container1.how_many_items(item2))

        self.assertEqual(36, container2.how_many_items(item2))


class TestItem(unittest.TestCase):

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


if __name__ == '__main__':

    unittest.main()
