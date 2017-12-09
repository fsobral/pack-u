#!/usr/bin/python3

import unittest
import re
# Working with files and directories
import os

from packu import Item, Container, parse_items_files, parse_containers_files, \
    calculate_maximum, load_items_to_place, PackuError, reduce


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


class TestReducer(unittest.TestCase):

    def test_calculate_max_items(self):

        items_list = [Item(1, 1, 0),
                      Item(2, 2, 2)]

        cont_list = [Container(10, 10, 0),
                     Container(40, 40, 2)]

        itmap = calculate_maximum(items_list, cont_list)

        (mc, mi) = itmap[items_list[0]]

        self.assertEqual(cont_list[0], mc)

        self.assertEqual(100, mi)

        (mc, mi) = itmap[items_list[1]]

        self.assertEqual(cont_list[1], mc)

        self.assertEqual(400, mi)

    def test_reduce(self):

        items_list = [Item(1, 1, 0, 1),
                      Item(2, 2, 2, 2)]

        itmap = dict()

        itmap[items_list[0]] = (Container(10, 10, 0), 100)

        itmap[items_list[1]] = (Container(40, 40, 2), 400)

        items_data = [100, 810]

        remaining = reduce(items_list, itmap, items_data)

        self.assertEquals(len(items_data), len(remaining))

        self.assertEquals(0, remaining[0])

        self.assertEquals(10, remaining[1])


class TestFileLoader(unittest.TestCase):

    def create_file(self, filename, obj_list):

        with open(filename, "w") as fp:

            fp.write(str(len(obj_list)) + "\n")

            for item in obj_list:

                fp.write(str(item) + "\n")

    def test_load_datafile(self):

        it_list = [10, 0, 0, 20]

        with open("data.txt", "w") as fp:

            for it in it_list:

                fp.write(str(it) + "\n")

        ret_list = load_items_to_place("data.txt", len(it_list))

        self.assertEqual(it_list, ret_list)

    def test_load_datafile_wrong_number(self):

        it_list = [10, 0, 0, 20]

        with open("data.txt", "w") as fp:

            for it in it_list:

                fp.write(str(it) + "\n")

        with self.assertRaises(PackuError):

            load_items_to_place("data.txt", 3)

        self.assertEqual(it_list, load_items_to_place("data.txt", 4))

    def test_load_datafile_wrong_value(self):

        it_list = [10, 0, 0, -1]

        with open("data.txt", "w") as fp:

            for it in it_list:

                fp.write(str(it) + "\n")

        with self.assertRaises(PackuError):

            load_items_to_place("data.txt", len(it_list))

    def test_load_item_uid(self):

        item_list = [Item(2, 3, 1, 1),
                     Item(2, 3, 1, 2)]

        self.create_file("items.txt", item_list)

        result_list = parse_items_files("items.txt")

        self.assertEqual(2, len(result_list))

        self.assertEqual(item_list, result_list)

    def test_load_container_uid(self):

        cont_list = [Container(2, 3, 1, 1),
                     Container(2, 3, 0, 2)]

        self.create_file("containers.txt", cont_list)

        result_list = parse_containers_files("containers.txt")

        self.assertEqual(2, len(result_list))

        self.assertEqual(cont_list, result_list)

    def test_load_item_file(self):

        item1 = Item(2, 3, 1, 1)

        item_list = [item1]

        self.create_file("items.txt", item_list)

        result_list = parse_items_files("items.txt")

        self.assertEqual(1, len(result_list))

        self.assertEqual(item_list, result_list)

    def test_load_container_file(self):

        cont1 = Container(5, 5, 0, 1)

        cont_list = [cont1]

        self.create_file("containers.txt", cont_list)

        result_list = parse_containers_files("containers.txt")

        self.assertEqual(1, len(result_list))

        self.assertEqual(cont_list, result_list)

    def test_load_file_comments(self):

        item = Item(5, 5, 0, 1)

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

        self.assertEqual(0, container2.how_many_items(item1))

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
