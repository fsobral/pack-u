#!/usr/bin/python3


class Item:

    def __init__(self, ile, iwi, iid):

        self.length_ = ile

        self.width_ = iwi

        self.id_ = iid

    def __str__(self):

        return "{0:5.2f} {1:5.2f} {2:3d}".format(
            self.length_, self.width_, self.id_)

    def __eq__(self, other):

        if type(other) is type(self):

            return self.__dict__ == other.__dict__

        return False


class Container:

    def __init__(self, cle, cwi, cid):

        self.length_ = cle

        self.width_ = cwi

        self.id_ = cid

    def __str__(self):

        return "{0:5.2f} {1:5.2f} {2:3d}".format(
            self.length_, self.width_, self.id_)

    def how_many_items(self, item):

        if item.id_ <= self.id_:

            nL = self.length_ / item.length_

            nW = self.width_ / item.width_

            return nL * nW

        return 0


def parse_items_files(filename):

    """This function opens the file associated with the type of items,
    parses it and creates a list of items. Returns this list of items.

    """

    nItemsTypes = 0

    item_lists = []

    with open(filename, "r") as fp:

        for line in fp:

            if line.startswith('#'):

                continue

            tokens = line.split()

            if nItemsTypes <= 0:

                try:

                    nItemsTypes = int(tokens[0])

                except Exception as e:

                    print("The first parameter is the number of items.")

                    break

            else:

                try:

                    item = Item(float(tokens[0]), float(tokens[1]),
                                int(tokens[2]))

                    print("Created item " + str(item))

                    item_lists.append(item)

                except Exception as e:

                    print("Item type format: length width id. Found: " +
                          line)

                    break

        return item_lists
