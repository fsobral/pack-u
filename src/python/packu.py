#!/usr/bin/python3


class PackuError(Exception):

    def __init__(self):

        pass


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


class Container(Item):

    def __init__(self, cle, cwi, cid):

        super().__init__(cle, cwi, cid)

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

    item_list = []

    with open(filename, "r") as fp:

        for line in fp:

            if line.startswith('#'):

                continue

            tokens = line.split()

            if nItemsTypes <= 0:

                try:

                    nItemsTypes = int(tokens[0])

                except Exception as e:

                    print("ERROR: The first parameter is the number of items.")

                    break

            else:

                try:

                    item = Item(float(tokens[0]), float(tokens[1]),
                                int(tokens[2]))

                    print("INFO: Created item " + str(item))

                    item_list.append(item)

                except Exception as e:

                    print("ERROR: Item type format: length width id. Found: " +
                          line)

                    break

        return item_list


def parse_containers_files(filename):

    """This function opens the file associated with the type of
    containers, parses it and creates a list of containers. Returns
    this list of containers.

    """

    nContTypes = 0

    cont_list = []

    with open(filename, "r") as fp:

        for line in fp:

            if line.startswith('#'):

                continue

            tokens = line.split()

            if nContTypes <= 0:

                try:

                    nContTypes = int(tokens[0])

                except Exception as e:

                    print("ERROR: The first parameter is the number " +
                          "of containers.")

                    break

            else:

                try:

                    cont = Container(float(tokens[0]), float(tokens[1]),
                                     int(tokens[2]))

                    print("INFO: Created container" + str(cont))

                    cont_list.append(cont)

                except Exception as e:

                    print("ERROR: Container type format: length width id. " +
                          "Found: " + line)

                    break

        return cont_list


def calculate_maximum(itemfile, contfile):
    """This function calculates the which available containers can pack
       the maximum number of the same items, using a simple heuristic.

    Returns a map item -> (container, number of items) or raise an
    exception if something goes wrong.

    """

    items_list = parse_items_files(itemfile)

    cont_list = parse_containers_files(contfile)

    cont_for_items = {}

    for item in items_list:

        maxItems = 0
        maxItCont = None

        for cont in cont_list:

            howMany = cont.how_many_items(item)

            if howMany > maxItems:

                maxItems = howMany

                maxItCont = cont

        if maxItems is 0:

            print("ERROR: Item does not fit in any container.")

            raise PackuError()

        cont_for_items[item] = (maxItCont, maxItems)

    return cont_for_items


def reduce(datafile, items_list, itcont_map):

    with open(datafile, "r") as fp:

        for it in items_list:

            try:

                nit = int(fp.readline())

            except Exception:

                print("ERROR: Inconsistent data file {0:s}.".
                      format(datafile))

                raise PackuError()

            maxcont, maxit = itcont_map[it]

            (ncont, remit) = divmod(nit, maxit)

            print("Packed {0:d} items of type {1:d} in {2:d} " +
                  "containers of type {3:d}".
                  format(ncont * maxit, -1, ncont, -1))
