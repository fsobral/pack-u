#!/usr/bin/python3

import logging


logger = logging.getLogger('packu')


class PackuError(Exception):

    def __init__(self):

        pass


class Item:

    UIDFMT = '{0:03d}'

    def __init__(self, ile, iwi, iid, uid=0):

        self.length_ = ile

        self.width_ = iwi

        self.id_ = iid

        self.uid_ = uid

    def getUid(self):

        return self.uid_

    def getArea(self):

        return self.width_ * self.length_

    def __str__(self):

        return "{0:5.2f} {1:5.2f} {2:3d}".format(
            self.length_, self.width_, self.id_)

    def __eq__(self, other):

        if type(other) is type(self):

            return self.__dict__ == other.__dict__

        return False

    def __hash__(self):

        return self.getUid()


class Container(Item):

    def __init__(self, cle, cwi, cid, uid=0):

        super().__init__(cle, cwi, cid, uid=uid)

    def how_many_items(self, item):

        if item.id_ == self.id_:

            nL = self.length_ // item.length_

            nW = self.width_ // item.width_

            return int(nL * nW)

        return 0


def parse_items_files(filename):

    """

    This function opens the file associated with the type of items,
    parses it and creates a list of items. Returns this list of items.

    """

    nItemsTypes = 0

    item_list = []

    itemUid = 0

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
                    itemUid += 1

                    item = Item(float(tokens[0]), float(tokens[1]),
                                int(tokens[2]), uid=itemUid)

                    print("INFO: Created item " + str(item))

                    item_list.append(item)

                except Exception as e:

                    print("ERROR: Item type format: length width id. Found: " +
                          line)

                    break

        return item_list


def parse_containers_files(filename):

    """

    This function opens the file associated with the type of
    containers, parses it and creates a list of containers. Returns
    this list of containers.

    """

    nContTypes = 0

    cont_list = []

    contUid = 0

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
                    contUid += 1

                    cont = Container(float(tokens[0]), float(tokens[1]),
                                     int(tokens[2]), uid=contUid)

                    print("INFO: Created container " + str(cont))

                    cont_list.append(cont)

                except Exception as e:

                    print("ERROR: Container type format: length width id. " +
                          "Found: " + line)

                    break

        return cont_list


def load_items_to_place(datafile, numberOfItems):

    """

    This function returns a list with the quantity of each type of item
    to be packed. Raise an PackuError if an inconsistent file was found.

    """

    with open(datafile, "r") as fp:

        items_to_place = []

        for line in fp:

            try:

                nit = int(line)

            except Exception:

                print("ERROR: Inconsistent data file {0:s}.".
                      format(datafile))

                raise PackuError()

            else:

                if nit < 0:

                    print("ERROR: Inconsistent data file {0:s}.".
                          format(datafile))

                    raise PackuError()

            items_to_place.append(nit)

    if len(items_to_place) != numberOfItems:

        print("ERROR: Wrong number of item types in file {0:s}.".
              format(datafile))

        raise PackuError()

    return items_to_place


def calculate_maximum(items_list, cont_list):
    """

    This function calculates which available containers can pack
    the maximum number of the same items, using a simple heuristic.

    Returns a map item -> (container, number of items) or raise an
    exception if something goes wrong.

    """

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

            print("ERROR: Item {0:d} does not fit in any container.".
                  format(item.getUid()))

            raise PackuError()

        cont_for_items[item] = (maxItCont, maxItems)

        logger.debug('{0:5d} items of type {1:3d} fit in container {2:3d}'.
                     format(maxItems, item.getUid(), maxItCont.getUid()))

    return cont_for_items


def reduce(items_list, itcont_map, items_to_place, remainingLevel=0):

    """

    This function receives a list of items types, the map relating
    items with the best container and a list with the number of items
    of each type to be packed.

    'remainingLevel' is the level of remaining items left. 0 means
    maximum packing.

    Returns two lists:
    - a list with the number of containers used to reduce each type of item
    - a list with the number of remaining items of each type to be packed
      after using the heuristic.

    """

    remaining = []

    allocated = []

    for i in range(0, len(items_list)):

        it = items_list[i]

        nit = items_to_place[i]

        maxcont, maxit = itcont_map[it]

        ncont = nit // maxit

        # Apply remainingLevel strategy
        ncont = max(0, ncont - remainingLevel)

        remit = nit - ncont * maxit

        # Save remaining items to be packed
        remaining.append(remit)

        # Save allocated items and container
        allocated.append(ncont)

        strFormat = "INFO: Packed {0:d} items of type {1:d} in {2:d} " + \
                    "containers of type {3:d}"

        print(strFormat.format(ncont * maxit, it.getUid(), ncont,
                               maxcont.getUid()))

    return allocated, remaining


def draw_allocated(items_list, cont_list, itcont_map, allocated, drawFile):

    """
    This function draws in ASY format all the solutions that were obtained
    by the Python reduction strategy.

    """

    fmtC = "draw(box((0,0), ({0:10.5f},{1:10.5f})), " + \
           "currentpen + dashed + 2);\n"
    fmtB = "drawBox(({0:10.5f},{1:10.5f}), {2:10.5f}, {3:10.5f}, " + \
           "cor({4:2d}), currentpen, \"{5:4d}\");\n"

    with open(drawFile, "w") as df:

        df.write("import packlib;\nsettings.outformat = \"pdf\";\n\n")

        for i in range(0, len(items_list)):

            item = items_list[i]

            container, nitems = itcont_map[item]

            scale = max(1.0E-4, min(1.0, 15.0 / container.length_,
                                    30.0 / container.width_))

            for cnt in range(0, allocated[i]):

                if (i > 0 or cnt > 0):

                    df.write("\nnewpage();\n\n")

                df.write("unitsize({0:10.5f}cm);\n".format(scale))

                dx = 0.0

                dy = 0.0

                for j in range(0, nitems):

                    if (dy + item.width_ > container.width_):

                        dx += item.length_

                        dy = 0.0

                    df.write(fmtB.format(dx, dy, item.length_, item.width_,
                                         item.getUid(), item.getUid()))

                    dy += item.width_

                df.write(fmtC.format(container.length_, container.width_))


def save_remaining_items(datafile, remaining_list):

    """

    This function saves the remaining items in a data file.

    """

    with open(datafile, 'w') as fp:

        for nit in remaining_list:

            fp.write('{0:d}\n'.format(nit))


def recursive_heuristic(items_list, items_to_place, containers_list):

    """

    This is the implementation of the Fortran heuristic.

    """

    tmp = zip(items_list, items_to_place)

    tmp.sort(key=lambda x: x[0], reverse=True)

    ziter = zip(*tmp)

    oIList = [i for i in next(ziter)]

    oItoPlace = [i for i in next(ziter)]
    
    
def _call_rec_heuristic(oIList, oItoPlace, pcItem,
                        xo, yo, length, width, pos=None):

    # Check if have to store the positions
    if (pos is not None):

        if (len(pos) < len(oIList)):

            pos = None

            logger.warning("Invalid 'pos' list. Setting to None.")

        else:

            for l in pos:

                if (type(l) is not list):

                    logger.warning("Invalid 'pos' list. Setting to None.")

                    pos = None

                    break

    # Check if there are items to pack
    if (pcItem >= len(oIList)):

        logger.debug("Returning. No more item types to place.")

        return

    dy = yo
    dx = xo

    # Get the current item
    it = oIList[pcItem]

    logger.debug("Alocating item %d: (%s)", it.getUid(), it)

    # Check if at least one item fits
    if (yo + it.width_ > width or xo + it.length_ > length):

        logger.debug("Returning. No more room for item %d.",
                     it.getUid())

        return

    while (oItoPlace[pcItem] > 0):

        if (dy + it.width_ > width):

            dy = yo

            dx += it.length_

        if (dx + it.length_ > length):

            logger.debug("No more space for item type %d.", it.getUid())

            break

        if (pos is not None):

            pos[pcItem].append((dx, dy))

        logger.debug("Placed item %d at position %6.2f, %6.2f",
                     it.getUid(), dx, dy)

        dy += it.width_

        oItoPlace[pcItem] -= 1

    # If the number of current items has finished (or they do not fit
    # in the available space) and there is an unfinished column, then
    # it is necessary to work in the reduced space. Otherwise, there
    # is full room to place the remaining items.
    if (dy > yo):

        logger.debug("Solving reduced problem of size %6.2f x %6.2f"
                     " with %d types of items.",
                     it.length_, width - dy,
                     len(oItoPlace) - (pcItem + 1))

        _call_rec_heuristic(oIList, oItoPlace, pcItem + 1,
                            dx, dy, dx + it.length_, width, pos=pos)

        dy = yo
        dx += it.length_

    logger.debug("Solving remaining problem of size %6.2f x %6.2f with %d"
                 " types of items.", length - dx, width - dy,
                 len(oItoPlace) - (pcItem + 1))
    _call_rec_heuristic(oIList, oItoPlace, pcItem + 1,
                        dx, dy, length, width, pos=pos)

    return
