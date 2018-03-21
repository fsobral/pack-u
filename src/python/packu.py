#!/usr/bin/python3

import logging
# For safe copying
import shutil
from packu import packurun, packureduce

# This map converts the number A of items of type i to B items of type
# j plus C items of type i as a map (B, C). The numbers i and j used
# as keys are in **Python mode**, which means that the first item is
# 0.
fisiomap = {
    2: (4, lambda x: (int(round(x / 4, 0)), x % 4))
    }


def convert_items(ITEMS, DATA, DATATMP):
    """This function convert some items using the given map. It saves the
    original data in file DATATMP, which has to be restored after the
    packing process.

    """

    items_list = packureduce.parse_items_files(ITEMS)

    nof_items = packureduce.load_items_to_place(DATA, len(items_list))

    for i in range(0, len(items_list)):

        if i in fisiomap:

            (j, convert) = fisiomap[i]

            (b, c) = convert(nof_items[i])

            logger.debug("Converted %d items of type %d in %d items "
                         "of type %d and %d items of type %d.", nof_items[i],
                         i, c, i, b, j)

            nof_items[i] = c

            nof_items[j] += b

    shutil.copy2(DATA, DATATMP)

    packureduce.save_remaining_items(DATA, nof_items)


def restore_items(DATA, DATATMP):
    """This function restores the saved file DATATMP to its original
    name.

    """

    shutil.copy2(DATATMP, DATA)


# Run Packu

if __name__ == "__main__":

    # Set up logging

    logger = logging.getLogger('packu')

    handler = logging.StreamHandler()

    handler.setFormatter(logging.Formatter('%(levelname)s: %(message)s'))

    logger.addHandler(handler)

    logger.setLevel(logging.DEBUG)

    # Preprocessing

    try:

        convert_items(packurun.ITEMS, packurun.DATA, "data.fisio.txt")

    except:

        logger.error("Error when converting problem data.")

        restore_items(packurun.DATA, "data.fisio.txt")

    # Run Pack-U

    packurun.runPacku()

    # Restore

    restore_items(packurun.DATA, "data.fisio.txt")
