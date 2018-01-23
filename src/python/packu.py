#!/usr/bin/python3

import logging
from packu import packurun


# # Parameters

# ITEMS = 'items.txt'

# CONTAINERS = 'containers.txt'

# DATA = 'data.txt'

# DATATMP = 'data.txt.tmp'

# SOLFILE = 'solution.csv'

# STATSFILE = 'stats.csv'


# Run Packu

if __name__ == "__main__":

    # Set up logging

    logger = logging.getLogger('packu')

    handler = logging.StreamHandler()

    handler.setFormatter(logging.Formatter('%(levelname)s: %(message)s'))

    logger.addHandler(handler)

    logger.setLevel(logging.DEBUG)

    # Run Pack-U

    packurun.runPacku()
