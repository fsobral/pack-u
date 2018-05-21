#!/usr/bin/python3

import unittest
import re
# Working with files and directories
import os


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

    def test_problems_python(self):

        tstPattern = re.compile("^t[0-9]+$")

        test_dirs = os.listdir(self.TESTPATH)

        for t in [td for td in test_dirs if tstPattern.match(td) is not None]:

            os.system('cp {0:s}/{1:s}/*.txt . -v'.format(
                self.TESTPATH, t))

            os.system('../src/python/packu.py')

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
