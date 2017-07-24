#!/usr/bin/env python

import os
import sys
import sqlite3
import urllib
import unittest
import StringIO
from gzip import GzipFile

import xml.etree.ElementTree as ET
from parameterized import parameterized

xmlns = {
    '_': 'http://linux.duke.edu/metadata/common',
    'rpm': 'http://linux.duke.edu/metadata/rpm',
}

def primary_xml_gz(repo_url):
    response = urllib.urlopen(repo_url + '/repodata/repomd.xml')
    xml = response.read()
    xml = ET.fromstring(xml)

    for data in xml:
        if data.tag.endswith('data') and 'type' in data.attrib and \
           data.attrib['type'] == 'primary':
            break
    else:
        raise Exception('Cannot find primary.xml')

    for child in data:
        if child.tag.endswith('location'):
            return repo_url + '/' + child.attrib['href']


def filename_to_srpm(fname):
    # remove -VER-REL.elX_Y.src.rpm suffix
    # and leave only the name prefix
    return '-'.join(fname.split('-')[:-2])


def load_packages_from_repo(repo_url):
    response = urllib.urlopen(primary_xml_gz(repo_url))
    io_stream = StringIO.StringIO(response.read())
    gzip_stream = GzipFile(fileobj=io_stream)
    xml = ET.parse(gzip_stream)

    for package in xml.findall('_:package', xmlns):
        name = package.find('_:name', xmlns).text
        srpm = package.find('./_:format/rpm:sourcerpm', xmlns).text
        srpm = filename_to_srpm(srpm)

        yield name, srpm



_RECIPES = []
# load packages from all repos passed on the command line
for url in sys.argv[1:]:
    # allow -v or other unittest arguments
    if not url.startswith('http'):
        continue

    sys.argv.remove(url)
    for rpm_srpm in load_packages_from_repo(url):
        _RECIPES.append(rpm_srpm)

class ImportedDBSanityTestCase(unittest.TestCase):
    """
        Basic test to validate that after import metadata.db is
        not totally busted.
    """

    @classmethod
    def setUpClass(cls):
        cls.conn = sqlite3.connect(os.environ.get('MDDB', './mddb/metadata.db'))
        cls.db = cls.conn.cursor()

    @classmethod
    def tearDownClass(cls):
        cls.conn.close()

    @parameterized.expand(_RECIPES)
    def test_rpm_was_imorted(self, rpm, srpm):
        # `groups` table contains information about rpms
        self.db.execute('SELECT * FROM groups WHERE name="%s"' % rpm)
        results = self.db.fetchall()
        self.assertGreater(len(results), 0, '%s not found in groups' % rpm)

        # @bcl: Something to be aware of is that projects table will have the
        # srpms for all of them, and groups will have the rpms, with some
        # entries in groups not having entries in projects.

        #TODO: how do we deal with these failures ???

        # `projects` table contains information about srpms
        self.db.execute('SELECT * FROM projects WHERE name="%s"' % srpm)
        results = self.db.fetchall()
        self.assertGreater(len(results), 0, '%s not found in projects' % srpm)


if __name__ == "__main__":
    unittest.main()
