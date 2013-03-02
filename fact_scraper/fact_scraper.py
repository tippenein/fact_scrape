#!/usr/bin/env python

# file   : fact_scraper.py
# author : tippenein

#check front page daily for new personalities
# for each personality not found in personalities table, use /personalities/<Person>/statements/<?page=#>
# else insert each new event into an existing table

# tables - Personalities, Scores, Statements
# statuses | True | Mostly True | Half True | Mostly False | False | Pants on Fire

'''
.scoretableContainer
    .scoretableStatement
        .mugshot
        .meter
            a(href=actual statement) /truth-o-meter/statements/2013/mar/10/personality/specific statement...
                img(alt="True|False|etc")
            p.quote
        h2
            reality
'''

import os
from bs4 import BeautifulSoup
import urllib2
import StringIO
import gzip, zlib

__version__ = "0.1"
__author__ = "tippenein"

URL = "http://politifact.com"
AGENT = "{}/{}".format(__name__, __version__)

class Scrape_Teh_Truth(object):

    def __init__(self, url):
        self.url = url
        self.urls = []

    def scrape(self):
        page = Fetch(self.url)
        page.fetch()


class Fetch(object):

    def __init__(self, url):
        self.url = url

    def decode(self, page):
        encoding = page.info().get("Content-Encoding")
        if encoding in ('gzip', 'x-gzip', 'deflate'):
            content = page.read()
            if encoding == 'deflate':
                data = StringIO.StringIO(zlib.decompress(content))
            else:
                data = gzip.GzipFile('', 'rb', 9, StringIO.StringIO(content))
            page = data.read()

        return page

    def fetch(self):
        url = self.url
        if os.environ.get("DEBUG"):
            soup = BeautifulSoup(open("test/example.html"), "html.parser")
        else:
            try:
                opener = urllib2.build_opener()
                opener.addheaders = [('User-Agent', AGENT),
                ('Accept-Encoding', 'gzip,deflate')]
                usock = opener.open(url)
                url = usock.geturl()
                data = self.decode(usock)
                usock.close()
                soup = BeautifulSoup(data)
            except:
                sys.exit("troubles in Gotham, Batman")
        print soup.find_all('div', {'class': 'scoretableContainer'})

def main():
    print "filthy bureaucratic scum"

if __name__ == '__main__':
    import sys
    scraper = Scrape_Teh_Truth(URL)
    scraper.scrape()
    main()
