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

from bs4 import BeautifulSoup
import urllib2

__version__ = "0.1"
__author__ = "tippenein"

URL = "http://www.politifact.com"
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

    def __getitem__(self, x):
        return self.urls[x]

    def _addHeaders(self, request):
        request.add_header("User-Agent", AGENT)

    def open(self):
        url = self.url
        try:
            request = urllib2.Request(url)
            handle = urllib2.build_opener()
        except IOError:
            return None
        return (request, handle)

    def fetch(self):
        request, handle = self.open()
        self._addHeaders(request)
        if handle:
            try:
                content = unicode(handle.open(request).read(), "utf-8",
                        errors="replace")
                soup = BeautifulSoup(content)
            except urllib2.HTTPError, error:
                sys.exit("{}: soup fail".format(error))

def main():
    print "filthy bureaucratic scum"

if __name__ == '__main__':
    import sys
    scraper = Scrape_Teh_Truth(URL)
    scraper.scrape()
    main()
