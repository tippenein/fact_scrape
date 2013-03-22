#!/usr/bin/env python

# file   : fact_scraper.py
# author : tippenein

#check front page daily for new personalities
# for each personality not found in personalities table,
# use /personalities/<Person>/statements/<?page=#>
# else insert each new event into an existing table

# tables - Personalities, Scores, Statements

'''
.scoretableContainer
    .scoretableStatement
        .mugshot
        .meter
            a(href=actual statement)
                /truth-o-meter/statements/2013/mar/10/personality/specific statement...
                img(alt="True|False|etc")
            p.quote
        h2
            reality
'''

import os
import time
import urllib2
import StringIO
import gzip, zlib
from bs4 import BeautifulSoup

from models import Statement, Personality, Session, Base, engine, get_or_create
from util import create_datetime

__version__ = "0.1"
__author__ = "tippenein"

URL = "http://politifact.com"
AGENT = "{}/{}".format(__name__, __version__)

session = Session()
Base.metadata.create_all(engine)


class Scrape_Teh_Truth(object):

    def __init__(self, url=None):
        self.url = url
        self.urls = []

    def scrape(self, url, test=False):
        ''' Scrapes the front page for new statements '''
        print "scraping -> {}".format(url)
        page = Fetch(url)
        info = page.extract()
        print info
        if test:
            pass
        else:
            for d in info:
                _personality = Personality(
                        name = d['name'],
                        pers_link= d['pers_link'])
                _personality = get_or_create(session, Personality, name=d['name'])
                _statement = Statement(
                        claim = d['claim'],
                        truthiness = d['truthiness'],
                        personality = _personality,
                        date = create_datetime(d['date']))
                session.add(_statement)
                session.commit()


    def scrape_all(self):
        ''' scrapes the archived statements
        '''
        url = 'http://www.politifact.com/truth-o-meter/statements/?page='
        # artificial cap for testing
        i = 1; cap=20
        while i < cap:
            try:
                self.scrape(url + i)
                print "sleeping 1 second to avoid flooding"
                time.sleep(1)
            except:
                break

    def insert(self, name, pers_link, truthiness):
        pass


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
            soup = BeautifulSoup(open("tests/example.html"), "html.parser")
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
        return soup

    def extract(self):
        soup = self.fetch()
        info = []
        for container in soup.find_all('div', {'class': 'scoretableContainer'}):
            d = {}
            truth = container.find('div', {'class':'meter'})
            truthiness = truth.img.get('alt')
            link = truth.a.get('href')
            date_string = link.split('/')[3:6] # get the date from /blah/blah/year/month/day
            claim = container.find('h2').string
            source = container.find('p', {'class':'quotesource'})
            name = source.text
            pers_link = source.a.get('href')

            d['name'] = name
            d['pers_link'] = pers_link
            d['truthiness'] = self._truth_to_int(truthiness)
            d['claim'] = claim
            d['date'] = date_string
            info.append(d)

        return info

    def _truth_to_int(self, s):
        ''' returns the integer representation of a 'truth string'
        '''
        truth_rank = {
                'Pants on Fire!': 0,
                'False'         : 1,
                'Mostly False'  : 2,
                'Half-True'     : 3,
                'Mostly True'   : 4,
                'True'          : 5 }
        return truth_rank[s]

def main():
    from optparse import OptionParser
    parser = OptionParser()
    parser.add_option("-a", "--all", dest='all', action="store_true", default=False, help="scrape the archived statements")
    parser.add_option("-t", "--test", dest='test', action="store_true", default=False, help="Don't commit to database")
    (options, args) = parser.parse_args()

    scraper = Scrape_Teh_Truth()
    if options.test:
        scraper.scrape(URL, test=True)
    elif options.all:
        scraper.scrape_all() if options else scraper.scrape(URL)
    else:
        scraper.scrape(URL)

if __name__ == '__main__':
    import sys
    main()
