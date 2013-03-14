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
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

from models import Statement, Personality

__version__ = "0.1"
__author__ = "tippenein"

URL = "http://politifact.com"
AGENT = "{}/{}".format(__name__, __version__)

# use db uri supplied by environment perhaps
db = create_engine('sqlite:///database.db', echo=True)
Session = sessionmaker(bind=db)

class Scrape_Teh_Truth(object):

    def __init__(self, url):
        self.url  = url
        self.urls = []
        self.session = Session()

    def scrape(self):
        ''' Scrapes the front page for new statements '''
        page = Fetch(self.url)
        info = page.extract()
        for d in info:
            self.session.add( Personality(d['name'], None) )
            self.session.add( Statement(d['statement'], d['truthiness']) )

    def scrape_all(self):
        ''' scrapes the archived statements 
        http://www.politifact.com/truth-o-meter/statements/?page=2
        '''
        pass
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
            source = container.find('p', {'class' : 'quotesource'})
            truth = container.find('div', {'class' : 'meter'})
            name = source.text
            # for testing
            claim = "hooha"
            truthiness = truth.img.get('alt')
            d['name'] = name
            d['truthiness'] = self._truth_to_int(truthiness)
            d['claim'] = claim
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
    print "filthy bureaucratic scum"

if __name__ == '__main__':
    import sys
    scraper = Scrape_Teh_Truth(URL)
    scraper.scrape()
    main()
