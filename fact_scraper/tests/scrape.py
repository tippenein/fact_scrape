import unittest
import datetime

TEST_URL = "http://politifact.com"


class TestScraper(unittest.TestCase):

    def test_fetch(self):
        ''' confirm data fetched is unicode '''

        page = Fetch(TEST_URL)
        data = page.fetch()
        self.assertTrue(isinstance(data, unicode))

    def test_scraper(self):
        ''' no errors '''
        scraper = Scraper(TEST_URL)
        scraper.scrape()

    def cleanUp(self):
        ''' delete statements database '''
        pass


class TestUtils(unittest.TestCase):

    def test_create_datetime(self):
        ''' datetime objects from date list '''

        date_list = ['2013', 'jan', '10']
        obj = create_datetime(date_list)
        datetime_obj = datetime.date(2013, 1, 10)
        print datetime_obj, obj
        self.assertEquals(obj, datetime_obj)


if __name__ == '__main__':
    import sys
    sys.path.append('..')
    from util import create_datetime
    from fact_scraper import Fetch
    from fact_scraper import Scrape_Teh_Truth as Scraper

    unittest.main()
