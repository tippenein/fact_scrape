
import unittest

TEST_URL = "http://politifact.com"

class TestScraper(unittest.TestCase):

    def test_fetch(self):
        page = Fetch(TEST_URL)
        data = page.fetch()
        self.assertTrue(isinstance(data, unicode))

    def test_scraper(self):
        scraper = Scraper(TEST_URL)
        scraper.scrape()

if __name__ == '__main__':
    import sys
    sys.path.append('..')
    from fact_scraper import Fetch
    from fact_scraper import Scrape_Teh_Truth as Scraper

    unittest.main()
