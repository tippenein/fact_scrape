scrape_the_truth
-----

[![Build Status](https://secure.travis-ci.org/tippenein/scrape_the_truth.png)](http://travis-ci.org/tippenein/scrape_the_truth)

``` sh
stack build
stack install
scrape-the-truth
```

this will scrape politifacts website, store the results in a local sqlite db
and host an api to query from

## endpoints

`/persons`

`/statements`
  - query params
    - person_name

## example

![example](example.png)
