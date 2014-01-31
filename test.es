curl -XPOST 'localhost:9200/foo' -d'{
  "query": {
    "query_string": {
      "query": "stuff and things",
      "default_field": "body"
    }
  },
  "size": 2,
  "from": 0
}'

curl -XDELETE 'localhost:9200/foo' -d'{
  "query": {
    "filtered": {
      "filter": {},
      "query": {
        "match_all": {}
      }
    }
  }
}'


"function_score": {
  "query": {"term": {"category": "budget"}},
  "functions": [
    {
      "filter": {"terms": {"facilities": ["balcony"]}},
      "boost_factor": 2
    },
    {
      "gauss": {
        "field": "location",
        "scale": "1km",
        "reference": [51,0]
      }
    },
    {
      "script": "log10(doc[\'popularity'].value)\""
    },
    {
      "random_score": {}
    }
  ],
  "max_boost": 10,
  "score_mode": "sum",
  "boost_mode": "replace"
}
