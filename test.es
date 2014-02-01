## this is a comment
curl -XPOST 'localhost:9200/foo/_search' -d'{
  "query": {
    "filtered": {
      "query": {
        "bool": {
          "must": [
            {
              "query_string": {
                "query": "stuff and things",
                "default_field": "body"
              }
            }
          ],
          "should": [
            {
              "simple_query_string": {
                "query": "more things",
                "fields": ["body", "name"]
              }
            },
            {
              "match": {"description": "potato"}
            }
          ]
        }
      },
      "filter": {
        "range": {
          "timestamp": {
            "gt": 10,
            "lte": 100
          }
        }
      }
    },
    "size": 2,
    "from": 0
  }
}'

# another comment
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

curl -XPOST 'localhost:9200/decide' -d'{
  "settings": {
    "number_of_shards": 1,
    "number_of_replicas": 1
  },
  "mappings": {
    "doc": {
      "properties": {
        "body": {"type": "string"}
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
