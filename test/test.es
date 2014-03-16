{
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
}
