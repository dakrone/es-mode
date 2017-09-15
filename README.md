## Elasticsearch mode for Emacs

Provides a major mode for editing ES query examples. Better highlighting and
indention than sh-mode or js-mode.

It is intended to be a mixture of the three modes as well as mimicing some of
the highlighting from Marvel's sense console.

You can also think of it as
[Marvel's Sense](http://www.elasticsearch.org/guide/en/marvel/current/index.html#_sense)-envy
for Emacs users.

### Installation

`es-mode` is available in the MELPA repository. Do this, if MELPA isn't already in
your sources:

```elisp
(require 'package)
(add-to-list 'package-archives
             '("MELPA" . "https://melpa.org/packages/" ))
```

Then run `M-x package-refresh-contents` to load the contents of the new
repository, and `M-x package-install RET es-mode RET` to install `es-mode`.

### Usage

```elisp
(add-to-list 'load-path "/path/to/es-mode-dir")
(autoload 'es-mode "es-mode.el"
  "Major mode for editing Elasticsearch queries" t)
(add-to-list 'auto-mode-alist '("\\.es$" . es-mode))
```

You can now open a file with an `.es` extension and `es-mode` will
automatically load..

#### Keyboard Shortcuts

| Key | Action | Description |
|-----|--------|-------------|
| `C-c C-c` | es-execute-request-dwim | Execute the request you're currently on |
| `C-c C-p` | es-goto-previous-request | Go to the previous request |
| `C-c C-n` | es-goto-next-request | Go to the next request |
| `C-c M-w` | es-copy-as | Copy the request as a `curl` request |
| `C-c C-u` | es-set-endpoint-url | Set the ES endpoint requests will be executed against |
| `C-c RET` | es-set-request-method | Set the rest method type (only for non-prefixed requests) |
| `C-c s` | (requires `yasnippet`) | Insert a snippet for searching |
| `C-c i` | (requires `yasnippet`) | Insert a snippet for new index creation |

### Features

- Highlighting for builtin queries, facets, aggregations, special paramaters
- A [company-mode](http://company-mode.github.io/) backend for completing ES
  queries
- Better indenting than sh-mode (indents like js-mode)
- Sending the queries as a http-request to Elasticsearch endpoints.
- Navigate via goto-(next|previous)-request with `C-c C-n` and `C-c C-p` (when
  using parameters)
- An Elasticsearch Command Center mode for monitoring clusters
- Hooks for responses, see `es-response-success-functions` and
  `es-response-failure-functions`
- Support for "es" mode blocks in org-mode
- Pass the resulting JSON through [jq](https://stedolan.github.io/jq/) to return
  only the values you want in org-mode
- Displays warning headers from Elasticsearch in the results buffers for
  deprecated features
- [Yasnippet](https://github.com/joaotavora/yasnippet) support with built-in
  snippets for common operations
- Ability to copy a request as a [curl](https://curl.haxx.se/) request, or other
  type of request (customizing `es-copy-as-fn`)

#### Using hide-show mode in results buffers

If you would like to enabled `hs-minor-mode` automatically on ES results
buffers, use the following:

```elisp
(add-hook 'es-result-mode-hook 'hs-minor-mode)
```

### Example

You can specify requests with two different formats:

#### With parameters

In the document, specify parameters similar to Sense, like so:

```json
POST /myindex/_search?pretty
{
  "query": {
    "match_all": {}
  }
}
```

Hitting `C-c C-c` anywhere on the parameter or body of the request will execute
the request, opening a response buffer. The base-url can be configured by
customizing the `es-default-url` var.

You also don't have to provide the leading "/", similar to Sense (I personally
think the leading "/" looks better though), like this:

```json
POST myindex/_search?pretty
{
  "query": {
    "match_all": {}
  }
}
```

If you do not want to specify `?pretty` every time, you can customize the
`es-always-pretty-print` var (defaults to `nil`).

#### Without parameters (deprecated)

Without any parameters, you can specify a request:

```json
{
  "query": {
    "match_all": {}
  }
}
```

With the request region highlighted or inside the query structure, hit `C-c C-c`
to execute it. The first time you do this you will be prompted for the URL and
HTTP method. You can also set the URL with `C-c C-u` and the method with `C-c
C-m`.

This is deprecated in favor of the sense/console-like syntax, as it increases
complexity for maintaining es-mode.

#### Screenshot

See `test.es`, `test2.es`, and `all.org`, here's a screenshot from my theme:

![picture of es-mode](http://writequit.org/files/es-mode.png)

And here's an example of the completion of queries/filters:

![picture of es-mode completion](http://writequit.org/files/es-mode-completion.png)

### Org-babel support

One of the main reasons I started this was better highlighting and indention for
org-babel. So add the snippet below to your .emacs:

```
(org-babel-do-load-languages
 'org-babel-load-languages
 '((elasticsearch . t)))
```

And then you will be able to hit `C-c C-c` on code like this in your org-mode
file:

```
#+BEGIN_SRC es
POST /_search?pretty
{
  "query": {
    "match_all": {}
  }
}
#+END_SRC
```

OR (without parameters):

```
#+BEGIN_SRC es :method POST :url localhost:9200/_search?pretty
{
  "query": {
    "match_all": {}
  }
}
#+END_SRC
```

org-mode uses the arguments `:url` and `:method` to know where and how
to send a query. If they are not present org-mode will use
`es-default-url` and `es-defaul-request-method` instead.

Tangling these blocks will produce `<filename>.es`, if you specify the filename
with `:tangle foo.sh`, es-mode will instead create a curl request for the body
of the request.

#### Generating org-mode tables from aggregations

Es-mode supports rudimentary table creation from aggregations using the
`:tablify` header parameter. For example, consider the following aggregations:

```
#+BEGIN_SRC es :tablify prices :results raw table
POST /test/doc/_search
{
  "aggs" : {
    "prices" : {
      "histogram" : {
        "field" : "price",
        "interval" : 20,
        "min_doc_count": 0
      }
    }
  },
  "size": 0
}
#+END_SRC

#+RESULTS:
| key | document count |
|-----+----------------|
|   0 |              4 |
|  20 |              0 |
|  40 |              1 |
|  60 |              2 |
|  80 |              2 |
| 100 |              1 |
| 120 |              0 |
| 140 |              0 |
| 160 |              2 |
```

Note that the "tablify" argument must be the name of the aggregation to be
tablified, in this example, "prices" is the name of the argument.

This also works for `terms` aggregations:

```
#+BEGIN_SRC es :tablify my_terms_agg :results raw table
POST /test/doc/_search
{
  "aggs" : {
    "my_terms_agg" : {
      "terms" : {
        "field" : "type"
      }
    }
  },
  "size": 0
}
#+END_SRC

#+RESULTS:
| key      | document count |
|----------+----------------|
| eggplant |              5 |
| foo      |              4 |
| widget   |              2 |
| cog      |              1 |
```

If you are using org-mode 8.3.1 or later, you can generate pretty ASCII graphs
from org-mode using
[orgtbl-ascii-plot](http://orgmode.org/worg/org-contrib/orgtbl-ascii-plot.html)
like so (hit `C-c C-c` on the `TBLFM` line to generate the graph):

```
#+RESULTS:
| key | document count |            |
|-----+----------------+------------|
|   0 |              4 | WWWWWWWWWl |
|  20 |              0 |            |
|  40 |              1 | WWc        |
|  60 |              2 | WWWWV      |
|  80 |              2 | WWWWV      |
| 100 |              1 | WWc        |
| 120 |              0 |            |
| 140 |              0 |            |
| 160 |              2 | WWWWV      |
#+TBLFM: $3='(orgtbl-ascii-draw $2 0 5)

Or:

#+RESULTS:
| key      | document count |              |
|----------+----------------+--------------|
| eggplant |              5 | WWWWWWWWWWWW |
| foo      |              4 | WWWWWWWWWl   |
| widget   |              2 | WWWWV        |
| cog      |              1 | WWc          |
#+TBLFM: $3='(orgtbl-ascii-draw $2 0 5)
```

Be sure to pass the correct minimum and maximum values for the table (in this
example, 0 and 5) to the `orgtbl-ascii-draw` method.

#### Passing JSON through [jq](https://stedolan.github.io/jq/)

In org-mode you can also reduce the size of results by passing them through the
jq command-line tool. For example, compare the output of these two different org
blocks:

```
#+BEGIN_SRC es
GET /
{}
#+END_SRC

#+RESULTS:
#+begin_example
{
  "status" : 200,
  "name" : "Everyman",
  "version" : {
    "number" : "1.3.2",
    "build_hash" : "dee175dbe2f254f3f26992f5d7591939aaefd12f",
    "build_timestamp" : "2014-08-13T14:29:30Z",
    "build_snapshot" : false,
    "lucene_version" : "4.9"
  },
  "tagline" : "You Know, for Search"
}
#+end_example
```

And the same thing, but passed through the `jq` tool, extracting the "name" and
"version.number" fields:

```
#+BEGIN_SRC es :jq .name, .version.number
GET /
{}
#+END_SRC

#+RESULTS:
: "Everyman"
: "1.3.2"
```

You can use this to return only a certain hit, or the score of a hit, etc,
easily, so you can format the output as desired. See the
[full jq manual](https://stedolan.github.io/jq/manual/) for how to use jq.

es-mode uses `jq` in the `PATH`, however, if you want to specify an absolute
path you can customize the `es-jq-path` var as you like.

jq will only be run if the response is an HTTP 20[0-9].

#### Variable Substitution

`es-mode` includes support for variable substitution in org-babel
source blocks.  The variable references in the body should be in the
form `${var-name}`.

```
#+BEGIN_SRC es :var index="theindex"
POST /${index}/_search?pretty
{
  "query": {
    "match_all": {}
  }
}
#+END_SRC
```

Vars can also be used to use the results from other org-babel blocks.

In the example below, the first source block searches the index
`child-docs` for documents from the past month and `jq` is used to
select the `parent-ids` from the hits that are returned.

The second source block takes the `parent-ids` and binds it to the
variable `ids` in the header of the source block (`:var
ids=parent-ids`).  When the code is run `${ids}` is replaced with the
JSON array prior to executing the search request against the
`parent-docs` index.

```
#+NAME: parent-ids
#+BEGIN_SRC es :jq "[.hits.hits[]._source.\"parent-id\"]"
  GET /child-docs/_search?pretty
  {
    "query": {
        {"range": {"time": {"gte": "now-1M/d"}}}
    },
    "_source": "parent-id"
  }
#+END_SRC

#+RESULTS: parent-ids
#+begin_example
[
  "id1",
  "id2",
  "id3"
]
#+end_example

#+BEGIN_SRC es :var ids=parent-ids
  GET /parent-docs/_search?pretty
  {
    "query": {
      {"ids": {"values": ${ids}}}
    }
  }
#+END_SRC

#+RESULTS:
#+begin_example
{
  "took" : 227,
  "timed_out" : false,
  "_shards" : {
    "total" : 4,
    "successful" : 4,
    "failed" : 0
  },
  "hits" : {
    "total" : 3,
    "max_score" : 5.3673162,
    "hits" : [
      {
        "_index" : "parent-docs",
        "_type" : "t",
        "_id" : "id1",
        "_score" : 5.3673162,
        "_source" : {
          ...
        }
      },
      {
        "_index" : "parent-docs",
        "_type" : "t",
        "_id" : "id2",
        "_score" : 5.3673162,
        "_source" : {
          ...
        }
      },
      {
        "_index" : "parent-docs",
        "_type" : "t",
        "_id" : "id3",
        "_score" : 5.3673162,
        "_source" : {
           ...
        }
      }
    ]
  }
}
#+end_example


```

### Elasticsearch Command Center

`es-mode` includes a mode called the "Elasticsearch Command Center", which is
meant for monitoring your cluster. This provides a graphical representation of
what's happening in the cluster.

To invoke it, simply do `M-x es-command-center`. ES-CC will automatically
refresh at `es-cc-refresh-interval` seconds, check out `M-x customize-group
es-cc` to see all of the customization options.

Here's a screenshot of what it looks like:

![picture of es-command-center](http://writequit.org/images/es-command-center.png)

### Feedback

This is my first major mode for Emacs, feedback is welcome, especially pull
requests that show me what I'm doing wrong.
