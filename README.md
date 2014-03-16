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
             '("MELPA" . "http://melpa.milkbox.net/packages/" ))
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

### Features

- Highlighting for builtin queries, facets, aggregations, special paramaters
- A [company-mode](http://company-mode.github.io/) backend for completing ES
  queries
- Better indenting than sh-mode (indents like js-mode)
- Sending the queries as a http-request to Elasticsearch endpoints.

### Example

See `test.es`, here's a screenshot from my theme:

![picture of es-mode](http://writequit.org/files/es-mode.png)

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
#+BEGIN_SRC elasticsearch :request POST url http://localhost:9200/_search?pretty=true
{
  "query": {
    "match_all": {}
  }
}
#+END_SRC
```

org-mode uses the arguments `:url` and `:request` to know where and how
to send a query. If they are not present org-mode will use
`es-default-url` and `es-defaul-request-method` instead.

### Feedback

This is my first major mode for Emacs, feedback is welcome, especially pull
requests that show me what I'm doing wrong.
