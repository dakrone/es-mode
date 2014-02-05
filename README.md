## Elasticsearch mode for Emacs

Provides a major mode for editing ES curl examples. Better highlighting and
indention than sh-mode or js-mode.

It is intended to be a mixture of the three modes as well as mimicing some of
the highlighting from Marvel's sense console.

You can also think of it as
[Marvel's Sense](http://www.elasticsearch.org/guide/en/marvel/current/index.html#_sense)-envy
for Emacs users.

### Installation

You should be able to install this as a package from MELPA. It's not currently
on marmalade until I get a stable release I'm happy with.

### Usage

```elisp
(require 'es-mode)
(es-mode)
```

Or open a file with a `.es` extension.

### Features

- Highlighting for builtin queries, facets, aggregations, special paramaters
- Highlighting for curl's HTTP (`-XPOST`) flags
- A [company-mode](http://company-mode.github.io/) backend for completing ES
  queries
- Better indenting than sh-mode (indents like js-mode)

### Example

See `test.es`, here's a screenshot from my theme:

![picture of es-mode](http://writequit.org/files/es-mode.png)

### Org-babel support

One of the main reasons I started this was better highlighting and indention for
org-babel. So add the snippet below to your .emacs:

Put into your `~/.emacs.d/init.el`:

```elisp
(defun org-babel-execute:es (body params)
  "Execute a block of ES code with org-babel."
  (message "executing ES source code block")
  (org-babel-eval "/bin/sh" body))
```

And then you will be able to hit `C-c C-c` on code like this in your org-mode
file:

```
#+BEGIN_SRC es
curl -XPOST 'localhost:9200/_search' -d'{
  "query": {
    "match_all": {}
  }
}'
#+END_SRC
```

### Feedback

This is my first major mode for Emacs, feedback is welcome, especially pull
requests that show me what I'm doing wrong.
