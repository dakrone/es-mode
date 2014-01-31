Elasticsearch mode for Emacs
============================

Provides a major mode for editing ES curl examples. Better highlighting and
indention than sh-mode or js-mode.

Installation
=============

Don't install this yet. I'm still working on it.

Usage
=====

```elisp
(require 'es-mode)
(es-mode)
```

Or open a file with a `.es` extension.

Example
-------

See `test.es`, here's a screenshot from my theme:

![picture of es-mode](http://writequit.org/files/es-mode.png)

Org-babel support
-----------------

One of the main reasons I started this was better highlighting and indention for
org-babel. So add the snippet below to your .emacs to be able to hit `C-c C-c`
and execute blocks like:

```
#+BEGIN_SRC es
curl -XPOST 'localhost:9200/_search' -d'{
  "query": {
    "match_all": {}
  }
}'
#+END_SRC
```

Put into your `~/.emacs.d/init.el`:

```elisp
(defun org-babel-execute:es (body params)
  "Execute a block of ES code with org-babel."
  (message "executing ES source code block")
  (org-babel-eval "/bin/sh" body))
```

Feedback
========

This is my first major mode for Emacs, feedback is welcome, especially pull
requests that show me what I'm doing wrong.
