;;; commonmark-gfm-test.el --- Tests for commonmark-gfm -*- lexical-binding: t; -*-

;; Copyright (C) 2026  kn66
;; SPDX-License-Identifier: MIT
;;
;; Some test examples are adapted from the CommonMark and GFM specs.  See
;; ../NOTICE and README.md in this directory for attribution and licensing
;; notes for those examples.

;; This file is not part of GNU Emacs.

;;; Code:

(require 'ert)
(require 'commonmark-gfm)
(require 'commonmark-gfm-spec)

(defmacro commonmark-gfm-test--renders (markdown html)
  "Assert that MARKDOWN renders to HTML."
  `(should (string= ,html (commonmark-gfm-render-to-html ,markdown))))

(ert-deftest commonmark-gfm-render-heading ()
  (commonmark-gfm-test--renders "# Hello\n"
                                "<h1>Hello</h1>\n"))

(ert-deftest commonmark-gfm-sourcepos-document-and-blocks ()
  (let* ((doc (commonmark-gfm-parse "# Hello\n\nparagraph\n"))
         (children (commonmark-gfm-node-children doc)))
    (should (equal '((1 1) (3 9))
                   (commonmark-gfm-node-sourcepos doc)))
    (should (equal '(((1 1) (1 7))
                     ((3 1) (3 9)))
                   (mapcar #'commonmark-gfm-node-sourcepos children)))))

(ert-deftest commonmark-gfm-sourcepos-list-and-items ()
  (let* ((doc (commonmark-gfm-parse "- one\n- two\n"))
         (list-node (car (commonmark-gfm-node-children doc)))
         (items (commonmark-gfm-node-children list-node)))
    (should (equal '((1 1) (2 5))
                   (commonmark-gfm-node-sourcepos list-node)))
    (should (equal '(((1 1) (1 5))
                     ((2 1) (2 5)))
                   (mapcar #'commonmark-gfm-node-sourcepos items)))))

(ert-deftest commonmark-gfm-sourcepos-inline-parse ()
  (let* ((nodes (commonmark-gfm-inline-parse-with-sourcepos "a *b* c" 2 3))
         (emph (nth 1 nodes)))
    (should (equal '(text emph text)
                   (mapcar #'commonmark-gfm-node-type nodes)))
    (should (equal '(((2 3) (2 4))
                     ((2 5) (2 7))
                     ((2 8) (2 9)))
                   (mapcar #'commonmark-gfm-node-sourcepos nodes)))
    (should (equal '((2 6) (2 6))
                   (commonmark-gfm-node-sourcepos
                    (car (commonmark-gfm-node-children emph)))))))

(ert-deftest commonmark-gfm-sourcepos-paragraph-inline-children ()
  (let* ((doc (commonmark-gfm-parse "a *b* c\n"))
         (paragraph (car (commonmark-gfm-node-children doc)))
         (children (commonmark-gfm-node-children paragraph))
         (emph (nth 1 children)))
    (should (equal '(((1 1) (1 2))
                     ((1 3) (1 5))
                     ((1 6) (1 7)))
                   (mapcar #'commonmark-gfm-node-sourcepos children)))
    (should (equal '((1 4) (1 4))
                   (commonmark-gfm-node-sourcepos
                    (car (commonmark-gfm-node-children emph)))))
    (should (null (text-properties-at
                   0
                   (commonmark-gfm-node-literal (car children)))))))

(ert-deftest commonmark-gfm-sourcepos-heading-inline-children ()
  (let* ((doc (commonmark-gfm-parse "  ## Hi *x*\n"))
         (heading (car (commonmark-gfm-node-children doc)))
         (children (commonmark-gfm-node-children heading))
         (emph (nth 1 children)))
    (should (equal '(((1 6) (1 8))
                     ((1 9) (1 11)))
                   (mapcar #'commonmark-gfm-node-sourcepos children)))
    (should (equal '((1 10) (1 10))
                   (commonmark-gfm-node-sourcepos
                    (car (commonmark-gfm-node-children emph)))))))

(ert-deftest commonmark-gfm-render-setext-heading ()
  (commonmark-gfm-test--renders "Hello\n-----\n"
                                "<h2>Hello</h2>\n"))

(ert-deftest commonmark-gfm-render-paragraph-escapes-html ()
  (commonmark-gfm-test--renders "a < b & c\n"
                                "<p>a &lt; b &amp; c</p>\n"))

(ert-deftest commonmark-gfm-render-thematic-break ()
  (commonmark-gfm-test--renders " - - - \n"
                                "<hr />\n"))

(ert-deftest commonmark-gfm-render-code-fence ()
  (commonmark-gfm-test--renders
   "```elisp\n(+ 1 2)\n```\n"
   "<pre><code class=\"language-elisp\">(+ 1 2)\n</code></pre>\n"))

(ert-deftest commonmark-gfm-render-default-css-option ()
  (let ((html (commonmark-gfm-render-to-html
               "# Styled\n"
               '(:html-include-default-css t))))
    (should (string-prefix-p "<style>\nbody {" html))
    (should (string-suffix-p "<h1>Styled</h1>\n" html))))

(ert-deftest commonmark-gfm-render-user-css-option ()
  (should
   (string=
    "<style>\nh1 { color: red; }</style>\n<h1>Styled</h1>\n"
    (commonmark-gfm-render-to-html
     "# Styled\n"
     '(:html-user-css "h1 { color: red; }")))))

(ert-deftest commonmark-gfm-render-mermaid-script-option ()
  (let ((html (commonmark-gfm-render-to-html
               "```mermaid\ngraph TD\n  A --> B\n```\n"
               '(:html-include-mermaid-script t))))
    (should (string-match-p
             "<div class=\"mermaid\">graph TD\n  A --&gt; B\n</div>\n"
             html))
    (should (string-suffix-p
             (concat "<script type=\"module\">\n"
                     "import mermaid from \"https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.esm.min.mjs\";\n"
                     "mermaid.initialize({ startOnLoad: true });\n"
                     "</script>\n")
             html))))

(ert-deftest commonmark-gfm-render-mermaid-script-url-option ()
  (should
   (string-suffix-p
    (concat "<script type=\"module\">\n"
            "import mermaid from \"https://example.com/mermaid.mjs\";\n"
            "mermaid.initialize({ startOnLoad: true });\n"
            "</script>\n")
    (commonmark-gfm-render-to-html
     "# Mermaid\n"
     '(:html-include-mermaid-script t
       :html-mermaid-script-url "https://example.com/mermaid.mjs")))))

(ert-deftest commonmark-gfm-render-mermaid-code-fence ()
  (commonmark-gfm-test--renders
   "```mermaid\ngraph TD\n  A --> B\n```\n"
   "<div class=\"mermaid\">graph TD\n  A --&gt; B\n</div>\n"))

(ert-deftest commonmark-gfm-render-commonmark-block-regressions ()
  (dolist
      (case
       '(("foo\n    # bar\n"
          . "<p>foo\n# bar</p>\n")
         ("foo  \n     bar\n"
          . "<p>foo<br />\nbar</p>\n")
         ("foo\\\n     bar\n"
          . "<p>foo<br />\nbar</p>\n")
         ("foo  \n"
          . "<p>foo</p>\n")
         ("foo \n baz\n"
          . "<p>foo\nbaz</p>\n")
         ("> ```\n> aaa\n\nbbb\n"
          . "<blockquote>\n<pre><code>aaa\n</code></pre>\n</blockquote>\n<p>bbb</p>\n")
         (" ```\n aaa\naaa\n```\n"
          . "<pre><code>aaa\naaa\n</code></pre>\n")
         ("   ```\n   aaa\n    aaa\n  aaa\n   ```\n"
          . "<pre><code>aaa\n aaa\naaa\n</code></pre>\n")
         ("    chunk1\n      \n      chunk2\n"
          . "<pre><code>chunk1\n  \n  chunk2\n</code></pre>\n")
         ("Foo\n    bar\n\n"
          . "<p>Foo\nbar</p>\n")
         (" *-*\n"
          . "<p><em>-</em></p>\n")
         ("- foo\n***\n- bar\n"
          . "<ul>\n<li>foo</li>\n</ul>\n<hr />\n<ul>\n<li>bar</li>\n</ul>\n")
         ("* Foo\n* * *\n* Bar\n"
          . "<ul>\n<li>Foo</li>\n</ul>\n<hr />\n<ul>\n<li>Bar</li>\n</ul>\n")
         ("- Foo\n- * * *\n"
          . "<ul>\n<li>Foo</li>\n<li>\n<hr />\n</li>\n</ul>\n")
         ("  - foo\n\n    bar\n"
          . "<ul>\n<li>\n<p>foo</p>\n<p>bar</p>\n</li>\n</ul>\n")
         ("> foo\nbar\n===\n"
          . "<blockquote>\n<p>foo\nbar\n===</p>\n</blockquote>\n")
         (">\t\tfoo\n"
          . "<blockquote>\n<pre><code>  foo\n</code></pre>\n</blockquote>\n")
         ("-\t\tfoo\n"
          . "<ul>\n<li>\n<pre><code>  foo\n</code></pre>\n</li>\n</ul>\n")
         (" - foo\n   - bar\n\t - baz\n"
          . "<ul>\n<li>foo\n<ul>\n<li>bar\n<ul>\n<li>baz</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>\n")))
    (commonmark-gfm-test--renders (car case) (cdr case))))

(ert-deftest commonmark-gfm-render-commonmark-code-span-regressions ()
  (dolist
      (case
       `(("` `` `\n"
          . "<p><code>``</code></p>\n")
         ("`  ``  `\n"
          . "<p><code> `` </code></p>\n")
         ("`  `\n"
          . "<p><code>  </code></p>\n")
         ("``\nfoo\nbar  \nbaz\n``\n"
          . "<p><code>foo bar   baz</code></p>\n")
         ("``\nfoo \n``\n"
          . "<p><code>foo </code></p>\n")
         ("`foo   bar \nbaz`\n"
          . "<p><code>foo   bar  baz</code></p>\n")
         ("` foo `` bar `\n"
          . "<p><code>foo `` bar</code></p>\n")
         ("[not a `link](/foo`)\n"
          . "<p>[not a <code>link](/foo</code>)</p>\n")
         ("<https://foo.bar.`baz>`\n"
          . ,(concat "<p><a href=\"https://foo.bar.%60baz\">"
                     "https://foo.bar.`baz</a>`</p>\n"))
         ("```foo``\n"
          . "<p>```foo``</p>\n")
         ("`foo``bar``\n"
          . "<p>`foo<code>bar</code></p>\n")))
    (commonmark-gfm-test--renders (car case) (cdr case))))

(ert-deftest commonmark-gfm-render-blockquote ()
  (commonmark-gfm-test--renders
   "> # Quote\n> body\n"
   "<blockquote>\n<h1>Quote</h1>\n<p>body</p>\n</blockquote>\n"))

(ert-deftest commonmark-gfm-render-blockquote-lazy-continuation ()
  (commonmark-gfm-test--renders
   "> quote\nlazy\n"
   "<blockquote>\n<p>quote\nlazy</p>\n</blockquote>\n"))

(ert-deftest commonmark-gfm-render-commonmark-blockquote-regressions ()
  (dolist
      (case
       '(("> foo\n    - bar\n"
          . "<blockquote>\n<p>foo\n- bar</p>\n</blockquote>\n")
         ("> foo\n\n> bar\n"
          . "<blockquote>\n<p>foo</p>\n</blockquote>\n<blockquote>\n<p>bar</p>\n</blockquote>\n")
         ("> > > foo\nbar\n"
          . "<blockquote>\n<blockquote>\n<blockquote>\n<p>foo\nbar</p>\n</blockquote>\n</blockquote>\n</blockquote>\n")
         (">>> foo\n> bar\n>>baz\n"
          . "<blockquote>\n<blockquote>\n<blockquote>\n<p>foo\nbar\nbaz</p>\n</blockquote>\n</blockquote>\n</blockquote>\n")
         (">     code\n\n>    not code\n"
          . "<blockquote>\n<pre><code>code\n</code></pre>\n</blockquote>\n<blockquote>\n<p>not code</p>\n</blockquote>\n")))
    (commonmark-gfm-test--renders (car case) (cdr case))))

(ert-deftest commonmark-gfm-render-tight-unordered-list ()
  (commonmark-gfm-test--renders
   "- one\n- two\n"
   "<ul>\n<li>one</li>\n<li>two</li>\n</ul>\n"))

(ert-deftest commonmark-gfm-render-loose-list ()
  (commonmark-gfm-test--renders
   "- a\n\n- b\n"
   "<ul>\n<li>\n<p>a</p>\n</li>\n<li>\n<p>b</p>\n</li>\n</ul>\n"))

(ert-deftest commonmark-gfm-render-commonmark-list-regressions ()
  (dolist
      (case
       '(("- one\n\n two\n"
          . "<ul>\n<li>one</li>\n</ul>\n<p>two</p>\n")
         ("-\n\n  foo\n"
          . "<ul>\n<li></li>\n</ul>\n<p>foo</p>\n")
         ("  1.  A paragraph\n    with two lines.\n"
          . "<ol>\n<li>A paragraph\nwith two lines.</li>\n</ol>\n")
         ("> 1. > Blockquote\ncontinued here.\n"
          . "<blockquote>\n<ol>\n<li>\n<blockquote>\n<p>Blockquote\ncontinued here.</p>\n</blockquote>\n</li>\n</ol>\n</blockquote>\n")
         ("- foo\n - bar\n  - baz\n   - boo\n"
          . "<ul>\n<li>foo</li>\n<li>bar</li>\n<li>baz</li>\n<li>boo</li>\n</ul>\n")
         ("The number of windows in my house is\n14.  The number of doors is 6.\n"
          . "<p>The number of windows in my house is\n14.  The number of doors is 6.</p>\n")
         ("- foo\n  - bar\n    - baz\n\n\n      bim\n"
          . "<ul>\n<li>foo\n<ul>\n<li>bar\n<ul>\n<li>\n<p>baz</p>\n<p>bim</p>\n</li>\n</ul>\n</li>\n</ul>\n</li>\n</ul>\n")
         ("- a\n- ```\n  b\n\n\n  ```\n- c\n"
          . "<ul>\n<li>a</li>\n<li>\n<pre><code>b\n\n\n</code></pre>\n</li>\n<li>c</li>\n</ul>\n")
         ("- a\n  - b\n\n    c\n- d\n"
          . "<ul>\n<li>a\n<ul>\n<li>\n<p>b</p>\n<p>c</p>\n</li>\n</ul>\n</li>\n<li>d</li>\n</ul>\n")))
    (commonmark-gfm-test--renders (car case) (cdr case))))

(ert-deftest commonmark-gfm-render-ordered-list-start ()
  (commonmark-gfm-test--renders
   "3. three\n4. four\n"
   "<ol start=\"3\">\n<li>three</li>\n<li>four</li>\n</ol>\n"))

(ert-deftest commonmark-gfm-render-html-block ()
  (commonmark-gfm-test--renders
   "<div>\nraw & html\n</div>\n"
   "<div>\nraw & html\n</div>\n"))

(ert-deftest commonmark-gfm-render-inline-link ()
  (commonmark-gfm-test--renders
   "[Example](https://example.invalid)\n"
   "<p><a href=\"https://example.invalid\">Example</a></p>\n"))

(ert-deftest commonmark-gfm-render-commonmark-link-regressions ()
  (let ((eszett (string #x1e9e)))
    (dolist
        (case
         `(("[link]()\n"
            . "<p><a href=\"\">link</a></p>\n")
           ("[]()\n"
            . "<p><a href=\"\"></a></p>\n")
           ("[a](<b)c>)\n"
            . "<p><a href=\"b)c\">a</a></p>\n")
           ("[link](foo(and(bar)))\n"
            . "<p><a href=\"foo(and(bar))\">link</a></p>\n")
           ("[link](foo(and(bar))\n"
            . "<p>[link](foo(and(bar))</p>\n")
           ("[link](foo%20b&auml;)\n"
            . ,(concat "<p><a href=\"foo%20b%C3%A4\">link</a></p>\n"))
           ("[link](/url (title))\n"
            . "<p><a href=\"/url\" title=\"title\">link</a></p>\n")
           ("[link](/url \"title \"and\" title\")\n"
            . "<p>[link](/url &quot;title &quot;and&quot; title&quot;)</p>\n")
           (,(concat "[" eszett "]\n\n[SS]: /url\n")
            . ,(concat "<p><a href=\"/url\">" eszett "</a></p>\n"))
           ("[foo](not a link)\n\n[foo]: /url1\n"
            . "<p><a href=\"/url1\">foo</a>(not a link)</p>\n")
           ("[foo [bar](/uri)](/uri)\n"
            . "<p>[foo <a href=\"/uri\">bar</a>](/uri)</p>\n")
           ("[foo *[bar [baz](/uri)](/uri)*](/uri)\n"
            . "<p>[foo <em>[bar <a href=\"/uri\">baz</a>](/uri)</em>](/uri)</p>\n")
           ("![[[foo](uri1)](uri2)](uri3)\n"
            . "<p><img src=\"uri3\" alt=\"[foo](uri2)\" /></p>\n")
           ("[foo <bar attr=\"](baz)\">\n"
            . "<p>[foo <bar attr=\"](baz)\"></p>\n")
           ("[foo<https://example.com/?search=](uri)>\n"
            . "<p>[foo<a href=\"https://example.com/?search=%5D(uri)\">https://example.com/?search=](uri)</a></p>\n")
           ("[foo [bar](/uri)][ref]\n\n[ref]: /uri\n"
            . "<p>[foo <a href=\"/uri\">bar</a>]<a href=\"/uri\">ref</a></p>\n")
           ("[foo][ref[]\n\n[ref[]: /uri\n"
            . "<p>[foo][ref[]</p>\n<p>[ref[]: /uri</p>\n")))
      (commonmark-gfm-test--renders (car case) (cdr case)))))

(ert-deftest commonmark-gfm-render-full-reference-link ()
  (commonmark-gfm-test--renders
   "[foo]: /url \"Title\"\n\nA [link][foo].\n"
   "<p>A <a href=\"/url\" title=\"Title\">link</a>.</p>\n"))

(ert-deftest commonmark-gfm-render-multiline-reference-title ()
  (commonmark-gfm-test--renders
   "[foo]: /url\n  \"Title\"\n\nA [link][foo].\n"
   "<p>A <a href=\"/url\" title=\"Title\">link</a>.</p>\n"))

(ert-deftest commonmark-gfm-render-collapsed-reference-link ()
  (commonmark-gfm-test--renders
   "[foo]: /url\n\n[foo][]\n"
   "<p><a href=\"/url\">foo</a></p>\n"))

(ert-deftest commonmark-gfm-render-shortcut-reference-link ()
  (commonmark-gfm-test--renders
   "[foo]: /url\n\n[foo]\n"
   "<p><a href=\"/url\">foo</a></p>\n"))

(ert-deftest commonmark-gfm-render-reference-image ()
  (commonmark-gfm-test--renders
   "![alt][img]\n\n[img]: /img.png \"Image title\"\n"
   "<p><img src=\"/img.png\" alt=\"alt\" title=\"Image title\" /></p>\n"))

(ert-deftest commonmark-gfm-render-commonmark-image-regressions ()
  (dolist
      (case
       '(("![foo ![bar](/url)](/url2)\n"
          . "<p><img src=\"/url2\" alt=\"foo bar\" /></p>\n")
         ("![foo [bar](/url)](/url2)\n"
          . "<p><img src=\"/url2\" alt=\"foo bar\" /></p>\n")))
    (commonmark-gfm-test--renders (car case) (cdr case))))

(ert-deftest commonmark-gfm-reference-definition-first-wins ()
  (commonmark-gfm-test--renders
   "[foo]: /first\n[foo]: /second\n\n[foo]\n"
   "<p><a href=\"/first\">foo</a></p>\n"))

(ert-deftest commonmark-gfm-reference-definitions-do-not-render ()
  (commonmark-gfm-test--renders
   "[foo]: /url\n\nparagraph\n"
   "<p>paragraph</p>\n"))

(ert-deftest commonmark-gfm-reference-definition-in-blockquote ()
  (commonmark-gfm-test--renders
   "> [foo]: /url\n>\n> [foo]\n"
   "<blockquote>\n<p><a href=\"/url\">foo</a></p>\n</blockquote>\n"))

(ert-deftest commonmark-gfm-reference-definition-in-list-item ()
  (commonmark-gfm-test--renders
   "- [foo]: /url\n\n  [foo]\n"
   "<ul>\n<li>\n<p><a href=\"/url\">foo</a></p>\n</li>\n</ul>\n"))

(ert-deftest commonmark-gfm-render-commonmark-reference-definition-regressions ()
  (let* ((phi (string #x03c6))
         (omicron (string #x03bf))
         (upsilon (string #x03c5))
         (alpha (string #x03b1))
         (gamma (string #x03b3))
         (omega (string #x03c9))
         (upper-alpha (string #x0391))
         (upper-gamma (string #x0393))
         (upper-omega (string #x03a9))
         (greek-url (concat "/" phi omicron upsilon))
         (greek-label (concat alpha gamma omega))
         (upper-greek-label (concat upper-alpha upper-gamma upper-omega)))
    (dolist
        (case
         `(("   [foo]: \n      /url  \n           'the title'  \n\n[foo]\n"
            . "<p><a href=\"/url\" title=\"the title\">foo</a></p>\n")
           ("[Foo*bar\\]]:my_(url) 'title (with parens)'\n\n[Foo*bar\\]]\n"
            . ,(concat "<p><a href=\"my_(url)\" title=\"title (with parens)\">"
                       "Foo*bar]</a></p>\n"))
           ("[Foo bar]:\n<my url>\n'title'\n\n[Foo bar]\n"
            . "<p><a href=\"my%20url\" title=\"title\">Foo bar</a></p>\n")
           ("[foo]: /url '\ntitle\nline1\nline2\n'\n\n[foo]\n"
            . ,(concat "<p><a href=\"/url\" title=\"\n"
                       "title\nline1\nline2\n\">foo</a></p>\n"))
           ("[foo]:\n/url\n\n[foo]\n"
            . "<p><a href=\"/url\">foo</a></p>\n")
           ("[foo]: <bar>(baz)\n\n[foo]\n"
            . "<p>[foo]: <bar>(baz)</p>\n<p>[foo]</p>\n")
           ("[foo]: /url\\bar\\*baz \"foo\\\"bar\\baz\"\n\n[foo]\n"
            . ,(concat "<p><a href=\"/url%5Cbar*baz\" "
                       "title=\"foo&quot;bar\\baz\">foo</a></p>\n"))
           (,(concat "[" upper-greek-label "]: " greek-url
                     "\n\n[" greek-label "]\n")
            . ,(concat "<p><a href=\"/%CF%86%CE%BF%CF%85\">"
                       greek-label "</a></p>\n"))
           ("[\nfoo\n]: /url\nbar\n"
            . "<p>bar</p>\n")
           ("    [foo]: /url \"title\"\n\n[foo]\n"
            . ,(concat "<pre><code>[foo]: /url &quot;title&quot;\n"
                       "</code></pre>\n<p>[foo]</p>\n"))
           ("Foo\n[bar]: /baz\n\n[bar]\n"
            . "<p>Foo\n[bar]: /baz</p>\n<p>[bar]</p>\n")
           ("[foo]\n\n> [foo]: /url\n"
            . ,(concat "<p><a href=\"/url\">foo</a></p>\n"
                       "<blockquote>\n</blockquote>\n"))))
      (commonmark-gfm-test--renders (car case) (cdr case)))))

(ert-deftest commonmark-gfm-render-character-references ()
  (commonmark-gfm-test--renders
   "AT&amp;T &#42; &unknown;\n"
   "<p>AT&amp;T * &amp;unknown;</p>\n"))

(ert-deftest commonmark-gfm-render-commonmark-entity-regressions ()
  (dolist
      (case
       `((,(concat "&nbsp; &amp; &copy; &AElig; &Dcaron;\n"
                   "&frac34; &HilbertSpace; &DifferentialD;\n"
                   "&ClockwiseContourIntegral; &ngE;\n")
          . ,(concat "<p>" (string #xa0) " &amp; " (string #xa9)
                     " " (string #xc6) " " (string #x10e) "\n"
                     (string #xbe) " " (string #x210b) " "
                     (string #x2146) "\n"
                     (string #x2232) " " (string #x2267)
                     (string #x338) "</p>\n"))
         ("&#35; &#1234; &#992; &#0;\n"
          . ,(concat "<p># " (string #x4d2) " " (string #x3e0)
                     " " (string #xfffd) "</p>\n"))
         ("[foo](/f&ouml;&ouml; \"f&ouml;&ouml;\")\n"
          . ,(concat "<p><a href=\"/f%C3%B6%C3%B6\" title=\"f"
                     (string #xf6) (string #xf6) "\">foo</a></p>\n"))
         ("``` f&ouml;&ouml;\nfoo\n```\n"
          . ,(concat "<pre><code class=\"language-f"
                     (string #xf6) (string #xf6) "\">foo\n"
                     "</code></pre>\n"))))
    (commonmark-gfm-test--renders (car case) (cdr case))))

(ert-deftest commonmark-gfm-render-commonmark-backslash-regressions ()
  (dolist
      (case
       `((,(concat "\\\t\\A\\a\\ \\3\\"
                   (string #x03c6)
                   "\\"
                   (string #xab)
                   "\n")
          . ,(concat "<p>\\\t\\A\\a\\ \\3\\"
                     (string #x03c6)
                     "\\"
                     (string #xab)
                     "</p>\n"))
         ("<a href=\"/bar\\/)\">\n"
          . "<a href=\"/bar\\/)\">\n")
         ("``` foo\\+bar\nfoo\n```\n"
          . "<pre><code class=\"language-foo+bar\">foo\n</code></pre>\n")))
    (commonmark-gfm-test--renders (car case) (cdr case))))

(ert-deftest commonmark-gfm-render-emphasis ()
  (commonmark-gfm-test--renders
   "A *word* and _word_.\n"
   "<p>A <em>word</em> and <em>word</em>.</p>\n"))

(ert-deftest commonmark-gfm-render-strong-emphasis ()
  (commonmark-gfm-test--renders
   "A **word** and __word__.\n"
   "<p>A <strong>word</strong> and <strong>word</strong>.</p>\n"))

(ert-deftest commonmark-gfm-render-nested-emphasis ()
  (commonmark-gfm-test--renders
   "*foo **bar** baz*\n"
   "<p><em>foo <strong>bar</strong> baz</em></p>\n"))

(ert-deftest commonmark-gfm-render-triple-emphasis ()
  (commonmark-gfm-test--renders
   "***foo***\n"
   "<p><em><strong>foo</strong></em></p>\n"))

(ert-deftest commonmark-gfm-render-intraword-underscore-literal ()
  (commonmark-gfm-test--renders
   "foo_bar_baz\n"
   "<p>foo_bar_baz</p>\n"))

(ert-deftest commonmark-gfm-render-unclosed-emphasis-literal ()
  (commonmark-gfm-test--renders
   "*foo\n"
   "<p>*foo</p>\n"))

(ert-deftest commonmark-gfm-render-commonmark-emphasis-regressions ()
  (let ((nbsp (string #xa0)))
    (dolist (case
             `(("a*\"foo\"*\n"
                . "<p>a*&quot;foo&quot;*</p>\n")
               (,(concat "*" nbsp "a" nbsp "*\n")
                . ,(concat "<p>*" nbsp "a" nbsp "*</p>\n"))
               ("*foo bar\n*\n"
                . "<p>*foo bar\n*</p>\n")
               ("*(*foo*)*\n"
                . "<p><em>(<em>foo</em>)</em></p>\n")
               ("_(_foo_)_\n"
                . "<p><em>(<em>foo</em>)</em></p>\n")
               ("*foo**\n"
                . "<p><em>foo</em>*</p>\n")
               ("**foo***\n"
                . "<p><strong>foo</strong>*</p>\n")
               ("_foo__\n"
                . "<p><em>foo</em>_</p>\n")
               ("****foo****\n"
                . "<p><strong><strong>foo</strong></strong></p>\n")
               ("******foo******\n"
                . ,(concat "<p><strong><strong><strong>foo</strong>"
                           "</strong></strong></p>\n"))
               ("_____foo_____\n"
                . ,(concat "<p><em><strong><strong>foo</strong>"
                           "</strong></em></p>\n"))
               ("*[bar*](/url)\n"
                . "<p>*<a href=\"/url\">bar*</a></p>\n")
               ("_foo [bar_](/url)\n"
                . "<p>_foo <a href=\"/url\">bar_</a></p>\n")
               ("*<img src=\"foo\" title=\"*\"/>\n"
                . "<p>*<img src=\"foo\" title=\"*\"/></p>\n")
               ("**a<https://foo.bar/?q=**>\n"
                . ,(concat "<p>**a<a href=\"https://foo.bar/?q=**\">"
                           "https://foo.bar/?q=**</a></p>\n"))
               ("__foo_ bar_\n"
                . "<p><em><em>foo</em> bar</em></p>\n")
               ("*foo *bar**\n"
                . "<p><em>foo <em>bar</em></em></p>\n")
               ("*foo**bar**baz*\n"
                . "<p><em>foo<strong>bar</strong>baz</em></p>\n")
               ("*foo**bar*\n"
                . "<p><em>foo**bar</em></p>\n")
               ("***foo** bar*\n"
                . "<p><em><strong>foo</strong> bar</em></p>\n")
               ("*foo **bar***\n"
                . "<p><em>foo <strong>bar</strong></em></p>\n")
               ("*foo**bar***\n"
                . "<p><em>foo<strong>bar</strong></em></p>\n")
               ("____foo__ bar__\n"
                . "<p><strong><strong>foo</strong> bar</strong></p>\n")
               ("**foo **bar****\n"
                . "<p><strong>foo <strong>bar</strong></strong></p>\n")
               ("***foo* bar**\n"
                . "<p><strong><em>foo</em> bar</strong></p>\n")
               ("**foo *bar***\n"
                . "<p><strong>foo <em>bar</em></strong></p>\n")
               ("*foo __bar *baz bim__ bam*\n"
                . "<p><em>foo <strong>bar *baz bim</strong> bam</em></p>\n")))
      (should (string= (cdr case)
                       (commonmark-gfm-render-to-html
                        (car case)
                        '(:gfm nil)))))))

(ert-deftest commonmark-gfm-render-gfm-emphasis-regressions ()
  (dolist
      (case
       '(("__foo, __bar__, baz__\n"
          . "<p><strong>foo, bar, baz</strong></p>\n")
         ("foo******bar*********baz\n"
          . "<p>foo<strong>bar</strong>***baz</p>\n")
         ("__foo __bar__ baz__\n"
          . "<p><strong>foo bar baz</strong></p>\n")
         ("____foo__ bar__\n"
          . "<p><strong>foo bar</strong></p>\n")
         ("**foo **bar****\n"
          . "<p><strong>foo bar</strong></p>\n")
         ("****foo****\n"
          . "<p><strong>foo</strong></p>\n")
         ("____foo____\n"
          . "<p><strong>foo</strong></p>\n")
         ("******foo******\n"
          . "<p><strong>foo</strong></p>\n")
         ("_____foo_____\n"
          . "<p><em><strong>foo</strong></em></p>\n")))
    (commonmark-gfm-test--renders (car case) (cdr case))))

(ert-deftest commonmark-gfm-render-gfm-strikethrough ()
  (commonmark-gfm-test--renders "a ~~deleted~~ word\n"
                                "<p>a <del>deleted</del> word</p>\n"))

(ert-deftest commonmark-gfm-render-gfm-strikethrough-regressions ()
  (dolist
      (case
       '(("~~Hi~~ Hello, ~there~ world!\n"
          . "<p><del>Hi</del> Hello, <del>there</del> world!</p>\n")
         ("This will ~~~not~~~ strike.\n"
          . "<p>This will ~~~not~~~ strike.</p>\n")))
    (commonmark-gfm-test--renders (car case) (cdr case))))

(ert-deftest commonmark-gfm-render-hardbreak ()
  (commonmark-gfm-test--renders "foo\\\nbar\n"
                                "<p>foo<br />\nbar</p>\n"))

(ert-deftest commonmark-gfm-render-autolink ()
  (commonmark-gfm-test--renders
   "<https://example.invalid>\n"
   "<p><a href=\"https://example.invalid\">https://example.invalid</a></p>\n"))

(ert-deftest commonmark-gfm-render-commonmark-autolink-regressions ()
  (dolist
      (case
       '(("<https://foo.bar/baz bim>\n"
          . "<p>&lt;https://foo.bar/baz bim&gt;</p>\n")
         ("<https://example.com/\\[\\>\n"
          . "<p><a href=\"https://example.com/%5C%5B%5C\">https://example.com/\\[\\</a></p>\n")
         ("<foo\\+@bar.example.com>\n"
          . "<p>&lt;foo+@bar.example.com&gt;</p>\n")
         ("< https://foo.bar >\n"
          . "<p>&lt; https://foo.bar &gt;</p>\n")
         ("<m:abc>\n"
          . "<p>&lt;m:abc&gt;</p>\n")
         ("<foo.bar.baz>\n"
          . "<p>&lt;foo.bar.baz&gt;</p>\n")
         ("https://example.com\n"
          . "<p>https://example.com</p>\n")
         ("foo@bar.example.com\n"
          . "<p>foo@bar.example.com</p>\n")))
    (should (string= (cdr case)
                     (commonmark-gfm-render-to-html
                      (car case)
                      '(:gfm nil))))))

(ert-deftest commonmark-gfm-render-gfm-bare-url-autolink ()
  (commonmark-gfm-test--renders
   "Visit https://example.invalid/path.\n"
   "<p>Visit <a href=\"https://example.invalid/path\">https://example.invalid/path</a>.</p>\n"))

(ert-deftest commonmark-gfm-render-gfm-www-autolink ()
  (commonmark-gfm-test--renders
   "Visit www.example.invalid\n"
   "<p>Visit <a href=\"http://www.example.invalid\">www.example.invalid</a></p>\n"))

(ert-deftest commonmark-gfm-render-gfm-email-autolink ()
  (commonmark-gfm-test--renders
   "Mail user@example.invalid\n"
   "<p>Mail <a href=\"mailto:user@example.invalid\">user@example.invalid</a></p>\n"))

(ert-deftest commonmark-gfm-render-gfm-autolink-regressions ()
  (dolist
      (case
       '(("www.google.com/search?q=Markup+(business)))\n"
          . "<p><a href=\"http://www.google.com/search?q=Markup+(business)\">www.google.com/search?q=Markup+(business)</a>))</p>\n")
         ("www.google.com/search?q=commonmark&hl=en\n\nwww.google.com/search?q=commonmark&hl;\n"
          . "<p><a href=\"http://www.google.com/search?q=commonmark&amp;hl=en\">www.google.com/search?q=commonmark&amp;hl=en</a></p>\n<p><a href=\"http://www.google.com/search?q=commonmark\">www.google.com/search?q=commonmark</a>&amp;hl;</p>\n")
         ("Anonymous FTP is available at ftp://foo.bar.baz.\n"
          . "<p>Anonymous FTP is available at <a href=\"ftp://foo.bar.baz\">ftp://foo.bar.baz</a>.</p>\n")
         ("a.b-c_d@a.b\n\na.b-c_d@a.b.\n\na.b-c_d@a.b-\n\na.b-c_d@a.b_\n"
          . "<p><a href=\"mailto:a.b-c_d@a.b\">a.b-c_d@a.b</a></p>\n<p><a href=\"mailto:a.b-c_d@a.b\">a.b-c_d@a.b</a>.</p>\n<p>a.b-c_d@a.b-</p>\n<p>a.b-c_d@a.b_</p>\n")
         ("<http://foo.bar/baz bim>\n"
          . "<p>&lt;http://foo.bar/baz bim&gt;</p>\n")))
    (commonmark-gfm-test--renders (car case) (cdr case))))

(ert-deftest commonmark-gfm-render-gfm-tagfilter-inline ()
  (commonmark-gfm-test--renders
   "a <script> b\n"
   "<p>a &lt;script> b</p>\n"))

(ert-deftest commonmark-gfm-render-commonmark-raw-html-regressions ()
  (dolist
      (case
       '(("<a foo=\"bar\" bam = 'baz <em>\"</em>'\n_boolean zoop:33=zoop:33 />\n"
          . "<p><a foo=\"bar\" bam = 'baz <em>\"</em>'\n_boolean zoop:33=zoop:33 /></p>\n")
         ("<a h*#ref=\"hi\">\n"
          . "<p>&lt;a h*#ref=&quot;hi&quot;&gt;</p>\n")
         ("foo <!--> foo -->\n\nfoo <!---> foo -->\n"
          . "<p>foo <!--> foo --&gt;</p>\n<p>foo <!---> foo --&gt;</p>\n")
         ("foo <?php echo $a; ?>\n"
          . "<p>foo <?php echo $a; ?></p>\n")
         ("foo <!ELEMENT br EMPTY>\n"
          . "<p>foo <!ELEMENT br EMPTY></p>\n")
         ("foo <![CDATA[>&<]]>\n"
          . "<p>foo <![CDATA[>&<]]></p>\n")))
    (should (string= (cdr case)
                     (commonmark-gfm-render-to-html
                      (car case)
                      '(:gfm nil))))))

(ert-deftest commonmark-gfm-render-gfm-tagfilter-block ()
  (commonmark-gfm-test--renders
   "<script>alert(1)</script>\n"
   "&lt;script>alert(1)&lt;/script>\n"))

(ert-deftest commonmark-gfm-render-commonmark-html-block-regressions ()
  (dolist
      (case
       '(("<table><tr><td>\n<pre>\n**Hello**,\n\n_world_.\n</pre>\n</td></tr></table>\n"
          . "<table><tr><td>\n<pre>\n**Hello**,\n<p><em>world</em>.\n</pre></p>\n</td></tr></table>\n")
         ("<textarea>\n\n*foo*\n\n_bar_\n\n</textarea>\n"
          . "<textarea>\n\n*foo*\n\n_bar_\n\n</textarea>\n")
         ("<style\n  type=\"text/css\">\nh1 {color:red;}\n\np {color:blue;}\n</style>\nokay\n"
          . "<style\n  type=\"text/css\">\nh1 {color:red;}\n\np {color:blue;}\n</style>\n<p>okay</p>\n")
         ("Foo\n<a href=\"bar\">\nbaz\n"
          . "<p>Foo\n<a href=\"bar\">\nbaz</p>\n")
         ("<table>\n\n<tr>\n\n<td>\nHi\n</td>\n\n</tr>\n\n</table>\n"
          . "<table>\n<tr>\n<td>\nHi\n</td>\n</tr>\n</table>\n")))
    (should (string= (cdr case)
                     (commonmark-gfm-render-to-html
                      (car case)
                      '(:gfm nil))))))

(ert-deftest commonmark-gfm-render-gfm-table ()
  (commonmark-gfm-test--renders
   "| a | b |\n|---|---:|\n| c | d |\n"
   (concat "<table>\n<thead>\n<tr>\n"
           "<th>a</th>\n<th align=\"right\">b</th>\n"
           "</tr>\n</thead>\n<tbody>\n<tr>\n"
           "<td>c</td>\n<td align=\"right\">d</td>\n"
           "</tr>\n</tbody>\n</table>\n")))

(ert-deftest commonmark-gfm-render-gfm-table-regressions ()
  (dolist
      (case
       '(("| f\\|oo  |\n| ------ |\n| b `\\|` az |\n| b **\\|** im |\n"
          . "<table>\n<thead>\n<tr>\n<th>f|oo</th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td>b <code>|</code> az</td>\n</tr>\n<tr>\n<td>b <strong>|</strong> im</td>\n</tr>\n</tbody>\n</table>\n")
         ("| abc | def |\n| --- | --- |\n| bar | baz |\nbar\n\nbar\n"
          . "<table>\n<thead>\n<tr>\n<th>abc</th>\n<th>def</th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td>bar</td>\n<td>baz</td>\n</tr>\n<tr>\n<td>bar</td>\n<td></td>\n</tr>\n</tbody>\n</table>\n<p>bar</p>\n")
         ("| abc | def |\n| --- |\n| bar |\n"
          . "<p>| abc | def |\n| --- |\n| bar |</p>\n")))
    (commonmark-gfm-test--renders (car case) (cdr case))))

(ert-deftest commonmark-gfm-render-gfm-task-list ()
  (commonmark-gfm-test--renders
   "- [x] done\n- [ ] todo\n"
   (concat "<ul>\n"
           "<li><input checked=\"\" disabled=\"\" type=\"checkbox\"> done</li>\n"
           "<li><input disabled=\"\" type=\"checkbox\"> todo</li>\n"
           "</ul>\n")))

(ert-deftest commonmark-gfm-render-commonmark-mode-disables-gfm-extensions ()
  (dolist
      (case
       '(("~~x~~\n"
          . "<p>~~x~~</p>\n")
         ("- [x] done\n"
          . "<ul>\n<li>[x] done</li>\n</ul>\n")
         ("| a | b |\n| - | - |\n"
          . "<p>| a | b |\n| - | - |</p>\n")
         ("www.commonmark.org\n"
          . "<p>www.commonmark.org</p>\n")))
    (should (string= (cdr case)
                     (commonmark-gfm-render-to-html
                      (car case)
                      '(:gfm nil))))))

(ert-deftest commonmark-gfm-render-markdown-command ()
  (with-temp-buffer
    (insert "# Command\n")
    (let ((source (current-buffer))
          (output (generate-new-buffer " *commonmark-gfm-test-output*")))
      (unwind-protect
          (progn
            (with-current-buffer source
              (commonmark-gfm-markdown-command (point-min) (point-max) output))
            (with-current-buffer output
              (should (string= "<h1>Command</h1>\n" (buffer-string)))))
        (kill-buffer output)))))

(ert-deftest commonmark-gfm-spec-define-tests-from-json ()
  (let ((file (make-temp-file "commonmark-gfm-spec-" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "[{\"example\":1,\"section\":\"Smoke\",")
            (insert "\"markdown\":\"# Smoke\\n\",")
            (insert "\"html\":\"<h1>Smoke</h1>\\n\"}]"))
          (commonmark-gfm-spec-define-tests file 'smoke)
          (should (ert-test-boundp 'commonmark-gfm-spec/smoke/0001-smoke)))
      (delete-file file))))

(ert-deftest commonmark-gfm-spec-run-file-counts-results ()
  (let ((file (make-temp-file "commonmark-gfm-spec-" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "[{\"example\":1,\"section\":\"Smoke\",")
            (insert "\"markdown\":\"# Smoke\\n\",")
            (insert "\"html\":\"<h1>Smoke</h1>\\n\"},")
            (insert "{\"example\":2,\"section\":\"Smoke\",")
            (insert "\"markdown\":\"plain\\n\",")
            (insert "\"html\":\"<p>wrong</p>\\n\"}]"))
          (let ((result (commonmark-gfm-spec-run-file file)))
            (should (= 2 (plist-get result :total)))
            (should (= 1 (plist-get result :passed)))
            (should (= 1 (length (plist-get result :failed))))))
      (delete-file file))))

(ert-deftest commonmark-gfm-spec-run-text-file-with-options-function ()
  (let ((file (make-temp-file "commonmark-gfm-spec-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "## Autolinks\n\n")
            (insert "```` example\n")
            (insert "www.commonmark.org\n")
            (insert ".\n")
            (insert "<p>www.commonmark.org</p>\n")
            (insert "````\n\n")
            (insert "```` example autolink\n")
            (insert "www.commonmark.org\n")
            (insert ".\n")
            (insert "<p><a href=\"http://www.commonmark.org\">")
            (insert "www.commonmark.org</a></p>\n")
            (insert "````\n"))
          (let ((result (commonmark-gfm-spec-run-file
                         file
                         #'commonmark-gfm-spec-gfm-options)))
            (should (= 2 (plist-get result :total)))
            (should (= 2 (plist-get result :passed)))
            (should (null (plist-get result :failed)))))
      (delete-file file))))

(provide 'commonmark-gfm-test)

;;; commonmark-gfm-test.el ends here
