# commonmark-gfm.el

Pure Emacs Lisp CommonMark/GFM renderer scaffold.

## Status

This package is not CommonMark-compatible yet.  It currently provides:

- An AST shape for block and inline nodes.
- A growing block parser for headings, thematic breaks, block quotes, lists,
  code blocks, HTML blocks, raw HTML, and GFM tables.
- Block parsing includes CommonMark-oriented block quote lazy continuation,
  tab-aware container indentation, fenced-code indentation, and loose/tight
  list paragraph rendering.
- A small bootstrap inline parser for escapes, code spans, emphasis, strong
  emphasis, links, images, reference links, character references, autolinks,
  hard breaks, GFM strikethrough, GFM bare autolinks, and GFM tagfilter.
- CommonMark mode can be selected with `(:gfm nil)`; this is the mode used
  when checking the official CommonMark examples.
- The implementation currently passes 652/652 CommonMark 0.31.2 examples in
  CommonMark mode.
- The bundled `make check` suite now includes a focused GFM smoke fixture for
  tables, task lists, strikethrough, autolink literals, and tagfilter.
- CommonMark-style link reference definitions, including multiline labels,
  multiline titles, first-definition-wins precedence, and document-wide
  references discovered in basic containers.
- Emphasis parsing uses CommonMark-style flanking checks for `*` and `_`,
  skips code/link/html/autolink spans while scanning for closers, and handles
  balanced long delimiter runs, but it is not yet a full delimiter stack
  implementation.
- An HTML renderer.
- A `markdown-command` compatible function.
- An ERT bridge for CommonMark/GFM JSON spec examples and cmark-gfm
  side-by-side `spec.txt` examples.
- Local cmark-gfm `spec.txt` measurement currently reaches 672/672 using
  `commonmark-gfm-spec-gfm-options`, which checks the base examples and GFM
  extension examples with the render mode expected by the cmark-gfm spec text.
- Source positions on parsed block nodes and paragraph/heading inline spans
  using `((start-line start-column) (end-line end-column))`.

The goal is to grow this into a CommonMark/GFM implementation while keeping
the public API stable.

## Usage

Add this directory to `load-path`, then:

```elisp
(require 'commonmark-gfm)

(setq markdown-command #'commonmark-gfm-markdown-command)
(setq markdown-command-needs-filename nil)
```

Or call:

```elisp
(commonmark-gfm-use-as-markdown-command)
```

Programmatic rendering:

```elisp
(commonmark-gfm-render-to-html "# Hello\n")

;; Disable GFM extensions when checking pure CommonMark behavior.
(commonmark-gfm-render-to-html "https://example.com\n" '(:gfm nil))
```

## Development

Run the smoke tests and byte compilation:

```sh
make check
```

Run only ERT:

```sh
make test
```

Run the bundled JSON spec smoke fixture:

```sh
make spec
```

Run the bundled GFM extension smoke fixture:

```sh
make gfm-spec
```

Run another CommonMark-style JSON spec file and get pass/fail counts:

```sh
make spec SPEC=/path/to/spec.json
```

`SPEC` may also point at cmark-gfm's side-by-side `spec.txt` format for a
single render mode.  To measure cmark-gfm's full spec text with GFM extension
examples enabled and CommonMark examples checked in CommonMark mode:

```sh
make gfm-full-spec GFM_FULL_SPEC=/path/to/cmark-gfm/test/spec.txt
```

Or from Emacs Lisp:

```elisp
(commonmark-gfm-spec-report-file "/path/to/spec.json")

;; cmark-gfm spec.txt with GFM extension examples enabled.
(commonmark-gfm-spec-run-file
 "/path/to/cmark-gfm/test/spec.txt"
 #'commonmark-gfm-spec-gfm-options)
```

## Compatibility Roadmap

1. Add the official GFM spec fixture or fetch step and track the full GFM pass
   count in CI.
2. Extend source positions to exact columns for nested containers, GFM table
   cells, and every remaining inline edge case.
3. Replace the remaining bootstrap inline parser pieces with a full delimiter
   stack implementation rather than the current compatibility-oriented
   recursive scanner.
4. Finish any remaining GFM extension edge cases discovered by the official
   fixture.
5. Use `cmark-gfm` only as a development oracle, not as a runtime dependency.

## Public API

- `commonmark-gfm-parse`
- `commonmark-gfm-render-to-html`
- `commonmark-gfm-render-region-to-buffer`
- `commonmark-gfm-markdown-command`
- `commonmark-gfm-use-as-markdown-command`
- `commonmark-gfm-spec-run-file`
- `commonmark-gfm-spec-report-file`
- `commonmark-gfm-spec-define-tests`
