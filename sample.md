# commonmark-gfm.el Sample

This file is a small rendering sample for `commonmark-gfm.el`.

## Inline Markup

Normal text can contain *emphasis*, **strong emphasis**, and
~~strikethrough~~.

Use backticks for code spans: `(+ 1 2)`.

Escapes work too: \*this is literal asterisks\*.

## Links and Images

Inline link:

[GNU Emacs](https://www.gnu.org/software/emacs/)

Reference link:

[CommonMark][commonmark]

Autolink literals in GFM mode:

Visit www.commonmark.org/help or https://github.github.com/gfm/.

Email autolink:

contact@example.com

[commonmark]: https://commonmark.org/ "CommonMark"

## Lists

- One
- Two
  - Nested item
  - Another nested item
- Three

1. First ordered item
2. Second ordered item
3. Third ordered item

## Task List

- [x] Parse CommonMark examples
- [x] Render GFM tables
- [ ] Build an interactive preview buffer
- [ ] Add more source position coverage

## Table

| Feature | Status | Notes |
| :------ | :----: | ----: |
| CommonMark mode | done | 652/652 |
| GFM tables | done | pipe escape: `\|` |
| Task lists | done | disabled checkboxes |
| Preview UI | todo | Emacs buffer integration |

## Block Quote

> Markdown is plain text first.
>
> - It should be readable before rendering.
> - It should produce predictable HTML after rendering.

## Code Fence

```elisp
(require 'commonmark-gfm)

(commonmark-gfm-render-to-html "# Hello\n")
```

## Raw HTML And Tagfilter

Raw inline HTML such as <span class="sample">span</span> is preserved.

GFM tagfilter escapes dangerous-looking tags such as <script>.

## Hard Break

This line ends with a backslash.\
This is the next visual line.

