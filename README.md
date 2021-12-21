
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/arcaldwell49/sportrxivdown/workflows/R-CMD-check/badge.svg)](https://github.com/arcaldwell49/sportrxivdown/actions)

<!-- badges: end -->

# `sportrxivdown`

The `sportrxivdown` package (pronounced “Sport Archive Down”) provides a
suite of custom [R Markdown](https://rmarkdown.rstudio.com) templates
(currently just Word documents). The templates are provided and
maintained by the community, and anyone can contribute a new template.
See [How to contribute](#how-to-contribute) below. The main requirement
is that templates must meet the submission standards of
[SportRxiv](https://sportrxiv.org).

## Installation

You can install and use `sportrxivdown` from GitHub as follows:

``` r
devtools::install_github("arcaldwell49/sportrxivdown")
```

## Templates

Currently included templates and their contributors are the following:

1.  SportRxiv Word document (`sportrxiv_word`)
2.  SportRxiv PDF document’s are the next on the list to be added

## Code of Conduct

Please note that the sportrxivdown is a community run initiative. The
owner of this repository, Aaron Caldwell, expects all contributors
(himself included) to treat those in this community with dignity and
respect. All contributor’s are expected to follow the general
[Contributor’s Convenant](https://www.contributor-covenant.org/).

## How to contribute?

Most of the templates are contributed directly by the users in the
community. If you want `sportrxivdown` to offer a new journal format,
you can contribute by the following way.

### Suggest an idea for new format opening an issue.

You may not feel confident enough or may not have time to contribute a
new format. By opening a new issue, you can share the idea for this
format, and see if someone in the community can help on it.  
This is not the best way to quickly get your format included but at
least it is a great way to see if others are interested too.

To see the existing suggested formats, just filter issues with the [help
wanted](https://github.com/arcaldwell49/sportrxivdown/labels/help%20wanted)
label. You can then add a :+1: or help to add the template :wink:.

### Contribute a new template format opening a pull request.

To contribute a new format, you need to open a new pull request (PR).
When opening the PR, you’ll see the [PR
template](https://pkgs.rstudio.com/rticles/PULL_REQUEST_TEMPLATE.html)
explaining how to proceed and what is important to check. Please follow
it.  
Even if you are just starting or you are not finished, you share your
work by creating a [draft
PR](https://docs.github.com/en/github/collaborating-with-issues-and-pull-requests/about-pull-requests#draft-pull-requests).
It is a great way to let us know that you are still working on it (like
[these opened
ones](https://github.com/rstudio/rticles/pulls?q=is%3Apr+draft%3Atrue+)),
and it is also a great way to ask for help from the community.  
When you are ready, you can submit the PR for review, and we will
iterate until it is merged.

#### Technical resources helpful to contribute a template

The best way to get started is to look at the previous examples of
submitted PR. You’ll find links to them in the table [above](#overview).

All the `sportrxivdown` format are build similarly by providing a new
pandoc template to replace the default one. You’ll learn more about
pandoc templates in these places:

-   [R Markdown
    Cookbook](https://bookdown.org/yihui/rmarkdown-cookbook/latex-template.html)
-   [The Pandoc manual](https://pandoc.org/MANUAL.html#templates)

You can study [existing formats](inst/rmarkdown/templates) to see how
all this works.
