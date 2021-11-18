.onLoad <- function(libname, pkgname) { # nocov start
  lang <- if(length(knitr::opts_knit$get("rmarkdown.pandoc.to")) > 0 && !is.null(rmarkdown::metadata$lang)) {
    rmarkdown::metadata$lang
  } else "english"

  op <- options()
  op_sportrxiv <- list(
    sportrxiv.language = lang
    , sportrxiv.terms = localize(lang)
    , sportrxiv.na_string = "NA"
    , sportrxiv.plot_colors = "greyscale"
    , sportrxiv.mse = TRUE
    , sportrxiv.sphericity_correction = "GG"
  )
  toset <- !(names(op_sportrxiv) %in% names(op))
  if(any(toset)) options(op_sportrxiv[toset])


  # Fix dplyr R CMD Check note
  if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

  invisible()
} # nocov end
