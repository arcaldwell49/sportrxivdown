#' Replace parentheses with brackets
#'
#' Takes a single character or a list of characters and replaces parentheses with brackets. Can be used to
#' prepare a string of statistics (e.g. containing degrees of freedom) for reporting within paretheses.
#'
#' @param x Character. Single character or list of characters.
#' @return An object of the same type as \code{x}.
#' @export

in_paren <- function(x) {
  if(is.list(x) & length(x) > 1) {
    simplify <- FALSE
    use_names <- TRUE
  } else {
    simplify <- TRUE
    use_names <- FALSE
  }

  x_in_paren <- sapply(
    x
    , gsub
    , pattern = "\\("
    , replacement = "["
    , simplify = simplify
    , USE.NAMES = use_names
  )
  x_in_paren <- sapply(
    x_in_paren
    , gsub, pattern = "\\)"
    , replacement = "]"
    , simplify = simplify
    , USE.NAMES = use_names
  )

  x_in_paren
}
