#' Round the numeric columns in a data.frame.
#'
#' @description
#' Can run on any `data.frame` with at least one numeric column.
#' This function defaults to excluding the first column of the input data.frame,
#' assuming that it contains a descriptive variable, but this can be overridden by
#' specifying the columns to round in the `cols` argument.
#'
#' If you're formatting percentages, e.g., the result of [adorn_percentages()],
#' use [adorn_pct_formatting()] instead.  This is a more flexible variant for ad-hoc usage.
#' Compared to `adorn_pct_formatting()`, it does not multiply by 100 or pad the
#' numbers with spaces for alignment in the results `data.frame`.
#' This function retains the class of numeric input columns.
#'
#' @param dat A `tabyl` or other `data.frame` with similar layout.
#'   If given a list of data.frames, this function will apply itself to each
#'   `data.frame` in the list (designed for 3-way `tabyl` lists).
#' @param digits How many digits should be displayed after the decimal point?
#' @param rounding Method to use for rounding - either "half to even"
#'   (the base R default method), or "half up", where 14.5 rounds up to 15.
#' @param cols Columns to adorn. This takes a tidyselect specification and is
#'   the recommended interface for selecting columns.
#' @param ... Legacy columns-to-adorn tidyselect specification. Supply either
#'   `cols` or `...`, not both.
#'   By default, all numeric columns (besides the initial column, if numeric)
#'   are adorned, but this allows you to manually specify which columns should
#'   be adorned, for use on a data.frame that does not result from a call to `tabyl`.
#'
#' @return The `data.frame` with rounded numeric columns.
#' @export
#' @examples
#'
#' mtcars %>%
#'   tabyl(am, cyl) %>%
#'   adorn_percentages() %>%
#'   adorn_rounding(digits = 2, rounding = "half up")
#'
#' # tolerates non-numeric columns:
#' library(dplyr)
#' mtcars %>%
#'   tabyl(am, cyl) %>%
#'   adorn_percentages("all") %>%
#'   mutate(dummy = "a") %>%
#'   adorn_rounding()
#'
#' # Select columns without positional placeholder commas:
#' cases <- data.frame(
#'   region = c("East", "West"),
#'   year = 2015,
#'   recovered = c(125, 87),
#'   died = c(13, 12)
#' )
#'
#' cases %>%
#'   adorn_percentages(cols = ends_with("ed")) %>%
#'   adorn_rounding(cols = all_of(c("recovered", "died")))
adorn_rounding <- function(dat, digits = 1, rounding = "half to even", ..., cols = NULL) {
  cols_enquo <- rlang::enquo(cols)
  has_cols <- !rlang::quo_is_null(cols_enquo)
  has_dots <- rlang::dots_n(...) > 0
  if (has_cols && has_dots) {
    stop("Specify columns to adorn using either 'cols' or '...', not both.")
  }
  # if input is a list, call purrr::map to recursively apply this function to each data.frame
  if (is.list(dat) && !is.data.frame(dat)) {
    if (has_cols) {
      purrr::map(dat, function(x) {
        adorn_rounding(x, digits = digits, rounding = rounding, cols = !!cols_enquo)
      })
    } else {
      purrr::map(dat, adorn_rounding, digits, rounding, ...)
    }
  } else {
    # catch bad inputs
    if (!is.data.frame(dat)) {
      stop("adorn_rounding() must be called on a data.frame or list of data.frames")
    }
    if (!rounding %in% c("half to even", "half up")) {
      stop("'rounding' must be one of 'half to even' or 'half up'")
    }
    numeric_cols <- which(vapply(dat, is.numeric, logical(1)))
    non_numeric_cols <- setdiff(1:ncol(dat), numeric_cols)
    # assume 1st column should not be included so remove it from numeric_cols.
    # Moved up to this line so that if only 1st col is numeric, the function errors
    numeric_cols <- setdiff(numeric_cols, 1)

    if (has_cols) {
      cols_to_round <- tidyselect::eval_select(cols_enquo, data = dat)
      if (any(cols_to_round %in% non_numeric_cols)) {
        message("At least one non-numeric column was specified and will not be modified.")
        cols_to_round <- setdiff(cols_to_round, non_numeric_cols)
      }
    } else if (!has_dots) {
      cols_to_round <- numeric_cols
    } else {
      expr <- rlang::expr(c(...))
      cols_to_round <- tidyselect::eval_select(expr, data = dat)
      if (any(cols_to_round %in% non_numeric_cols)) {
        message("At least one non-numeric column was specified and will not be modified.")
        cols_to_round <- setdiff(cols_to_round, non_numeric_cols)
      }
    }

    if (rounding == "half to even") {
      dat[cols_to_round] <- lapply(dat[cols_to_round], function(x) round(x, digits = digits))
    } else {
      dat[cols_to_round] <- lapply(dat[cols_to_round], function(x) round_half_up(x, digits = digits))
    }
    dat
  }
}
