#' Bundle item names with their anchor values
#'
#' `items` and `values` are two vectors that must stay in the same order:
#' sequence number is position, so the anchor file, the item subset file and
#' the data file all index items the same way. Keeping them in one object
#' makes that invariant structural rather than something each function
#' re-checks and each caller has to remember.
#'
#' Validation happens once here rather than in every writer, so a duplicated
#' item name, a name carrying the anchor file's own delimiters, or a missing
#' anchor value is caught at the point the bank is assembled -- before any
#' file is written.
#'
#' @param items Character vector of item names. Must be unique and free of
#'   tabs and semicolons.
#' @param values Numeric vector of anchor values (e.g. IRT b-parameters), the
#'   same length and order as `items`. Must all be finite.
#'
#' @return An object of class `winsteps_anchors`: a list of `items` and
#'   `values`, which prints as a summary of the bank.
#' @export
#'
#' @examples
#' anchors <- winsteps_anchors(c("q1", "q2", "q3"), c(-0.4, 0.1, 0.6))
#' anchors
#' anchors$items
winsteps_anchors <- function(items, values) {
  if (length(items) != length(values)) {
    stop("items and values must be the same length", call. = FALSE)
  }
  items <- as.character(items)
  values <- as.numeric(values)
  check_items(items)
  check_finite_anchors(values, items)
  structure(list(items = items, values = values), class = "winsteps_anchors")
}

#' @export
print.winsteps_anchors <- function(x, n = 6, ...) {
  cat("<winsteps_anchors> ", length(x$items), " items\n", sep = "")
  cat("  Logits  ",
      format(min(x$values), digits = 3), " to ", format(max(x$values), digits = 3),
      "  (mean ", format(mean(x$values), digits = 3), ")\n", sep = "")
  cat("  Items   ", format_examples(x$items, n = n), "\n", sep = "")
  invisible(x)
}

# Accept either a prebuilt bank or the two loose vectors, so callers can move
# to winsteps_anchors() at their own pace.
resolve_anchors <- function(anchors, items, values) {
  if (!is.null(anchors)) {
    if (!is.null(items) || !is.null(values)) {
      stop("Pass either anchors, or items and anchor_values -- not both.",
           call. = FALSE)
    }
    if (!inherits(anchors, "winsteps_anchors")) {
      stop("anchors must be a winsteps_anchors object; build one with ",
           "winsteps_anchors(items, values).", call. = FALSE)
    }
    return(anchors)
  }
  if (is.null(items) || is.null(values)) {
    stop("Supply anchors, or both items and anchor_values.", call. = FALSE)
  }
  winsteps_anchors(items, values)
}

# Items to retain must name items the bank actually holds. Checked before any
# file is written, rather than when the subset file is reached.
check_keep <- function(keep, items) {
  if (length(keep) == 0) {
    stop("keep is empty, which would delete every item from the estimation. ",
         "Pass the items to retain, or omit the item subset file entirely.",
         call. = FALSE)
  }
  unknown <- setdiff(keep, items)
  if (length(unknown) > 0) {
    stop("keep contains items not present in items: ",
         format_examples(unknown), call. = FALSE)
  }
  invisible(keep)
}
