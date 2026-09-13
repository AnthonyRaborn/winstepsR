#' Write a Winsteps item anchor file (IAFILE)
#'
#' Produces a tab-delimited file with one line per item:
#' `sequence<TAB>value<TAB>;<TAB>item_name`. The trailing `;` marks the
#' rest of the line as a Winsteps comment, so the item name is carried
#' through purely for human readability and does not affect estimation.
#'
#' @param x Either a [winsteps_anchors()] object, or a character vector of
#'   item names in the same order used to build the data file (see `items` in
#'   [winsteps_prepare_person_data()]).
#' @param values Numeric vector of anchor values (e.g. IRT b-parameters),
#'   same length and order as `x`. Must all be finite -- a missing anchor
#'   would be written literally and silently leave that item unanchored. Not
#'   used when `x` is a `winsteps_anchors`, which already carries them.
#' @param file Path to write the anchor file to.
#' @param digits Optional number of decimal places to round the values to
#'   before writing.
#'
#' @return The lines written to `file`, invisibly.
#' @examples
#' anchors <- winsteps_anchors(c("q01", "q02", "q03"), c(-1.2, 0, 0.8))
#' f <- tempfile(fileext = ".txt")
#'
#' winsteps_write_anchor_file(anchors, file = f)
#' readLines(f)
#'
#' # the bare item/value vectors work too, and digits rounds before writing
#' winsteps_write_anchor_file(c("q01", "q02"), c(-1.234, 0.789), f, digits = 1)
#' readLines(f)
#'
#' unlink(f)
#' @export
winsteps_write_anchor_file <- function(x, values = NULL, file, digits = NULL) {
  # A winsteps_anchors object was already validated at construction; only a
  # bare item vector needs validating here.
  if (!inherits(x, "winsteps_anchors")) {
    x <- winsteps_anchors(x, values)
  }

  item_values <- x$values
  if (!is.null(digits)) item_values <- round(item_values, digits)

  # decimal.mark is pinned for the same reason scientific notation is
  # suppressed: Winsteps parses "0.5", and options(OutDec = ",") would
  # otherwise write "0,5" here without any error.
  formatted_values <- format(item_values, scientific = FALSE, trim = TRUE,
                             decimal.mark = ".")
  lines <- paste(seq_along(x$items), formatted_values, ";", x$items, sep = "\t")
  writeLines(lines, con = file)
  invisible(lines)
}

#' Write a Winsteps item-subset (delete) file (IDFILE)
#'
#' Winsteps' `IDFILE=` lists item sequence numbers to exclude from
#' estimation. This is the mechanism used to score a subset of items
#' (e.g. a single content domain) while keeping one shared anchor file
#' for the full item set. This function inverts a "keep" list into the
#' "delete" list Winsteps expects.
#'
#' @param x Either a [winsteps_anchors()] object, or a character vector of
#'   all item names in the same order as the anchor/data files (i.e. sequence
#'   number = position).
#' @param keep Character vector of item names to *retain* for this
#'   estimation run; every other item is written to the delete file. Must be
#'   non-empty.
#' @param file Path to write the delete file to.
#'
#' @return The lines written to `file`, invisibly.
#' @examples
#' anchors <- winsteps_anchors(sprintf("q%02d", 1:6), seq(-1.5, 1.5, length.out = 6))
#' f <- tempfile(fileext = ".txt")
#'
#' # name the items to keep; every other item is written to the delete file,
#' # which is what Winsteps IDFILE= expects
#' winsteps_write_item_subset_file(anchors, keep = c("q01", "q03", "q05"), file = f)
#' readLines(f)
#'
#' unlink(f)
#' @export
winsteps_write_item_subset_file <- function(x, keep, file) {
  # A winsteps_anchors object's items were already validated at construction;
  # only a bare item vector needs validating here.
  if (inherits(x, "winsteps_anchors")) {
    items <- x$items
  } else {
    items <- x
    check_items(items)
  }
  check_keep(keep, items)

  exclude <- !(items %in% keep)
  lines <- paste(seq_along(items)[exclude], ";", items[exclude], sep = "\t")
  writeLines(lines, con = file)
  invisible(lines)
}
