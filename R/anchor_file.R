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
#' @param ... Passed between methods.
#'
#' @return `file`, invisibly.
#' @export
winsteps_write_anchor_file <- function(x, ...) {
  UseMethod("winsteps_write_anchor_file")
}

#' @rdname winsteps_write_anchor_file
#' @export
winsteps_write_anchor_file.winsteps_anchors <- function(x, file, digits = NULL, ...) {
  values <- x$values
  if (!is.null(digits)) values <- round(values, digits)

  out <- data.frame(
    seq = seq_along(x$items),
    # decimal.mark is pinned for the same reason scientific notation is
    # suppressed: Winsteps parses "0.5", and options(OutDec = ",") would
    # otherwise write "0,5" here without any error.
    value = format(values, scientific = FALSE, trim = TRUE, decimal.mark = "."),
    delim = ";",
    item = x$items,
    stringsAsFactors = FALSE
  )
  utils::write.table(out, file = file, sep = "\t",
                     row.names = FALSE, col.names = FALSE, quote = FALSE)
  invisible(file)
}

#' @rdname winsteps_write_anchor_file
#' @export
winsteps_write_anchor_file.default <- function(x, values, file, digits = NULL, ...) {
  winsteps_write_anchor_file(winsteps_anchors(x, values), file = file, digits = digits)
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
#' @param ... Passed between methods.
#'
#' @return `file`, invisibly.
#' @export
winsteps_write_item_subset_file <- function(x, ...) {
  UseMethod("winsteps_write_item_subset_file")
}

#' @rdname winsteps_write_item_subset_file
#' @export
winsteps_write_item_subset_file.winsteps_anchors <- function(x, keep, file, ...) {
  winsteps_write_item_subset_file(x$items, keep = keep, file = file)
}

#' @rdname winsteps_write_item_subset_file
#' @export
winsteps_write_item_subset_file.default <- function(x, keep, file, ...) {
  check_items(x)
  check_keep(keep, x)

  exclude <- !(x %in% keep)
  out <- data.frame(
    seq = seq_along(x)[exclude],
    delim = ";",
    item = x[exclude],
    stringsAsFactors = FALSE
  )
  utils::write.table(out, file = file, sep = "\t",
                     row.names = FALSE, col.names = FALSE, quote = FALSE)
  invisible(file)
}
