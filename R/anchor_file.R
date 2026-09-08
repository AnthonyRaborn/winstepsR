#' Write a Winsteps item anchor file (IAFILE)
#'
#' Produces a tab-delimited file with one line per item:
#' `sequence<TAB>value<TAB>;<TAB>item_name`. The trailing `;` marks the
#' rest of the line as a Winsteps comment, so the item name is carried
#' through purely for human readability and does not affect estimation.
#'
#' @param items Character vector of item names, in the same order used to
#'   build the data file (see `items` in [winsteps_prepare_person_data()]).
#' @param values Numeric vector of anchor values (e.g. IRT b-parameters),
#'   same length and order as `items`.
#' @param file Path to write the anchor file to.
#' @param digits Optional number of decimal places to round `values` to
#'   before writing.
#'
#' @return `file`, invisibly.
#' @export
winsteps_write_anchor_file <- function(items, values, file, digits = NULL) {
  if (length(items) != length(values)) {
    stop("items and values must be the same length", call. = FALSE)
  }
  if (!is.null(digits)) values <- round(values, digits)

  out <- data.frame(
    seq = seq_along(items),
    value = format(values, scientific = FALSE, trim = TRUE),
    delim = ";",
    item = items,
    stringsAsFactors = FALSE
  )
  utils::write.table(out, file = file, sep = "\t",
                      row.names = FALSE, col.names = FALSE, quote = FALSE)
  invisible(file)
}

#' Write a Winsteps item-subset (delete) file (IDFILE)
#'
#' Winsteps' `IDFILE=` lists item sequence numbers to exclude from
#' estimation. This is the mechanism used to score a subset of items
#' (e.g. a single content domain) while keeping one shared anchor file
#' for the full item set. This function inverts a "keep" list into the
#' "delete" list Winsteps expects.
#'
#' @param items Character vector of all item names, in the same order as
#'   the anchor/data files (i.e. sequence number = position in `items`).
#' @param keep Character vector of item names to *retain* for this
#'   estimation run; every other item in `items` is written to the delete
#'   file.
#' @param file Path to write the delete file to.
#'
#' @return `file`, invisibly.
#' @export
winsteps_write_item_subset_file <- function(items, keep, file) {
  unknown <- setdiff(keep, items)
  if (length(unknown) > 0) {
    stop("keep contains items not present in items: ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  exclude <- !(items %in% keep)
  out <- data.frame(
    seq = seq_along(items)[exclude],
    delim = ";",
    item = items[exclude],
    stringsAsFactors = FALSE
  )
  utils::write.table(out, file = file, sep = "\t",
                      row.names = FALSE, col.names = FALSE, quote = FALSE)
  invisible(file)
}
