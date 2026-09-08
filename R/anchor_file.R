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
#'   same length and order as `items`. Must all be finite -- a missing anchor
#'   would be written literally and silently leave that item unanchored.
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
  winsteps_check_items(items)
  # A non-finite anchor would be written literally (e.g. "NA"), which Winsteps
  # cannot read as a logit; it would then estimate the item freely rather than
  # anchoring it, so the run silently stops being the anchored run requested.
  bad <- !is.finite(values)
  if (any(bad)) {
    stop("anchor values must all be finite; ", sum(bad), " are not, at item(s): ",
         paste(utils::head(items[bad], 5), collapse = ", "),
         if (sum(bad) > 5) ", ..." else "", call. = FALSE)
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
#'   file. Must be non-empty.
#' @param file Path to write the delete file to.
#'
#' @return `file`, invisibly.
#' @export
winsteps_write_item_subset_file <- function(items, keep, file) {
  winsteps_check_items(items)
  if (length(keep) == 0) {
    stop("keep is empty, which would delete every item from the estimation. ",
         "Pass the items to retain, or omit the item subset file entirely.",
         call. = FALSE)
  }
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

# Shared validation for the `items` vector that indexes the anchor file, the
# delete file and the data file by position.
#
# Sequence numbers are positional, so a duplicated name makes those three files
# disagree about which sequence number an item has.
winsteps_check_items <- function(items) {
  if (length(items) == 0) {
    stop("items is empty", call. = FALSE)
  }
  # Item names are written into tab-delimited files after a ";" comment
  # marker, so either character would silently restructure the line.
  structural <- grepl("[\t;]", items)
  if (any(structural)) {
    stop("item names must not contain tabs or semicolons, which delimit the ",
         "anchor and subset files; offending item(s): ",
         paste(utils::head(items[structural], 5), collapse = ", "),
         if (sum(structural) > 5) ", ..." else "", call. = FALSE)
  }
  dupes <- unique(items[duplicated(items)])
  if (length(dupes) > 0) {
    stop("items must be unique; duplicated: ",
         paste(utils::head(dupes, 5), collapse = ", "),
         if (length(dupes) > 5) ", ..." else "", call. = FALSE)
  }
  invisible(items)
}
