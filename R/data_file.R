#' Pivot long-format item response data into Winsteps person-data rows
#'
#' Winsteps expects one fixed-format line per person: a left-justified,
#' fixed-width person ID, followed by a delimiter, followed by one
#' character per item in a known, stable order. This function performs that
#' reshape without assuming anything about item names, ID format, or exam
#' identity.
#'
#' Every response must render to exactly one character, since `ITEM1` and
#' `NI` are computed on that basis; a wider code (e.g. a partial-credit
#' score of `10`) would silently shift every column after it, so it is
#' rejected instead. Winsteps' mechanism for wider codes is `XWIDE=`, which
#' this function does not implement -- recode to single characters first.
#'
#' @param data A data frame in long format with one row per person-item
#'   response. Must have at least one row, and at most one row per
#'   person-item pair.
#' @param id_col Name of the person identifier column in `data`.
#' @param item_col Name of the item identifier column in `data`.
#' @param score_col Name of the (numeric-coercible) score column in `data`.
#' @param id_width Fixed width to pad/left-justify the person ID to. If
#'   `NULL` (default), the width of the longest ID actually present in the
#'   filtered data is used.
#' @param delimiter Single-character (or short string) separator written
#'   between the ID field and the first item response. Defaults to `"*"`.
#' @param missing_code Single-character code written for a person-item
#'   combination with no response. Defaults to `"."`.
#' @param item_order Optional character vector giving the exact, ordered
#'   set of items to include (and their column order). If omitted, items
#'   are ordered as they naturally sort after pivoting. Supplying this
#'   explicitly is recommended so that the anchor file and data file stay
#'   in sync.
#'
#' @return A list with:
#'   \describe{
#'     \item{lines}{Character vector, one fixed-format line per person.}
#'     \item{items}{Character vector of item names, in file column order.}
#'     \item{n_items}{Number of items (`NI` in Winsteps terms).}
#'     \item{id_width}{Width used for the ID field (`NAMLEN`).}
#'     \item{item1}{Column position of the first item response (`ITEM1`).}
#'     \item{delimiter}{The delimiter used.}
#'   }
#' @export
winsteps_prepare_person_data <- function(data,
                                          id_col,
                                          item_col,
                                          score_col,
                                          id_width = NULL,
                                          delimiter = "*",
                                          missing_code = ".",
                                          item_order = NULL) {
  stopifnot(is.data.frame(data))
  missing_cols <- setdiff(c(id_col, item_col, score_col), names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (nchar(delimiter) < 1) stop("delimiter must be at least one character", call. = FALSE)

  if (nrow(data) == 0) {
    # An empty cohort cannot produce a valid layout: id_width would be -Inf and
    # paste0() would still emit one delimiter-only line for a person who does
    # not exist. Whether an empty day is normal is the caller's call, not ours.
    stop("data has no rows, so no Winsteps person data can be written. ",
         "Handle the empty case before calling.", call. = FALSE)
  }

  long <- data[, c(id_col, item_col, score_col)]
  names(long) <- c("id", "item", "score")
  long$id <- as.character(long$id)
  long$score <- as.character(suppressWarnings(as.numeric(long$score)))
  long$score[is.na(long$score)] <- missing_code

  # The response block is built one character per item and ITEM1/NI are
  # computed on that assumption, so a wider code would silently shift every
  # column after it rather than failing. Winsteps' own mechanism for wider
  # codes is XWIDE=, which this function does not implement.
  if (nchar(missing_code) != 1L) {
    stop("missing_code must be exactly one character, not \"", missing_code,
         "\". Winsteps reads one character per item unless XWIDE= is set, ",
         "which this function does not support.", call. = FALSE)
  }
  wide_codes <- unique(long$score[nchar(long$score) != 1L])
  if (length(wide_codes) > 0) {
    stop("Response codes must be exactly one character each; found ",
         length(wide_codes), " that are not: ",
         paste(dQuote(utils::head(wide_codes, 5), FALSE), collapse = ", "),
         if (length(wide_codes) > 5) ", ..." else "",
         ". Winsteps reads one character per item unless XWIDE= is set, which ",
         "this function does not support; recode these responses (e.g. map ",
         "10 to \"A\") before calling.", call. = FALSE)
  }

  # pivot_wider() fails on duplicates from deep inside vctrs with a message that
  # names neither the person nor the item, so catch them here instead.
  dup_key <- duplicated(long[, c("id", "item")])
  if (any(dup_key)) {
    d <- unique(long[dup_key, c("id", "item")])
    stop("data has more than one response for the same person and item: ",
         paste(utils::head(paste0(d$id, "/", d$item), 5), collapse = ", "),
         if (nrow(d) > 5) ", ..." else "",
         ". De-duplicate before calling.", call. = FALSE)
  }

  wide <- tidyr::pivot_wider(
    long,
    id_cols = "id",
    names_from = "item",
    values_from = "score",
    values_fill = missing_code
  )

  if (is.null(item_order)) {
    item_order <- setdiff(names(wide), "id")
  } else {
    winsteps_check_items(item_order)
    missing_items <- setdiff(item_order, setdiff(names(wide), "id"))
    if (length(missing_items) > 0) {
      stop("item_order contains items not present in data: ",
           paste(missing_items, collapse = ", "), call. = FALSE)
    }
  }
  wide <- wide[, c("id", item_order), drop = FALSE]

  if (is.null(id_width)) {
    id_width <- max(nchar(wide$id))
  }
  if (any(nchar(wide$id) > id_width)) {
    stop("id_width (", id_width, ") is smaller than at least one person ID", call. = FALSE)
  }
  ids <- formatC(wide$id, width = -id_width, flag = "-")

  response_block <- do.call(paste0, wide[item_order])
  lines <- paste0(ids, delimiter, response_block)

  list(
    lines = lines,
    items = item_order,
    n_items = length(item_order),
    id_width = id_width,
    item1 = id_width + nchar(delimiter) + 1L,
    delimiter = delimiter
  )
}

#' Write prepared Winsteps person-data lines to a file
#'
#' @param prepared Output of [winsteps_prepare_person_data()].
#' @param file Path to write the Winsteps `DATA=` file to.
#' @export
winsteps_write_person_data <- function(prepared, file) {
  writeLines(prepared$lines, con = file)
  invisible(file)
}
