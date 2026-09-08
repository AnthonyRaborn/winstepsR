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
#' @examples
#' responses <- data.frame(
#'   person_id = rep(c("00001", "00002"), each = 2),
#'   item      = rep(c("q1", "q2"), 2),
#'   score     = c(1, 0, 1, 1)
#' )
#' prepared <- winsteps_prepare_person_data(
#'   responses, "person_id", "item", "score", item_order = c("q1", "q2")
#' )
#' prepared$lines
#' prepared$item1
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
  check_columns_present(data, c(id_col, item_col, score_col))
  check_has_rows(data)
  if (nchar(delimiter) < 1) stop("delimiter must be at least one character", call. = FALSE)
  check_single_char(missing_code, "missing_code")

  long <- data[, c(id_col, item_col, score_col)]
  names(long) <- c("id", "item", "score")
  long$id <- as.character(long$id)
  long$score <- coerce_scores(long$score, missing_code)

  check_single_char_codes(long$score)
  check_one_response_per_pair(long)

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
    check_items(item_order)
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
  if (!is.list(prepared) || !is.character(prepared$lines)) {
    stop("prepared must be the list returned by winsteps_prepare_person_data(), ",
         "with a character `lines` element", call. = FALSE)
  }
  writeLines(prepared$lines, con = file)
  invisible(file)
}

# Coerce a response column to the single-character text Winsteps expects.
#
# Values that were already missing become the missing code silently; values
# that merely failed to parse are almost always an upstream problem (a
# "correct"/"incorrect" column, a stray "N/A") that would otherwise become a
# run scoring nobody, so those warn.
coerce_scores <- function(score, missing_code) {
  parsed <- as.character(suppressWarnings(as.numeric(score)))
  unreadable <- is.na(parsed) & !is.na(score)
  if (any(unreadable)) {
    warning(sum(unreadable), " response(s) could not be read as numbers and ",
            "were written as the missing code \"", missing_code, "\": ",
            format_examples(unique(score[unreadable]), quote = TRUE),
            call. = FALSE)
  }
  parsed[is.na(parsed)] <- missing_code
  parsed
}
