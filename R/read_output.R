#' Read a Winsteps person output file (PFILE) into a tibble
#'
#' Winsteps writes a one-line comment header followed by a whitespace-
#' delimited table (`NAME`, `MEASURE`, `COUNT`, `SCORE`, ...). Column
#' names are left exactly as Winsteps writes them; renaming to
#' study-specific names (e.g. `RegID`, `winsteps_estimate`) is left to the
#' caller, since those names are project-specific.
#'
#' If Winsteps found no eligible persons to estimate, the PFILE will
#' contain only the header line (or not exist at all, depending on how it
#' was pre-seeded). By default this is treated as "zero rows" rather than
#' an error, since that is a normal outcome, not a failure.
#'
#' `NAME` is always read as text. Left to type guessing it comes back as
#' character for IDs like `00123` but numeric for IDs like `123`, so the same
#' code would return a different type for the same column depending only on
#' which persons happened to be in the run -- and any downstream join on that
#' column would break on the day the types flipped.
#'
#' @param file Path to the PFILE.
#' @param empty_ok If `TRUE` (default), a missing file or a file with no
#'   data rows returns a zero-row tibble instead of raising an error.
#' @param col_types Optional [readr::cols()] specification passed to
#'   [readr::read_table()]. If `NULL` (default), types are guessed except for
#'   `NAME`, which is forced to character.
#'
#' @return A tibble of person output. Zero rows if `empty_ok` and the file
#'   has no data. A PFILE that still carries its column-name line returns the
#'   full set of columns with zero rows; the column-less tibble is returned
#'   only when there is no column-name line to read (a missing file, or one
#'   holding nothing but its leading comment).
#' @export
winsteps_read_person_output <- function(file, empty_ok = TRUE, col_types = NULL) {
  if (!file.exists(file)) {
    if (empty_ok) return(tibble::tibble())
    stop("Winsteps output file not found: ", file, call. = FALSE)
  }
  # Only the first two lines are needed to decide whether there is a table and
  # to learn its column names; the file itself may be large.
  head_lines <- readLines(file, n = 2L, warn = FALSE)
  if (length(head_lines) <= 1) {
    if (empty_ok) return(tibble::tibble())
    stop("Winsteps output file has no data rows: ", file, call. = FALSE)
  }
  if (is.null(col_types)) {
    col_names <- strsplit(trimws(head_lines[2]), "[[:space:]]+")[[1]]
    if ("NAME" %in% col_names) {
      col_types <- readr::cols(NAME = readr::col_character())
    }
  }
  readr::read_table(file, skip = 1, show_col_types = FALSE, col_types = col_types)
}

#' Read a Winsteps batch table/report file
#'
#' Winsteps' `TFILE=` requests one or more numbered tables (e.g. `"17.1"`,
#' `"3.1"`); in `BATCH=YES` mode those tables are written to the report
#' file passed as Winsteps' third command-line argument -- the `out_file`
#' given to [winsteps_write_bat()] (and, via [winsteps_estimate()],
#' returned there as `report_file`). Table layouts vary too much across
#' Winsteps tables to usefully parse into a data frame generically, so
#' this just returns the raw lines for the caller to inspect or parse
#' themselves.
#'
#' @param file Path to the report file.
#' @param empty_ok If `TRUE` (default), a missing file returns
#'   `character(0)` instead of raising an error.
#'
#' @return Character vector of raw lines from the report file.
#' @export
winsteps_read_report <- function(file, empty_ok = TRUE) {
  if (!file.exists(file)) {
    if (empty_ok) return(character(0))
    stop("Winsteps report file not found: ", file, call. = FALSE)
  }
  readLines(file, warn = FALSE)
}
