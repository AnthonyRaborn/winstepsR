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
#' @param file Path to the PFILE.
#' @param empty_ok If `TRUE` (default), a missing file or a file with no
#'   data rows returns a zero-row tibble instead of raising an error.
#'
#' @return A tibble of person output. Zero rows if `empty_ok` and the file
#'   has no data.
#' @export
winsteps_read_person_output <- function(file, empty_ok = TRUE) {
  if (!file.exists(file)) {
    if (empty_ok) return(tibble::tibble())
    stop("Winsteps output file not found: ", file, call. = FALSE)
  }
  n_lines <- length(readLines(file, warn = FALSE))
  if (n_lines <= 1) {
    if (empty_ok) return(tibble::tibble())
    stop("Winsteps output file has no data rows: ", file, call. = FALSE)
  }
  readr::read_table(file, skip = 1, show_col_types = FALSE)
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
