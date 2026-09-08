#' Read a Winsteps person output file (PFILE) into a tibble
#'
#' Winsteps writes a one-line comment header followed by a whitespace-
#' delimited table (`ENTRY`, `MEASURE`, `COUNT`, `SCORE`, ..., `NAME`).
#' Column names are left as Winsteps writes them, except that the leading
#' `;` Winsteps uses to comment out its own column-name line is stripped from
#' the first name -- it arrives as `;ENTRY` otherwise. Renaming to
#' study-specific names (e.g. `RegID`) is left to the caller, since those
#' names are project-specific.
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
    col_names <- strsplit(trimws(sub("^;", "", head_lines[2])), "[[:space:]]+")[[1]]
    if ("NAME" %in% col_names) {
      col_types <- readr::cols(NAME = readr::col_character())
    }
  }
  out <- readr::read_table(file, skip = 1, show_col_types = FALSE,
                           col_types = col_types)
  # Winsteps marks its column-name line as a comment, so the leading ";" runs
  # into the first name and the column arrives as `;ENTRY` -- awkward to reach
  # without backticks, and not part of the name Winsteps means.
  names(out) <- sub("^;", "", names(out))
  out
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
#' @param empty_ok If `TRUE` (default), a missing file returns an empty
#'   report instead of raising an error.
#'
#' @return A `winsteps_report`: the raw lines of the report file, as a
#'   character vector with a class attached so that printing it summarises
#'   the tables it contains rather than echoing every line. It inherits from
#'   `character`, so it can be used anywhere a character vector can --
#'   `grepl()`, `writeLines()`, `length()` and subsetting all behave as
#'   before. Use [as.character()] to drop the class.
#' @export
winsteps_read_report <- function(file, empty_ok = TRUE) {
  if (!file.exists(file)) {
    if (empty_ok) return(as_winsteps_report(character(0)))
    stop("Winsteps report file not found: ", file, call. = FALSE)
  }
  as_winsteps_report(readLines(file, warn = FALSE))
}

as_winsteps_report <- function(lines) {
  structure(as.character(lines), class = c("winsteps_report", "character"))
}

# Winsteps heads each requested table with a line naming it, e.g.
# "TABLE 17.1 PERSON MEASURE ORDER ...". Layouts below that line vary too much
# between tables to parse generically, but the headings themselves are enough
# to say what a report holds.
winsteps_report_tables <- function(x) {
  at <- grep("^[[:space:]]*TABLE[[:space:]]+[0-9]", x)
  if (length(at) == 0) return(NULL)
  number <- sub("^[[:space:]]*TABLE[[:space:]]+([0-9.]+).*$", "\\1", x[at])
  data.frame(
    table = number,
    start = at,
    lines = diff(c(at, length(x) + 1L)),
    stringsAsFactors = FALSE
  )
}

#' @export
print.winsteps_report <- function(x, n = 2, ...) {
  tables <- winsteps_report_tables(x)
  cat("<winsteps_report> ", length(x), " lines",
      if (!is.null(tables)) paste0(" from ", nrow(tables), " table",
                                   if (nrow(tables) > 1) "s" else ""),
      "\n", sep = "")

  if (length(x) == 0) {
    cat("  no report file, or nothing written to it\n")
    return(invisible(x))
  }
  if (is.null(tables)) {
    cat("  no TABLE headings found\n")
  } else {
    for (i in seq_len(nrow(tables))) {
      cat("  TABLE ", formatC(tables$table[i], width = -6),
          " line ", formatC(tables$start[i], width = -6),
          formatC(tables$lines[i], width = 6), " lines\n", sep = "")
    }
  }
  shown <- utils::head(as.character(x), n)
  cat("\n", paste0("  ", substr(shown, 1, 68), collapse = "\n"), "\n", sep = "")
  if (length(x) > n) cat("  ... ", length(x) - n, " more lines\n", sep = "")
  invisible(x)
}
