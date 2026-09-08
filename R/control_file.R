#' Write a Winsteps control file
#'
#' Assembles a Winsteps `.ctr` control file from parameters. No exam- or
#' organization-specific defaults are baked in beyond Winsteps' own
#' reasonable estimation defaults, which can be overridden via
#' `estimation` and further extended via `extra`.
#'
#' @param file Path to write the control file to.
#' @param data_file Value for `DATA=` -- the person-response data file
#'   written by [winsteps_write_person_data()], as Winsteps will see it
#'   (typically just the file name, since the control file and data file
#'   are usually run from the same working directory).
#' @param n_items Number of items (`NI=`). Use `n_items` from
#'   [winsteps_prepare_person_data()].
#' @param item1 Column position of the first item response (`ITEM1=`).
#'   Use `item1` from [winsteps_prepare_person_data()].
#' @param name1 Column position of the first character of the person ID
#'   (`NAME1=`). Defaults to `1`.
#' @param delimiter_width Width of the delimiter separating the person ID
#'   from the first item response, used only to derive the `namlen` default.
#'   Defaults to `1`; pass `nchar(delimiter)` from
#'   [winsteps_prepare_person_data()] if you used a longer one.
#' @param namlen Width of the person ID field (`NAMLEN=`). Defaults to
#'   `item1 - name1 - delimiter_width`, i.e. everything between `NAME1` and
#'   `ITEM1` except the delimiter itself. Passing `id_width` from
#'   [winsteps_prepare_person_data()] directly is more direct and always
#'   correct.
#' @param codes String of valid single-character response codes
#'   (`CODES=`). Defaults to `"01"`.
#' @param iafile Optional path for `IAFILE=` (item anchor file).
#' @param idfile Optional path for `IDFILE=` (item subset/delete file).
#' @param pfile Path for `PFILE=`, the person output file Winsteps will
#'   write. Defaults to `"person.out"`.
#' @param item_labels Optional character vector of item labels to list
#'   after `&END`. If supplied, must have length `n_items` and be in
#'   data-file column order.
#' @param estimation Named list of estimation/convergence control lines,
#'   written as `NAME=value;`. Merged over (not replacing) a set of
#'   built-in defaults -- 20 PROX iterations, unlimited JMLE iterations,
#'   converge on logit change, 4 decimal places -- via
#'   [utils::modifyList()], so passing e.g. `list(RCONV = 0.5)` adds
#'   `RCONV` alongside the defaults rather than dropping `MPROX`/`MJMLE`/
#'   etc. Set a built-in key to `NULL` to remove it (e.g.
#'   `list(MJMLE = NULL)` to omit `MJMLE=` from the control file
#'   entirely). Double check keyword spelling against the Winsteps manual:
#'   an unrecognized keyword is silently ignored by Winsteps rather than
#'   erroring, so e.g. `UDECIMALS` (not a real keyword; the correct one is
#'   `UDECIM`) has no effect and won't be reported as a mistake.
#' @param tfile Optional character vector of Winsteps table numbers to
#'   request via `TFILE=*` ... `*;`.
#' @param extra Optional character vector of additional raw control-file
#'   lines to insert before `&END`, for any Winsteps keyword not
#'   otherwise covered here.
#'
#' @return `file`, invisibly.
#' @export
winsteps_write_control_file <- function(file,
                                         data_file,
                                         n_items,
                                         item1,
                                         name1 = 1,
                                         delimiter_width = 1,
                                         namlen = item1 - name1 - delimiter_width,
                                         codes = "01",
                                         iafile = NULL,
                                         idfile = NULL,
                                         pfile = "person.out",
                                         item_labels = NULL,
                                         estimation = list(),
                                         tfile = NULL,
                                         extra = character(0)) {
  # These three index character positions and item counts in the data file;
  # anything non-positive is a programmer error that Winsteps would accept
  # without complaint and then misread.
  check_position <- function(x, nm) {
    if (length(x) != 1 || is.na(x) || !is.numeric(x) || x < 1 || x != as.integer(x)) {
      stop(nm, " must be a single positive whole number, not: ",
           paste(format(x), collapse = ", "), call. = FALSE)
    }
  }
  check_position(n_items, "n_items")
  check_position(item1, "item1")
  check_position(name1, "name1")
  check_position(delimiter_width, "delimiter_width")
  check_position(namlen, "namlen")

  if (!is.null(item_labels) && length(item_labels) != n_items) {
    stop("item_labels must have length n_items (", n_items, ")", call. = FALSE)
  }

  # Winsteps splits an unquoted value at whitespace, so a path containing a
  # space needs quoting; paths without one are left exactly as before.
  quote_if_needed <- function(x) {
    if (grepl("[[:space:]]", x) && !grepl('^".*"$', x)) paste0('"', x, '"') else x
  }

  fmt <- function(x) {
    if (is.numeric(x)) format(x, scientific = FALSE, trim = TRUE) else as.character(x)
  }

  default_estimation <- list(
    MPROX = 20,
    MJMLE = 0,
    CONVERGE = "L",
    LCONV = 0.0001,
    UDECIM = 4
  )
  estimation <- utils::modifyList(default_estimation, estimation)

  # DATA= is always quoted (that form is known to work); the rest are quoted
  # only when the path actually needs it, so control files that work today are
  # written byte-for-byte as they were.
  lines <- character(0)
  if (!is.null(iafile)) lines <- c(lines, paste0("IAFILE=", quote_if_needed(iafile), ";"))
  if (!is.null(idfile)) lines <- c(lines, paste0("IDFILE=", quote_if_needed(idfile), ";"))

  estimation <- estimation[!vapply(estimation, is.null, logical(1))]
  if (length(estimation) > 0) {
    est_lines <- vapply(
      names(estimation),
      function(k) paste0(k, "=", fmt(estimation[[k]]), ";"),
      character(1)
    )
    lines <- c(lines, est_lines)
  }

  if (!is.null(tfile)) {
    lines <- c(lines, "TFILE=*", tfile, "*;")
  }

  lines <- c(
    lines,
    paste0("CODES=", codes, ";"),
    paste0('DATA="', data_file, '";'),
    paste0("NAME1=", name1, ";"),
    paste0("NAMLEN=", namlen, ";"),
    "ITEM=Item;",
    paste0("ITEM1=", item1, ";"),
    paste0("NI=", n_items, ";"),
    paste0("PFILE=", quote_if_needed(pfile), ";"),
    extra,
    "&END"
  )

  if (!is.null(item_labels)) {
    lines <- c(lines, item_labels, "END LABELS")
  }

  writeLines(lines, con = file)
  invisible(file)
}
