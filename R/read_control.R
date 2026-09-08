#' Read a Winsteps control file into the arguments that wrote it
#'
#' Parses an existing `.ctr` file into the arguments
#' [winsteps_write_control_file()] takes, so an established Winsteps setup can
#' be brought into R rather than rebuilt by hand:
#'
#' ```
#' args <- winsteps_read_control_file("existing.ctr")
#' args$estimation$LCONV <- 0.001
#' do.call(winsteps_write_control_file, c(list(file = "new.ctr"), args))
#' ```
#'
#' Values keep their case and their leading zeros. Both matter: lowercasing a
#' value corrupts file paths on a case-sensitive filesystem and any `TITLE=`
#' text, and coercing `CODES=01` to a number turns two valid response codes
#' into one.
#'
#' @section What is recognised:
#'
#' Keywords with a dedicated argument (`DATA`, `NI`, `ITEM1`, `NAME1`,
#' `NAMLEN`, `CODES`, `IAFILE`, `IDFILE`, `PFILE`, `IFILE`, `TFILE`) are
#' returned under that argument's name. Item labels between `&END` and
#' `END LABELS` become `item_labels`. Every other `KEYWORD=value` line becomes
#' an entry in `estimation`, which is where [winsteps_write_control_file()]
#' will write it back. Comments -- everything after a `;` -- are discarded,
#' along with anything after `END LABELS`.
#'
#' `ITEM=` is dropped, because [winsteps_write_control_file()] always writes
#' `ITEM=Item;` itself and returning it as well would emit the keyword twice.
#' A value other than `Item` warns rather than disappearing silently.
#'
#' @param file Path to the control file.
#'
#' @return An object of class `winsteps_control`: a named list of arguments for
#'   [winsteps_write_control_file()], usable directly with [do.call()].
#' @export
#'
#' @examples
#' ctr <- system.file("extdata", "example_full_control.ctr", package = "winstepsR")
#' args <- winsteps_read_control_file(ctr)
#' args
#' args$n_items
#' args$estimation$LCONV
winsteps_read_control_file <- function(file) {
  if (!file.exists(file)) {
    stop("Winsteps control file not found: ", file, call. = FALSE)
  }
  raw <- readLines(file, warn = FALSE)

  # Split the keyword section from the item labels that follow &END.
  end_at <- which(toupper(trimws(raw)) == "&END")
  item_labels <- NULL
  if (length(end_at) > 0) {
    end_at <- end_at[1]
    after <- raw[-seq_len(end_at)]
    stop_at <- which(toupper(trimws(after)) == "END LABELS")
    if (length(stop_at) > 0) after <- after[seq_len(stop_at[1] - 1L)]
    after <- after[nzchar(trimws(after))]
    if (length(after) > 0) item_labels <- after
    raw <- raw[seq_len(end_at - 1L)]
  }

  entries <- parse_control_entries(raw)
  control_entries_to_args(entries, item_labels)
}

# Walk the keyword section, returning a named list of values. Block keywords
# (KEY=* ... *) come back as character vectors.
parse_control_entries <- function(lines) {
  entries <- list()
  i <- 1L
  while (i <= length(lines)) {
    # Winsteps treats everything after ";" as a comment.
    code <- trimws(sub(";.*$", "", lines[i]))
    if (!nzchar(code) || !grepl("=", code, fixed = TRUE)) {
      i <- i + 1L
      next
    }
    key <- toupper(trimws(sub("=.*$", "", code)))
    value <- trimws(sub("^[^=]*=", "", code))

    if (value == "*") {
      block <- character(0)
      j <- i + 1L
      repeat {
        if (j > length(lines)) {
          stop("Unterminated ", key, "=* block: no closing \"*\" before the ",
               "end of the file.", call. = FALSE)
        }
        inner <- trimws(sub(";.*$", "", lines[j]))
        if (inner == "*") break
        if (nzchar(inner)) block <- c(block, inner)
        j <- j + 1L
      }
      if (!identical(key, "TFILE")) {
        stop(key, "=* is an inline block, which this reader does not ",
             "understand; only TFILE=* is supported. Convert it to a file ",
             "reference (", key, "=<path>) or edit the control file by hand.",
             call. = FALSE)
      }
      entries[[key]] <- block
      i <- j + 1L
      next
    }
    entries[[key]] <- unquote_value(value)
    i <- i + 1L
  }
  entries
}

unquote_value <- function(x) {
  if (grepl('^".*"$', x)) substr(x, 2L, nchar(x) - 1L) else x
}

# Map parsed keywords onto winsteps_write_control_file()'s arguments.
control_entries_to_args <- function(entries, item_labels) {
  named <- c(DATA = "data_file", NI = "n_items", ITEM1 = "item1",
             NAME1 = "name1", NAMLEN = "namlen", CODES = "codes",
             IAFILE = "iafile", IDFILE = "idfile", PFILE = "pfile",
             IFILE = "ifile", TFILE = "tfile")
  # These are positions and counts; the writer validates them as whole numbers.
  numeric_args <- c("n_items", "item1", "name1", "namlen")

  args <- list()
  for (key in intersect(names(named), names(entries))) {
    arg <- named[[key]]
    value <- entries[[key]]
    if (arg %in% numeric_args) {
      num <- suppressWarnings(as.numeric(value))
      if (is.na(num)) {
        stop(key, "= is not a number: \"", value, "\"", call. = FALSE)
      }
      value <- num
    }
    args[[arg]] <- value
  }

  # ITEM= is written unconditionally by winsteps_write_control_file(), so
  # returning it here would emit the keyword twice.
  # [[ ]] rather than $: $ partial-matches on lists, so entries$ITEM would
  # return the value of ITEM1 in any control file that sets it.
  item_kw <- entries[["ITEM"]]
  if (!is.null(item_kw) && !identical(item_kw, "Item")) {
    warning("ITEM=", item_kw, " was not kept: ",
            "winsteps_write_control_file() always writes ITEM=Item. Add it ",
            "via `extra` if you need it.", call. = FALSE)
  }

  estimation <- entries[setdiff(names(entries), c(names(named), "ITEM"))]
  # CODES and file paths stay text; estimation values are numeric where they
  # look numeric, so they round-trip through the writer's formatting.
  estimation <- lapply(estimation, function(v) {
    num <- suppressWarnings(as.numeric(v))
    if (length(v) == 1 && !is.na(num)) num else v
  })
  if (length(estimation) > 0) args$estimation <- estimation
  if (!is.null(item_labels)) args$item_labels <- item_labels

  structure(args, class = "winsteps_control")
}

#' @export
print.winsteps_control <- function(x, ...) {
  cat("<winsteps_control> arguments for winsteps_write_control_file()\n")
  field <- function(label, value) {
    cat("  ", formatC(label, width = -13), value, "\n", sep = "")
  }
  layout <- c("data_file", "n_items", "item1", "name1", "namlen", "codes")
  for (nm in intersect(layout, names(x))) field(nm, format(x[[nm]]))

  files <- intersect(c("iafile", "idfile", "pfile", "ifile"), names(x))
  if (length(files) > 0) {
    field("files", paste0(files, "=", unlist(x[files]), collapse = ", "))
  }
  if (!is.null(x$tfile)) field("tfile", paste(x$tfile, collapse = ", "))
  if (!is.null(x$item_labels)) {
    field("item_labels", paste0(length(x$item_labels), ": ",
                                format_examples(x$item_labels, n = 4)))
  }
  if (!is.null(x$estimation)) {
    # Shown the way the writer will write it: plain decimal, never 1e-04.
    shown <- vapply(x$estimation, function(v) {
      paste(format(v, scientific = FALSE, trim = TRUE), collapse = "/")
    }, character(1))
    field("estimation", paste0(names(x$estimation), "=", shown, collapse = ", "))
  }
  invisible(x)
}
