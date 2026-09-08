# Internal validation vocabulary.
#
# These guards exist because the contract with Winsteps is a fixed-width text
# file with no schema on the far side: almost every bad input produces a file
# Winsteps accepts and silently misreads rather than one it rejects. Keeping
# them here lets each exported function open with a short list of what it
# refuses, and get to the work it actually does.

# "A, B, C, D, E, ..." -- the first `n` values, with an ellipsis if there are
# more. Used by every guard that reports offending values.
format_examples <- function(x, n = 5L, quote = FALSE) {
  x <- as.character(x)
  shown <- utils::head(x, n)
  if (quote) shown <- dQuote(shown, FALSE)
  paste0(paste(shown, collapse = ", "), if (length(x) > n) ", ..." else "")
}

# Character positions and counts written into the control file. Winsteps
# accepts nonsense here (NI=-5, ITEM1=0) and then misreads the data file.
check_positive_int <- function(x, what) {
  if (length(x) != 1 || is.na(x) || !is.numeric(x) || x < 1 || x != as.integer(x)) {
    stop(what, " must be a single positive whole number, not: ",
         paste(format(x), collapse = ", "), call. = FALSE)
  }
  invisible(x)
}

# The item vector indexes the anchor file, the delete file and the data file by
# position, so a duplicate name makes those three disagree about which sequence
# number an item has. Tabs and semicolons are the anchor file's own delimiter
# and comment marker.
check_items <- function(items) {
  if (length(items) == 0) {
    stop("items is empty", call. = FALSE)
  }
  structural <- grepl("[\t;]", items)
  if (any(structural)) {
    stop("item names must not contain tabs or semicolons, which delimit the ",
         "anchor and subset files; offending item(s): ",
         format_examples(items[structural]), call. = FALSE)
  }
  dupes <- unique(items[duplicated(items)])
  if (length(dupes) > 0) {
    stop("items must be unique; duplicated: ", format_examples(dupes),
         call. = FALSE)
  }
  invisible(items)
}

# A non-finite anchor would be written literally (e.g. "NA"), which Winsteps
# cannot read as a logit; it would then estimate the item freely rather than
# anchoring it, so the run silently stops being the anchored run requested.
check_finite_anchors <- function(values, items) {
  bad <- !is.finite(values)
  if (any(bad)) {
    stop("anchor values must all be finite; ", sum(bad),
         " are not, at item(s): ", format_examples(items[bad]), call. = FALSE)
  }
  invisible(values)
}

check_columns_present <- function(data, cols) {
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }
  invisible(data)
}

# An empty cohort cannot produce a valid layout: id_width would be -Inf and
# paste0() would still emit one delimiter-only line for a person who does not
# exist. Whether an empty day is normal is the caller's call, not ours.
check_has_rows <- function(data) {
  if (nrow(data) == 0) {
    stop("data has no rows, so no Winsteps person data can be written. ",
         "Handle the empty case before calling.", call. = FALSE)
  }
  invisible(data)
}

# pivot_wider() fails on duplicates from deep inside vctrs with a message that
# names neither the person nor the item.
check_one_response_per_pair <- function(long) {
  dup <- duplicated(long[, c("id", "item")])
  if (any(dup)) {
    d <- unique(long[dup, c("id", "item")])
    stop("data has more than one response for the same person and item: ",
         format_examples(paste0(d$id, "/", d$item)),
         ". De-duplicate before calling.", call. = FALSE)
  }
  invisible(long)
}

# The response block is built one character per item and ITEM1/NI are computed
# on that assumption, so a wider code would silently shift every column after
# it rather than failing.
xwide_note <- "Winsteps reads one character per item unless XWIDE= is set, which this function does not support"

# Widths are measured in bytes because Winsteps counts file columns in bytes,
# not characters.
check_single_char <- function(x, what) {
  if (length(x) != 1 || is.na(x) || nchar(x, type = "bytes") != 1L) {
    stop(what, " must be exactly one character, and one byte, not \"", x, "\". ",
         xwide_note, ".", call. = FALSE)
  }
  invisible(x)
}

check_single_char_codes <- function(codes) {
  wide <- unique(codes[nchar(codes, type = "bytes") != 1L])
  if (length(wide) > 0) {
    stop("Response codes must be exactly one character each; found ",
         length(wide), " that are not: ", format_examples(wide, quote = TRUE),
         ". ", xwide_note, "; recode these responses (e.g. map 10 to \"A\") ",
         "before calling.", call. = FALSE)
  }
  invisible(codes)
}

# run_id names a subdirectory, so a separator would place the run's files
# outside working_dir entirely.
check_run_id <- function(run_id) {
  if (length(run_id) != 1 || is.na(run_id) || !nzchar(run_id) ||
      grepl("[/\\\\]", run_id) || run_id %in% c(".", "..")) {
    stop("run_id must be a single non-empty name with no path separators, ",
         "since it names a subdirectory of working_dir; got: ",
         paste(format(run_id), collapse = ", "), call. = FALSE)
  }
  invisible(run_id)
}

# Arguments winsteps_estimate() derives itself would otherwise reach do.call()
# twice and fail with "matched by multiple actual arguments".
check_no_reserved_args <- function(control_args, reserved) {
  clash <- intersect(names(control_args), reserved)
  if (length(clash) > 0) {
    stop("control_args cannot set argument(s) that winsteps_estimate() ",
         "derives itself: ", paste(clash, collapse = ", "),
         ". Call winsteps_write_control_file() directly if you need control ",
         "over these.", call. = FALSE)
  }
  invisible(control_args)
}
