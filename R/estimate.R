#' Estimate person measures with Winsteps, end to end
#'
#' A convenience wrapper around the individual `winsteps_write_*()` /
#' [winsteps_run()] / [winsteps_read_person_output()] functions for the
#' common case: given long-format response data and a fixed set of
#' anchored item difficulties, estimate person ability (optionally on a
#' subset of items, e.g. one content domain) and return the result.
#'
#' Files for a call are written to the `run_id` subdirectory of
#' `working_dir`, so calls with distinct `run_id`s (e.g. one per exam, one per
#' domain) don't overwrite each other's intermediate files. Paths are
#' deterministic rather than unique: two calls sharing a `run_id` and
#' `working_dir` write to the same directory, and the second overwrites the
#' first.
#'
#' @param data Long-format response data; see [winsteps_prepare_person_data()].
#' @param id_col,item_col,score_col Column names in `data`; see
#'   [winsteps_prepare_person_data()].
#' @param items Character vector of all item names, in a fixed order
#'   shared with `anchor_values` (and with `keep_items`, if used).
#' @param anchor_values Numeric vector of anchor values, same order as
#'   `items`.
#' @param keep_items Optional character vector of items to estimate on
#'   (e.g. one domain's items); every other item in `items` is excluded
#'   via an `IDFILE`. If `NULL` (default), all items are used.
#' @param run_id A short label for this run, used as a subdirectory name
#'   within `working_dir` (e.g. `"exam1"`, `"exam1_domain2"`). Must contain no
#'   path separators. Give concurrent or repeated runs distinct values --
#'   reusing one overwrites the earlier run's files. Defaults to
#'   `"winsteps_run"`.
#' @param working_dir Directory under which a `run_id` subdirectory is
#'   created for this run's files. Defaults to a temp directory.
#' @param control_args Named list of extra arguments passed through to
#'   [winsteps_write_control_file()] (e.g. `estimation`, `codes`,
#'   `extra`). Cannot include arguments this function derives itself
#'   (`file`, `data_file`, `n_items`, `item1`, `namlen`, `iafile`, `idfile`,
#'   `pfile`). Use `control_args$tfile` (e.g. `tfile = "17.1"`) to request
#'   specific Winsteps tables; their output is written to `report_file`
#'   and read back into `contents$report` (see below).
#' @param winsteps_exe Passed through to [winsteps_write_bat()].
#' @param run Whether to actually invoke Winsteps ([winsteps_run()]). Set
#'   to `FALSE` to only generate input files (e.g. to inspect them, or to
#'   run Winsteps manually / on a different machine).
#'
#' @details When `run = TRUE`, a failed Winsteps run raises an error rather
#'   than returning an empty result: a non-zero exit status errors via
#'   [winsteps_run()], and a run that reports success but writes no PFILE
#'   errors here. A PFILE containing only its header is *not* a failure --
#'   that is Winsteps' normal output for a cohort with no estimable persons,
#'   and it yields a zero-row `results` tibble.
#'
#' @examples
#' responses <- data.frame(
#'   person_id = rep(c("00001", "00002"), each = 2),
#'   item      = rep(c("q1", "q2"), 2),
#'   score     = c(1, 0, 1, 1)
#' )
#'
#' # Generate the Winsteps input files without running Winsteps, which is
#' # possible on any platform.
#' result <- winsteps_estimate(
#'   data = responses,
#'   id_col = "person_id", item_col = "item", score_col = "score",
#'   items = c("q1", "q2"), anchor_values = c(-0.4, 0.6),
#'   run_id = "example_run", winsteps_exe = "Winsteps.exe", run = FALSE
#' )
#' result$contents$control
#'
#' @return An object of class `winsteps_result`, which prints as a summary of
#'   the run rather than dumping every file it read back. Underneath it is a
#'   list carrying `run_id` and `run_dir`; the paths of every file written
#'   (`data_file`,
#'   `anchor_file`, `delete_file`, `control_file`, `bat_file`,
#'   `person_file`, `report_file`); a parallel `contents` list holding the
#'   actual lines written to each of those files (`contents$data`,
#'   `contents$anchor`, `contents$delete`, `contents$control`,
#'   `contents$bat`, and, if `run = TRUE`, `contents$person` -- the raw
#'   PFILE lines Winsteps wrote -- and `contents$report` -- the raw lines
#'   of any `TFILE=`-requested tables, e.g. Table 17.1, written to
#'   `report_file`); and, if `run = TRUE`, `results`: the tibble from
#'   [winsteps_read_person_output()]. `working_dir` defaults to a temp
#'   directory that the OS is free to clear, so `contents` lets callers
#'   inspect exactly what was written/read without depending on those
#'   paths still being valid later.
#' @export
winsteps_estimate <- function(data,
                               id_col,
                               item_col,
                               score_col,
                               items,
                               anchor_values,
                               keep_items = NULL,
                               run_id = "winsteps_run",
                               working_dir = tempdir(),
                               control_args = list(),
                               winsteps_exe = getOption("winstepsR.exe_path"),
                               run = TRUE) {
  check_run_id(run_id)
  check_no_reserved_args(
    control_args,
    c("file", "data_file", "n_items", "item1", "namlen", "iafile", "idfile", "pfile")
  )

  run_dir <- file.path(working_dir, run_id)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

  prepared <- winsteps_prepare_person_data(
    data = data, id_col = id_col, item_col = item_col, score_col = score_col,
    item_order = items
  )

  paths <- winsteps_run_paths(run_dir, subset = !is.null(keep_items))

  winsteps_write_person_data(prepared, paths$data_file)
  winsteps_write_anchor_file(items, anchor_values, paths$anchor_file)
  if (!is.null(paths$delete_file)) {
    winsteps_write_item_subset_file(items, keep = keep_items, file = paths$delete_file)
  }

  do.call(winsteps_write_control_file, c(
    list(
      file = paths$control_file,
      data_file = basename(paths$data_file),
      n_items = prepared$n_items,
      item1 = prepared$item1,
      # Passed explicitly rather than left to the control file's derived
      # default: the person name field is exactly the ID field, whatever
      # delimiter width was used.
      namlen = prepared$id_width,
      iafile = basename(paths$anchor_file),
      idfile = if (!is.null(paths$delete_file)) basename(paths$delete_file) else NULL,
      pfile = basename(paths$person_file)
    ),
    control_args
  ))

  winsteps_write_bat(
    file = paths$bat_file,
    control_file = basename(paths$control_file),
    out_file = basename(paths$report_file),
    winsteps_exe = winsteps_exe
  )

  result <- c(list(run_id = run_id, run_dir = run_dir), paths)
  result$contents <- winsteps_read_back(paths)

  if (run) {
    # Errors on a non-zero exit status, so a crashed run cannot be mistaken
    # downstream for a cohort with no eligible persons.
    winsteps_run(paths$bat_file)

    # A run that succeeded but wrote no PFILE at all is a failure, not an empty
    # cohort: Winsteps writes at least a header line when it estimates nobody.
    # A header-only PFILE is still the legitimate empty case and stays a
    # zero-row result, so only the missing-file case is treated as an error.
    if (!file.exists(paths$person_file)) {
      stop(
        "Winsteps reported success but wrote no person output file: ",
        paths$person_file,
        ". Check the control file (PFILE=) and the run directory: ", run_dir,
        call. = FALSE
      )
    }
    result$results <- winsteps_read_person_output(paths$person_file)
    result$contents$person <- readLines(paths$person_file, warn = FALSE)
    result$contents$report <- winsteps_read_report(paths$report_file)
  }

  structure(result, class = "winsteps_result")
}

#' @export
print.winsteps_result <- function(x, ...) {
  cat("<winsteps_result> ", x$run_id, "\n", sep = "")

  field <- function(label, value) {
    cat("  ", formatC(label, width = -10), value, "\n", sep = "")
  }
  field("Directory", x$run_dir)
  field("Items", paste0(
    length(x$contents$anchor), " anchored",
    if (!is.null(x$contents$delete)) {
      paste0(", ", length(x$contents$anchor) - length(x$contents$delete),
             " estimated (", length(x$contents$delete), " excluded via IDFILE)")
    } else ""
  ))
  field("Persons", length(x$contents$data))

  if (is.null(x$results)) {
    field("Winsteps", "not run - input files generated only")
  } else {
    field("Winsteps", paste0("run; ", nrow(x$results), " person measures returned"))
  }

  # unlist() drops the NULL delete_file when no item subset was used; the
  # existence check then drops the output files Winsteps has not written yet.
  candidates <- unlist(x[c("data_file", "anchor_file", "delete_file",
                           "control_file", "bat_file", "person_file",
                           "report_file")])
  field("Files", paste(basename(candidates[file.exists(candidates)]),
                       collapse = ", "))

  held <- names(x$contents)[!vapply(x$contents, is.null, logical(1))]
  cat("\n  $contents  ", paste(held, collapse = ", "), "\n", sep = "")
  if (is.null(x$results)) {
    cat("  $results   NULL (run = FALSE)\n")
  } else {
    cat("  $results   tibble ", nrow(x$results), " x ", ncol(x$results), "\n", sep = "")
  }
  invisible(x)
}

# The fixed set of files a run reads and writes, all inside its own directory.
#
# Named separately from winsteps_estimate() so the wrapper's body reads as a
# sequence of steps rather than a block of path arithmetic, and so the file
# names live in exactly one place.
winsteps_run_paths <- function(run_dir, subset = FALSE) {
  list(
    data_file    = file.path(run_dir, "data.dat"),
    anchor_file  = file.path(run_dir, "anchor.txt"),
    delete_file  = if (subset) file.path(run_dir, "delete.txt") else NULL,
    control_file = file.path(run_dir, "control.ctr"),
    bat_file     = file.path(run_dir, "run.bat"),
    person_file  = file.path(run_dir, "person.out"),
    report_file  = file.path(run_dir, "OUT.csv")
  )
}

# Read back what was just written.
#
# Deliberately read from disk rather than kept from memory: working_dir
# defaults to a temp directory the OS may clear, and reading back confirms what
# actually landed on disk for a format with no validation on the far side.
winsteps_read_back <- function(paths) {
  read_or_null <- function(f) if (!is.null(f) && file.exists(f)) readLines(f, warn = FALSE) else NULL
  list(
    data    = read_or_null(paths$data_file),
    anchor  = read_or_null(paths$anchor_file),
    delete  = read_or_null(paths$delete_file),
    control = read_or_null(paths$control_file),
    bat     = read_or_null(paths$bat_file)
  )
}
