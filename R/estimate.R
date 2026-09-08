#' Estimate person measures with Winsteps, end to end
#'
#' A convenience wrapper around the individual `winsteps_write_*()` /
#' [winsteps_run()] / [winsteps_read_person_output()] functions for the
#' common case: given long-format response data and a fixed set of
#' anchored item difficulties, estimate person ability (optionally on a
#' subset of items, e.g. one content domain) and return the result.
#'
#' Each call uses its own subdirectory of `working_dir` so that repeated
#' calls (e.g. once per exam, once per domain) don't overwrite each
#' other's intermediate files.
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
#' @param run_id A short, filesystem-safe label for this run, used to
#'   namespace files within `working_dir` (e.g. `"exam1"`,
#'   `"exam1_domain2"`). Defaults to `"winsteps_run"`.
#' @param working_dir Directory under which a `run_id` subdirectory is
#'   created for this run's files. Defaults to a temp directory.
#' @param control_args Named list of extra arguments passed through to
#'   [winsteps_write_control_file()] (e.g. `estimation`, `codes`,
#'   `extra`). Use `control_args$tfile` (e.g. `tfile = "17.1"`) to request
#'   specific Winsteps tables; their output is written to `report_file`
#'   and read back into `contents$report` (see below).
#' @param winsteps_exe Passed through to [winsteps_write_bat()].
#' @param run Whether to actually invoke Winsteps ([winsteps_run()]). Set
#'   to `FALSE` to only generate input files (e.g. to inspect them, or to
#'   run Winsteps manually / on a different machine).
#'
#' @return A list with the paths of every file written (`data_file`,
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
  run_dir <- file.path(working_dir, run_id)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

  prepared <- winsteps_prepare_person_data(
    data = data, id_col = id_col, item_col = item_col, score_col = score_col,
    item_order = items
  )

  data_file <- file.path(run_dir, "data.dat")
  anchor_file <- file.path(run_dir, "anchor.txt")
  control_file <- file.path(run_dir, "control.ctr")
  bat_file <- file.path(run_dir, "run.bat")
  person_file <- file.path(run_dir, "person.out")
  report_file <- file.path(run_dir, "OUT.csv")

  winsteps_write_person_data(prepared, data_file)
  winsteps_write_anchor_file(items, anchor_values, anchor_file)

  delete_file <- NULL
  if (!is.null(keep_items)) {
    delete_file <- file.path(run_dir, "delete.txt")
    winsteps_write_item_subset_file(items, keep = keep_items, file = delete_file)
  }

  control_call_args <- c(
    list(
      file = control_file,
      data_file = basename(data_file),
      n_items = prepared$n_items,
      item1 = prepared$item1,
      iafile = basename(anchor_file),
      idfile = if (!is.null(delete_file)) basename(delete_file) else NULL,
      pfile = basename(person_file)
    ),
    control_args
  )
  do.call(winsteps_write_control_file, control_call_args)

  winsteps_write_bat(
    file = bat_file,
    control_file = basename(control_file),
    out_file = basename(report_file),
    winsteps_exe = winsteps_exe
  )

  result <- list(
    data_file = data_file,
    anchor_file = anchor_file,
    delete_file = delete_file,
    control_file = control_file,
    bat_file = bat_file,
    person_file = person_file,
    report_file = report_file
  )

  result$contents <- list(
    data = readLines(data_file, warn = FALSE),
    anchor = readLines(anchor_file, warn = FALSE),
    delete = if (!is.null(delete_file)) readLines(delete_file, warn = FALSE) else NULL,
    control = readLines(control_file, warn = FALSE),
    bat = readLines(bat_file, warn = FALSE)
  )

  if (run) {
    winsteps_run(bat_file)
    result$results <- winsteps_read_person_output(person_file)
    result$contents$person <-
      if (file.exists(person_file)) readLines(person_file, warn = FALSE) else NULL
    result$contents$report <- winsteps_read_report(report_file)
  }

  result
}
