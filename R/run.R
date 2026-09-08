#' Write a Winsteps batch (.bat) runner file
#'
#' @param file Path to write the `.bat` file to.
#' @param control_file Control file name/path, as Winsteps will see it
#'   (typically relative to `file`'s directory, since Winsteps is run from
#'   that directory).
#' @param out_file Name of the batch report file Winsteps writes on exit
#'   (its `BATCH=YES` third argument), e.g. `"OUT.csv"`.
#' @param winsteps_exe Path to `Winsteps.exe`. Defaults to
#'   `getOption("winstepsR.exe_path")`, so a project can set this once via
#'   `options(winstepsR.exe_path = "C:/Winsteps/Winsteps.exe")` instead of
#'   hardcoding it at every call site.
#' @param extra_args Additional batch arguments appended verbatim, e.g.
#'   `"HLINES=YES"`.
#'
#' @details Paths are quoted for `cmd.exe` regardless of the platform this
#'   function runs on, so a `.bat` generated on macOS or Linux and handed to a
#'   Windows machine (see `run = FALSE` in [winsteps_estimate()]) is valid
#'   there.
#'
#' @return `file`, invisibly.
#' @export
winsteps_write_bat <- function(file,
                                control_file,
                                out_file,
                                winsteps_exe = getOption("winstepsR.exe_path"),
                                extra_args = "HLINES=YES") {
  if (is.null(winsteps_exe)) {
    stop(
      "winsteps_exe not supplied and options(\"winstepsR.exe_path\") is unset. ",
      "Pass winsteps_exe explicitly or set options(winstepsR.exe_path = \"<path to Winsteps.exe>\").",
      call. = FALSE
    )
  }
  # type = "cmd" is deliberate and must not be left to the default: shQuote()
  # otherwise picks its quoting style from the platform *writing* the file, so a
  # .bat generated on macOS/Linux (the documented `run = FALSE` handoff) would be
  # single-quoted and cmd.exe would read the quotes as part of the path.
  line <- paste(
    shQuote(winsteps_exe, type = "cmd"), "BATCH=YES",
    shQuote(control_file, type = "cmd"), shQuote(out_file, type = "cmd"),
    extra_args
  )
  writeLines(line, con = file)
  invisible(file)
}

#' Run a Winsteps batch file
#'
#' Runs a `.bat` file written by [winsteps_write_bat()] from its own
#' directory (Winsteps writes its output files relative to the working
#' directory it is launched from), then restores the previous working
#' directory. Winsteps itself only runs on Windows.
#'
#' @param bat_file Path to the `.bat` file to run.
#' @param wait Whether to block until Winsteps exits. Defaults to `TRUE`.
#' @param error_on_failure If `TRUE` (default), a non-zero exit status from
#'   Winsteps raises an error rather than being returned silently. Only
#'   meaningful when `wait = TRUE`, since a status is not available otherwise.
#'
#' @return The integer status code, invisibly.
#' @export
winsteps_run <- function(bat_file, wait = TRUE, error_on_failure = TRUE) {
  if (.Platform$OS.type != "windows") {
    stop(
      "winsteps_run() calls shell() and only works on Windows, where ",
      "Winsteps.exe itself runs. Generate the input files with this ",
      "package and run this step on a Windows machine.",
      call. = FALSE
    )
  }
  bat_file <- normalizePath(bat_file, mustWork = TRUE)
  old_wd <- setwd(dirname(bat_file))
  on.exit(setwd(old_wd), add = TRUE)

  # Called via do.call() rather than directly: shell() exists only in base R
  # on Windows, so a direct call is an undefined global everywhere else and
  # static analysis flags it. The platform guard above means this line is only
  # ever reached where shell() does exist.
  status <- do.call("shell", list(basename(bat_file), wait = wait, intern = FALSE))
  if (isTRUE(error_on_failure) && isTRUE(wait)) {
    winsteps_stop_on_status(status, bat_file)
  }
  invisible(status)
}

# Raise an informative error for a non-zero Winsteps exit status.
#
# Split out from winsteps_run() so the failure path is testable off Windows,
# where shell() does not exist.
winsteps_stop_on_status <- function(status, bat_file) {
  if (length(status) != 1 || is.na(status) || !is.numeric(status) || status == 0) {
    return(invisible(status))
  }
  stop(
    "Winsteps exited with status ", status, " (", basename(bat_file), "). ",
    "No person output should be trusted from this run. Check the control ",
    "file and the Winsteps executable path, and inspect the run directory: ",
    dirname(bat_file),
    call. = FALSE
  )
}
