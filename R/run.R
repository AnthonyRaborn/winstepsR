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
#'   The batch file begins by changing to its own directory, since Winsteps
#'   writes its output relative to wherever it was launched. That makes the
#'   file self-locating: it can be run from any working directory, by
#'   [winsteps_run()] or by double-clicking it, and still finds its control
#'   file and leaves its output alongside itself.
#'
#' @return `file`, invisibly.
#' @examples
#' f <- tempfile(fileext = ".bat")
#' winsteps_write_bat(
#'   f, control_file = "control.ctr", out_file = "OUT.csv",
#'   winsteps_exe = "C:/Winsteps/Winsteps.exe"
#' )
#'
#' # the leading cd makes the file self-locating, and paths are quoted for
#' # cmd.exe whatever platform generated them
#' writeLines(readLines(f))
#'
#' unlink(f)
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
  lines <- c(
    # Winsteps writes its output relative to the directory it is launched
    # from, so the batch file moves there itself rather than making the caller
    # do it. %~dp0 is the drive and path of this .bat; /d allows a drive
    # change. This is what lets winsteps_run() avoid setwd(), which is
    # process-global and would make concurrent runs clobber one another.
    'cd /d "%~dp0"',
    paste(
      shQuote(winsteps_exe, type = "cmd"), "BATCH=YES",
      shQuote(control_file, type = "cmd"), shQuote(out_file, type = "cmd"),
      extra_args
    )
  )
  writeLines(lines, con = file)
  invisible(file)
}

#' Run a Winsteps batch file
#'
#' Runs a `.bat` file written by [winsteps_write_bat()]. That file changes to
#' its own directory on the first line, so this function does not touch the R
#' session's working directory and several runs can proceed concurrently
#' without interfering with one another. Winsteps itself only runs on Windows.
#'
#' A hand-written `.bat` without that leading `cd` will resolve its control
#' file and write its output relative to whatever the current directory
#' happens to be.
#'
#' @param bat_file Path to the `.bat` file to run.
#' @param wait Whether to block until Winsteps exits. Defaults to `TRUE`.
#' @param error_on_failure If `TRUE` (default), a non-zero exit status from
#'   Winsteps raises an error rather than being returned silently. Only
#'   meaningful when `wait = TRUE`, since a status is not available otherwise.
#'
#' @return The integer status code, invisibly.
#' @examples
#' \dontrun{
#' # Winsteps runs on Windows only, so this cannot be executed here.
#' options(winstepsR.exe_path = "C:/Winsteps/Winsteps.exe")
#'
#' bat <- file.path(tempdir(), "run.bat")
#' winsteps_write_bat(bat, "control.ctr", "OUT.csv")
#' winsteps_run(bat)
#' }
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

  # Called via do.call() rather than directly: shell() exists only in base R
  # on Windows, so a direct call is an undefined global everywhere else and
  # static analysis flags it. The platform guard above means this line is only
  # ever reached where shell() does exist.
  status <- do.call(
    "shell",
    list(shQuote(bat_file, type = "cmd"), wait = wait, intern = FALSE)
  )
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
