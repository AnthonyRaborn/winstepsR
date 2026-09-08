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
  line <- paste(
    shQuote(winsteps_exe), "BATCH=YES",
    shQuote(control_file), shQuote(out_file),
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
#'
#' @return The integer status code from [shell()], invisibly.
#' @export
winsteps_run <- function(bat_file, wait = TRUE) {
  if (.Platform$OS.type != "windows") {
    stop(
      "winsteps_run() calls shell() and only works on Windows, where ",
      "Winsteps.exe itself runs. Generate the input files with this ",
      "package and run this step on a Windows machine.",
      call. = FALSE
    )
  }
  bat_file <- normalizePath(bat_file, mustWork = TRUE)
  run_dir <- dirname(bat_file)
  old_wd <- setwd(run_dir)
  on.exit(setwd(old_wd), add = TRUE)

  status <- shell(basename(bat_file), wait = wait, intern = FALSE)
  invisible(status)
}
