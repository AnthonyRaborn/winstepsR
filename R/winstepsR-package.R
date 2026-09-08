#' winstepsR: an R interface to Winsteps
#'
#' Writes the files Winsteps needs (person data, item anchors, control file,
#' batch runner), runs Winsteps in batch mode on Windows, and reads the person
#' output file (PFILE) back into R. It implements no exam- or
#' organization-specific scoring logic; scale-score conversions, penalty
#' rules and domain definitions belong in the calling project.
#'
#' Set the Winsteps executable path once per session with
#' `options(winstepsR.exe_path = "C:/Winsteps/Winsteps.exe")`, or pass
#' `winsteps_exe` at each call site.
#'
#' [winsteps_estimate()] is the end-to-end wrapper; the
#' `winsteps_write_*()`, [winsteps_run()] and `winsteps_read_*()` functions
#' are the building blocks it composes, and can be used directly.
#'
#' @keywords internal
"_PACKAGE"
