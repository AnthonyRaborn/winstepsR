#' Summarise the person measures from a Winsteps run
#'
#' Reports the distribution of the estimated person measures and nothing more.
#' There are deliberately no fit flags, no "extreme" or "misfitting" counts and
#' no cut-offs of any kind: every such label needs a threshold, and choosing
#' thresholds is exam- and program-specific work that belongs in the calling
#' project rather than in this package.
#'
#' Everything reported here is computed from `object$results`, which remains
#' available in full -- all of Winsteps' columns, one row per person -- so any
#' analysis this summary does not cover can be done directly on that tibble.
#'
#' Counts of zero and perfect scores are included because they follow from
#' `SCORE` and `COUNT` by arithmetic rather than from a chosen threshold.
#'
#' @param object A [winsteps_estimate()] result.
#' @param ... Ignored.
#'
#' @return An object of class `summary.winsteps_result`: a list with `run_id`,
#'   `n`, the run's `run_at` / `elapsed` provenance, the `measure` and `se`
#'   distributions, and `n_zero` / `n_perfect`,
#'   suitable for programmatic use as well as printing. Fields that the PFILE
#'   did not carry are `NULL`.
#' @export
#'
#' @examples
#' # summary() reports the distribution; the full table stays in $results
#' pfile <- system.file("extdata", "pfile_example.out", package = "winstepsR")
#' measures <- winsteps_read_person_output(pfile)
#' measures$MEASURE
summary.winsteps_result <- function(object, ...) {
  res <- object$results
  out <- list(run_id = object$run_id, ran = !is.null(res), n = NROW(res),
              run_at = object$run_at, elapsed = object$elapsed,
              winsteps_elapsed = object$winsteps_elapsed)

  if (!is.null(res) && "MEASURE" %in% names(res)) {
    m <- res$MEASURE[is.finite(res$MEASURE)]
    if (length(m) > 0) {
      q <- stats::quantile(m, c(0.25, 0.5, 0.75), names = FALSE)
      out$measure <- c(mean = mean(m), sd = stats::sd(m), min = min(m),
                       q1 = q[1], median = q[2], q3 = q[3], max = max(m))
    }
  }
  if (!is.null(res) && "MODLSE" %in% names(res)) {
    se <- res$MODLSE[is.finite(res$MODLSE)]
    if (length(se) > 0) out$se <- c(mean = mean(se), min = min(se), max = max(se))
  }
  # Arithmetic, not a threshold: a person whose score equals 0 or equals the
  # number of items they answered has no interior measure to estimate.
  if (!is.null(res) && all(c("SCORE", "COUNT") %in% names(res))) {
    out$n_zero <- sum(res$SCORE == 0, na.rm = TRUE)
    out$n_perfect <- sum(res$SCORE == res$COUNT, na.rm = TRUE)
  }
  structure(out, class = "summary.winsteps_result")
}

#' @export
print.summary.winsteps_result <- function(x, ...) {
  cat("<winsteps_result summary> ", x$run_id, "\n", sep = "")
  field <- function(label, value) {
    cat("  ", formatC(label, width = -16), value, "\n", sep = "")
  }
  if (!is.null(x$run_at)) {
    field("Run at", paste0(format(x$run_at, "%Y-%m-%d %H:%M:%S"), "  (",
                           format_secs(x$elapsed), ")"))
  }
  num <- function(v, digits = 2) formatC(v, format = "f", digits = digits)

  if (!isTRUE(x$ran)) {
    field("Winsteps", "not run - no measures to summarise")
    return(invisible(x))
  }
  field("Persons", paste(x$n, "measured"))

  if (is.null(x$measure)) {
    field("Measure", "no MEASURE column in the person output")
  } else {
    m <- x$measure
    field("Measure", paste0("mean ", num(m[["mean"]]), ", SD ", num(m[["sd"]]),
                            " logits"))
    field("", paste0("min ", num(m[["min"]]), ", Q1 ", num(m[["q1"]]),
                     ", median ", num(m[["median"]]), ", Q3 ", num(m[["q3"]]),
                     ", max ", num(m[["max"]])))
  }
  if (!is.null(x$se)) {
    field("Model SE", paste0("mean ", num(x$se[["mean"]]), ", range ",
                             num(x$se[["min"]]), " to ", num(x$se[["max"]])))
  }
  if (!is.null(x$n_zero)) {
    field("Extreme scores", paste0(x$n_zero, " scored zero, ", x$n_perfect,
                                   " scored every item"))
  }
  cat("\n  Full person table in $results; this summary adds no cut-offs.\n")
  invisible(x)
}

#' Summarise the tables in a Winsteps batch report
#'
#' @param object A [winsteps_read_report()] result.
#' @param ... Ignored.
#'
#' @return A data frame with one row per table found, giving its `table`
#'   number, the `start` line and how many `lines` it runs to. Zero rows if the
#'   report holds no `TABLE` headings.
#' @export
summary.winsteps_report <- function(object, ...) {
  tables <- winsteps_report_tables(object)
  if (is.null(tables)) {
    return(data.frame(table = character(0), start = integer(0),
                      lines = integer(0), stringsAsFactors = FALSE))
  }
  tables
}
