# winstepsR 0.1.0.9000 (development)

Fixes from a full review of 0.1.0. The theme is that the package used to write
files Winsteps would accept and silently misread; most changes turn those
cases into errors at the point the bad input arrives.

## Correctness

* `winsteps_write_bat()` now quotes paths for `cmd.exe` regardless of the
  platform writing the file. A `.bat` generated on macOS or Linux was
  single-quoted and could not run on the Windows machine it was handed to --
  the workflow `run = FALSE` exists to support.
* `winsteps_run()` gains `error_on_failure` (default `TRUE`) and
  `winsteps_estimate()` errors when a run reports success but writes no PFILE.
  A failed Winsteps run previously returned a zero-row tibble, indistinguishable
  from a cohort with no eligible persons. A header-only PFILE is still the
  legitimate empty case.
* `winsteps_prepare_person_data()` rejects responses wider than one character.
  A partial-credit style score such as `10` was pasted in whole and shifted
  every response column after it while `NI`/`ITEM1` still described the narrow
  layout.
* `winsteps_read_person_output()` reads `NAME` as character. Type guessing
  returned character for IDs like `00123` but numeric for IDs like `123`, so
  the column's type depended on which persons were in the run. `col_types` is
  now available for callers who want full control.
* `winsteps_write_control_file()` gains `delimiter_width` so the `namlen`
  default is right for delimiters longer than one character, and
  `winsteps_estimate()` passes the prepared ID width explicitly.

## Validation

* Non-finite anchor values, zero-row response data, an empty `keep` set,
  duplicated item names, duplicated person-item rows, non-positive layout
  positions, `run_id` values containing path separators, and `control_args`
  entries that collide with derived arguments are all now errors.
* Responses that cannot be read as numbers warn rather than silently becoming
  the missing code.
* `IAFILE`/`IDFILE`/`PFILE` paths containing whitespace are quoted; paths
  without whitespace are written exactly as before.

## Correctness (third pass, after verifying behaviour against Winsteps)

* `winsteps_write_bat()` writes `cd /d "%~dp0"` as the batch file's first
  line, so it changes to its own directory and can be run from anywhere.
  `winsteps_run()` therefore no longer calls `setwd()`, which was
  process-global and made concurrent runs -- one per exam, one per domain, the
  case the `run_id` design exists for -- clobber one another's working
  directory. A hand-written `.bat` without that leading `cd` will now resolve
  its control file relative to the current directory instead.
* Parsing is now tested against a real Winsteps PFILE, installed at
  `inst/extdata/pfile_example.out`, rather than a reconstruction of the format.
  No code changes were needed: Winsteps' leading comment line, its
  `;`-commented column-name line, values written without a leading zero
  (`.72`), the trailing blank line and the full 21-column layout were all
  already handled.
* `winsteps_read_person_output()` strips the leading `;` from the first column
  name. Winsteps comments out its own column-name line, so the column
  previously arrived as `` `;ENTRY` `` and needed backticks to reach.

## Correctness (second pass)

* Anchor values and estimation keywords are written with an explicit
  `decimal.mark`, so `options(OutDec = ",")` can no longer turn `0.5` into
  `0,5` or `LCONV=0.0001` into `LCONV=0,0001`. This is the same hazard the
  existing `scientific = FALSE` guard covers, from a different setting.
* Person-ID and delimiter widths are measured in bytes rather than characters,
  since Winsteps counts file columns in bytes. A multi-byte ID previously
  shifted that person's responses one column left of the `ITEM1` the package
  reported.
* `winsteps_prepare_person_data()` documented its `item_order` default as
  sorting the items; it actually uses first-appearance order, so reordering the
  input rows silently reordered the response columns. Documented accurately,
  with a stronger recommendation to pass `item_order` explicitly.

## Item output

* New `winsteps_read_item_output()` reads a Winsteps `IFILE=` into a tibble,
  sharing the PFILE's layout and its handling of empty output. Item names are
  read as text for the same reason person IDs are.
* `winsteps_write_control_file()` gains `ifile`, and `winsteps_estimate()`
  requests item output on every run: `result$items` holds the tibble,
  `result$item_file` the path and `result$contents$item` the raw lines.

  This closes the one place where nothing could verify what Winsteps did with
  the anchors. The displacement column reports the gap between the anchor value
  supplied and the value the data implies, so it is the evidence that anchors
  were applied rather than quietly re-estimated -- and a run whose anchor file
  was ignored succeeds and returns plausible measures. As with the PFILE, a
  missing IFILE after a successful run is now an error rather than an empty
  result. What counts as excessive displacement is left to the caller.

## Methods

* `summary()` on a `winsteps_result` reports the distribution of the person
  measures: mean, SD and quartiles of `MEASURE`, the range of `MODLSE`, and
  counts of zero and perfect scores. Deliberately no fit flags and no cut-offs
  of any kind -- those need thresholds, which are exam-specific and belong in
  the calling project. The full person table remains at `$results`, so anything
  the summary omits can be computed directly from it. The returned object is a
  list, so the figures are usable programmatically as well as printable.
* `summary()` on a `winsteps_report` returns its table index as a data frame:
  one row per table, with the table number, its starting line and its length.

## Structure

* New `winsteps_anchors()` pairs item names with their anchor values in one
  object. Sequence number is position, so the anchor file, delete file and
  data file all depend on those two vectors staying in the same order -- an
  invariant previously stated in five doc blocks and enforced nowhere.
  Duplicated names, delimiter characters in names, mismatched lengths and
  non-finite values are now caught once, when the bank is built, rather than
  at each writer. `winsteps_write_anchor_file()` and
  `winsteps_write_item_subset_file()` are generic and take either the object
  or the bare vectors; `winsteps_estimate()` gains an `anchors` argument
  alongside `items`/`anchor_values`, and validates `keep_items` before it
  writes anything.
* Validation moved into a shared internal vocabulary (`R/validate.R`), so each
  exported function opens with a short list of what it refuses instead of
  inline error construction. `winsteps_prepare_person_data()` is about half its
  previous length as a result.
* `winsteps_prepare_person_data()` returns a classed `winsteps_person_data`
  object that prints the fixed-width layout -- which columns hold the ID, the
  delimiter and the responses -- instead of dumping every person's response
  line at the console.
* `winsteps_estimate()` returns a classed `winsteps_result` object carrying
  `run_id` and `run_dir` alongside the paths it already returned. It prints a
  summary of the run -- items anchored and estimated, persons, whether Winsteps
  ran, files on disk -- instead of dumping every line of every file it read
  back.
* `winsteps_read_report()` returns a `winsteps_report`, a character vector
  that prints a summary of the tables it holds -- their numbers, where each
  starts and how many lines it runs to -- rather than echoing several hundred
  lines of table output. It inherits from `character`, so `grepl()`,
  `writeLines()`, `length()` and subsetting are unaffected; `as.character()`
  drops the class. Code comparing the return value against a bare character
  vector with `identical()` or `expect_equal()` needs that call.
* `winsteps_estimate()` delegates its file paths and its read-back step to
  internal helpers, so its body reads as prepare, write, run, read.

## Documentation

* `?winstepsR` package documentation, runnable examples on
  `winsteps_estimate()` and `winsteps_prepare_person_data()`.
* The claim that every call gets its own directory is corrected: paths are
  deterministic, so a reused `run_id` overwrites the earlier run.
