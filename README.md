# winstepsR

A small, organization-agnostic interface between R and [Winsteps](https://www.winsteps.com/index.htm)
(Rasch measurement software). It handles the mechanical part of the R–Winsteps
handoff: writing Winsteps' fixed-format person-data file, item anchor file,
and control file from data you already have in R; running Winsteps in batch
mode; and reading its person output file (PFILE) back into a tibble.

It deliberately does **not** implement any exam-, program-, or organization-
specific logic: scale-score conversions, penalty scoring rules, domain
definitions, item-bank formats, or database/file-share access all stay in the
calling project.

## Why this exists

This package is a generalized extraction of a one-off "daily score check"
pipeline originally built for a single testing program. That original code
hardcoded exam names, domain counts, file-share paths, and scoring formulas
directly into the Winsteps file-writing logic, which made it unusable outside
that one context. `winstepsR` factors out just the reusable part: the
Winsteps I/O layer.

## Installation

```r
# from the winstepsR/ directory:
devtools::install()
```

## Usage

```r
library(winstepsR)

# Long-format item response data: one row per person x item.
responses <- data.frame(
  person_id = c("00001", "00001", "00002", "00002"),
  item      = c("q1", "q2", "q1", "q2"),
  score     = c(1, 0, 1, 1)
)

# Item names paired with their anchor values (e.g. IRT b-parameters,
# already calibrated elsewhere). Sequence number is position, so keeping the
# two vectors in one object stops them drifting out of order, and validates
# them once here rather than at every writer.
anchors <- winsteps_anchors(
  items  = c("q1", "q2"),
  values = c(-0.4, 0.6)
)

# Point winstepsR at your local Winsteps.exe once per session/project:
options(winstepsR.exe_path = "C:/Winsteps/Winsteps.exe")

result <- winsteps_estimate(
  data = responses,
  id_col = "person_id", item_col = "item", score_col = "score",
  anchors = anchors,
  run_id = "example_run"
)

result          # a winsteps_result: prints a summary of the run
result$results  # tibble read from Winsteps' PFILE

# result$*_file paths point into working_dir (a temp dir by default, which
# the OS may clean up). result$contents holds the actual lines written to
# (and, for the person file, read from) each of those files, so you can
# inspect a run after the fact without depending on the temp files still
# being there:
result$contents$control  # character vector: the .ctr file as written
result$contents$data     # the Winsteps person-data file as written
result$contents$anchor   # the IAFILE as written
result$contents$person   # the raw PFILE lines Winsteps produced
result$contents$report   # raw lines of any TFILE=-requested tables (see below)
```

Both `winsteps_estimate()` and `winsteps_prepare_person_data()` return classed
objects that print a summary rather than their full contents, so inspecting one
at the console tells you what the run did instead of scrolling past every
response line:

```
<winsteps_result> exam1_domain2
  Directory /tmp/wtest/exam1_domain2
  Items     50 anchored, 12 estimated (38 excluded via IDFILE)
  Persons   1204
  Winsteps  run; 1204 person measures returned
  Files     data.dat, anchor.txt, delete.txt, control.ctr, run.bat, person.out, OUT.csv

  $contents  data, anchor, delete, control, bat, person, report
  $results   tibble 1204 x 11
```

### Requesting specific Winsteps tables (e.g. Table 17.1)

Pass `tfile` through `control_args` to request one or more numbered
Winsteps tables via `TFILE=`; their output is written to the batch report
file and read back as raw lines in `result$contents$report` (also
available at the path in `result$report_file`, though that path lives
under `working_dir` and isn't guaranteed to persist):

```r
result <- winsteps_estimate(
  data = responses, id_col = "person_id", item_col = "item", score_col = "score",
  anchors = anchors,
  control_args = list(tfile = "17.1"),
  run_id = "example_run"
)
result$contents$report  # raw lines of Table 17.1
```

Table layouts vary too much across Winsteps tables to parse generically, so
`winsteps_read_report()` (and `contents$report`) return the raw lines for you
to inspect or parse per-table. They come back as a `winsteps_report`, which is
a character vector — `grepl()`, `writeLines()`, `length()` and subsetting all
work as usual — that prints a summary of the tables it holds instead of
echoing several hundred lines:

```
<winsteps_report> 299 lines from 2 tables
  TABLE 17.1   line 1        140 lines
  TABLE 3.1    line 141      159 lines

  TABLE 17.1 PERSON MEASURE ORDER      ZOU870ws.txt Sep  8 09:14 2026
  ... 297 more lines
```

Use `as.character()` to drop the class.

### Estimation settings (PROX vs. JMLE, convergence criteria)

`control_args$estimation` is a named list merged *over* built-in defaults
(`MPROX=20`, `MJMLE=0`, `CONVERGE=L`, `LCONV=0.0001`, `UDECIM=4`) via
`modifyList()` — passing e.g. `list(RCONV = 0.5)` adds `RCONV` alongside
those defaults rather than replacing the whole list and silently dropping
`MPROX`/`MJMLE`. Set a built-in key to `NULL` to omit it entirely, e.g.
`list(MJMLE = NULL)`.

```r
winsteps_estimate(
  ..., control_args = list(
    estimation = list(CONVERGE = "L", LCONV = 0.0001, RCONV = 0.5)
  )
)
```

Two things worth double-checking if a recreated control file doesn't
behave like the original you're matching:

- **Keyword spelling matters and isn't validated.** Winsteps silently
  ignores unrecognized keywords rather than erroring, so a typo like
  `UDECIMALS` (the real keyword is `UDECIM`) has no effect and won't be
  reported as a mistake — the line is written verbatim but Winsteps just
  never sees it as meaningful.
- **Numeric values are written in plain decimal, not scientific
  notation.** `paste0("LCONV=", 0.0001)` in base R produces
  `"LCONV=1e-04;"`, which Winsteps' control-file parser does not treat as
  the intended value — this package explicitly formats numeric
  estimation values (and anchor values) to avoid that.
- Whether Winsteps' final `PFILE` `MEASURE` reflects PROX-only or
  JMLE-refined estimation is governed by Winsteps' own estimation
  keywords (`MPROX`/`MJMLE`/`XMLE`/etc.), not by anything this package
  adds on top — consult the Winsteps manual for what those specific
  keywords do; this package only guarantees the keywords you pass reach
  the control file intact.

To estimate on a subset of items only (e.g. one content domain out of many),
pass `keep_items`:

```r
winsteps_estimate(
  data = responses, id_col = "person_id", item_col = "item", score_col = "score",
  anchors = anchors,
  keep_items = c("q1"),          # every other item is excluded via IDFILE
  run_id = "example_domain1_run"
)
```

To generate the input files without invoking Winsteps (e.g. to inspect them,
or hand them off to a Windows machine that will run Winsteps separately),
pass `run = FALSE`.

## Lower-level functions

`winsteps_estimate()` is a convenience wrapper around these building blocks,
each of which can be used independently for more custom workflows:

- `winsteps_prepare_person_data()` / `winsteps_write_person_data()` — reshape
  long-format responses into Winsteps' fixed-format data file.
- `winsteps_anchors()` — pair item names with anchor values in one object, so
  the shared ordering the anchor, delete and data files all depend on is
  structural rather than something each call has to keep straight. Both
  writers below accept either it or the two bare vectors.
- `winsteps_write_anchor_file()` — write an IAFILE of anchored item values.
- `winsteps_write_item_subset_file()` — write an IDFILE excluding items not
  in a given "keep" set (used for domain/subset scoring).
- `winsteps_write_control_file()` — assemble the `.ctr` control file.
- `winsteps_write_bat()` / `winsteps_run()` — write and execute the batch
  runner. `winsteps_run()` only works on Windows, since that's the only
  platform Winsteps itself runs on.
- `winsteps_read_person_output()` — read a PFILE into a tibble, handling the
  empty-output case.
- `winsteps_read_item_output()` — read an IFILE into a tibble. Worth doing even
  on a pure anchored run: its displacement column is the evidence that the
  anchors were applied rather than quietly re-estimated.
- `winsteps_read_control_file()` — parse an existing `.ctr` back into the
  arguments `winsteps_write_control_file()` takes, so an established Winsteps
  setup can be brought into R rather than rebuilt by hand.
- `winsteps_read_report()` — read a `TFILE=`-requested table report as raw
  lines, handling the missing-file case. Returns a `winsteps_report`, which
  prints a per-table summary but is otherwise a plain character vector.

## Input validation

The package errors rather than writing a file Winsteps would accept and
misread. In particular it rejects response codes wider than one character
(Winsteps needs `XWIDE=` for those, which this package does not implement),
non-finite anchor values, duplicated item names, duplicated person-item rows,
zero-row input, and an empty `keep_items` set. Responses that cannot be read
as numbers warn rather than silently becoming the missing code.

When `run = TRUE`, a failed Winsteps run raises an error: a non-zero exit
status, or a run that reports success but writes no PFILE. A PFILE containing
only its header is *not* a failure — that is Winsteps' normal output for a
cohort with no estimable persons, and it yields a zero-row `results` tibble.

## Vignettes

- `vignette("winstepsR")` — getting started, end to end.
- `vignette("anatomy-of-a-run")` — the files Winsteps reads and writes, how the
  fixed-width layout works, and why `ITEM1`, `NAMLEN` and `NI` have to agree
  with the bytes on the line. Start here if the file formats are unfamiliar.
- `vignette("domain-scoring")` — scoring one content domain at a time while
  keeping every domain on the same scale.

All three build without Winsteps installed, using real Winsteps output shipped
in `inst/extdata`.

## Notes and known gaps

- **Windows only for execution.** File generation works anywhere R runs;
  actually invoking Winsteps (`winsteps_run()`) requires Windows.
- **No credentials, paths, or scoring formulas are baked in.** Configure the
  Winsteps executable path via `options(winstepsR.exe_path = ...)` (or pass
  `winsteps_exe` explicitly); everything else is a function argument.
- **Column naming from Winsteps output is left as-is** (`NAME`, `MEASURE`,
  `COUNT`, `SCORE`, ...); rename downstream in your own project, since what
  those columns should be called is project-specific.
- **Person IDs come back as text.** `winsteps_read_person_output()` forces the
  `NAME` column to character, so leading zeros survive and the column's type
  does not change between runs. Left to type guessing, `NAME` would come back
  as character for IDs like `00123` but numeric for IDs like `123` — a
  downstream join on that column would then work every day until the day a
  cohort's IDs all happened to lack leading zeros. That said, round-tripping
  IDs through Winsteps still costs you anything Winsteps' own fixed-width
  `NAMLEN` field cannot hold, so a separate ID lookup remains the safer choice
  for long or structured identifiers.
