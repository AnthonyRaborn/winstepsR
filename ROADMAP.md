# winstepsR roadmap

Working notes on what is next and why. Not a changelog — see `NEWS.md` for what
has landed.

CRAN submission is **not** the goal. The package follows CRAN guidelines as a
quality bar, and anywhere a guideline costs more than it returns for a
single-maintainer package, that is noted rather than silently followed.

---

## 0. Blocked on a machine with Winsteps

Questions that cannot be answered without a Winsteps install; the full protocol
with runnable code is in section 10 of the code review.

### Settled

**W1 -- confirmed, 8 Sep 2026.** A single-quoted `.bat`, which is what
`shQuote()` produced on a macOS or Linux host before the fix, is rejected by
`cmd.exe`:

```
'C:' is not recognized as an internal or external command,
operable program or batch file.
```

The double-quoted form runs. C1 was therefore a live bug rather than a latent
one: every `.bat` generated off Windows for the documented `run = FALSE`
handoff was unusable on arrival. Fixed in 7572027 and now verified against a
real `cmd.exe`.

**W4 -- partly settled, 8 Sep 2026.** LF-only line endings make no difference
to the **control file**: Winsteps produced identical output from `control.ctr`
and an LF-converted copy. The `.bat` and `.dat` were not tested, and the `.bat`
is the more exacting case since `cmd.exe` is fussier than Winsteps' own control
parser. No CRLF work is warranted unless that turns up something.

### Open


| | Question | What it changes |
|---|---|---|
| W2 | How does Winsteps read a two-character score? | Decides whether `XWIDE=` deserves real support or stays a guard |
| W3 | Does a two-character delimiter shift the person name? | Confirms whether the `NAMLEN` fix ever mattered |
| W4b | Do LF endings matter to the `.bat` and `.dat`? | Remainder of W4 |
| W5 | Can the `.bat` change to its own directory? | Would let `winsteps_run()` drop `setwd()` and become safe to parallelise |

**Note for W2 and W3:** both probes as originally written now fail inside the
package rather than reaching Winsteps -- W2 because the C2 guard rejects the
two-character score it depends on, and W3 because the protocol reused W2's data
frame, which carries that same score even though the delimiter question has
nothing to do with it. Corrected probes that build the malformed input directly
are needed; see the review notes.

### W5 in detail

`winsteps_run()` changes the process-global working directory, because Winsteps
writes its output relative to wherever it was launched. The `run_id`
subdirectory design exists so that many runs can coexist -- one per exam, one
per domain -- and the obvious way to speed that up is to run them in parallel.
Two in-process workers would clobber each other's working directory, and the
`on.exit(setwd(old_wd))` restore does not help, because the calls interleave.

If a batch file can change to its own directory, R never needs to:

```bat
cd /d "%~dp0"
"C:/Winsteps/Winsteps.exe" BATCH=YES "control.ctr" "OUT.csv" HLINES=YES
```

Write that two-line `.bat` next to a working control file, run it from a
*different* directory, and check whether Winsteps still finds `control.ctr` and
writes its output beside the batch file rather than into the directory you
launched from. If it does, `winsteps_run()` loses its `setwd()` entirely and
the package becomes parallel-safe.


### Collect fixtures at the same time

Cheap to grab while at the Windows machine, awkward to arrange later. Run a
small cohort with `control_args = list(tfile = c("17.1", "3.1"))` and keep:

- **A real PFILE.** The fixtures in `tests/testthat/test-read_output.R` were
  written from the format description, not from real output. A real file would
  confirm the header structure, the true column set, and whether `skip = 1` is
  right for this Winsteps version.
- **A real batch report.** `print.winsteps_report()` finds tables by matching
  `^TABLE <n>`, a pattern derived from documentation and never seen in the
  wild. One real report confirms or corrects it.

Both then become test fixtures *and* the static output vignettes need, since a
vignette cannot run Winsteps.

---

## 0b. Closed, no action needed

**Performance.** The review flagged that `winsteps_read_person_output()` reads
the PFILE twice and that `result$contents` holds whole files in memory,
speculating these might matter at scale. Measured, they do not:

| Cohort | Long rows | Elapsed | Result object |
|---|---|---|---|
| 1,000 persons x 200 items | 200k | 0.13s | 0.3 MB |
| 10,000 persons x 200 items | 2M | 1.41s | 2.5 MB |

Linear and fast at any cohort size this package would plausibly score. Dropped
rather than carried as implied future work.

---

## 1. Gaps against Rwinsteps

`Rwinsteps` (Albano & Babcock, GPL-3, archived from CRAN in 2012; a 2017
GitHub branch at 1.0.2.9000) is the only prior R-Winsteps bridge. Comparing
the two is mostly a list of things it does that this package does not -- worth
recording, because the one that matters is small and easy to miss.

**It is not a fallback.** Run under R 4.6, the archived code fails in several
places. `write.wcmd()` drops every command-file component outside a hardcoded
list of eight keywords (`unique(c(...), names(cmd)[...])` -- the second
argument to `unique()` is `incomparables`, not a second vector to concatenate),
writes a stray `END LABELS` whenever there are no labels (the guard tests base
R's `labels` function, not `cmd$labels`), and emits `CODES= 01` with a leading
space. The whole `rirf()` / `plot.ifile()` family errors under R >= 4.2 on
`if (class(x) == "ifile")`, since that class vector has length 2. Neither
`read.ifile()` nor `read.pfile()` passes `header = TRUE` in the CRAN release,
so column naming fails on real output. Two files in the GitHub branch do not
parse at all (repeated `main` formal). So "use Rwinsteps for the parts
winstepsR skips" is not advice that works: these gaps get closed here or not
at all.

**Licence boundary.** Rwinsteps is GPL-3, this package is MIT. Nothing can be
copied across -- anything below is re-implemented from the Winsteps file
formats.

### What it has that this package does not

| | Rwinsteps | Worth doing here |
|---|---|---|
| Item output (`IFILE=`) | `read.ifile()` | **Yes -- 1a below** |
| Control file reader | `read.wcmd()` | Maybe -- 1b |
| Run date and elapsed time | `daterun`, `comptime` | Cheap, yes -- 1c |
| Data file reader | `read.wdat()` | No |
| Rasch IRF/IIF/TIF math | `rirf` `riif` `rief` `rtrf` `rtif` `rtef` | No |
| Plot methods | 7 `plot.*`, 3 `lines.*` | No |
| Person ID after the responses | `name1 > item1` layouts | Only on demand |

### 1a. `winsteps_read_item_output()` -- the real gap

Item output never comes back into R at all. Anchors go in, person measures come
out, and nothing reports what Winsteps did with the anchors in between.

That matters even for pure anchored scoring, which is the workflow this package
was built for. `IFILE=`'s `DISPLACE` column is how you confirm the anchors were
actually applied rather than quietly re-estimated -- and a run whose anchor file
was ignored succeeds, returns plausible-looking measures, and is wrong. That is
precisely the class of silent-misread failure every guard in `R/validate.R`
exists to prevent, left unguarded at the one point where Winsteps itself is the
one doing the misreading.

Shape is close to what already exists:

- `IFILE=` in `winsteps_write_control_file()`, alongside `PFILE=`.
- `item_file` in `winsteps_run_paths()`, `contents$item`, and `result$items`.
- A reader mirroring `winsteps_read_person_output()`: same header-plus-table
  format, same empty-file handling, `NAME` forced to character for the same
  reason it is there.

Depends on nothing. Rank it above everything in sections 2 and 3.

### 1b. `winsteps_read_control_file()`

Parses an existing `.ctr` into the arguments `winsteps_write_control_file()`
takes, so a team with an established Winsteps setup can bring their control
file into R rather than rebuilding it by hand. Good for adoption; no current
caller, so not urgent.

A faithful round-trip is harder than `read.wcmd()` makes it look -- that
implementation lowercases every value it reads, which corrupts file paths on a
case-sensitive filesystem and any `TITLE=` text.

### 1c. Run metadata on `winsteps_result`

Rwinsteps records `daterun` and `comptime` on its result object. Free to add in
`winsteps_estimate()`, and for a package descended from a daily score check,
"when did this run, and how long did it take" is exactly the provenance a log
wants. Fits the existing `print()` summary without redesigning it.

### Declined

- **IRF/IIF/TIF math and plotting.** Real value, and the part of Rwinsteps most
  worth reading. But it is a different package's job -- `WrightMap`, `TAM` and
  `ShinyItemAnalysis` all cover it -- and taking a dependency on `graphics`
  would blur the thin-I/O-layer scope line `DESCRIPTION` commits to. Same
  answer as the scope question in section 2, for the same reason.
- **Reading a Winsteps data file back.** `read.wdat()` parses a fixed-format
  `.dat` given its layout. This package writes that file and already knows the
  layout; reading one back only helps for data that arrived from somewhere
  else, which no caller has needed.
- **`name1 > item1` layouts.** `write.wdat()` can put the person ID after the
  responses. `winsteps_prepare_person_data()` always writes ID, delimiter, then
  responses. A real capability difference, but only if a legacy fixed-format
  spec demands it. Wait for someone to ask.

---

## 2. Methods on the existing S3 classes

The classes are `winsteps_anchors`, `winsteps_person_data`, `winsteps_result`
and `winsteps_report`; all four have `print()` and nothing else.

- **`[` and `length()` for `winsteps_anchors`.** Subsetting a bank to a domain
  (`anchors[domain_items]`) is exactly what `keep_items` exists to express, and
  would read far better than passing a bank plus a keep vector. `length()`
  pairs with it.
- **`summary.winsteps_report()`** returning the table index as a data frame.
  Nearly free — the internal `winsteps_report_tables()` already computes it and
  is simply not exposed.
- **`summary.winsteps_result()`** showing the measure distribution. Higher user
  value than either of the above, but see the scope question below.

### Open question: where is the scope line?

`DESCRIPTION` says the package "intentionally does not implement any
exam-specific scoring rules". Mean and SD of person measures are generic Rasch
summary, not exam-specific — but "extreme" and "misfitting" need thresholds,
and thresholds are where project-specific judgment creeps in. Suggested
resolution: report distributional facts only, and leave every cutoff to the
caller. Decide before building.

---

## 3. Examples

Three of eleven exported objects have `@examples` (`winsteps_estimate`,
`winsteps_prepare_person_data`, `winsteps_anchors`). The gap is the writers and
readers. Most can be genuinely runnable against `tempfile()`; only
`winsteps_run()` needs `\dontrun{}`, which is legitimate since it cannot run
off Windows.

Prefer runnable examples over `\dontrun{}` wherever `run = FALSE` makes that
possible — an example that executes is checked, and one that does not is prose.

---

## 4. Vignettes

Needs `Suggests: knitr, rmarkdown` and `VignetteBuilder: knitr`. All three are
blocked on the fixtures above, since none can invoke Winsteps.

1. **Anatomy of a Winsteps run** — what each generated file is, how the
   fixed-width layout works, and why `ITEM1` and `NAMLEN` have to agree. Write
   this one first: it is the document that answers "I could not follow what the
   functions were doing", which is what prompted the S3 and structure work.
2. **Getting started** — the README flow, but executed.
3. **Domain and subset scoring** — the `keep_items` / `IDFILE` workflow.

---

## 5. CRAN-guideline hygiene

Done:

- Software name single-quoted in `Title` and `Description`; URL in angle
  brackets.
- `SystemRequirements` declares Winsteps and the Windows-only execution path.
- `shell()` called via `do.call()` so it is not an undefined global on
  platforms where base R does not define it.
- `LazyData` dropped, `Depends: R (>= 4.1)` stated, `.Rbuildignore` and
  `.gitignore` present, `NEWS.md` maintained.

Deliberately skipped:

- **`URL` and `BugReports`.** Both need a public remote, which does not exist.
  Add if the repository is ever published.
- **Chasing `codetools::checkUsageEnv(all = TRUE)` clean.** It reports
  "parameter changed by assignment" for the normalise-then-use idiom in eight
  places. `R CMD check` does not report these, and the idiom is standard R;
  rewriting them would be churn.

Still worth doing:

- A Windows CI runner. More valuable here than in most packages, because the
  execution path cannot be tested on the maintainer's machine at all — every
  `run = TRUE` branch is currently unexercised by the suite.
- Consider `spelling::spell_check_package()` and a `URL` check pass before any
  wider release.

---

## Sequencing

Section 1a is the highest-value item here and depends on nothing; 1b and 1c
likewise. Nothing in sections 2 and 3 depends on anything else either, so all of
these can be done at any time. Section 4 waits on the fixtures. The scope
question in section 2 wants an answer before `summary.winsteps_result()` is
built.
