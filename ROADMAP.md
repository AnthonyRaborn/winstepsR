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

### Settled, second round (8 Sep 2026)

**W2 -- silent corruption confirmed.** Winsteps accepted a data file whose
response block was four characters wide while the control file declared
`NI=3`, and returned plausible measures with no warning:

```
;ENTRY MEASURE ST COUNT SCORE ...
     1    0.72  1     3     2
     2   -0.72  1     3     1
```

Reading one character per column from `ITEM1=5` it saw `1,1,0` and `0,1,0`,
reported `COUNT = 3` for both persons, and ignored column 8 entirely. Item C
was scored 0 for both persons although both answered it correctly. This is the
worst case the review predicted: a run that looks successful and is wrong.
**The C2 guard stays a hard error.** Real `XWIDE=` support is now a genuine
feature request rather than a bug fix -- worth doing only if the item banks
ever go beyond dichotomous.

**W3 -- bug and fix both confirmed.** With the derived `NAMLEN=4`, person IDs
came back carrying the delimiter; with `delimiter_width = 2`, they came back
clean:

| `NAMLEN` | `winsteps_read_person_output(...)$NAME` |
|---|---|
| 4 (derived, pre-fix behaviour) | `"001*" "002*"` |
| 3 (`delimiter_width = 2`) | `"001" "002"` |

C4 was real, and 1dd4d60 fixes it.

**W5 -- works.** A `.bat` beginning `cd /d "%~dp0"` runs correctly and produces
a valid `person.out`. Applied in 2ff7a4b: `winsteps_run()` no longer calls
`setwd()`, so concurrent runs no longer share a working directory.

### Found by the probes

**The first PFILE column arrives as `;ENTRY`.** Winsteps comments out its own
column-name line, and the marker runs into the first name, so the column needed
backticks to reach. Stripped in 2ff7a4b. Not something the review caught --
it only surfaced once real Winsteps output was parsed.

**The real PFILE column set** is 21 columns: `ENTRY`, `MEASURE`, `ST`, `COUNT`,
`SCORE`, `MODLSE`, `IN.MSQ`, `INZSTD`, `OUTMSQ`, `OUTZST`, `DISPL`, `PTMA`,
`WEIGHT`, `OBSMA`, `EXPMA`, `PMA-E`, `RMSR`, `WMLE`, `INDF`, `OUTDF`, `NAME`.
`skip = 1` is confirmed correct for this version with `HLINES=YES`, and `NAME`
does come back as character. The read-output tests now use this column set.

### Open

| | Question | What it changes |
|---|---|---|
| W4b | Do LF endings matter to the `.bat` and `.dat`? | Remainder of W4; the control file is already known to tolerate them |

Still worth collecting: a raw `person.out` and a raw `OUT.csv` kept as files,
rather than their parsed contents. The column set above was recovered from a
printed tibble; the actual header lines and column spacing are still
unverified, and vignettes need static output they can show without running
Winsteps.

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
