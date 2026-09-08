# winstepsR roadmap

Working notes on what is next and why. Not a changelog — see `NEWS.md` for what
has landed.

CRAN submission is **not** the goal. The package follows CRAN guidelines as a
quality bar, and anywhere a guideline costs more than it returns for a
single-maintainer package, that is noted rather than silently followed.

---

## 0. Blocked on a machine with Winsteps

Four questions cannot be answered without a Winsteps install; the full protocol
with runnable code is in section 10 of the code review. Summary:

| | Question | What it changes |
|---|---|---|
| W1 | Does `cmd.exe` reject a single-quoted `.bat`? | Confirms the quoting fix was a live bug, not a latent one |
| W2 | How does Winsteps read a two-character score? | Decides whether `XWIDE=` deserves real support or stays a guard |
| W3 | Does a two-character delimiter shift the person name? | Confirms whether the `NAMLEN` fix ever mattered |
| W4 | Do LF-only line endings matter to Winsteps or `cmd.exe`? | Decides whether files must be written CRLF for the cross-platform handoff |

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

## 1. Methods on the existing S3 classes

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

## 2. Examples

Three of eleven exported objects have `@examples` (`winsteps_estimate`,
`winsteps_prepare_person_data`, `winsteps_anchors`). The gap is the writers and
readers. Most can be genuinely runnable against `tempfile()`; only
`winsteps_run()` needs `\dontrun{}`, which is legitimate since it cannot run
off Windows.

Prefer runnable examples over `\dontrun{}` wherever `run = FALSE` makes that
possible — an example that executes is checked, and one that does not is prose.

---

## 3. Vignettes

Needs `Suggests: knitr, rmarkdown` and `VignetteBuilder: knitr`. All three are
blocked on the fixtures above, since none can invoke Winsteps.

1. **Anatomy of a Winsteps run** — what each generated file is, how the
   fixed-width layout works, and why `ITEM1` and `NAMLEN` have to agree. Write
   this one first: it is the document that answers "I could not follow what the
   functions were doing", which is what prompted the S3 and structure work.
2. **Getting started** — the README flow, but executed.
3. **Domain and subset scoring** — the `keep_items` / `IDFILE` workflow.

---

## 4. CRAN-guideline hygiene

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

Nothing in sections 1 and 2 depends on anything else and can be done at any
time. Section 3 waits on the fixtures. The scope question in section 1 wants an
answer before `summary.winsteps_result()` is built.
