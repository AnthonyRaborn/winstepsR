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

## Documentation

* `?winstepsR` package documentation, runnable examples on
  `winsteps_estimate()` and `winsteps_prepare_person_data()`.
* The claim that every call gets its own directory is corrected: paths are
  deterministic, so a reused `run_id` overwrites the earlier run.
