test_that("the shipped example data generates a valid Winsteps input set", {
  # Guards the fixture itself: if the example data or the writers drift, the
  # sample run set stops matching what the package would produce today.
  resp_path <- system.file("extdata", "example_responses.csv", package = "winstepsR")
  anch_path <- system.file("extdata", "example_anchors.csv", package = "winstepsR")
  skip_if(resp_path == "" || anch_path == "", "example data not installed")

  responses <- utils::read.csv(resp_path,
                               colClasses = c("character", "character", "integer"))
  anchor_tbl <- utils::read.csv(anch_path,
                                colClasses = c("character", "numeric", "character"))

  expect_equal(nrow(responses), 2400)
  expect_equal(nrow(anchor_tbl), 12)
  expect_equal(length(unique(responses$person_id)), 200)
  expect_equal(sum(is.na(responses$score)), 25)

  anchors <- winsteps_anchors(anchor_tbl$item, anchor_tbl$value)
  result <- winsteps_estimate(
    data = responses, id_col = "person_id", item_col = "item", score_col = "score",
    anchors = anchors, run_id = "example_full", working_dir = tempfile(),
    control_args = list(tfile = "17.1"),
    winsteps_exe = "Winsteps.exe", run = FALSE
  )

  expect_length(result$contents$data, 200)
  expect_true(all(nchar(result$contents$data) == 17))   # 4 ID + 1 delim + 12
  expect_true("NI=12;" %in% result$contents$control)
  expect_true("ITEM1=6;" %in% result$contents$control)
  expect_true("NAMLEN=4;" %in% result$contents$control)
  expect_true("IFILE=item.out;" %in% result$contents$control)
  expect_true("TFILE=*" %in% result$contents$control)

  # every missing response reaches the file as the missing code
  expect_equal(sum(unlist(gregexpr(".", paste(result$contents$data, collapse = ""),
                                   fixed = TRUE)) > 0), 25)

  # item names are written as labels, so item output can be joined back
  end <- which(result$contents$control == "&END")
  expect_equal(result$contents$control[(end + 1):(end + 12)], anchor_tbl$item)
})

test_that("the example data exercises the extreme-score paths", {
  resp_path <- system.file("extdata", "example_responses.csv", package = "winstepsR")
  skip_if(resp_path == "", "example data not installed")
  responses <- utils::read.csv(resp_path,
                               colClasses = c("character", "character", "integer"))

  totals <- tapply(responses$score, responses$person_id, sum, na.rm = TRUE)
  expect_gte(sum(totals == 12), 1)   # at least one perfect score
  expect_gte(sum(totals == 0), 1)    # at least one zero score
})

test_that("each example domain spans the full difficulty range", {
  anch_path <- system.file("extdata", "example_anchors.csv", package = "winstepsR")
  skip_if(anch_path == "", "example data not installed")
  anchor_tbl <- utils::read.csv(anch_path,
                                colClasses = c("character", "numeric", "character"))

  # A domain of only the easiest items is answered correctly by nearly
  # everyone, which makes a domain run degenerate rather than illustrative.
  by_domain <- split(anchor_tbl$value, anchor_tbl$domain)
  expect_length(by_domain, 2)
  for (d in by_domain) {
    expect_lt(min(d), -1)
    expect_gt(max(d), 1)
  }
})

test_that("a domain subset of the example items excludes the rest", {
  anch_path <- system.file("extdata", "example_anchors.csv", package = "winstepsR")
  skip_if(anch_path == "", "example data not installed")
  anchor_tbl <- utils::read.csv(anch_path,
                                colClasses = c("character", "numeric", "character"))
  anchors <- winsteps_anchors(anchor_tbl$item, anchor_tbl$value)

  tmp <- tempfile()
  on.exit(unlink(tmp))
  domain1 <- anchor_tbl$item[anchor_tbl$domain == "domain1"]
  winsteps_write_item_subset_file(anchors, keep = domain1, file = tmp)

  deleted <- readLines(tmp)
  expect_length(deleted, 6)
  # domains are interleaved, so the deleted set is the even-numbered items
  expect_true(all(grepl("q(02|04|06|08|10|12)$", deleted)))
})

test_that("the example item output is a real, correctly anchored Winsteps run", {
  f <- system.file("extdata", "example_domain1_item.out", package = "winstepsR")
  skip_if(f == "", "example item output not installed")
  items <- winsteps_read_item_output(f)

  expect_equal(nrow(items), 6)
  expect_equal(ncol(items), 24)

  # item names come back as the caller's names, not Winsteps' invented
  # I0001-style ones -- this is what item_labels buys
  expect_equal(items$NAME, c("q01", "q03", "q05", "q07", "q09", "q11"))
  expect_false(any(grepl("^I[0-9]+$", items$NAME)))

  # ENTRY keeps the sequence numbers from the full bank rather than
  # renumbering the retained items, so a domain run joins back on either key
  expect_equal(items$ENTRY, c(1, 3, 5, 7, 9, 11))

  # the anchors were applied: MEASURE equals the supplied difficulty exactly
  anchor_tbl <- utils::read.csv(
    system.file("extdata", "example_anchors.csv", package = "winstepsR"),
    colClasses = c("character", "numeric", "character")
  )
  supplied <- anchor_tbl$value[match(items$NAME, anchor_tbl$item)]
  expect_equal(items$MEASURE, supplied)

  # and displacement is modest, as it should be on a well-fitting anchored run
  expect_true(all(abs(items$DISPL) < 1))
})

test_that("the example person output covers the full score range", {
  f <- system.file("extdata", "example_domain1_person.out", package = "winstepsR")
  skip_if(f == "", "example person output not installed")
  persons <- winsteps_read_person_output(f)

  expect_equal(nrow(persons), 200)
  expect_type(persons$NAME, "character")
  expect_equal(persons$NAME[1], "P001")

  # every score from zero to perfect is represented, so the domain run
  # demonstrates a distribution rather than a wall of extreme scores
  expect_equal(sort(unique(persons$SCORE)), 0:6)
  expect_gt(sum(persons$SCORE > 0 & persons$SCORE < 6), 150)
})

test_that("the example delete file matches the interleaved domains", {
  f <- system.file("extdata", "example_domain1_delete.txt", package = "winstepsR")
  skip_if(f == "", "example delete file not installed")
  deleted <- readLines(f)
  expect_length(deleted, 6)
  expect_equal(sub(".*\t", "", deleted),
               c("q02", "q04", "q06", "q08", "q10", "q12"))
})

test_that("the full-run item output shows correctly applied anchors", {
  f <- system.file("extdata", "example_full_item.out", package = "winstepsR")
  skip_if(f == "", "full item output not installed")
  items <- winsteps_read_item_output(f)
  anchor_tbl <- utils::read.csv(
    system.file("extdata", "example_anchors.csv", package = "winstepsR"),
    colClasses = c("character", "numeric", "character")
  )

  expect_equal(nrow(items), 12)
  expect_equal(items$NAME, anchor_tbl$item)
  expect_equal(items$ENTRY, seq_len(12))
  # MEASURE is the supplied anchor, exactly -- the anchors were not re-estimated
  expect_equal(items$MEASURE, anchor_tbl$value)
  # displacement stays well inside a logit on a well-fitting anchored run
  expect_lt(max(abs(items$DISPL)), 0.6)
})

test_that("the full-run person output covers every score from zero to perfect", {
  f <- system.file("extdata", "example_full_person.out", package = "winstepsR")
  skip_if(f == "", "full person output not installed")
  persons <- winsteps_read_person_output(f)

  expect_equal(nrow(persons), 200)
  expect_equal(ncol(persons), 21)
  expect_type(persons$NAME, "character")
  expect_equal(range(persons$SCORE), c(0, 12))
  # persons who skipped items answered fewer than 12
  expect_true(any(persons$COUNT < 12))
})

test_that("a real Winsteps batch report is recognised as a table", {
  # The only real TFILE= output available. Its heading line is partly
  # overwritten by Winsteps -- "...Rtmpuk OUT.csvs Sep 08 2026 12:29mple_full\\c"
  # -- so it also exercises table detection against a mangled header.
  f <- system.file("extdata", "example_full_report.csv", package = "winstepsR")
  skip_if(f == "", "example report not installed")
  report <- winsteps_read_report(f)

  expect_s3_class(report, "winsteps_report")
  expect_length(report, 215)

  tables <- summary(report)
  expect_equal(nrow(tables), 1)
  expect_equal(tables$table, "17.1")
  expect_equal(tables$start, 1L)
  expect_equal(tables$lines, 215L)

  # 215 lines summarise to a handful
  out <- capture.output(print(report))
  expect_match(out[1], "215 lines from 1 table")
  expect_lt(length(out), 10)

  # and it still behaves as a character vector
  expect_true(is.character(report))
  expect_equal(sum(grepl("MINIMUM MEASURE", report)), 2)
})

test_that("the shipped data file matches what the writers produce today", {
  # Guards against the package drifting from the run that produced the output.
  f <- system.file("extdata", "example_full_data.dat", package = "winstepsR")
  resp_path <- system.file("extdata", "example_responses.csv", package = "winstepsR")
  skip_if(f == "" || resp_path == "", "example files not installed")

  responses <- utils::read.csv(resp_path,
                               colClasses = c("character", "character", "integer"))
  anchor_tbl <- utils::read.csv(
    system.file("extdata", "example_anchors.csv", package = "winstepsR"),
    colClasses = c("character", "numeric", "character")
  )
  prepared <- winsteps_prepare_person_data(
    responses, "person_id", "item", "score", item_order = anchor_tbl$item
  )
  expect_equal(prepared$lines, readLines(f))
})
