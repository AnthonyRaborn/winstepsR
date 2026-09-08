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
  expect_true(all(grepl("q(07|08|09|10|11|12)$", deleted)))
})
