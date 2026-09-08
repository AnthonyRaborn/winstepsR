test_that("prepare_person_data pivots and pads correctly", {
  data <- data.frame(
    id = c("1", "1", "2", "2"),
    item = c("A", "B", "A", "B"),
    score = c(1, 0, NA, 1),
    stringsAsFactors = FALSE
  )

  prepared <- winsteps_prepare_person_data(
    data, id_col = "id", item_col = "item", score_col = "score",
    id_width = 3, item_order = c("A", "B")
  )

  expect_equal(prepared$items, c("A", "B"))
  expect_equal(prepared$n_items, 2)
  expect_equal(prepared$id_width, 3)
  expect_equal(prepared$item1, 3 + 1 + 1)
  expect_equal(prepared$lines, c("1  *10", "2  *.1"))
})

test_that("prepare_person_data errors on id_width too small", {
  data <- data.frame(id = "1000", item = "A", score = 1)
  expect_error(
    winsteps_prepare_person_data(data, "id", "item", "score", id_width = 2),
    "id_width"
  )
})

test_that("prepare_person_data errors on unknown item_order entries", {
  data <- data.frame(id = "1", item = "A", score = 1)
  expect_error(
    winsteps_prepare_person_data(data, "id", "item", "score", item_order = c("A", "B")),
    "item_order"
  )
})

test_that("write_person_data writes lines to file", {
  prepared <- list(lines = c("foo", "bar"))
  tmp <- tempfile()
  on.exit(unlink(tmp))
  winsteps_write_person_data(prepared, tmp)
  expect_equal(readLines(tmp), c("foo", "bar"))
})

test_that("prepare_person_data rejects multi-character response codes", {
  # C2: a score of 10 used to be pasted in whole, silently shifting every
  # response column after it while NI/ITEM1 still described the narrow layout.
  data <- data.frame(
    id = c("1", "1"),
    item = c("A", "B"),
    score = c(10, 1)
  )
  expect_error(
    winsteps_prepare_person_data(data, "id", "item", "score"),
    "exactly one character"
  )
  expect_error(
    winsteps_prepare_person_data(data, "id", "item", "score"),
    "XWIDE"
  )
})

test_that("prepare_person_data rejects a multi-character missing_code", {
  data <- data.frame(id = "1", item = "A", score = NA_real_)
  expect_error(
    winsteps_prepare_person_data(data, "id", "item", "score", missing_code = ".."),
    "missing_code must be exactly one character"
  )
})

test_that("prepare_person_data still accepts ordinary single-character codes", {
  data <- data.frame(
    id = c("1", "1", "2", "2"),
    item = c("A", "B", "A", "B"),
    score = c(1, 0, 2, NA)
  )
  prepared <- winsteps_prepare_person_data(
    data, "id", "item", "score", item_order = c("A", "B")
  )
  expect_equal(prepared$lines, c("1*10", "2*2."))
})

test_that("prepare_person_data rejects zero-row input", {
  # C5: an empty cohort used to yield id_width/item1 of -Inf and one
  # delimiter-only line for a person who does not exist.
  data <- data.frame(id = character(0), item = character(0), score = numeric(0))
  expect_error(
    winsteps_prepare_person_data(data, "id", "item", "score"),
    "no rows"
  )
})

test_that("prepare_person_data names the duplicated person and item", {
  # R6: this used to fail inside vctrs with a message naming neither.
  data <- data.frame(
    id = c("1", "1", "1"),
    item = c("A", "A", "B"),
    score = c(1, 0, 1)
  )
  expect_error(
    winsteps_prepare_person_data(data, "id", "item", "score"),
    "more than one response"
  )
  expect_error(
    winsteps_prepare_person_data(data, "id", "item", "score"),
    "1/A"
  )
})

test_that("prepare_person_data rejects a duplicated item_order", {
  data <- data.frame(id = c("1", "1"), item = c("A", "B"), score = c(1, 0))
  expect_error(
    winsteps_prepare_person_data(data, "id", "item", "score",
                                 item_order = c("A", "B", "A")),
    "items must be unique"
  )
})

test_that("write_person_data rejects anything that is not prepared output", {
  # R10: any list with a `lines` element used to reach writeLines().
  tmp <- tempfile()
  on.exit(unlink(tmp))
  expect_error(winsteps_write_person_data(list(lines = c(1, 2)), tmp),
               "character `lines` element")
  expect_error(winsteps_write_person_data("not a list", tmp),
               "winsteps_prepare_person_data")
})

test_that("unreadable scores warn instead of silently becoming missing", {
  # R5: a "correct"/"incorrect" column used to produce an all-missing data
  # file and a clean run that scored nobody.
  data <- data.frame(
    id = c("1", "1"),
    item = c("A", "B"),
    score = c("correct", "0")
  )
  expect_warning(
    prepared <- winsteps_prepare_person_data(data, "id", "item", "score"),
    "could not be read as numbers"
  )
  expect_equal(prepared$lines, "1*.0")
})

test_that("genuinely missing scores do not warn", {
  data <- data.frame(
    id = c("1", "1"),
    item = c("A", "B"),
    score = c(NA_real_, 0)
  )
  expect_no_warning(
    prepared <- winsteps_prepare_person_data(data, "id", "item", "score")
  )
  expect_equal(prepared$lines, "1*.0")
})

test_that("prepared data carries a class and prints its layout, not its rows", {
  data <- data.frame(
    id = rep(c("00001", "00002"), each = 2),
    item = rep(c("q1", "q2"), 2),
    score = c(1, 0, 1, 1)
  )
  prepared <- winsteps_prepare_person_data(
    data, "id", "item", "score", item_order = c("q1", "q2")
  )
  expect_s3_class(prepared, "winsteps_person_data")

  out <- capture.output(print(prepared))
  expect_match(out[1], "2 persons x 2 items")
  expect_true(any(grepl("NAMLEN=5", out)))
  expect_true(any(grepl("ITEM1=7, NI=2", out)))
  # the response block is summarized, not dumped in full
  expect_lt(length(out), 12)
})

test_that("the printed layout tracks a multi-character delimiter", {
  data <- data.frame(
    id = rep("00001", 2), item = c("q1", "q2"), score = c(1, 0)
  )
  prepared <- winsteps_prepare_person_data(
    data, "id", "item", "score", item_order = c("q1", "q2"), delimiter = "**"
  )
  out <- capture.output(print(prepared))
  expect_true(any(grepl("Delimiter 6-7", out)))
  expect_true(any(grepl("ITEM1=8", out)))
})

test_that("column positions are measured in bytes, not characters", {
  # Winsteps counts file columns in bytes, so a multi-byte ID must still land
  # the first response at the byte position ITEM1 advertises.
  data <- data.frame(
    id = c("Ana\u00efs", "Bob"),
    item = c("A", "A"),
    score = c(1, 0),
    stringsAsFactors = FALSE
  )
  prepared <- winsteps_prepare_person_data(data, "id", "item", "score")

  # "Anais" with an accent is 5 characters but 6 bytes
  expect_equal(prepared$id_width, 6)
  expect_equal(prepared$item1, 8)

  # every line is the same number of bytes wide...
  widths <- vapply(prepared$lines, function(l) length(charToRaw(l)), integer(1))
  expect_true(all(widths == widths[1]))

  # ...and the response byte really is at item1, for the ASCII and the
  # non-ASCII person alike
  at_item1 <- vapply(
    prepared$lines,
    function(l) rawToChar(charToRaw(l)[prepared$item1]),
    character(1),
    USE.NAMES = FALSE
  )
  expect_equal(at_item1, c("1", "0"))
})

test_that("a non-numeric multi-byte score is coerced to missing, not written", {
  # Scores pass through as.numeric() first, so a multi-byte value can never
  # reach the response block; it warns and becomes the missing code.
  data <- data.frame(
    id = c("1", "1"), item = c("A", "B"),
    score = c("\u00e9", "0"), stringsAsFactors = FALSE
  )
  expect_warning(
    prepared <- winsteps_prepare_person_data(data, "id", "item", "score"),
    "could not be read as numbers"
  )
  expect_equal(prepared$lines, "1*.0")
  expect_equal(length(charToRaw(prepared$lines)), 4L)
})

test_that("items in data but not in item_order are dropped with a warning", {
  # Subsetting via item_order is supported, but silently discarding responses
  # is also what a mistyped item code or a form-version mismatch looks like.
  data <- data.frame(
    id = rep("1", 3),
    item = c("q01", "q02", "q99"),
    score = c(1, 0, 1)
  )
  expect_warning(
    prepared <- winsteps_prepare_person_data(
      data, "id", "item", "score", item_order = c("q01", "q02")
    ),
    "not in item_order"
  )
  expect_warning(
    winsteps_prepare_person_data(data, "id", "item", "score",
                                 item_order = c("q01", "q02")),
    "q99"
  )
  # the retained items are still written correctly
  expect_equal(prepared$lines, "1*10")
  expect_equal(prepared$n_items, 2)
})

test_that("the dropped-response count is reported, not just the item names", {
  data <- data.frame(
    id = rep(c("1", "2", "3"), each = 2),
    item = rep(c("q01", "q99"), 3),
    score = c(1, 1, 0, 1, 1, 0)
  )
  expect_warning(
    winsteps_prepare_person_data(data, "id", "item", "score", item_order = "q01"),
    "3 response\\(s\\) were dropped"
  )
})

test_that("no warning when item_order covers every item in the data", {
  data <- data.frame(
    id = rep("1", 2), item = c("q01", "q02"), score = c(1, 0)
  )
  expect_no_warning(
    winsteps_prepare_person_data(data, "id", "item", "score",
                                 item_order = c("q01", "q02"))
  )
})
