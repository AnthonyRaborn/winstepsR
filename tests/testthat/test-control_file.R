test_that("write_control_file produces expected keywords with defaults", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  winsteps_write_control_file(
    file = tmp,
    data_file = "data.dat",
    n_items = 50,
    item1 = 11,
    iafile = "anchor.txt",
    pfile = "person.out",
    item_labels = paste0("item_", 1:50)
  )
  lines <- readLines(tmp)

  expect_true("IAFILE=anchor.txt;" %in% lines)
  expect_false(any(grepl("^IDFILE=", lines)))
  expect_true("MPROX=20;" %in% lines)
  expect_true('DATA="data.dat";' %in% lines)
  expect_true("NAME1=1;" %in% lines)
  expect_true("NAMLEN=9;" %in% lines)
  expect_true("ITEM1=11;" %in% lines)
  expect_true("NI=50;" %in% lines)
  expect_true("PFILE=person.out;" %in% lines)
  expect_true("&END" %in% lines)
  expect_true("item_1" %in% lines)
  expect_equal(lines[length(lines)], "END LABELS")
})

test_that("write_control_file includes idfile, tfile, extra, and lets estimation be overridden", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  winsteps_write_control_file(
    file = tmp,
    data_file = "d.dat",
    n_items = 4,
    item1 = 11,
    idfile = "delete.txt",
    tfile = c("3.1", "3.2"),
    extra = "XWIDE=2;",
    estimation = list(MPROX = 50, MJMLE = NULL)
  )
  lines <- readLines(tmp)

  expect_true("IDFILE=delete.txt;" %in% lines)
  expect_true("MPROX=50;" %in% lines)
  expect_false(any(grepl("^MJMLE=", lines)))
  expect_true("TFILE=*" %in% lines)
  expect_true("XWIDE=2;" %in% lines)
})

test_that("a partial estimation override merges over defaults instead of replacing them", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  winsteps_write_control_file(
    file = tmp,
    data_file = "d.dat",
    n_items = 4,
    item1 = 11,
    estimation = list(RCONV = 0.5)
  )
  lines <- readLines(tmp)

  # the new key is added...
  expect_true("RCONV=0.5;" %in% lines)
  # ...and the built-in defaults are still present, not silently dropped
  expect_true("MPROX=20;" %in% lines)
  expect_true("MJMLE=0;" %in% lines)
  expect_true("CONVERGE=L;" %in% lines)
  expect_true("LCONV=0.0001;" %in% lines)
  expect_true("UDECIM=4;" %in% lines)
})

test_that("small numeric estimation values are written in plain decimal, not scientific notation", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  winsteps_write_control_file(
    file = tmp,
    data_file = "d.dat",
    n_items = 4,
    item1 = 11,
    estimation = list(LCONV = 0.0001, RCONV = 0.00005)
  )
  lines <- readLines(tmp)

  expect_true("LCONV=0.0001;" %in% lines)
  expect_true("RCONV=0.00005;" %in% lines)
  expect_false(any(grepl("e-0", lines, fixed = TRUE)))
})

test_that("write_control_file errors when item_labels length mismatches n_items", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  expect_error(
    winsteps_write_control_file(
      file = tmp, data_file = "d.dat", n_items = 3, item1 = 11,
      item_labels = c("a", "b")
    ),
    "item_labels"
  )
})

test_that("write_control_file rejects non-positive layout positions", {
  # R4: NI=-5 and ITEM1=0 used to be written without complaint.
  tmp <- tempfile()
  on.exit(unlink(tmp))
  expect_error(
    winsteps_write_control_file(tmp, "d.dat", n_items = -5, item1 = 11),
    "n_items must be a single positive whole number"
  )
  expect_error(
    winsteps_write_control_file(tmp, "d.dat", n_items = 4, item1 = 0),
    "item1 must be a single positive whole number"
  )
  expect_error(
    winsteps_write_control_file(tmp, "d.dat", n_items = 4, item1 = 11, name1 = NA),
    "name1 must be a single positive whole number"
  )
  # namlen is derived, so a name1/item1 pair that implies a zero-width ID
  # is caught too
  expect_error(
    winsteps_write_control_file(tmp, "d.dat", n_items = 4, item1 = 2, name1 = 1),
    "namlen must be a single positive whole number"
  )
})

test_that("namlen accounts for a multi-character delimiter", {
  # C4: the default assumed a one-character delimiter, so a two-character one
  # made NAMLEN one too long and the person name absorbed a delimiter char.
  tmp <- tempfile()
  on.exit(unlink(tmp))

  # ID width 3, delimiter "**" -> ITEM1 = 3 + 2 + 1 = 6, NAMLEN should be 3
  winsteps_write_control_file(tmp, "d.dat", n_items = 2, item1 = 6,
                              delimiter_width = 2)
  expect_true("NAMLEN=3;" %in% readLines(tmp))

  # the single-character default is unchanged
  winsteps_write_control_file(tmp, "d.dat", n_items = 2, item1 = 5)
  expect_true("NAMLEN=3;" %in% readLines(tmp))
})

test_that("an explicit namlen still wins over the derived default", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  winsteps_write_control_file(tmp, "d.dat", n_items = 2, item1 = 6, namlen = 4)
  expect_true("NAMLEN=4;" %in% readLines(tmp))
})
