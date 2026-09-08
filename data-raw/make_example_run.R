# Produces the example Winsteps output shipped in inst/extdata.
#
# RUN THIS ON A WINDOWS MACHINE WITH WINSTEPS INSTALLED. It reads the response
# and anchor CSVs from inst/extdata, performs two anchored runs, and copies the
# output back into inst/extdata. Everything before the runs works on any
# platform; only winsteps_estimate(run = TRUE) needs Windows.
#
# Set the executable path, then source this file from the package root.

options(winstepsR.exe_path = "C:/Winsteps/Winsteps.exe")   # <- edit if needed

library(winstepsR)

stopifnot(file.exists("inst/extdata/example_responses.csv"))
responses <- utils::read.csv("inst/extdata/example_responses.csv",
                             colClasses = c("character", "character", "integer"))
anchor_tbl <- utils::read.csv("inst/extdata/example_anchors.csv",
                              colClasses = c("character", "numeric", "character"))

anchors <- winsteps_anchors(anchor_tbl$item, anchor_tbl$value)
work <- file.path(tempdir(), "winstepsR_examples")

# --- Run 1: all items, with Table 17.1 requested so the report is not empty --
full <- winsteps_estimate(
  data = responses,
  id_col = "person_id", item_col = "item", score_col = "score",
  anchors = anchors,
  run_id = "example_full", working_dir = work,
  control_args = list(tfile = "17.1"),
  run = TRUE
)

# --- Run 2: one domain only, to exercise IDFILE ------------------------------
domain1 <- anchor_tbl$item[anchor_tbl$domain == "domain1"]
domain <- winsteps_estimate(
  data = responses,
  id_col = "person_id", item_col = "item", score_col = "score",
  anchors = anchors, keep_items = domain1,
  run_id = "example_domain1", working_dir = work,
  run = TRUE
)

# --- Collect -----------------------------------------------------------------
dest <- "inst/extdata"
copy <- function(from, to) {
  ok <- file.copy(from, file.path(dest, to), overwrite = TRUE)
  cat(if (ok) "  saved " else "  MISSING ", to, "\n", sep = "")
}
cat("Full run:\n")
copy(full$person_file,  "example_full_person.out")
copy(full$item_file,    "example_full_item.out")
copy(full$report_file,  "example_full_report.csv")
copy(full$control_file, "example_full_control.ctr")
copy(full$data_file,    "example_full_data.dat")
copy(full$anchor_file,  "example_full_anchor.txt")

cat("Domain run:\n")
copy(domain$person_file, "example_domain1_person.out")
copy(domain$item_file,   "example_domain1_item.out")
copy(domain$delete_file, "example_domain1_delete.txt")

# --- What to check before sending the files back -----------------------------
cat("\n--- full run ---\n"); print(full); print(summary(full))
cat("\nDisplacement on the anchored items (should sit near zero):\n")
print(full$items[, intersect(c("ENTRY", "NAME", "MEASURE", "DISPL"),
                             names(full$items))])
cat("\nItem output column names:\n"); print(names(full$items))
cat("\n--- domain run ---\n"); print(domain)
cat("\nReport tables found:\n"); print(summary(winsteps_read_report(full$report_file)))
