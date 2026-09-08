# Generates the example response data and anchors shipped in inst/extdata.
#
# Run once, on any platform. Responses are simulated from the Rasch model using
# the same item difficulties that are then used as anchors, so an anchored run
# against this data is a well-fitting run: displacement should sit near zero,
# which is what makes it a usable example of anchors being applied correctly.
set.seed(2026)

n_persons <- 30
items <- sprintf("q%02d", 1:12)
difficulty <- round(seq(-2, 2, length.out = length(items)), 2)
persons <- sprintf("P%03d", seq_len(n_persons))

# Person abilities, with two deliberately extreme people so that the
# zero-score and perfect-score paths are exercised downstream.
theta <- round(stats::rnorm(n_persons, mean = 0.2, sd = 1.1), 3)
theta[1] <- 6      # will answer everything correctly
theta[2] <- -6     # will answer nothing correctly

p <- outer(theta, difficulty, function(th, b) 1 / (1 + exp(-(th - b))))
score <- matrix(stats::rbinom(length(p), 1, p), nrow = n_persons)
score[1, ] <- 1L
score[2, ] <- 0L

responses <- data.frame(
  person_id = rep(persons, times = length(items)),
  item      = rep(items, each = n_persons),
  score     = as.vector(score),
  stringsAsFactors = FALSE
)
responses <- responses[order(responses$person_id, responses$item), ]

# A few genuinely missing responses, as any real administration has.
missing_at <- c(50, 123, 204, 281, 305)
responses$score[missing_at] <- NA_integer_

anchors <- data.frame(
  item   = items,
  value  = difficulty,
  domain = rep(c("domain1", "domain2"), each = 6),
  stringsAsFactors = FALSE
)

dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
utils::write.csv(responses, "inst/extdata/example_responses.csv", row.names = FALSE)
utils::write.csv(anchors, "inst/extdata/example_anchors.csv", row.names = FALSE)

cat("persons:", n_persons, " items:", length(items),
    " rows:", nrow(responses), " missing:", sum(is.na(responses$score)), "\n")
cat("score range per person:",
    paste(range(tapply(responses$score, responses$person_id, sum, na.rm = TRUE)),
          collapse = " to "), "\n")
