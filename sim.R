library(bonanza)
library(ggplot2)

odds_totals <- 3:30
n_rounds <- 1000000

with_rule <- function(oddser, oddsee, total, verbose = FALSE) {
  if (oddser == oddsee) {
    if (verbose) {
      dev.glue_message("\nGot the odds on same number ({oddser}).\n\n\n")
    }
    TRUE
  } else if (oddser + oddsee == total) {
    if (verbose) {
      dev.glue_message("\nGot the odds on sum ({oddser} + {oddsee} = {total}).\n\n\n")
    }
    TRUE
  } else {
    if (verbose) {
      message("\n Didn't get the odds\n\n\n")
    }
    FALSE
  }
}

without_rule <- function(oddser, oddsee, total) {
  if (oddser == oddsee) {
    TRUE
  } else {
    FALSE
  }
}

sim <- function(odds_total = 10, fun = with_rule, n_rounds) {
  oddser <- sample(1:odds_total, n_rounds, replace = TRUE)
  oddsee <- sample(1:odds_total, n_rounds, replace = TRUE)

  bools <-
    purrr::map2_lgl(
      .x = oddser,
      .y = oddsee,
      .f = fun,
      total = odds_total
    )

  sum(bools) / n_rounds
}

all_with <-
  purrr::map_dbl(
    odds_totals,
    sim,
    n_rounds = n_rounds
  )

tbl <-
  tibble(
    total = odds_totals,
    with_new_rule = all_with,
    without_new_rule = 1 / total
  )

ggplot(tbl) +
  geom_smooth(aes(x = total, y = with_new_rule, colour = "with_new_rule"), se = FALSE) +
  geom_point(aes(x = total, y = with_new_rule), se = FALSE) +
  geom_smooth(aes(x = total, y = without_new_rule, colour = "without_new_rule"), se = FALSE) +
  theme_bw() +
  scale_x_discrete(limits = odds_totals) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("odds baby") +
  scale_colour_manual("", breaks = c("with_new_rule", "without_new_rule"), values = c("blue", "red"))
