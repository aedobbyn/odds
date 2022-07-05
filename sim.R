library(dplyr)
library(ggplot2)

# Vector of odds an oddsee can give the oddser
# The new rule doesn't work for 1 in 2 odds so start at 3
odds_given <- 3:100
# Number of simulation rounds
n_rounds <- 1000000

# Spoiler alert
equation <- function(x) {
  if (x %% 2 == 0) {
    1 / x + (x - 2) / x^2
  } else {
    1 / x + (x - 1) / x^2
  }
}

# The classic
run_odds_classic <- function(oddser, oddsee, odds_given) {
  if (oddser == oddsee) {
    TRUE
  } else {
    FALSE
  }
}

# Odds with the new rule: if the guesses add up to the total, odds works
run_odds_new <- function(oddser, oddsee, odds_given) {
  if (oddser == oddsee) {
    TRUE
  } else if (oddser + oddsee == odds_given) {
    TRUE
  } else {
    FALSE
  }
}

# Do `n_rounds` of an odds with or without the new rule
# Set the odds given by the oddsee with `odds_given`
sim <- function(odds_given, fun = run_odds_new, n_rounds) {
  message(glue::glue("Simulating {n_rounds} rounds of odds {odds_given}."))

  # For a given odds set by the oddsee (`odds_given`), pick a random number
  # between 1 and `odds_given` for both oddsee and oddser
  # Do that for every round we have
  oddser <- sample(1:odds_given, n_rounds, replace = TRUE)
  oddsee <- sample(1:odds_given, n_rounds, replace = TRUE)

  # For each round (pair of oddsee & oddser guesses), determine wither the
  # oddser won the odds or not
  bools <-
    purrr::map2_lgl(
      .x = oddser,
      .y = oddsee,
      .f = fun,
      odds_given = odds_given
    )

  # Find the percent of odds the oddser wins
  sum(bools) / n_rounds
}

# For each number an oddsee could give to the oddser (each `odds_givens`)
# run `n_rounds` of simulations to figure out the approx chance that the
# oddser will win
p_new_simed <-
  purrr::map_dbl(
    odds_given,
    sim,
    fun = run_odds_new,
    n_rounds = n_rounds
  )

# Make a dataframe of
# 1) odds the oddsee gave the oddser
# 2) probability the oddser wins in the classic game
# 3) probability the oddser wins with the new rule (simulated)
# 4) probability the oddser wins with the new rule (with the equation)
out <-
  tibble(
    odds_given = odds_given,
    p_classic = 1 / odds_given,
    p_new_simed = p_new_simed,
    p_new_solved = purrr::map_dbl(odds_given, equation)
  )

# Plotter? i barely know her
ggplot(out) +
  geom_smooth(aes(x = odds_given, y = p_new_simed, colour = "with new rule"), se = FALSE, span = 0.1) +
  geom_point(aes(x = odds_given, y = p_new_simed)) +
  geom_smooth(aes(x = odds_given, y = p_classic, colour = "without new rule"), se = FALSE, span = 0.1) +
  geom_point(aes(x = odds_given, y = p_classic)) +
  # scale_x_discrete(limits = odds_given) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual("", breaks = c("with new rule", "without new rule"), values = c("#02aaf7", "#a903fc")) +
  ggtitle("odds odds baby") +
  labs(x = "odds given", y = "p(odds works)") +
  theme_bw()
