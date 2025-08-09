library(ggplot2)
library(blackjack)

set.seed(123)
chip_stacks <- c(1000, 1000, 1000)
betting_units <- c(5, 10, 25)
minimum_bet <- 1
num_decks <- 6
num_rounds <- 10000
verbose <- 0
game <- play_rounds(
  chip_stacks = chip_stacks,
  betting_units = betting_units,
  minimum_bet = minimum_bet,
  num_decks = num_decks,
  num_rounds = num_rounds,
  verbose = verbose
)
chip_counts <- game[['chip_counts']]
plot_chip_counts(chip_counts)
