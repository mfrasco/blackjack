library(blackjack)

set.seed(0)
game <- play_rounds(chip_stacks = c(100, 100), num_decks = 10, num_rounds=500, verbose=0)
chip_counts <- game[['chip_counts']]
plot_chip_counts(chip_counts)
