library(blackjack)

set.seed(2)
game <- create_game(chip_stacks = c(100, 200, 300), num_decks = 2)
game <- play_round(game)

player_hands <- game[['players']][[1]][['hands']]
dealer_hand <- game[['players']][[length(game[['players']])]][['hands']]

player_total <- get_hand_total(player_hands[[1]])
visible_dealer_total <- get_hand_total(dealer_hand[[1]][1])


player_hand <- list(
  list(rank=4, suit='hearts'),
  list(rank=4, suit='spades')
)
dealer_hand <- list(
  list(rank=6, suit='clubs'),
  list(rank='ace', suit='diamonds')
)

print(determine_player_action(player_hand, dealer_hand))

game <- create_game(chip_stacks = c(100, 200, 300), num_decks = 2)


# For each player
  # While hand_index <= length(hands)
    # determine action
    # if action == 'split'
      # update hand_index by removing card
      # add hand_index+1 by adding card
      # increase bet size on hand_index + 1
    # elif action == 'double'
      # increase bet size
      # deal one card
      # update hand_index
    # elif action == 'hit'
      # deal one card
      # if bust
        # update_hand_index
    # else
      # update hand_index
