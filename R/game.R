# hand actions
HIT <- 'hit'
STAND <- 'stand'
DOUBLE <- 'double'
SPLIT <- 'split'
SURRENDER <- 'surrender'

# hand outcomes
BLACKJACK <- 'blackjack'
WIN <- 'win'
PUSH <- 'push'
LOSE <- 'lose'


#' Create a Player
#'
#' Initialize a player with a certain number of chips
#'
#' @param num_chips Amount of money available to bet
#'
#' @return A list with information about the player
#' @export
create_player <- function(num_chips, betting_unit) {
  return(list(
    role='player',
    num_chips=num_chips,
    betting_unit=betting_unit,
    hands=list()
  ))
}

#' Create a table of players
#'
#' Initialize a full table of players, including the dealer
#'
#' @inheritParams play_rounds
#'
#' @return A list, with the dealer as the final element
#' @export
create_players <- function(chip_stacks, betting_units) {
  players <- list()
  num_players <- length(chip_stacks)
  for (i in 1:num_players) {
    players[[i]] <- create_player(
      num_chips=chip_stacks[i],
      betting_unit=betting_units[i]
    )
  }
  dealer <- list(role='dealer', hands=list())
  players[[i + 1]] <- dealer
  return(players)
}

#' Create a blackjack game
#'
#' Initialize a full table of players, including the dealer, and a random deck
#'
#' @inheritParams play_rounds
#' @inheritParams shuffle_cards
#'
#' @return An object that represents all information in the game
#' @export
create_game <- function(chip_stacks, betting_units, minimum_bet, num_decks) {
  game <- list()
  game[['players']] <- create_players(chip_stacks = chip_stacks, betting_units = betting_units)
  game[['deck']] <- shuffle_cards(num_decks = num_decks)
  game[['minimum_bet']] <- minimum_bet
  game[['running_count']] <- 0
  game[['true_count']] <- 0
  return(game)
}

#' Move Money From Chip Stacks To Place Bets
#'
#' @inheritParams play_round
#'
#' @return A game object
#' @export
place_bets <- function(game) {
  num_players <- length(game[['players']]) - 1
  for (player_num in 1:num_players) {
    num_chips <- game[['players']][[player_num]][['num_chips']]
    bet_size <- determine_bet_size(game = game, player_num = player_num)
    game[['players']][[player_num]][['num_chips']] <- num_chips - bet_size
    game[['players']][[player_num]][['bets']] <- bet_size
  }
  return(game)
}

#' Deal Initial Cards to all Players
#'
#' @inheritParams play_round
#'
#' @return A game object
#' @export
deal_initial_cards <- function(game) {
  players <- game[['players']]
  deck <- game[['deck']]
  num_players <- length(players)
  for (i in 1:num_players) {
    new_card <- deck[[1]]
    game <- update_count(game, new_card)
    players[[i]][['hands']][[1]] <- list(new_card)
    deck <- deck[-1]
  }
  for(i in 1:num_players) {
    new_card <- deck[[1]]
    if (i != num_players) {
      game <- update_count(game, new_card)
    }
    players[[i]][['hands']][[1]] <- c(players[[i]][['hands']][[1]], list(new_card))
    deck <- deck[-1]
  }
  game[['players']] <- players
  game[['deck']] <- deck
  return(game)
}

#' Take Actions For All Players
#'
#' @inheritParams play_round
#'
#' @return An updated game object
#' @export
do_player_actions <- function(game, verbose=0) {
  num_players <- length(game[['players']]) - 1
  dealer_hand <- game[['players']][[num_players + 1]][['hands']][[1]]
  for (player_num in 1:num_players) {
    player <- game[['players']][[player_num]]
    hand_index = 1
    while (hand_index <= length(game[['players']][[player_num]][['hands']])) {
      hand <- player[['hands']][[hand_index]]
      bet_size <- player[['bets']][hand_index]
      action <- determine_player_action(
        player_hand = hand, dealer_hand = dealer_hand
      )
      if (verbose > 2) {
        print_cards(hand)
        print_cards(dealer_hand[1])
        print(paste0("player: ", player_num, ", hand: ", hand_index, ", action: ", action))
      }

      if (action == SPLIT) {
        num_hands <- length(game[['players']][[player_num]][['hands']])
        player[['bets']][num_hands + 1] <- bet_size
        player[['num_chips']] <- player[['num_chips']] - bet_size
        player[['hands']][[num_hands + 1]] <- hand[1]
        player[['hands']][[hand_index]] <- hand[-1]
      } else if (action == STAND) {
        hand_index = hand_index + 1
      } else if (action == SURRENDER) {
        # TODO: Mark hand as lost
        hand_index = hand_index + 1
      } else {
        new_card <- game[['deck']][[1]]
        game <- update_count(game, new_card)
        game[['deck']] <- game[['deck']][-1]
        hand <- deal_new_card(hand, new_card)
        player[['hands']][[hand_index]] <- hand
        if (action == DOUBLE) {
          player[['bets']][[hand_index]] <- 2 * bet_size
          player[['num_chips']] <- player[['num_chips']] - bet_size
          hand_index = hand_index + 1
        } else if (is_hand_busted(hand)) {
          hand_index = hand_index + 1
        }
      }
    }
    game[['players']][[player_num]] <- player
  }
  return(game)
}

#' Is this hand not busted or black jack
#'
#' @inheritParams deal_new_card
#'
#' @return Boolean
#' @export
is_hand_active <- function(hand) {
  if (is_hand_busted(hand)) {
    return(FALSE)
  }
  if (is_black_jack(hand)) {
    return(FALSE)
  }
  return(TRUE)
}

#' Are any hands not busted or black jack
#'
#' @inheritParams play_round
#'
#' @return Boolean
#' @export
is_one_player_still_active <- function(game) {
  num_players <- length(game[['players']]) - 1
  for (player_num in 1:num_players) {
    num_hands <- length(game[['players']][[player_num]][['hands']])
    for (hand_index in 1:num_hands) {
      hand <- game[['players']][[player_num]][['hands']][[hand_index]]
      if (is_hand_active(hand)) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

#' Does the dealer have blackjack
#'
#' @inheritParams play_round
#'
#' @return Boolean
#' @export
dealer_has_blackjack <- function(game) {
  num_players <- length(game[['players']])
  return(is_black_jack(game[['players']][[num_players]][['hands']][[1]]))
}

#' Take Actions For Dealer
#'
#' @inheritParams play_round
#'
#' @return An updated game object
#' @export
do_dealer_actions <- function(game, verbose=0) {
  num_players <- length(game[['players']])
  dealer_hand <- game[['players']][[num_players]][['hands']][[1]]
  while (should_dealer_hit(get_hand_total(dealer_hand))) {
    new_card <- game[['deck']][[1]]
    game <- update_count(game, new_card)
    game[['deck']] <- game[['deck']][-1]
    dealer_hand <- deal_new_card(dealer_hand, new_card)
  }
  if (verbose > 2) {
    print('dealer has ...')
    print_cards(dealer_hand)
  }
  game[['players']][[num_players]][['hands']][[1]] <- dealer_hand
  return(game)
}

#' Determine the outcome of the round
#'
#' @param player_hand The player's cards
#' @param dealer_hand The dealer's cards
#'
#' @return One of "blackjack", "win", "push", or "lose"
#' @export
determine_hand_outcome <- function(player_hand, dealer_hand) {
  is_player_black_jack <- is_black_jack(player_hand)
  is_dealer_black_jack <- is_black_jack(dealer_hand)
  if (is_player_black_jack) {
    if (is_dealer_black_jack) {
      return(PUSH)
    } else {
      return(BLACKJACK)
    }
  }
  if (is_dealer_black_jack) {
    return(LOSE)
  }
  is_player_busted <- is_hand_busted(player_hand)
  is_dealer_busted <- is_hand_busted(dealer_hand)
  if (is_player_busted) {
    return(LOSE)
  }
  if (is_dealer_busted) {
    return(WIN)
  }
  player_total <- get_best_hand_total(player_hand)
  dealer_total <- get_best_hand_total(dealer_hand)
  if (player_total > dealer_total) {
    return(WIN)
  } else if (player_total < dealer_total) {
    return(LOSE)
  } else {
    return(PUSH)
  }
}

#' Give Additional Chips Based On Round Outcomes
#'
#' @inheritParams play_round
#'
#' @return An updated game object
#' @export
update_chip_stacks <- function(game, verbose=0) {
  num_players <- length(game[['players']])
  dealer_hand <- game[['players']][[num_players]][['hands']][[1]]
  for (player_num in 1:(num_players - 1)) {
    player <- game[['players']][[player_num]]
    num_hands <- length(player[['hands']])
    current_chips <- player[['num_chips']]
    for (hand_index in 1:num_hands) {
      player_hand <- player[['hands']][[hand_index]]
      outcome <- determine_hand_outcome(player_hand, dealer_hand)
      if (verbose > 1) {
        print(paste0("player: ", player_num, ", hand: ", hand_index, ", outcome: ", outcome))
      }
      bet_size <- player[['bets']][hand_index]
      if (outcome == BLACKJACK) {
        net_change <- 2.5 * bet_size
      } else if (outcome == WIN) {
        net_change <- 2 * bet_size
      } else if (outcome == PUSH) {
        net_change <- bet_size
      } else if (outcome == LOSE) {
        net_change <- 0
      }
      current_chips <- current_chips + net_change
    }
    if (verbose > 1) {
      print(paste0("player: ", player_num, ", num_chips: ", current_chips))
    }
    game[['players']][[player_num]][['num_chips']] <- current_chips
  }
  return(game)
}

#' Reset Chip Stacks and Hands
#'
#' @inheritParams play_round
#'
#' @return An updated game object
#' @export
reset_round <- function(game) {
  num_players <- length(game[['players']])
  for (player_num in 1:num_players) {
    if (player_num != num_players) {
      game[['players']][[player_num]][['bets']] <- c()
    }
    game[['players']][[player_num]][['hands']] <- list()
  }
  return(game)
}

#' Play a round a blackjack
#'
#' @param game An object that represents all information in the game
#' @param verbose An intger that determines how much information is print during
#'                a simulation. 0 means no information. A higher integer results in more printing.
#'
#' @return An updated game object
#' @export
play_round <- function(game, verbose=0) {
  game <- place_bets(game)
  game <- deal_initial_cards(game)
  if (!dealer_has_blackjack(game)) {
    game <- do_player_actions(game, verbose=verbose)
    if (is_one_player_still_active(game)) {
      game <- do_dealer_actions(game, verbose=verbose)
    }
  }
  game <- update_chip_stacks(game, verbose=verbose)
  game <- reset_round(game)
  return(game)
}

#' Add Player Chip Counts to a Vector
#'
#' @inheritParams play_round
#'
#' @return A vector where each element is the number of chips for a given player
#' @export
record_chip_counts <- function(game) {
  num_players <- length(game[['players']]) - 1
  counts <- rep(0, length=num_players)
  for (i in 1:num_players) {
    counts[i] <- game[['players']][[i]][['num_chips']]
  }
  return(counts)
}

#' Play multiple rounds of blackjack
#'
#' @param chip_stacks Number of chips for each player
#' @param betting_units Size of betting unit for each player
#' @param minimum_bet Smallest bet allowed on the table
#' @param num_decks Number of decks of cards to use
#' @param num_rounds An integer representing the number of rounds to play
#' @inheritParams play_round
#'
#' @return An object that represents all information in the game
#' @export
play_rounds <- function(chip_stacks, betting_units, minimum_bet, num_decks, num_rounds, verbose=0) {
  game <- create_game(
    chip_stacks = chip_stacks,
    betting_units = betting_units,
    minimum_bet = minimum_bet,
    num_decks = num_decks
  )
  total_cards <- length(game[['deck']])
  stop_point <- sample(round(total_cards / 8):round(total_cards / 4), 1)
  num_players <- length(game[['players']]) - 1
  chip_counts <- matrix(nrow=num_rounds, ncol=num_players)
  for (round_index in 1:num_rounds) {
    game <- play_round(game = game, verbose=verbose)
    chip_counts[round_index, ] <- record_chip_counts(game)
    if (length(game[['deck']]) < stop_point) {
      stop_point <- sample(round(total_cards / 8):round(total_cards / 4), 1)
      game[['deck']] <- shuffle_cards(num_decks = num_decks)
      game[['running_count']] <- 0
      game[['true_count']] <- 0
    }
  }
  game[['chip_counts']] <- chip_counts
  return(game)
}
