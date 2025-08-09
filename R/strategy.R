#' Get value of card to update the count
#'
#' @param card A list containing two attributes: rank and suit
#'
#' @return 1, if the rank is 10, J, Q, K, A. -1, if the rank is less than 7. Otherwise, 0.
#' @export
count_incrementer <- function(card) {
  if (!is.numeric(card$rank)) {
    return(1)
  }
  if (card$rank < 7) {
    return(-1)
  } else if (card$rank == 10) {
    return(1)
  } else {
    return(0)
  }
}

#' Update value of the count based on a new card
#'
#' @inheritParams play_round
#' @inheritParams count_incrementer
#'
#' @return A game object
#' @export
update_count <- function(game, card) {
  game[['running_count']] <- game[['running_count']] + count_incrementer(card)
  # TODO: Figure out what I need to divide by to get the true count
  game[['true_count']] <- game[['running_count']] / 1
  return(game)
}



#' Should Dealer Hit
#'
#' Dealer hits on a soft 16
#'
#' @param hand_total A hand total
#'
#' @return Boolean
#' @export
should_dealer_hit <- function(hand_total) {
  has_ace <- hand_total[['has_ace']]
  total <- hand_total[['total']]
  if (has_ace) {
    return(total < 7)
  } else {
    return(total < 17)
  }
}


#' Determine if a player should split a pair
#'
#' @param player_total The player's total
#' @param dealer_total The dealer's visible total
#'
#' @return Boolean
#' @export
should_player_split_pairs <- function(player_total, dealer_total) {
  total <- player_total[['total']]
  upcard <- dealer_total[['total']]
  if (total == 20) {
    return(FALSE)
  } else if (total == 18) {
    return(upcard %in% c(2:6, 8:9))
  } else if (total == 16) {
    return(TRUE)
  } else if (total == 14) {
    return(upcard %in% 2:7)
  } else if (total == 12) {
    return(upcard %in% 2:6)
  } else if (total == 10) {
    return(FALSE)
  } else if (total == 8) {
    return(upcard %in% 5:6)
  } else if (total == 6) {
    return(upcard %in% 2:7)
  } else if (total == 4) {
    return(upcard %in% 2:7)
  } else if (total == 2) {
    return(TRUE)
  }
}


#' Determine what decision a player should make on a soft total
#'
#' @param player_total The player's total
#' @param dealer_total The dealer's visible total
#'
#' @return One of 'hit', 'stand', 'double', 'split', or 'surrender'
#' @export
determine_soft_total_strategy <- function(player_total, dealer_total) {
  total <- player_total[['total']]
  upcard <- dealer_total[['total']]
  if (total == 11) {
    return(STAND)
  } else if (total == 10) {
    return(STAND)
  } else if (total == 9) {
    if (upcard == 6){
      return(DOUBLE)
    } else {
      return(STAND)
    }
  } else if (total == 8) {
    if (upcard %in% 2:6){
      return(DOUBLE)
    } else if (upcard %in% 7:8) {
      return(STAND)
    } else {
      return(HIT)
    }
  } else if (total == 7) {
    if (upcard == 2) {
      return(HIT)
    } else if (upcard %in% 3:6) {
      return(DOUBLE)
    } else {
      return(HIT)
    }
  } else if (total == 6) {
    if (upcard %in% 2:3) {
      return(HIT)
    } else if (upcard %in% 4:6) {
      return(DOUBLE)
    } else {
      return(HIT)
    }
  } else if (total == 5) {
    if (upcard %in% 2:3) {
      return(HIT)
    } else if (upcard %in% 4:6) {
      return(DOUBLE)
    } else {
      return(HIT)
    }
  } else if (total == 4) {
    if (upcard %in% 2:4) {
      return(HIT)
    } else if (upcard %in% 5:6) {
      return(DOUBLE)
    } else {
      return(HIT)
    }
  } else if (total == 3) {
    if (upcard %in% 2:4) {
      return(HIT)
    } else if (upcard %in% 5:6) {
      return(DOUBLE)
    } else {
      return(HIT)
    }
  }
}

#' Determine what decision a player should make on a hard total
#'
#' @param player_total The player's total
#' @param dealer_total The dealer's visible total
#'
#' @return One of 'hit', 'stand', 'double', 'split', or 'surrender'
#' @export
determine_hard_total_strategy <- function(player_total, dealer_total) {
  total <- player_total[['total']]
  upcard <- dealer_total[['total']]
  if (total >= 17) {
    return(STAND)
  } else if (total %in% 13:16) {
    if (upcard %in% 2:6) {
      return(STAND)
    } else {
      return(HIT)
    }
  } else if (total == 12) {
    if (upcard %in% 2:3) {
      return(HIT)
    } else if (upcard %in% 4:6) {
      return(STAND)
    } else {
      return(HIT)
    }
  } else if (total == 11) {
    return(DOUBLE)
  } else if (total == 10) {
    if (upcard %in% 2:9) {
      return(DOUBLE)
    } else {
      return(HIT)
    }
  } else if (total == 9) {
    if (upcard %in% 3:6) {
      return(DOUBLE)
    } else {
      return(HIT)
    }
  } else {
    return(HIT)
  }
}

#' Determine what decision a player should make when dealt cards
#'
#' @param player_hand A list of cards for a single hand
#' @param dealer_hand A list of cards
#'
#' @return One of 'hit', 'stand', 'double', 'split', or 'surrender'
#' @export
determine_player_action <- function(player_hand, dealer_hand) {
  if (length(player_hand) == 1) {
    return(HIT)
  }

  player_total <- get_hand_total(player_hand)
  visible_dealer_total <- get_hand_total(dealer_hand[1])

  if (is_hand_a_pair(player_hand)) {
    split_decision <- should_player_split_pairs(
      player_total = player_total, dealer_total = visible_dealer_total
    )
    if (split_decision) {
      return(SPLIT)
    }
  }

  if (is_hand_a_soft_ace(hand_total = player_total)) {
    action <- determine_soft_total_strategy(
      player_total = player_total, dealer_total = visible_dealer_total
    )
  } else {
    action <- determine_hard_total_strategy(
      player_total = player_total, dealer_total = visible_dealer_total
    )
  }
  return(action)
}

#' Determine what amount to bet at the beginning of the hand
#'
#' @inheritParams play_round
#'
#' @return A numeric value
#' @export
determine_bet_size <- function(game) {
  true_count <- game[['true_count']]
  unit_bet <- 1
  return(unit_bet)
}
