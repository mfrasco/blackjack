NUMBER_RANKS <- 2:10
FACE_RANKS <- c('jack', 'queen', 'king')
ACE <- 'ace'
CLUBS <- 'clubs'
DIAMONDS <- 'diamonds'
HEARTS <- 'hearts'
SPADES <- 'spades'
SUITS <- c(CLUBS, DIAMONDS, HEARTS, SPADES)

CARDS <- list(
  list(rank=2, suit=CLUBS),
  list(rank=2, suit=DIAMONDS),
  list(rank=2, suit=HEARTS),
  list(rank=2, suit=SPADES),
  list(rank=3, suit=CLUBS),
  list(rank=3, suit=DIAMONDS),
  list(rank=3, suit=HEARTS),
  list(rank=3, suit=SPADES),
  list(rank=4, suit=CLUBS),
  list(rank=4, suit=DIAMONDS),
  list(rank=4, suit=HEARTS),
  list(rank=4, suit=SPADES),
  list(rank=5, suit=CLUBS),
  list(rank=5, suit=DIAMONDS),
  list(rank=5, suit=HEARTS),
  list(rank=5, suit=SPADES),
  list(rank=6, suit=CLUBS),
  list(rank=6, suit=DIAMONDS),
  list(rank=6, suit=HEARTS),
  list(rank=6, suit=SPADES),
  list(rank=7, suit=CLUBS),
  list(rank=7, suit=DIAMONDS),
  list(rank=7, suit=HEARTS),
  list(rank=7, suit=SPADES),
  list(rank=8, suit=CLUBS),
  list(rank=8, suit=DIAMONDS),
  list(rank=8, suit=HEARTS),
  list(rank=8, suit=SPADES),
  list(rank=9, suit=CLUBS),
  list(rank=9, suit=DIAMONDS),
  list(rank=9, suit=HEARTS),
  list(rank=9, suit=SPADES),
  list(rank=10, suit=CLUBS),
  list(rank=10, suit=DIAMONDS),
  list(rank=10, suit=HEARTS),
  list(rank=10, suit=SPADES),
  list(rank='jack', suit=CLUBS),
  list(rank='jack', suit=DIAMONDS),
  list(rank='jack', suit=HEARTS),
  list(rank='jack', suit=SPADES),
  list(rank='queen', suit=CLUBS),
  list(rank='queen', suit=DIAMONDS),
  list(rank='queen', suit=HEARTS),
  list(rank='queen', suit=SPADES),
  list(rank='king', suit=CLUBS),
  list(rank='king', suit=DIAMONDS),
  list(rank='king', suit=HEARTS),
  list(rank='king', suit=SPADES),
  list(rank='ace', suit=CLUBS),
  list(rank='ace', suit=DIAMONDS),
  list(rank='ace', suit=HEARTS),
  list(rank='ace', suit=SPADES)
)

#' Shuffle a Deck of Cards
#'
#' Uses a standard 52 card deck. A card is represented as a list with two
#' attributes: rank and suit. A deck is a list of cards.
#'
#' @param num_decks The number of decks you want to include in the shuffle.
#'
#' @return A list, where each element represents a card.
#' @export
shuffle_cards <- function(num_decks) {
  num_cards <- num_decks * length(CARDS)
  unshuffled_deck <- rep(CARDS, num_decks)
  return(unshuffled_deck[sample(1:num_cards, num_cards)])
}


#' Get Hand Total
#'
#' Determine the total value of the cards in a hand
#'
#' @param hand A list of cards
#'
#' @return A list containing the numeric total and an indicator on whether the
#'  hand includes an ace
#' @export
get_hand_total <- function(hand) {

  total <- 0
  has_ace <- FALSE

  for (card in hand) {
    if (is.numeric(card[['rank']])) {
      total <- total + card[['rank']]
    } else if (card[['rank']] %in% FACE_RANKS) {
      total <- total + 10
    } else {
      if (has_ace) {
        total <- total + 11
      } else {
        total <- total + 1
        has_ace <- TRUE
      }
    }
  }
  return(list(total=total, has_ace=has_ace))
}

#' Get Hand Total
#'
#' Determine the total value of the cards in a hand
#'
#' @inheritParams get_hand_total
#'
#' @return The best numeric value that a hand can make without going over
#' @export
get_best_hand_total <- function(hand) {
  hand_total <- get_hand_total(hand)
  if (!hand_total[['has_ace']]) {
    return(hand_total[['total']])
  }
  if (hand_total[['total']] <= 11) {
    return(hand_total[['total']] + 10)
  } else {
    return(hand_total[['total']])
  }
}

#' Deal a New Card to a Player's Hand
#'
#' @inheritParams get_hand_total
#' @param new_card A new card
#'
#' @return An updated list of new cards
#' @export
deal_new_card <- function(hand, new_card) {
  new_card_index <- length(hand) + 1
  hand[[new_card_index]] <- new_card
  return(hand)
}

#' Determine if a players hand is a pair
#'
#' @inheritParams get_hand_total
#'
#' @return Boolean
#' @export
is_hand_a_pair <- function(hand) {
  if (length(hand) != 2) {
    return(FALSE)
  }
  rank_1 <- hand[[1]][['rank']]
  rank_2 <- hand[[2]][['rank']]
  if (rank_1 %in% NUMBER_RANKS) {
    return(rank_1 == rank_2)
  } else if (rank_1 %in% FACE_RANKS) {
    return(rank_2 %in% FACE_RANKS)
  } else {
    return(rank_2 == ACE)
  }
}

#' Determine if a player's hand has a soft ace
#'
#' @param hand_total A list with the total value of the ranks and a flag on
#'  whether the hand has an ace. The first ace in the hand contributes 1 to the total.
#'
#' @return Boolean
#' @export
is_hand_a_soft_ace <- function(hand_total) {
  has_ace <- hand_total[['has_ace']]
  total <- hand_total[['total']]
  return(has_ace && total <= 11)
}

#' Determine if a hand has gone over 21
#'
#' @inheritParams get_hand_total
#'
#' @return Boolean
#' @export
is_hand_busted <- function(hand) {
  total <- get_hand_total(hand)
  return(total[['total']] > 21)
}

#' Determine if a hand is a 2-card 21
#'
#' @inheritParams get_hand_total
#'
#' @return Boolean
#' @export
is_black_jack <- function(hand) {
  total <- get_hand_total(hand)
  return(total[['has_ace']] && (total[['total']] == 11) && (length(hand) == 2))
}


#' Pretty Print Cards
#'
#' More beautiful than printing a list
#'
#' @inheritParams get_hand_total
#'
#' @return None
#' @export
print_cards <- function(hand) {
  output <- c()
  for (card in hand) {
    output <- c(output, paste(card$rank, card$suit))
  }
  print(paste(output, collapse = ', '))
}


