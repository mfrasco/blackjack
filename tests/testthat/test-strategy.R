library(blackjack)

test_that("don't split 9s on a 7", {
  player_hand <- list(list(rank=9, suit='hearts'), list(rank=9, suit='spades'))
  dealer_hand <- list(list(rank=7, suit='clubs'), list(rank='ace', suit='diamonds'))
  player_total <- get_hand_total(player_hand)
  visible_dealer_total <- get_hand_total(dealer_hand[1])

  expect_false(
    should_player_split_pairs(
      player_total = player_total, dealer_total = visible_dealer_total
    )
  )
})

test_that("split 8s", {
  player_hand <- list(list(rank=8, suit='hearts'), list(rank=8, suit='spades'))
  dealer_hand <- list(list(rank=7, suit='clubs'), list(rank='ace', suit='diamonds'))
  player_total <- get_hand_total(player_hand)
  visible_dealer_total <- get_hand_total(dealer_hand[1])

  expect_true(
    should_player_split_pairs(
      player_total = player_total, dealer_total = visible_dealer_total
    )
  )
})

test_that("split 4s on a 5", {
  player_hand <- list(list(rank=4, suit='hearts'), list(rank=4, suit='spades'))
  dealer_hand <- list(list(rank=5, suit='clubs'), list(rank='ace', suit='diamonds'))
  player_total <- get_hand_total(player_hand)
  visible_dealer_total <- get_hand_total(dealer_hand[1])

  expect_true(
    should_player_split_pairs(
      player_total = player_total, dealer_total = visible_dealer_total
    )
  )
})
