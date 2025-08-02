test_that("a player is created", {
  num_chips <- 123
  result <- create_player(num_chips=num_chips)
  expect_equal(num_chips, result[['num_chips']])
  expect_equal('player', result[['role']])
})

test_that("players are created", {
  chip_stacks <- c(123, 456, 789)
  num_players <- length(chip_stacks)
  result <- create_players(chip_stacks=chip_stacks)
  expect_equal(num_players + 1, length(result))
  expect_equal('dealer', result[[num_players + 1]][['role']])
})

test_that("initial cards are dealt", {
  deck <- shuffle_cards(num_decks=1)
  players <- create_players(chip_stacks = c(100, 200))
  game <- list(deck=deck, players=players)
  result <- deal_initial_cards(game)
  player_1 <- result[['players']][[1]]
  player_2 <- result[['players']][[2]]
  dealer <- result[['players']][[3]]
  expect_equal(deck[[1]], player_1[['hands']][[1]][[1]])
  expect_equal(deck[[2]], player_2[['hands']][[1]][[1]])
  expect_equal(deck[[3]], dealer[['hands']][[1]][[1]])
  expect_equal(deck[[4]], player_1[['hands']][[1]][[2]])
  expect_equal(deck[[5]], player_2[['hands']][[1]][[2]])
  expect_equal(deck[[6]], dealer[['hands']][[1]][[2]])
})
