test_that("shuffling multiple decks works", {
  num_decks <- 2
  expected <- shuffle_cards(num_decks)
  expect_equal(length(expected), num_decks * 52)
})

test_that("hand totals are calculated", {
  hand <- list(
    list(rank='king', suit='spades'),
    list(rank='ace', suit='spades')
  )
  result <- get_hand_total(hand)
  expected <- list(total=11, has_ace=TRUE)
  expect_equal(result, expected)


  hand <- list(
    list(rank='ace', suit='hearts'),
    list(rank=2, suit='spades'),
    list(rank=7, suit='clubs')
  )
  result <- get_hand_total(hand)
  expected <- list(total=10, has_ace=TRUE)
  expect_equal(result, expected)

  hand <- list(
    list(rank='ace', suit='hearts'),
    list(rank=3, suit='spades'),
    list(rank='queen', suit='diamonds')
  )
  result <- get_hand_total(hand)
  expected <- list(total=14, has_ace=TRUE)
  expect_equal(result, expected)

  hand <- list(
    list(rank='ace', suit='hearts'),
    list(rank=3, suit='spades'),
    list(rank='ace', suit='hearts'),
    list(rank='queen', suit='diamonds')
  )
  result <- get_hand_total(hand)
  expected <- list(total=15, has_ace=TRUE)
  expect_equal(result, expected)

  hand <- list(
    list(rank='king', suit='clubs'),
    list(rank=5, suit='spades'),
    list(rank='queen', suit='diamonds')
  )
  result <- get_hand_total(hand)
  expected <- list(total=25, has_ace=FALSE)
  expect_equal(result, expected)

  hand <- list(
    list(rank=2, suit='clubs'),
    list(rank=5, suit='spades'),
    list(rank=3, suit='diamonds'),
    list(rank=3, suit='hearts')
  )
  result <- get_hand_total(hand)
  expected <- list(total=13, has_ace=FALSE)
  expect_equal(result, expected)
})
