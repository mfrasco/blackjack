library(blackjack)

# === PAIR SPLITTING TESTS ===

test_that("never split 10s (total 20)", {
  player_total <- list(total=20, has_ace=FALSE)
  for (upcard in 2:10) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_false(should_player_split_pairs(player_total, dealer_total),
                info = paste("Should not split 10s vs", upcard))
  }
})

test_that("split 9s correctly (total 18)", {
  player_total <- list(total=18, has_ace=FALSE)

  # Split vs 2-6, 8-9
  for (upcard in c(2:6, 8:9)) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_true(should_player_split_pairs(player_total, dealer_total),
               info = paste("Should split 9s vs", upcard))
  }

  # Don't split vs 7, 10
  for (upcard in c(7, 10)) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_false(should_player_split_pairs(player_total, dealer_total),
                info = paste("Should not split 9s vs", upcard))
  }
})

test_that("always split 8s (total 16)", {
  player_total <- list(total=16, has_ace=FALSE)
  for (upcard in 2:10) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_true(should_player_split_pairs(player_total, dealer_total),
               info = paste("Should split 8s vs", upcard))
  }
})

test_that("split 7s correctly (total 14)", {
  player_total <- list(total=14, has_ace=FALSE)

  # Split vs 2-7
  for (upcard in 2:7) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_true(should_player_split_pairs(player_total, dealer_total),
               info = paste("Should split 7s vs", upcard))
  }

  # Don't split vs 8-10
  for (upcard in 8:10) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_false(should_player_split_pairs(player_total, dealer_total),
                info = paste("Should not split 7s vs", upcard))
  }
})

test_that("split 6s correctly (total 12)", {
  player_total <- list(total=12, has_ace=FALSE)

  # Split vs 2-6
  for (upcard in 2:6) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_true(should_player_split_pairs(player_total, dealer_total),
               info = paste("Should split 6s vs", upcard))
  }

  # Don't split vs 7-10
  for (upcard in 7:10) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_false(should_player_split_pairs(player_total, dealer_total),
                info = paste("Should not split 6s vs", upcard))
  }
})

test_that("never split 5s (total 10)", {
  player_total <- list(total=10, has_ace=FALSE)
  for (upcard in 2:10) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_false(should_player_split_pairs(player_total, dealer_total),
                info = paste("Should not split 5s vs", upcard))
  }
})

test_that("split 4s only vs 5-6 (total 8)", {
  player_total <- list(total=8, has_ace=FALSE)

  # Split vs 5-6
  for (upcard in 5:6) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_true(should_player_split_pairs(player_total, dealer_total),
               info = paste("Should split 4s vs", upcard))
  }

  # Don't split vs 2-4, 7-10
  for (upcard in c(2:4, 7:10)) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_false(should_player_split_pairs(player_total, dealer_total),
                info = paste("Should not split 4s vs", upcard))
  }
})

test_that("split 3s correctly (total 6)", {
  player_total <- list(total=6, has_ace=FALSE)

  # Split vs 2-7
  for (upcard in 2:7) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_true(should_player_split_pairs(player_total, dealer_total),
               info = paste("Should split 3s vs", upcard))
  }

  # Don't split vs 8-10
  for (upcard in 8:10) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_false(should_player_split_pairs(player_total, dealer_total),
                info = paste("Should not split 3s vs", upcard))
  }
})

test_that("split 2s correctly (total 4)", {
  player_total <- list(total=4, has_ace=FALSE)

  # Split vs 2-7
  for (upcard in 2:7) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_true(should_player_split_pairs(player_total, dealer_total),
               info = paste("Should split 2s vs", upcard))
  }

  # Don't split vs 8-10
  for (upcard in 8:10) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_false(should_player_split_pairs(player_total, dealer_total),
                info = paste("Should not split 2s vs", upcard))
  }
})

test_that("always split Aces (total 2)", {
  player_total <- list(total=2, has_ace=TRUE)
  for (upcard in 2:10) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_true(should_player_split_pairs(player_total, dealer_total),
               info = paste("Should split Aces vs", upcard))
  }
})

# === SOFT TOTAL TESTS ===

test_that("soft 21 (A,10) always stands", {
  player_total <- list(total=11, has_ace=TRUE)
  for (upcard in 2:10) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_soft_total_strategy(player_total, dealer_total), 'stand',
                info = paste("Soft 21 vs", upcard))
  }
})

test_that("soft 20 (A,9) always stands", {
  player_total <- list(total=10, has_ace=TRUE)
  for (upcard in 2:10) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_soft_total_strategy(player_total, dealer_total), 'stand',
                info = paste("Soft 20 vs", upcard))
  }
})

test_that("soft 19 (A,8) strategy", {
  player_total <- list(total=9, has_ace=TRUE)

  # Double vs 6, stand otherwise
  dealer_total <- list(total=6, has_ace=FALSE)
  expect_equal(determine_soft_total_strategy(player_total, dealer_total), 'double')

  for (upcard in c(2:5, 7:10)) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_soft_total_strategy(player_total, dealer_total), 'stand',
                info = paste("Soft 19 vs", upcard))
  }
})

test_that("soft 18 (A,7) strategy", {
  player_total <- list(total=8, has_ace=TRUE)

  # Double vs 2-6
  for (upcard in 2:6) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_soft_total_strategy(player_total, dealer_total), 'double',
                info = paste("Soft 18 should double vs", upcard))
  }

  # Stand vs 7-8
  for (upcard in 7:8) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_soft_total_strategy(player_total, dealer_total), 'stand',
                info = paste("Soft 18 should stand vs", upcard))
  }

  # Hit vs 9-10
  for (upcard in 9:10) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_soft_total_strategy(player_total, dealer_total), 'hit',
                info = paste("Soft 18 should hit vs", upcard))
  }
})

test_that("soft 17 (A,6) strategy", {
  player_total <- list(total=7, has_ace=TRUE)

  # Hit vs 2
  dealer_total <- list(total=2, has_ace=FALSE)
  expect_equal(determine_soft_total_strategy(player_total, dealer_total), 'hit')

  # Double vs 3-6
  for (upcard in 3:6) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_soft_total_strategy(player_total, dealer_total), 'double',
                info = paste("Soft 17 should double vs", upcard))
  }

  # Hit vs 7-10
  for (upcard in 7:10) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_soft_total_strategy(player_total, dealer_total), 'hit',
                info = paste("Soft 17 should hit vs", upcard))
  }
})

test_that("soft 16 (A,5) strategy", {
  player_total <- list(total=6, has_ace=TRUE)

  # Hit vs 2-3
  for (upcard in 2:3) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_soft_total_strategy(player_total, dealer_total), 'hit',
                info = paste("Soft 16 should hit vs", upcard))
  }

  # Double vs 4-6
  for (upcard in 4:6) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_soft_total_strategy(player_total, dealer_total), 'double',
                info = paste("Soft 16 should double vs", upcard))
  }

  # Hit vs 7-10
  for (upcard in 7:10) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_soft_total_strategy(player_total, dealer_total), 'hit',
                info = paste("Soft 16 should hit vs", upcard))
  }
})

test_that("soft 15 (A,4) strategy", {
  player_total <- list(total=5, has_ace=TRUE)

  # Hit vs 2-3
  for (upcard in 2:3) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_soft_total_strategy(player_total, dealer_total), 'hit',
                info = paste("Soft 15 should hit vs", upcard))
  }

  # Double vs 4-6
  for (upcard in 4:6) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_soft_total_strategy(player_total, dealer_total), 'double',
                info = paste("Soft 15 should double vs", upcard))
  }

  # Hit vs 7-10
  for (upcard in 7:10) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_soft_total_strategy(player_total, dealer_total), 'hit',
                info = paste("Soft 15 should hit vs", upcard))
  }
})

test_that("soft 14 (A,3) strategy", {
  player_total <- list(total=4, has_ace=TRUE)

  # Hit vs 2-4
  for (upcard in 2:4) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_soft_total_strategy(player_total, dealer_total), 'hit',
                info = paste("Soft 14 should hit vs", upcard))
  }

  # Double vs 5-6
  for (upcard in 5:6) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_soft_total_strategy(player_total, dealer_total), 'double',
                info = paste("Soft 14 should double vs", upcard))
  }

  # Hit vs 7-10
  for (upcard in 7:10) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_soft_total_strategy(player_total, dealer_total), 'hit',
                info = paste("Soft 14 should hit vs", upcard))
  }
})

test_that("soft 13 (A,2) strategy", {
  player_total <- list(total=3, has_ace=TRUE)

  # Hit vs 2-4
  for (upcard in 2:4) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_soft_total_strategy(player_total, dealer_total), 'hit',
                info = paste("Soft 13 should hit vs", upcard))
  }

  # Double vs 5-6
  for (upcard in 5:6) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_soft_total_strategy(player_total, dealer_total), 'double',
                info = paste("Soft 13 should double vs", upcard))
  }

  # Hit vs 7-10
  for (upcard in 7:10) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_soft_total_strategy(player_total, dealer_total), 'hit',
                info = paste("Soft 13 should hit vs", upcard))
  }
})

# === HARD TOTAL TESTS ===

test_that("hard 17+ always stands", {
  for (total in 17:21) {
    player_total <- list(total=total, has_ace=FALSE)
    for (upcard in 2:10) {
      dealer_total <- list(total=upcard, has_ace=FALSE)
      expect_equal(determine_hard_total_strategy(player_total, dealer_total), 'stand',
                  info = paste("Hard", total, "vs", upcard))
    }
  }
})

test_that("hard 13-16 strategy", {
  for (total in 13:16) {
    player_total <- list(total=total, has_ace=FALSE)

    # Stand vs 2-6
    for (upcard in 2:6) {
      dealer_total <- list(total=upcard, has_ace=FALSE)
      expect_equal(determine_hard_total_strategy(player_total, dealer_total), 'stand',
                  info = paste("Hard", total, "should stand vs", upcard))
    }

    # Hit vs 7-10
    for (upcard in 7:10) {
      dealer_total <- list(total=upcard, has_ace=FALSE)
      expect_equal(determine_hard_total_strategy(player_total, dealer_total), 'hit',
                  info = paste("Hard", total, "should hit vs", upcard))
    }
  }
})

test_that("hard 12 strategy", {
  player_total <- list(total=12, has_ace=FALSE)

  # Hit vs 2-3
  for (upcard in 2:3) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_hard_total_strategy(player_total, dealer_total), 'hit',
                info = paste("Hard 12 should hit vs", upcard))
  }

  # Stand vs 4-6
  for (upcard in 4:6) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_hard_total_strategy(player_total, dealer_total), 'stand',
                info = paste("Hard 12 should stand vs", upcard))
  }

  # Hit vs 7-11
  for (upcard in 7:10) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_hard_total_strategy(player_total, dealer_total), 'hit',
                info = paste("Hard 12 should hit vs", upcard))
  }
})

test_that("hard 11 always doubles", {
  player_total <- list(total=11, has_ace=FALSE)
  for (upcard in 2:10) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_hard_total_strategy(player_total, dealer_total), 'double',
                info = paste("Hard 11 vs", upcard))
  }
})

test_that("hard 10 strategy", {
  player_total <- list(total=10, has_ace=FALSE)

  # Double vs 2-9
  for (upcard in 2:9) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_hard_total_strategy(player_total, dealer_total), 'double',
                info = paste("Hard 10 should double vs", upcard))
  }

  # Hit vs 10
  for (upcard in 10) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_hard_total_strategy(player_total, dealer_total), 'hit',
                info = paste("Hard 10 should hit vs", upcard))
  }
})

test_that("hard 9 strategy", {
  player_total <- list(total=9, has_ace=FALSE)

  # Hit vs 2
  dealer_total <- list(total=2, has_ace=FALSE)
  expect_equal(determine_hard_total_strategy(player_total, dealer_total), 'hit')

  # Double vs 3-6
  for (upcard in 3:6) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_hard_total_strategy(player_total, dealer_total), 'double',
                info = paste("Hard 9 should double vs", upcard))
  }

  # Hit vs 7-10
  for (upcard in 7:10) {
    dealer_total <- list(total=upcard, has_ace=FALSE)
    expect_equal(determine_hard_total_strategy(player_total, dealer_total), 'hit',
                info = paste("Hard 9 should hit vs", upcard))
  }
})

test_that("hard 8 and below always hits", {
  for (total in 4:8) {
    player_total <- list(total=total, has_ace=FALSE)
    for (upcard in 2:10) {
      dealer_total <- list(total=upcard, has_ace=FALSE)
      expect_equal(determine_hard_total_strategy(player_total, dealer_total), 'hit',
                  info = paste("Hard", total, "vs", upcard))
    }
  }
})

# === DEALER HITTING TESTS ===

test_that("dealer hits soft 16 and below", {
  # Soft totals with ace
  for (total in 2:6) {
    hand_total <- list(total=total, has_ace=TRUE)
    expect_true(should_dealer_hit(hand_total),
               info = paste("Dealer should hit soft", total + 10))
  }

  # Hard totals
  for (total in 4:16) {
    hand_total <- list(total=total, has_ace=FALSE)
    expect_true(should_dealer_hit(hand_total),
               info = paste("Dealer should hit hard", total))
  }
})

test_that("dealer stands on soft 18+ and hard 17+", {
  # Soft 17+ (total 7+ with ace)
  for (total in 7:10) {
    hand_total <- list(total=total, has_ace=TRUE)
    expect_false(should_dealer_hit(hand_total),
                info = paste("Dealer should stand on soft", total + 10))
  }

  # Hard 17+
  for (total in 17:21) {
    hand_total <- list(total=total, has_ace=FALSE)
    expect_false(should_dealer_hit(hand_total),
                info = paste("Dealer should stand on hard", total))
  }
})
