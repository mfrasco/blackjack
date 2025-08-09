#' Plot Chip Counts for Each Player Over Time
#'
#' @param chip_counts A matrix, where each row is a round and each column is a player
#'
#' @return A ggplot object
#' @import ggplot2
#' @export
plot_chip_counts <- function(chip_counts) {
  # Convert matrix to long format for ggplot
  rounds <- 1:nrow(chip_counts)
  df <- data.frame(
    round = rep(rounds, ncol(chip_counts)),
    player = rep(paste("Player", 1:ncol(chip_counts)), each = nrow(chip_counts)),
    chips = as.vector(chip_counts)
  )
  
  ggplot(df, aes(x = round, y = chips, color = player)) +
    geom_line(size = 1) +
    labs(
      title = "Chip Stacks Over Time",
      x = "Round Number",
      y = "Chip Stack",
      color = "Player"
    ) +
    theme_minimal() +
    theme(legend.position = "right")
}
