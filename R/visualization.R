#' Plot Chip Counts for Each Player Over Time
#'
#' @param chip_counts A matrix, where each row is a round and each column is a player
#'
#' @return A line plot
#' @export
plot_chip_counts <- function(chip_counts) {
  plot(1:nrow(chip_counts), chip_counts[, 1], type='l', col='red', xlab = 'Round Number', ylab = 'Chip Stack')
  lines(1:nrow(chip_counts), chip_counts[, 2],  col='blue')
  title('Chip Stacks Over Time')
}
