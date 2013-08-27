#' Inverse logit of a probability
#' 
#' @param p probability
#' @return the inverse logit of \code{p}
inv_logit <- function(p) {
  exp(p) / (1 + exp(p))
}

#' Given two teams abilities estimated via a Bradley-Terry model, this function
#' calculates the probability that team 1 will beat team 2.
#' 
#' @param ability_1 team 1's ability under a Bradley-Terry model
#' @param ability_2 team 2's ability under a Bradley-Terry model
#' @return probability that team 1 beats team 2
prob_BT <- function(ability_1, ability_2) {
  inv_logit(ability_1 - ability_2)
}

#' Automatically cleans ESPN's MLB Grid data
#' 
#' @param standings data.frame containg the data scraped from ESPN
#' @param league Which MLB league is given in \code{standings}?
#' @return list containing the cleaned \code{standings} and the \code{teams}
clean_ESPN_grid_data <- function(standings, league = c("AL", "NL")) {
  standings <- data.table(standings)
  league <- match.arg(league)
  
  if (league == "AL") {
    standings <- standings[Opponent != "NL"]
  } else {
    standings <- standings[Opponent != "AL"]
  }
  
  # We remove the symmetric and redundant entries by exploiting the alphabetic
  # ordering of the teams. For example, we have the row
  # BAL, BOS, 6, 4
  # So, the following row is redundant
  # BOS, BAL, 4, 6
  standings <- standings[Team < Opponent]
  
  # Coerces Team and Opponent to factors having the same levels.
  # This is necessary to use the BradleyTerry2's BTm function.
  teams <- with(standings, union(unique(Team), unique(Opponent)))
  standings$Team <- factor(standings$Team, levels = teams)
  standings$Opponent <- factor(standings$Opponent, levels = teams)
  
  list(standings = standings, teams = teams)
}

#' Plots a heatmap of the probabilities of each team matchup within a dataframe
#'
#' The cuts in the heatmap are computed as the quantiles of the nonzero
#' probabilities. The quantiles are determined by \code{num_breaks}.
#'
#' @param probs data.frame of the probabilties for each team matchup
#' @param num_breaks the number of cuts in the heatmap. See details.
#' @param title title to add to the heatmap
#' @return a \code{\link{ggplot2}} object
heatmap_MLB_grid_probs <- function(probs, num_breaks = 7, title = "") {
  breaks <- with(probs, quantile(Probability[Probability > 0],
                                 probs = seq(0, 1, length = num_breaks),
                                 names = FALSE))
  breaks <- c(0, breaks, 1)
  probs$cuts <- cut(probs$Probability, breaks = breaks, dig = 2, ordered = TRUE, right = FALSE)
  p <- ggplot(probs, aes(x = Team, y = Opponent, z = cuts, color = cuts))
  p <- p + geom_tile(aes(fill = cuts))
  p <- p + scale_fill_brewer(palette = "Blues")
  p + ggtitle(title)
}