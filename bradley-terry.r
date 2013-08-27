library(ProjectTemplate)
load.project()

# Cleans the American League (AL) and National League (NL) data scraped from
# ESPN's MLB Grid
AL_cleaned <- clean_ESPN_grid_data(AL.standings, league = "AL")
NL_cleaned <- clean_ESPN_grid_data(NL.standings, league = "NL")

# Fits the Bradley-Terry models for both leagues
set.seed(42)
AL_model <- BTm(cbind(Wins, Losses), Team, Opponent, ~ `team_`,
                id = "team_", data = AL_cleaned$standings)
NL_model <- BTm(cbind(Wins, Losses), Team, Opponent, ~ `team_`,
                id = "team_", data = NL_cleaned$standings)

# Extracts team abilities for each league
AL_abilities <- data.frame(BTabilities(AL_model))$ability
names(AL_abilities) <- AL_cleaned$teams

NL_abilities <- data.frame(BTabilities(NL_model))$ability
names(NL_abilities) <- NL_cleaned$teams

# Bar graphs of the team abilities for each league
barplot(sort(AL_abilities), main = "American League Team Abilities")
barplot(sort(NL_abilities), main = "National League Team Abilities")

# Here, we create a heatmap of probabilities winning for each team pairing.
# To do so, we create a grid of the probabilities.
# Given that the inverse logit of 0 is 0.5, the probability that team beats
# itself is estimated as 0.5. To avoid this confusing situation, we set these
# probabilities to 0. The point is that these events can never happen unless
# you play for Houston or have A-Rod on your team.
AL_probs <- outer(AL_abilities, AL_abilities, prob_BT)
diag(AL_probs) <- 0
AL_probs <- melt(AL_probs)

NL_probs <- outer(NL_abilities, NL_abilities, prob_BT)
diag(NL_probs) <- 0
NL_probs <- melt(NL_probs)

colnames(AL_probs) <- colnames(NL_probs) <- c("Team", "Opponent", "Probability")

# Plots of heatmaps of the probabilties of each team matchup
heatmap_MLB_grid_probs(AL_probs, title = "American League Matchup Probabilities")
heatmap_MLB_grid_probs(NL_probs, title = "National League Matchup Probabilities")

# American League probabilities of matchups of best, worst, and middle ranked teams

# Probability that the top-ranked AL team, Tampa Bay, beats each team
TB_probs <- subset(AL_probs, Team == "TB" & Opponent != "TB")
p <- ggplot(TB_probs, aes(x = Opponent)) + geom_bar(aes(weight = Probability))
p <- p + ylab("Probability") + theme_bw()
p <- p + geom_hline(aes(yintercept = 0.5), color = "red")
p + ggtitle("Probability of Tampa Bay Beating Other American League Opponents")

# Probability that Oakland beats each team
# Oakland is ranked in the middle according to the BT model
OAK_probs <- subset(AL_probs, Team == "OAK" & Opponent != "OAK")
p <- ggplot(OAK_probs, aes(x = Opponent)) + geom_bar(aes(weight = Probability))
p <- p + ylab("Probability") + theme_bw()
p <- p + geom_hline(aes(yintercept = 0.5), color = "red")
p + ggtitle("Probability of Oakland Beating Other American League Opponents")

# Probability that the worst-ranked AL team, Houston, beats each team
HOU_probs <- subset(AL_probs, Team == "HOU" & Opponent != "HOU")
p <- ggplot(HOU_probs, aes(x = Opponent)) + geom_bar(aes(weight = Probability))
p <- p + ylab("Probability") + theme_bw()
p <- p + geom_hline(aes(yintercept = 0.5), color = "red")
p + ggtitle("Probability of Houston Beating Other American League Opponents")



# National League probabilities of matchups of best, worst, and middle ranked teams

# Probability that the top-ranked NL team, Atlanta, beats each team
ATL_probs <- subset(NL_probs, Team == "ATL" & Opponent != "ATL")
p <- ggplot(ATL_probs, aes(x = Opponent)) + geom_bar(aes(weight = Probability))
p <- p + ylab("Probability") + theme_bw()
p <- p + geom_hline(aes(yintercept = 0.5), color = "red")
p + ggtitle("Probability of Atlanta Beating Other National League Opponents")

# Probability that Washington beats each team
# Washington is ranked in the middle according to the BT model
WSH_probs <- subset(NL_probs, Team == "WSH" & Opponent != "WSH")
p <- ggplot(WSH_probs, aes(x = Opponent)) + geom_bar(aes(weight = Probability))
p <- p + ylab("Probability") + theme_bw()
p <- p + geom_hline(aes(yintercept = 0.5), color = "red")
p + ggtitle("Probability of Washington Beating Other National League Opponents")

# Probability that the worst-ranked NL team, Miami, beats each team
MIA_probs <- subset(NL_probs, Team == "MIA" & Opponent != "MIA")
p <- ggplot(MIA_probs, aes(x = Opponent)) + geom_bar(aes(weight = Probability))
p <- p + ylab("Probability") + theme_bw()
p <- p + geom_hline(aes(yintercept = 0.5), color = "red")
p + ggtitle("Probability of Miami Beating Other National League Opponents")
