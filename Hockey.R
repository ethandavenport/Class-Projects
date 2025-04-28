# Hockey

library(tidyverse)
library(stringi)
library(fuzzyjoin)
library(lubridate)
library(hockeyR)
library(ggimage)
library(ineq)
library(readr)
library(boot)



hrskaters_2324 <- read_csv('hockeyskaters2023-24.csv') %>% mutate(Year = "2023-24")
hrskaters_2223 <- read_csv('hockeyskaters2022-23.csv') %>% 
  select(1:33) %>%
  mutate(Year = "2022-23") %>%
  mutate(
    Rk = Rk.x,
    Player = Player.x,
    Age = Age.x,
    Pos = Pos.x,
    GP = GP.x
  ) %>%
  select(-Rk.x, -Player.x, -Age.x, -Pos.x, -GP.x)
hrskaters_2122 <- read_csv('hockeyskaters2021-22.csv') %>% 
  select(1:33) %>%
  mutate(Year = "2021-22") %>%
  mutate(
    Rk = Rk.x,
    Player = Player.x,
    Age = Age.x,
    Pos = Pos.x,
    GP = GP.x
  ) %>%
  select(-Rk.x, -Player.x, -Age.x, -Pos.x, -GP.x)
hrskaters <- rbind(hrskaters_2324, hrskaters_2223, hrskaters_2122)

hrgoalies_2324 <- read_csv('Hockey Reference Goalies 2023-24.csv') %>% mutate(Year = "2023-24")
hrgoalies_2223 <- read_csv('Hockey Reference Goalies 2022-23.csv') %>% mutate(Year = "2022-23")
hrgoalies_2122 <- read_csv('Hockey Reference Goalies 2021-22.csv') %>% mutate(Year = "2021-22")
hrgoalies <- rbind(hrgoalies_2324, hrgoalies_2223, hrgoalies_2122) %>%
  mutate(
    MIN = if_else(
      str_count(MIN, ":") == 2,
      str_replace(MIN, ":\\d{2}$", ""),  # remove last ":00" if there are 3 parts
      MIN
    )
  ) %>%
  separate(MIN, into = c("min_part", "sec_part"), sep = ":", convert = TRUE) %>%
  mutate(TOI = min_part + sec_part / 60) %>%
  filter(Team != "2TM", Team != "3TM") %>%
  mutate(
    GS = 0.1*SV - 0.75*GA,
    GSPG = GS/GP,
    Player.x = Player,
    Team.x = Team
  ) %>%
  select(Player.x, Team.x, Year, GP, TOI, SV, GA, GS, GSPG)

nstindividual_2324 <- read_csv('Natural Stat Trick Individual 2023-24.csv') %>% mutate(Year = "2023-24")
nstindividual_2223 <- read_csv('Natural Stat Trick Individual 2022-23.csv') %>% mutate(Year = "2022-23")
nstindividual_2122 <- read_csv('Natural Stat Trick Individual 2021-22.csv') %>% mutate(Year = "2021-22")
nstindividual <- rbind(nstindividual_2324, nstindividual_2223, nstindividual_2122)

nstonice_2324 <- read_csv('Natural Stat Trick On-Ice 2023-24.csv') %>% mutate(Year = "2023-24")
nstonice_2223 <- read_csv('Natural Stat Trick On-Ice 2022-23.csv') %>% mutate(Year = "2022-23")
nstonice_2122 <- read_csv('Natural Stat Trick On-Ice 2021-22.csv') %>% mutate(Year = "2021-22")
nstonice <- rbind(nstonice_2324, nstonice_2223, nstonice_2122)

# merge Natural Stat Trick
nst <- full_join(nstindividual, nstonice, by = c("Player", "Team", "Year", "Position", "GP", "TOI"))

# do everything
merge <-
  stringdist_join(
    nst %>% mutate(Player = stri_trans_general(Player, "Latin-ASCII")),
    hrskaters %>% mutate(Player = stri_trans_general(Player, "Latin-ASCII")),
    by = c("Player", "Year"), method = "jw", max_dist = 0.18, mode = "full") %>%
  filter(Year.x == Year.y) %>%
  filter(Pos != "G") %>%
  mutate(
    Team.x = sub("L.A", "LAK", Team.x),
    Team.x = sub("N.J", "NJD", Team.x),
    Team.x = sub("S.J", "SJS", Team.x),
    Team.x = sub("T.B", "TBL", Team.x),
    Team.y = sub("VEG", "VGK", Team.y),
    A1 = `First Assists` * GP.y / GP.x,
    A2 = `Second Assists` * GP.y / GP.x,
    PD = `Penalties Drawn` * GP.y / GP.x,
    PT = `Total Penalties` * GP.y / GP.x,
    CF = CF * GP.y / GP.x,
    CA = CA * GP.y / GP.x,
    GF = GF * GP.y / GP.x,
    GA = GA * GP.y / GP.x,
    TOI = `TOI.x`,
    TOI = TOI * GP.y / GP.x,
    GP = `GP.y`,
    Year = Year.x
  ) %>%
  select(Player.x, Team.x, Player.y, Team.y, Year, GP, TOI, G, A1, A2, SOG, BLK, PD, PT, FOW, FOL, CF, CA, GF, GA) %>%
  mutate(
    GS = 0.75*G + 0.7*A1 + 0.55*A2 + 0.075*SOG + 0.05*BLK + 0.15*PD - 0.15*PT + 
      0.01*FOW - 0.01*FOL + 0.05*CF - 0.05*CA + 0.15*GF - 0.15*GA,
    GSPG = GS/GP
  ) %>%
  rowwise() %>%
  filter(
    Team.x == Team.y | 
      (Team.y == "2TM" & grepl(",", Team.x)) |
      (Team.y == "3TM" & grepl(",", Team.x)) |
      Team.y %in% unlist(str_split(Team.x, ", "))
  ) %>%
  ungroup() %>%
  filter(
    !Team.y %in% c("2TM", "3TM"),
    !(Player.x == "Charlie Coyle" & Player.y == "Charlie McAvoy"),
    !(Player.x == "Charlie McAvoy" & Player.y == "Charlie Coyle"),
    !(Player.x == "Josh Brown" & Player.y == "Josh Doan"),
    !(Player.x == "Josh Doan" & Player.y == "Josh Brown"),
    !(Player.x == "Marcus Johansson" & Player.y == "Lucas Johansen"),
    !(Player.x == "Lucas Johansen" & Player.y == "Marcus Johansson"),
    !(Player.x == "Eric Staal" & Player.y == "Marc Staal"),
    !(Player.x == "Marc Staal" & Player.y == "Eric Staal")
  )

# did fuzzyjoin work well enough?
merge %>%
  filter(rowSums(is.na(.)) > 0)

# double check for problems in merge. players should have as many rows as teams they played for
merge %>%
  group_by(Player.x, Year) %>%
  summarize(count = n()) %>%
  filter(count == 3)

# check mismatching players
mismatch <- merge %>%
  filter(Player.x != Player.y)

# add goalie data onto skater data
all_cols <- union(names(merge), names(hrgoalies))
merge[setdiff(all_cols, names(merge))] <- NA
hrgoalies[setdiff(all_cols, names(hrgoalies))] <- NA
merge <- merge[all_cols]
hrgoalies <- hrgoalies[all_cols]
merge <- rbind(merge, hrgoalies)


##### ##### MAKE SOME PLOTS ##### ##### 

# plot team skill
merge %>%
  filter(Year == "2023-24") %>%
  group_by(Team.y) %>%
  summarize(team_skill = weighted.mean(GSPG, TOI)) %>%
  mutate(TSOA = team_skill - median(team_skill)) %>%
  arrange(desc(TSOA)) %>%
  inner_join(team_logos_colors %>% select(team_abbr, team_logo_espn), by = c(`Team.y` = "team_abbr")) %>%
  mutate(`Team.y` = factor(`Team.y`, levels = `Team.y`)) %>%
  ggplot(aes(x = Team.y, y = TSOA)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_image(aes(image=team_logo_espn, y = ifelse(TSOA > 0, TSOA + 0.03, TSOA - 0.03)), size = 0.07) +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  labs(x = "Team", y = "Weighted Average Team Skill vs. Average")

# plot Gini Index
merge %>%
  filter(TOI > 400, Year == "2023-24") %>%
  mutate(GSPG_shifted = GSPG - min(GSPG) + 0.2) %>%
  #filter(Team.y == "CHI")
  group_by(Team.y) %>%
  summarize(gini = Gini(GSPG_shifted), count = n()) %>%
  mutate(giniOA = gini - median(gini)) %>%
  arrange(desc(giniOA)) %>%
  inner_join(team_logos_colors %>% select(team_abbr, team_logo_espn), by = c(`Team.y` = "team_abbr")) %>%
  mutate(`Team.y` = factor(`Team.y`, levels = `Team.y`)) %>%
  ggplot(aes(x = Team.y, y = giniOA)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_image(aes(image=team_logo_espn, y = ifelse(giniOA > 0, giniOA + 0.005, giniOA - 0.005)), size = 0.07) +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  labs(x = "Team", y = "Intra-Team Skill Balance (Gini)")

# load NHL records data
nhl_records2324 <- read.csv('nhl_records2324.csv') %>% mutate(Year = "2023-24")
nhl_records2223 <- read.csv('nhl_records2223.csv') %>% mutate(Year = "2022-23")
nhl_records2122 <- read.csv('nhl_records2122.csv') %>% mutate(Year = "2021-22")
nhlrecords <- rbind(nhl_records2324, nhl_records2223, nhl_records2122)

# put it all together
two_metrics <- merge  %>%
  filter(TOI >= 400) %>%
  mutate(GSPG_shifted = GSPG - min(GSPG) + 0.2) %>%
  group_by(Team.x, Year) %>%
  summarize(
    team_skill = weighted.mean(GSPG, TOI),
    gini = Gini(GSPG_shifted),
    nplayers = n()
  ) %>%
  inner_join(nhlrecords, by = c("Team.x" = "Team", "Year"))

# plot correlation between team skill and gini
ggplot(two_metrics, aes(x = team_skill, y = gini)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Weighted Average Game Score", y = "Gini Index", title = "Comparing Team Skill vs Balance") +
  theme(plot.title = element_text(hjust = 0.5))

# see the correlation between team skill and gini (we want zero!!)
cor(two_metrics$team_skill, two_metrics$gini)

# see whether best or worst players matter more
four_metrics <- merge  %>%
  filter(TOI >= 400) %>%
  #group_by(Team.y, Year) %>%
  #slice_max(order_by = TOI, n = 22, with_ties = FALSE) %>%
  mutate(GSPG_shifted = GSPG - min(GSPG) + 0.2) %>%
  group_by(Team.y, Year) %>%
  mutate(skill_group = case_when(
    ntile(GSPG_shifted, 4) == 4 ~ "Best",
    ntile(GSPG_shifted, 4) == 3 ~ "G2",
    ntile(GSPG_shifted, 4) == 2 ~ "G3",
    ntile(GSPG_shifted, 4) == 1 ~ "Worst"
  )) %>%
  group_by(Team.y, Year, skill_group) %>%
  summarize(skill = mean(GSPG)) %>%
  pivot_wider(
    names_from = skill_group,
    values_from = skill
  ) %>%
  inner_join(nhlrecords, by = c("Team.y" = "Team", "Year"))

# put everything together
final <- inner_join(
  four_metrics %>% select(-W, -L, -OTL, -POW, -POGP, -POGF, -POGA), 
  two_metrics %>% select(-W, -L, -OTL, -POW, -POGP, -POGF, -POGA), 
  by = c('Team.y' = 'Team.x', 'Year', 'PTS', 'Power')
)

star_count <- merge %>%
  filter(TOI > 400) %>%
  mutate(GSPG_shifted = GSPG - min(GSPG) + 0.2) %>%
  group_by(Year) %>%
  mutate(percentile = percent_rank(GSPG)) %>%
  mutate(skill_group = case_when(
    percentile > 0.75 ~ "G4",
    percentile > 0.50 ~ "G3",
    percentile > 0.25 ~ "G2",
    percentile < 0.25 ~ "G1"
  )) %>%
  group_by(Year, Team.y) %>%
  summarize(
    G4 = sum(skill_group == "G4"),
    G3 = sum(skill_group == "G3"),
    G2 = sum(skill_group == "G2"),
    G1 = sum(skill_group == "G1")
  ) %>%
  inner_join(nhlrecords, by = c("Team.y" = "Team", "Year"))

merge %>%
  filter(TOI > 400, !is.na(Team.y)) %>%
  mutate(TOIPG = TOI/GP) %>%
  ggplot(aes(x = TOIPG, y = GSPG)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Time on Ice Per Game", y = "Game Score Per Game", title = "Comparing Team Skill vs Balance") +
  theme(plot.title = element_text(hjust = 0.5))

cor(final$team_skill, final$gini)



##### ##### USE DIFFERENT METHOD TO MAKE PREDICTIONS ##### ##### 

model <- lm(Power ~ team_skill + gini, data = final)
summary(model)

model <- lm(Power ~ Best + G2 + G3 + Worst, data = final)
summary(model)

model <- lm(Power ~ Best + G6 + G5 + Middle + G3 + G2 + Worst + team_skill + gini, data = final)
summary(model)

model <- lm(PTS ~ G1, data = star_count)
summary(model)

library(broom)
library(ggplot2)

# Tidy the model to get coefficients, standard errors, and CIs
tidy_model <- tidy(model, conf.int = TRUE) %>%
  filter(term != "(Intercept)")
  mutate(term = recode(term,
                       "Best" = "Best",
                       "G2" = "Group 2",
                       "G3" = "Group 3",
                       "Worst" = "Worst"
  ))

# Plot
ggplot(tidy_model, aes(x = term, y = estimate)) +
  geom_col(fill = "steelblue") +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = "red"), width = 0.2) +
  labs(
    title = "Model Coefficients with 95% Confidence Intervals",
    x = "Player Skill Group",
    y = "Estimate"
  ) +
  theme_minimal() + 
  theme(legend.position = "none")

# perform LOOCV
n <- nrow(final)
squared_errorsa <- numeric(n)
squared_errorsb <- numeric(n)

for (i in 1:n) {
  train_data <- final[-i, ]
  test_data <- final[i, , drop = FALSE]
  
  modela_i <- lm(PTS ~ team_skill + gini, data = train_data)
  preda_i <- predict(modela_i, newdata = test_data)
  squared_errorsa[i] <- (test_data$PTS - preda_i)^2
  
  modelb_i <- lm(PTS ~ Best + G2 + G3 + Worst, data = train_data)
  predb_i <- predict(modelb_i, newdata = test_data)
  squared_errorsb[i] <- (test_data$PTS - predb_i)^2
}

rmse_loocva <- sqrt(mean(squared_errorsa))
rmse_loocvb <- sqrt(mean(squared_errorsb))
paste(round(rmse_loocva,2), round(rmse_loocvb,2))


##### ##### MAKE PLAYOFF PREDICTIONS FOR 2024-25 ##### ##### 


hrskaters_2425 <- read_csv('hockeyskaters2024-25.csv') %>% mutate(Year = "2024-25")
hrgoalies_2425 <- read_csv('Hockey Reference Goalies 2024-25.csv') %>% 
  mutate(Year = "2024-25") %>%
  mutate(
    MIN = if_else(
      str_count(MIN, ":") == 2,
      str_replace(MIN, ":\\d{2}$", ""),  # remove last ":00" if there are 3 parts
      MIN
    )
  ) %>%
  separate(MIN, into = c("min_part", "sec_part"), sep = ":", convert = TRUE) %>%
  mutate(TOI = min_part + sec_part / 60) %>%
  filter(Team != "2TM", Team != "3TM") %>%
  mutate(
    GS = 0.1*SV - 0.75*GA,
    GSPG = GS/GP,
    Player.x = Player,
    Team.x = Team
  ) %>%
  select(Player.x, Team.x, Year, GP, TOI, SV, GA, GS, GSPG)

nstindividual_2425 <- read_csv('Natural Stat Trick Individual 2024-25.csv') %>% mutate(Year = "2024-25")
nstonice_2425 <- read_csv('Natural Stat Trick On-Ice 2024-25.csv') %>% mutate(Year = "2024-25")
nst_2425 <- full_join(nstindividual_2425, nstonice_2425, by = c("Player", "Team", "Year", "Position", "GP", "TOI"))

# do everything
merge_2425 <-
  stringdist_join(
    nst_2425 %>% mutate(Player = stri_trans_general(Player, "Latin-ASCII")),
    hrskaters_2425 %>% mutate(Player = stri_trans_general(Player, "Latin-ASCII")),
    by = c("Player", "Year"), method = "jw", max_dist = 0.18, mode = "full") %>%
  filter(Year.x == Year.y) %>%
  filter(Pos != "G") %>%
  mutate(
    Team.x = sub("L.A", "LAK", Team.x),
    Team.x = sub("N.J", "NJD", Team.x),
    Team.x = sub("S.J", "SJS", Team.x),
    Team.x = sub("T.B", "TBL", Team.x),
    Team.y = sub("VEG", "VGK", Team.y),
    A1 = `First Assists` * GP.y / GP.x,
    A2 = `Second Assists` * GP.y / GP.x,
    PD = `Penalties Drawn` * GP.y / GP.x,
    PT = `Total Penalties` * GP.y / GP.x,
    CF = CF * GP.y / GP.x,
    CA = CA * GP.y / GP.x,
    GF = GF * GP.y / GP.x,
    GA = GA * GP.y / GP.x,
    TOI = `TOI.x`,
    TOI = TOI * GP.y / GP.x,
    GP = `GP.y`,
    Year = Year.x
  ) %>%
  select(Player.x, Team.x, Player.y, Team.y, Year, GP, TOI, G, A1, A2, SOG, BLK, PD, PT, FOW, FOL, CF, CA, GF, GA) %>%
  mutate(
    GS = 0.75*G + 0.7*A1 + 0.55*A2 + 0.075*SOG + 0.05*BLK + 0.15*PD - 0.15*PT + 
      0.01*FOW - 0.01*FOL + 0.05*CF - 0.05*CA + 0.15*GF - 0.15*GA,
    GSPG = GS/GP
  ) %>%
  rowwise() %>%
  filter(
    Team.x == Team.y | 
      (Team.y == "2TM" & grepl(",", Team.x)) |
      (Team.y == "3TM" & grepl(",", Team.x)) |
      Team.y %in% unlist(str_split(Team.x, ", "))
  ) %>%
  ungroup() %>%
  filter(
    !Team.y %in% c("2TM", "3TM"),
    !(Player.x == "Marcus Pettersson" & Player.y == "Elias Pettersson"),
    !(Player.x == "Elias Pettersson" & Player.y == "Marcus Pettersson"),
    !(Player.x == "Charlie Coyle" & Player.y == "Charlie McAvoy"),
    !(Player.x == "Charlie McAvoy" & Player.y == "Charlie Coyle"),
  )

# did fuzzyjoin work well enough?
merge_2425 %>%
  filter(rowSums(is.na(.)) > 0)

# double check for problems in merge. players should have as many rows as teams they played for
merge_2425 %>%
  group_by(Player.x, Year) %>%
  summarize(count = n()) %>%
  filter(count >= 3)

# check mismatching players
mismatch_2425 <- merge_2425 %>%
  filter(Player.x != Player.y)

# add goalie data onto skater data
all_cols <- union(names(merge_2425), names(hrgoalies_2425))
merge_2425[setdiff(all_cols, names(merge_2425))] <- NA
hrgoalies_2425[setdiff(all_cols, names(hrgoalies_2425))] <- NA
merge_2425 <- merge_2425[all_cols]
hrgoalies_2425 <- hrgoalies_2425[all_cols]
merge_2425 <- rbind(merge_2425, hrgoalies_2425)

# put it all together
final_2425 <- merge_2425 %>%
  #group_by(Team.y) %>%
  #slice_max(order_by = TOI, n = 22, with_ties = FALSE) %>%
  filter(TOI > 400) %>%
  mutate(GSPG_shifted = GSPG - min(GSPG) + 0.2) %>%
  group_by(Team.y, Year) %>%
  summarize(
    team_skill = weighted.mean(GSPG, TOI),
    gini = Gini(GSPG_shifted),
    nplayers = n()
  ) %>%
  filter(!is.na(Team.y)) %>%
  ungroup()

model <- lm(Power ~ team_skill, data = final)
summary(model)

final_2425 <- final_2425 %>%
  mutate(predicted_power = predict(model, newdata = final_2425))



















best_players <- merge %>%
  filter(GP >= 30) %>%
  group_by(Team.x) %>%
  slice_max(order_by = GSPG, n = 5, with_ties = FALSE) %>%
  summarize(best_mean = mean(GSPG))

worst_players <- merge %>%
  filter(GP >= 30) %>%
  group_by(Team.x) %>%
  slice_min(order_by = GSPG, n = 5, with_ties = FALSE) %>%
  summarize(worst_mean = mean(GSPG))

inner_join(best_players, worst_players, by = "Team.x") %>%
  mutate(diff = best_mean - worst_mean) %>%
  arrange(desc(diff))

  # this just kinda seems to rank teams by how good they are, not balance
  # USE AS REGRESSORS
  # add boxplots


