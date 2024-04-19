
library(tidyverse)
library(tidyr)
library(mosaic)
library(ggthemes)
library(Metrics)
library(ggimage)
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

library(devtools)
devtools::install_github("abresler/nbastatR")
library(nbastatR)

# DATA MUNGING 

# read in data sets
epm23 = read.csv("C:/Users/ethan/OneDrive/Fall 2023/MAN 337/EPM_2023.csv")
epm22 = read.csv("C:/Users/ethan/OneDrive/Fall 2023/MAN 337/EPM_2022.csv")
epm21 = read.csv("C:/Users/ethan/OneDrive/Fall 2023/MAN 337/EPM_2021.csv")
epm20 = read.csv("C:/Users/ethan/OneDrive/Fall 2023/MAN 337/EPM_2020.csv")
epm19 = read.csv("C:/Users/ethan/OneDrive/Fall 2023/MAN 337/EPM_2019.csv")
epm18 = read.csv("C:/Users/ethan/OneDrive/Fall 2023/MAN 337/EPM_2018.csv")
epm17 = read.csv("C:/Users/ethan/OneDrive/Fall 2023/MAN 337/EPM_2017.csv")
epm16 = read.csv("C:/Users/ethan/OneDrive/Fall 2023/MAN 337/EPM_2016.csv")
epm15 = read.csv("C:/Users/ethan/OneDrive/Fall 2023/MAN 337/EPM_2015.csv")
epm14 = read.csv("C:/Users/ethan/OneDrive/Fall 2023/MAN 337/EPM_2014.csv")

# widen the data sets to show each player's path over their career
epm1 = rbind(epm22, epm21)
tp1 = epm1 %>% select(season, nba_id, name, team) %>% spread(key = season, value = team)
ep1 = epm1 %>% select(season, nba_id, name, epm) %>% spread(key = season, value = epm)
op1 = epm1 %>% select(season, nba_id, name, oepm) %>% spread(key = season, value = oepm)
dp1 = epm1 %>% select(season, nba_id, name, depm) %>% spread(key = season, value = depm)

# filter data sets to only players that changed teams over the offseason
tp1 = tp1 %>% filter(`2021` != `2022` & !is.na(`2021`) & !is.na(`2022`))
ep1 = ep1 %>% filter(name %in% tp1$name)
op1 = op1 %>% filter(name %in% tp1$name)
dp1 = dp1 %>% filter(name %in% tp1$name)
  
# combine the path data sets, naming columns appropriately
paths1 = tp1 %>%
  left_join(ep1, by = c("nba_id", "name"), suffix = c("", ".e")) %>%
  left_join(op1, by = c("nba_id", "name"), suffix = c("", ".o")) %>%
  left_join(dp1, by = c("nba_id", "name"), suffix = c("", ".d"))

paths1 = paths1 %>% mutate(old.yr = 2021, new.yr = 2022) %>%
  rename(oldtm = `2021`, newtm = `2022`, old.epm = `2021.e`, new.epm = `2022.e`,
         old.oepm = `2021.o`, new.oepm = `2022.o`, old.depm = `2021.d`, new.depm = `2022.d`)

# repeat this process for the next set of years
epm2 = rbind(epm21, epm20)
tp2 = epm2 %>% select(season, nba_id, name, team) %>% spread(key = season, value = team)
ep2 = epm2 %>% select(season, nba_id, name, epm) %>% spread(key = season, value = epm)
op2 = epm2 %>% select(season, nba_id, name, oepm) %>% spread(key = season, value = oepm)
dp2 = epm2 %>% select(season, nba_id, name, depm) %>% spread(key = season, value = depm)

tp2 = tp2 %>% filter(`2020` != `2021` & !is.na(`2020`) & !is.na(`2021`))
ep2 = ep2 %>% filter(name %in% tp2$name)
op2 = op2 %>% filter(name %in% tp2$name)
dp2 = dp2 %>% filter(name %in% tp2$name)

paths2 = tp2 %>%
  left_join(ep2, by = c("nba_id", "name"), suffix = c("", ".e")) %>%
  left_join(op2, by = c("nba_id", "name"), suffix = c("", ".o")) %>%
  left_join(dp2, by = c("nba_id", "name"), suffix = c("", ".d"))

paths2 = paths2 %>% mutate(old.yr = 2020, new.yr = 2021) %>%
  rename(oldtm = `2020`, newtm = `2021`, old.epm = `2020.e`, new.epm = `2021.e`,
         old.oepm = `2020.o`, new.oepm = `2021.o`, old.depm = `2020.d`, new.depm = `2021.d`)

# repeat this process for the next set of years
epm3 = rbind(epm20, epm19)
tp3 = epm3 %>% select(season, nba_id, name, team) %>% spread(key = season, value = team)
ep3 = epm3 %>% select(season, nba_id, name, epm) %>% spread(key = season, value = epm)
op3 = epm3 %>% select(season, nba_id, name, oepm) %>% spread(key = season, value = oepm)
dp3 = epm3 %>% select(season, nba_id, name, depm) %>% spread(key = season, value = depm)

tp3 = tp3 %>% filter(`2019` != `2020` & !is.na(`2019`) & !is.na(`2020`))
ep3 = ep3 %>% filter(name %in% tp3$name)
op3 = op3 %>% filter(name %in% tp3$name)
dp3 = dp3 %>% filter(name %in% tp3$name)

paths3 = tp3 %>%
  left_join(ep3, by = c("nba_id", "name"), suffix = c("", ".e")) %>%
  left_join(op3, by = c("nba_id", "name"), suffix = c("", ".o")) %>%
  left_join(dp3, by = c("nba_id", "name"), suffix = c("", ".d"))

paths3 = paths3 %>% mutate(old.yr = 2019, new.yr = 2020) %>%
  rename(oldtm = `2019`, newtm = `2020`, old.epm = `2019.e`, new.epm = `2020.e`,
         old.oepm = `2019.o`, new.oepm = `2020.o`, old.depm = `2019.d`, new.depm = `2020.d`)

# repeat this process for the next set of years
epm4 = rbind(epm19, epm18)
tp4 = epm4 %>% select(season, nba_id, name, team) %>% spread(key = season, value = team)
ep4 = epm4 %>% select(season, nba_id, name, epm) %>% spread(key = season, value = epm)
op4 = epm4 %>% select(season, nba_id, name, oepm) %>% spread(key = season, value = oepm)
dp4 = epm4 %>% select(season, nba_id, name, depm) %>% spread(key = season, value = depm)

tp4 = tp4 %>% filter(`2018` != `2019` & !is.na(`2018`) & !is.na(`2019`))
ep4 = ep4 %>% filter(name %in% tp4$name)
op4 = op4 %>% filter(name %in% tp4$name)
dp4 = dp4 %>% filter(name %in% tp4$name)

paths4 = tp4 %>%
  left_join(ep4, by = c("nba_id", "name"), suffix = c("", ".e")) %>%
  left_join(op4, by = c("nba_id", "name"), suffix = c("", ".o")) %>%
  left_join(dp4, by = c("nba_id", "name"), suffix = c("", ".d"))

paths4 = paths4 %>% mutate(old.yr = 2018, new.yr = 2019) %>%
  rename(oldtm = `2018`, newtm = `2019`, old.epm = `2018.e`, new.epm = `2019.e`,
         old.oepm = `2018.o`, new.oepm = `2019.o`, old.depm = `2018.d`, new.depm = `2019.d`)

# repeat this process for the next set of years
epm5 = rbind(epm18, epm17)
tp5 = epm5 %>% select(season, nba_id, name, team) %>% spread(key = season, value = team)
ep5 = epm5 %>% select(season, nba_id, name, epm) %>% spread(key = season, value = epm)
op5 = epm5 %>% select(season, nba_id, name, oepm) %>% spread(key = season, value = oepm)
dp5 = epm5 %>% select(season, nba_id, name, depm) %>% spread(key = season, value = depm)

tp5 = tp5 %>% filter(`2017` != `2018` & !is.na(`2017`) & !is.na(`2018`))
ep5 = ep5 %>% filter(name %in% tp5$name)
op5 = op5 %>% filter(name %in% tp5$name)
dp5 = dp5 %>% filter(name %in% tp5$name)

paths5 = tp5 %>%
  left_join(ep5, by = c("nba_id", "name"), suffix = c("", ".e")) %>%
  left_join(op5, by = c("nba_id", "name"), suffix = c("", ".o")) %>%
  left_join(dp5, by = c("nba_id", "name"), suffix = c("", ".d"))

paths5 = paths5 %>% mutate(old.yr = 2017, new.yr = 2018) %>%
  rename(oldtm = `2017`, newtm = `2018`, old.epm = `2017.e`, new.epm = `2018.e`,
         old.oepm = `2017.o`, new.oepm = `2018.o`, old.depm = `2017.d`, new.depm = `2018.d`)

# repeat this process for the next set of years
epm6 = rbind(epm17, epm16)
tp6 = epm6 %>% select(season, nba_id, name, team) %>% spread(key = season, value = team)
ep6 = epm6 %>% select(season, nba_id, name, epm) %>% spread(key = season, value = epm)
op6 = epm6 %>% select(season, nba_id, name, oepm) %>% spread(key = season, value = oepm)
dp6 = epm6 %>% select(season, nba_id, name, depm) %>% spread(key = season, value = depm)

tp6 = tp6 %>% filter(`2016` != `2017` & !is.na(`2016`) & !is.na(`2017`))
ep6 = ep6 %>% filter(name %in% tp6$name)
op6 = op6 %>% filter(name %in% tp6$name)
dp6 = dp6 %>% filter(name %in% tp6$name)

paths6 = tp6 %>%
  left_join(ep6, by = c("nba_id", "name"), suffix = c("", ".e")) %>%
  left_join(op6, by = c("nba_id", "name"), suffix = c("", ".o")) %>%
  left_join(dp6, by = c("nba_id", "name"), suffix = c("", ".d"))

paths6 = paths6 %>% mutate(old.yr = 2016, new.yr = 2017) %>%
  rename(oldtm = `2016`, newtm = `2017`, old.epm = `2016.e`, new.epm = `2017.e`,
         old.oepm = `2016.o`, new.oepm = `2017.o`, old.depm = `2016.d`, new.depm = `2017.d`)

# repeat this process for the next set of years
epm7 = rbind(epm16, epm15)
tp7 = epm7 %>% select(season, nba_id, name, team) %>% spread(key = season, value = team)
ep7 = epm7 %>% select(season, nba_id, name, epm) %>% spread(key = season, value = epm)
op7 = epm7 %>% select(season, nba_id, name, oepm) %>% spread(key = season, value = oepm)
dp7 = epm7 %>% select(season, nba_id, name, depm) %>% spread(key = season, value = depm)

tp7 = tp7 %>% filter(`2015` != `2016` & !is.na(`2015`) & !is.na(`2016`))
ep7 = ep7 %>% filter(name %in% tp7$name)
op7 = op7 %>% filter(name %in% tp7$name)
dp7 = dp7 %>% filter(name %in% tp7$name)

paths7 = tp7 %>%
  left_join(ep7, by = c("nba_id", "name"), suffix = c("", ".e")) %>%
  left_join(op7, by = c("nba_id", "name"), suffix = c("", ".o")) %>%
  left_join(dp7, by = c("nba_id", "name"), suffix = c("", ".d"))

paths7 = paths7 %>% mutate(old.yr = 2015, new.yr = 2016) %>%
  rename(oldtm = `2015`, newtm = `2016`, old.epm = `2015.e`, new.epm = `2016.e`,
         old.oepm = `2015.o`, new.oepm = `2016.o`, old.depm = `2015.d`, new.depm = `2016.d`)

# repeat this process for the next set of years
epm8 = rbind(epm15, epm14)
tp8 = epm8 %>% select(season, nba_id, name, team) %>% spread(key = season, value = team)
ep8 = epm8 %>% select(season, nba_id, name, epm) %>% spread(key = season, value = epm)
op8 = epm8 %>% select(season, nba_id, name, oepm) %>% spread(key = season, value = oepm)
dp8 = epm8 %>% select(season, nba_id, name, depm) %>% spread(key = season, value = depm)

tp8 = tp8 %>% filter(`2014` != `2015` & !is.na(`2014`) & !is.na(`2015`))
ep8 = ep8 %>% filter(name %in% tp8$name)
op8 = op8 %>% filter(name %in% tp8$name)
dp8 = dp8 %>% filter(name %in% tp8$name)

paths8 = tp8 %>%
  left_join(ep8, by = c("nba_id", "name"), suffix = c("", ".e")) %>%
  left_join(op8, by = c("nba_id", "name"), suffix = c("", ".o")) %>%
  left_join(dp8, by = c("nba_id", "name"), suffix = c("", ".d"))

paths8 = paths8 %>% mutate(old.yr = 2014, new.yr = 2015) %>%
  rename(oldtm = `2014`, newtm = `2015`, old.epm = `2014.e`, new.epm = `2015.e`,
         old.oepm = `2014.o`, new.oepm = `2015.o`, old.depm = `2014.d`, new.depm = `2015.d`)

# combine all path data sets
df = rbind(paths1, paths2, paths3, paths4, paths5, paths6, paths7, paths8)
df = df %>% mutate(delta.epm = new.epm - old.epm,
                   delta.oepm = new.oepm - old.oepm,
                   delta.depm = new.depm - old.depm)
# check that the final data is clean
df %>% is.na %>% sum



# ANALYSIS
# adjust for age? would be really difficult to collect data for that
# compare offensive vs defensive EPMs
# compare team effect on negative vs positive players

# create subsets of players
pp = df %>% filter(old.epm > 0) # positive players
np = df %>% filter(old.epm < 0) # negative players
pop = df %>% filter(old.oepm > 0) # positive offensive players
nop = df %>% filter(old.depm < 0) # negative offensive players
pdp = df %>% filter(old.depm > 0) # positive defensive players
ndp = df %>% filter(old.depm < 0) # negative defensive players

# rank teams by change in overall, offensive, or defensive EPM for some set of players
otp = df %>%
  group_by(newtm) %>%
  summarize(avg.delta.epm = mean(delta.epm),
            avg.delta.oepm = mean(delta.oepm),
            avg.delta.depm = mean(delta.depm),
            count = n()) %>%
  arrange(avg.delta.epm)
  # every single team has an average decrease in EPM when acquiring a player
  # who had a positive EPM the prior year for a different team
  # explore this --

  # appears easier to sustain positive defensive EPM than offensive EPM
  # positive offensive players are very likely to see offensive EPM decrease

  # often, players get worse when moving teams

pp %>% summarize(pos = prop(delta.epm > 0),
                 neg = prop(delta.epm < 0))
  # 77% of the time, a positive EPM player who goes to a new team sees a decrease in EPM

np %>% summarize(pos = prop(delta.epm > 0),
                 neg = prop(delta.epm < 0))
  # This is compared to 49% of negative players who see a decrease in EPM on a new team
  # The overall average is 57% of players see a decrease in EPM when moving teams

pop %>% summarize(pos = prop(delta.oepm > 0),
                  neg = prop(delta.oepm < 0))
  # 78% of the time, a positive offensive EPM player sees a decrease in offensive EPM

pop %>% filter(delta.oepm < 0) %>% summarize(avg.drop = mean(delta.oepm))
  # In fact, this average decrease in offensive EPM is by 1.75 points!!

df %>% summarize(avg.change = mean(abs(delta.oepm)))
  # A 'normal' change in offensive OEPM from year to year is 1.38 points
  # Meaning a POP is likely to see a negative change 27% greater than the average

nop %>% summarize(pos = prop(delta.oepm > 0),
                  neg = prop(delta.oepm < 0))
  # This is compared to 60% of negative players who see a decrease in EPM on a new team
  # The overall average is 57% of players see a decrease in EPM when moving teams


# turn teams into dummy variables for building a model
ot_dummies = model.matrix(~oldtm - 1, data = df)
nt_dummies = model.matrix(~newtm - 1, data = df)
dfm = cbind(df, ot_dummies, nt_dummies)
dfm = dfm %>% select(-oldtm, -newtm, -old.yr, -new.yr, -delta.epm, -delta.oepm, -delta.depm)

# build and summarize models
m.full = lm(new.epm ~ . -nba_id -name -old.oepm -new.oepm -old.depm -new.depm, data = dfm)
summary(m.full)
m.empty = lm(new.epm ~ old.epm, data = df)
summary(m.empty)
  # the full model is hardly better in-sample than the empty model -- KEEP IT SIMPLE

# split data into training and testing sets
set.seed(1)
sample = sample(c(TRUE, FALSE), nrow(dfm), replace=TRUE, prob=c(0.75,0.25))
train = dfm[sample, ]
train = train %>% select(-nba_id, -name, -old.oepm, -new.oepm, -old.depm, -new.depm)
test = dfm[!sample, ]
test = test %>% select(-nba_id, -name, -old.oepm, -new.oepm, -old.depm, -new.depm)

# now observe performance metrics on training and testing sets
m.train = lm(new.epm ~ ., data = train)
summary(m.train)
predictions = predict(m.train, newdata = test)
rmse(train$new.epm, m.train$fitted.values)
rmse(test$new.epm, predictions)
  # I thought this would be overfitting, but
  # out-of-sample RMSE is only slightly worse than in-sample RMSE
  # the true problem is that the full model is hardly better than the empty model to begin with

df %>% filter(newtm == "CLE",
              delta.epm < 0) %>% arrange(desc(delta.epm))


# DATA VISUALIZATION

  # column chart that plots avg.delta.epm for each team
  # 4 quadrant chart, maybe offensive and defensive floor raising as the axes
  # scroll through Kirk Goldsberry's instagram for inspiration

# cbind data set to plot team logos
test = otp
test = test %>% arrange(newtm)
NBA_Logos = read.csv("C:/Users/ethan/OneDrive/UT Courses/MAN 337/NBA_Logos.csv")
teamLogos = c(NBA_Logos$URI)
test = cbind(test, teamLogos)

# plot change in offensive EPM vs change in defensive EPM for every team
ggplot(test, aes(x = avg.delta.oepm, y = avg.delta.depm)) +
  geom_image(aes(image= teamLogos), size = .065) +
  geom_point(size = 0) +
  geom_point(aes(x = 0, y = 0), color = "red", size = 5) +
  geom_vline(xintercept = mean(test$avg.delta.oepm)) +
  geom_hline(yintercept = mean(test$avg.delta.depm)) +
  xlim(mean(test$avg.delta.oepm) - 0.75, mean(test$avg.delta.oepm) + 0.75) +
  ylim(mean(test$avg.delta.depm) - 0.75, mean(test$avg.delta.depm) + 0.75) +
  labs(title = "4 Quadrants of Floor Raising",
       x = "Average Change in Offensive EPM", y = "Average Change in Defensive EPM") +
  theme(plot.title = element_text(hjust = 0.5))

# plot average change in EPM for all 30 NBA teams
test = test %>% arrange(avg.delta.depm) %>% mutate(oldtm = factor(oldtm, levels = oldtm))

ggplot(test, aes(x = oldtm, y = avg.delta.depm)) +
  geom_col() +
  geom_image(aes(image=teamLogos), size = 0.045,
             y = ifelse(test$avg.delta.depm > 0, test$avg.delta.depm + 0.05, test$avg.delta.depm - 0.05)) +
  theme(axis.text.x = element_blank()) +
  labs(title = "Defensive Floor Raising", x = "", y = "Average Change in Defensive EPM") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(min(test$avg.delta.depm) - 0.1, max(test$avg.delta.depm) + 0.1)
  
# plot new EPMs vs old EPMs
ggplot(df, aes(x = old.epm, y = new.epm)) +
  geom_point(color = "#3333FF") +
  geom_smooth(method = "lm", formula = y ~ x, color = "red", linewidth = 1.5, fullrange = TRUE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  xlim(-10,10) + ylim(-10,10) +
  labs(title = "Regression Towards the Mean for Returning Players", x = "Old EPM", y = "New EPM") +
  theme(plot.title = element_text(hjust = 0.5))
  # this shows that EPM tends to regress towards the center
  # this trend is even more pronounced among players who move teams

# plot average change in OEPM for POPs compared to overall distribution
ggplot(df) +
  geom_density(aes(x = delta.oepm), linewidth = 1) +
  geom_vline(aes(xintercept = -1.748658), linetype = "dashed", col = "#00AA00", linewidth = 1.5) +
  geom_text(aes(x = -2.8, y = 0.0, label = "18.9%"), vjust = -3, color = "red", size = 6) +
  geom_text(aes(x = 0.8, y = 0.025, label = "81.1%"), vjust = -3, color = "red", size = 6)

ggplot() +
  geom_density(aes(x = delta.oepm), data = pop, linewidth = 1) +
  geom_density(aes(x = delta.oepm), data = nop, linewidth = 1) +
  geom_vline(aes(xintercept = -1.748658), col = "#00AA00", linewidth = 2)

# just for fun:
# find the best player increases over the last 10 years
df %>% select(-nba_id, -old.oepm, -new.oepm, -old.depm, -new.depm) %>%
  arrange(desc(delta.epm)) %>% head(5)
df %>% select(name, oldtm, old.yr, newtm, new.yr, old.oepm, new.oepm, delta.oepm) %>%
  arrange(desc(delta.oepm)) %>% head(5)
df %>% select(name, oldtm, old.yr, newtm, new.yr, old.depm, new.depm, delta.depm) %>%
  arrange(desc(delta.depm)) %>% head(5)

# find the worst player decreases over the last 10 years
df %>% select(-nba_id, -old.oepm, -new.oepm, -old.depm, -new.depm) %>%
  arrange(delta.epm) %>% head(5)
df %>% select(name, oldtm, old.yr, newtm, new.yr, old.oepm, new.oepm, delta.oepm) %>%
  arrange(delta.oepm) %>% head(5)
df %>% select(name, oldtm, old.yr, newtm, new.yr, old.depm, new.depm, delta.depm) %>%
  arrange(delta.depm) %>% head(5)

# plot these increases and decreases
ggplot(epm_indiv) +
  geom_point(aes(x = name, y = old.epm)) + #WHY IS IT PLOTTING THESE IN THE WRONG ORDER
  geom_point(aes(x = name, y = new.epm)) +
  geom_segment(aes(x = 1 , y = old.epm[1], xend = 1, yend = new.epm[1])) +
  geom_segment(aes(x = 2 , y = old.epm[2], xend = 2, yend = new.epm[2])) +
  geom_segment(aes(x = 3 , y = old.epm[3], xend = 3, yend = new.epm[3])) +
  geom_segment(aes(x = 4 , y = old.epm[4], xend = 4, yend = new.epm[4])) +
  geom_segment(aes(x = 5 , y = old.epm[5], xend = 5, yend = new.epm[5])) +
  ylim(-10,10) +
  labs(title = "Best Increases in EPM", x = "Player", y = "EPM values") +
  theme(plot.title = element_text(hjust = 0.5))
  
  

