big2 <- Big_Dance_Seeds %>%
  filter(`high seed` == 1, `low seed` == 1)%>%
  mutate(Difference = abs(`high seed score` - `low seed score`))%>%
  group_by(`high seed win`)%>%
  summarise(Games = n(), highSeedScore = mean(`high seed score`), lowSeedScore = mean(`low seed score`), avgDiff = mean(Difference))
View(big2)
Big_Dance_Seeds %>%
  filter(`high seed` == 5, `low seed` == 12)%>%
  mutate(Difference = abs(`high seed score` - `low seed score`))%>%
  ggplot(aes(Difference))+
  geom_histogram()+
  facet_wrap(~ `high seed win`, ncol = 1)

big3 <- Big_Dance_Seeds %>%
  filter(`high seed` == 1, `low seed` == 1)%>%
  mutate(Difference = abs(`high seed score` - `low seed score`))%>%
  summarise(winningScore = mean(`winning score`), losingScore = mean(`losing score`), avgDiff = mean(Difference))
View(big3)
Big_Dance_Seeds %>%
  filter(`high seed` == 1, `low seed` == 1)%>%
  mutate(Difference = abs(`high seed score` - `low seed score`))%>%
  ggplot(aes(Difference))+
  geom_histogram()

big4 <- Big_Dance_Seeds %>%
  filter(`high seed` == 1, `low seed` == 1)%>%
  mutate(Difference = abs(`high seed score` - `low seed score`))
View(big4)
big4 %>% ggplot(aes(Difference))+
  geom_boxplot()




scratch3 <- Big_Dance_Seeds %>%
  group_by(`high seed`, `low seed`)%>%
  summarise(`win %` = case_when(`high seed`==`low seed` ~.5,
                      TRUE ~ mean(`high seed win`))) %>%
  select(`high seed`, `low seed`, `win %`, games = n())

scratch3 <- Big_Dance_Seeds %>%
  na.omit()%>%
  group_by(`high seed`, `low seed`)%>%
  summarise(`win %` = mean(`high seed win`), wins = sum(`high seed win`), games = n()) %>%
  select(`high seed`, `low seed`,`win %`, wins, games)%>%
  mutate(prob = case_when(`high seed`==`low seed` ~.5,
                          TRUE ~ `win %`),
         new_wins = case_when(wins == 0 ~ wins +2,
                              wins == games ~ wins +2,
                              TRUE ~ wins), 
         new_games = case_when(wins == 0 ~ games +4L,
                               wins == games ~ games +4L,
                               TRUE ~ games),
         new_prob = case_when(`high seed`==`low seed` ~.5,
                              TRUE ~ new_wins/new_games))%>%
  select(`high seed`, `low seed`,new_prob)

win_probs <- scratch3%>%
  pull(prob)


team_names <- c(Big_Dance_Seeds$`high seed team`, Big_Dance_Seeds$`low seed team`)
View(team_names)
teams <- as.data.frame(team_names)

teams <- as.data.frame(distinct(teams, team_names))

install.packages("xlsx")
library(xlsx)

getwd()

write.xlsx(teams, file = "teams.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

teams$`high seed team`
Big_Dance_Seeds$`high seed team`

Big_Dance_Teams <- Big_Dance_Seeds %>%
  select(`high seed team`:`low seed score`)
library(dplyr)
conference <- left_join(Big_Dance_Seeds, teams)
conferences <- left_join(conference, teams, by= c("low seed team" = "high seed team"))
conferences <- conferences%>%
  rename("High Seed Conference" = Conference.x, "Low Seed Conference" = Conference.y)

conferences$`High Seed Conference`[2]



View(conferences%>%
  filter( `High Seed Conference` == "Big Ten" & `Low Seed Conference` == "SEC")%>%
  mutate(Difference = abs(`high seed score` - `low seed score`))%>%
  group_by(`high seed win`)%>%
  summarise(Games = n(), highSeedScore = round(mean(`high seed score`),2), lowSeedScore = round(mean(`low seed score`),2), avgDiff = round(mean(Difference),2)))

conf <- "Big Ten"
View(conferences%>%
       filter( `High Seed Conference` == "SEC" & `Low Seed Conference` %in% conf)%>%
       mutate(Difference = abs(`high seed score` - `low seed score`))%>%
       group_by(`high seed win`)%>%
       mutate("Team" = paste(conf, " Score"))%>%
       summarise(Games = n(), highSeedScore = round(mean(`high seed score`),2), Team = round(mean(`low seed score`),2), avgDiff = round(mean(Difference),2)))

View(conferences%>%
    filter(`High Seed Conference` == "SEC" & `Low Seed Conference` == "Big Ten"))
