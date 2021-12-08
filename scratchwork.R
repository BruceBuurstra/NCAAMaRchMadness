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
  select(`high seed`, `low seed`,`win %`, wins, games) %>%
  mutate(prob = case_when(`high seed`==`low seed` ~.5,
                          TRUE ~ `win %`),
         new_wins = case_when(wins == 0 ~ wins +2,
                              wins == games ~ wins +2,
                              TRUE ~ wins), 
         new_games = case_when(wins == 0 ~ games +4L,
                               wins == games ~ games +4L,
                               TRUE ~ games),
         new_prob = case_when(`high seed`==`low seed` ~.5,
                              TRUE ~ new_wins/new_games),
         seed_diff = `low seed` - `high seed`) %>%
  select(`high seed`, `low seed`, new_prob, seed_diff)
prediction <- tibble(`high seed` = rep(c(1:16), each = 16), `low seed` = rep(1:16,16)) %>% 
  filter(`high seed` <= `low seed`) %>% 
  mutate("seed_diff" = `low seed` - `high seed`,
         "new_prob" = 0.481681 + 0.027568 * seed_diff) %>% 
  select(`high seed`, `low seed`, new_prob, seed_diff) %>% 
  filter(`high seed` %in% scratch3$`high seed` | `low seed` %in% scratch3$`low seed`)



win_probs <- scratch3%>%
  pull(prob)


ggplot(data = scratch3, mapping = aes(x = seed_diff, y = new_prob)) +
  geom_point() +
  stat_smooth(method = lm)
summary(lm(scratch3$new_prob ~ scratch3$seed_diff))
# 0.481681 + 0.027568
tibble
              

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
       filter( `High Seed Conference` == "SEC" & `Low Seed Conference` %in% conf|
                 `High Seed Conference` == conf & `Low Seed Conference` == "SEC"))

View(conferences%>%
    filter(`High Seed Conference` == "SEC" & `Low Seed Conference` == conf |
             `High Seed Conference` == conf & `Low Seed Conference` == "SEC")%>%
      mutate("Conf Win" = case_when(`High Seed Conference` == conf & `high seed win` == 1 ~ 1,
                                    `Low Seed Conference` == conf & `high seed win` == 0 ~ 1,
                                    TRUE ~ 0),
             "Conf Score" = case_when(`High Seed Conference` == conf & `high seed win` == 1 ~ `winning score`,
                                      `Low Seed Conference` == conf & `high seed win` == 0 ~ `winning score`,
                                      TRUE ~ `losing score`),
             "Non Conf Score" = case_when(`High Seed Conference` == conf & `high seed win` == 1 ~ `losing score`,
                                      `Low Seed Conference` == conf & `high seed win` == 0 ~ `losing score`,
                                      TRUE ~ `winning score`))%>%
      mutate(Difference = abs(`high seed score` - `low seed score`))%>%
      group_by(`Conf Win`)%>%
      summarise(Games = n(), ConfScore = round(mean(`Conf Score`),2), nonConfScore = round(mean(`Non Conf Score`),2), avgDiff = round(mean(Difference),2)))


conferences%>%
  filter(`High Seed Conference` == input$conference2 & `Low Seed Conference` == input$conference1 |
           `High Seed Conference` == input$conference1 & `Low Seed Conference` == input$conference2)%>%
  mutate("Conf Win" = case_when(`High Seed Conference` == input$conference1 & `high seed win` == 1 ~ 1L,
                                `Low Seed Conference` == input$conference1 & `high seed win` == 0 ~ 1L,
                                TRUE ~ 0L))%>%
           summarise(`win %` = round(mean(`Conf Win`) * 100,2))

View(Big_Dance_Seeds %>% filter(`high seed` == 1 | `low seed` == 1) %>% 
  mutate("Win" = case_when((`high seed` == 1 & `high seed score` > `low seed score`) | (`low seed` == 1 & `low seed score` > `high seed score`) ~ 1,
                           (`high seed` == 1 & `high seed score` < `low seed score`) | (`low seed` == 1 & `low seed score` < `high seed score`) ~ 0),
         "Championship" = case_when((`high seed` == 1 & `high seed score` > `low seed score` & Round == 6) | (`low seed` == 1 & `high seed score` < `low seed score` & Round == 6) ~ 1,
                                    TRUE ~ 0),
         "Champion" = case_when((`high seed` == 1 & `high seed score` > `low seed score` & Round == 5) | (`low seed` == 1 & `high seed score` < `low seed score` & Round == 5) ~ 1,
                                TRUE ~ 0),
         "Final Four" = case_when((`high seed` == 1 & `high seed score` > `low seed score` & Round == 4) | (`low seed` == 1 & `high seed score` < `low seed score` & Round == 4) ~ 1,
                                  TRUE ~ 0),
         "PF" = case_when(`high seed` == 1 ~ `high seed score`,
                          `low seed` == 1 ~ `low seed score`),
         "PA" = case_when(`high seed` == 1 ~ `low seed score`,
                          `low seed` == 1 ~ `high seed score`)) %>% 
  summarise("Games Played" = n(),
            "win%" = mean(Win),
            "# of wins" = `Games Played` * `win%`,
            "# of losses" = `Games Played` - `# of wins`,
            "Record" = paste(`# of wins`, "-", `# of losses`, sep = ""),
            "Championships Won" = mean(Championship) * n(),
            "Championships Made" = mean(Champion) * n(),
            "Final Fours" = mean(`Final Four`) * n(),
            "Average Points Scored" = round(mean(PF),2),
            "Average Points Allowed" = round(mean(PA),2)) %>% 
  select(`Games Played`, Record, `Championships Won`, `Championships Made`, `Final Fours`, `Average Points Scored`, `Average Points Allowed`))

Upsets <- Big_Dance_Seeds%>%
  filter(`high seed` != `low seed`, `round name` %in% c("Round of 64"))%>%
  mutate(Upset = case_when(`low seed score` > `high seed score` ~ 1,
                           TRUE ~ 0))%>%
  group_by(Year)%>%
  summarise(Upset = mean(Upset))
