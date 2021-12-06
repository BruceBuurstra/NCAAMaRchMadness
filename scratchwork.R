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



win_probs <- scratch3%>%
  pull(prob)

ggplot(data = scratch3, mapping = aes(x = seed_diff, y = new_prob)) +
  geom_point() +
  stat_smooth(method = lm)
summary(lm(scratch3$new_prob ~ scratch3$seed_diff))
# 0.481681 + 0.027568

tibble
              