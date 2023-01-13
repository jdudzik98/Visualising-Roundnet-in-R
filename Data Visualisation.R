
library(ggradar)
library(dplyr)
library(scales)
library(tibble)
library(tidyr)
#install.packages("plotly")
library(plotly)

df_long <- match_stats %>%
  gather(key = "group", value = "value", Player1:Player4) %>%
  select(group, value, GameID, Game, Hitting_1:Hitting_4, Serving_1:Serving_4, Defense_1:Defense_4, Cleanliness_1:Cleanliness_4) %>%
  arrange(GameID, Game)


df_long$Hitting <- 0
df_long$Serving <- 0
df_long$Defense <- 0
df_long$Cleanliness <- 0

row <- 1
for (row in 1:nrow(df_long)){
  number = as.numeric(str_sub(df_long$group[row],-1,-1))
  df_long$Hitting[row] <- as.numeric(df_long[row,4+number])
  df_long$Serving[row] = as.numeric(df_long[row, 8+number])
  df_long$Defense[row] = as.numeric(df_long[row, 12+number])
  df_long$Cleanliness[row] = as.numeric(df_long[row, 16+number])
}

df_long$Hitting <- df_long$Hitting/max(df_long$Hitting)
df_long$Serving <- df_long$Serving/max(df_long$Serving)
df_long$Defense <- df_long$Defense/max(df_long$Defense)
df_long$Cleanliness <- df_long$Cleanliness/max(df_long$Cleanliness)

vis <- df_long[1:4,c(1, 21:24)]
match_stats$Tournament

ggradar(vis)


# Scatterplot Rallies vs Aces ---------------------------------------------

scatter <- ggplot(match_stats, aes(x = Rallies_percentage, y = Aces_percentage,
                                   color = Category, size = point, 
                                   text = paste("Tournament: ", Tournament))) +
  geom_point() +
  labs(title = 'Aces percentage vs Rallies percentage plot',
       subtitle = 'Upper right corner presents the most interesting matches',
       x = 'Rallies %' , y = 'Aces %')  + 
  theme_bw() 
ggplotly(scatter)


# Funnel chart ------------------------------------------------------------

funnel_data <- df %>%
  filter(GameID == "1", Game == "2", Break_team != 0, Break_team == "34") %>%
  group_by(Break_team, Break_reason) %>% summarize(occurrence_count = n())

  fig <- plot_ly(
    type = "funnelarea",
    textinfo = c("value", "text"),
    text = as.vector(t(funnel_data$Break_reason)),
    values = as.vector(t(funnel_data$occurrence_count)))
  
  fig
