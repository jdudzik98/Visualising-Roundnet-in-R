
setwd("C:/Users/jdudzik/Documents/Studia/Adv Vis in R/Visualising-Roundnet-in-R")
library(ggradar)
library(dplyr)
library(scales)
library(tibble)
library(tidyr)
#install.packages("plotly")
library(plotly)
library(ggplot2)
df_long <- match_stats %>%
  gather(key = "group", value = "value", Player1:Player4) %>%
  select(group, value, GameID, Game, Hitting_1:Hitting_4, Serving_1:Serving_4, Defense_1:Defense_4, Cleanliness_1:Cleanliness_4) %>%
  arrange(GameID, Game)


df_long$Hitting <- 0
df_long$Serving <- 0
df_long$Defense <- 0
df_long$Cleanliness <- 0


for (row in 1:nrow(df_long)){
  number = as.numeric(str_sub(df_long$group[row],-1,-1))
  df_long$Hitting[row] <- as.numeric(df_long[row,4+number])
  df_long$Serving[row] = as.numeric(df_long[row, 8+number])
  df_long$Defense[row] = as.numeric(df_long[row, 12+number])
  df_long$Cleanliness[row] = as.numeric(df_long[row, 16+number])
}

radar <- df_long[df_long$GameID == 1 & df_long$Game == 1,c(1, 21:24)]
radar$Hitting <- radar$Hitting/max(radar$Hitting)
radar$Serving <- radar$Serving/max(radar$Serving)
radar$Defense <- radar$Defense/max(radar$Defense)
radar$Cleanliness <- radar$Cleanliness/max(radar$Cleanliness)



ggradar(radar)

df_longer <- df_long %>% 
  gather(key = "Stat", value = "RPR_Score", Hitting, Serving, Defense, Cleanliness) %>%
  select(GameID, Game, group, value, Stat, RPR_Score)


# Scatterplot Rallies vs Aces ---------------------------------------------

scatter <- ggplot(match_stats, aes(x = Rallies_percentage, y = Aces_percentage,
                                   color = Category, size = point)) +
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

# Line plot ---------------------------------------------------------------

  ggplot(filter(Data, GameID == "4" & Game == "1"))+
    geom_line(aes(y=score12, x = point, color = "Team 12"))+
    geom_line(aes(y=score34, x = point, color = "Team 34"))+
    geom_point(aes(y=score12, x = point, color = "Team 12"))+
    geom_point(aes(y=score34, x = point, color = "Team 34"))+
    #scale_color_manual(name = "Team", values = c("Team 12" = "red", "Team 34" = "blue"))+
    labs(title = 'Points achieved over time', x = 'Point', y = 'Scores')+
    theme_gray()


# Density RPRs ------------------------------------------------------------

    
  ggplot(data = df_long, aes(x = Hitting)) +
    geom_density(color = 'black', fill = 'indianred', alpha = .4)
  
  ggplot(data = df_longer, aes(y = RPR_Score, x = Stat)) +
    geom_boxplot() +
    coord_flip()
  # But we can add some jitter:
  ggplot(data = df_longer, aes(y = RPR_Score, x = Stat)) +
    geom_boxplot() +
    geom_jitter(width = .3)+
    geom_jitter(data = labs, aes(x = Stat, y = RPR_Score, color = group), size = 3)
    geom_point(data = labs, aes(x = Stat, y = RPR_Score, color = group), size = 3)
  
  
    geom_point(data = labs[labs$group == "Player1",], aes(x = Stat, y = RPR_Score), color = "Red")+
    geom_point(data = labs[labs$group == "Player2",], aes(x = Stat, y = RPR_Score), color = "Blue")+
    geom_point(data = labs[labs$group == "Player3",], aes(x = Stat, y = RPR_Score), color = "Green")+
    geom_point(data = labs[labs$group == "Player4",], aes(x = Stat, y = RPR_Score), color = "Yellow")
  
  ggplot(data = df_longer, aes(y = RPR_Score, x = Stat)) +
    geom_violin()+
    geom_point(data = df_longer[df_longer$GameID == 1 & df_longer$Game == 1,], aes(x = Stat, y = RPR_Score, color = group), size = 3)
  

# Stacked Bar chart -------------------------------------------------------

df$ID <- paste(df$GameID, "_", df$Game)
df$Crossings <- as.character(df$Crossings)

stacked <- df %>% 
  group_by(ID, Crossings) %>% 
  summarize(count = n()) %>%
  arrange(Crossings, ID)

glimpse(stacked)

  
Stacked_bc <- ggplot(stacked, aes(x = factor(ID), y = count, fill = factor(Crossings)))+
  geom_bar(position = "fill", stat = "identity")+
  coord_flip()
Stacked_bc

# Yet another challenge is the stacked bar plot:
ggplot(data = rs4, aes(x = factor(Age), y = number, fill = factor(Gender))) +
  geom_bar(na.rm = TRUE, position = "stack", width = 0.5, stat = "identity") +
  ggtitle('Gender and age in the sample') +
  xlab('Age') +
  ylab('Population') +
  labs(fill = 'Gender') +
  theme_minimal() +
  scale_fill_manual(values = c('grey78', 'khaki'))


fig <- plot_ly(data, x = ~x1, y = ~y, type = 'bar', orientation = 'h',
               marker = list(color = 'rgba(38, 24, 74, 0.8)',
                             line = list(color = 'rgb(248, 248, 249)', width = 1))) 
