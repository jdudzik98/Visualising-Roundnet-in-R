
setwd("C:/Users/jdudzik/Documents/Studia/Adv Vis in R/Visualising-Roundnet-in-R")
#devtools::install_github("ricardo-bion/ggradar", 
#                         dependencies = TRUE)
library(ggradar)
library(dplyr)
library(scales)
library(tibble)
library(tidyr)
#install.packages("plotly")
library(plotly)
library(ggplot2)
library(wesanderson)
library(gridExtra)

# data manipulations ------------------------------------------------------

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


radar <- df_long[df_long$GameID == 3,c(1, 21:24)]
radar$Hitting <- radar$Hitting/max(radar$Hitting)
radar$Serving <- radar$Serving/max(radar$Serving)
radar$Defense <- radar$Defense/max(radar$Defense)
radar$Cleanliness <- radar$Cleanliness/max(radar$Cleanliness)
radar <- replace(radar, radar<0, 0)



df_longer <- df_long %>% 
  gather(key = "Stat", value = "RPR_Score", Hitting, Serving, Defense, Cleanliness) %>%
  select(GameID, Game, group, value, Stat, RPR_Score)

funnel_data34 <- df %>%
  filter(GameID == "1", Break_team != 0, Break_team == "34") %>%
  group_by(Break_team, Break_reason) %>% summarize(occurrence_count = n())

funnel_data12 <- df %>%
  filter(GameID == "1", Break_team != 0, Break_team == "12") %>%
  group_by(Break_team, Break_reason) %>% summarize(occurrence_count = n())


df$ID <- paste(df$GameID, "_", df$Game)
df$Crossings2 <- df$Crossings
df$Crossings2 <- as.character(df$Crossings2)
df[df$Crossings >= 3,63] <- "3+"


stacked <- df %>% 
  group_by(GameID, Crossings2) %>% 
  summarize(count = n()) %>%
  arrange(Crossings2, GameID)


# Scatterplot Rallies vs Aces ---------------------------------------------

scatter <- ggplot(match_stats, aes(x = Rallies_percentage, y = Aces_percentage,
                                   color = Category)) +
  geom_point(aes(text = paste("Match ID: ", match_stats$GameID, 
                              "<br>Rallies: ", match_stats$Rallies_percentage, 
                              "<br>Aces: ", match_stats$Aces_percentage, 
                              "<br>Category: ", match_stats$Category)), size = 2)+
  scale_color_manual(values = wes_palette("Darjeeling1")) +
  labs(title = 'Highlight matches plot',
       subtitle = 'Points on the right side represent defensively-rich matches while those on the top show high serve pressure games',
       x = 'Rallies %' , y = 'Aces %')  + 
  theme_bw() 
ggplotly(scatter) %>%
  layout(title = list(text = paste0('Highlight matches plot',
                                    '<br>',
                                    '<sup>',
                                    'Points on the right side represent defensively-rich matches whileD those on the top show high serve pressure games',
                                    '</sup>')))


# Scatterplot avg crossings vs succesful serves ---------------------------------------------

scatter2 <- ggplot(match_stats, aes(x = Avg_crossings, y = succesful_serves_percentage,
                                   color = Category)) +
  geom_point(aes(text = paste("Match ID: ", match_stats$GameID, 
                              "<br>Possessions changes per point: ", match_stats$Avg_crossings, 
                              "<br>Succesful serve percentage ", match_stats$succesful_serves_percentage, 
                              "<br>Category: ", match_stats$Category)), size = 2)+
  labs(title = 'Match attractiveness plot',
       subtitle = 'Plots in the right upper corner tend to contain more ball exchanges and less serve faults',
       x = 'Possessions changes per point' , y = 'Successful serves Ratio in %')+
  scale_color_manual(values = wes_palette("Darjeeling1"))
ggplotly(scatter2) %>%
  layout(title = list(text = paste0('Match attractiveness plot',
                                    '<br>',
                                    '<sup>',
                                    'Plots in the right upper corner tend to contain more ball exchanges and less serve faults',
                                    '</sup>')))

# Stacked Bar chart -------------------------------------------------------


Stacked_bc <-
  ggplot(stacked, aes(x = factor(GameID), y = count, fill = reorder(factor(Crossings2), desc(factor(Crossings2)))))+
  geom_bar(position = "fill", stat = "identity")+
  coord_flip() +
  scale_fill_manual(values = wes_palette("GrandBudapest1"), name = "Net crossings")+
  guides(fill = guide_legend(reverse = TRUE))+
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), labels = scales::percent, sec.axis = sec_axis(~ ., breaks = seq(0, 1, by = 0.25), labels = scales::percent))+
  labs(title = 'Net crossings per match ', subtitle = 'Games with many Rallies (3+ changes of possession) tend to be more atractive to watch, while games with huge 
       share of 0 values are less attractive (Double Faults)', x = 'Match', y = 'Bins distribution')+
  theme(legend.position = "right")


Stacked_bc

# Line plot ---------------------------------------------------------------

line_plot <- 
  ggplot(filter(cs_df, GameID == "4" & Game == "1"))+
  geom_line(aes(y=score12, x = point, color = "Team 12"))+
  geom_line(aes(y=score34, x = point, color = "Team 34"))+
  geom_point(aes(y=score12, x = point, color = "Team 12"))+
  geom_point(aes(y=score34, x = point, color = "Team 34"))+
  #scale_color_manual(name = "Team", values = c("Team 12" = "red", "Team 34" = "blue"))+
  labs(title = 'Points achieved over time', x = 'Point', y = 'Scores')+
  theme_gray()+
  scale_color_manual(values = wes_palette("Darjeeling1"))

# Funnel chart ------------------------------------------------------------

funnel12 <- plot_ly(
  type = "funnelarea",
  textinfo = c("value", "text"),
  text = as.vector(t(funnel_data12$Break_reason)),
  marker = list(colors = c("#F1BB7B","#FD6467","#5B1A18")),
  values = as.vector(t(funnel_data12$occurrence_count)))

funnel12


funnel34 <- plot_ly(
    type = "funnelarea",
    textinfo = c("value", "text"),
    text = as.vector(t(funnel_data34$Break_reason)),
    marker = list(colors = c("#F1BB7B","#FD6467","#5B1A18")),
    values = as.vector(t(funnel_data34$occurrence_count)))
  
funnel34



# Breaks Line plot --------------------------------------------------------
  cs_df$Break_reason <- factor(df$Break_reason)
  cs_df$Break_team <- df$Break_team
ggplot(filter(cs_df, GameID == "4" & Game == "1"))+
  geom_line(aes(y=score12, x = point, color = "Team 12"))+
  geom_line(aes(y=score34, x = point, color = "Team 34"))+
  geom_bar(cs_df$bre)
cs_df$Break_reason
cs_df$Break_team
break_line <- 
ggplot(stacked, aes(x = factor(GameID), y = count, fill = reorder(factor(Crossings2), desc(factor(Crossings2)))))+
  geom_bar(position = "fill", stat = "identity")

# Overall RPR over the match -------------------------------------------------------------------
palette <- wes_palette("Darjeeling1")[c(2,5,3,4)]

ggplot(filter(cs_df, GameID == "1"))+
  geom_line(aes(y = RPR_1, x = point, color = "Player 1"), size = 3)+
  geom_line(aes(y = RPR_2, x = point, color = "Player 2"), size = 3)+
  geom_line(aes(y = RPR_3, x = point, color = "Player 3"), size = 3)+
  geom_line(aes(y = RPR_4, x = point, color = "Player 4"), size = 3)+
  scale_y_continuous(breaks=seq(0,150,10), sec.axis = dup_axis())+
  scale_color_manual(name = "Player", 
                     values = c("Player 1" = palette[1], "Player 2" = palette[2], "Player 3" = palette[3], "Player 4" = palette[4]))+
  labs(title = 'RPR Overall Scores across game',
       subtitle = '90 is an average score and usually overall scores are in range 75(poor game)-100(great game)',
       x = 'point number' , y = 'Overall RPR')




+
  scale_color_manual(name='Regression Model',
                     breaks=c('Linear', 'Quadratic', 'Cubic', 'ttt'),
                     values=c('Cubic'='pink', 'Quadratic'='blue', 'Linear'='purple', 'aa' = 'red'))+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=14))

# Density RPRs ------------------------------------------------------------

  
  violin <-  
  ggplot(data = df_longer, aes(y = RPR_Score, x = Stat)) +
    geom_violin()+
    geom_point(data = df_longer[df_longer$GameID == 1 & df_longer$Game == 1,], aes(x = Stat, y = RPR_Score, color = group), size = 3) +
    labs(title = 'RPR Metrics distributions', x = 'Metric', y = 'Value')+
    scale_color_manual(values = wes_palette("Darjeeling1"))
  


# radar -------------------------------------------------------------------


radar <- ggradar(radar, group.colours = palette)



# test --------------------------------------------------------------------

grid.arrange(arrangeGrob(scatter, scatter2, Stacked_bc),
             nrow = 2, # number of rows for the grid.arrange function
             top = "Example title",  # title
             heights = c(10, 1) # relative height of two rows
) 

# Now let's put it inside the `multiplot()` function as a value assigned to the `layout` parameter:
multiplot(scatter, scatter2, Stacked_bc, layout = design)




