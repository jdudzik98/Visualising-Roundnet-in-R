# Intro  --------------------------------------------------------------------


# _packages and loading ---------------------------------------------------


#install.packages("stringr")
#install.packages("data.table")
#install.packages("magrittr")
library(readr)
df <- read_delim("Roundnet_dataset.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)

library(dplyr)
library(stringr)
library(magrittr)

# Data manipulation - calculate columns ----------------------------------------

### Generate putaway columns

# Add blank columns
df[c('putaway_1','putaway_2','putaway_3','putaway_4')] <- NA

# Assign values to columns
df <- df %>%
  mutate(putaway_1 = ifelse(str_sub(Action, -2,-1) == "1W", 1, 0))
df <- df %>%
  mutate(putaway_2 = ifelse(str_sub(Action, -2,-1) == "2W", 1, 0))
df <- df %>%
  mutate(putaway_3 = ifelse(str_sub(Action, -2,-1) == "3W", 1, 0))
df <- df %>%
  mutate(putaway_4 = ifelse(str_sub(Action, -2,-1) == "4W", 1, 0))


### Generate Aced columns

# Add blank columns
df[c('Aced_1','Aced_2','Aced_3','Aced_4')] <- NA

# Assign values to columns
df <- df %>%
  mutate(Aced_1 = ifelse(str_sub(Action, -2,-1) == "1A", 1, 0))
df <- df %>%
  mutate(Aced_2 = ifelse(str_sub(Action, -2,-1) == "2A", 1, 0))
df <- df %>%
  mutate(Aced_3 = ifelse(str_sub(Action, -2,-1) == "3A", 1, 0))
df <- df %>%
  mutate(Aced_4 = ifelse(str_sub(Action, -2,-1) == "4A", 1, 0))

### Generate Ace columns

# Add blank columns
df[c('Ace_1','Ace_2','Ace_3','Ace_4')] <- NA

# Assign values to columns
df <- df %>%
  mutate(Ace_1 = ifelse(str_detect(str_sub(Action, -3,-1), "1.A"), 1, 0))
df <- df %>%
  mutate(Ace_2 = ifelse(str_detect(str_sub(Action, -3,-1), "2.A"), 1, 0))
df <- df %>%
  mutate(Ace_3 = ifelse(str_detect(str_sub(Action, -3,-1), "3.A"), 1, 0))
df <- df %>%
  mutate(Ace_4 = ifelse(str_detect(str_sub(Action, -3,-1), "4.A"), 1, 0))

### Generate Serve_made columns

# Add blank columns
df[c('Serve_made_1','Serve_made_2','Serve_made_3','Serve_made_4')] <- NA




# Assign values to columns
df <- df %>%
  mutate(Serve_made_1 = ifelse(str_detect(str_sub(Action, 0, 3), "^1[^D]*$"), 1, 0))
df <- df %>%
  mutate(Serve_made_2 = ifelse(str_detect(str_sub(Action, 0, 3), "^2[^D]*$"), 1, 0))
df <- df %>%
  mutate(Serve_made_3 = ifelse(str_detect(str_sub(Action, 0, 3), "^3[^D]*$"), 1, 0))
df <- df %>%
  mutate(Serve_made_4 = ifelse(str_detect(str_sub(Action, 0, 3), "^4[^D]*$"), 1, 0))

### Generate Double_fault columns

# Add blank columns
df[c('Double_fault_1','Double_fault_2','Double_fault_3','Double_fault_4')] <- NA

# Assign values to columns
df <- df %>%
  mutate(Double_fault_1 = ifelse(str_sub(Action, -2,-1) == "1D", 1, 0))
df <- df %>%
  mutate(Double_fault_2 = ifelse(str_sub(Action, -2,-1) == "2D", 1, 0))
df <- df %>%
  mutate(Double_fault_3 = ifelse(str_sub(Action, -2,-1) == "3D", 1, 0))
df <- df %>%
  mutate(Double_fault_4 = ifelse(str_sub(Action, -2,-1) == "4D", 1, 0))

### Generate Error columns

# Add blank columns
df[c('Error_1','Error_2','Error_3','Error_4')] <- NA

# Assign values to columns
df <- df %>%
  mutate(Error_1 = ifelse(str_sub(Action, -2,-1) == "1E", 1, 0))
df <- df %>%
  mutate(Error_2 = ifelse(str_sub(Action, -2,-1) == "2E", 1, 0))
df <- df %>%
  mutate(Error_3 = ifelse(str_sub(Action, -2,-1) == "3E", 1, 0))
df <- df %>%
  mutate(Error_4 = ifelse(str_sub(Action, -2,-1) == "4E", 1, 0))


# Shorten the Actions by the defender (1343221W -> 132W)
shorten_action_def <- function(input) {
  result <- ""
  buffer <- ""
  prev <- ""
  for (i in 1:nchar(input)) {
    c <- substr(input, i, i)
    if (c %in% c("1", "2")) {
      if (prev %in% c("1", "2")) {
        buffer <- paste(buffer, c, sep="")
      } else {
        if (nchar(buffer) > 0) {
          result <- paste(result, substr(buffer, 1, 1), sep="")
          buffer <- ""
        }
        buffer <- paste(buffer, c, sep="")
      }
    } else if (c %in% c("3", "4")) {
      if (prev %in% c("3", "4")) {
        buffer <- paste(buffer, c, sep="")
      } else {
        if (nchar(buffer) > 0) {
          result <- paste(result, substr(buffer, 1, 1), sep="")
          buffer <- ""
        }
        buffer <- paste(buffer, c, sep="")
      }
    } else {
      if (nchar(buffer) > 0) {
        result <- paste(result, substr(buffer, 1, 1), sep="")
        buffer <- ""
      }
      result <- paste(result, c, sep="")
    }
    prev <- c
  }
  if (nchar(buffer) > 0) {
    result <- paste(result, substr(buffer, 1, 1), sep="")
  }
  return(result)
}

# Calculate Defensive Touches Returned and Defensive Touches Not Returned per player
calc_touches_def <- function(input) {
  DTR <- c(0,0,0,0)
  DTNR <- c(0,0,0,0)
  if(str_sub(input,-1,-1) %in%  c("W","E") && nchar(input)>3) {
    # Case of put away after defensive touch - populating both DTNR and DTR
    DTR[as.numeric(str_sub(input, -2,-2))] = 1
    loc_input <- substr(input, 3, nchar(input)-2)
    for (letter in 1:nchar(loc_input)){
      DTR[as.numeric(substring(loc_input, letter, letter))] = 
        DTR[as.numeric(substring(loc_input, letter, letter))] + 1
    }
  }
  else {
    if(str_sub(input, -1, -1) == "L" && nchar(input)>3) {
      # Case of Error/lost point after defensive touch - populating both DTR only
      DTNR[as.numeric(str_sub(input, -2,-2))] = 1
      loc_input <- substr(input, 3, nchar(input)-2)
      for (letter in 1:nchar(loc_input)){
        DTR[as.numeric(substring(loc_input, letter, letter))] = 
          DTR[as.numeric(substring(loc_input, letter, letter))] + 1
      }
    }
  }
return(c(DTR, DTNR))
}

# Shorten the Actions by the hitter (1343221W -> 131W)
shorten_action_off <- function(input) {
  result <- ""
  buffer <- ""
  prev <- ""
  for (i in 1:nchar(input)) {
    c <- substr(input, i, i)
    if (c %in% c("1", "2")) {
      if (prev %in% c("1", "2")) {
        buffer <- paste(buffer, c, sep="")
      } else {
        if (nchar(buffer) > 0) {
          result <- paste(result, substr(buffer, nchar(buffer), nchar(buffer)), sep="")
          buffer <- c
        } else {
          buffer <- paste(buffer, c, sep="")
        }
      }
    } else if (c %in% c("3", "4")) {
      if (prev %in% c("3", "4")) {
        buffer <- paste(buffer, c, sep="")
      } else {
        if (nchar(buffer) > 0) {
          result <- paste(result, substr(buffer, nchar(buffer), nchar(buffer)), sep="")
          buffer <- c
        } else {
          buffer <- paste(buffer, c, sep="")
        }
      }
    } else {
      if (nchar(buffer) > 0) {
        result <- paste(result, substr(buffer, nchar(buffer), nchar(buffer)), sep="")
        buffer <- ""
      }
      result <- paste(result, c, sep="")
    }
    prev <- c
  }
  if (nchar(buffer) > 0) {
    result <- paste(result, substr(buffer, nchar(buffer), nchar(buffer)), sep="")
  }
  return(result)
}

# Calculate Hits Returned per player

calc_touches_off <- function(input) {
  SR <- c(0,0,0,0)
  if(str_sub(input,-1,-1) == "W" && nchar(input)>3) {
    # Case of put away after defensive touch - removing 2 from back
    loc_input <- str_sub(input, 2, -3)
    for (letter in 1:nchar(loc_input)){
      SR[as.numeric(substring(loc_input, letter, letter))] = 
        SR[as.numeric(substring(loc_input, letter, letter))] + 1
    }
  }
  else {
    if(str_sub(input, -1, -1) %in%  c("L","E") && nchar(input)>4) {
      # Case of Error/lost point after defensive touch - removing 3 from back
      loc_input <- str_sub(input, 2, -4)
      for (letter in 1:nchar(loc_input)){
        SR[as.numeric(substring(loc_input, letter, letter))] = 
          SR[as.numeric(substring(loc_input, letter, letter))] + 1
      }
    }
  }
  return(SR)
}


# Create new columns to measure DTR, DTNR, SR per player

df[c('DTR_1','DTR_2','DTR_3','DTR_4')] <- NA
df[c('DTNR_1','DTNR_2','DTNR_3','DTNR_4')] <- NA
df[c('SR_1','SR_2','SR_3','SR_4')] <- NA

# Looping over the table calculating hit information
for (i in 1:nrow(df)){
  loc_d <- calc_touches_def(shorten_action_def(df$Action[i]))
  df$DTR_1[i] <- loc_d[1]
  df$DTR_2[i] <- loc_d[2]
  df$DTR_3[i] <- loc_d[3]
  df$DTR_4[i] <- loc_d[4]
  df$DTNR_1[i] <- loc_d[5]
  df$DTNR_2[i] <- loc_d[6]
  df$DTNR_3[i] <- loc_d[7]
  df$DTNR_4[i] <- loc_d[8]
  loc_o <- calc_touches_off(shorten_action_off(df$Action[i]))
  df$SR_1[i] <- loc_o[1]
  df$SR_2[i] <- loc_o[2]
  df$SR_3[i] <- loc_o[3]
  df$SR_4[i] <- loc_o[4]
}

# Calculating Total Spikes as sum of Spikes Returned (SR) + putaways
df$TS_1 <- df$SR_1 + df$putaway_1
df$TS_2 <- df$SR_2 + df$putaway_2
df$TS_3 <- df$SR_3 + df$putaway_3
df$TS_4 <- df$SR_4 + df$putaway_4









# Aggregating -------------------------------------------------------------

# Building a cumulative sum

Data <- df %>% 
  group_by(GameID, Game) %>% 
  mutate(putaway_1cs = cumsum(putaway_1),
         putaway_2cs = cumsum(putaway_2),
         putaway_3cs = cumsum(putaway_3),
         putaway_4cs = cumsum(putaway_4),
         Aced_1cs = cumsum(Aced_1),
         Aced_2cs = cumsum(Aced_2),
         Aced_3cs = cumsum(Aced_3),
         Aced_4cs = cumsum(Aced_4),
         Ace_1cs = cumsum(Ace_1),
         Ace_2cs = cumsum(Ace_2),
         Ace_3cs = cumsum(Ace_3),
         Ace_4cs = cumsum(Ace_4),
         Serve_made_1cs = cumsum(Serve_made_1),
         Serve_made_2cs = cumsum(Serve_made_2),
         Serve_made_3cs = cumsum(Serve_made_3),
         Serve_made_4cs = cumsum(Serve_made_4),
         Double_fault_1cs = cumsum(Double_fault_1),
         Double_fault_2cs = cumsum(Double_fault_2),
         Double_fault_3cs = cumsum(Double_fault_3),
         Double_fault_4cs = cumsum(Double_fault_4),
         Error_1cs = cumsum(Error_1),
         Error_2cs = cumsum(Error_2),
         Error_3cs = cumsum(Error_3),
         Error_4cs = cumsum(Error_4),
         DTR_1cs = cumsum(DTR_1),
         DTR_2cs = cumsum(DTR_2),
         DTR_3cs = cumsum(DTR_3),
         DTR_4cs = cumsum(DTR_4),
         DTNR_1cs = cumsum(DTNR_1),
         DTNR_2cs = cumsum(DTNR_2),
         DTNR_3cs = cumsum(DTNR_3),
         DTNR_4cs = cumsum(DTNR_4),
         SR_1cs = cumsum(SR_1),
         SR_2cs = cumsum(SR_2),
         SR_3cs = cumsum(SR_3),
         SR_4cs = cumsum(SR_4),
         TS_1cs = cumsum(TS_1),
         TS_2cs = cumsum(TS_2),
         TS_3cs = cumsum(TS_3),
         TS_4cs = cumsum(TS_4),
         )

#keeping only agregated stats

Data <- Data[,c(1:10,15,58:97)]

# Calculating stats -------------------------------------------------------

# Intrducing RPR Statistics, we will start with hitting. Roundnet Player Rating
# (RPR) statistics are calculated based on the document presented by Max Model
# (https://docs.google.com/document/d/14VjayAbhGYx1feoCXWjmilP3eZj_-WcoWcjk9LMJISw)

Data$Hitting_1 <- 20-20*coalesce(Data$SR_1cs/Data$TS_1cs, 0)
Data$Hitting_2 <- 20-20*coalesce(Data$SR_2cs/Data$TS_2cs, 0)
Data$Hitting_3 <- 20-20*coalesce(Data$SR_3cs/Data$TS_3cs, 0)
Data$Hitting_4 <- 20-20*coalesce(Data$SR_4cs/Data$TS_4cs, 0)

Data$Defense_1 <- Data$DTNR_1cs+0.4*Data$Hitting_1*Data$DTR_1cs
Data$Defense_2 <- Data$DTNR_2cs+0.4*Data$Hitting_2*Data$DTR_2cs
Data$Defense_3 <- Data$DTNR_3cs+0.4*Data$Hitting_3*Data$DTR_3cs
Data$Defense_4 <- Data$DTNR_4cs+0.4*Data$Hitting_4*Data$DTR_4cs

Data$Serving_1 <- 5.5*Data$Ace_1cs + 15*coalesce((Data$Serve_made_1cs/(Data$Serve_made_1cs+Data$Double_fault_1cs)),0)
Data$Serving_2 <- 5.5*Data$Ace_2cs + 15*coalesce((Data$Serve_made_2cs/(Data$Serve_made_2cs+Data$Double_fault_2cs)),0)
Data$Serving_3 <- 5.5*Data$Ace_3cs + 15*coalesce((Data$Serve_made_3cs/(Data$Serve_made_3cs+Data$Double_fault_3cs)),0)
Data$Serving_4 <- 5.5*Data$Ace_4cs + 15*coalesce((Data$Serve_made_4cs/(Data$Serve_made_4cs+Data$Double_fault_4cs)),0)

Data$Cleanliness_1 <- 20-5*(Data$Error_1cs) - 2*(Data$Aced_1cs)
Data$Cleanliness_2 <- 20-5*(Data$Error_2cs) - 2*(Data$Aced_2cs)
Data$Cleanliness_3 <- 20-5*(Data$Error_3cs) - 2*(Data$Aced_3cs)
Data$Cleanliness_4 <- 20-5*(Data$Error_4cs) - 2*(Data$Aced_4cs)


# Building Scoring --------------------------------------------------------


Data <- Data %>% group_by(GameID, Game) %>% mutate(point = row_number(), score12 = 0, score34 = 1)

for (row in 2:nrow(Data)){
  # Considering the case of an initial point
  if(Data$GameID[row]!= Data$GameID[row-1] || Data$Game[row]!= Data$Game[row-1]){
    if (Data$`Break?`[row] == 1){
      if(substr(Data$Action[row],1,1) %in% c("1","2",1,2)){
        Data$score12[row] = 1
        Data$score34[row] = 0
      }
      else{
        Data$score34[row] = 1
        Data$score12[row] = 0
      }
      #Data$score12[row] = Data$score12[row-1]+1
    }
    # Considering the case of point taken by the receiving team
    else{
      if(substr(Data$Action[row],1,1) %in% c("1","2",1,2)){
        
        Data$score34[row] = 1
        Data$score12[row] = 0
      }
      else{
        Data$score12[row] = 1
        Data$score34[row] = 0
      }
    
    }
  }
  # Considering the case of a break (point received by the serving team)
  else if (Data$`Break?`[row] == 1){
    if(substr(Data$Action[row],1,1) %in% c("1","2",1,2)){
      Data$score12[row] = Data$score12[row-1]+1
      Data$score34[row] = Data$score34[row-1]
    }
    else{
      Data$score34[row] = Data$score34[row-1]+1
      Data$score12[row] = Data$score12[row-1]
    }
    #Data$score12[row] = Data$score12[row-1]+1
  }
  # Considering the case of point taken by the receiving team
  else{
    if(substr(Data$Action[row],1,1) %in% c("1","2",1,2)){
      
      Data$score34[row] = Data$score34[row-1]+1
      Data$score12[row] = Data$score12[row-1]
    }
    else{
      Data$score12[row] = Data$score12[row-1]+1
      Data$score34[row] = Data$score34[row-1]
    }
  }
}

match_stats <- Data %>%
  group_by(GameID, Game) %>%
  slice_tail(n = 1)