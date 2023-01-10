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

# Data manipulation -------------------------------------------------------

# _Basic columns ----------------------------------------------------------

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
  mutate(Serve_made_1 = ifelse(str_detect(str_sub(Action, -3,-1), "^1[^D]*$"), 1, 0))
df <- df %>%
  mutate(Serve_made_2 = ifelse(str_detect(str_sub(Action, -3,-1), "^2[^D]*$"), 1, 0))
df <- df %>%
  mutate(Serve_made_3 = ifelse(str_detect(str_sub(Action, -3,-1), "^3[^D]*$"), 1, 0))
df <- df %>%
  mutate(Serve_made_4 = ifelse(str_detect(str_sub(Action, -3,-1), "^4[^D]*$"), 1, 0))

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
  if(str_sub(input,-1,-1) == "W" && nchar(input)>3) {
    # Case of put away after defensive touch - populating both DTNR and DTR
    DTR[as.numeric(str_sub(input, -2,-2))] = 1
    loc_input <- substr(input, 3, nchar(input)-2)
    for (letter in 1:nchar(loc_input)){
      DTR[as.numeric(substring(loc_input, letter, letter))] = 
        DTR[as.numeric(substring(loc_input, letter, letter))] + 1
    }
  }
  else {
    if(str_sub(input, -1, -1) %in%  c("L","E") && nchar(input)>3) {
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



t <- df[,c(10,42:53)]






# Iterate through the rows of the data frame
for (i in 1:nrow(df2)) {
  # Access data for each column by column name
  a <- toString(df2[i, "Action"])
  for (char in strsplit(a, "")[[1]]) {
    print(char)
  }
  
  # Calculate the new value based on the data in the row
  new_value <- letter
  # Append the new value to the vector
  new_values <- c(new_values, new_value)
}




# Aggregating -------------------------------------------------------------




# create a dataframe with a column of numbers and a grouping column
df <- data.frame(x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), group = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3))


# load the data.table package
library(data.table)

# convert the dataframe to a data.table
dt <- data.table(df)

# calculate the running sum for each group
dt[, running_sum := cumsum(x), by = group]

# view the data.table
dt

calc_touches_def("414L")
