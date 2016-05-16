library(dplyr)
library(ggplot2)

setwd("/Users/asuarez/Desktop/rprog-data-ProgAssignment3-data")

####################################################################################################
########################### 1. Plot 30-day data for mortality rates ################################
####################################################################################################

# Reading the files into variables

filename = "outcome-of-care-measures.csv"
outcome = read.csv(filename, header=TRUE, stringsAsFactors=FALSE, 
                   colClasses="character")
head(outcome)


# Exploring the data frame
dim(outcome)
colnames(outcome)


# Histogram
outcome[,11] = as.numeric(outcome[,11])
hist(outcome[,11])



####################################################################################################
########################### 2. Find the best hospital in the state #################################
####################################################################################################

# Putting it all into a function
best = function(state, outcome){
      
      # Reads the outcome data
      filename = "outcome-of-care-measures.csv"
      df = read.csv(filename, header=TRUE, stringsAsFactors=FALSE, 
                         colClasses="character")
      
      
      if(sum(state==unique(df$State))==0){
            stop("invalid state")
      }     # validates the 2-char state name given matches at least 1 state
      
      
      df = subset(df, State==state)
      if(outcome=="heart attack") {df = df[,c(2,11)]}
      else if(outcome=="heart failure") {df = df[,c(2,17)]}
      else if(outcome=="pneumonia") {df = df[,c(2,23)]}
      else {stop("invalid outcome")}
      # now 'df' will only contain the desired outcome for the hospitals in the queried state
      
      df[,2] = as.numeric(df[,2]) # converts the desired outcome to numeric
      colnames(df) = c("name", "outcome")
      
      df = arrange(df, outcome, name)
      
      
      return(df$name[1])
}


####################################################################################################
########################### 2. Find the best hospital in the state #################################
####################################################################################################


# Putting it all into a function
best = function(state, outcome){
      
      options(warn=-1) # avoids seeing warning messages
      
      # Reads the outcome data
      df = read.csv("outcome-of-care-measures.csv", header=TRUE, 
                    stringsAsFactors=FALSE, colClasses="character")
      
      
      if(sum(state==unique(df$State))==0){
            stop("invalid state")
      }     # validates the 2-char state name given matches at least 1 state
      
      
      df = subset(df, State==state)
      if(outcome=="heart attack") {df = df[,c(2,11)]}
      else if(outcome=="heart failure") {df = df[,c(2,17)]}
      else if(outcome=="pneumonia") {df = df[,c(2,23)]}
      else {stop("invalid outcome")}
      # now 'df' will only contain the desired outcome for the hospitals in the queried state
      
      
      df[,2] = as.numeric(df[,2]) # converts the desired outcome to numeric
      colnames(df) = c("name", "outcome")
      
      
      df = arrange(df, outcome, name) # sorts the hospitals by outcome, then their name
      
      
      return(df$name[1])
}


####################################################################################################
######################### 3. Ranking hospitals by outcome in a state ###############################
####################################################################################################


rankhospital = function(state, outcome, num="best") {
      
      options(warn=-1) # avoids seeing warning messages
      
      # Reads the outcome data
      df = read.csv("outcome-of-care-measures.csv", header=TRUE, 
                    stringsAsFactors=FALSE, colClasses="character")
      
      
      if(sum(state==unique(df$State))==0){
            stop("invalid state")
      }     # validates the 2-char state name given matches at least 1 state
      
      
      df = subset(df, State==state)
      if(outcome=="heart attack") {df = df[,c(2,11)]}
      else if(outcome=="heart failure") {df = df[,c(2,17)]}
      else if(outcome=="pneumonia") {df = df[,c(2,23)]}
      else {stop("invalid outcome")}
      # now 'df' will only contain the desired outcome for the hospitals in the queried state
      
      
      df[,2] = as.numeric(df[,2]) # converts the desired outcome to numeric
      colnames(df) = c("name", "outcome")
      df = na.exclude(df)
      
      
      df = arrange(df, outcome, name) # sorts the hospitals by outcome, then their name
      df = mutate(df, Rank=rownames(df))
      
      
      if(class(num)=="numeric" & num>length(df$name)) {return(NA)}
      else if(num=="worst") {return(tail(df, n=1L)$name)}
      else if(num=="best") {return(df[1, 1])}
      else {return(df[num, 1])}
      
}


####################################################################################################
############################ 4. Ranking hospitals in all states ###################################
####################################################################################################

rankall = function(outcome, num="best") {
      
      options(warn=-1) # avoids seeing warning messages
      
      
      # Reads the outcome data
      df = read.csv("outcome-of-care-measures.csv", header=TRUE, 
                    stringsAsFactors=FALSE, colClasses="character")
      
      
      if(class(num)=="numeric" & num>length(df$State)) {stop(NA)}
      # validates num length
      
      
      if(outcome=="heart attack") {df = df[,c(2,7,11)]}
      else if(outcome=="heart failure") {df = df[,c(2,7,17)]}
      else if(outcome=="pneumonia") {df = df[,c(2,7,23)]}
      else {stop("invalid outcome")}
      # now 'df' will only contain the desired outcome for the hospitals in the queried state
      
      
      df[,3] = as.numeric(df[,3]) # converts the desired outcome to numeric
      colnames(df) = c("name", "state", "outcome")
      
      
      if(num=="best") {
            df = arrange(df, state, outcome, name)
            df = aggregate(df, list(df$state), FUN=head, 1)
            df = select(df, name, state)
      }
      else if(num=="worst") {
            df = arrange(df, state, desc(outcome), name)
            df = aggregate(df, list(df$state), FUN=head, 1)
            df = select(df, name, state)
      }
      else {
            df = arrange(df, state, outcome, name)
            df = group_by(df, state)
            df = summarise(df, name = nth(name, num))
            df = select(df, name, state)
      }
      
      return(df)
}