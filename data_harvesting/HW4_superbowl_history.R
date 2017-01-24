library(XML)
library(dplyr) # Data manipulation library
library(lubridate) # For data manipulation
library(stringi) # Provides a host of string opperations

# load the website
nfl.url <- "http://www.espn.com/nfl/superbowl/history/winners"
nfl.table <- readHTMLTable(nfl.url, header= TRUE, StringsAsFactor = FALSE)
head(nfl.table)

# convert it to data frame
df.nfl <- data.frame(nfl.table)


# remove first 2 columns
df.nfl <- df.nfl[-(1:2),]

# change names of columns
names(df.nfl) <- c("superbowl", "date", "location", "result")


# we want roman numbers in superbowl number
df.nfl$superbowl <- 1:49

# convert date
df.nfl$date <- as.Date(df.nfl$date, "%B. %d, %Y")

# split result column into winner and loser
df.nfl <- separate(df.nfl, result, c('winner', 'loser'), sep = ',', remove = TRUE)

# extract score from the winner and loser column and convert it to numberical
df.nfl$winnerScore <- stri_sub(df.nfl$winner, -2,-1)
df.nfl$loserScore <- stri_sub(df.nfl$loser, -2,-1)

# remove remaining numbers from winner and loser column
df.nfl$winner <- gsub(" \\d+$", " ", df.nfl$winner)
df.nfl$loser <- gsub(" \\d+$", " ", df.nfl$loser)

# check variables
str(df.nfl)

#set correct variables
df.nfl$superbowl <- as.numeric(df.nfl$superbowl)
df.nfl$date <- as.Date(df.nfl$date)
df.nfl$location <- as.character(df.nfl$location)
df.nfl$winner <- as.character(df.nfl$winner)
df.nfl$loser <- as.character(df.nfl$loser)
df.nfl$winnerScore <- as.numeric(df.nfl$winnerScore)
df.nfl$loserScore <- as.numeric(df.nfl$loserScore)

