library(dplyr)
library(ggplot2)
library(shiny)
library(DBI)
library(RPostgreSQL)
library(qcc)
library(DT)
library(shinydashboard)

#read the database settings

drv <- dbDriver("PostgreSQL")

host = 'localhost'  # Set the hostname or IP address
port = '15432'  # Set the port the db server is listing at
dbname = 'db'  # Define which databse should be used
user = 'username'  # Set the username
pwd = 'password'  # Set password


flights.db <- dbConnect(drv,
                        host = host,
                        port = port,
                        dbname = dbname, 
                        user = user, 
                        password = pwd)


flights.db <- src_postgres(dbname =dbname,
                           host = host,
                           port = port, 
                           user = user,
                           password = pwd)

flights <- tbl(flights.db, "connections")


# create data frame for the needed infomation        
flights.df <- flights %>%
  select(Month, UniqueCarrier, Origin, DepDelayMinutes) %>%
  filter(UniqueCarrier == "B6") %>%
  filter(DepDelayMinutes > 0) %>%
  collect (n = Inf)

flights.df <- na.omit(flights.df)
flights.df$Month <- as.character(flights.df$Month)




 
