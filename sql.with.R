install.packages("RODBC")
install.packages("bigrquery")

library(RODBC)
library(RPostgreSQL)


myconn <- odbcConnect("Database_Name", uid="User_ID", pwd="password")
dat <- sqlFetch(myconn, "Table_Name")
querydat <- sqlQuery(myconn, "SELECT * FROM table")
close(myconn)