######################################### data frame part 1 ##################################
days <- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri')
temp <- c(22.2, 21, 23, 24.3, 25)
rain <- c(T, T, F, F, T)

# combine vectors in a data frame
df <- data.frame(days, temp, rain)
str(df)

#select just first row
df[1,]
df[,1]

df[,"rain"]
df[1:5,  c("days", "temp")]
df$days

# when using square bracets it returns a data frame
# $ returns a vector

subset(df, subset = rain == TRUE)
subset(df, subset = rain ==TRUE & temp >23)

sorted.temp <- order(df['temp'])
df[sorted.temp,] 

######################################### data frame part 2 ##################################

empty <- data.frame()
c1 <- c(1:10)
c2 <- letters[1:10]
df <- data.frame(col.name.1 = c1, col.name.2 = c2)
df <- data.frame(c1, c2)

df[[2,'c1']] <- 10000
df


nrow(df)
ncol(df)
colnames(df)
rownames(df)
str(df)
summary(df)
######################################### data frame part 3 ######################################
# different ways to select a column - return VECTOR
mtcars$mpg
mtcars[,'mpg']
mtcars[,1]
mtcars[['mpg']]

# return DATA FRAME
mtcars['mpg']
mtcars[c('mpg','cyl')] 


df2 <- data.frame(c1=2000, c2 = 'new')
df.new <- rbind(df,df2)

df$newcol <- 2*df$c1
df$newcol.copy <- df$newcol

colnames(df)

# change column names
colnames(df) <- c( "1", "2",'3','4')

# select forst 7 rows
head(df,7)
#select everything but 2nd row
df[-2,]

# don't forget , --> it means select everything that fits in the condition stated
mtcars
mtcars[(mtcars$mpg >20) & (mtcars$cyl ==6) ,]
mtcars[(mtcars$mpg >20) & (mtcars$cyl ==6) , c('mpg','cyl','hp')]

mtcars

# you can also use subset 
subset(mtcars, mpg > 20 & cyl == 6)

######################################### dealing with missing data ####################################
# check if there is null or na data
is.na(mtcars)

# nicer way 
any(is.na(mtcars))
mtcars[is.na(mtcars)] <- 0
# repace missing data with the averga of all values in that column
mtcars$mpg[is.na(mtcars)] <- mean(mtcars$mpg)


###################################### EXERCISE ##################################################

Age <- c(22,25,26)
Weight <- c(150,165,120)
Sex <- c('M','M','F') 
Names<- c('Sam','Frank','Amy')

df <- data.frame(row.names = Names,Age,Weight,Sex)

is.data.frame(mtcars)


mat <- matrix(1:25,nrow = 5)
is.data.frame(mat)
mat <- as.data.frame(mat)

df<- mtcars

head(mtcars,6)
mean(mtcars$mpg)

subset(mtcars,cyl == 6)
df[c("mpg","cyl",'carb')]
df$performance <- NA
df$performance <- df$hp/df$wt

mean((subset(df,hp >100 & wt >2.5))$mpg)
df['Hornet Sportabout',]$mpg


################################################# lists ########################################

v <- c(1,2,3)
m <- matrix(1:10, nrow=2)
df <- mtcars
class(v)
class(m)
class(df)
# if you wanna put everything togethet
m.list <- list(v,m,df)
# numberic order of lists is showin in double square brackets
my.name.list <- list(sample.vec = v, my.matrix = m, sample.df = df)

my.name.list$my.matrix

m.list[1]
my.name.list['sample.vec']
# square brackets will return a vector 
class(my.name.list['sample.vec'])
#$ sign will return vector
my.name.list$sample.vec

# you can combine lists
double.list <- c(my.name.list, my.name.list)
str(my.name.list)

