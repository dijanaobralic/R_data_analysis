###################################### Create Matrix ################################################
goog <- c(450, 451,452,445,468)
msft <- c(230,231,232,233,220)

stocks <- c(goog,msft)
stocks

stock.matrix <- matrix(stocks, byrow = T, nrow = 2)
stock.matrix

days <- c('Mon', 'Tue', 'Wed', 'Thu','Fri')
stock.names <- c("Google", "Microsoft")

colnames(stock.matrix) <- days
rownames(stock.matrix) <- stock.names

colSums(stock.matrix)
rowMeans(stock.matrix)

# add a vector to the matrix
fb <- c(323,324,235,236,254)

tech.stocks <- rbind(stock.matrix,fb)

avg <- rowMeans(tech.stocks)
tech.stocks <- cbind(stock.matrix,avg)

###################################### Create Matrix 2 ################################################
mat <- matrix(1:25, byrow = T, nrow = 5)
# you can do arithrmics with matrices
mat*2
mat > 2
# return a vector of all the values that are larget than 15
mat[mat>15]

mat %*% mat


###################################### Create Matrix 3 ################################################
mat2 <- matrix(1:50, byrow = T, nrow = 5)
mat2[1,]
mat2[,1]
mat2[1:3,]
mat2[1:2,1:3]
