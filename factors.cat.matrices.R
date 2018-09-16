###################################### Factors and categorical matrices ################################################
# Why do we need them?
# factor function useful for machine learning, creation of dummy variables

animal <- c('d','c','d','c','c')
id <- c(1,2,3,4,5)

# with factor we can check levels or number of different variables
fac.animal <- factor(animal)

# Types of factors
# Nominal --- no order (example factor(animal))
# Ordinal --- order

temp <- c('cold', 'med', 'hot', 'hot', 'hot', 'hot', 'med', 'cold', 'cold', 'cold', 'cold', 'cold')
fac.temp <- factor(temp, ordered = T, levels = c('cold', 'med', 'hot'))

summary(fac.temp)
summary(temp)

###################################### Exercise ################################################

A <- c(1,2,3)
B <- c(4,5,6)
ab.matrix <- rbind(A,B)

matrix2 <- matrix(1:9, byrow = TRUE, nrow=3)
#check whether something is matrix
is.matrix(matrix2)

mat2 <- matrix(1:25, byrow = TRUE, nrow=5)

mat2[2,2:3]
mat2[3,2:3]
mat2[2:3,2:3]

#sum of matrix
sum(mat2)
#sum of columns
colSums(mat2)
rowSums(mat2)

mat3 <- runif(20, min=1, max=100)
is.matrix(mat3)
mat3 <- matrix(mat3, byrow = T, nrow = 4)
