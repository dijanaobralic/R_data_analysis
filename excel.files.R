write.csv(mtcars, file="mtcars.csv")

install.packages('readxl')
install.packages('xlsx')
install.packages("devtools")
install.packages("rJava")

devtools::install_github("AndreaCirilloAC/updateR")
available.packages()
library(readxl)
library(xlsx)
library(rJava)


# first you gotta call excel_sheets to get names of the sheets available
excel_sheets('Sample-Sales-Data.xlsx')
#call specific sheet
df <- read_excel('Sample-Sales-Data.xlsx', sheet = 'Sheet1')
head(df)

sum(df$Value)
summary(df$Value)

# get me a list of all sheet names THEN apply read excel to all sheets within that excel file with the path of this excel file
# if there were more than 1 sheet, it would get them listed as lists
entire.workbook <- lapply(excel_sheets('Sample-Sales-Data.xlsx'),read_excel,path='Sample-Sales-Data.xlsx')

# write to excel files
head(mtcars)

write.xlsx(mtcars,"output_example.xlsx")
