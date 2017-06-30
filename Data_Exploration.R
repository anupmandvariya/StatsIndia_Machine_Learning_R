### Release Memory
rm(list = ls())
gc()


##########################
### R Data Exploration ###
##########################
a <- 1:10
save(a, file="./data/SI_test.Rdata")
rm(a)
load("./data/SI_test.Rdata")
print(a)

############################
### CSV Data Exploration ###
############################
int_var <- c(1:3)
char_var <- c('M', 'F', 'F')
str_var <- c("StatsIndia", "Data Science", "Training")
real_var <- c(8.5, 9.2, 7.5)

data.df <- data.frame(int_var, char_var, str_var, real_var)
names(data.df) <- c("ID", "Gender", "Course", "Grade")

write.csv(data.df, "./data/SI_Students.csv", row.names = FALSE)
write.table(data.df, "./data/SI_Students.xls", row.names = FALSE)
data.df.read <- read.csv("./data/SI_Students.csv")
print(data.df.read)

############################
### SAS Data Exploration ###
############################

# library(foreign)
# # the path of SAS on your computer
# sashome <- "$PATH/SAS/SASFoundation/9.2" 
# # read data from a SAS dataset
# a <- read.ssd(fileName, sascmd=file.path(sashome, "sas.exe"))
# print(a)

############################
### DB Data Exploration ###
############################

## library(RODBC)
## connection <- odbcConnect(dsn="servername",uid="userid",pwd="******")
## query <- "SELECT * FROM tablename limit 100"

## # or read query from file
## # query <- readChar("data/myQuery.sql", nchars=99999)

## myData <- sqlQuery(connection, query, errors=TRUE)
## odbcClose(connection)


######################
### Data Structure ###
######################
str(iris)
dim(iris)
names(iris)
rownames(iris)
colnames(iris)
attributes(iris)
head(iris)
tail(iris)

######################
### Summarize Data ###
######################
summary(iris)
aggregate(. ~ Species, summary, data=iris)
aggregate(Sepal.Length ~ Species, summary, data=iris)

var(iris$Sepal.Length)
sd(iris$Sepal.Length)

cov(iris$Sepal.Length, iris$Petal.Length)
cov(iris[,1:4])
cor(iris$Sepal.Length, iris$Petal.Length)
cor(iris[,1:4])

quantile(iris$Sepal.Length)
quantile(iris$Sepal.Length, seq(0.1, 1, 0.1))

#######################
### Describe Data #####
#######################
library(Hmisc)
describe(iris)

#################################
### Plot Data -  2D Plot ########
#################################
with(iris, plot(Sepal.Length, Sepal.Width, col=Species, pch=as.numeric(Species)))
plot(jitter(iris$Sepal.Length), jitter(iris$Sepal.Width))
pairs(iris)

#################################
### Plot Data -  3D Plot ########
#################################
library(scatterplot3d)
scatterplot3d(iris$Petal.Width, iris$Sepal.Length, iris$Sepal.Width)

library(rgl) 
plot3d(iris$Petal.Width, iris$Sepal.Length, iris$Sepal.Width)

###############################
### Plot Data - Histogram #####
###############################
hist(iris$Sepal.Length)
hist(iris)

###############################
### Plot Data - Density #######
###############################
plot(density(iris$Sepal.Length))

#################################
### Plot Data - Pie Chart #######
#################################
pie(table(iris$Species))

#################################
### Plot Data - Barplot #######
#################################
barplot(table(iris$Species))


#################################
### Plot Data - Boxplot #######
#################################
boxplot(iris)
boxplot(iris$Sepal.Width)
boxplot(Sepal.Width ~ Species, data=iris)

#################################
### Plot Data - HeatMap #######
#################################
distMatrix <- as.matrix(dist(iris[,1:4]))
heatmap(distMatrix)

#################################
### Plot Data - Lattice #######
#################################
library(lattice)
levelplot(Petal.Width~Sepal.Length*Sepal.Width, iris, cuts=9,
          col.regions=grey.colors(10)[10:1])
parallelplot(~iris[1:4] | Species, data=iris)

#################################################
### Plot Data - Parallel Coordinates Plot #######
#################################################
library(MASS)
parcoord(iris[1:4], col=iris$Species)

#####################################
### Plot Data - Using GGPLOT2 #######
#####################################
library(ggplot2)
qplot(Sepal.Length, Sepal.Width, data=iris, facets=Species ~.)

#####################################
### Plot Data - Save Plots ##########
#####################################

# Save as a PDF file
pdf("./data/SI_testPlots.pdf")
x <- 1:100
plot(x, x^2)
graphics.off()

# Save as a PNG file
png("./data/SI_testPlots.png")
x <- 1:100
plot(x, x^2)
graphics.off()

