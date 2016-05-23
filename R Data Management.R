
#Data Management

#sqldf - https://cran.r-project.org/web/packages/sqldf/index.html
#xlsx - https://cran.r-project.org/web/packages/xlsx/index.html
#RODBC - https://cran.r-project.org/web/packages/RODBC/index.html
#dplyr - https://cran.r-project.org/web/packages/dplyr/index.html
#reshape2 - https://cran.r-project.org/web/packages/reshape2/index.html

install.packages(c("sqldf","xlsx","RODBC","dplyr","reshape2"))

install.packages("dplyr")
install.packages("sqldf")
install.packages("RODBC")
install.packages("reshape2")
install.packages("xlsx")

getwd()

library(sqldf) #OK
library(xlsx) #OK
library(RODBC) #OK
library(dplyr) #OK
library(reshape2) #OK

getwd()
#"C:/Users/Daniel/Documents"

setwd("C:/Users/Daniel/Documents")

#read.csv default settings assume your data has headers in the first row
datacsv1 <- read.csv("DM_2305_ExcelExample_plots.csv")
datacsv2 <- read.csv("DM_2305_ExcelExample_sites.csv")
#look at the top 6 rows of the first sheet
head(datacsv1)

sessionInfo()

con <- odbcConnect("Example")
#We can then read tables from the database into R.

#List the tables in the database
sqlTables(con)
#Extract the plot measurement table
dataodbc1 <- sqlFetch(con, "Plot measurements")
head(dataodbc1)

#CTRL enter to run current line
#check CRAN mirror take make sure it is UK

iris <- iris

is.data.frame(iris)

is.matrix(iris)

typeof(iris)

iris.mat <- as.matrix(iris)

iris.list <- as.list(iris)

iris$Plot <- rep(c(rep(1,10), rep(2, 10), rep(3,5)),3)

lm1 <- lm(Sepal.Length ~ Species + Plot, data = iris)

summary(lm1)


is.factor(iris$Plot)

iris$Plotf <- factor(iris$Plot)

is.factor(iris$Plotf)

lm2 <- lm(Sepal.Length ~ Species + Plotf, data = iris)

summary(lm2)

str(iris.list)


iris.list$Sepal.Length
iris.list[1]
iris.list[[1]]
iris.list[[1]][1]

iris[order(iris$Petal.Width),]

iris[order(-iris$Petal.Width),]

iris[order(iris$Species, iris$Petal.Width),]

duplicated(iris)

duplicated(iris[,3:4])

duplicated(iris[,3:4])[1:6] #first six results of duplicated

head(iris,6)#first six rows of the dataset

iris.unique <- iris[!duplicated(iris[,3:4]),] #The exclamation marks means 'not'
nrow(iris.unique) #102 rows remain in this dataset from the 150 original rows

library(dplyr)
iris.unique2 <- distinct(iris, Petal.Length, Petal.Width)
nrow(iris.unique2)



predicts <- read.csv("http://onlinelibrary.wiley.com/store/10.1002/ece3.1303/asset/supinfo/ece31303-sup-0002-DataS1.csv?v=1&s=f1c0f0c5a047aa08c65fb48a3186cecc18faa8a0")
predicts <- tbl_df(predicts)  # convert to tbl_df


# INVESTIGATE
# how many names?
length(names(predicts))
# how many columns?
ncol(predicts)
# print, don't worry dplyr won't spend hours printing!
print(predicts)

# HOW HABITATS ARE REPRESENTED BY EACH STUDY?

# Method 1: for loop
# Identify the studies by creating a new studies column
predicts$SSID <- paste0(predicts$Source_ID, '_', predicts$Study_number)
stds <- unique(predicts$SSID)
# Loop through and identify the the number of habitats in each study
nhabitats <- rep(0, length(stds))
for(i in 1:length(nhabitats)) {
  nhabitats[i] <- length(unique(predicts$Predominant_habitat[predicts$SSID == stds[i]]))
}
res_1 <- data.frame(stds, nhabitats)

# Method 2: group_by + summarise
# group
res_2 <- group_by(predicts, SSID)
# summarise
res_2 <- summarise(res_2, N_habitats=n_distinct(Predominant_habitat))


# Method 3: Behold, the power of the pipe!
res_3 <- mutate(predicts, SSID=paste0(Source_ID, '_', Study_number)) %>%
  group_by(SSID) %>%
  summarise(N_habitats=n_distinct(Predominant_habitat))

#swirl package
#Hadley Wickham Paper on Tidy, read this


iris.NA <- iris
iris.NA[1:4,1] <- NA #replace the first four entries in column 1 with NA
head(iris.NA)

summary(iris.NA)

iris.NA[is.na(iris.NA$Sepal.Length),]

iris.NA.cc <- iris.NA[complete.cases(iris.NA),]
head(iris.NA.cc)
summary(iris.NA.cc)


library(reshape2)

install.packages("swirl")

library(swirl)

#| Hi! I see that you have some variables saved in your workspace. To keep things running smoothly, I recommend you clean up
#| before starting swirl.

#| Type ls() to see a list of the variables in your workspace. Then, type rm(list=ls()) to clear your workspace.

iris.melt <- melt(iris)
summary(iris.melt)

iris.cast <- dcast(iris.melt, value~variable)
head(iris.cast)

iris.agg <- aggregate(iris,list(iris$Species),mean)

iris.agg

tapply(iris$Sepal.Length,iris$Species,mean)

iris$LogSepLength <- log(iris$Sepal.Length)

install.packages("tidyr")

#create some new random data for plot 4
iris.extra <- data.frame(Sepal.Length = rnorm(10, 5, 0.7), Sepal.Width = rnorm(10,3.2,0.5),Petal.Length = rnorm(10,1.3,0.3), Petal.Width = rnorm(10,0.2, 0.001), Species = "setosa", Plot = 4)

rbind(iris, iris.extra)

iris.extra$Plotf <- factor(iris.extra$Plot)
iris.extra$LogSepLength <- log(iris.extra$Sepal.Length)
iris.all <- rbind(iris, iris.extra)

iris.all$ObsID <- paste0(iris.all$Species, row.names(iris.all))

irisspdata <- data.frame(Species = unique(iris$Species), avgheight = c(42.3, 33.5, 35.7), colour = c("violet", "blue", "blue"))

irisindivdata <- data.frame(ObsID = iris.all$ObsID, noseeds = c(rpois(50, 10), rpois(50,8), rpois(50, 9), rpois(10,10)))
irisindivdata$germprop <- c(rpois(50, 3), rpois(50,2), rpois(50, 3), rpois(10,3))/irisindivdata$noseeds


iris_all <- iris.all
irismatchsp <- sqldf("select * from iris_all, irisspdata where iris_all.Species = irisspdata.Species")



irismatchindiv <- sqldf("select * from irismatchsp, irisindivdata where irismatchsp.ObsID = irisindivdata.ObsID")

#source() careful about recursion
#DLL dynamic load libaray for integration of other languages

