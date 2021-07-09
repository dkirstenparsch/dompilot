shhh <- suppressPackageStartupMessages # stops annoying warnings when loading libraries
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(MASS)
library(Matrix)
library(reshape2)
library(ape) # stats
library(vegan) # stats
library(RColorBrewer)
library(cocor)
library(DescTools)
library(reshape2)
library(grid)
library(ggplotify)
library(stringr)
library(tidyverse)


# read the csv data files into a dataframe
files = list.files(pattern="*.csv")
pilotdata = sapply(files, read.csv, simplify=FALSE)%>% bind_rows(.id = "id")


participants <- unique(pilotdata$id)


#Select variables we need for analysis 
trial_vars<- c( "participant", "practice_comparison", "pracsimilarity", "realcomparison", "similarity", "response_time", "trials_2.thisN") 
catch_vars<- c("participant", "catch_response_time", "catchnumberprac", "catchpracsimilarity", "catchnumber", "catchsimilarity", "catchRT", "catchtrialorder")
trialdata <- pilotdata[trial_vars]
catchdata <- pilotdata[catch_vars]

#plots the catch responses and prompts against time needed to make them
ggplot(catchdata, aes(x=catchRT)) +
  geom_point(shape = 3, aes(y=catchnumber, colour="prompt")) +
  geom_point(shape = 4, aes(y=catchsimilarity, colour="response"))

trialdata$pass <- "practice"
trialdata$pass[8:167] <- "first pass"
trialdata$pass[168:327] <- "second pass"

organised <- data.frame(c(1:160))
organised$firstpasses <- trialdata$similarity[8:167]
organised$firsttime <- trialdata$response_time[8:167]
organised$secondpasses <- trialdata$similarity[168:327]
organised$secondtime <- trialdata$response_time[168:327]
organised$comparison <- trialdata$realcomparison[8:167]
organised$meanresponse <- (organised$firstpasses + organised$secondpasses)/2
organised$meantime <- (organised$firsttime + organised$secondtime)/2
organised <- organised %>% rename(trialno = c.1.160.)
organised$difference <- organised$firstpasses - organised$secondpasses

#plots first and second pass, plus the mean response, against trial number
ggplot(data=organised, aes(x=trialno)) +
  geom_line(aes(y=firstpasses, colour = "first pass")) + 
  geom_point(aes(y=firstpasses, colour = "first pass"))+
  geom_line(aes(y=secondpasses, colour = "second pass")) + 
  geom_point(aes(y=secondpasses, colour= "second pass"))+
  labs(title="participant dominik \nr= 0.392 \nrho = 0.398", y="similarity rating", x= "trial number")



propercatches$accuracy <- propercatches$catchnumber - propercatches$catchsimilarity
#plots difference between first and second pass as a line
ggplot(data=organised, aes(x=trialno, y=difference, colour = "difference between trials"))+
  geom_line() +
  geom_line(aes(y=firsttime, colour = "response time on 1st trial")) +
  geom_line(aes(y=secondtime, colour = "response time on 2nd trial")) +
  labs(title = "difference between passes; also reaction times")


catchcoords <- unique(catchdata$catchtrialorder)
catchcoords <- catchcoords[-c(1)]
catchcoords <- str_sub(catchcoords, 2, -2)
catchcoords <- str_split(catchcoords, pattern = fixed(","))
catchcoords <- as.numeric(unlist(catchcoords))
catchcoords <- sort(catchcoords)

propercatches <- catchdata[-c(1:7),]
propercatches <- propercatches[c(catchcoords),]
propercatches$number <- c(1:20)

meancatchrt <- mean(propercatches$catchRT)
meancatchacc <- mean(propercatches$accuracy)


bepis <- data.frame(1)
bepis$meanrt <- meancatchrt
bepis$correct <- 1



ggplot(data=bepis, aes(x=meanrt, y=correct)) +
  geom_point() +
  labs(y="catch score", x="mean response time on catch trials", title="catch score for dominik")


spearman <- cor.test(x=organised$firstpasses, y=organised$secondpasses, method="spearman")
pearson <- cor.test(x=organised$firstpasses, y=organised$secondpasses, method="pearson")