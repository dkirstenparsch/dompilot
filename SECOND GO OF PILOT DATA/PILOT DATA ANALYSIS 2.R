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
library(plot.matrix)

# read the csv data files into a dataframe
files = list.files(pattern="*.csv")
pilotdata = sapply(files, read.csv, simplify=FALSE)%>% bind_rows(.id = "id")


participants <- data.frame(unique(pilotdata$participant))

#ill put the accuracy ratings and mean rt on the catches in here
resultsagg <- participants
resultsagg <- resultsagg %>% rename(participant = unique.pilotdata.participant.)
#Select variables we need for analysis, put stuff for catches and trials in seperate dataframes
trial_vars<- c( "participant", "practice_comparison", "pracsimilarity", "realcomparison", "similarity", "response_time", "trials_2.thisN") 
catch_vars<- c("participant", "catch_response_time", "catchnumberprac", "catchpracsimilarity", "catchnumber", "catchsimilarity", "catchRT", "catchtrialorder")
trialdata <- pilotdata[trial_vars]
catchdata <- pilotdata[catch_vars]

trialdata$pass <- "practice"
trialdata$pass[8:167] <- "first pass"
trialdata$pass[168:327] <- "second pass"

#will put the mean responses of participants by comparison index in here
allcomps <- data.frame(c(1:4371))
allcomps <- allcomps %>% rename(comparisons = c.1.4371.)
stretchedmeans <- data.frame(c(1:4371))
stretchedmeans <- stretchedmeans %>% rename(comparisons = c.1.4371.)

for (i in c(1:nrow(participants))){
  
  #dataframes for participant i's trial and catch data
  partitrials <- trialdata %>% filter(participant == participants[i,])
  catchitrials <- catchdata %>% filter(participant == participants[i,])

  
  #makes a dataframe where its like, 1-160 with the first and second passes in different 
  #columns.
  organised <- data.frame(c(1:160))
  organised$firstpasses <- partitrials$similarity[8:167]
  organised$firsttime <- partitrials$response_time[8:167]
  organised$secondpasses <- partitrials$similarity[168:327]
  organised$secondtime <- partitrials$response_time[168:327]
  organised$comparison <- partitrials$realcomparison[8:167]
  organised$meanresponse <- (organised$firstpasses + organised$secondpasses)/2
  organised$meantime <- (organised$firsttime + organised$secondtime)/2
  organised <- organised %>% rename(trialno = c.1.160.)
  organised$difference <- abs(organised$firstpasses - organised$secondpasses)
  
  #extracts and cleans up the coordinates of the catch trials
  catchcoords <- unique(catchitrials$catchtrialorder)
  catchcoords <- catchcoords[-c(1)]
  catchcoords <- str_sub(catchcoords, 2, -2)
  catchcoords <- str_split(catchcoords, pattern = fixed(","))
  catchcoords <- as.numeric(unlist(catchcoords))
  catchcoords <- sort(catchcoords)
  #makes a dataframe with just the catch shit in it, without all the empty lines
  propercatches <- catchitrials[-c(1:7),]
  propercatches <- propercatches[c(catchcoords),]
  
  
  propercatches$accuracy <- propercatches$catchnumber == propercatches$catchsimilarity
  correctcatches <- 0
  
  for (j in c(1:nrow(propercatches))){
    
    if (propercatches$accuracy[j] == TRUE){
      correctcatches <- correctcatches + 1
    }
  }
  

  spearman <- cor.test(x=organised$firstpasses, y=organised$secondpasses, method="spearman")
  spearman <- spearman$estimate[[1]]
  pearson <- cor.test(x=organised$firstpasses, y=organised$secondpasses, method="pearson")
  pearson <- pearson$estimate[[1]]
  
  #plots the participant responses
  responses <- ggplot(data=organised, aes(x=trialno)) +
    geom_line(aes(y=firstpasses, colour = "first pass")) + 
    geom_point(aes(y=firstpasses, colour = "first pass"))+
    geom_line(aes(y=secondpasses, colour = "second pass")) + 
    geom_point(aes(y=secondpasses, colour= "second pass"))+
    labs(title=paste("participant:", participants[i,], "\nrho =", spearman, "\nr =", pearson, sep=" "), y="similarity rating", x= "trial number")
  
  print(responses)
  
  resultsagg$spearman[i] <- spearman
  resultsagg$pearson[i] <- pearson
  resultsagg$meanrt[i] <- mean(organised$meantime)
  resultsagg$meandiff[i] <- mean(organised$difference)
  resultsagg$meancatchrt[i] <- mean(propercatches$catchRT)
  resultsagg$catchacc[i] <- sum(correctcatches/nrow(propercatches))
  
  
  
  stretchedmeans$newthing <- "NA"
  for (k in (1:nrow(stretchedmeans))){
    if(k %in% organised$comparison ){
    stretchedmeans$newthing[k] <- organised$meanresponse[which(organised$comparison==k)]
    
    }
  }
  
  names(stretchedmeans)[names(stretchedmeans) == "newthing"] <- participants[i,]
  
  
  
#end of the big for loop  
}

stretchedmeans <- subset(stretchedmeans, select = -c(comparisons))
stretchedmeans <- data.frame(lapply(stretchedmeans,as.numeric))

#this makes the similarity column have the average response on each comparison
#across the whole sample
#now i jsut need a way to feed that into a dissimilarity matrix
allcomps$similarity <- rowMeans(stretchedmeans, na.rm=TRUE)



ggplot(data=resultsagg, aes(x=meancatchrt, y=catchacc)) +
  geom_point() +
  labs(y="catch score accuracy", x="mean response time on catch trials (seconds)", title="catch scores")

#correlation between trials by participant against mean reaction time
ggplot(data=resultsagg, aes(x=meanrt, y=spearman, colour=participant, shape="spearman rho")) +
  geom_point() +
  geom_point(aes(y=pearson, shape="pearson r")) +
  labs(title = "correlation between trials by participant", x= "mean reaction time", y = "correlation between trials")+
  guides(color = FALSE)


#TODO get the comparison-with-colour-codes list from my function from before
#use those to fit each comp to a grid coordinate
#grid is 93 x 93
#put them in the same order i put them into the thing, i guess

colours <- read.csv("C:\\Users\\domin\\Desktop\\New folder\\SECOND GO OF PILOT DATA\\colourcodes\\colourcodes.csv")
colournames <- c("r1", "g1", "b1", "r2", "g2", "b2")

for (z in c(1:6)){
names(colours)[z] <- colournames[z]
}

#this shit is to pull out the rgb codes and shit for each coordinate in the comparison list
colours2 <- data.frame(paste(colours$r1, ",", colours$g1, ",", colours$b1))
colours2$second <- paste(colours$r2, ",", colours$g2, ",", colours$b2)
names(colours2)[1] <- "first"

colourslist <- unique(colours2$first)
colourcomps <- data.frame(c(1:93))
colourcomps <- colourcomps[-c(1)]
colourindex <- colourcomps
for (i in c(1:length(colourslist))){
 temp <- colours2$second[c(which(colours2$first==colourslist[i]))]
 index <- c(which(colours2$first==colourslist[i]))
 if(length(temp)<93){
   diff <- 93 - length(temp)
   for (j in c(1:diff)){
     temp <- append(temp, NA)
     index <- append(index, NA)
   }
 }
 colourcomps$temp <- temp
 colourindex$temp <- index
 names(colourcomps)[i] <- colourslist[i]
 names(colourindex)[i] <- colourslist[i]
}

plot(matrix(allcomps$similarity, ncol = 93))

