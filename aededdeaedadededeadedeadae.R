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
trial_vars<- c( "participant", "practice_comparison", "pracsimilarity", "catch_response_time", "catchnumberprac", "catchpracsimilarity", "realcomparison", "similarity", "catchnumber", "catchsimilarity", "catchRT", "catchtrialorder", "response_time", "trials_2.thisN") 

analysisdata <- pilotdata[trial_vars]

#prolific gives us really fucked up participant codes so we need to keep track of them
#participants <- unique(analysisdata[c("participant")])

#establishes data frame the pearson results will be put in
pearsonresults <- cor(mtcars$disp, mtcars$cyl)
pearsonresults <- pearsonresults[-c(0)]
#meantimes <- data.frame(c(1:nrow(participants)))
#meantimes <- meantimes[-c(0)]

#gonna store the average responses in here
compset <- data.frame(c(1:4371))
stretchedmeans <- compset




#for (i in c(1:nrow(participants))){
  i=1
  #tempframe <- analysisdata %>% filter(participant == participants[i,])
  tempframe <- analysisdata
  #labels the first and second passes
  tempframe$pass <- "first pass"
  tempframe$pass[169:nrow(tempframe)] <- "second pass"
  tempframe$pass[0:7] <- "practice"
  
  
  
  #the following is stuff for catch trials that i copied from beths code.
  #i rewrote it so that id understand it, but its essentially the same as hers
  #print("babbis")
  #pulls the catch trials order and makes it a list of numbers "catches"
  allcatches <- (unique(pilotdata$catchtrialorder))
  allcatches <- str_sub(allcatches, 2, -2)
  allcatches <- str_split(allcatches, pattern = fixed(","))
  
  #for some reason the first item in allcatches was empty so i remove it
  allcatches <- allcatches[-c(1)]
  #access whichever participant's catch trial sequence you want
  #remember to move all this shit within the for loop that goes through all the participants
  #and swap this 17 for an "i"
  catches <- allcatches[[i]]
 
  #including the trial ones
  catches <- as.numeric(catches)
  catches <- sort(catches)

  
  
  
  
  
  # in the catchbool column, rows whose trialnumber matches an entry in the catches
  # list (which is a list of trials that were catch trials) will be marked TRUE
  tempframe$catchbool <- is.element(tempframe$trials_2.thisN, catches)
  
  catchesframe <- tempframe[-c(1:7),]
  
  
  #i fucked up the experiment program somehow and there's two observations missing from each set. gotta drop the singletons.
  # also, none of the responses are "2"
  # also, there's something wrong with the catch trials
  # what is going on?
  for (j in c(8:nrow(tempframe))){
    
    if (length(which(tempframe$realcomparison == tempframe$realcomparison[j], arr.ind=TRUE)) < 2){
      print("missing at")
      print(which(tempframe$realcomparison == tempframe$realcomparison[j], arr.ind=TRUE))
      tempframe <- tempframe[-c(j),]
    }
  }
  
  #make the first pass and second pass in seperate datasets, and then 
  #arrange them so theyre in the same order. simplifies things
  firstpasses <- tempframe[with(tempframe,which(pass == "first pass")), ]
  firstpasses <- firstpasses  %>% arrange(realcomparison)
  secondpasses <- tempframe[with(tempframe,which(pass == "second pass")), ]
  secondpasses <- secondpasses  %>% arrange(realcomparison)
  
  
  #this generates graphs of the similarity ratings- but the x axis is in order 
  #of comparison number in the reference list, not the order that the participants 
  #made the comparisons in.
  
  complist <- firstpasses$realcomparison
  complist <- data.frame(complist)
  complist$meanresponses <- (firstpasses$similarity +secondpasses$similarity)/2
  complist$first <- firstpasses$similarity
  complist$timefirst <- firstpasses$response_time
  complist$second <- secondpasses$similarity
  complist$timesecond <- secondpasses$response_time
  complist$trialnos <- firstpasses$trials_2.thisTrialN
  
  complist$meantime <- (complist$timefirst + complist$timesecond) /2
  
  totalmeantime <- mean(complist$meantime)
  meantimes <- rbind(meantimes, totalmeantime)
  
  #correlates the first passes with the second passes, pearson style
  pearson <- cor(firstpasses$similarity, secondpasses$similarity)
  
  #print(tempframe$participant[i])
  #print("in firstpasses:")
  #print(nrow(firstpasses))
  #print(pearson)
  pearsonresults <- rbind(pearsonresults, pearson)
  
  rownames(pearsonresults)[i] = as.character(tempframe$participant[i])
  
  
    
    
    responses<- ggplot(data=complist, aes(x=trialnos)) + 
    geom_point(shape = 3, aes(y = first, colour = "first pass")) + 
    geom_point(shape = 4, aes(y = second, colour = "second pass")) +
    geom_line(aes(y=meanresponses, colour = "average")) +
    labs(title=participants[i,]) + 
    theme(legend.position = "top", axis.text.x = element_text(angle = 90), axis.ticks.length= unit(.5, "cm"))
    
    
    
    print(responses)
  #tried using geom_vline() to add vertical gridlines but it looked shit unfortunately
    
#this is for getting the average results for each colour pair to feed into the sim matrix
    stretchedmeans$newthing <- "NA"
    for (k in 1:nrow(complist)){
      iter <- complist$complist[k]
     stretchedmeans$newthing[iter] <- complist$meanresponses[k]
    }
    
    names(stretchedmeans)[names(stretchedmeans) == "newthing"] <- as.character(participants[i,])
    
    cbind(compset, stretchedmeans)
    
    
    #the pilot data didnt have catch trial responses in it due to my fuckup
    catchzone <- tempframe[tempframe[, "catchbool"] == TRUE, ]
    catchzone$corrs <- cor(tempframe$similarity, tempframe$catchnumber)
    catchplot <- ggplot(data= catchzone, aes(x= trialno, y= similarity, colour = "response"), shape=3) +
      geom_point() +
      geom_point(aes(y=catchnumber, colour="catch target"), shape=4) +
      labs(title=participants[i,])
    
    
    
    
    #here id put a graph of correlation between catch prompt and response against reaction time
    #but im fairly sure the catches are fucked somehow so theres no point
#}



#stretchedmeans contains the mean response for each comparison that each participant made
#it's "stretched" cause each column has room for every single comparison im looking at
#obviously, all the ones a participant didn't touch are NA. So like, the vast majority
#of each column is NA
stretchedmeans <- subset(stretchedmeans, select= -c(c.1.4371.))
stretchedmeans[] <- lapply(stretchedmeans, as.numeric)
#allsims then contains the average response on each comparison across the entire sample
#for use in the matrices and whatever
allsims <- rowMeans(stretchedmeans, na.rm=TRUE)

#pearson correlations ~ modifying the dataframe to make it easier to wrangle
pearsonresults <- data.frame(pearsonresults)
pearsonresults$participants <- row.names(pearsonresults)
pearsonresults <- pearsonresults %>% rename(correlations = pearsonresults)

#boxplot of correlation between first and second passes.
#individual participants' corr is added as a randomised jitter
#legend is set to not exist due to i dont want to list out every participant id
ggplot(pearsonresults, aes(y=correlations)) +
  geom_boxplot() +
  geom_jitter(aes(x=0, colour=participants)) +
  labs(title="variance in observation correlation", x="participants", y="correlation (pearson)") +
  theme(legend.position = "none") +
  scale_x_discrete(labels = NULL)

#make allsims a dataframe and rename its singular column so its easier to wrangle
allsims <- data.frame(allsims)
allsims <- allsims %>% rename(results = allsims)


#this will plot the responses for each comparison on a scatter. obv its unreadable cause the x axis has more than
#4000 ticks on it, but whatever.
#ggplot(allsims, aes(x=as.numeric(rownames(allsims)), y=results, colour=as.numeric(rownames(allsims)))) +
  #geom_point(na.rm=TRUE) 
 
names(meantimes)[1] <- "meanresponsetime"
meantimes$meanresponsetime <- as.numeric(meantimes$meanresponsetime)
meantimes$ID <- participants
names(meantimes)[2] <- "participant"
meantimes$meanresponsetime <- as.character(meantimes$meanresponsetime)



ggplot(data=pearsonresults, aes(y=correlations, x=meantimes$meanresponsetime, colour = participants)) +
  geom_point()


ggplot(data=meantimes, aes(y=meanresponsetime))+
  geom_boxplot() +
  geom_jitter(aes(x=0, colour=pearsonresults$participants)) +
  labs(title="variance in avg response time", x="participants", y="correlation (pearson)") +
  scale_x_discrete(labels = NULL)






