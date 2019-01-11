library(lme4)
library(multcomp)
require(Rmisc)
library(ggplot2)
require(DescTools)
require(BayesFactor)
setwd("C:\\Users\\Mike\\Desktop\\School\\DAdat")


#read in all files with Gray in the name

facefiles = list.files(pattern = "*Face*")


#combine all of the files into one big dataframe called df
for (file in 1:length(facefiles)){ #for each file in the list of files called facefiles
  if (file == 1){ #if it's the first one
    df<- read.table(facefiles[file], sep = " ") #assign it to a dataframe called df
    df$expnum<-substr(facefiles[file],4,4)
  }
  else{ #if it's not the first one
    tempdf<- read.table(facefiles[file], sep = " ") #assign it to a temporary dataframe called tempdf
    tempdf$expnum<-substr(facefiles[file],4,4)
    df<- rbind(df,tempdf) #combine tempdf and df, make that the new df
  }
}

#get rid of the weird columns that are entirely NA values
df<-df[,colSums(is.na(df)) == 0]

#name the columns
colnames(df)<-c("subnum", "setnum", "prepost", "resp", "rt", "filename", "expnum")
#turn subject numbers into a factor
df$subnum<-factor(df$subnum)


#this line calculates subject mean biases and puts them in a new dataframe called means
means<-aggregate(resp ~ subnum + prepost, data = df, FUN = mean)

maxrating<-aggregate(resp ~ subnum, data = df, FUN = max)
colnames(means)<-c("subnum", "prepost", "rating")
colnames(maxrating)<-c("subnum", "rating")

#correct for subjects being mean to faces
#make a new dataset to calculate correction that is shaped the same as raw mean scores
correctedmeans<-means
#change every value in the rating column to 999 so we know if it didn't work correctly
correctedmeans$rating<-999
#go through each subject and calculate the correction
#rating column in "means" df is the participants' mean score, rating column in "maxrating" df is the participants' maximum score
for (sub in correctedmeans$subnum){
  #corrected mean is mean/maximum rating for that subject
  correctedmeans$rating[correctedmeans$subnum == sub & correctedmeans$prepost==1] <- means$rating[means$subnum == sub & means$prepost==1]/maxrating$rating[maxrating$subnum==sub]
  correctedmeans$rating[correctedmeans$subnum == sub & correctedmeans$prepost==2] <- means$rating[means$subnum == sub & means$prepost==2]/maxrating$rating[maxrating$subnum==sub]
}

#create a vector with subject numbers from subjects that recieved anodal stim
anodalsubnums<-c(34,36,38,40,42,46,48,50,52,55,56,58,60,62,64,66)
#make a new column for stimulation condition called stim. set every value to S for sham
correctedmeans$stim<-"S"
#loop through each row and change the "S" to "A" if the subject number is in the anodalsubnums vector
for (sub in correctedmeans$subnum){
  if (sub %in% anodalsubnums){
    correctedmeans$stim[correctedmeans$subnum == sub]<-"A"
  }
}

means$stim<-"S"
for (sub in correctedmeans$subnum){
  if (sub %in% anodalsubnums){
    means$stim[means$subnum == sub]<-"A"
  }
}

#make a new dataset to calculate change in rating pre- and post-stimulation (using correctedmeans as a template)
correctedmeanchange<-correctedmeans[correctedmeans$prepost==1, c("subnum", "rating", "stim")]
#change every value in the rating column to 0.5 so we know if it didn't work correctly
correctedmeanchange$rating<-0.5
#loop through the rows and calculate the change in rating
for (sub in correctedmeanchange$subnum){
  correctedmeanchange$rating[correctedmeanchange$subnum == sub] <- correctedmeans$rating[correctedmeans$subnum == sub & correctedmeans$prepost == 2] - correctedmeans$rating[correctedmeans$subnum == sub & correctedmeans$prepost == 1]
}

correctedmeanchange$stim <- as.factor(correctedmeanchange$stim)
correctedmeans$stim <- as.factor(correctedmeans$stim)


#make a new dataset to calculate change in rating pre- and post-stimulation (using means as a template)
meanchange<-means[means$prepost==1, c("subnum", "rating", "stim")]
#change every value in the rating column to 0.5 so we know if it didn't work correctly
meanchange$rating<-0.5
#loop through the rows and calculate the change in rating
for (sub in meanchange$subnum){
  meanchange$rating[meanchange$subnum == sub] <- means$rating[means$subnum == sub & means$prepost == 2] - means$rating[means$subnum == sub & means$prepost == 1]
}

meanchange$stim <- as.factor(meanchange$stim)
means$stim <- as.factor(means$stim)


#t-test, DV is change in rating, IV is stim condition
t.test(correctedmeanchange$rating ~ correctedmeanchange$stim)

CohenD(correctedmeanchange$rating[correctedmeanchange$stim == "S"], correctedmeanchange$rating[correctedmeanchange$stim == "A"])

ttestBF(formula = rating ~ stim, data = correctedmeanchange)

#ANOVA
summary(aov(rating ~ prepost*stim + Error(subnum/prepost), data = correctedmeans))

#Kruskal-Wallis
kruskal.test(rating ~ prepost*stim + Error (subnum/prepost), data = means)

wilcox.test(meanchange$rating ~ meanchange$stim)

ttestBF(formula = rating ~ stim, data = meanchange)


#####################################################################################
#################################SECONDARY ANALYSES##################################
#####################################################################################
faces<-read.csv("faces.csv", stringsAsFactors = FALSE)

demos<-read.csv("demos-2.csv", stringsAsFactors = FALSE)
#take out weird stuff before questions
demos<-demos[,18:37]
#change column names
colnames(demos)<-as.character(demos[1,])
#take out rows and questions we don't care about
demos<-demos[-c(1,2), c(1,9,11,17,18)]

anodalsubnums<-c(34,36,38,40,42,46,48,50,52,55,56,58,60,62,64,66)

#recode answers to sexual orientation

######BIG LONG THING TO RECODE ORIENTATION INTO A SCALE THAT MATCHES GENDER PREFERENCE
########higher numbers indicate more attraction to males
demos$orientation<-NA

#big long loop to code orientation into attracted to male/female rather than gay/straight
for (rown in (1:length(demos$`Subject Number`))){
  
  if (demos$`Which of the following most accurately reflects your current understanding of yourself?`[rown] 
      == "Exclusively Straight - Only sexually attracted to the opposite sex."){
    if (demos$`Gender Identity: - Selected Choice`[rown] == "Male"){
      demos$orientation[rown]<-0
    }else{
      demos$orientation[rown]<-8
    }
  }
  
  if (demos$`Which of the following most accurately reflects your current understanding of yourself?`[rown] 
      == "Straight - Nearly always sexually attracted to the opposite sex.  Rarely attracted to the same sex."){ 
      if (demos$`Gender Identity: - Selected Choice`[rown] == "Male"){
      demos$orientation[rown]<-1
    }else{
      demos$orientation[rown]<-7
    }
  }
  
  if (demos$`Which of the following most accurately reflects your current understanding of yourself?`[rown] 
      == "Mostly Straight - Mostly sexually attracted to the opposite sex. Occasionally attracted to the same sex."){ 
      if (demos$`Gender Identity: - Selected Choice`[rown] == "Male"){
      demos$orientation[rown]<-2
    }else{
      demos$orientation[rown]<-6
    }
  }
  
  if (demos$`Which of the following most accurately reflects your current understanding of yourself?`[rown] 
      == "Bisexual-Leaning Straight - Primarily sexually attracted to the opposite sex and definitely attracted to same sex."){ 
      if (demos$`Gender Identity: - Selected Choice`[rown] == "Male"){
      demos$orientation[rown]<-3
    }else{
      demos$orientation[rown]<-5
    }
  }
  
  if (demos$`Which of the following most accurately reflects your current understanding of yourself?`[rown] 
      == "Bisexual - More or less equally sexually attracted to opposite sex and same sex."){ 
      if (demos$`Gender Identity: - Selected Choice`[rown] == "Male"){
      demos$orientation[rown]<-4
    }else{
      demos$orientation[rown]<-4
    }
  }
  
  if (demos$`Which of the following most accurately reflects your current understanding of yourself?`[rown] 
      == "Bisexual-Leaning Gay/Lesbian - Primarily sexually attracted to the same sex and definitely attracted to the opposite sex."){ 
      if (demos$`Gender Identity: - Selected Choice`[rown] == "Male"){
      demos$orientation[rown]<-5
    }else{
      demos$orientation[rown]<-3
    }
  }
  
  if (demos$`Which of the following most accurately reflects your current understanding of yourself?`[rown] 
      == "Bisexual - More or less equally sexually attracted to opposite sex and same sex."){ 
      if (demos$`Gender Identity: - Selected Choice`[rown] == "Male"){
      demos$orientation[rown]<-6
    }else{
      demos$orientation[rown]<-2
    }
  }
  
  if (demos$`Which of the following most accurately reflects your current understanding of yourself?`[rown] 
      == "Gay/Lesbian - Nearly always sexually attracted to the same sex. Rarely attracted to the opposite sex."){
      if (demos$`Gender Identity: - Selected Choice`[rown] == "Male"){
      demos$orientation[rown]<-7
    }else{
      demos$orientation[rown]<-1
    }
  }
  
  if (demos$`Which of the following most accurately reflects your current understanding of yourself?`[rown] 
      == "Exclusively Gay/Lesbian - Only sexually attracted to the same sex." ){
      if (demos$`Gender Identity: - Selected Choice`[rown] == "Male"){
      demos$orientation[rown]<-8
    }else{
      demos$orientation[rown]<-0
    }
  }
  
}
#######END ORIENTATION RECODING
###############################

#make race_abbreviated column to match race column in the face data
demos$race_abbreviated<-"A" #asian
demos$race_abbreviated[demos$`Race: Do you consider yourself : - Selected Choice` == "White"]<-"W"
demos$race_abbreviated[demos$`Race: Do you consider yourself : - Selected Choice` == "Black or African America"]<-"B"
demos$race_abbreviated[demos$`Ethnicity: Do you consider yourself of Hispanic origin?` == "Hispanic or Latino, regardless of Race (below)"]<- "L"

###get info about faces shown
faces1<-read.csv("faces1.csv", header = FALSE, stringsAsFactors = FALSE)
faces2<-read.csv("faces2.csv", header = FALSE, stringsAsFactors = FALSE)
df$race<-NA
df$gender<-NA

#this loop matches facenumbers in df to filenames in faces to get face info
for (rown in (1:nrow(df))){
  
  #figure out which filename corresponds to the face number in df
  if (df$expnum[rown] == 1 && df$prepost[rown] == 1){
    filename <- faces1$V1[df$filename[rown]]
  }#end if expnum = 1 and prepost = 1
  if (df$expnum[rown] == 1 && df$prepost[rown] == 2){
    filename <- faces2$V1[df$filename[rown]]
  }#end if expnum = 1 and prepost = 2
  if (df$expnum[rown] == 2 && df$prepost[rown] == 1){
    filename <- faces2$V1[df$filename[rown]]
  }#end if expnum = 2 and prepost = 1
  if (df$expnum[rown] == 2 && df$prepost[rown] == 2){
    filename <- faces2$V1[df$filename[rown]]
  }#end if expnum = 2 and prepost = 2
  
  #get race info from faces.csv
  df$race[rown]<-faces$Race[faces$Target == substring(filename,5,10)]
  #get gender info from faces.csv
  df$gender[rown]<-faces$Gender[faces$Target == substring(filename,5,10)]
}

#add same race info to trial data
#0 is same, 1 is different
df$samerace<-0
for (rown in (1:nrow(df))){
  if (df$race[rown] == demos$race_abbreviated[demos$`Subject Number` == df$subnum[rown]]){
    df$samerace[rown]<-1
  }
}

#find mean change in attractiveness by gender
gendermeans<-aggregate(resp ~ subnum + prepost + gender, data = df, FUN = mean)
correctedgendermeans<-gendermeans
for (rown in (1:length(correctedgendermeans$subnum))){
  #corrected mean is mean/maximum rating for that subject
  subj<-correctedgendermeans$subnum[rown]
  correctedgendermeans$rating[rown]<-correctedgendermeans$rating[rown] / maxrating$rating[maxrating$subnum == subj]
}

#make gender dummy code column
correctedgendermeans$gender_dummy<-0
correctedgendermeans$gender_dummy[correctedgendermeans$gender == "M"]<-1
#add orientation column to means
correctedgendermeans$orientation<-NA
for (rown in (1:length(correctedgendermeans$subnum))){
  subj<-correctedgendermeans$subnum[rown]
  correctedgendermeans$orientation[rown]<-demos$orientation[demos$`Subject Number` == subj]
}

#make a new column for stimulation condition called stim. set every value to S for sham
correctedgendermeans$stim<-"S"
#loop through each row and change the "S" to "A" if the subject number is in the anodalsubnums vector
for (sub in correctedgendermeans$subnum){
  if (sub %in% anodalsubnums){
    correctedgendermeans$stim[correctedmeans$subnum == sub]<-"A"
  }
}

correctedgendermeans$stim_dummy<-0
correctedgendermeans$stim_dummy[correctedgendermeans$stim == "A"]<-1

#make a new dataset to calculate change in rating pre- and post-stimulation (using correctedgenders as a template)
correctedgendermeanchange<-correctedgendermeans[correctedgendermeans$prepost==1, c("subnum", "resp", "orientation", "gender_dummy", "stim", "stim_dummy")]
#change every value in the rating column to 0.5 so we know if it didn't work correctly
correctedgendermeanchange$rating<-0.5
#loop through the rows and calculate the change in rating
for (rown in (1:length(correctedgendermeanchange$subnum))){
  thisgender<-correctedgendermeanchange$gender_dummy[rown]
  subj<-correctedgendermeanchange$subnum[rown]
  correctedgendermeanchange$rating[rown]<-correctedgendermeans$resp[correctedgendermeans$gender_dummy == thisgender & correctedgendermeans$subnum == subj & correctedgendermeans$prepost == 2] - correctedgendermeans$resp[correctedgendermeans$gender_dummy == thisgender & correctedgendermeans$subnum == subj & correctedgendermeans$prepost == 1]
}




#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

#find mean change in attractiveness by race
racemeans<-aggregate(resp ~ subnum + prepost + samerace, data = df, FUN = mean)
correctedracemeans<-racemeans
for (rown in (1:length(correctedracemeans$subnum))){
  #corrected mean is mean/maximum rating for that subject
  subj<-correctedracemeans$subnum[rown]
  correctedracemeans$rating[rown]<-correctedracemeans$rating[rown] / maxrating$rating[maxrating$subnum == subj]
}

#make a new column for stimulation condition called stim. set every value to S for sham
correctedracemeans$stim<-"S"
#loop through each row and change the "S" to "A" if the subject number is in the anodalsubnums vector
for (sub in correctedracemeans$subnum){
  if (sub %in% anodalsubnums){
    correctedracemeans$stim[correctedmeans$subnum == sub]<-"A"
  }
}

correctedracemeans$samerace_word<-"same"
correctedracemeans$samerace_word[correctedracemeans$samerace==0]<-"different"

#make a new dataset to calculate change in rating pre- and post-stimulation (using correctedracemeans as a template)
correctedracemeanchange<-correctedracemeans[correctedracemeans$prepost==1, c("subnum", "resp", "samerace", "stim", "samerace_word")]
#change every value in the rating column to 0.5 so we know if it didn't work correctly
correctedracemeanchange$rating<-0.5
#loop through the rows and calculate the change in rating
for (rown in (1:length(correctedgendermeanchange$subnum))){
  thisrace<-correctedracemeanchange$samerace[rown]
  subj<-correctedracemeanchange$subnum[rown]
  correctedracemeanchange$rating[rown]<-correctedracemeans$resp[correctedracemeans$samerace == thisrace & correctedracemeans$subnum == subj & correctedracemeans$prepost == 2] - correctedracemeans$resp[correctedracemeans$samerace == thisrace & correctedracemeans$subnum == subj & correctedracemeans$prepost == 1]
}






####REGRESSION FOR ORIENTATION * GENDER * STIM INTERACTION
gendermod<-lm(rating ~ stim_dummy*gender_dummy*orientation, data = correctedgendermeanchange)

gendermod1<-lm(rating ~ gender_dummy*orientation, data = correctedgendermeanchange)


####MIXED ANOVA FOR STIM*RACE INTERACTION
#lme_race<-lmer(rating ~ stim*samerace + (1|subnum), data = correctedracemeanchange)
raceaov<-aov(rating ~ stim*samerace + Error(subnum/samerace), data = correctedracemeanchange)


summary(raceaov)
summary(gendermod)

t.test(correctedmeanchange$rating ~ correctedmeanchange$stim)

summary(aov(rating ~ stim*prepost + Error(subnum/prepost), data = correctedmeans))

#write.table(correctedmeans, "faceratingsprepost-2.txt")
#write.table(correctedmeanchange, "faceratingschange-2.txt")
write.table(correctedmeans, "faceratingsprepost-2.txt")
write.table(correctedmeanchange, "faceratingschange-2.txt")
