setwd("C:\\Users\\Mike\\Desktop\\School\\DAdat")

require(DescTools)
require(BayesFactor)
#read in all files with Gray in the name
grayfiles = list.files(pattern = "*Gray*")


#combine all of the files into one big dataframe called df
for (file in 1:length(grayfiles)){ #for each file in the list of files called grayfiles
  if (file == 1){ #if it's the first one
    df<- read.table(grayfiles[file], sep = " ") #assign it to a dataframe called df
  }
  else{ #if it's not the first one
    tempdf<- read.table(grayfiles[file], sep = " ") #assign it to a temporary dataframe called tempdf
    df<- rbind(df,tempdf) #combine tempdf and df, make that the new df
  }
}

#get rid of the weird columns that are entirely NA values
df<-df[,colSums(is.na(df)) == 0]

#name the columns
colnames(df)<-c("subnum", "setnum", "prepost", "rt", "resp", "acc", "darkside", "length", "filename")
#turn subject numbers into a factor
df$subnum<-factor(df$subnum)

#######################################################################
#analyze bias for trials where there is a no difference between images#
#######################################################################
nodiff<-df[df$filename > 24, ]
#next two lines calculate bias, assuming 'darkside' column is coded so that
#0 = bottom stim is dark on the left 
nodiff$bias<-0
nodiff$bias[nodiff$resp == nodiff$darkside]<-1
#this line calculates subject mean biases and puts them in a new dataframe called nodiff_means
nodiff_means<-aggregate(bias ~ subnum + prepost, data = nodiff, FUN = mean)
#create a vector with subject numbers from subjects that recieved anodal stim
anodalsubnums<-c(34,36,38,40,42,46,48,50,52,55,56,58,60,62,64,66)
nodiff_means$stim<-"S"
#loop through each row and change the "S" to "A" if the subject number is in the anodalsubnums vector
for (sub in nodiff_means$subnum){
  if (sub %in% anodalsubnums){
    nodiff_means$stim[nodiff_means$subnum == sub]<-"A"
  }
}

#make a new dataset to calculate change in biase pre- and post-stimulation (using nodiff_means as a template)
nodiff_meanchange<-nodiff_means[nodiff_means$prepost==1, c("subnum", "bias", "stim")]
#change every value in the bias column to 2 so we know if it didn't work correctly
nodiff_meanchange$bias<-2
#loop through the rows and calculate the change in bias
for (sub in nodiff_meanchange$subnum){
  nodiff_meanchange$bias[nodiff_meanchange$subnum == sub] <- nodiff_means$bias[nodiff_means$subnum == sub & nodiff_means$prepost == 2] - nodiff_means$bias[nodiff_means$subnum == sub & nodiff_means$prepost == 1]
}


#write out results for later use
write.table(nodiff_means, "biasprepost-2.txt")
write.table(nodiff_meanchange, "biaschange-2.txt")

nodiff_means<-subset(nodiff_means)
nodiff_meanchange<-subset(nodiff_meanchange)


#t-test, DV is change in bias, IV is stim condition
t.test(nodiff_meanchange$bias ~ nodiff_meanchange$stim)

CohenD(nodiff_meanchange$bias[nodiff_meanchange$stim == "S"], nodiff_meanchange$bias[nodiff_meanchange$stim == "A"])

ttestBF(formula = bias ~ stim, data = nodiff_meanchange)

summary(aov(bias ~ stim*factor(prepost) + Error(subnum/prepost), data = nodiff_means))
#####################################################################
#analyze bias for trials where there IS a  difference between images#
#####################################################################
diff<-df[df$filename < 25, ]
#calculate accuracy rate so we can take out people that are too good
freaksubs<-c()
accuracies<-aggregate(acc ~ subnum, data = diff, FUN = mean)
for (sub in accuracies$subnum){
  if (accuracies$acc[accuracies$subnum == sub] > 0.9){
    freaksubs<- c(sub, freaksubs)
  }
}
#remove freaksubs from diff df
diff<-diff[!diff$subnum %in% freaksubs,]

#take out correct trials
diff_use<-diff[diff$acc==0,]

diff$bias<-0
diff$bias[diff$resp == diff$darkside]<-1
#this line calculates subject mean biases and puts them in a new dataframe called diff_means
diff_means<-aggregate(bias ~ subnum + prepost, data = diff, FUN = mean)

#make a new column for stimulation condition called stim. set every value to S for sham
diff_means$stim<-"S"
#loop through each row and change the "S" to "A" if the subject number is in the anodalsubnums vector
for (sub in diff_means$subnum){
  if (sub %in% anodalsubnums){
    diff_means$stim[diff_means$subnum == sub]<-"A"
  }
}

#make a new dataset to calculate change in biase pre- and post-stimulation (using diff_means as a template)
diff_meanchange<-diff_means[diff_means$prepost==1, c("subnum", "bias", "stim")]
#change every value in the bias column to 2 so we know if it didn't work correctly
diff_meanchange$bias<-2
#loop through the rows and calculate the change in bias
for (sub in diff_meanchange$subnum){
  diff_meanchange$bias[diff_meanchange$subnum == sub] <- diff_means$bias[diff_means$subnum == sub & diff_means$prepost == 2] - diff_means$bias[diff_means$subnum == sub & diff_means$prepost == 1]
}


#t-test, DV is change in bias, IV is stim condition
t.test(diff_meanchange$bias ~ diff_meanchange$stim)

#ANOVA
summary(aov(bias ~ stim*prepost + Error(subnum/prepost), data = diff_means))

CohenD(diff_meanchange$bias[diff_meanchange$stim == "A"], diff_meanchange$bias[diff_meanchange$stim == "S"])
