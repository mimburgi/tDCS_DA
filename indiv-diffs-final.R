###################################################################
#RUN THE GREYSCALES ANALYSIS AND FACE ANALYSIS SCRIPTS FIRST!!!####
###################################################################
setwd("C:\\Users\\Mike\\Desktop\\School\\DAdat")


require(jtools)
require(ggthemes)
require(DescTools)
require(Rmisc)
require(BayesFactor)

blinksallA<-read.csv("ebr-2A.csv")
blinksallB<-read.csv("ebr-2B.csv")

blinksall<-blinksallA[,c("Subject", "Average.EBR", "PrePost")]
blinksall$Average.EBR_A<-blinksallA$Average.EBR
blinksall$Average.EBR_B<-blinksallB$Average.EBR
blinksall$Average.EBR<-rowMeans(blinksall[c("Average.EBR_A", "Average.EBR_B")])

blinkschangeA<-subset(blinksallA, !is.na(Change.Score))
blinkschangeB<-subset(blinksallB, !is.na(Change.Score))

blinkschange<-blinkschangeA[c("Subject", "Change.Score")]
blinkschange$Change.Score_A<-blinkschangeA$Change.Score
blinkschange$Change.Score_B<-blinkschangeB$Change.Score
blinkschange$Change.Score<-rowMeans(blinkschange[c("Change.Score_A", "Change.Score_B")])

#change to blinks per min
blinkschange$Change.Score<-2*blinkschange$Change.Score
blinksall$Average.EBR<-2*blinksall$Average.EBR

blinkschange<-blinkschange[,c("Subject", "Change.Score")]
blinksall<-blinksall[,c("Subject", "Average.EBR", "PrePost")]

colnames(blinkschange)<-c("Subject", "ChangeEBR")
colnames(blinksall)<-c("Subject", "AvgEBR", "prepost")

anodalsubnums<-c(34,36,38,40,42,46,48,50,52,55,56,58,60,62,64,66)

#remove subjects with missing data
blinksprepost<-subset(blinksprepost, Subject != 50 & Subject != 59)

blinkschange$stim<-"S"
#loop through each row and change the "S" to "A" if the subject number is in the anodalsubnums vector
for (sub in blinkschange$Subject){
  if (sub %in% anodalsubnums){
    blinkschange$stim[blinkschange$Subject == sub]<-"A"
  }
}

blinksall$stim<-"S"
#loop through each row and change the "S" to "A" if the subject number is in the anodalsubnums vector
for (sub in blinksall$Subject){
  if (sub %in% anodalsubnums){
    blinksall$stim[blinksall$Subject == sub]<-"A"
  }
}

blinksall$stim<-as.factor(blinksall$stim)
#blinksall$prepost<-as.factor(blinksall$prepost)
blinkschange$stim<-as.factor(blinkschange$stim)

t.test(ChangeEBR ~ stim, blinkschange)

ttestBF(formula = ChangeEBR ~ stim, data = blinkschange)

CohenD(blinkschange$ChangeEBR[blinkschange$stim == "S"], blinkschange$ChangeEBR[blinkschange$stim == "A"])

summary(aov(AvgEBR ~ stim*prepost + Error(Subject/prepost), data = blinksall))

blinksprepost<-blinksall

blinksprepost$prepostnum[blinksprepost$prepost == "Pre "]<-1
blinksprepost$prepostnum[blinksprepost$prepost == "Post"]<-2

blinksprepost$prepost<-blinksprepost$prepostnum
blinksprepost<-blinksprepost[,c("Subject", "AvgEBR", "prepost", "stim")]
#####aggregate all three measures in a df

biasprepost<-read.table("biasprepost-2.txt")
biaschange<-read.table("biaschange-2.txt")
faceprepost<-read.table("faceratingsprepost-2.txt")
facechange<-read.table("faceratingschange-2.txt")
allprepost<-biasprepost
allchange<-biaschange

for (i in 1:length(allchange$subnum)){
  allchange$blinks[i]<-blinkschange$ChangeEBR[blinkschange$Subject == allchange$subnum[i]]
  allchange$face[i]<-facechange$rating[facechange$subnum == allchange$subnum[i]]
}


for (i in 1:length(allprepost$subnum)){
  allprepost$blinks[i]<-blinksprepost$AvgEBR[blinksprepost$Subject == allprepost$subnum[i] & blinksprepost$prepost == allprepost$prepost[i]]
  allprepost$face[i]<-faceprepost$rating[faceprepost$subnum == allprepost$subnum[i] & faceprepost$prepost == allprepost$prepost[i]]
}


allchange$subnum<-factor(allchange$subnum)
allprepost$subnum<-factor(allprepost$subnum)
allprepost$prepost<-factor(allprepost$prepost)

######################################################################################
######################################################################################
######################################################################################

for (i in 1:length(allchange$subnum)){
  allchange$preblinks[i]<-blinksprepost$AvgEBR[blinksprepost$Subject == allprepost$subnum[i] & blinksprepost$prepost == 1]
  allchange$prerating[i]<-faceprepost$rating[faceprepost$subnum == allprepost$subnum[i] & faceprepost$prepost == 1]
  allchange$prebias[i]<-biasprepost$bias[biasprepost$subnum == allprepost$subnum[i] & biasprepost$prepost == 1]
}

allchange$stimdummy<-0
allchange$stimdummy[allchange$stim == "A"]<-1

#DV is bias, IV is blinks
BiasByBlinkMod<-lm(bias ~ stim*preblinks, data = allchange) #nothing sign
BiasByFaceMod<-lm(bias ~ stim*prerating, data = allchange) #interaction not sign, change is related to prerating
FaceByBiasMod<-lm(face ~ stim*prebias, data = allchange) #nothing significant
FaceByBlinkMod<-lm(face ~ stim*preblinks, data = allchange) #nothing significant
BlinkByBiasMod<-lm(scale(blinks) ~ stim*scale(prebias), data = allchange) #ineraction .02
BlinkByFaceMod<-lm(scale(blinks) ~ stim*scale(prerating), data = allchange) #interaction .008


BlinkByBiasMod_active<-lm(scale(blinks) ~ scale(prebias), data = allchange[allchange$stim == "A",]) 
BlinkByBiasMod_sham<-lm(scale(blinks) ~ scale(prebias), data = allchange[allchange$stim == "S",]) 

BlinkByFaceMod_active<-lm(scale(blinks) ~ scale(prerating), data = allchange[allchange$stim == "A",]) 
BlinkByFaceMod_sham<-lm(scale(blinks) ~ scale(prerating), data = allchange[allchange$stim == "S",]) 

####graph this stuff
blinkbybiasplot<-interact_plot(BlinkByBiasMod, pred = "prebias", modx = "stim", plot.points = TRUE, y.label = "Change in EBR, Post-Pre", x.label = "Pre-stim Greyscales Bias")
blinkbyfaceplot<-interact_plot(BlinkByFaceMod, pred = "prerating", modx = "stim", plot.points = TRUE, y.label = "Change in EBR, Post-Pre", x.label = "Pre-stim Attractiveness Rating")
facebybiasplot<-interact_plot(FaceByBiasMod, pred = "prebias", modx = "stim", plot.points = TRUE, y.label = "Change in Attractiveness Rating, Post-Pre", x.label = "Pre-stim Greyscales Bias")
facebyblinkplot<-interact_plot(FaceByBlinkMod, pred = "preblinks", modx = "stim", plot.points = TRUE, y.label = "Change in Attractiveness Rating, Post-Pre", x.label = "Pre-stim EBR", legend.main = "Stimulation\nCondition")

library(gridExtra)
grid.arrange(facebybiasplot, facebyblinkplot, ncol = 2)
grid.arrange(blinkbybiasplot, blinkbyfaceplot, ncol = 2)

#look for outliers
prebiasrange<-3*sd(allchange$prebias)
prebiasmean<-mean(allchange$prebias)
prebiaslow<-prebiasmean - prebiasrange
prebiashi<-prebiasmean + prebiasrange
length(allchange$prebias[allchange$prebias < prebiaslow])
length(allchange$prebias[allchange$prebias > prebiashi])

preratingrange<-3*sd(allchange$prerating)
preratingmean<-mean(allchange$prerating)
preratinglow<-preratingmean - preratingrange
preratinghi<-preratingmean + preratingrange
length(allchange$prerating[allchange$prerating < preratinglow])
length(allchange$prerating[allchange$prerating > preratinghi])

preblinksrange<-3*sd(allchange$preblinks)
preblinksmean<-mean(allchange$preblinks)
preblinkslow<-preblinksmean - preblinksrange
preblinkshi<-preblinksmean + preblinksrange
length(allchange$preblinks[allchange$preblinks < preblinkslow])
length(allchange$preblinks[allchange$preblinks > preblinkshi])

blinksrange<-3*sd(allchange$blinks)
blinksmean<-mean(allchange$blinks)
blinkslow<-blinksmean - blinksrange
blinkshi<-blinksmean + blinksrange
length(allchange$blinks[allchange$blinks < blinkslow])
length(allchange$blinks[allchange$blinks > blinkshi])

###median split ANOVAs
prebiasmed<-median(allchange$prebias)
preratingmed<-median(allchange$prerating)
allchange$prebiasgroup<-NA
allchange$preratinggroup<-NA
for (subj in allchange$subnum){
  if (allchange$prebias[allchange$subnum == subj] > prebiasmed){
    allchange$prebiasgroup[allchange$subnum == subj]<-"H"
  }
  else if (allchange$prebias[allchange$subnum == subj] < prebiasmed){
    allchange$prebiasgroup[allchange$subnum == subj]<-"L"
  }
}
for (subj in allchange$subnum){
  if (allchange$prerating[allchange$subnum == subj] > preratingmed){
    allchange$preratinggroup[allchange$subnum == subj]<-"H"
  }
  else if (allchange$prerating[allchange$subnum == subj] < preratingmed){
    allchange$preratinggroup[allchange$subnum == subj]<-"L"
  }
}

allchange$preratinggroup<-as.factor(allchange$preratinggroup)
allchange$prebiasgroup<-as.factor(allchange$prebiasgroup)

splitprerating<-aov(blinks ~ stim*preratinggroup, data = allchange)
splitprerating_high<-aov(blinks ~ stim, data = allchange[allchange$preratinggroup == "H",])
splitprerating_low<-aov(blinks ~ stim, data = allchange[allchange$preratinggroup == "L",])

splitprebias<-aov(blinks ~ stim*prebiasgroup, data = allchange)
splitprebias_high<-aov(blinks ~ stim, data = allchange[allchange$prebiasgroup == "H",])
splitprebias_low<-aov(blinks ~ stim, data = allchange[allchange$prebiasgroup == "L",])

library(ggplot2)
splitprerating_sum <- summarySE(allchange, measurevar="blinks", groupvars=c("preratinggroup","stim"))
splitprerating_bars_agg<-aggregate(blinks ~ preratinggroup + stim, allchange, mean)

splitprerating_bars<-ggplot(data = splitprerating_sum, aes(y = blinks, x = preratinggroup, fill = stim)) + 
                              geom_bar(position = "dodge", stat = "identity", colour = "black") + 
                              geom_errorbar(aes(ymin=blinks-se, ymax=blinks+se), 
                                            width = .2, position = position_dodge(.9)) + 
                              labs(x = "Pre-Stim Facial Attractiveness Rating, Median Split (High or Low)", 
                                   y = "Change in EBR (Post-Pre)", 
                                   fill = "stim",
                                   title="Change in EBR by Pre-Stim Facial Attractiveness Rating, Median Split")

splitprebias_sum <- summarySE(allchange, measurevar="blinks", groupvars=c("prebiasgroup","stim"))
splitprebias_bars_agg<-aggregate(blinks ~ prebiasgroup + stim, allchange, mean)

splitprebias_bars<-ggplot(data = splitprebias_sum, aes(y = blinks, x = prebiasgroup, fill = stim)) + 
  geom_bar(position = "dodge", stat = "identity", colour = "black") + 
  geom_errorbar(aes(ymin=blinks-se, ymax=blinks+se), 
                width = .2, position = position_dodge(.9)) + 
  labs(x = "Pre-Stim Greyscales Bias, Median Split (High or Low)", 
       y = "Change in EBR (Post-Pre)", 
       fill = "stim",
       title="Change in EBR by Greyscales Bias, Median Split")

grid.arrange(splitprebias_bars, splitprerating_bars, ncol = 2)
#####average z scores
allchange$preratingz<-scale(allchange$prerating)
allchange$prebiasz<-scale(allchange$prebias)
allchange$prebiasz<-allchange$prebiasz*-1
allchange$preblinkz<-scale(allchange$preblinks)
allchange$zcomposite_justtwo<-rowMeans(allchange[c("preratingz", "prebiasz")])
allchange$zcomposite_allthree<-rowMeans(allchange[c("preratingz", "prebiasz", "preblinkz")])
allchange$zcomposite_justtwo_trytwo<-rowMeans(allchange[c("preblinkz", "prebiasz")])


BlinkByCompositeMod_ztwo<-lm(blinks ~ stim*zcomposite_justtwo, data = allchange)
interact_plot(BlinkByCompositeMod_ztwo, pred = "zcomposite_justtwo", modx = "stim", plot.points = TRUE, y.label = "Change in EBR, Post-Pre", x.label = "Pre-stim Composite, attractiveness and bias")

BlinkByCompositeMod_zall<-lm(scale(blinks) ~ stim*scale(zcomposite_allthree), data = allchange)

BlinkByCompositeMod_zall_sham<-lm(blinks ~ zcomposite_allthree, data = allchange[allchange$stim == "S",])
BlinkByCompositeMod_zall_active<-lm(blinks ~ zcomposite_allthree, data = allchange[allchange$stim == "A",])

interact_plot(BlinkByCompositeMod_zall, pred = "zcomposite_allthree", modx = "stim", plot.points = TRUE, y.label = "Change in EBR, Post-Pre", x.label = "Pre-stim Composite, all three")

BiasByCompositeMod_zall<-lm(bias ~ stim*zcomposite_allthree, data = allchange)
FaceByCompositeMod_zall<-lm(face ~ stim*zcomposite_allthree, data = allchange)

BlinkByCompositeMod_ztwo_active<-lm(blinks ~ zcomposite_justtwo, data = allchange[allchange$stim == "A",])
BlinkByCompositeMod_ztwo_sham<-lm(blinks ~ zcomposite_justtwo, data = allchange[allchange$stim == "S",])

###median split ANOVAs for z scores
ztwomed<-median(allchange$zcomposite_justtwo)
zthreemed<-median(allchange$zcomposite_allthree)
allchange$ztwogroup<-NA
allchange$zthreegroup<-NA
for (subj in allchange$subnum){
  if (allchange$zcomposite_justtwo[allchange$subnum == subj] > ztwomed){
    allchange$ztwogroup[allchange$subnum == subj]<-"H"
  }
  else if (allchange$zcomposite_justtwo[allchange$subnum == subj] < ztwomed){
    allchange$ztwogroup[allchange$subnum == subj]<-"L"
  }
}
for (subj in allchange$subnum){
  if (allchange$zcomposite_allthree[allchange$subnum == subj] > zthreemed){
    allchange$zthreegroup[allchange$subnum == subj]<-"H"
  }
  else if (allchange$zcomposite_allthree[allchange$subnum == subj] < zthreemed){
    allchange$zthreegroup[allchange$subnum == subj]<-"L"
  }
}

allchange$ztwogroup<-as.factor(allchange$ztwogroup)
allchange$zthreegroup<-as.factor(allchange$zthreegroup)

split_ztwo<-aov(blinks ~ stim*ztwogroup, data = allchange) #trend
split_ztwo_high<-aov(blinks ~ stim, data = allchange[allchange$ztwogroup == "H",]) #sign
split_ztwo_low<-aov(blinks ~ stim, data = allchange[allchange$ztwogroup == "L",]) #not sign

splitztwo_sum <- summarySE(allchange, measurevar="blinks", groupvars=c("ztwogroup","stim"))
splitztwo_bars_agg<-aggregate(blinks ~ ztwogroup + stim, allchange, mean)

ggplot(data = splitztwo_sum, aes(y = blinks, x = ztwogroup, fill = stim)) + 
  geom_bar(position = "dodge", stat = "identity", colour = "black") + 
  geom_errorbar(aes(ymin=blinks-se, ymax=blinks+se), 
                width = .2, position = position_dodge(.9)) + 
  labs(x = "Pre-Stim DA Score (attractiveness and bias composite), Median Split (High or Low)", 
       y = "Change in EBR (Post-Pre)", 
       fill = "stim",
       title="Change in EBR by Pre-Stim DA Score (attractiveness and bias composite)")

split_zthree<-aov(blinks ~ stim*zthreegroup, data = allchange)
split_zthree_high<-aov(blinks ~ stim, data = allchange[allchange$zthreegroup == "H",]) #sign
split_zthree_low<-aov(blinks ~ stim, data = allchange[allchange$zthreegroup == "L",]) #not sign

CohenD(allchange$blinks[allchange$stim == "A" & allchange$zthreegroup == "H"],
       allchange$blinks[allchange$stim == "A" & allchange$zthreegroup == "L"])

splitzthree_sum <- summarySE(allchange, measurevar="blinks", groupvars=c("zthreegroup","stim"))
splitzthree_bars_agg<-aggregate(blinks ~ zthreegroup + stim, allchange, mean)

ggplot(data = splitzthree_sum, aes(y = blinks, x = zthreegroup, fill = stim)) + 
  geom_bar(position = "dodge", stat = "identity", colour = "black") + 
  geom_errorbar(aes(ymin=blinks-se, ymax=blinks+se), 
                width = .2, position = position_dodge(.9)) + 
  labs(x = "Baseline Dopamine Composite, median split", 
       y = "Change in EBR (blinks per minute)", 
       fill = "stim",
       title="Change in EBR by Baseline Dopamine") + 
  scale_x_discrete(labels = c("High", "Low")) + 
  scale_fill_discrete(labels = c("Active", "Sham"),
                      name = "Stimulation\nCondition\n") + 
  theme(    # remove the vertical grid lines
            panel.grid.major.x = element_blank() ,
            # explicitly set the horizontal lines (or they will disappear too)
            panel.grid.major.y = element_line( size=.05, color="lightgrey" ),
            panel.grid.minor.y = element_line( size=.05, color="lightgrey" ),
            panel.background = element_blank(),
            axis.line.x = element_line(color="black", size = .2),
            axis.line.y = element_line(color="black", size = .2),
            plot.title = element_text(hjust = 0.5),
            text=element_text(size=14),
            legend.key.width=unit(3,"line")
            )

###########################
###test individual slopes##
###########################

ztwosham<-lm(blinks ~ zcomposite_justtwo, data = allchange[allchange$stim == "S",])
ztwoactive<-lm(blinks ~ zcomposite_justtwo, data = allchange[allchange$stim == "A",])

summary(ztwosham)
summary(ztwoactive)

zthreesham<-lm(scale(blinks) ~ scale(zcomposite_allthree), data = allchange[allchange$stim == "S",])
zthreeactive<-lm(scale(blinks) ~ scale(zcomposite_allthree), data = allchange[allchange$stim == "A",])

summary(zthreesham)
summary(zthreeactive)



###tests


################################
######MULT COMP CORR############
################################

#if we want to correct for simple effects comparisonst aht we never cared about, regressions should be
#changed to [2:4,4] and anovas to [1:3]

#first six regressions
facebybias_regression_pvals<-unname(summary(FaceByBiasMod)$coefficients[4,4])
facebyblink_regression_pvals<-unname(summary(FaceByBlinkMod)$coefficients[4,4])
biasbyface_regression_pvals<-unname(summary(BiasByFaceMod)$coefficients[4,4])
biasbyblink_regression_pvals<-unname(summary(BiasByBlinkMod)$coefficients[4,4])
blinkbyface_regression_pvals<-unname(summary(BlinkByFaceMod)$coefficients[4,4])
blinkbybias_regression_pvals<-unname(summary(BlinkByBiasMod)$coefficients[4,4])

#median split ANOVAs, individual scores as predictors
splitprebias_pvals<-summary(splitprebias)[[1]][["Pr(>F)"]][3]
splitprerating_pvals<-summary(splitprerating)[[1]][["Pr(>F)"]][3]

#z composite, just two
ztwo_regression_pvals<-unname(summary(BlinkByCompositeMod_ztwo)$coefficients[4,4])
split_ztwo_pvals<-summary(split_ztwo)[[1]][["Pr(>F)"]][3]

#z composite, all three
zthree_regression_pvals<-unname(summary(BlinkByCompositeMod_zall)$coefficients[4,4])
split_zthree_pvals<-summary(split_zthree)[[1]][["Pr(>F)"]][3]

#individual slopes for z composite
zthreeactive_pvals<-as.vector(unname(summary(zthreeactive)$coefficients[2,4]))
zthreesham_pvals<-as.vector(unname(summary(zthreesham)$coefficients[2,4]))

#main effect of stimulation for z composite, broken down by baseline DA
split_zthree_high_pvals<-summary(split_zthree_high)[[1]][["Pr(>F)"]][1]
split_zthree_low_pvals<-summary(split_zthree_low)[[1]][["Pr(>F)"]][1]

#z composite, all three, other two DVs
zthree_regression_bias_pvals<-unname(summary(BiasByCompositeMod_zall)$coefficients[4,4])
zthree_regression_face_pvals<-unname(summary(FaceByCompositeMod_zall)$coefficients[4,4])


#####adjusted p values
all_pvals<-c(facebybias_regression_pvals, facebyblink_regression_pvals, 
                 biasbyblink_regression_pvals, biasbyface_regression_pvals,
                 blinkbybias_regression_pvals, blinkbyface_regression_pvals,
                 splitprebias_pvals, splitprerating_pvals,
                 ztwo_regression_pvals, split_ztwo_pvals,
                 zthree_regression_pvals, split_zthree_pvals,
                 zthreeactive_pvals, zthreesham_pvals, 
             zthree_regression_bias_pvals, zthree_regression_face_pvals,
             split_zthree_high_pvals, split_zthree_low_pvals)

num_comparisons<-length(all_pvals)


################
#adjust p values
################
all_pvals_adjusted<-p.adjust(all_pvals, method = "fdr")

zthreesham_pvals_adjusted<-all_pvals_adjusted[14]
zthreeactive_pvals_adjusted<-all_pvals_adjusted[13]
split_zthree_pvals_adjusted<-all_pvals_adjusted[12]
zthree_regression_pvals_adjusted<-all_pvals_adjusted[11]
split_ztwo_pvals_adjusted<-all_pvals_adjusted[10]
ztwo_regression_pvals_adjusted<-all_pvals_adjusted[9]
splitprerating_pvals_adjusted<-all_pvals_adjusted[8]
splitprebias_pvals_adjusted<-all_pvals_adjusted[7]
blinkbyface_regression_pvals_adjusted<-all_pvals_adjusted[6]
blinkbybias_regression_pvals_adjusted<-all_pvals_adjusted[5]

zthree_regression_bias_pvals_adjusted<-all_pvals_adjusted[15]
zthree_regression_face_pvals_adjusted<-all_pvals_adjusted[16]

split_zthree_high_pvals_adjusted<-all_pvals_adjusted[17]
split_zthree_low_pvals_adjusted<-all_pvals_adjusted[18]

facebybias_regression_pvals_adjusted<-all_pvals_adjusted[1]
facebyblink_regression_pvals_adjusted<-all_pvals_adjusted[2]
biasbyblink_regression_pvals_adjusted<-all_pvals_adjusted[3]
biasbyface_regression_pvals_adjusted<-all_pvals_adjusted[4]
###################################
#############plots#################
###################################

dev.off()
tiff("barplot.tiff", res = 900, width = 8, height = 8, units = 'in')
ggplot(data = splitzthree_sum, aes(y = blinks, x = zthreegroup, fill = stim)) + 
  geom_bar(position = "dodge", stat = "identity", colour = "black", size = .6) + 
  geom_errorbar(aes(ymin=blinks-se, ymax=blinks+se), 
                width = .4, position = position_dodge(.9), size = .6) + 
  labs(x = "Baseline Dopamine Composite", 
       y = "Change in EBR (blinks per minute)", 
       fill = "stim",
       title="Change in EBR by Baseline Dopamine") + 
  scale_x_discrete(labels = c("High", "Low")) + 
  scale_fill_discrete(labels = c("Active", "Sham"),
                      name = "Stimulation\nCondition\n") + 
  theme(    # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.2, color="darkgrey" ),
    panel.grid.minor.y = element_line( size=.2, color="darkgrey" ),
    panel.background = element_blank(),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    plot.title = element_text(hjust = 0.5),
    text=element_text(size=22),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20))
  )
dev.off()

tiff("scatterplot.tiff", res = 900, width = 8, height = 8, units = 'in')
ggplot(data = allchange, aes(y = blinks, x = zcomposite_allthree, color = stim)) + 
  geom_point(size = 3, color = "black") +
  geom_point(size = 2) + 
  geom_smooth(method="lm", fill = NA, size = 2) +
  labs(x = "Baseline Dopamine Composite", 
       y = "Change in EBR (blinks per minute)",
       title="Change in EBR by Baseline Dopamine") + 
  scale_color_discrete(labels = c("Active", "Sham"),
                      name = "Stimulation\nCondition\n") + 
  theme(    # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="lightgrey" ),
    panel.grid.minor.y = element_line( size=.1, color="lightgrey" ),
    panel.background = element_blank(),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    plot.title = element_text(hjust = 0.5),
    text=element_text(size=22),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20))
  )
dev.off()


face_sum<-summarySE(allchange, measurevar = "face", groupvars = c("stim"))
tiff("barplot-face.tiff", res = 900, width = 8, height = 8, units = 'in')
ggplot(data = face_sum, aes(y = face, x = stim, fill = stim)) + 
  geom_bar(position = "dodge", stat = "identity", colour = "black", size = .6) + 
  geom_errorbar(aes(ymin=face-se, ymax=face+se), 
                width = .4, position = position_dodge(.9), size = .6) + 
  labs(x = "Stimulation Condition", 
       y = "Change in Attractiveness Ratings") + 
  scale_x_discrete(labels = c("Active", "Sham")) +
  theme(    # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.2, color="darkgrey" ),
    panel.grid.minor.y = element_line( size=.2, color="darkgrey" ),
    panel.background = element_blank(),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    plot.title = element_text(hjust = 0.5),
    text=element_text(size=22),
    legend.position = "none",
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20))
  )
dev.off()

blinks_sum<-summarySE(allchange, measurevar = "blinks", groupvars = c("stim"))
tiff("barplot-blinks.tiff", res = 900, width = 8, height = 8, units = 'in')
ggplot(data = blinks_sum, aes(y = blinks, x = stim, fill = stim)) + 
  geom_bar(position = "dodge", stat = "identity", colour = "black", size = .6) + 
  geom_errorbar(aes(ymin=blinks-se, ymax=blinks+se), 
                width = .4, position = position_dodge(.9), size = .6) + 
  labs(x = "Stimulation Condition", 
       y = "Change in EBR") + 
  scale_x_discrete(labels = c("Active", "Sham")) +
  theme(    # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.2, color="darkgrey" ),
    panel.grid.minor.y = element_line( size=.2, color="darkgrey" ),
    panel.background = element_blank(),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    plot.title = element_text(hjust = 0.5),
    text=element_text(size=22),
    legend.position = "none",
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20))
  )
dev.off()

bias_sum<-summarySE(allchange, measurevar = "bias", groupvars = c("stim"))
tiff("barplot-bias.tiff", res = 900, width = 8, height = 8, units = 'in')
ggplot(data = bias_sum, aes(y = bias, x = stim, fill = stim)) + 
  geom_bar(position = "dodge", stat = "identity", colour = "black", size = .6) + 
  geom_errorbar(aes(ymin=bias-se, ymax=bias+se), 
                width = .4, position = position_dodge(.9), size = .6) + 
  labs(x = "Stimulation Condition", 
       y = "Change in Visuospatial Bias") + 
  scale_x_discrete(labels = c("Active", "Sham")) +
  theme(    # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.2, color="darkgrey" ),
    panel.grid.minor.y = element_line( size=.2, color="darkgrey" ),
    panel.background = element_blank(),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    plot.title = element_text(hjust = 0.5),
    text=element_text(size=22),
    legend.position = "none",
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20))
  )
dev.off()


allchange$logblinks<-log(allchange$blinks + 15)
summary(lm(logblinks ~ stim*zcomposite_allthree, data = subset(allchange, logblinks > 2)))

ggplot(data = subset(allchange, logblinks > 2), aes(y = logblinks, x = zcomposite_allthree, color = stim)) + 
  geom_point(size = 3, color = "black") +
  geom_point(size = 2) + 
  geom_smooth(method="lm", fill = NA, size = 2) +
  labs(x = "Baseline Dopamine Composite", 
       y = "Change in EBR (blinks per minute)",
       title="Change in EBR by Baseline Dopamine") + 
  scale_color_discrete(labels = c("Active", "Sham"),
                       name = "Stimulation\nCondition\n") + 
  theme(    # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="lightgrey" ),
    panel.grid.minor.y = element_line( size=.1, color="lightgrey" ),
    panel.background = element_blank(),
    axis.line.x = element_line(color="black", size = 1),
    axis.line.y = element_line(color="black", size = 1),
    plot.title = element_text(hjust = 0.5),
    text=element_text(size=22),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20))
  )


###############
#####MIXED MODEL
###############

for (subj in levels(allchange$subnum)){
  composite<-allchange$zcomposite_allthree[allchange$subnum == subj]
  blinksprepost$zcomposite[blinksprepost$Subject == subj]<-composite
}

blinksprepost$prepost[blinksprepost$prepost == 1]<-0
blinksprepost$prepost[blinksprepost$prepost == 2]<-1

library(lme4)
model = lmer(AvgEBR ~ stim*prepost*zcomposite + (1|Subject), data = blinksprepost, REML = TRUE)
modelA = lmer(AvgEBR ~ prepost*zcomposite + (1|Subject) + Error(prepost/Subject), data = blinksprepost[blinksprepost$stim == "A",], REML = TRUE)
modelS = lmer(AvgEBR ~ prepost*zcomposite + (1|Subject), data = blinksprepost[blinksprepost$stim == "S",], REML = TRUE)

# extract coefficients
coefs <- data.frame(coef(summary(model)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs


interact_plot(modelA, pred = "prepost", modx = "zcomposite", plot.points = TRUE, y.label = "Anodal")
interact_plot(modelS, pred = "prepost", modx = "zcomposite", plot.points = TRUE, y.label = "Anodal")

# extract coefficients
coefs <- data.frame(coef(summary(modelA)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

# extract coefficients
coefs <- data.frame(coef(summary(modelS)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

model = lmer(AvgEBR ~ stim*prepost + (1|Subject), data = blinksprepost)
# extract coefficients
coefs <- data.frame(coef(summary(model)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

