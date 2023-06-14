# ANALYSIS.R
#
remove(list = ls()) # clear all

###############################################################################
#
# GENERAL INFORMATION BEFORE USING:
#
# This script was written to analyse the data for the study
# "THE EFFECT OF VIGILANCE ON A LINE BISECTION TASK" (working title)
# and is still in progress.
# It was basically structured by using the "AsPredicted" registration.
#
#
# First of all, you need the statistics software R (and maybe an editor)
# (see https://www.r-project.org/).
# In order to grasp the essential results, you do not need advanced knowledge 
# about R or coding in general. After running this script, almost everything 
# you need will be displayed in the R console. Moreover, the short comments 
# should help you to understand the function of every part.
# However, it is easier to follow if you are familiar with basic concepts 
# (e.g. see https://cran.r-project.org/doc/manuals/r-release/R-intro.pdf).
#
#
# After that, follow these steps to run this script (note that there are other 
# solutions which might be more elegant; feel free to adjust the code):
#
# - bring your data in the correct structure and format
# (see the attached input files to get an impression) 
# - transfer the relevant parts from the automatically constructed files to 
# another and convert them to .csv-files
# - open R and set your working directory by typing (see an example below):
# setwd("PATH TO YOUR CSV-FILES")
# - select all, copy and paste it from this script in the R console or 
# type:
# source("ANALYSIS.R", echo = T)
# - everything (code and results) should be displayed now (and several 
# figures pop up)
# - install required packages and uncomment where you are asked to in the
# code
# - to learn more about the functions, type:
# ?<function>, e.g. ?sum
#
#
# ------------------
# YOU MAY NEED THIS:
# ------------------
#
# install packages once with: 
# install.packages("effsize")
#
# loading libraries (uncomment next line after installing packages)
# library(effsize)
#
# ------------------
#
#
# If you find mistakes or have any further questions about the code, 
# you are welcome to contact me via e-mail:
# dominik.bauder@student.uni-tuebingen.de
#
# For any questions about the study in general, contact the principal 
# investigator:
# stefan.smaczny@uni-tuebingen.de
#
###############################################################################
#
# input: 	demography_02-20.csv
#		McInt_Vig_04-20.csv
#		McInt_Vig_cue_04-20.csv
#		McInt_Vig_vig_04-20.csv
# output:	--
#
#
# set working directory
 setwd("C:/Users/ssmaczny.NEUROLOGIE/Desktop/Line Bisection/McIntosh_LB")
###############################################################################

### STANFORD SLEEPINESS SCALE (SSS) AND VIGILANCE TASK (EXCLUSION CRITERIA) ###
## data sets of every task
dat.d <- read.csv("McInt_Vig_04-20.csv")
dat.d$cond <-factor(dat.d$cond, levels = c("pre","post"))
dat.s <- read.csv("McInt_Vig_cue_04-20.csv")
dat.s$cond <-factor(dat.s$cond, levels = c("pre","post"))
dat.vig <- read.csv("McInt_Vig_vig_04-20.csv")

# exclude participant 88 for being too sleepy and having a low r-square
dat.d <- subset(dat.d,code!=88)
dat.s <- subset(dat.s,code!=88)
dat.vig <- subset(dat.vig,code!=88)




# reaction time threshold: .15 to 1 sec (180 to 1000 ms)

dat.vig <- dat.vig[dat.vig$rt < 1,]
# The calculation of this value is based on the following (see the .py-script):
# The distance between the lowest point the black line can be when a trial
# begins (posY = 4) and the point where the error message occurs (posY = 7) 
# is 3. This value describes the average time it would take for the line to 
# move this distance.
#
# speedManipulator = 0.1
# lineSpeed = 0.05
#
# Once a trial begins, the black line always moves up by the rate of:
#    posY += change*difspeed
#
# whereby 
#   difspeed = randint(1,20)*speedManipulator
#   change = lineSpeed
#
# Therefore, the average position change would be
#   posY += lineSpeed * 10.5 * speedManipulator
#        += 0.05 * 10.5 * 0.1 
#        += 0.0525
#
# This means, on average the posY of the black line moves upwards by 0.0525 
# values, which would require
#   3 / 0.0525 ~= 58 monitor flips
#
# 1 monitor flip takes on average 0.0167 seconds, 
# therefore 58 * 0.0167 = 0.9686 seconds.
# This was rounded up to 1.

dat.vig <- dat.vig[dat.vig$rt > .15,]
# The reaction time of 150 ms is like a human threshold to detect visual stimuli ( ... ).








# 
# 
# ## SSS-criteria
 names(dat.d)[names(dat.d) == "mouse.clicked_name"] <- "sleepiness.d"
 names(dat.s)[names(dat.s) == "mouse.clicked_name"] <- "sleepiness.s"
 dat.sss <- dat.d[c("code", "cond", "balance", "sleepiness.d")]
 dat.sss$sleepiness.s <- dat.s$sleepiness


# ----------------------------------------------------------------------------------------------
## ADDITIONS BY STEFAN

library(tidyr)
library(dplyr)
library(ggplot2)
library(effsize)
library(plotly)
library('ez')
library(pastecs)
library(Hmisc)

# ----------------------------------------------------------------------------------------------

## RT-criteria (vigilance task)

 # RT per (FALSE) participant
dat.vig.rt <- NULL
for(i in 1:(max(dat.d$code))) {
  dat.vig.time <- dat.vig[dat.vig$code == i,]$rt
  dat.vig.rt[i] <- list(na.omit(dat.vig.time))
}

# length of first and last 10 minutes (per (FALSE) participant) 
# so that we know which RT we want to compare in dat.vig.rt
dat.vig.ss <- NULL
for(i in 1:(max(dat.d$code))) {
  dat.vig.sst <- dat.vig[dat.vig$code == i,]$stimulus.started
  dat.vig.ss[i] <- list(na.omit(dat.vig.sst/60))
}

dat.vig.ss.index <- NULL
for(i in 1:(max(dat.d$code))) {
  temp <- c(length(dat.vig.ss[i][[1]][dat.vig.ss[i][[1]] <= 10]), 
            length(dat.vig.ss[i][[1]][dat.vig.ss[i][[1]] >= tail(dat.vig.ss[i][[1]], 1)-10]))
  dat.vig.ss.index[i] <- list(temp)
}


# carry out t-test of reaction times
rt.comp <- data.frame()
for(i in 1:(max(dat.d$code))) {
  temp.pre <- mean(dat.vig.rt[i][[1]][1:dat.vig.ss.index[i][[1]][1]])
  temp.post <- mean(dat.vig.rt[i][[1]][(length(dat.vig.rt[i][[1]])
                                     -dat.vig.ss.index[i][[1]][2]+1):(length(dat.vig.rt[i][[1]]))])
  rt.comp[i,1] <- temp.pre
  rt.comp[i,2] <- temp.post
}
t.test(rt.comp[,1],rt.comp[,2],paired=TRUE)

# carry out t-test of SSS
t.test(subset(dat.sss,cond=="pre"&code>45)$sleepiness.d,subset(dat.sss,cond=="post"&code>45)$sleepiness.d,paired=TRUE)





### MAIN HYPOTHESIS ###

## LB WITHOUT SPATIAL CUEING (McInt_Vig_pre/post)
# adjusted data sets
dat.d.pre <- dat.d[dat.d$cond == "pre",]
dat.d.post <- dat.d[dat.d$cond == "post",]

# EWB
dat.d.pre.ewb <- dat.d.pre$EWB
dat.d.post.ewb <- dat.d.post$EWB
t.test(dat.d.pre.ewb, dat.d.post.ewb, paired = T)
mean(dat.d.pre.ewb)
sd(dat.d.pre.ewb)
mean(dat.d.post.ewb)
sd(dat.d.post.ewb)
cohen.d(dat.d.pre.ewb, dat.d.post.ewb, paired = T)

 ewb.prepost <- ggplot(dat.d,aes(x=cond,y=EWB)) +
   geom_boxplot() +
   geom_point() +
   geom_line(aes(group=as.factor(code)),size=0.1) + 
   theme_bw() +
   theme(panel.grid.major = element_blank()) +
   ylab("EWB") +
   xlab("No cue") +
   theme(axis.text=element_text(size=15),
         axis.title=element_text(size=15))
 ewb.prepost 
 
 
 
# EWS
dat.d.pre.ews <- dat.d.pre$EWS
dat.d.post.ews <- dat.d.post$EWS
t.test(dat.d.pre.ews, dat.d.post.ews, paired = T)
mean(dat.d.pre.ews)
sd(dat.d.pre.ews)
mean(dat.d.post.ews)
sd(dat.d.post.ews)
cohen.d(dat.d.pre.ews, dat.d.post.ews, paired = T)

ews.prepost <- ggplot(dat.d,aes(x=cond,y=EWS)) +
  geom_boxplot() +
  geom_point() +
  geom_line(aes(group=code),size=0.1) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  ylab("EWS") +
  xlab("No cue") +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15))
  ews.prepost 


# DBE
dat.d.dbe.pre <- dat.d.pre[c(1,2,14:17)] 
dat.d.dbe.pre$meanB <- dat.d.dbe.pre$meanB + 2
dat.d.dbe.pre$meanC <- dat.d.dbe.pre$meanC - 2
dat.d.dbe.pre$mean <- rowMeans(dat.d.dbe.pre[c(3:6)])

dat.d.dbe.post <- dat.d.post[c(1,2,14:17)] 
dat.d.dbe.post$meanB <- dat.d.dbe.post$meanB + 2
dat.d.dbe.post$meanC <- dat.d.dbe.post$meanC - 2
dat.d.dbe.post$mean <- rowMeans(dat.d.dbe.post[c(3:6)])

t.test(dat.d.dbe.pre$mean, dat.d.dbe.post$mean, paired = T)
mean(dat.d.dbe.pre$mean)
sd(dat.d.dbe.pre$mean)
mean(dat.d.dbe.post$mean)
sd(dat.d.dbe.post$mean)
cohen.d(dat.d.dbe.pre$mean, dat.d.dbe.post$mean, paired = T)

dat.dbe <- cbind(rbind(dat.d.dbe.pre,dat.d.dbe.post))

dbe.prepost <- ggplot(dat.dbe,aes(x=cond,y=mean)) +
  geom_boxplot() +
  geom_point() +
  geom_line(aes(group=as.factor(code)),size=0.1) + 
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  ylab("DBE") +
  xlab("No cue") +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15))
dbe.prepost 


## 2) LB WITH SPATIAL CUEING (McInt_Vig_cue_pre/post)
# adjusted data sets
dat.s.pre <- dat.s[dat.s$cond == "pre",]
dat.s.post <- dat.s[dat.s$cond == "post",]

# EWB
dat.s.pre.ewb <- dat.s.pre[c(1,2,12,29)]
dat.s.post.ewb <- dat.s.post[c(1,2,12,29)]
dat.av <- rbind(dat.s.pre.ewb,dat.s.post.ewb)
dat.av <- rename(dat.av,"left" = leftEWB)
dat.av <- rename(dat.av,"right" = rightEWB)

dat.av <- gather(dat.av,key,value,-code,-cond)
colnames(dat.av) <- c('code','cond','cue','EWB')

# ANOVA of cued condition?
EWB.anova <- ezANOVA(data=dat.av, dv = .(EWB), wid = .(code), within = .(cond,cue), type = 3, detailed = TRUE)
EWB.anova
dat.av$condcue <- paste(dat.av$cond,dat.av$cue,sep="_")
dat.av$condcue<- factor(dat.av$condcue,levels = c("pre_leftEWB","post_leftEWB","pre_rightEWB","post_rightEWB"))

newgraph.EWB <- ggplot(dat.av,aes(x=cond,y=EWB,group=cue)) +
  stat_summary(fun=mean, geom="point",aes(colour=cue),size=4) +
  stat_summary(fun.data = mean_cl_boot, 
               geom = "errorbar", 
               width = .2,
               aes(colour=cue),
               size=2) +
  stat_summary(fun=mean, 
               geom="line", 
               aes(colour=cue),
               size=2) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=40)) 
newgraph.EWB  

pre.EWB <- subset(dat.av,cond=="pre")
post.EWB <- subset(dat.av,cond=="post")

# EWS
dat.s.pre.EWS <- dat.s.pre[c(1,2,13,30)]
dat.s.post.EWS <- dat.s.post[c(1,2,13,30)]
dat.av <- rbind(dat.s.pre.EWS,dat.s.post.EWS)
dat.av <- rename(dat.av,"left" = leftEWS)
dat.av <- rename(dat.av,"right" = rightEWS)

dat.av <- gather(dat.av,key,value,-code,-cond)
colnames(dat.av) <- c('code','cond','cue','EWS')

pre.EWS <- subset(dat.av,cond=="pre")
post.EWS <- subset(dat.av,cond=="post")

# ANOVA of cued condition?
EWS.anova <- ezANOVA(data=dat.av, dv = .(EWS), wid = .(code), within = .(cond,cue), type = 3, detailed = TRUE)
EWS.anova

by(dat.av$EWS, list(dat.av$cond), stat.desc, basic = FALSE)
by(dat.av$EWS, list(dat.av$cue), stat.desc, basic = FALSE)

newgraph.EWS <- ggplot(dat.av,aes(x=cond,y=EWS,group=cue)) +
  stat_summary(fun=mean, geom="point",aes(colour=cue),size=4) +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = .2,
               aes(colour=cue),
               size=2) +
  stat_summary(fun=mean, 
               geom="line", 
               aes(colour=cue),
               size=2) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=40)) 
newgraph.EWS  


# DBE
dat.s.pre.DBE <- dat.s.pre[c(1,2,14:17,31:34)]
dat.s.post.DBE <- dat.s.post[c(1,2,14:17,31:34)]
dat.s.DBE <- rbind(dat.s.pre.DBE,dat.s.post.DBE)
dat.s.DBE$leftmean <- rowMeans(dat.s.DBE[3:6])
dat.s.DBE$rightmean <- rowMeans(dat.s.DBE[7:10])
dat.s.DBE <- dat.s.DBE[c(1,2,11,12)]

dat.s.DBE <- rename(dat.s.DBE,"left" = leftmean)
dat.s.DBE <- rename(dat.s.DBE,"right" = rightmean)

dat.av <- gather(dat.s.DBE,key,value,-code,-cond)

colnames(dat.av) <- c('code','cond','cue','DBE')

pre.DBE <- subset(dat.av,cond=="pre")
post.DBE <- subset(dat.av,cond=="post")

# ANOVA of cued condition?
DBE.anova <- ezANOVA(data=dat.av, dv = .(DBE), wid = .(code), within = .(cond,cue), type = 3, detailed = TRUE)
DBE.anova
dat.av$condcue <- paste(dat.av$cond,dat.av$cue,sep="_")
by(dat.av$DBE, list(dat.av$cue), stat.desc, basic = FALSE)


newgraph.DBE <- ggplot(dat.av,aes(x=cond,y=DBE,group=cue)) +
  stat_summary(fun=mean, geom="point",aes(colour=cue),size=4) +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = .2,
               aes(colour=cue),
               size=2) +
  stat_summary(fun=mean, 
               geom="line", 
               aes(colour=cue),
               size=2) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=40)) 
newgraph.DBE  




# DELTAS
# or "Comparison of the effect of alertness reduction in the cued and uncued line bisection task"
# ---------------------------------------------------------------------
dif.EWB <- pre.EWB
dif.EWB$EWB <- pre.EWB$EWB - post.EWB$EWB
nocue <- dat.d[c(1,2,12)]
nocue <- reshape(nocue, timevar = "cond", idvar = "code", dir = "wide")
nocue$dif <- nocue$EWB.pre-nocue$EWB.post
temp <- subset(dif.EWB,dif.EWB$cue=="left")
temp$cue <- "none"
temp$EWB <- nocue$dif
dif.EWB <-rbind(dif.EWB,temp)

EWB.delta <- ezANOVA(dif.EWB,
                     dv=.(EWB),
                     wid=.(code),
                     within=.(cue),
                     type=3,
                     detailed=TRUE)
EWB.delta

delta.EWB <- ggplot(dif.EWB,aes(x=cue,y=EWB)) +
  geom_violin(aes(group=cue)) +
  geom_point(aes(group=cue)) +
  stat_summary(fun=mean, geom="point",aes(colour=cue),size=4) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               width = .2,
               aes(colour=cue),
               size=2) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=40))
delta.EWB


dif.EWS <- pre.EWS
dif.EWS$EWS <- pre.EWS$EWS - post.EWS$EWS
nocue <- dat.d[c(1,2,13)]
nocue <- reshape(nocue, timevar = "cond", idvar = "code", dir = "wide")
nocue$dif <- nocue$EWS.pre-nocue$EWS.post
temp <- subset(dif.EWS,dif.EWS$cue=="left")
temp$cue <- "none"
temp$EWS <- nocue$dif
dif.EWS <-rbind(dif.EWS,temp)


EWS.delta <- ezANOVA(dif.EWS,
                     dv=.(EWS),
                     wid=.(code),
                     within=.(cue),
                     type=3,
                     detailed=TRUE)
EWS.delta

delta.EWS <- ggplot(dif.EWS,aes(x=cue,y=EWS)) +
  stat_summary(fun=mean, geom="point",aes(colour=cue),size=4) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               width = .2,
               aes(colour=cue),
               size=2) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=40))
delta.EWS


dif.DBE <- pre.DBE
dif.DBE$DBE <- pre.DBE$DBE - post.DBE$DBE
temp <-rowMeans(dat.d[c(14:17)])
nocue <- cbind(dat.d[c(1,2)],temp)
nocue <- reshape(nocue, timevar = "cond", idvar = "code", dir = "wide")
colnames(nocue) <- c("code","pre","post")
nocue$dif <- nocue$pre - nocue$post
temp <- subset(dif.DBE,dif.DBE$cue=="left")
temp$cue <- "none"
temp$DBE <- nocue$dif
colnames(temp) <- c("code","cond","cue","DBE")
dif.DBE <-rbind(dif.DBE,temp)

DBE.delta <- ezANOVA(dif.DBE,
                     dv=.(DBE),
                     wid=.(code),
                     within=.(cue),
                     type=3,
                     detailed=TRUE)
DBE.delta


delta.DBE <- ggplot(dif.DBE,aes(x=cue,y=DBE)) +
  geom_violin(aes(group=cue)) +
  geom_point(aes(group=cue)) +
  stat_summary(fun=mean, geom="point",aes(colour=cue),size=4) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               width = .2,
               aes(colour=cue),
               size=2) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=40))
delta.DBE
