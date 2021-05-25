library(dplyr)
library(car)
library(ggplot2)
library(hrbrthemes)
library(tidyr)
library(viridis)
library(bbmle)
library(stats4)
library(corrplot)
library(maptools)
library(writexl)
library(ggpubr)
library(tidyr)
library(rstatix)
library(writexl)
library(readxl)
library(plotrix)
getwd()
#### Import and prepare data ####
finaldata_adults <- read_excel("finaldata_adults.xlsx")
#final.data = pups included, final.data.adults = eemsdollard pups removed
dat = finaldata_adults # we edit this data and will eventually work with object dat2
#dat=final.data
pv = subset(dat, dat$species == "pv")
hg = subset(dat, dat$species == "hg")
summary(dat)


#adults size limit eemsdollard
hist(eemsdollard.finaldata$`seal surface area (m2)`, breaks=100, xlim=c(0,1), xlab="surface area (m2)")
lines(density(eemsdollard.finaldata$`seal surface area (m2)`), col="blue")

#### Seal sizes & species determination ####
# rif, renesse, noorderh, goedereede unkown species
noorderhaaks = subset(dat, dat$`haul-out site`=="noorderhaaks")
hist(noorderhaaks$`seal surface area (m2)`, breaks=100, xlim=c(0,2), axes=T, freq=F)

hist(renesse.finaldata$`seal surface area (m2)`, breaks=50, xlim=c(0,2), ylim=c(0,3), axes=T, freq=F, xlab="surface area (m2)")
#hist(pv$`seal surface area (m2)`, breaks = 50, add=T, freq=F, col=F, border="coral")
#hist(hg$`seal surface area (m2)`, breaks = 100, add=T, freq=F, col=F, border="turquoise")
abline(v=min(rif.finaldata$`seal surface area (m2)`), col="blue", lwd=2, lty=2)
abline(v=min(richel.finaldata$`seal surface area (m2)`), col="blue", lwd=2, lty=2)
abline(v=min(goedereede.finaldata$`seal surface area (m2)`), col="blue", lwd=2, lty=2)

abline(v=mean(pv$`seal surface area (m2)`), col="blue", lwd=2, lty=1)
text(0.3, 3., col="blue", paste("mean PV"), cex=.8)
abline(v=mean(hg$`seal surface area (m2)`), col="red", lwd=2)
text(0.85, 3, col="red", paste("mean HG"), cex=.8)
abline(v=mean(noorderhaaks.finaldata$`seal surface area (m2)`), col="orange", lwd=2, lty=1)

mean(renesse$`seal surface area (m2)`)
# Rif --> kleiner dan avg PV, dus probably PV
  # datum= 30 maart, dan zijn HG aan het verharen dus die zitten dan vooral op andere plekken
# Renesse mix
# noorderhaaks; lastig, we zouden er HG of mix verwachten, HG pups zouden er kunnen zitten
# goedereede --> PV

## correct the species ##
renesse = subset(dat, dat$`haul-out site`=="renesse")
renesse$species = "mix"  # for renesse, HG pups may be present, so we cannot safely split this data based on size
renesse.new = renesse
noorderhaaks = subset(dat, dat$`haul-out site`=="noorderhaaks")
noorderhaaks$species = "mix"
noordh.hg = subset(noorderhaaks, noorderhaaks$`seal surface area (m2)` >= 0.85) #OR  (mean(hg$`seal surface area (m2)`)-0.1))
noordh.hg$species = "hg"
noordh.mix = subset(noorderhaaks, noorderhaaks$`seal surface area (m2)` <= 0.85)# OR(mean(hg$`seal surface area (m2)`)-0.1))
noordh.mix$species = "mix"
#### new dataset to work with ####
dat2 = rbind(pv, hg, renesse, noordh.mix, noordh.hg)
dat2$`NND rndm KDE (m)`[dat2$`NND rndm KDE (m)` <= 0.1] <- 0.1 
dat2$`nnd (m)`[dat2$`nnd (m)` <= 0.1] <- 0.1
write_xlsx(dat2, "/Users/Anne/Documents/Internship WMR zeehonden/Data Analyses/dat2_adults_pvhgmix.xlsx")
# new subsets:
pv2 = subset(dat2, dat2$species == "pv")
pv3 = subset(pv2, pv2$`haul-out site` != "eemsdollard") # zonder eemsdollard data
hg2 = subset(dat2, dat2$species == "hg")
mix = subset(dat2, dat2$species == "mix")

sd(pv3$`number of seals within 5m distance`)

std.error(pv3$`NND rndm KDE (m)`)
std.error(pv3$`nnd rndm kde.b (m)`)

quantile(hg2$`NND rndm KDE (m)`, probs = seq(.9, by = .1))

#### STATISTICAL TESTS ####

#### LOG t-test ####
# log transformed > simple t-test
log.nnd.data = log(dat2$`nnd (m)`+0.01)
log.nnd.rndm = log(dat2$`nnd rndm kde.b (m)`+0.01)
hist(log.nnd.data)

# t-test on the log-transformed data
t.test(log.nnd.data, log.nnd.rndm) #significant
t.test(log.pv.nnd, log.hg.nnd) #significant
# log, ttest dat~rndm per species (test de mean van de dataset)
log.pv.nnd = log(pv2$`nnd (m)`+0.01)
log.hg.nnd = log(hg2$`nnd (m)`+0.01)
log.pv.r = log(pv2$`nnd rndm kde.b (m)`+0.01)
log.hg.r = log(hg2$`nnd rndm kde.b (m)`+0.01) #limited to 0-10 m distances nnd
t.test(log.pv.r, log.pv.nnd)
t.test(log.hg.r, log.hg.nnd)
t.test(log.hg.r, log.hg.nnd)

wilcox.test(pv2$`nnd rndm kde.b (m)`, pv2$`nnd (m)`, paired=F)  #wilcox, limited 0-10m data
wilcox.test(hg2$`nnd rndm kde.b (m)`, hg2$`nnd (m)`, paired=F)  #wilcox, limited 0-10m data

wilcox.test(pv2$`nnd rndm kde.b (m)`, pv2$`NND rndm KDE (m)`, paired=F)  #wilcox, limited 0-10m data
wilcox.test(hg2$`nnd rndm kde.b (m)`, hg2$`NND rndm KDE (m)`, paired=F)  #wilcox, limited 0-10m data

#### multi pairwise comp ####
set.seed(1234)

kruskal.test(glmdata$nnd ~ glmdata$randomness) # different randomness
kruskal.test(glmdata.hg$nnd ~ glmdata.hg[,4])#sites hg
kruskal.test(glmdata.pv$nnd ~ glmdata.pv[,4])# haul out sites pv
wilcox.test(pv2$`nnd (m)`, hg2$`nnd (m)`)#species

# Sign diff between the groups
krusk.pv = kruskal.test(glmdata.pv$nnd ~ glmdata.pv$randomness)
krusk.pv
krusk.hg = kruskal.test(glmdata.hg$nnd ~ glmdata.hg$randomness)
krusk.hg
# A significant Kruskal-Wallis test is generally followed up by Dunn’s test to identify which groups are different. It’s also possible to use the Wilcoxon’s test to calculate pairwise comparisons between group levels with corrections for multiple testing.
dunn_test(glmdata, nnd ~ randomness, p.adjust.method = "bonferroni")
dunn_test(glmdata.pv, nnd ~ randomness, p.adjust.method = "bonferroni")
dunn_test(glmdata.hg, nnd ~ randomness, p.adjust.method = "bonferroni")

## Man withney U assumes that data is independent. Out data is dependent/paired because the new location is dependent on the old
## Wilcox: H0: difference between the pairs follows a symmetric distribution around zero
# H1: difference between the pairs does not follow a symmetric distribution around zero.
# data vs random a / b
wilcox.test(dat2$`nnd (m)`, dat2$`nnd rndm kde.b (m)`, paired = F, p.adjust.method = "bonferroni") #dat x b
wilcox.test(dat2$`nnd (m)`, dat2$`NND rndm KDE (m)`, paired = F, p.adjust.method = "bonferroni") # dat x a
wilcox.test(dat2$`nnd rndm kde.b (m)`, dat2$`NND rndm KDE (m)`, paired = F, p.adjust.method = "bonferroni") # a x b

## Per species
#Random B x or
wilcox.test(pv2$`nnd (m)`, pv2$`nnd rndm kde.b (m)`, paired = F, p.adjust.method = "bonferroni")
wilcox.test(hg2$`nnd (m)`, hg2$`nnd rndm kde.b (m)`, paired = F, p.adjust.method = "bonferroni")

# random A x or
wilcox.test(pv2$`nnd (m)`, pv2$`NND rndm KDE (m)`, paired = F)
wilcox.test(hg2$`nnd (m)`, hg2$`NND rndm KDE (m)`, paired = F)

#random a x b
wilcox.test(pv2$`nnd rndm kde.b (m)`, pv2$`NND rndm KDE (m)`, paired = F)
wilcox.test(hg2$`nnd rndm kde.b (m)`, hg2$`NND rndm KDE (m)`, paired = F)

summary(pv2$`nnd (m)`)
summary(pv2$`NND rndm KDE (m)`)
summary(pv2$`nnd rndm kde.b (m)`)

summary(hg2$`nnd (m)`)
summary(hg2$`NND rndm KDE (m)`)
summary(hg2$`nnd rndm kde.b (m)`)


#### Analyses data ~ rndm ####
# 1) Check for normality (full dataset)
#actual data
hist(dat2$`nnd (m)` , breaks=200, xlim=c(0,10), freq=F)
qqnorm(dat2$`nnd (m)`)
qqline(dat2$`nnd (m)`, col="blue")
shapiro.test(sample(dat2$`nnd (m)`, size=5000))
#rndm data
hist(dat2$`NND rndm KDE (m)`, breaks=200, xlim=c(0,10), freq=F)
qqnorm(dat2$`NND rndm KDE (m)`)
qqline(dat2$`NND rndm KDE (m)`, col="blue")
shapiro.test(sample(dat2$`NND rndm KDE (m)`, size=5000))
# normality violated, but Central Limit Theorem????
# With large enough sample sizes (n > 30) the violation of the normality assumption should not cause major problems (central limit theorem). This implies that we can ignore the distribution of the data and use parametric tests.


#### boxplots NND ####
# dat, rndm.a, rndmb
dat3 = rbind(pv3, hg2) # pv + hg zonder dollard
boxplot(dat3$`nnd (m)`, dat3$`NND rndm KDE (m)`, dat3$`nnd rndm kde.b (m)`, horizontal=T, ylim=c(0,5), 
        names = c("original data", "random (A)", "random (B)"), xlab="Nearest neighbour distance (m) ", las=1,
        col=c("seagreen1", "seagreen3", "seagreen"), cex.axis=0.8, outcol="grey", outwex=1.5, outline=F)

## hg pv splitted
boxplot(pv3$`nnd (m)`, pv3$`NND rndm KDE (m)`,pv3$`nnd rndm kde.b (m)`,  hg2$`nnd (m)`, hg2$`NND rndm KDE (m)`, hg2$`nnd rndm kde.b (m)`,
        main = "NND's",
        at = c(1,2,3, 5,6,7),
        names = c("original", "random A", "random B", "original", "random A", "random B"), 
        cex.axis = .85, outcol = "grey", cex.lab = 1.0,
        las = 1, 
        col = c("thistle2", "thistle", "thistle4","cadetblue1", "cadetblue3", "cadetblue"),
        border = "black", 
        horizontal = T,
        notch = F,
        ylim=c(0,10), xlab="Nearest neighbour distance (m)", ylab="PV, HG")

#### glm data defined ####
nnd.dat = as.data.frame(dat3$`nnd (m)`)
nnd.ra = as.data.frame(dat3$`NND rndm KDE (m)`)
nnd.rb = as.data.frame(dat3$`nnd rndm kde.b (m)`)
colnames(nnd.dat) = "nnd"
colnames(nnd.ra) = "nnd"
colnames(nnd.rb) = "nnd"
dist = rbind(nnd.dat, nnd.ra, nnd.rb)
randomness = (rep(c("original","random A", "random B"), each=nrow(nnd.dat)))
species.glmdat = rbind(as.data.frame(dat3$species), as.data.frame(dat3$species), as.data.frame(dat3$species))
region.glmdat = rbind(as.data.frame(dat3$`haul-out site`), as.data.frame(dat3$`haul-out site`), as.data.frame(dat3$`haul-out site`))
date.glmdat = rbind(as.data.frame(dat3$`date image`), as.data.frame(dat3$`date image`), as.data.frame(dat3$`date image`))
glmdata = cbind(dist, randomness,species.glmdat, region.glmdat, date.glmdat) #also needed for GLM

glmdata.pv = as.data.frame(subset(glmdata, glmdata$`dat3$species` == "pv"))
glmdata.hg = subset(glmdata, glmdata$`dat3$species` == "hg")

#pv.ggbox = 
ggplot(data = glmdata.pv,
       aes(x =nnd, y = randomness, fill=randomness)) +
  geom_boxplot() +
  stat_boxplot(geom ='errorbar', width = 0.5)+
  geom_boxplot() +
  xlab("Nearest neighbour distance (m)") +
  ylab("") +
  labs(title = "PV")+
  xlim(0,10) + scale_fill_discrete(name = "Data")+
  scale_fill_brewer(palette="BuPu")+
  theme(legend.position = "none" )


#hg.ggbox = 
ggplot(data = glmdata.hg,
       aes(x = nnd, y = randomness, fill=randomness)) +
  geom_boxplot() +
  stat_boxplot(geom ='errorbar', width = 0.5)+
  geom_boxplot() +
  xlab("Nearest neighbour distance (m)") +
  ylab("") +
  labs(title = "HG")+
  xlim(0,5) + scale_fill_discrete(name = "Data")+
  scale_fill_brewer(palette="BuPu")+
  theme(legend.position = "none")
  
#ggarrange(pv.ggbox, hg.ggbox)
  

#### Visualize compare data ~ randomized ####


## 2) smoothed density ggplot data~random per species
#ggplot(data=glmdata.pv, aes(x=nnd, group=randomness, fill=randomness, color=randomness)) +
glmdata.pv.nodol = subset(glmdata.pv, glmdata.pv[,4]!= "eemsdollard")
ggplot(data=glmdata.pv.nodol, aes(x=nnd, group=randomness, fill=randomness, color=randomness)) +
 geom_density(adjust=.8, alpha=0.3) +
  theme_ipsum()+
  labs(title="Smoothed density PV",x="Nearest neighbour distance (m)", y = "Density")+
  xlim(0,5) + scale_fill_discrete(name = "Data")

ggplot(data=glmdata.hg, aes(x=nnd, group=randomness, fill=randomness, color=randomness)) +
  geom_density( adjust=.8, alpha=0.3) +
  theme_ipsum()+
  labs(title="Smoothed density HG",x="Nearest neighbour distance (m)", y = "Density")+
  xlim(0,5) + scale_fill_discrete(name = "Data")  #+
  #scale_fill_manual(values=c("maroon", "mediumseagreen", "cornflowerblue"))+
  #scale_color_manual(values=c("maroon", "mediumseagreen", "cornflowerblue"))

plot(glmdata.hg$nnd, cex=0.1)
points( glmdata.pv$nnd, add=T, col="red", cex=.1)

## 3) bins
# precies even veel waarnemingen, als ze identiek zijn moet de waarde ongeveer 0.5 zijn
# kleiner dan 0.5 = komt vaker voor in rndm
# groter dan 0.5 = komt vaker voor in data
dat4 = subset(dat2, dat2$`nnd (m)` <= 10) #limit the data
# all data
hist(dat4$`seal surface area (m2)`)
hist.dat = hist(dat4$`nnd (m)`, xlim=c(0,5), breaks=seq(from=0, to=400, by=.25))
hist.rndm = hist(dat4$`NND rndm KDE (m)`, xlim=c(0,5), breaks=seq(from=0, to=400, by=.25))
plot(hist.dat$counts / (hist.rndm$counts + hist.dat$counts), xlim=c(0,20)) # x is number of bins
abline(h=0.5, col="blue", lty = 2)

#### bins ####
#pv
#hist(pv2$`seal surface area (m2)`)
hist.dat = hist(pv2$`nnd (m)`, xlim=c(0,5), breaks=seq(from=0, to=400, by=.1))
hist.rndm = hist(pv2$`NND rndm KDE (m)`, xlim=c(0,5), breaks=seq(from=0, to=400, by=.1))
plot(hist.dat$counts / (hist.rndm$counts + hist.dat$counts), xlim=c(0,20), main="pv ratio 10cm bins") # x is number of bins
abline(h=0.5, col="blue", lty = 2)
# niet de ratio maar absolute counts per bin

hist.rndm.b = hist(pv2$`nnd rndm kde.b (m)`, xlim=c(0,5), breaks=seq(from=0, to=400, by=.1))
plot(hist.dat$counts / (hist.rndm$counts + hist.dat$counts), xlim=c(0,60), main="pv ratio 10cm bins") # x is number of bins
abline(h=0.5, col="blue", lty = 2)

plot(hist.dat$counts, cex=0.9, xlim=c(0,40), ylim=c(0,150), main= "PV 10cm bins absolute counts, red=random")
points(hist.rndm.b$counts, cex=0.9, add=T, col="red")
points(hist.rndm$counts, cex=0.9, add=T, col="blue")

# hg
#hist(hg2$`seal surface area (m2)`)
hist.dat = hist(hg2$`nnd (m)`, xlim=c(0,5), breaks=seq(from=0, to=400, by=.1))
hist.rndm = hist(hg2$`NND rndm KDE (m)`, xlim=c(0,5), breaks=seq(from=0, to=400, by=.1))
plot(hist.dat$counts / (hist.rndm$counts + hist.dat$counts), xlim=c(0,20), main="hg ratio 10cm bins") # x is number of bins
abline(h=0.5, col="blue", lty = 2)
plot(hist.dat$counts, cex=0.5, xlim=c(0,40), ylim=c(0,900), main= "HG 10cm bins absolute counts, red=random")
points(hist.rndm$counts, cex=0.5, add=T, col="red")

#### kleine afstanden? ####
# binoomiaal model?
# kansverdeling; 1000 zeehonden, scen. 1 = 50 zeehonden binnen 0-50 cm, random: bijv 100 vd 1000

dat<-data.frame(S=c(50,100),T=c(1000-50,1000-100),cat=c("real","random"))
summary(glm(cbind(S,T)~cat,data=dat,family="binomial")) # Look at "catreel" = effect vd variabele

# success = S# "estimate" catreal kleiner, 
# HG
hist.dat = hist(hg2$`nnd (m)`, xlim=c(0,10), breaks=seq(from=0, to=400, by=.5))
hist.rndm.a = hist(hg2$`NND rndm KDE (m)`, xlim=c(0,10), breaks=seq(from=0, to=400, by=.5))
hist.rndm.b = hist(hg2$`nnd rndm kde.b (m)`, xlim=c(0,10), breaks=seq(from=0, to=400, by=.5))

print(hist.dat$counts) 
sum(hist.dat$counts) 
print(hist.rndm.a$counts ) 
print(hist.rndm.b$counts) 
#1389, 1530, 1073


# en zonder eemsdollard?
pv.nondol = subset(pv2, pv2$`haul-out site` != "eemsdollard")
hist.dat.ndol = hist(pv.nondol$`nnd (m)`, xlim=c(0,10), breaks=seq(from=0, to=400, by=1.))
hist.rndm.a.ndol = hist(pv.nondol$`NND rndm KDE (m)`, xlim=c(0,10), breaks=seq(from=0, to=400, by=1.))
hist.rndm.b.ndol = hist(pv.nondol$`nnd rndm kde.b (m)`, xlim=c(0,10), breaks=seq(from=0, to=400, by=1.5))
print(hist.dat.ndol$counts)
print(hist.rndm.a.ndol$counts)
print(hist.rndm.b.ndol$counts)
# 105, 232, 164 (ori, a, b) == zonder eemsdollard totaal=854
sum(hist.rndm.a.ndol$counts)

#hoe groot kan pv rndm a worden?
dat<-data.frame(S=c(309,353),T=c(854-309,854 - 353),cat=c("real","random"))
summary(glm(cbind(S,T)~cat,data=dat,family="binomial"))

# test 0-50 cm
dat<-data.frame(S=c(105,232),T=c(854-105,854 - 232),cat=c("real","random"))
summary(glm(cbind(S,T)~cat,data=dat,family="binomial"))
dat<-data.frame(S=c(105,164),F=c(854-105,854-164),cat=c("real","random"))
summary(glm(cbind(S,F)~cat,data=dat,family="binomial"))
# test 0-70 cm
dat<-data.frame(S=c(188,279),T=c(854-188,854 - 279),cat=c("real","random"))
summary(glm(cbind(S,T)~cat,data=dat,family="binomial"))
dat<-data.frame(S=c(268,221),F=c(854-268,854-221),cat=c("real","random"))
summary(glm(cbind(S,F)~cat,data=dat,family="binomial"))

#### Analyses pv ~ hg ####
#3.  sign. difference pv/hg?
# independent 2-group Mann-Whitney U Test 
wilcox.test(pv2$`nnd (m)`, hg2$`nnd (m)`, paired=F) ## p<0.001 sign diff
#If we assume normality can be violated (CLT), we can apply t-test
t.test(pv2$`nnd (m)`, hg2$`nnd (m)`)
# t = 4.4509, df = 1129.2, p-value = 9.397e-06 >> Sign diff
boxplot((pv2$`nnd (m)`), (hg2$`nnd (m)`), col=c("turquoise3", "seagreen3"), ylim=c(0,6), ylab="NND (m)", varwidth=TRUE, names=c("PV", "HG"), 
        horizontal = T, las=1, xlab="nearest neighbour distance (m) ") 

#vioplot::vioplot((pv$`nnd (m)`), (hg$`nnd (m)`), ylim=c(0,50))

ggplot(data = dat3,
       aes(x = `nnd (m)`, y = species, fill=species)) +
  geom_boxplot(outlier.shape = NA) +
  stat_boxplot(geom ='errorbar', width = 0.5)+
  geom_boxplot(outlier.shape = NA) +
  xlab("Nearest neighbour distance (m)") +
  ylab("") +
  labs(title = "Nearest neighbour distance")+
  xlim(0,5) + scale_fill_discrete(name = "Species")+
  scale_fill_brewer(palette="BuPu")+
  theme(legend.position = "None")


#### Analyses individual sizes ####

# combined density plot of the size
 ggplot(data=(dat3), aes(x=`seal surface area (m2)`, group=species, fill=species, color=species)) +
  geom_density(adjust=.8, alpha=0.3) +
  theme_ipsum()+
  xlim(0,2)+
  #scale_fill_brewer(palette="Set1")+
  #scale_color_brewer(palette="Set1")+
  labs(title="Smoothed density body sizes",x="Surface area (m2)", y = "Density")

# plot nnd's pv vd h (data only)
ggplot(data=(dat3), aes(x=dat3$`nnd (m)`, group=species, fill=species, color=species)) +
  geom_density(adjust=.8, alpha=0.3) +
  theme_ipsum()+
  xlim(0,5)+
  labs(title="Smoothed density original data",x="Nearest neighbour distance (m)", y = "Density")



#pv surface area
avg.hgsize = mean(hg2$`seal surface area (m2)`)
avg.pvsize = mean(pv2$`seal surface area (m2)`)
hist(pv2$`seal surface area (m2)`, breaks = 50, freq=F, xlab="Surface area (m2)", ylim=c(0,3.5), xlim=c(0,1))
abline(v=mean(pv2$`seal surface area (m2)`), col="red", lwd=2)
text(x = avg.pvsize * 1.27, y = avg.pvsize *7.5, paste("Mean =", avg.pvsize), col = "red", cex = 0.5)
abline(v=mean(hg$`seal surface area (m2)`), col="blue", lty=2)
text(x = avg.pvsize * 1.85, y = avg.pvsize *7, paste("Mean =", avg.hgsize), col = "blue", cex = 0.5)
#hg surface area
hist(hg2$`seal surface area (m2)`, breaks = 50,  freq=F, xlab="Surface area (m2)", ylim=c(0,2.5))
abline(v=mean(hg2$`seal surface area (m2)`), col="red", lty=2)
text(x = avg.hgsize * 1.31, y = avg.hgsize *3.2, paste("Mean =", avg.hgsize), col = "red", cex = 0.5)
abline(v=mean(pv2$`seal surface area (m2)`), col="blue", lty=2)
text(x = avg.hgsize * 0.31, y = avg.hgsize * 2.9, paste("Mean =", avg.pvsize), col = "blue", cex = 0.5)

#### NND ####
# we werken met de median, omdat er hele hoge outliers zijn die de mean omhoog trekken
#pv NND
hist(pv2$`nnd (m)`, breaks = 2000, freq=F, xlab="NND (m)", ylim=c(0,0.6), xlim=c(0,15))
abline(v=median(pv2$`nnd (m)`), col="red", lwd=2)
text(x = 3.45, y = 0.59, paste("Median PV"), col = "red", cex = 0.5)
abline(v=mean(hg2$`nnd (m)`), col="blue", lty=2)
text(x = 3.5, y = 0.56, paste("Median HG"), col = "blue", cex = 0.5)
#hg NND
hist(hg2$`nnd (m)`, breaks = 1000,  freq=F, xlab="NND (m)", ylim=c(0,1.5), xlim=c(0,5))
abline(v=median(hg2$`nnd (m)`), col="red", lwd=2)
text(x = 0.8, y = 1.45, paste("Median HG"), col = "red", cex = 0.5)
abline(v=median(pv2$`nnd (m)`), col="blue", lty=2)
text(x = 1.6, y = 1.45, paste("Median PV "), col = "blue", cex = 0.5)


####  graphics fun ####

# combined histogram of PV and HG
hist(pv2$`nnd (m)`, xlim=c(0,8), breaks=500, col=rgb(1,0,0,0.5), border=F, freq=F, ylim=c(0,1.5))
hist(hg2$`nnd (m)`, add=T, breaks=500, col=rgb(0,0,1,0.5), border=F, freq=F)

#### density plot without mix ####
dat3 = subset(dat2, dat2$species != "mix") #remove mix for aesthetic

ggplot(data=dat3, aes(x=`nnd (m)`, group=species, fill=species)) +
  geom_density(adjust=1, alpha=0.3) +
  theme_ipsum()+
  xlim(0,5)+
  labs(title="Smoothed density original data",x="Nearest neighbour distance (m)", y = "Density")





#### Radius info ####
boxplot(pv2$`number of seals within 1m distance`, pv2$`number of seals within 5m distance`, pv2$`number of seals within 10m distance`,
        hg2$`number of seals within 1m distance`, hg2$`number of seals within 5m distance`, hg2$`number of seals within 10m distance`,
        main = "Crowdedness neighbourhood",
        at = c(1,2,3,5,6,7),
        names = c("1m", "5m", "10m", "1m", "5m", '10m'), 
        cex.axis = 1, cex.lab = 1.2,
        las = 1, outcol = "grey", 
        col = c( "thistle2", "thistle", "thistle4","cadetblue1", "cadetblue3", "cadetblue"),
        border = "black",
        horizontal = TRUE,
        notch = F, cex.lab = 0.8,
        ylim=c(0,70), xlab="Number of seals within distance", ylab="PV                    HG")
# Is this difference significant?
t.test(pv2$`number of seals within 1m distance`, hg2$`number of seals within 1m distance`, paired=F)
t.test(pv2$`number of seals within 5m distance`, hg2$`number of seals within 5m distance`, paired=F)
t.test(pv2$`number of seals within 10m distance`, hg2$`number of seals within 10m distance`, paired=F)
summary(hg2$`number of seals within 1m distance`)
summary(hg2$`number of seals within 5m distance`)
summary(hg2$`number of seals within 10m distance`)



# What is the effect of the number of indiv. nearby on on the NND value ?
plot(log(pv2$`nnd (m)`) ~ pv2$`number of seals within 10m distance`)
model.pv10m=glm( pv2$`nnd (m)` ~ pv2$`number of seals within 10m distance` )
anova(model.pv10m) #anova

plot(log(hg2$`nnd (m)`) ~ hg2$`number of seals within 10m distance`)
model.hg10m=glm( hg2$`nnd (m)` ~ hg2$`number of seals within 10m distance` )
anova(model.hg10m) #anova

hist(pv2$`number of seals within 1m distance`)
hist(pv2$`number of seals within 5m distance`)
hist(pv2$`number of seals within 10m distance`)

wilcox.test(pv2$`number of seals within 1m distance`, hg2$`number of seals within 1m distance`)
wilcox.test(pv2$`number of seals within 5m distance`, hg2$`number of seals within 5m distance`)
wilcox.test(pv2$`number of seals within 10m distance`, hg2$`number of seals within 10m distance`)



# ndd x dist.sea
plot((pv2$`distance to sea (m)`), log(pv2$`nnd (m)`), cex=0.5)
plot(hg2$`distance to sea (m)`, log(hg2$`nnd (m)`))
plot(dat2$`distance to sea (m)`, dat2$`nnd (m)`)
plot(dat2$`NND rndm KDE (m)`, dat2$`nnd (m)`)
# nnd x drukte omgeving no.
plot(hg2$`nnd (m)` ~ hg2$`number of seals within 10m distance`, ylim=c(0,15), cex=0.5, col="red")


#### Fitting gamma to NND ####

##1. Fitting models to data
# 5 steps;
# SKIP: 1) specify a function of how the mean of y depends on x
# 2) specify a probability distribution to describe variations around the mean
# 3) Specify function to calculate NLL (based on data and paramter values)
# 4) Choose parameters of both deterministic and prob model -- lowest nll
# 5) compare alternative models (NLL, AIC, BIC)

#gamma distr
dat2.nnd = dat2$`nnd (m)`
dat2.nnd.rndm = dat2$`NND rndm KDE (m)`
hist(dat2.nnd, xlim=c(0,10), breaks=2000)
curve(dgamma())
hist(dat2.nnd.rndm, xlim=c(0,10), breaks=1000)
glmdata

#### GLM ####

# baseline: random / nonrandom
glm1=glm(nnd ~ factor(randomness), family="Gamma", data=glmdata)
summary(glm1) # randomness (both pv and hg on one pile)
glm1.pv=glm(nnd ~ factor(randomness), family="Gamma", data=glmdata.pv)
summary(glm1.pv) #randomness (only pv)
glm1.hg=glm(nnd ~ factor(randomness), family="Gamma", data=glmdata.hg)
summary(glm1.hg) # randomness (only hg)

glm2=glm(nnd ~ factor(glmdata$`dat2$species`), family="Gamma", data=glmdata)
summary(glm2) # original dataset, difference pv x hg

glm3=glm(nnd ~ factor(glmdata.pv[,4]), family="Gamma", data=glmdata.pv)
summary(glm3) #origineel dataset, difference between haul-out sites

glm3=glm(nnd ~ factor(glmdata.hg[,4]), family="Gamma", data=glmdata.hg)
summary(glm3) #origineel dataset, difference between haul-out sites

glm4=glm(nnd ~ factor(glmdata.pv[,5]), family="Gamma", data=glmdata.pv)
summary(glm4) #origineel dataset, difference between dates

glm4=glm(nnd ~ factor(glmdata.hg[,5]), family="Gamma", data=glmdata.hg)
summary(glm4) #origineel dataset, difference between dates


#### Correlation ####
#limit data 0-10m nnd distances
pv4 = subset(pv3, pv2$`nnd (m)` <= 10)
hg4 = subset(hg2, hg2$`nnd (m)` <= 10)

cor.data.pv = cbind(pv3$`nnd (m)`, pv3$`NND rndm KDE (m)`, pv3$`nnd rndm kde.b (m)`, dat2$`seal surface area (m2)`, pv3$`distance to sea (m)`,pv3$`number of seals within 1m distance`, pv3$`number of seals within 5m distance`, pv3$`number of seals within 10m distance`)
cor.mat.pv = cor(cor.data.pv, method="spearman")
colnames(cor.mat.pv) = c("NND","NND (random A)","NND (random B)", "distance to sea","body size", "seals within 1m", "seals within 5m", "seals within 10m")
rownames(cor.mat.pv) = c("NND","NND (random A)","NND (random B)", "distance to sea","body size", "seals within 1m", "seals within 5m", "seals within 10m")
corrplot(cor.mat.pv, main="Correlogram PV a=0.001 spearman",tl.col="black", tl.srt=45, diag=F, type="upper",  insig="pch", sig.level = 0.001, p.mat=cor.mat.pv,  pch.cex=5, pch.col="grey", tl.cex=.8)

cor.data.hg = cbind(hg3$`nnd (m)`,hg3$`NND rndm KDE (m)`, hg3$`nnd rndm kde.b (m)`,dat2$`seal surface area (m2)` , hg3$`distance to sea (m)`,hg3$`number of seals within 1m distance`, hg3$`number of seals within 5m distance`, hg3$`number of seals within 10m distance` )
cor.mat.hg = cor(cor.data.hg, method="spearman")
colnames(cor.mat.hg) = c("NND","NND (random A)","NND (random B)","distance to sea","body size", "seals within 1m", "seals within 5m", "seals within 10m")
rownames(cor.mat.hg) = c("NND","NND (random A)","NND (random B)","distance to sea","body size", "seals within 1m", "seals within 5m", "seals within 10m")
corrplot(cor.mat.hg, main="Correlogram HG a=0.001 spearman",tl.col="black", tl.srt=45, diag=F, type="upper",  insig="pch", sig.level = 0.001, p.mat=cor.mat.hg, pch.cex=5, pch.col="darkgrey", tl.cex=.8)

#all data
cor.data.pv = cbind(pv2$`nnd (m)`, pv2$`distance to sea (m)`,pv2$`number of seals within 1m distance`, pv2$`number of seals within 5m distance`, pv2$`number of seals within 10m distance` ,pv2$`NND rndm KDE (m)`)
cor.mat.pv = cor(cor.data.pv)
corrplot(cor.mat.pv )
cor.data.hg = cbind(hg2$`nnd (m)`, hg2$`distance to sea (m)`,hg2$`number of seals within 1m distance`, hg2$`number of seals within 5m distance`, hg2$`number of seals within 10m distance` ,hg2$`NND rndm KDE (m)`)
cor.mat.hg = cor(cor.data.hg)
corrplot(cor.mat.hg )


#### Distance to sea ####
ggplot(data=dat3, aes(x=`distance to sea (m)`, group=species, fill=species, color=species)) +
  geom_density(adjust=.8, alpha=0.3) +
  theme_ipsum()+
  xlim(0,50)+
  labs(title="Distance to sea",x="Distance to sea (m)", y = "Density")

ggplot(pv2, aes(x = `distance to sea (m)`, y = `nnd (m)`)) +
  geom_point(aes(color = `haul-out site`))+
  xlim(0,80)+
  geom_point(alpha=0.2) + 
  geom_smooth(method = "lm",se=F, alpha = .15, aes(colour = (`haul-out site`)))+
  scale_y_log10()+
  labs(title = "PV")+
  scale_colour_brewer(palette="Set1")

ggplot(hg2, aes(x = `distance to sea (m)`, y = `nnd (m)`)) +
  geom_point(aes(color = `haul-out site`))+
  xlim(0,150)+
  geom_point(alpha=0.2) + 
  geom_smooth(method = "lm",se=F, alpha = .15, aes(colour = (`haul-out site`)))+
  scale_y_log10()+
  labs(title = "HG")+
  scale_colour_brewer(palette="Set1")

ggplot(dat3, aes(x = `distance to sea (m)`, y = `nnd (m)`)) +
  geom_point(aes(color = species))+
  xlim(0,150)+
  geom_point(alpha=0.1) + 
  geom_smooth(method = "lm", se=F,alpha = .15, aes(colour = species))+
  scale_y_log10()+
  labs(title = "species")

## distance to sea
# test for difference
shapiro.test(pv3$`distance to sea (m)`)
shapiro.test(hg2$`distance to sea (m)`)
t.test(pv3$`distance to sea (m)`, hg2$`distance to sea (m)`)
wilcox.test(pv3$`distance to sea (m)`, hg2$`distance to sea (m)`, paired = F)
# visualize
boxplot(pv2$`distance to sea (m)`, hg2$`distance to sea (m)`, ylim=c(0,60), names=c("pv", "hg"), main="distance to sea (m)", ylab="distance (m)")
dat3 = rbind(pv3, hg2)
ggplot(data = dat3,
       aes(x = `distance to sea (m)`, y = species, fill=species)) +
  geom_boxplot(outlier.shape = NA) +
  stat_boxplot(geom ='errorbar', width = 0.5)+
  geom_boxplot(outlier.shape = NA) +
  xlab("Distance to sea (m)") +
  ylab("") +
  labs(title = "Distance to sea")+
  xlim(0,30) + scale_fill_discrete(name = "Data")+
  scale_fill_brewer(palette="BuPu")+
  theme(legend.position = "none" )
  


hist(dat2$`distance to sea (m)`, breaks=200, xlim=c(0,150), freq=F)
hist(pv2$`distance to sea (m)`,  breaks=200, xlim=c(0,100), freq=F)
lines(density(pv2$`distance to sea (m)`), col="blue", lwd=2)
abline(v=median(pv2$`distance to sea (m)`), col="red")
hist(hg2$`distance to sea (m)`,  breaks=100, xlim=c(0,150), freq=F)
lines(density(hg2$`distance to sea (m)`), col="blue", lwd=2)
abline(v=median(hg2$`distance to sea (m)`), col="red")

hist(hg2$`distance to sea (m)`,  xlim=c(0,100),  breaks=50, col="blue", freq=F)
hist(pv2$`distance to sea (m)`, breaks=100, add=T, freq=F)


#### other vars ####

#regions
### klopt niet want zit ook RANDOM in!!
ggplot(data = glmdata.pv,
       aes(x = nnd, y = glmdata.pv[,4], fill=glmdata.pv[,4])) +
  geom_boxplot() +
  stat_boxplot(geom ='errorbar', width = 0.5)+
  geom_boxplot() +
  xlab("Nearest neighbour distance (m)") +
  ylab("") +
  labs(title = "Nearest neighbour distance (m)")+
  xlim(0,10) + scale_fill_discrete(name = "Data")+
  scale_fill_brewer(palette="BuPu")+
  theme(legend.position = "none" )

### klopt niet want zit ook RANDOM in!!
ggplot(data = glmdata.hg,
       aes(x = nnd, y = glmdata.hg[,4], fill=glmdata.hg[,4])) +
  geom_boxplot() +
  stat_boxplot(geom ='errorbar', width = 0.5)+
  geom_boxplot() +
  xlab("Nearest neighbour distance (m)") +
  ylab("") +
  labs(title ="Nearest neighbour distance (m)")+
  xlim(0,10) + scale_fill_discrete(name = "Data")+
  scale_fill_brewer(palette="BuPu")+
  theme(legend.position = "none" )

#dates
### klopt niet want zit ook RANDOM in!!
ggplot(data = glmdata.pv,
       aes(x = nnd, y = glmdata.pv[,5], fill=glmdata.pv[,5])) +
  geom_boxplot(outlier.shape = NA) +
  stat_boxplot(geom ='errorbar', width = 0.5)+
  geom_boxplot(outlier.shape = NA) +
  xlab("Nearest neighbour distance (m)") +
  ylab("") +
  labs(title = "Nearest neighbour distance (m)")+
  xlim(0,10) + scale_fill_discrete(name = "Data")+
  scale_fill_brewer(palette="BuPu")+
  theme(legend.position = "none" )

#### Regions ####
# remove mixed regions
glmdata.nomix.0 = subset(glmdata, glmdata$`dat2$species` != "mix")
# get a dataset with only original data
glmdata.nomix = subset(glmdata.nomix.0, glmdata.nomix.0$randomness == "original") ## only original

## pv and hg plotted per region
#p = ggplot(data = glmdata.nomix,
p = ggplot(data = glmdata,
       aes(x = nnd, y = glmdata[,4], fill=glmdata[,3])) +
  geom_boxplot(outlier.shape = NA) +
  stat_boxplot(geom ='errorbar', width = 0.5)+
  geom_boxplot(outlier.shape = NA) +
  xlab("Nearest neighbour distance (m)") +
  ylab("") +
  labs(title ="Nearest neighbour distance (m)")+
  xlim(0,8) + scale_fill_discrete(name = "Data")+
  scale_fill_brewer(palette="BuPu")+
  theme(legend.position = "right" )
p$labels$fill <- "Species"
p


## visualizing original x random
p = ggplot(data = glmdata.nomix.0,
           aes(x = nnd, y = glmdata.nomix[,4], fill=glmdata.nomix[,2]), 
           ) +
  geom_boxplot(outlier.shape = NA) +
  stat_boxplot(geom ='errorbar')+ #, width = 0.5
  geom_boxplot(outlier.shape = NA) +
  xlab("Nearest neighbour distance (m)") +
  ylab("") +
  labs(title ="Nearest neighbour distance (m)")+
  xlim(0,10) + scale_fill_discrete(name = "Data")+
  scale_fill_brewer(palette="BuPu")+
  theme(legend.position = "right", axis.text.y=element_text(size=rel(1.3), axis.text.x=element_text(size=rel(1.3))))
  #facet_wrap(~glmdata.nomix.0[,3]) # block per species
p$labels$fill <- "Data"
p

boxplot(noorderhaaks.finaldata$`nnd (m)`,noordh.mix$`nnd (m)`,hg2$`nnd (m)`,pv3$`nnd (m)`, ylim=c(0,5))


#### crowd ggplot ####
#define data
r1m = as.data.frame(dat3$`number of seals within 1m distance`)
r5m = as.data.frame(dat3$`number of seals within 5m distance`)
r10m = as.data.frame(dat3$`number of seals within 10m distance`)
colnames(r1m) = "n indiv"
colnames(r5m) = "n indiv"
colnames(r10m) = "n indiv"
n.inrad = rbind(r1m, r5m, r10m)
species.rdat = rbind(as.data.frame(dat3$species), as.data.frame(dat3$species), as.data.frame(dat3$species))
region.rdat = rbind(as.data.frame(dat3$`haul-out site`), as.data.frame(dat3$`haul-out site`), as.data.frame(dat3$`haul-out site`))
date.rdat = rbind(as.data.frame(dat3$`date image`), as.data.frame(dat3$`date image`), as.data.frame(dat3$`date image`))
rad = (rep(c("1 m","5 m", "10 m "), each=nrow(dat3)))
crowdplotdata.withmix = cbind(n.inrad, rad, species.rdat, region.rdat, date.rdat)
crowdplotdata = subset(crowdplotdata.withmix, crowdplotdata.withmix[,3] != "mix")

#boxplot
p = ggplot(data = crowdplotdata,
           aes(x = crowdplotdata[,1], y = crowdplotdata[,2], fill=crowdplotdata[,3])) +
  geom_boxplot(outlier.shape = NA) +
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(outlier.shape = NA) +
  xlab("Number of individuals") +
  ylab("") +
  labs(title ="Number of individuals within set distance")+
  xlim(0,70) + scale_fill_discrete(name = "Data")+
  scale_fill_brewer(palette="BuPu")+
  theme(legend.position = "right" )
  #facet_wrap(~crowdplotdata[,2]) # blocking
p$labels$fill <- "Species"
p


#### crowd ~ nnd ####
ggplot(pv2, aes(x = `number of seals within 10m distance`, y = `nnd (m)`)) +
  geom_point(aes(color = `haul-out site`))+
  xlim(0,40)+
  geom_point(alpha=0.2) + 
  geom_smooth(method = "lm",se=F, alpha = .15, aes(colour = (`haul-out site`)))+
  scale_y_log10()+
  labs(title = "PV")+
  scale_colour_brewer(palette="Set1")

ggplot(hg2, aes(x = `number of seals within 10m distance`, y = `nnd (m)`)) +
  geom_point(aes(color = `haul-out site`))+
  xlim(0,45)+
  geom_point(alpha=0.2) + 
  geom_smooth(method = "lm",se=F, alpha = .15, aes(colour = (`haul-out site`)))+
  scale_y_log10()+
  labs(title = "HG")+
  scale_colour_brewer(palette="Set1")

# Does the crowdedness differ per site?

rad0 = (rep(c("1","5 ", "10 "), each=nrow(dat3)))
rad = as.data.frame(as.numeric(rad0))
crowdplotdata.withmix = cbind(n.inrad, rad, species.rdat, region.rdat, date.rdat)
crowdplotdata = subset(crowdplotdata.withmix, crowdplotdata.withmix[,3] != "mix")
colnames(crowdplotdata) = c("indiv.in.rad", "rad", "species", "Location", "date")

p = ggplot(crowdplotdata, aes( x = crowdplotdata[,2], crowdplotdata[,1])) +
  geom_point(aes(color = Location))+
  xlim(0,10)+
  geom_point(alpha=0.02) + 
  geom_smooth(method = "lm",se=F, alpha = .15, aes(colour = (Location)))+
  scale_y_log10()+
  labs(title = "How crowded are the haul-out sites?", fill = "Haul-out site")+
  xlab("Distance") +
  ylab("Number of seals within x distance") +
  scale_colour_brewer(palette="Set1")+
  facet_wrap(~crowdplotdata[,3]) # blocking
p 

summary(goedereede$`nnd (m)`)
summary(rif.finaldata$`nnd (m)`)
