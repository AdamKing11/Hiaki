# Hiaki project
# looking at final vowels in dictionary of Hiaki
# m = medial, f= final

# load various libraries
# maybe not all needed...
library(ggplot2)
library(plyr)
library(reshape2)
library(lsr)

d = read.csv("phonotact.csv")

#only look at nouns, verbs and adjectives
summary(d)

#convert the position and vowels to numbers
# for discrete analysis
d$VowelCat <- as.numeric(d$Vowel)
vNames = c("[a]","[e]","[i]","[o]","[u]")
# Re-arrange data for position
# make sure medial come before final
d$Position <- as.character(d$Position)
d$Position[d$Position=="f"] <- 2
d$Position[d$Position=="m"] <- 1

#reorder the POS's
POSList = c()
POSList[1] = "n."
POSList[2] = "adj."
POSList[3] = "iv."
POSList[4] = "tv."
POSList[5] = "adv."

POSList.names = c()
POSList.names[1] <- "N."
POSList.names[2] <- "Adj."
POSList.names[3] <- "I. Verb"
POSList.names[4] <- "T. Verb"
POSList.names[5] <- "Adv."



d$POSCat <- as.character(d$POS)
d$POSCat[d$POS==POSList[1]] <- 1
d$POSCat[d$POS==POSList[2]] <- 2
d$POSCat[d$POS==POSList[3]] <- 3
d$POSCat[d$POS==POSList[4]] <- 4
d$POSCat[d$POS==POSList[5]] <- 5

#same for POS tags


allCats <- table(d$Position,d$Vowel)
# make data tables for graphs later
#CATEGORIES
nouns <- table(d$Position[d$POS=='n.'],d$VowelCat[d$POS=='n.'])
adj <- table(d$Position[d$POS=='adj.'],d$VowelCat[d$POS=='adj.'])
iv <- table(d$Position[d$POS=='iv.'],d$VowelCat[d$POS=='iv.'])
tv <- table(d$Position[d$POS=='tv.'],d$VowelCat[d$POS=='tv.'])
verbs <-table(d$Position[d$POS=='tv.' | d$POS=='iv.'],d$Vowel[d$POS=='tv.' | d$POS=='iv.'])

#VOWELS
e <- subset(d, Position==2)
finalA <- table(e$Position[e$Vowel=='a'],e$POSCat[e$Vowel=='a'])
finalE <- table(e$Position[e$Vowel=='e'],e$POSCat[e$Vowel=='e'])
finalI <- table(e$Position[e$Vowel=='i'],e$POSCat[e$Vowel=='i'])
finalO <- table(e$Position[e$Vowel=='o'],e$POSCat[e$Vowel=='o'])
finalU <- table(e$Position[e$Vowel=='u'],e$POSCat[e$Vowel=='u'])
finalALL <- table(e$Vowel,e$POSCat)

beardownColors = c("black","white")

###################################
# Time to graph
###################################

# position of vowel vs vowel
#par(mfrow = c(1,1))
ratioV <- barplot(allCats, 
                  horiz=TRUE,           #to make horizontal
                  las=1,                # to make labels parralell with bottom
                  beside=FALSE,          # same bars or different
                  main="All Lexical Categories", # for title
                  col=beardownColors,  # for colors
                  density=seq(15,65,15), # for the pattern
                  cex.names=1.2,            # to make y-axis labels a little bigger
                  xlim=c(0,4200),
                  names=vNames)
#add the legend
legend("topright", c("Medial", "Final"), cex=1.3, bty="n", fill=beardownColors, density=seq(15,65,15))
# VERY inefficient but recycle code to make the ratios
#Aggregates
#f <- subset(d,d$POS=="n.")
f <- data.frame(table(d$Vowel,d$Position))
g <- aggregate(f$Freq,list(vowel = f$Var1),sum)
f$Sum <- g$x
f$Ratio <- f$Freq/f$Sum
f$RatioChar <- paste(as.character(round(f$Ratio*100)),"%")

text(x=allCats[seq(1,10,2)]+100,y=ratioV, labels=f$RatioChar[f$Var2==2], pos=4, cex=1.1, font=1, col="black")
legend("topright", c("Medial", "Final"), cex=1.3, bty="n", fill=beardownColors, density=seq(15,65,15))

chisq.test(allCats)
cramersV(allCats)

#CATEGORIES
#break up into 3
# NOUNS
tempPlot <- barplot(nouns, horiz=TRUE, las=1, beside=FALSE, main="Nouns", col=beardownColors,density=seq(15,65,15),cex.names=1.2,names=vNames)            
f <- subset(d,d$POS=="n.")
f <- data.frame(table(f$Vowel,f$Position))
g <- aggregate(f$Freq,list(vowel = f$Var1),sum)
f$Sum <- g$x
f$Ratio <- f$Freq/f$Sum
f$RatioChar <- paste(as.character(round(f$Ratio*100)),"%")
text(x=nouns[seq(1,10,2)]+50,y=tempPlot, labels=f$RatioChar[f$Var2==2], pos=4, cex=1.1, font=1, col="black")
legend("topright", c("Medial", "Final"), cex=1.3, bty="n", fill=beardownColors, density=seq(15,65,15))

chisq.test(nouns)
cramersV(nouns)


# ADJ
tempPlot <- barplot(adj, horiz=TRUE, las=1, beside=FALSE, main="Adjectives", col=beardownColors,density=seq(15,65,15),cex.names=1.2,names=vNames)            
f <- subset(d,d$POS=="adj.")
f <- data.frame(table(f$Vowel,f$Position))
g <- aggregate(f$Freq,list(vowel = f$Var1),sum)
f$Sum <- g$x
f$Ratio <- f$Freq/f$Sum
f$RatioChar <- paste(as.character(round(f$Ratio*100)),"%")
text(x=adj[seq(1,10,2)]+5,y=tempPlot, labels=f$RatioChar[f$Var2==2], pos=4, cex=1.1, font=1, col="black")
legend("topright", c("Medial", "Final"), cex=1.3, bty="n", fill=beardownColors, density=seq(15,65,15))

chisq.test(adj)
cramersV(adj)


# VERBS
tempPlot <- barplot(verbs, horiz=TRUE, las=1, beside=FALSE, main="Verbs", col=beardownColors,density=seq(15,65,15),cex.names=1.2,names=vNames)            
f <- subset(d,d$POS=="iv." | d$POS=="tv.")
f <- data.frame(table(f$Vowel,f$Position))
g <- aggregate(f$Freq,list(vowel = f$Var1),sum)
f$Sum <- g$x
f$Ratio <- f$Freq/f$Sum
f$RatioChar <- paste(as.character(round(f$Ratio*100)),"%")
text(x=verbs[seq(1,10,2)]+25,y=tempPlot, labels=f$RatioChar[f$Var2==2], pos=4, cex=1.1, font=1, col="black")
legend("topright", c("Medial", "Final"), cex=1.3, bty="n", fill=beardownColors, density=seq(15,65,15))

chisq.test(verbs)
cramersV(verbs)


#verbs
# iv
tempPlot <- barplot(iv, horiz=TRUE, las=1, beside=FALSE, main="Intransitive Verbs", col=beardownColors,density=seq(15,65,15),cex.names=1.2,names=vNames)            
f <- subset(d,d$POS=="iv.")
f <- data.frame(table(f$Vowel,f$Position))
g <- aggregate(f$Freq,list(vowel = f$Var1),sum)
f$Sum <- g$x
f$Ratio <- f$Freq/f$Sum
f$RatioChar <- paste(as.character(round(f$Ratio*100)),"%")
text(x=iv[seq(1,10,2)]+25,y=tempPlot, labels=f$RatioChar[f$Var2==2], pos=4, cex=1.1, font=1, col="black")
legend("topright", c("Medial", "Final"), cex=1.3, bty="n", fill=beardownColors, density=seq(15,65,15))

chisq.test(iv)
cramersV(iv)



tempPlot <- barplot(tv, horiz=TRUE, las=1, beside=FALSE, main="Transitve Verbs", col=beardownColors,density=seq(15,65,15),cex.names=1.2,names=vNames)            
f <- subset(d,d$POS=="tv.")
f <- data.frame(table(f$Vowel,f$Position))
g <- aggregate(f$Freq,list(vowel = f$Var1),sum)
f$Sum <- g$x
f$Ratio <- f$Freq/f$Sum
f$RatioChar <- paste(as.character(round(f$Ratio*100)),"%")
text(x=tv[seq(1,10,2)]+25,y=tempPlot, labels=f$RatioChar[f$Var2==2], pos=4, cex=1.1, font=1, col="black")
legend("topright", c("Medial", "Final"), cex=1.3, bty="n", fill=beardownColors, density=seq(15,65,15))

chisq.test(tv)
cramersV(tv)


#VOWELS
summary(e)

#all vowels
#par(mfrow = c(1,1))
#barplot(finalALL, horiz=TRUE, las=1, beside=TRUE, main="", col=c("darkblue","red","goldenrod","green","black"),cex.names=1.2,names=POSList.names)            
#legend("topright", c("[a]","[e]","[i]","[o]","[u]"), cex=1, bty="n", fill=c("darkblue","red","goldenrod","green","black"))

# break up into 4
#par(mfrow = c(2,2))
# O
tempPlot <- barplot(finalO, horiz=TRUE, las=1, beside=FALSE, main="Final [o]", col=c("black"),density=seq(15,65,15),cex.names=1,names=POSList.names,xlim=c(0,350))            
f <- subset(d,d$Position==2 & d$Vowel=="o")
f <- data.frame(table(f$POSCat,f$Position))
g <- aggregate(f$Freq,list(cat = f$Var2),sum)
f$Sum <- g$x
f$Ratio <- f$Freq/f$Sum
f$RatioChar <- paste(as.character(round(f$Ratio*100)),"%")
text(x=finalO-15,y=tempPlot, labels=f$RatioChar[f$Var2==2], pos=4, cex=1.1, font=1, col="black",offset=2)

chisq.test(finalO)
cramersV(finalO)


# U
barplot(finalU, horiz=TRUE, las=1, beside=FALSE, main="Final [u]", col=c("white"),density=seq(15,65,15),cex.names=1,names=POSList.names,xlim=c(0,50))            
f <- subset(d,d$Position==2 & d$Vowel=="u")
f <- data.frame(table(f$POSCat,f$Position))
g <- aggregate(f$Freq,list(cat = f$Var2),sum)
f$Sum <- g$x
f$Ratio <- f$Freq/f$Sum
f$RatioChar <- paste(as.character(round(f$Ratio*100)),"%")
text(x=finalU,y=tempPlot, labels=f$RatioChar[f$Var2==2], pos=4, cex=1.1, font=1, col="black")

chisq.test(finalU)
cramersV(finalU)

# I
barplot(finalI, horiz=TRUE, las=1, beside=FALSE, main="Final [i]", col=c("black"),density=seq(15,65,15),cex.names=1,names=POSList.names,xlim=c(0,300))
f <- subset(d,d$Position==2 & d$Vowel=="i")
f <- data.frame(table(f$POSCat,f$Position))
g <- aggregate(f$Freq,list(cat = f$Var2),sum)
f$Sum <- g$x
f$Ratio <- f$Freq/f$Sum
f$RatioChar <- paste(as.character(round(f$Ratio*100)),"%")
text(x=finalI-20,y=tempPlot, labels=f$RatioChar[f$Var2==2], pos=4, cex=1.1, font=1, col="black", offset=2)

chisq.test(finalI)
cramersV(finalI)

# E
tempPlot <- barplot(finalE, horiz=TRUE, las=1, beside=FALSE, main="Final [e]", col=c("white"),density=seq(15,65,15),cex.names=1,names=POSList.names,xlim=c(0,350))            
f <- subset(d,d$Position==2 & d$Vowel=="e")
f <- data.frame(table(f$POSCat,f$Position))
g <- aggregate(f$Freq,list(cat = f$Var2),sum)
f$Sum <- g$x
f$Ratio <- f$Freq/f$Sum
f$RatioChar <- paste(as.character(round(f$Ratio*100)),"%")
text(x=finalE-15,y=tempPlot, labels=f$RatioChar[f$Var2==2], pos=4, cex=1.1, font=1, col="black", offset=1.3)

chisq.test(finalE)
cramersV(finalE)

# A
tempPlot <- barplot(finalA, horiz=TRUE, las=1, beside=FALSE, main="Final [a]", col=c("black"),density=seq(15,65,15),cex.names=1,names=POSList.names,xlim=c(0,650))            
f <- subset(d,d$Position==2 & d$Vowel=="a")
f <- data.frame(table(f$POSCat,f$Position))
g <- aggregate(f$Freq,list(cat = f$Var2),sum)
f$Sum <- g$x
f$Ratio <- f$Freq/f$Sum
f$RatioChar <- paste(as.character(round(f$Ratio*100)),"%")
text(x=finalA-25,y=tempPlot, labels=f$RatioChar[f$Var2==2], pos=4, cex=1.1, font=1, col="black", offset=1.3)

chisq.test(finalA)
cramersV(finalA)

# All
tempPlot <- barplot(finalALL, horiz=TRUE, las=1, beside=TRUE, main="All Vowels", col=c("black"),density=seq(15,65,15),cex.names=1.2,names=POSList.names)            
f <- subset(d,d$Position==2)
f <- data.frame(table(f$POSCat,f$Position))
g <- aggregate(f$Freq,list(cat = f$Var2),sum)
f$Sum <- g$x
f$Ratio <- f$Freq/f$Sum
f$RatioChar <- paste(as.character(round(f$Ratio*100)),"%")
text(x=1,y=tempPlot, labels=f$RatioChar[f$Var2==2], pos=4, cex=1.1, font=1, col="black", offset=1.3)

#par(mfrow = c(1,1))
#barplot(allVowels,horiz=TRUE, las=1, beside=TRUE, main="All Vowels", col=c("darkblue","red","goldenrod","green","black"),cex.names=1.2)            
#legend("topright", POSList.names, cex=1, bty="n", fill=c("darkblue","red","goldenrod","green","black"))


# i vs e
ieTable = matrix(nrow = 2, ncol=2)
# i
# adj + adv
ieTable[2,1] = sum(as.numeric(e$Vowel[(e$Vowel=='i') & (e$POS=='adj.' | e$POS=='adv.')]))
# verb
ieTable[2,2] = sum(as.numeric(e$Vowel[(e$Vowel=='i') & (e$POS=='iv.' | e$POS=='tv.')]))
# e
# adj + adv
ieTable[1,1] = sum(as.numeric(e$Vowel[(e$Vowel=='e') & (e$POS=='adj.' | e$POS=='adv.')]))
# verb
ieTable[1,2] = sum(as.numeric(e$Vowel[(e$Vowel=='e') & (e$POS=='iv.' | e$POS=='tv.')]))

ieTable
tempPlot <- barplot(ieTable, horiz=TRUE, las=1, beside=TRUE,names=c("[i]","[e]"),col=c(1,0),xlim=c(0,1200))
#tempPlot <- barplot(ieTable, horiz=TRUE, las=1, beside=TRUE, main="", col=c("black"),density=seq(15,65,15),cex.names=1.2,names=POSList.names)            
legend("topright", c("Verbs","Adj./Adv."), cex=1, bty="n", fill=c(0,1))
text(x=ieTable,y=tempPlot, labels=ieTable, pos=4, cex=.8, font=1, col="black")


chisq.test(ratioTable)
ratioTable
table(ieComp$Vowel,ieComp$POS)

colSums(table(e$Vowel,e$POS))

