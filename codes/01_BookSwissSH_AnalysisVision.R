## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----cars, message=FALSE, warning=F--------------------------------------
library(ggplot2)
library(likert)
library(foreign)
library(MASS)
library(Hmisc)
library(reshape2)
library(grid)
options(digits=4)


## ----plottheme_hist, echo=F----------------------------------------------
# plot theme.
blank_themep <-  #theme_minimal()+
  theme(
    axis.title.x = element_text(size=11, face="bold"),
    axis.title.y = element_text(size=11),
    axis.ticks = element_blank(),
    plot.title= element_text(size=15, face="bold", hjust = 0.5, color = "black"),
    legend.title=element_text(size=11, face="bold", color = "black"),
    legend.text=element_text(size=11, color = "black"),
    legend.key.size=unit(.01,"npc"),
    axis.text.x=element_text(size=10, color = "black"),
    axis.text.y=element_text(size=11, color = "black")
    #aspect.ratio = 1.5/1
   )


## ----multiplot, echo=F---------------------------------------------------
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


## ----data----------------------------------------------------------------
data.full <- read.csv("../data/20200625_SESHS_All.csv", sep = ",", header = T, encoding = "UTF-8")


## ----cleaning, results='hide'--------------------------------------------
data <- data.full[which(data.full$I_Participation == 1 & data.full$Progress != 0), ]
data <- data[!(data$X == "174" | data$X == "202"), ]
dim1 <- dim(data)
data <- data[which(data$Finished == 1), ]
dim2 <- dim(data)
attach(data)


## ----rescale, echo=T-----------------------------------------------------
visionlevels <- c('Completely unrealistic', 'Rather unealistic', 'Rather realistic', 'Completely realistic')
vision.list <- c("Q19","Q22","Q25","Q28","Q31_1","Q31_2","Q31_3","Q31_4")
vision.list.f <- c("Q19.f","Q22.f","Q25.f","Q28.f","Q31_1.f","Q31_2.f","Q31_3.f","Q31_4.f")
data[vision.list.f] <- lapply(data[vision.list], function(x) factor((5-x), labels = visionlevels))
detach(data)
attach(data)


## ----activefield, echo=T-------------------------------------------------
## note the original Q56_ are coded 1 or NA. we want it to be 0 or 1.
act.list <- c("Q56_1", "Q56_4", "Q56_5", "Q56_13", "Q56_2")
act.list.new <- c("act.hp", "act.wind", "act.dge", "act.ev", "act.nuc")
data[act.list.new] <- lapply(data[act.list], function(x) ifelse(is.na(x), 0, 1))
detach(data)
attach(data)

## ----activefieldcount, echo=F--------------------------------------------
## let's see how many were active in each field.
#lapply(data[, act.list.new], function(x) table(x, useNA = "ifany"))
actcount <-lapply(data[act.list.new], function(x) length(x[x==1]))


## ----interactionpolitician, echo=T---------------------------------------
## first, make dummies using Q35.
## (1) Both collec and alone. (2) Only alone. (3) Only collec. (4) Not at all. (999) DK.
pol.act.list <- c("Q35_1", "Q35_2")
pol.act.list.new <- c("pol.act1", "pol.act2")
data[pol.act.list.new] <- 
  lapply(data[pol.act.list], function(x) ifelse(x==999, NA, ifelse((x==1 | x==2 | x==3), 1, 0)))
## next, make dummies using Q52. change NA to 0.
pol.act.list2 <- c("Q52_1", "Q52_3", "Q52_4", "Q52_5", "Q52_6")
pol.act.list2.new <- c("pol.act3", "pol.act4", "pol.act5", "pol.act6", "pol.act7")
data[pol.act.list2.new] <- lapply(data[pol.act.list2], function(x) ifelse(is.na(x), 0, 1))

## aggregate. simple sum.
data$pol.act <- data$pol.act1 + data$pol.act2 + data$pol.act3 + data$pol.act4 + data$pol.act5 + data$pol.act6 + data$pol.act7
detach(data)
attach(data)

## mean, st dev.
c(summary(pol.act, detail), sd(na.omit(pol.act)))


## ----activeinmedia, echo=T-----------------------------------------------
## make dummies using Q52. change NA to 0.
media.act.list <- c("Q52_8", "Q52_9")
media.act.list.new <- c("media.act1", "media.act2")
data[media.act.list.new] <- lapply(data[media.act.list], function(x) ifelse(is.na(x), 0, 1))

## aggregate. simple sum.
data$media.act = data$media.act1 + data$media.act2
detach(data)
attach(data)


## ----acthist, echo=F, warning=F------------------------------------------
p.pol <- ggplot(data, aes(pol.act)) +
  geom_histogram(binwidth = 0.5, fill = "pink", color = "gray") +
  scale_x_continuous(breaks = seq(0,7,1)) +
  labs(x="Political activeness", y="Count") +
  blank_themep +
  theme(aspect.ratio = 1/0.6)
#p.pol + facet_grid(cols = vars(act.hp))
p.media <- ggplot(data, aes(media.act)) +
  geom_histogram(binwidth = 0.5, fill = "pink", color = "gray") +
  scale_x_continuous(breaks = seq(0,2,1)) +
  labs(x="Media activeness", y="") +
  blank_themep +
  theme(aspect.ratio = 1/0.3)
multiplot(p.pol, p.media, cols=2)


## ----barev, echo=T-------------------------------------------------------
## tech: Tech'l maturity(1) Environmental risks(2) High investment costs(3) Weak infra for EV(6) 
## stake: Low accept of EV by cons(4) Info deficits of cons(5) Oppos by int grps(7) Lack support by car dealers(8) 
## regulatory: Policy/regulatory risk(9) 

data$bartech.ev <- 0
data$bartech.ev[data$Q29_1==1 |data$Q29_2==1 |data$Q29_3==1 | data$Q29_6==1] <- 1
data$barstake.ev <- 0
data$barstake.ev[data$Q29_4==1 |data$Q29_5==1 | data$Q29_7==1 |data$Q29_8==1] <- 1
data$barreg.ev <- 0
data$barreg.ev[data$Q29_9==1] <- 1
detach(data)
attach(data)


## ----bardge, echo=T------------------------------------------------------
## tech: High investment costs. (1) High operational costs. (2) Seismic risk. (3) Exploration risk. (4)
## stake: Opposition by citizens (5) Opposition by interest groups. (6)
## regulatory: Policy/regulatory risks. (7)
## OMITTED bc not directly related to DGE tech. The electricity grid is not ready (8)

data$bartech.dge <- 0
data$bartech.dge[data$Q23_1==1 | data$Q23_2==1 | data$Q23_3==1 | data$Q23_4==1] <- 1
data$barstake.dge <- 0
data$barstake.dge[data$Q23_5==1 | data$Q23_6==1 ] <- 1
data$barreg.dge <- 0
data$barreg.dge[data$Q23_7==1] <- 1
detach(data)
attach(data)


## ----barwind, echo=T-----------------------------------------------------
## tech: High investment costs. (1) High energy costs. (2) Environmental risks. (3) Technical implementation. (4) 
## stake: Opposition by citizens. (5) Opposition by interest groups. (6) 
## regulatory: Political/regulatory risks. (7) 
## OMITTED bc not directly related to DGE tech. The electricity grid is not ready (8)

data$bartech.wind <- 0
data$bartech.wind[data$Q20_1==1 |data$Q20_2==1 |data$Q20_3==1 | data$Q20_4==1] <- 1
data$barstake.wind <- 0
data$barstake.wind[data$Q29_5==1 | data$Q20_6==1] <- 1
data$barreg.wind <- 0
data$barreg.wind[data$Q20_7==1] <- 1
detach(data)
attach(data)


## ----barhp, echo=T-------------------------------------------------------
## tech: High investment costs. (1) High operational costs. (2) High maintenance costs. (3) Environmental risks. (4) Technical implementation. (5) 
## stake: Opposition by citizens. (6) Opposition by interest groups. (7) 
## regulatory: Policy/regulatory risks. (8) Lacking regulatory coordination. (9) 
## OMITTED bc not directly related to DGE tech. The electricity grid is not ready (8)

data$bartech.hp <- 0
data$bartech.hp[data$Q32_1==1 |data$Q32_2==1 |data$Q32_3==1 | data$Q32_4==1 |data$Q32_5==1] <- 1
data$barstake.hp <- 0
data$barstake.hp[data$Q32_6==1 | data$Q32_7==1] <- 1
data$barreg.hp <- 0
data$barreg.hp[data$Q32_8==1 | data$Q32_9==1] <- 1
detach(data)
attach(data)


## ----looplogit, echo=T, results='hide', message=F------------------------
goal <- c('ev', 'dge', 'wind', 'hp.mod', 'hp.exp', 'hp.small', 'hp.pump')
goal2 <- c('ev', 'dge', 'wind', 'hp', 'hp', 'hp', 'hp')
depvar <- c('Q28.f', 'Q22.f', 'Q19.f', 'Q31_4.f', 'Q31_3.f', 'Q31_2.f', 'Q31_1.f')
results <- list() ## this is a super amazing placeholder for final results.

for (i in 1:7){
  ## policy goal specific varibles.
  field <- paste0("act.", goal2[i])
  bartech <- paste0("bartech.", goal2[i])
  barstake <- paste0("barstake.", goal2[i])
  barreg <- paste0("barreg.", goal2[i])

  ## define regression eqn.
  common.var <- paste0(" ~ act.nuc + pol.act + media.act ") ## for all goals
  spec.var <- paste0(field, " + ", bartech, " + ", barstake, " + ", barreg)## goal specific vars.
  eqn <- paste0(depvar[i], common.var, " + ", spec.var)
  
  ## run ordered logit.
  model <- polr(eqn, data = data, Hess = T)
  mod.sum <- summary(model) ## AIC is stored in this
  ## conf int & odd ratios calculations.
  (ctable <- coef(mod.sum))
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)*2
  (ctable <- cbind(ctable, "p value" = p)) ## combined table
  (ci <- confint(model, level = 0.95)) ## default method gives profiled CIs
  confint.default(model)
  #exp(coef(model))  ## odds ratios
  ## store odd rations & CI.
  results[[goal[i]]] <- exp(cbind(OR = coef(model), ci)) 
}


## ----logitresults, echo=T------------------------------------------------
results


## ----summarypol----------------------------------------------------------
lapply(data[act.list.new], function(x) summary(pol.act[x==1]))


## ----summarymedia--------------------------------------------------------
lapply(data[act.list.new], function(x) summary(media.act[x==1]))


## ----influenceselfQ16, echo=T--------------------------------------------
infl.sub.levels <- c('No influence at all', 'Very little influence', 'Little influence', 'Moderate influence', 'Strong influence', 'Very strong influence')
data$infl.sub <- data$Q16
data$infl.sub.f <- factor(data$Q16, labels = infl.sub.levels)


## ----fundpolads, echo=T--------------------------------------------------
data$fund.ad <- ifelse(is.na(data$Q52_11), 0, 1)


## ----research, echo=T----------------------------------------------------
data$fund.ad <- ifelse(is.na(data$Q52_7), 0, 1)
detach(data)
attach(data)

