## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, purl = T)


## ----cars, warning=FALSE, results='hide'---------------------------------
library(ggplot2)
library(likert)


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
vision <- subset(data, select = c(Q19,Q22,Q25,Q28,Q31_1,Q31_2,Q31_3,Q31_4))
visionlevels <- c('Completely unrealistic', 'Rather unealistic', 'Rather realistic', 'Completely realistic')
vision.list <- c("Q19","Q22","Q25","Q28","Q31_1","Q31_2","Q31_3","Q31_4")
vision[vision.list] <- lapply(vision[vision.list], function(x) factor((5-x), labels = visionlevels))


## ----plottheme-----------------------------------------------------------
# plot theme.
blank_themep <-  #theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title= element_text(size=15, face="bold", hjust = 0.5, color = "black"),
    legend.title=element_text(size=11, face="bold", color = "black"),
    legend.text=element_text(size=11, color = "black"),
    legend.key.size=unit(.01,"npc"),
    axis.text.x=element_text(size=10, color = "black"),
    axis.text.y=element_text(size=11, color = "black"),
    aspect.ratio = 1/2.5
   )


## ----gray----------------------------------------------------------------
# my gray scale.
n <- 4
visioncolors <- gray.colors(n, start = 0.5, end = 0.1, gamma = 1.8)


## ----plotvision, echo=T--------------------------------------------------
options(digits=4)
what <- subset(vision, select = c(Q19,Q22,Q25,Q28))
names(what) <- c(
			Q19="Construction of 800-900 wind turbines by 2050.",
			Q22="Large increase in power generation by DGE.",
			Q25="RE targets with subsidy (incl FITs) phase-outs.",
			Q28="15% EVs among newly registered cars by 2022.")
dimwhat<- dim(na.omit(what))

likewhat <- likert(what)
plot <- likert.bar.plot(likewhat, wrap = 25, low.color = visioncolors[1], high.color = visioncolors[n], neutral.color = "grey90", legend = "", legend.position = "bottom") +
  blank_themep
plot$layers[[2]]$geom_params$width = 0.3 ## for thinner bars
plot$layers[[3]]$geom_params$width = 0.3
plot(plot)


## ----plotvisionHP, echo=F------------------------------------------------
what <- subset(vision, select = c(Q31_1,Q31_2,Q31_3,Q31_4))
names(what) <- c(
			Q31_1="By new pumped storage facilities.",
			Q31_2="By new small hydropower plants.",
			Q31_3="By expansion of existing plants.",
			Q31_4="By modernization of existing plants.")
dimwhat <- dim(na.omit(what))

likewhat <- likert(what)
plot.hp <- likert.bar.plot(likewhat, wrap =25, low.color = visioncolors[1], high.color = visioncolors[n], neutral.color = "grey90", legend = "", legend.position = "bottom") +
  blank_themep
plot.hp$layers[[2]]$geom_params$width = 0.3
plot.hp$layers[[3]]$geom_params$width = 0.3
plot(plot.hp)


## ----saveplots, echo=T, results='hide', eval=T---------------------------
png(file="../outputs/fig_vision4_hr.png", width = 8, height = 4, units = "in", res = 300) ## hi
    plot
dev.off()
png(file="../outputs/fig_vision4_lr.png", width = 8, height = 4, units = "in", res = 70) ## low
    plot
dev.off()
png(file="../outputs/fig_vision_hp_hr.png", width = 8, height = 4, units = "in", res = 300)
    plot.hp
dev.off()
png(file="../outputs/fig_vision_hp_lr.png", width = 8, height = 4, units = "in", res = 70)
    plot.hp
dev.off()

