## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----cars, warning=FALSE-------------------------------------------------
library(ggplot2)


## ----data----------------------------------------------------------------
data <- data.frame(
   actor = c("Energy businesses", "Business associations", "Non-business associations", "Other businesses", "Municipalities", "Cantons", "Communal and cantonal assoc./orgs", "Political parties", "Educational institutions", "Energy businesses", "Business associations", "Non-business associations", "Other businesses", "Municipalities", "Cantons", "Communal and cantonal assoc./orgs", "Political parties", "Educational institutions"),
  percent = c(32, 22, 20, 7, 7, 4,  3, 3, 2, 30, 23, 23, 4, 4, 6, 4, 3, 3), ## % of population first, then sample
  group = c(rep("Population", 9), rep("Our Sample", 9)),
  order = c(as.character(rep(1:9,2)))
)


## ----plottheme-----------------------------------------------------------
# plot theme.
blank_themep <-  #theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #panel.border = element_blank(),
    #panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title= element_text(size=15, face="bold",hjust = 0.5),
    legend.title=element_text(size=12, face="bold"),
    legend.text=element_text(size=12),
    axis.text.x=element_text(size=12, face="bold"),
    axis.text.y=element_text(size=12)
   )


## ----gray----------------------------------------------------------------
# my gray scale.
n <- 9
grayscale <- gray.colors(n, start = 0.5, end = 0.9, gamma = 1.8)


## ----bar-----------------------------------------------------------------
base <- 
  ggplot(data, aes(x=group, y=percent, fill=order, label = paste(percent,"%"))) +
  geom_bar(width = 0.3,  color = "white", size = 0.4, stat = "identity", position = "stack") +  # color/size for borders
  scale_fill_manual(values = grayscale[1:n], labels = data$actor)+
  geom_text(size = 3.5, position = position_stack(vjust = 0.5)) +  # for graph labels
  labs(fill ="Respondent types") +
  blank_themep
plot(base)


## ----save, results='hide'------------------------------------------------
png(file="../outputs/fig_actor_hr.png", width = 7, height = 5, units = "in", res = 300) ## high res
    base
dev.off()
png(file="../outputs/fig_actor_lr.png", width = 7, height = 5, units = "in", res = 70) ## low res
    base
dev.off()

