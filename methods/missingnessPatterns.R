### missing data #########################################################

#site selection as a missing data problem:
#depend on the response value (species occurrence)

#https://cran.r-project.org/web/views/MissingData.html
#https://cran.r-project.org/web/packages/ipw/index.html
#https://blog.usejournal.com/missing-data-its-types-and-statistical-methods-to-deal-with-it-5cf8b71a443f

biodiversityX <- seq(-1,1,length.out=20)
environmentY <- seq(-1,1,length.out=20)
fullGrid <- expand.grid(biodiversityX=biodiversityX,
                        environmentY=environmentY)

#missing completely at random
fullGrid$MCAR <- plogis(0)
fullGrid$MCAR <- rbinom(nrow(fullGrid), 1, fullGrid$MCAR)

#missing at random
fullGrid$MAR <- plogis(0 + 3*fullGrid$environmentY)
fullGrid$MAR <- rbinom(nrow(fullGrid), 1, fullGrid$MAR)

#missing not at random
fullGrid$MNAR <- plogis(0 + 3*fullGrid$biodiversityX)
fullGrid$MNAR <- rbinom(nrow(fullGrid), 1, fullGrid$MNAR)

#draw grid - tile plot with probability that data is missing
library(tidyverse)
fullGrid_long <- fullGrid  %>%
  pivot_longer(cols = starts_with("M"), names_to = "Pattern", values_to = "Visited")

fullGrid_long$Pattern <- factor(fullGrid_long$Pattern,
                                levels=c("MCAR","MAR","MNAR"))
fullGrid_long$Visited <- ifelse(fullGrid_long$Visited==1,"Yes","No")

ggplot(fullGrid_long)+
  geom_tile(aes(x = environmentY, y = biodiversityX,fill=Visited))+
  facet_wrap(~Pattern,nrow=1)+
  theme_minimal()+
  scale_fill_manual(values=c("grey95","black"))+
  theme(axis.text = element_blank())+
  xlab("environmental gradient")+ylab("biodiversity gradient")
