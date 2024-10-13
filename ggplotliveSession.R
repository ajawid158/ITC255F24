#ggplot 2
#Week 7 Of the Fall 24 
#Midterm on Wednesday 16 October// Schedules will be shared this Week

#GGPLOT 2
   #Data
     #Mapping (aesthetics: x, y, color, fill, group)
       #geometry (scatter, point, line, etc.)
#facets 
#lables
#theme
#Statistics 
#Coordinate Space

dt=read.csv("tips.csv")
names(dt)
#work with tip and total bill

g1=ggplot(dt, aes(x=total_bill, y=tip, colour = smoker))+
    geom_point(aes(colour=day))+
  geom_smooth(method = lm, se=F)+
  facet_wrap(~time)+
  labs(x="Total Bill",
       y="Tip Amount", 
       title = "Tip vs TB, Smoker, DAY")+
  theme(legend.position="bottom")  

g1

ggsave("tipScatter.png")

#Standard Error = CI
library(plotly)
g2=plotly::ggplotly(g1)
htmlwidgets::saveWidget(g2, file = "tipsToday.html")




ggplot(dt, aes(x=sex))+
  geom_bar()+
  coord_polar()











