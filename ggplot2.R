#ggplot2 
#install.packages("ggplot2")
library(ggplot2)
dtTips=read.csv('tips.csv')
head(dtTips)

#univar gaphs
##++++++++++++++++++Pie chart
#Gender

fdtGender=table(dtTips$sex)
fdtGender

fdtGender=as.data.frame(fdtGender)

fdtGender
colnames(fdtGender)=c("Gender","Count")
fdtGender

##we use the FDT of Gender as input for ggplot

g0=ggplot(fdtGender, aes(x="", y=Count, fill=Gender))
g1=g0+geom_col()+
  coord_polar(theta = "y")+
  theme_void()+
  theme(plot.title = element_text(colour = "blue",
                                  size = 12, 
                                  face = "bold", 
                                  hjust = .5))+
  ggtitle('Gender Distribution of Customers')+
  geom_text(aes(label=Count), 
            position = position_stack(vjust = .5))+
  scale_fill_manual(values = c('#99FF33', '#BE2A3E'))+
  theme(legend.position = 'bottom')

ggsave('genderDist.png')

g1


###++++++++++++++++++++++Bar Chart

tGender=table(dtTips$sex)
tGender=as.data.frame(tGender)
colnames(tGender)=c('Gender', 'Count')

g0=ggplot(tGender, aes(x=Gender, y=Count, fill=Gender))
g0+geom_bar(stat='identity')+
  theme_classic()+
  theme(legend.position = '')+
  theme(axis.title.x = element_text(),
        axis.title.y = element_text(),
        plot.title = element_text(face = 'bold', hjust=.5))+
  ggtitle('Customers Gender Distribution')+
  geom_text(aes(label=Count), vjust=2)+
  scale_fill_manual(values=c('#FF9933', '#0000CC'))
ggsave('genderBar.pdf')


###++++++++++++++++++++++++++Histogram

g0=ggplot(dtTips, aes(x=tip))
g0+geom_histogram(bins = 10, fill='#99FFFF', colour=4)+
  theme_classic()+
  theme(plot.title = element_text(face = 'bold',
                                  hjust = .5), 
        axis.title.x = element_text(), 
        axis.title.y = element_text())+
  ggtitle('Tip Distribution')+
  xlab('Tip Amount')+
  ylab('Frequency')+
  geom_vline(xintercept = mean(dtTips$tip),
             linetype='dashed',
             color='red', 
             size=1)+
  geom_vline(xintercept = median(dtTips$tip),
             linetype='dashed',
             color='blue', 
             size=1)
ggsave('tipDistHist.png')


###++++++++++++++++++++Density plot

g0=ggplot(dtTips, aes(x=tip))
g0+geom_density(color='red', size=.1)+
  theme_classic()+
  xlim(0,12)+
  theme(plot.title = element_text(face = 'bold',
                                  hjust = .5), 
        axis.title = element_text(), 
        axis.title.y = element_text())+
  ggtitle('Tip Distribution')+
  xlab('Tip Amount')+
  ylab('Density')+
  geom_vline(xintercept = 5,
             linetype='dashed',
             color='blue', 
             size=1)+
  geom_hline(yintercept = .3,
             linetype='dashed',
             color='blue', 
             size=1)
ggsave('tipDistHist.png')


##ECDF 
g0=ggplot(dtTips, aes(x=tip))
g1=g0+stat_ecdf(geom = "step", 
                col="red")+
  theme_classic()+
  theme(plot.title = element_text(face = 'bold',
                                  hjust = .5), 
        axis.title = element_text(), 
        axis.title.y = element_text())+
  ggtitle('Tip C. Distribution')+
  xlab('Tip Amount')+
  ylab('C. Density')+
  geom_vline(xintercept = 5,
             linetype='dashed',
             color='blue', 
             size=1)+
  geom_hline(yintercept = .92,
             linetype='dashed',
             color='blue', 
             size=1)


g1

ecdf(dtTips$tip)(5)
###+++++++++++++++++++Box Plot

g0=ggplot(dtTips, aes(y='',x=tip))
g0+geom_boxplot(fill=5, 
                color=6, 
                alpha=0.3, 
                outlier.colour = 'blue', 
                linetype=2, 
                lwd=.6)+
  theme_classic()+
  theme(axis.title.x = element_text(), 
        plot.title = element_text(face = 'bold',
                                  hjust = .5, 
                                  color='darkgreen'))+
  ggtitle('Box Plot of the Tip')+
  xlab('Tip Amount')
ggsave('boxplotTip.png')




#Bivariate/Multivar graphs
#2 QL vars
#1 QL and 1 QNT
#2 QNTS
###+++++++++++++++++++Joint graphs
#Gender[F, M] and Smoker[Y, N] Join barplot for two QL vars

jtable=table(dtTips$sex, dtTips$smoker)
jtable=as.data.frame(jtable)
jtable

ggplot(jtable, aes(x=Var1, y=Freq, fill=Var2))+
  geom_col(position = position_dodge())+
  theme_classic()+
  theme(axis.title.x = element_text(),
        legend.title = element_text(color = 'blue'),
        plot.title = element_text(face = 'bold',
                                  hjust = .5, 
                                  color='darkgreen'),
        legend.position = "bottom")+
  ggtitle('Join bar graph of Gender and Smoking')+
  xlab('Gender')+
  guides(fill=guide_legend('Smoking'))

ggsave('jointBarGenderSmoke.pdf')


####+++++++++++++++++++Joint density 
##Gender[F, M]  tip for on QL and one QNT variables

g0=ggplot(dtTips, aes(x=tip, color=sex))
g0+geom_density()+
  theme_replace()+
  scale_color_manual(values = c('red', 'blue'))+
  xlim(-2,11)+
  theme(plot.title = element_text(face="bold",
                                  hjust = .5), 
        axis.title.x = element_text(),
        axis.title.y = element_text(), 
        legend.title = element_text(color='blue'),
        legend.position = "bottom")+
  ggtitle('Joint distribution of Tip amount across Gender')+
  xlab('Tib Amount')

ggsave('jointDensity.png')

#####+++++++++++++++++++Ridgeline plot
#smoker[y, n] tip 
#install.packages('ggridges')
library(ggridges)

ggplot(dtTips, aes(x=tip, y=sex, fill=sex))+
  geom_density_ridges(color=7, 
                      lwd=.5)+
  theme_gray()+
  theme(plot.title = element_text(face = 'bold', 
                                  hjust = .5), 
        axis.title.x = element_text(),
        axis.title.y = element_text(), 
        legend.title = element_text(color='blue'),
        legend.position = "bottom")+
  ggtitle('Joint dist of Tip and Gender')+
  xlab('Tip Amount')

ggsave('jointDistTipsSmoker.png')


######+++++++++++++++++++Joint box plot
#Gender[y,N] tip


ggplot(dtTips, aes(x=tip, y=sex, fill=sex))+
  geom_boxplot(color=2, 
               alpha=0.3, 
               outlier.colour = 'blue', 
               linetype=2, 
               lwd=.6)+
  stat_boxplot(geom = 'errorbar', 
               width=.5)+
  theme_gray()+
  theme(plot.title = element_text(face = 'bold', 
                                  hjust = .5), 
        axis.title.x = element_text(),
        axis.title.y = element_text(), 
        legend.title = element_text(color='blue'), 
        legend.position = "bottom")+
  ggtitle('Joint dist of Tip and Gender')+
  xlab('Tip Amount')+
  ylab('Gender')+
  xlim(-1,11)
ggsave('jointboxplot.png')

######+++++++++++++++++++boxplot with points##not part of ggplot

boxplot(dtTips$tip, col = 'white', horizontal = T)
stripchart(dtTips$tip, 
           method = 'jitter',
           pch=1, 
           col=4, 
           add = TRUE)

help("stripchart")
######+++++++++++++++++++Joint boxplot
##Gender[y,n] tip

boxplot(tip~sex,
        data = dtTips,
        col='white',
        horizontal = T)
stripchart(tip~sex,
           data = dtTips,
           method = 'jitter', 
           pch=19, 
           col=2:4,
           add = TRUE)
ggsave('jointboxplotwithpoints.pdf')
####+++++++++++++++++++Beeswarm graph

##install.packages('ggbeeswarm')

library(ggbeeswarm)
#smoker[y, n] tip

ggplot(dtTips, aes(x=smoker, y=tip, color=sex))+
  geom_beeswarm(cex=1)+
  theme(legend.position = "bottom", 
        legend.title = element_text(color='blue'))

ggsave("beeswarm.png")

#2 QNT vars: Scatter plot
g0=ggplot(dtTips, aes(x=total_bill, y=tip))
g0+geom_point()

#modifications inside geom_point
g0=ggplot(dtTips, aes(x=total_bill, y=tip))
g0+geom_point(color=10, shape=5, size=2)

names(dtTips)
#Modifications in aes
g0=ggplot(dtTips, aes(x=total_bill, y=tip, color=sex))
g0+geom_point()

#Modifications in both 
g0=ggplot(dtTips, aes(x=total_bill, y=tip, alpha=size))
g0+geom_point(color=4, size=4)

#Adding a vertical line and size
g0=ggplot(dtTips, aes(x=total_bill, y=tip,color=time, size=size))
g0+geom_point()+
  geom_vline(xintercept = 40, linetype='dashed')+
  geom_hline(yintercept = 5, linetype='dashed')

#Adding Facet:
g0=ggplot(dtTips, aes(x=total_bill, y=tip,color=time, size=size))
g0+geom_point()+
  facet_wrap(~day)

g0=ggplot(dtTips, aes(x=total_bill, y=tip, color=time, size=size))
g0+geom_point()+
  facet_wrap(~smoker)

##Adding Regression line
g0=ggplot(dtTips, aes(x=total_bill, y=tip))
g0+geom_smooth()

g0=ggplot(dtTips, aes(x=total_bill, y=tip, group=sex))
g0+geom_smooth()+
  geom_point()

names(dtTips)
#Adding more detials
g0=ggplot(dtTips, aes(x=total_bill, y=tip, color=sex))
g1=g0+geom_smooth(se=FALSE)+
  geom_point(mapping = aes(color=time))+
  theme_bw()+
  theme(axis.title.x = element_text(), 
        axis.title.y = element_text(), 
        plot.title = element_text(hjust = .5), 
        legend.title = element_blank())+
  ggtitle('Scatter plot total_bill/tip')+
  xlab('Total Bill in USD')+
  ylab('Tip Amount')+
  theme(legend.position = 'bottom')
g1

##Export as html and ggplotly
#install.packages('plotly')
library(plotly)

g2=plotly::ggplotly(g1)
htmlwidgets::saveWidget(g2, 
                        file = 'scatter.html')
