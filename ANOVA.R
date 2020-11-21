#ANOVA IN R
#Crop Data

# fertilizer type (type 1, 2, or 3)
# planting density (1 = low density, 2 = high density)
# planting location in the field (blocks 1, 2, 3, or 4)
# final crop yield (in bushels per acre).

mydata <- crop_data

head(mydata, n=5)

# After loading the dataset into our R environment, we can use the command aov() to run an ANOVA. 
#In this example we will model the differences in the mean of the response variable, crop yield, as a function of type of fertilizer.


ANOVA <- aov(yield~as.factor(fertilizer), data = crop_data)

#larger F, more likely it is the variation associated with teh ind var is real and not due to chance 
summary(ANOVA)

#Post-hoc Testing
#TukeyHSD
TukeyHSD(ANOVA)

paste('pairwise comparisons show fertilizer type 3 has higher yield than both fertilizer 2 and 1')

paste('We found a statistically-significant difference in average crop yield according to fertilizer type (f(2)=9.073, p < 0.001). ')

paste('A Tukey post-hoc test revealed significant pairwise differences between fertilizer types 3 and 2, with an average difference 
      of 0.42 bushels/acre (p < 0.05) and between fertilizer types 3 and 1, with an average difference of 0.59 bushels/acre (p < 0.01).')

summary(mydata)

hist(mydata$yield)

#two way anova lowered residual sum of squares from 35 to 30
#planting density and fertilizers are statistically significant
two.way <- aov(yield ~ as.factor(fertilizer) + as.factor(density), data=mydata)
summary(two.way)

#low sum of squares adn high p value, not much variation can be explained by interaction between fertilizer and planting density
interaction <- aov(yield~as.factor(fertilizer)*as.factor(density),mydata)
summary(interaction)

#blocking variable
#confounding variable
#low sum sq, high p-value
#not adding much info to model 
blocking <- aov(yield~as.factor(fertilizer)+as.factor(density)+as.factor(block), mydata)
summary(blocking)

#########BEST FIT################
library(AICcmodavg)

model.set <- list(one.way, two.way, interaction, blocking)
model.names <- c("one.way","two.way","interaction","blocking")

aictab(model.set, modnames = model.names)

#two.way seems to have the best fit (lowest AIC)
#cumulative weight .71, 75% of the total variation in the dependent variables is explained by the full set of models

###HOMOSCEDASTICITY##################3

#model fits the assumption of heteroscedasticity
par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

#post-hoc test
tukey.two.way <- TukeyHSD(two.way)
tukey.two.way
#3-1 and 3-2 have significiance 

#To display this information on a graph, 
#we need to show which of the combinations of fertilizer type + planting density 
#are statistically different from one another.

#find out which group means are statistically different from one another so we can add this information to the graph.
mydata$fertilizer <- as.factor(mydata$fertilizer)
mydata$density <- as.factor(mydata$density)

tukey.plot.aov<-aov(yield ~ fertilizer:density, data=mydata)

tukey.plot.test<-TukeyHSD(tukey.plot.aov)

#differences in mean levels of fertilizer density
#From this graph, we can see that the fertilizer + planting density combinations which are significantly different from one another are 3:1-1:1 (read as “fertilizer type three + planting density 1 contrasted with fertilizer type 1 + planting density type 1”), 1:2-1:1, 2:2-1:1, 3:2-1:1, and 3:2-2:1.
#To summarize, we have 3 distinct groupings. Fertilizer 3, planting density 2 is different from all of the other combinations, as is fertilizer 1, planting density 1. All of the others are intermediate. So we can make three labels for our graph: A (representing 1:1), B (representing all the intermediate combinations), and C (representing 3:2).


plot(tukey.plot.test, las = 1)


# First, summarize the original data using fertilizer type and planting density as grouping variables.
library(tidyverse)
mean.yield.data <- mydata %>%
  group_by(fertilizer, density) %>%
  summarise(
    yield = mean(yield)
  )

mean.yield.data$group <- c("a","b","b","b","b","c")

#plot the raw data
two.way.plot <- ggplot(mydata, aes(x = density, y = yield, group=fertilizer)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))

two.way.plot


#add mean and standard errors to graph 
two.way.plot <- two.way.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.yield.data, aes(x=density, y=yield))

two.way.plot

#split up the data
#facet_wrap() to split dat up over three types of fertilizer 
#geom_text() add labels
two.way.plot <- two.way.plot +
  geom_text(data=mean.yield.data, label=mean.yield.data$group, vjust = -8, size = 5) +
  facet_wrap(~ fertilizer) #split by fertilizer

two.way.plot

#make the graph ready for publication 
#remove grey background, add axis labels
two.way.plot <- two.way.plot +
  theme_classic() +
  labs(title = "Crop yield in response to fertilizer mix and planting density",
       x = "Planting density (1=low density, 2=high density)",
       y = "Yield (bushels per acre)")

two.way.plot

#Report Results 
#variables tested
#f-value, df, p-values for each independent varaible)
#what results mean 


# Example: Reporting the results of ANOVA
# 
# We found a statistically-significant difference in average crop yield by both fertilizer type (f(2)=9.018, p < 0.001) and by planting density (f(1)=15.316, p<0.001).
# A Tukey post-hoc test revealed that fertilizer mix 3 resulted in a higher yield on average than fertilizer mix 1 (0.59 bushels/acre), and a higher yield on average than fertilizer mix 2 (0.42 bushels/acre). Planting density was also significant, with planting density 2 resulting in an higher yield on average of 0.46 bushels/acre over planting density 1.
# 
# A subsequent groupwise comparison showed the strongest yield gains at planting density 2, fertilizer mix 3, suggesting that this mix of treatments was most advantageous for crop growth under our experimental conditions.

#SOURCE: https://www.scribbr.com/statistics/anova-in-r/#anova-graph






mean.yield.data