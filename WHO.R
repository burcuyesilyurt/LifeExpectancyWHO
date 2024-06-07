library(tidyverse) # metapackage with lots of helpful functions
install.packages("remotes")
library(remotes)

install.packages("car")
library(car)
library(caTools)

data <- read.csv("/Users/burcuyesilyurt/Downloads/master/Statistics/project/Life Expectancy Data.csv")
View(data)
data1 <- subset(data,Year==2014)
data1
data2<-data1[complete.cases(data1[, c(4,7,8,11,17,18,21,22)]),]
data2

#Remove all data points with NA for the variables we want.
model1<-lm(Life.expectancy~Schooling+Income.composition.of.resources+Total.expenditure+percentage.expenditure+BMI+Alcohol, data=data2)

summary(model1)

#This is just looking at demographic/behavioral variables, not health outcomes. 
#As in, what characteristics of a country or its population might predict life expectancy? 
#Not including things like malaria, etc. which probably signify a result more than an input
#Looks like a lot of variables are not significant. Let's use stepwise selection to find the best model.

model2<-step(model1, data=data2, direction = "both", test="F")
summary(model2)

#Okay, looks like total health expenditure as a % of total expenditure and Human Development Index 
#(i.e. income composition of resources) are the two strongest predictors. 
#Let's see if this changes when we add GDP per capita into the mix.

model3<-lm(Life.expectancy~Schooling+Income.composition.of.resources+Total.expenditure+percentage.expenditure+BMI+Alcohol+GDP, data=data2)

cor_table<-cor(data2[c(7,8,11,17,18,21,22)])

vif(model3)

#Yep, there's definitely likely to be some collinearity here. Let's find the best model out of these.

model4<-step(model3, data=data2, direction = "both", test="F")

summary(model4)

#Nothing changed from model2. not suprised.

vif(model4)

#Collinearity fixed though.

plot(model4)


#High leverage and low residual: number 2299, Sierra Leone. 
#QQ plot: well, it's more or less normal. 
#the only issue is how short the tails are. but I feel like that's not a huge deal. 
#Residuals vs. leverage: no point cook's distance even approaching .5. Perfect.

#Income composition of resources (ICR) seems like it's a composite variable in itself 
#It's a combination of life expectancy and a few other quality of life variables, so it's not surprise that we're seeing collinearity. 
#Let's see what happens if we remove it.

model5<-lm(Life.expectancy~Schooling+Total.expenditure+percentage.expenditure+BMI+Alcohol+GDP, data=data2)

vif(model5)

#Yep, there's definitely likely to be some collinearity here. Let's find the best model out of these.

model6<-step(model5, data=data2, direction = "both", test="F")
summary(model6)

vif(model6)

#Oh wow. It really does look like ICR was just a sum of all these other variables. 
#Interestingly, total expenditure was less significant. But schooling, BMI, and GDP are all significant. 
#BMI has a POSITIVE relationship with health. Weird, you'd think that it would be unhealthy. 
#I guess it is just a signifier of wealth. But i thought that was what GDP is? let's test these two against each other.

#Collinearity fixed!

data_developed <- subset(data,Status=="Developed")
data_developing <- subset(data,Status=="Developing")

data_developed <- data_developed[complete.cases(data_developed[, c(4,22)]),]
data_developing <- data_developing[complete.cases(data_developing[, c(4,22)]),]

developed_school_model <- lm(Life.expectancy~Schooling, data=data_developed)
developing_school_model <- lm(Life.expectancy~Schooling, data=data_developing)
school_model<- lm(Life.expectancy~Schooling, data=data)

summary(developed_school_model)
summary(developing_school_model)
summary(school_model)
#Interesting - since the schooling coefficient for developing is over 2x higher than that of developed,
#it appears that schooling has a stronger impact on life expectancy in developing countries. That is, on
#average, in developing countries, each additional year of school adds two years to one's life, while in
#developed countries, it adds about 0.9 years. In both cases, the varaible schooling is (very) significant.

#However, this may just result from the fact that schooling is strongly related to other variables (as found
#earlier). Now, let's control for other similar variables and see if schooling is still a significant predictor.
#Schooling was highly correlated with income composition of resources and alcohol. Controlling for those two,
#as well as status, let's see if schooling is still significant.

status_model <- lm(Life.expectancy~Status + Schooling + Alcohol + Income.composition.of.resources, data=data)
summary(status_model)
#Even when controlling for status, alcohol, and income composition of resources, schooling is still significant.

plot(status_model, which=2)
#That being said, the data definitely does not fit the model assumptions. Let's see if we can transform the data.

boxCox(status_model, lambda = seq(-5,5,1))
?boxCox
#Let's try a Box-Cox transformation with lambda = 3.5

life_transform <-((data$Life.expectancy)^3.5-1)/(3.5)
life_transform
data<-cbind(data,life_transform)

status_model2 <- lm(life_transform~Status + Schooling + Alcohol + Income.composition.of.resources, data=data)
plot(status_model2)
summary(status_model2)

#Well, looks like it helped for the lower end of the data, but not the upper end. Let's take a look at
#those problem values
data
#Seems like the isues is with Antigua and Barbuda.

dataNoAnt <- subset(data,Country!="Antigua and Barbuda")

status_model3 <- lm(life_transform~Status + Schooling + Alcohol + Income.composition.of.resources, data=dataNoAnt)
plot(status_model3)



#Well, the right-skewedness of the data (as evidenced by the concave-up Q-Q plot) was worse than I thought,
#and not just limited to Antigua and Barbuda. Even so, the model is still useful, and tells us that schooling
#does have a significantly positive relationship with life expectancy.


#Q2: Should a country having a lower life expectancy value(<65) increase its healthcare expenditure in order to improve its average lifespan? 
#We'll check to see if there's a significant difference in percentage of total expenditure between countries with a high life expectancy versus those with a low life expectancy.


data1.2 = data1[complete.cases(data1),]
data1.2$LowLifeExpectancy = data1.2$Life.expectancy < 65


healthcareExpendLLE = data1.2$Total.expenditure[data1.2$LowLifeExpectancy] #average percent health care expenditure for countries with low life expectancy (< 65)
healthcareExpendHLE = data1.2$Total.expenditure[!data1.2$LowLifeExpectancy] #average percent health care expenditure for countries with low life expectancy (< 65)
t.test(healthcareExpendLLE, healthcareExpendHLE)

#Because the resulting p-value is 0.1959 which is greater than our  𝛼=0.05 , we fail to reject the null hypothesis 
#and cannot conclude that countries with high life expectancy spend more of their money on healthcare than countries with low life expectancy. 
#We can also test to see if there's a linear relationship between life expectancy and percentage of total expenditure.

model.2 = lm(Life.expectancy ~ Total.expenditure, data = data1.2)
plot(model.2$fitted.values, model.2$residuals)
points(model.2$fitted.values[lm.influence(model.2)$hat > 2*mean(lm.influence(model.2)$hat)], model.2$residuals[lm.influence(model.2)$hat > 2*mean(lm.influence(model.2)$hat)], col="red")
data1.2[lm.influence(model.2)$hat > 2*mean(lm.influence(model.2)$hat), ]
plot(model.2, which = 4)
summary(model.2)

#From this result, we see that percentage of total expenditure is certainly a factor in predicting life expectancy because its p-value is  0.000201 , 
#but the  𝑅2  is still very low because using only expenditure is not going to yield very accurate predictions of life expectancy. 
#There are fifteen high leverage points shown with red circles on the plot above. 
#Since none of the points have a Cook's distance greater than 0.5, none of the points are influential.

#Q3: How does Infant and Adult mortality rates affect life expectancy?

#From a visual inspection of the data, India has erroneous values for infant deaths (most years are greater than 1000 infant deaths per 1000 people) 
#so we will filter out that country and fit a model without those values.

data1.3 = subset(data1.2, data1.2$Country != "India") 
model.3 = lm(Life.expectancy ~ Adult.Mortality + infant.deaths, data = data1.3)
plot(model.3$fitted.values, model.3$residuals)
points(model.3$fitted.values[lm.influence(model.3)$hat > 2*mean(lm.influence(model.3)$hat)], model.3$residuals[lm.influence(model.3)$hat > 2*mean(lm.influence(model.3)$hat)], col="red")
data1.3[lm.influence(model.3)$hat > 2*mean(lm.influence(model.3)$hat), ]
plot(model.3, which = 4)
summary(model.3)

#At a significance level of  0.05 , we see that both adult mortality and infant deaths have nonzero coefficients by looking at their p-values. 
#We also see that the coefficients  𝛽̂  are negative which means that when adult mortality or infant deaths is higher, life expectancy tends to be lower.

#There are six high leverage points shown with red circles on the plot above. 
#Since none of the points have a Cook's distance greater than 0.5, none of the points are influential.










