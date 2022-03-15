
##################################### importing required libraries ##################################
library(psych)  # general functions
library(ggplot2)  # data visualization
library(car)  # regression diagnostics
library(dplyr) #dataframe manipulation
##################################### loading the data set ##################################
df=read.csv('D:/Statistics-Eyvazian/Project/project- Regression- Geely Car Price/Geely.csv')

##################################### Analyzing the data set ##################################
head(df)   #first 6 rows
dim(df)  #dimension of dataset
sum(is.na(df))  #checking for null values
str(df)  #structure of predictors
describe(df)   #statistical summary of the dataset

##################################### Cleaning the data set ##################################
#the Car_id is of no use, so we remove it
df= df[,-c(1)]
head(df)

#checking for duplicated observations in dataset
sum(df%>%duplicated())

#the number of levels in each categorical variable
table(df$CarName)
table(df$fueltype)
table(df$aspiration)
table(df$carbody)
table(df$drivewheel)
table(df$enginelocation)
table(df$enginetype)
table(df$fuelsystem)
table(df$cylindernumber)

#the frequency for "rotor" enginetype is low, so it may be better to combine it with
#another level to which has the most similarity
ggplot(df, aes(x=enginetype, y= price)) + geom_point()
#according to the plot, it seems that "rotor" is most similar to "l"
#so we replace these two with "l & rotor"
df$enginetype[df$enginetype=='l']='l & rotor'
df$enginetype[df$enginetype=='rotor']='l & rotor'

#there are many levels for car name. By seeing the CarName, we can infere that the name comprises
#of two parts, Company Name and Car Name. So, we can categorize Cars with only Company Name
library(stringr)
a=data.frame(str_split_fixed(df$CarName, " ", 2))
df$CarCompany=a$X1

#seeing the levels in CarCompany
table(df$CarCompany)

#removing CarName from the dataset
df=df[,-c(2)]

# it seems that some of Company Names are misspelled.
df$CarCompany[df$CarCompany=='toyouta']='toyota'
df$CarCompany[df$CarCompany=='Nissan']='nissan'
df$CarCompany[df$CarCompany=='maxda']='mazda'
df$CarCompany[df$CarCompany=='vokswagen']='volkswagen'
df$CarCompany[df$CarCompany=='vw']='volkswagen'
df$CarCompany[df$CarCompany=='porcshce']='porsche'

#converting "char" variables to "factor"
df$fueltype=factor(df$fueltype)
df$aspiration=factor(df$aspiration)
df$carbody=factor(df$carbody)
df$drivewheel=factor(df$drivewheel)
df$enginetype=factor(df$enginetype)
df$enginelocation=factor(df$enginelocation)
df$fuelsystem=factor(df$fuelsystem)
df$cylindernumber=factor(df$cylindernumber)
df$CarCompany=factor(df$CarCompany)
df$doornumber =factor(df$doornumber)


table(df$symboling)
#as the frequency for symboling -2 is low, it is better to allocate only one group for negative symboling
df$symboling=num(df$symboling)
df$symboling=ifelse(df$symboling < 0, 'negative', df$symboling)
#convering "symboling" from numerical into categorical  
df$symboling=factor(df$symboling)


str(df)  #structure of predictors

#for having better analysis it is better to seperate numerical and categorical variables
df_cat=df[, sapply(df, class) == 'factor']
df_num=df[, sapply(df, class) != 'factor']

##################################### Visualizing the dataset ##################################

#plotting the distribution of wheel base
library(ggplot2)
ggplot(df, aes(x=wheelbase)) + geom_histogram()

#showing outlier in our response variable (price)
boxplot(df$price)
#he boxplot shows that there are a number of observations that have prices upper than 30000. these points can be considered as outlier.
#However, we do not remove them yet, until we run the model.

#visulizing CarCompany
#frequency of levels in CarCompany
library(treemap)
CarCompany=data.frame(table(df$CarCompany))
treemap(CarCompany, index="Var1",vSize="Freq")

#the average price for each level of CarCompany
CompanyPrice=by(df$price,list(df$CarCompany),mean)
barplot(CompanyPrice, col="#69b3a2",horiz=T , las=1)

#the boxplot for all levels of CarCompany
library(hrbrthemes)
library(viridis)
df %>%
  ggplot( aes(x=CarCompany, y=price, fill=CarCompany)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")
#Attention:zoom the plot to see all the details
#As is shown, about 15% (the largest share) of cars are from "toyota" company
#the most expensive cars are from "jaguar", "buick" and "porsche" company.
#However, they only account a small number of cars.


#density plot for price
d <- density(df$price)
plot(d, main="Kernel Density of Miles Per Gallon")
polygon(d, col="red", border="blue")

#list of numerical columns        
colnames(df_num)

# Pair plot for all the numeric variables
pairs(~price+wheelbase+carlength+carwidth+carheight+curbweight++enginesize+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+highwaympg ,data = df_num)
#as there are many predictors in the model, it is better to use corrologram for showing correlation
library(GGally)
ggcorr(df_num, method = c("everything", "pearson")) 
#As is shown in the pair plot, it seems that "wheelbase", "carlength", "carwidth", "curbweight", "enginesize", "boreratio" and "horsepower" are positively correlated with the response variable (price)
#on the contrary, "citympg" and "highwaympg" seem to have negative correlation with "price"
#it seems that "carheight" and "peakpm" do not have any significant correlation with "price"

#interaction plot for categorical variables
colnames(df_cat)
 a1=ggplot(df, aes(x=as.factor(symboling), y=price)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("cyl")
a2=ggplot(df, aes(x=as.factor(fueltype), y=price)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("cyl")
a3=ggplot(df, aes(x=as.factor(aspiration), y=price)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("cyl")
a4=ggplot(df, aes(x=as.factor(doornumber), y=price)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("cyl")
a5=ggplot(df, aes(x=as.factor(drivewheel), y=price)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("cyl")
a6=ggplot(df, aes(x=as.factor(enginelocation), y=price)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("cyl")
a7=ggplot(df, aes(x=as.factor(enginetype), y=price)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("cyl")
a8=ggplot(df, aes(x=as.factor(fuelsystem), y=price)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("cyl")
a9=ggplot(df, aes(x=as.factor(cylindernumber), y=price)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("cyl")

library(ggpubr)
ggarrange(a1,a2,a3, a4,a5,a6,a7,a8,a9 + rremove("x.text"), 
          labels = c("A", "B", "C", "D", "E", "F","G", "H", "I"),
          ncol = 3, nrow = 3)
#For "symboling", a value of +3 indicates that the auto is risky, -3 that it is probably pretty safe.
#on average, The cars with fueltype as diesel are more expensive than those with gas
#the cars with "turbo aspirations" have considerably higher average price than those with "std"
#there is no significant difference between cars with two or four "doornumber"
#generally, cars that have convertible and hardtop body, have wider range of prices than others
#cars with "rwd drivewheel" have significantly higher average prices than those with fwd and 4wd drivewheel
#on average, the cars with "rear engine locations" are considerably more expensive than those with "fron engine locations"
#cars with "ohcv engine type" have higher average prices than the others
#cars with "bbl fuel system" have lower average price than the care with other type of fuel system
#by increasing the number of cylinder, the price of cars rises


#As the levels for CarCompany is too many and the frequency for some of them is low, it is better to categorize them to larger groups.
# We can bin Car Companies by their average price.  
library(dplyr)
CarCompany=df %>% group_by(CarCompany) %>%summarise(mean_run = mean(price))
CarCompany$CompanyGroup=ifelse(CarCompany$mean_run<10000 ,'Budget_Friendly',ifelse(CarCompany$mean_run<20000 & CarCompany$mean_run>10000,'Medium_Range',
              'TopNotch_Cars'))
head(CarCompany)
df=merge(df, CarCompany)
head(df)

#remove CarCompany from the dataframe
df=df[,-c(1)]

#based on our visualization, we choose variables that have greater effect on the price
col = c('price','CompanyGroup','enginetype','enginelocation','fueltype', 'aspiration',
        'carbody','cylindernumber', 'drivewheel','symboling','wheelbase','carlength',
        'carwidth','curbweight','enginesize','boreratio','horsepower','carheight','peakrpm')

df=df[,col]

##################################### Data Preparation ##################################
#it is better to convert categorical variables into numerical variables, using Dummy Function. 
cat_col=c('CompanyGroup','enginetype','enginelocation','fueltype', 'aspiration',
          'carbody','cylindernumber', 'drivewheel','symboling')
library('fastDummies')
df1=dummy_cols(df,select_columns = cat_col,
  remove_first_dummy = TRUE,
  remove_most_frequent_dummy = FALSE,
  ignore_na = FALSE,
  split = NULL,
  remove_selected_columns = TRUE)
head(df1)
glimpse(df1)
colnames(df1)


#it is better to change the name of some columns
colnames(df1)[24]='cylindernumber_fivesix'
colnames(df1)[25]='cylindernumber_eighttwelve'
colnames(df1)[13]='enginetype_l_rotor'

str(df1)


#fit the model
fit=lm(price ~ wheelbase+carlength+carwidth+curbweight+enginesize+boreratio+horsepower+carheight+
       peakrpm+CompanyGroup_Medium_Range+CompanyGroup_TopNotch_Cars+enginetype_l_rotor+
       enginetype_ohc+enginetype_ohcf+ enginetype_ohcv+enginelocation_rear+fueltype_gas+aspiration_turbo+
       carbody_hardtop+carbody_hatchback+symboling_1+symboling_2+symboling_3+symboling_negative+carbody_sedan+
       carbody_wagon+cylindernumber_fivesix+cylindernumber_eighttwelve+drivewheel_fwd+drivewheel_rwd, data=df1)

#calculating Variance Inflation Factor for detecting "multicollinearity"
library(car)  # for regression diagnostics
vif(fit) 
#the VIF shows that there are some columns that have multicollinearity
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)

#it seems that there is not heteroscedasticity in dataset

#according to Normal Q-Q plot, it seems that Residuals are not normally distributed
#we can even test it by Anderson-Darling Test
library (nortest)
ad.test(residuals(fit))
#the p-value is less than 5, which rejects the hypothesis of normality

#So, we ue box-cox transformation to normalize the residuals
library(EnvStats)
b = boxcox(df1$price, objective.name = "Log-Likelihood", optimize = TRUE)
df1$price_trans = boxcoxTransform(df1$price, lambda = b$lambda)


#again, we run the model
fit=lm(price_trans ~ wheelbase+carlength+carwidth+curbweight+enginesize+boreratio+horsepower+carheight+
         peakrpm+CompanyGroup_Medium_Range+CompanyGroup_TopNotch_Cars+enginetype_l_rotor+
         enginetype_ohc+enginetype_ohcf+ enginetype_ohcv+enginelocation_rear+fueltype_gas+aspiration_turbo+
         carbody_hardtop+carbody_hatchback+symboling_1+symboling_2+symboling_3+symboling_negative+carbody_sedan+
         carbody_wagon+cylindernumber_fivesix+cylindernumber_eighttwelve+drivewheel_fwd+drivewheel_rwd, data=df1)

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)


#in order to detect "Outlier" residuals, we need to save residuals in a variable
library(MASS)
residuals= residuals(fit)

#calculating "Standardized Residuals"
stan_resid = rstandard(fit)

#calculating "Studentized Residuals"
stud_resid=rstudent(fit)

#in order to detect "oulier" predictors that are influenctial we can use "Cook's Distance" method
cook = cooks.distance(fit)

#Another method for detecting influential outliers is "Leverage", which uses Hat matrix
leverage = hat(model.matrix(fit))

#we need to know to which observations,the detected outliers belong
library(olsrr)  
ols_plot_cooksd_chart(fit)
ols_plot_dffits(fit)
ols_plot_resid_stud(fit)
ols_plot_resid_stand(fit)

#deleting outlier 
df1=df1[-c(26,30,75,129,130,139),]

##################################### Test and Train Split ##################################

library(lubridate)
library(caret)
library(tidyverse)

set.seed(123)

#train & test splitting
y_train=df1$price_trans %>%  createDataPartition(p = 0.8, list = FALSE)
train_data=df1[y_train,]
test_data=df1[-y_train,]

##################################### Model Building #######################################

#Building the model
fit2=lm(price_trans ~ wheelbase+carlength+carwidth+curbweight+enginesize+boreratio+horsepower+carheight+
         peakrpm+CompanyGroup_Medium_Range+CompanyGroup_TopNotch_Cars+enginetype_l_rotor+
         enginetype_ohc+enginetype_ohcf+ enginetype_ohcv+enginelocation_rear+fueltype_gas+aspiration_turbo+
         carbody_hardtop+carbody_hatchback+symboling_1+symboling_2+symboling_3+symboling_negative+carbody_sedan+
         carbody_wagon+cylindernumber_fivesix+cylindernumber_eighttwelve+drivewheel_fwd+drivewheel_rwd, data=train_data)

#make predictions for test data
pred=fit2 %>% predict(test_data)

#calculating R-Square, RMSE and MAE
data.frame( R2 = R2(pred, test_data$price_trans),
            RMSE = RMSE(pred, test_data$price_trans),
            MAE = MAE(pred, test_data$price_trans))

#We choose a model that has a lower rmse/mean(error rate)
RMSE1=RMSE(pred, test_data$price_trans)/mean(test_data$price_trans)
RMSE1


#Now we want to use STEPWISE method
library(MASS)

#stepwise-Forward
nullmod= lm(price_trans ~ 1, data = train_data)
fullmod=lm(price_trans ~  wheelbase+carlength+carwidth+curbweight+enginesize+boreratio+horsepower+carheight+
             peakrpm+CompanyGroup_Medium_Range+CompanyGroup_TopNotch_Cars+enginetype_l_rotor+
             enginetype_ohc+enginetype_ohcf+ enginetype_ohcv+enginelocation_rear+fueltype_gas+aspiration_turbo+
             carbody_hardtop+carbody_hatchback+symboling_1+symboling_2+symboling_3+symboling_negative+carbody_sedan+
             carbody_wagon+cylindernumber_fivesix+cylindernumber_eighttwelve+drivewheel_fwd+drivewheel_rwd,data=train_data)

reg1A= step(nullmod, scope = list(lower = nullmod, upper = fullmod),
            direction="forward")

reg1A
str(summary(reg1A))
summary(reg1A)


#stepwise-Backwards
reg2B= step(fullmod, scope = list(lower = nullmod, upper = fullmod),
            direction="backward")
str(summary(reg2B))
summary(reg2B)


#both Stepwise
reg1C= step(nullmod, scope = list(lower = nullmod, upper = fullmod),
            direction="both")
reg1C
summary(reg1C)

#all of subset 
library(leaps)
#attach(df)
leaps=regsubsets(price_trans ~ wheelbase+carlength+carwidth+curbweight+enginesize+boreratio+horsepower+carheight+
                   peakrpm+CompanyGroup_Medium_Range+CompanyGroup_TopNotch_Cars+enginetype_l_rotor+
                   enginetype_ohc+enginetype_ohcf+ enginetype_ohcv+enginelocation_rear+fueltype_gas+aspiration_turbo+
                   carbody_hardtop+carbody_hatchback+symboling_1+symboling_2+symboling_3+symboling_negative+carbody_sedan+
                   carbody_wagon+cylindernumber_fivesix+cylindernumber_eighttwelve+drivewheel_fwd+drivewheel_rwd, data=train_data,nbest=1)
graphics.off()
par("mar")
par(mar=c(1,1,1,1))
plot(leaps,scale="r2")

res.sum= summary(leaps)
res.sum
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)


#plot statistic by subset size, based on r2
library(car)
library(leaps)
par("mar")
par(mar=c(1,1,1,1))
subsets(leaps, statistic="rsq")

#seeing all possible steps
library(olsrr)
fit3=lm(price_trans ~  wheelbase+carlength+carwidth+curbweight+enginesize+boreratio+horsepower+carheight+
          peakrpm+CompanyGroup_Medium_Range+CompanyGroup_TopNotch_Cars+enginetype_l_rotor+
          enginetype_ohc+enginetype_ohcf+ enginetype_ohcv+enginelocation_rear+fueltype_gas+aspiration_turbo+
          carbody_hardtop+carbody_hatchback+symboling_1+symboling_2+symboling_3+symboling_negative+carbody_sedan+
          carbody_wagon+cylindernumber_fivesix+cylindernumber_eighttwelve+drivewheel_fwd+drivewheel_rwd, data=train_data)
test=ols_step_all_possible(fit3)
View(test)
plot(test)

#fiting the model
fit4= lm(price_trans ~ curbweight+horsepower+CompanyGroup_TopNotch_Cars+enginesize+CompanyGroup_Medium_Range+symboling_1+carwidth, data=train_data)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit4)

#make predictions for test data
pred4=fit4 %>% predict(test_data)

#calculating R-Square, RMSE and MAE
data.frame( R2 = R2(pred4, test_data$price_trans),
            RMSE = RMSE(pred4, test_data$price_trans),
            MAE = MAE(pred4, test_data$price_trans))

#We choose a model that has a lower rmse/mean(error rate)
RMSE4=RMSE(pred4, test_data$price_trans)/mean(test_data$price_trans)
RMSE4

vif(fit4) 

#fiting the model
fit5= lm(price_trans ~ poly(curbweight, 2,raw = TRUE) +horsepower+CompanyGroup_TopNotch_Cars+enginesize+CompanyGroup_Medium_Range+symboling_1+carwidth,data=train_data)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit5)

#make predictions for test data
pred5=fit5 %>% predict(test_data)


#calculating R-Square, RMSE and MAE
data.frame( R2 = R2(pred5, test_data$price_trans),
            RMSE = RMSE(pred5, test_data$price_trans),
            MAE = MAE(pred5, test_data$price_trans))

RMSE5=RMSE(pred4, test_data$price_trans)/mean(test_data$price_trans)
RMSE5

summary(fit5)

#kfold crossvalidation

#Define training control
library(caret)
set.seed(123) 
train_control= trainControl(method = "cv", number = 10)


#Train the model
fitcv= train(price_trans ~ poly(curbweight, 2,raw = TRUE) +horsepower+CompanyGroup_TopNotch_Cars+enginesize+CompanyGroup_Medium_Range+symboling_1+carwidth,data=train_data, method = "lm",
             trControl = train_control)

#Summarize the results
print(fitcv)









