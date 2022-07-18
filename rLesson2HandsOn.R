library("caret")
library("magrittr")
library("dplyr")
library("tidyr")
library("lmtest")
library("popbio")
library("e1071")

###Testing Assumptions###

##Sample Size
###Basic Linear Model
#Create the Logistic Model
mylogit <- glm(Gold ~ Antimony, data=minerals, family="binomial")
#Predicting if Gold was found in the different areas
probabilities <- predict(mylogit, type = "response")
minerals$Predicted <- ifelse(probabilities > .5, "pos", "neg" )
#Add predictions into a new columna and recode
minerals$PredictedR <- NA
minerals$PredictedR[minerals$Predicted == "pos"] <- 1
minerals$PredictedR[minerals$Predicted == "neg"] <- 0
#Change variables into factors for confusion matrix
minerals$PredictedR <- as.factor(minerals$PredictedR)
minerals$Gold <- as.factor(minerals$Gold)
###Confusion Matric
conf_mat <- caret::confusionMatrix(minerals$PredictedR, minerals$Gold)
conf_mat
### With an accuracy of ~ 84%:
# 34 We predicted to not have gold was correct
# 8 that we predicted to have gold that did not have any
# 2 we predicted to not have gold that did have gold
# 20 we predicted to have gold that did have gold
## We have 1 value < 5 which means: We have violated the sample size need to perform a proper binary logistic regression ##


##Logit Linearity
minerals1 <- minerals %>% 
  dplyr::select_if(is.numeric)

predictors <- colnames(minerals1)

minerals1 <- minerals1 %>%
  mutate(logit=log(probabilities/(1-probabilities))) %>%
  gather(key= "predictors", value="predictor.value", -logit)  
  
ggplot(minerals1, aes(logit, predictor.value))+
  geom_point(size=.5, alpha=.5)+
  geom_smooth(method= "loess")+
  theme_bw()+
  facet_wrap(~predictors, scales="free_y")
##We have met the assumption for linearity##


##Independent Errors
plot(mylogit$residuals)
dwtest(mylogit, alternative="two.sided")
##We have met the assumption for Independence of Errors


##Screening for Outliers
infl <- influence.measures(mylogit)
summary(infl)


###We have passed all asumptions but sample size. We will however move forward for testing purposes###



## Run the Logistic Regression ##
summary(mylogit)

##Looking at our antimony coefficient p-value (p < 0.001) we see that it is a significant predictor of wheter Gold will be found or not
##Our regression is inherently faulty due to sample size since every step in antimony we see a %176 increas ein the likelihood of finding Gold.
## I would recommend collecting more data to be able to actually predict the rate of finding Gold

  
  
  


