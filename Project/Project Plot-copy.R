## data cleaning

titanic <- read_csv("titanic.csv")

# extract the title
t <- titanic %>% 
  extract(Name, into = "Title", reg =' ([A-Za-z]+)\\.')

# combine the high-ranking title
highrank <-   filter(t, !Title %in% c("Mr", "Mrs", "Miss", "Ms")) %>% 
  select(-Title) %>% 
  cbind(data.frame(Title = c(rep("HighRank",66))))

clean <- filter(t, Title %in% c("Mr", "Mrs", "Miss", "Ms")) %>% 
  rbind(highrank) %>% 
  arrange(PassengerId) %>% 
  select(-Cabin) %>% 
  na.omit() %>% 
  select(-Ticket, -PassengerId) %>% 
  mutate(Pclass = as.factor(Pclass),
         Survived = as.logical(Survived))

clean[356, "Title"] = "Miss"

################### Data ###################
clean$Survived <- as.character(clean$Survived)

# Plot all variable, but there are too many categorical variable.
library('ggplot2')
plot(clean)

# Histogram of variables
clean2 <- clean[,-c(2,3,4,9)]
chart.Correlation(clean2, histogram=TRUE)

# Age & Sex histogram
ggplot(data=clean, mapping =aes (x=Age, fill=Sex)) +
  geom_histogram(binwidth=5)+facet_wrap(~Survived) 

# Fare & Sex histogram
ggplot(data=clean, mapping =aes (x=Fare, fill=Sex)) +
  geom_histogram(binwidth=20)+facet_wrap(~Survived) 

# SibSp histogram
ggplot(data=clean, mapping =aes (x=SibSp, fill=Survived)) +
  geom_histogram(binwidth=1)

# Title barplot
ggplot(clean, aes(x=Title, fill=Survived)) +geom_bar()

# Embarked barplot
ggplot(clean, aes(x=Embarked, fill=Survived)) +geom_bar(position='dodge')

# Parch barplot
ggplot(clean, aes(x=Parch, fill=Survived)) +geom_bar()

################### Model 5 ###################

plot(model5$fitted.values, model5$residuals) 
plot(train$Age, model5$residuals) 

identify(train$Age , model5$residuals) 

plot(train$Fare , model5$residuals) 
identify(train$Fare , model5$residuals) 

plot(train$SibSp , model5$residuals) 
plot(train$Parch , model5$residuals) 

# QQplot (useless for logistic regression)
qqnorm(model5$residuals)
qqline(model5$residuals)

# Residual Histogram
hist(model5$residuals)

# Residual plot 
# We discovered 3 outlier which are no. obs 202, 266, 323
plot(model5$fitted.values, model5$residuals) 
identify(model5$fitted.values, model5$residuals) 
train[c(202,266,323),]

# obs 202 passengers ID = 298
# Name: Allison, Miss. Helen Loraine
# Age: 2
# Fare: 152
# Survived: 0
p_298 <- titanic %>% filter (PassengerId == 298)
p_298

# obs 266 passengers ID = 401
p_401 <- titanic %>% filter (PassengerId == 401)
View(p_401)

# obs 323 passengers ID = 499
p_499 <- titanic %>% filter (PassengerId == 499)
View(p_298)



