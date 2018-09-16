library(pacman)

# This loads and installs the packages you need at once
pacman::p_load(tm,SnowballC,foreign,plyr, dplyr,
               twitteR,slam,foreign,
               caret,ranger,rpart,rpart.plot, xgboost,
               e1071,dummies,class,randomForest)


data = "~/Dropbox/Teaching/Classes-2018-2019-UGA/Big Data/Problem-Set-1/cox-violent-parsed.csv"
data = read.csv(data)

duplicates = duplicated(data$id)

data = data[duplicates == FALSE,]

race = dummy(factor(race))

age = dummy(factor(age_cat))

# Use on the first 1000 observations for computational reasons

#data = data[1:5000,]

attach(data)

# Violent predict
#violent.score.predict = ifelse(v_score_text == "High",1,0)

violent.predict = ifelse(is_recid > 0, 1,0)

feature_matrix = data.frame(age, decile_score,priors_count,sex )

fulldata = data.frame(violent.predict,age,decile_score,priors_count,sex )
train=sample(1:dim(data)[1],
             dim(data)[1]*0.7)
trainX = feature_matrix[train,]
testX = feature_matrix[-train,]
trainY = violent.predict[train]
testY = violent.predict[-train]

fulldatatrain = fulldata[train,]
fulldatatest = fulldata[-train,]


set.seed(7)

# Training without race

rf <-randomForest(as.factor(violent.predict)~.,data = fulldatatrain)
# c) advanced algorithms

# Make predictions using the test data
predictions = predict(rf,fulldatatest)

# Compare the predicted and actual values using the test labels
confusionMatrix(predictions, testY, positive = "1")



# Training with race


feature_matrix = data.frame(race,age,decile_score,priors_count,sex )

fulldata = data.frame(violent.predict,race,age,decile_score,priors_count,sex )
train=sample(1:dim(data)[1],
             dim(data)[1]*0.7)
trainX = feature_matrix[train,]
testX = feature_matrix[-train,]
trainY = violent.predict[train]
testY = violent.predict[-train]

fulldatatrain = fulldata[train,]
fulldatatest = fulldata[-train,]


set.seed(7)


# Training with race

rf <-randomForest(as.factor(violent.predict)~.,data = fulldatatrain)
# c) advanced algorithms

# Make predictions using the test data
predictions = predict(rf,fulldatatest)

# Compare the predicted and actual values using the test labels
confusionMatrix(predictions, testY, positive = "1")


