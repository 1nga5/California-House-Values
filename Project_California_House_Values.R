# PROJECT: Prediction of House Values in California
#
# ---------------------------------------------------------------

# ---------------------------------------------------------------

# Step 1 - Data Access

library(data.table)

#b. import data frame
#Set working directory!
housing <- fread("housing.csv", #fread() for faster reading of the .csv file
                         sep = ",", 
                         header = TRUE)

#c. cast ocean_proximity to factor variable
housing$ocean_proximity <- as.factor(housing$ocean_proximity)

#display resulting levels
levels(housing$ocean_proximity)
#[1] "<1H OCEAN"  "INLAND"     "ISLAND"     "NEAR BAY"   "NEAR OCEAN"

# ---------------------------------------------------------------

# ---------------------------------------------------------------

# Step 2 - EDA & Data Visualization

#a.
head(housing)
tail(housing)
#---> income and house value are greater NEAR BAY than INLAND
#---> age NEAR BAY double than INLAND

#b.
summary(housing)
#---> total_rooms has outliers (Max.: 39320)
#---> total_bedrooms has outliers (Max.: 6445) and many NA's (207)
#---> same for population, households, median_income
#---> only 5 obs.for ISLAND (small sample)


#c. correlation analysis
cor_table <- cor(housing[,c(1:9)], use = "complete.obs") #subsetting all numeric variables
cor_table
pairs(housing[,c(1:9)])
library(apaTables)
apa.cor.table(housing[,c(1:9)], filename = "cor.doc", show.sig.stars = TRUE)

#---> only for numeric variables (except ocean proximity)
#---> no dependence between longitude and latitude and the other variables
#---> slight negative dependence between median_age and the others except longitude and latitude
#---> strong dependence between total_rooms, total_bedrooms, population and household
#---> medium dependence between median_income and median_house_value
 

#d. histograms
#subset for numeric variables in housing data frame
housing_numeric <- subset(housing, select = c(which(sapply(housing, is.numeric))))

#create matrix for overview
library(Hmisc)
hist.data.frame(housing_numeric[,1:9]) 

#create for each column a histogram
for (col in names(housing_numeric)) {
  #create histogram
  hist(housing_numeric[[col]], 
       main = paste("Histogram of", col), 
       xlab = col)
    
  #save histogram as a png file
  png(paste0(col, "_histogram.png"))
  hist(housing_numeric[[col]], 
       main = paste("Histogram of", col), 
       xlab = col)
  dev.off() #close file
  
  #request an enter to continue
  readline("Press enter to continue to the next histogram!")
}

#---> total_rooms, total_bedrooms, population, households have many outliers: right-skewed probability distribution
#---> median_income has little outliers
#---> median_house_value has a different unit display


#e. boxplots
#create for each column a boxplot
for (col in names(housing_numeric)) {
  #create boxplot
  boxplot(housing_numeric[[col]],
          main = paste("Boxplot of", col), 
          xlab = col)
  
  #save boxplot as a png file
  png(paste0(col, "_boxplot.png"))
  boxplot(housing[[col]], 
          main = paste("Boxplot of", col), 
          xlab = col)
  dev.off() #close file
  
  #request an enter to continue
  readline("Press enter to continue to the next boxplot!")
}

#---> total_rooms, total_bedrooms, population, households have many outliers
#---> median_income has little outliers


#f. boxplots with respect to ocean_proximity
#create for the three columns a boxplot by ocean proximity
for (col in names(housing)) {
  if (col %in% c("housing_median_age", "median_income", "median_house_value")) {
  
    #create boxplot
    boxplot(housing[[col]] ~ ocean_proximity, 
            data = housing, 
            main = paste("Boxplot of", col), 
            xlab = "ocean proximity", 
            ylab = col,
            varwidth = TRUE)
    
    #save boxplot as a png file
    png(paste0(col, "_boxplot_by_ocean_proximity.png"))
    boxplot(housing[[col]] ~ ocean_proximity, 
            data = housing, 
            main = paste("Boxplot of", col), 
            xlab = "ocean_proximity", 
            ylab = col,
            varwidth = TRUE)
    dev.off() #close file
    
    #request an enter to continue
    readline("Press enter to continue to the next boxplot by ocean proximity!")
  }
}

#---> housing_median_age: ISLAND has the oldest houses followed by NEAR BAY
#---> median_income: ISLAND has the lowest median income and less variation due to the low number of observations, the others are similar
#---> median_house_value: ISLAND has the highest, followed by NEAR BAY; the lowest house value has INLAND
#---> ocean proximity is probably a good predictor for the house value

# ---------------------------------------------------------------

# ---------------------------------------------------------------

# Step 3 - Data Transformation (Data Munging)

#a. imputation of missing values in total_bedrooms using median(), stored in new data frame housing_1
library(e1071)
housing_1 <- data.frame(impute(housing[,1:9], what='median'),housing[,10])


#b. splitting of ocean_proximity into binary numbers 1s and 0s and removing it
#save the 5 levels from ocean_proximity factor variable
levels_ocean_proximity <- levels(housing_1$ocean_proximity)

#define a user defined function to build a logical map of level variable based on passed value
binary_levels_ocean_proximity <- function(c) {return(housing_1$ocean_proximity == c)}

#sapply to loop through all
newVars <- sapply(levels_ocean_proximity, binary_levels_ocean_proximity)
newVars[50:55,]

#<1H OCEAN INLAND ISLAND NEAR BAY NEAR OCEAN
#[1,]     FALSE  FALSE  FALSE     TRUE      FALSE
#[2,]     FALSE  FALSE  FALSE     TRUE      FALSE
#[3,]     FALSE  FALSE  FALSE     TRUE      FALSE
#[4,]     FALSE  FALSE  FALSE     TRUE      FALSE
#[5,]     FALSE  FALSE  FALSE     TRUE      FALSE
#[6,]     FALSE  FALSE  FALSE     TRUE      FALSE

#combine new binary categorical variables with original housing_repaired data frame
bin_housing <- housing_1       #copy of data set

bin_housing$'<1H_OCEAN' <- newVars[,1]       #include <1H OCEAN binaries
bin_housing$INLAND <- newVars[,2]   #include INLAND binaries
bin_housing$ISLAND <- newVars[,3]    #include ISLAND binaries
bin_housing$NEAR_BAY <- newVars[,4]    #include NEAR BAY binaries
bin_housing$NEAR_OCEAN <- newVars[,5]    #include NEAR OCEAN binaries

head(bin_housing)

#remove ocean proximity variable
housing_2 <- bin_housing[,-10]

head(housing_2)


#c. create two new variables: mean_number_bedrooms and mean_number_rooms
housing_3 <- housing_2 #make copy of data frame

housing_3$mean_number_bedrooms <- housing_3$total_bedrooms / housing_3$households
housing_3$mean_number_rooms <- housing_3$total_rooms / housing_3$households

#remove total_bedrooms and total_rooms
housing_3 <- housing_3[,-4:-5]

head(housing_3)


#d. feature scaling
#perform z-score standardization and store in new data frame
housing_4 <- data.frame(scale(housing_3[, -c(7:12)]), housing_3[, c(7:12)])
colnames(housing_4)[10] <- "<1H_OCEAN" #rename X.1H Ocean

head(housing_4)


#e. create cleaned data set
cleaned_housing <- housing_4

#organizing the location of variables
library(dplyr)
column_order <- c("NEAR_BAY", 
                  "<1H_OCEAN", 
                  "INLAND", 
                  "NEAR_OCEAN", 
                  "ISLAND", 
                  "longitude", 
                  "latitude", 
                  "housing_median_age", 
                  "population", 
                  "households", 
                  "median_income", 
                  "mean_number_bedrooms", #mean bedrooms
                  "mean_number_rooms",  #mean rooms
                  "median_house_value")

cleaned_housing <- cleaned_housing[ ,column_order]

head(cleaned_housing)

# ---------------------------------------------------------------

# ---------------------------------------------------------------

# Step 4 - Create Training and Test Sets

#a. create a random sample index
set.seed(1)

#b./c. split data set into training set and test set
n <- nrow(cleaned_housing)  #Number of observations = 20640
ntrain <- round(n*0.7)    #70% for training set
#set.seed(314)             #set seed for reproducible results
tindex <- sample(n, ntrain) #create an index

train <- cleaned_housing[tindex,]  #create training set
test <- cleaned_housing[-tindex,]  #create test set


#exploratory data analysis of training set
smoothScatter(train$median_house_value,train$median_income) #trend
smoothScatter(train$median_house_value,train$mean_number_rooms) #no trend, few outliers
smoothScatter(train$median_house_value,train$housing_median_age) #no trend

#or create a sample subset of the train data set to better plot (big data technique)
sampledSubsetTrain <- sample(1:ntrain, size = 500, replace = FALSE)
#plot sampledSubset
plot(train$median_house_value[sampledSubsetTrain], train$median_income[sampledSubsetTrain]) #trend
plot(train$median_house_value[sampledSubsetTrain], train$mean_number_rooms[sampledSubsetTrain]) #no trend, outliers
plot(train$median_house_value[sampledSubsetTrain], train$housing_median_age[sampledSubsetTrain]) #no trend

# ---------------------------------------------------------------

# ---------------------------------------------------------------

# Step 5 - Supervised Machine Learning - Regression

library(randomForest)

#separate train in train_x and train_y
train_x <- train[, !(names(train) %in% "median_house_value")] #data frame
train_y <- train$median_house_value #numeric vector

#call randomForest algorithm
rf = randomForest(x = train_x, 
                  y = train_y ,
                  ntree = 500, 
                  importance=TRUE) #always include importance arg to use varImpPlot(rf)

names(rf)
#[1] "call"            "type"            "predicted"       "mse"            
#[5] "rsq"             "oob.times"       "importance"      "importanceSD"   
#[9] "localImportance" "proximity"       "ntree"           "mtry"           
#[13] "forest"          "coefs"           "y"               "test"           
#[17] "inbag"

# ---------------------------------------------------------------

# ---------------------------------------------------------------

# Step 6 - Evaluating Model Performance

#a. calculate rmse for trained rf set
rmse_train <- sqrt(tail(rf$mse, n = 1))
rmse_train
#[1] 49566.27


#b. separate test in test_x and test_y
test_x <- test[, !(names(test) %in% "median_house_value")] #data frame
test_y <- test$median_house_value #numeric vector

#predict median_house_values_using test_x
predicted_median_house_value <- predict(rf, newdata = test_x, type = "class") #numeric vector

#c. calculate rmse for test set
#define rmse function
rmse <- function(y_hat, y)
{
  return(sqrt(mean((y_hat-y)^2)))
}

#calculate rmse
rmse_test <- rmse(predicted_median_house_value, 
                  test_y)
rmse_test
# [1] 48700.28


#d. RMSE for training and test set are close together which is good
#no overfitting and a suitable model for predictions


#e. variable importance plot
varImpPlot(rf)
#housing_median_age and median_income are the most important variables


# ---------------------------------------------------------------

# ---------------------------------------------------------------

#retraining rf model with median_income and housing_median_age as the only feature variables

#separate train in train_x and train_y
train_x <- train[, c("median_income", "housing_median_age")] #data frame
train_y <- train$median_house_value #numeric vector

#call randomForest algorithm
rf = randomForest(x = train_x, 
                  y = train_y ,
                  ntree = 500, 
                  importance=TRUE) #always include importance arg to use varImpPlot(rf)

#a. calculate rmse for trained rf set
rmse_train <- sqrt(tail(rf$mse, n = 1))
rmse_train
#[1] 80270.55


#b. separate test in test_x and test_y
test_x <- test[, c("median_income", "housing_median_age")] #data frame
test_y <- test$median_house_value #numeric vector

#predict median_house_values_using test_x
predicted_median_house_value <- predict(rf, newdata = test_x, type = "class") #numeric vector

#c. calculate RMSE for test set
rmse_test <- rmse(predicted_median_house_value, 
                  test_y)
rmse_test
# [1] 80489.76


#d. RMSE for training and test set are now closer together than before which is really good but the absolute value is higher
#no overfitting and a suitable model for predictions


#e. variable importance plot
varImpPlot(rf)
#median_income is the most important variable

# ---------------------------------------------------------------
