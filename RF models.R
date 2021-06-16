

#####################################################################

##0.84model ## Probably highest private

###############

#library packages
library(tidyverse)
library(tidymodels)
library(randomForest)

# Read data
fires_tr_full <- read_csv("student_training_release.csv") %>%
  mutate(cause = factor(cause))
fires_to_predict <- read_csv("student_predict_x_release.csv")

# Split into training test sets for model building
set.seed(3000)
split <- initial_split(fires_tr_full, 3/4, strata = "cause")
fires_tr <- training(split)
fires_ts <- testing(split)




#assign the values to the training dataset, then we use the fires_tr as our training to construct our model 

training<-fires_tr #!!!!!!!!! used as no log model

#Here, by observing the influence and in general rule. The sun exposure and other factors of fire causes are highly correlated with the holiday, as we know, 
#Australian summer times are generally have a duration of 5 months which are Oct, Nov, Dec, Jan, Feb,Mar

training <- filter(training, month %in% c(10,11,12,1,2,3))


# Fit a Random forest model with classification, we set default mtry=3 here for the reason they gave us best kaggle performance 

fires_rf <- rand_forest() %>%
  set_engine("randomForest",
             importance=TRUE, proximity=TRUE) %>%
  set_mode("classification") %>%
  fit(cause~dist_camp+dist_road+dist_cfa+lon+ase180+lat+aws_m24+aws_m12+arf360+ase90+month, data=training)
# we have already tested the variables by submit to kaggle multiple times (more than 20 times) and use the `lime` package to filter the useful modeling
#Then we finally decide such variables 



fires_rf # gain OOB 

summary(fires_rf)# Taking a reference of the accuracy of the model itself 




# Make predictions
fires_to_predict_p <- fires_to_predict %>%# load the prediction data 
  mutate(cause_p = predict(fires_rf, fires_to_predict)$.pred_class) %>% # we generate the cause  as our selection criteria 
  select(id, cause_p) %>%
  rename(Id = id, Category = cause_p)
write_csv(fires_to_predict_p, file="predictions5_2021-05-2910.csv")# Save the file 



# This is a way to check the global importance of variables, 
options(digit=2)
fires_rf$fit$importance[,1:4]
fires_rf$fit$importance[,5:6]

############################################################################################################################################################################

###############################################################ANOTHER MODEL ############################################################################################


############################################################################################################################################################################


#####################################################################

##0.85model 

###############
#library packages
library(tidyverse)
library(tidymodels)
library(randomForest)

# Read data
fires_tr_full <- read_csv("student_training_release.csv") %>%
  mutate(cause = factor(cause))
fires_to_predict <- read_csv("student_predict_x_release.csv")

# Split into training test sets for model building
set.seed(7000)
split <- initial_split(fires_tr_full, 3/4, strata = "cause")
fires_tr <- training(split)
fires_ts <- testing(split)






#assign the values to trianing dataset, then we use the fires_tr as our training to construct our model 

training<-fires_tr #!!!!!!!!! used as no log model

#Here, by observing the influence and in general rule. The sun explosure and other factors of fire causes are highly correlated with the holiday, as we know, 
#Australian summer times are generally have a duration of 5 months which are Oct, Nov, Dec, Jan, Feb,Mar

training <- filter(training, month %in% c(10,11,12,1,2,3))




training <- select(training, -FOREST, -year, -FOR_TYPE,-FOR_CODE, -FOR_CAT, -wod, -FIRE_START)

fires_ts<-select(fires_ts,-FOREST, -year, -FOR_TYPE,,-FOR_CODE, -FOR_CAT, -wod, -FIRE_START)

training<- na.omit(training)



library(outliers)
#Here we use the outliers package to fill the most extreme value with the column median, in order to minimise the influence of the outlier to data without 
# doing any modifications to data itself. 


training$lon<-rm.outlier(training$lon, fill = TRUE, median =TRUE, opposite = FALSE)

training$lat<-rm.outlier(training$lat, fill = TRUE, median =TRUE, opposite = FALSE)

training$dist_cfa<-rm.outlier(training$dist_cfa, fill = TRUE, median =TRUE, opposite = FALSE)
training$dist_camp<-rm.outlier(training$dist_camp, fill = TRUE, median =TRUE, opposite = FALSE)
training$dist_road<-rm.outlier(training$dist_road, fill =TRUE, median =TRUE, opposite = FALSE)



training$aws_m24<-rm.outlier(training$aws_m24, fill = TRUE, median =TRUE, opposite = FALSE)
training$aws_m12<-rm.outlier(training$aws_m12, fill = TRUE, median =TRUE, opposite = FALSE)

training$ase180<-rm.outlier(training$ase180, fill = TRUE, median =TRUE, opposite = FALSE)

training$ase90<-rm.outlier(training$ase90, fill = TRUE, median =TRUE, opposite = FALSE)


training$arf360<-rm.outlier(training$arf360, fill = TRUE, median =TRUE, opposite = FALSE)





# Fit a Random forest model with classfication, we set default mtry=3 here for the reason they gave us best kaggle performance 

fires_rf <- rand_forest() %>%
  set_engine("randomForest",
             importance=TRUE, proximity=TRUE) %>%
  set_mode("classification") %>%
  fit(cause~dist_camp+dist_road+dist_cfa+lon+ase180+lat+aws_m24+aws_m12+arf360+ase90+month, data=training)
# we have already tested the variables by submit to kaggle multiple times (more than 20 times) and use the `lime` package to filter the useful modeling
#Then we finally decide such variables 



fires_rf# gain OOB

summary(fires_rf)
# Taking a reference of the accuracy of the model itself 





library(outliers)

#Here we use the outliers package to fill the most extreme value with the column median, to minimize the influence of the outliers on data without 
#doing any modifications to the data itself

fires_to_predict$lon<-rm.outlier(fires_to_predict$lon, fill = TRUE, median =TRUE, opposite = FALSE)

fires_to_predict$lat<-rm.outlier(fires_to_predict$lat, fill = TRUE, median =TRUE, opposite = FALSE)

fires_to_predict$dist_cfa<-rm.outlier(fires_to_predict$dist_cfa, fill = TRUE, median =TRUE, opposite = FALSE)
fires_to_predict$dist_camp<-rm.outlier(fires_to_predict$dist_camp, fill = TRUE, median =TRUE, opposite = FALSE)
fires_to_predict$dist_road<-rm.outlier(fires_to_predict$dist_road, fill = TRUE, median =TRUE, opposite = FALSE)



fires_to_predict$aws_m24<-rm.outlier(fires_to_predict$aws_m24, fill = TRUE, median =TRUE, opposite = FALSE)
fires_to_predict$aws_m12<-rm.outlier(fires_to_predict$aws_m12, fill = TRUE, median =TRUE, opposite = FALSE)

fires_to_predict$ase180<-rm.outlier(fires_to_predict$ase180, fill = TRUE, median =TRUE, opposite = FALSE)

fires_to_predict$ase90<-rm.outlier(fires_to_predict$ase90, fill = TRUE, median =TRUE, opposite = FALSE)


fires_to_predict$arf360<-rm.outlier(fires_to_predict$arf360, fill = TRUE, median =TRUE, opposite = FALSE)



# Make predictions
fires_to_predict_p <- fires_to_predict %>%# load the prediction data 
  mutate(cause_p = predict(fires_rf, fires_to_predict)$.pred_class) %>% # we generate the cause  as our selection criteria 
  select(id, cause_p) %>%
  rename(Id = id, Category = cause_p)
write_csv(fires_to_predict_p, file="predictions5_2021-05-2910.csv")# Save the file 


# This is a way to check the global importance of variables, 
options(digit=2)
fires_rf$fit$importance[,1:4]
fires_rf$fit$importance[,5:6]






