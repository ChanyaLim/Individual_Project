### package installation if require ###
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(xgboost))install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")

### package loading ###
library(tidyverse)
library(caret)
library(readr)
library(data.table)
library(dplyr)
library(randomForest)
library(e1071)
library(xgboost)
library(tinytex)

### import mushroom dataset from my github ###
url<- "https://raw.githubusercontent.com/ChanyaLim/Individual_Project/master/mushrooms.csv"
## assign column names ##
col_name <- c("class","cap_shape","cap_surface","cap_color",
              "bruises","odor","gill_attachment","gill_spacing",
              "gill_size","gill_color","stalk_shape","stalk_root",
              "stalk_surface_above_ring","stalk_surface_below_ring",
              "stalk_color_above_ring","stalk_color_below_ring",
              "veil_type","veil_color","ring_number","ring_type","spore_print_color",
              "population","habitat")
## import mushrooms.csv file ###
mushroom <- fread(url,col.names=col_name)
## change character data to factor ##
mushroom <- mushroom %>% mutate_if(is.character, as.factor)
## overview ##
# definition of each variable level from kaggle #
def <- c("e=edible, p=poisonous",
         "b=bell, c=conical, x=convex, f=flat, k=knobbed, s=sunken",
         "f=fibrous, g=grooves, y=scaly, s=smooth",
         "n=brown, b=buff, c=cinnamon, g=gray, r=green, p=pink, u=purple, e=red, w=white, y=yellow",
         "t=bruises, f=no",
         "a=almond, l=anise, c=creosote, y=fishy, f=foul, m=musty, n=none, p=pungent, s=spicy",
         "a=attached, f=free",
         "c=close, w=crowded",
         "b=broad, n=narrow",
         "k=blank, n=brown, b=buff, h=chocolate, g=gray, r=green, o=orange, p=pink, u=purple, e=red, w=white, y=yellow",
         "e=enlarging, t=tapering",
         "b=bulbous, c=club, u=cup, e=equal, z=rhizomorphs, r=rooted, ?=missing",
         "f=fibrous, y=scaly, k=silky, s=smooth",
         "f=fibrous, y=scaly, k=silky, s=smooth",
         "n=brown, b=buff, c=cinnamon, g=gray, o=orange, p=pink, e=red, w=white, y=yellow",
         "n=brown, b=buff, c=cinnamon, g=gray, o=orange, p=pink, e=red, w=white, y=yellow",
         "p=partial, u=universal",
         "n=brown, o=orange, w=white, y=yellow",
         "n=none, o=one, t=two",
         "c=cobwebby, e=evanescent, f=flaring, l=large, n=none, p=pendent, s=sheathing, z=zone",
         "k=black, n=brown, b=buff, h=chocolate, r=green, o=orange, u=purple, w=white, y=yellow ",
         "a=abundant, c=clustered, n=numerous, s=scattered, v=several, y=solitary",
         "g=grasses, l=leaves, m=meadows, paths=p, u=urban, w=waste d=woods")

### Data cleaning ###
str(mushroom)
## NA checking ##
any(is.na(mushroom))
##check row duplication : There is no duplicated row##
sum(duplicated(mushroom))
## check number of level : 
level <- sapply(mushroom,levels)
Number_level <- sapply(mushroom,uniqueN)
dataoverview <- as.data.frame(Number_level) %>% mutate(Level=level)
dataoverview
## found veil_type have 1 level so I will cut this variable off ##
dataoverview %>% select(Number_level)
mushroom <- mushroom %>% select(-"veil_type")
## found stalk_root have "?" which is missing value so I will cut this variable off ##
mushroom <- mushroom %>% select(-"stalk_root")

### Exploration Data Analysis(EDA) and visualization ###
## class : target feature prediction ##
mushroom %>% group_by(class) %>% summarize(count=n()) %>%
  mutate(Percent=count/sum(count)) %>% 
  ggplot(aes(class,Percent)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  geom_text(aes(label=round(Percent*100,digit=2)),vjust=-0.25)

## cap_shape : imbalance(<1%) ##
## big portion are f and x : not significant ##
mushroom %>% group_by(cap_shape) %>% summarize(count=n()) %>%
  mutate(Percent=count/sum(count)) %>% 
  ggplot(aes(cap_shape,Percent)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

mushroom %>% ggplot(aes(cap_shape,fill=class))+
  geom_bar(position = "dodge")

## cap_surface : imbalance(<1%) ##
## big portion are f,s, and y : significant ##
mushroom %>% group_by(cap_surface) %>% summarize(count=n()) %>%
  mutate(Percent=count/sum(count)) %>% 
  ggplot(aes(cap_surface,Percent)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

mushroom %>% ggplot(aes(cap_surface,fill=class))+
  geom_bar(position = "dodge")

## cap_color : imbalance (<1%) ##
## big portion are e,g,n,w,y : significant.
mushroom %>% group_by(cap_color) %>% summarize(count=n()) %>%
  mutate(Percent=count/sum(count)) %>% 
  ggplot(aes(cap_color,Percent)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

mushroom %>% ggplot(aes(cap_color,fill=class))+
  geom_bar(position = "dodge")

## bruises : >1% and significant ##
mushroom %>% group_by(bruises) %>% summarize(count=n()) %>%
  mutate(Percent=count/sum(count)) %>% 
  ggplot(aes(bruises,Percent)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

mushroom %>% ggplot(aes(bruises,fill=class))+
  geom_bar(position = "dodge")

## odor : imbalance (<1%) ##
## big portion are f, n : significant.
mushroom %>% group_by(odor) %>% summarize(count=n()) %>%
  mutate(Percent=count/sum(count)) %>% 
  ggplot(aes(odor,Percent)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

mushroom %>% ggplot(aes(odor,fill=class))+
  geom_bar(position = "dodge")

## gill_attachment : imbalance(<1%) ##
## big portion is f : not significant.
mushroom %>% group_by(gill_attachment) %>% summarize(count=n()) %>%
  mutate(Percent=count/sum(count)) %>% 
  ggplot(aes(gill_attachment,Percent)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

mushroom %>% ggplot(aes(gill_attachment,fill=class))+
  geom_bar(position = "dodge")

## gill_spacing : >1% and significant ##
mushroom %>% group_by(gill_spacing) %>% summarize(count=n()) %>%
  mutate(Percent=count/sum(count)) %>% 
  ggplot(aes(gill_spacing,Percent)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

mushroom %>% ggplot(aes(gill_spacing,fill=class))+
  geom_bar(position = "dodge")

## gill_size : >1% and significant ##
mushroom %>% group_by(gill_size) %>% summarize(count=n()) %>%
  mutate(Percent=count/sum(count)) %>% 
  ggplot(aes(gill_size,Percent)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

mushroom %>% ggplot(aes(gill_size,fill=class))+
  geom_bar(position = "dodge")

## gill_color : imbalance (<1%) ##
## big portion are b,g,h,n,p,w : significant
mushroom %>% group_by(gill_color) %>% summarize(count=n()) %>%
  mutate(Percent=count/sum(count)) %>% 
  ggplot(aes(gill_color,Percent)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

mushroom %>% ggplot(aes(gill_color,fill=class))+
  geom_bar(position = "dodge")

## stalk_shape : >1% and significant ##
mushroom %>% group_by(stalk_shape) %>% summarize(count=n()) %>%
  mutate(Percent=count/sum(count)) %>% 
  ggplot(aes(stalk_shape,Percent)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

mushroom %>% ggplot(aes(stalk_shape,fill=class))+
  geom_bar(position = "dodge")

## stalk_surface_above_ring : imbalance(<1%)##
## big portion are k and s : significant
mushroom %>% group_by(stalk_surface_above_ring) %>% summarize(count=n()) %>%
  mutate(Percent=count/sum(count)) %>% 
  ggplot(aes(stalk_surface_above_ring,Percent)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

mushroom %>% ggplot(aes(stalk_surface_above_ring,fill=class))+
  geom_bar(position = "dodge")

## stalk_surface_below_ring : same characteristic with stalk_surface_above_ring ##
mushroom %>% group_by(stalk_surface_below_ring) %>% summarize(count=n()) %>%
  mutate(Percent=count/sum(count)) %>% 
  ggplot(aes(stalk_surface_below_ring,Percent)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

mushroom %>% ggplot(aes(stalk_surface_below_ring,fill=class))+
  geom_bar(position = "dodge")

## stalk_color_above_ring : imbalance(<1%)##
## big portion are p, w : significant
mushroom %>% group_by(stalk_color_above_ring) %>% summarize(count=n()) %>%
  mutate(Percent=count/sum(count)) %>% 
  ggplot(aes(stalk_color_above_ring,Percent)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

mushroom %>% ggplot(aes(stalk_color_above_ring,fill=class))+
  geom_bar(position = "dodge")

## stalk_color_below_ring : same characteristic with stalk_color_above_ring  ##

mushroom %>% group_by(stalk_color_below_ring) %>% summarize(count=n()) %>%
  mutate(Percent=count/sum(count)) %>% 
  ggplot(aes(stalk_color_below_ring,Percent)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

mushroom %>% ggplot(aes(stalk_color_below_ring,fill=class))+
  geom_bar(position = "dodge")

## veil_color : imbalance(<1%)##
## big portion w : not significant
mushroom %>% group_by(veil_color) %>% summarize(count=n()) %>%
  mutate(Percent=count/sum(count)) %>% 
  ggplot(aes(veil_color,Percent)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

mushroom %>% ggplot(aes(veil_color,fill=class))+
  geom_bar(position = "dodge")

## ring_number : imbalance(<1%) ##
## big portion o : not significant
mushroom %>% group_by(ring_number) %>% summarize(count=n()) %>%
  mutate(Percent=count/sum(count)) %>% 
  ggplot(aes(ring_number,Percent)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

mushroom %>% ggplot(aes(ring_number,fill=class))+
  geom_bar(position = "dodge")

## ring_type : imbalance (<1%)##
## big portion are e,l,p : significant
mushroom %>% group_by(ring_type) %>% summarize(count=n()) %>%
  mutate(Percent=count/sum(count)) %>% 
  ggplot(aes(ring_type,Percent)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

mushroom %>% ggplot(aes(ring_type,fill=class))+
  geom_bar(position = "dodge")

## spore_print_color : imbalance(<1%) ##
## big portion are h,k,n and w : significant
mushroom %>% group_by(spore_print_color) %>% summarize(count=n()) %>%
  mutate(Percent=count/sum(count)) %>% 
  ggplot(aes(spore_print_color,Percent)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

mushroom %>% ggplot(aes(spore_print_color,fill=class))+
  geom_bar(position = "dodge")

## population : 1% and significant  ##
mushroom %>% group_by(population) %>% summarize(count=n()) %>%
  mutate(Percent=count/sum(count)) %>% 
  ggplot(aes(population,Percent)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

mushroom %>% ggplot(aes(population,fill=class))+
  geom_bar(position = "dodge")

## habitat : 1% and significant ##
mushroom %>% group_by(habitat) %>% summarize(count=n()) %>%
  mutate(Percent=count/sum(count)) %>% 
  ggplot(aes(habitat,Percent)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

mushroom %>% ggplot(aes(habitat,fill=class))+
  geom_bar(position = "dodge")

### Model
## Target to minimize false positive rate (FP/N) or Maximize specificity
## Separate train and test set
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(mushroom$class,times = 1,p=0.2,list=FALSE)
train_set <- mushroom[-test_index,]
test_set <- mushroom[test_index,]

## Glm
# Model 1 : First group variables (>1% and significant) 
#bruises + gill_spacing + gill_size + stalk_shape + population + habitat
set.seed(1, sample.kind = "Rounding")
train_set_glm1 <- train_set %>% select(class,bruises,gill_spacing,gill_size,stalk_shape,population,habitat)
glm1 <- train(class ~ ., method = "glm", data = train_set_glm1)
predict_glm1 <- predict(glm1,test_set)
confusionMatrix(predict_glm1,test_set$class)
glm1_acc <- confusionMatrix(predict_glm1,test_set$class)$overall["Accuracy"]
glm1_sen <- confusionMatrix(predict_glm1,test_set$class)$byClass["Sensitivity"]
glm1_speci <- confusionMatrix(predict_glm1,test_set$class)$byClass["Specificity"]

result <- data.frame(Method="GLM(6 features)",Accuracy=glm1_acc,Sensitivity=glm1_sen,Specificity=glm1_speci)
rownames(result) <- NULL
knitr::kable(result)

## Random forest ##
# Model 1 : First group variables (>1% and significant) 
#bruises + gill_spacing + gill_size + stalk_shape + population + habitat
set.seed(1, sample.kind = "Rounding")
train_set_rf1 <- train_set %>% select(class,bruises,gill_spacing,gill_size,stalk_shape,population,habitat)
rf1 <- train(class~.,
             method="rf",
             data = train_set_rf1,
             tuneGrid = data.frame(mtry = 2))
predict_rf1 <- predict(rf1,test_set)
confusionMatrix(predict_rf1,test_set$class)

rf1_acc <- confusionMatrix(predict_rf1,test_set$class)$overall["Accuracy"]
rf1_sen <- confusionMatrix(predict_rf1,test_set$class)$byClass["Sensitivity"]
rf1_speci <- confusionMatrix(predict_rf1,test_set$class)$byClass["Specificity"]

result <- rbind(result,data.frame(Method="Random Forest(6 features)",
                                  Accuracy=rf1_acc,Sensitivity=rf1_sen,Specificity=rf1_speci))
rownames(result) <- NULL
knitr::kable(result)

##Tuning mtry
set.seed(1, sample.kind = "Rounding")
rf1_cv <- train(class~.,
                method="rf",
                data = train_set_rf1,
                tuneGrid = data.frame(mtry = seq(1,3,1)))
plot(rf1_cv)
rf1_cv
predict_rf1_cv <- predict(rf1_cv,test_set)
confusionMatrix(predict_rf1_cv,test_set$class)

rf1_cv_acc <- confusionMatrix(predict_rf1_cv,test_set$class)$overall["Accuracy"] 
rf1_cv_sen <- confusionMatrix(predict_rf1_cv,test_set$class)$byClass["Sensitivity"]
rf1_cv_speci <- confusionMatrix(predict_rf1_cv,test_set$class)$byClass["Specificity"]

result <- rbind(result,data.frame(Method="Random Forest with mtry tuning(6 features)",
                                            Accuracy=rf1_cv_acc,Sensitivity=rf1_cv_sen,Specificity=rf1_cv_speci))
rownames(result) <- NULL
knitr::kable(result)

# Model 2 : Second group variable(including <1% and significant)
#bruises + gill_spacing + gill_size + stalk_shape + population + habitat +
#cap_surface + cap_color + odor + gill_color + stalk_surface_above_ring + stalk_surface_below_ring +
#stalk_color_above_ring + stalk_color_below_ring + ring_type + spore_print_color
set.seed(1, sample.kind = "Rounding")
train_set_rf2 <- train_set %>% select(class,bruises,gill_spacing,gill_size,stalk_shape,population,habitat,
                                       cap_surface,cap_color,odor,gill_color,stalk_surface_above_ring,stalk_surface_below_ring,
                                       stalk_color_above_ring,stalk_color_below_ring,ring_type,spore_print_color)
rf2_cv <- train(class~.,
             method="rf",
             data = train_set_rf2,
             tuneGrid = data.frame(mtry = seq(1,4,1)))
plot(rf2_cv)
rf2_cv
varImp(rf2_cv)

# Model 3 : adjust from model 2 with top 20 variable important
#bruises + gill_spacing + gill_size + stalk_shape + population + habitat +
#odor + stalk_surface_above_ring + stalk_surface_below_ring + stalk_color_below_ring
#ring_type + spore_print_color
# Cut odor due to odorp is not the big portion of data (<5%)
set.seed(1, sample.kind = "Rounding")
train_set_rf3 <- train_set %>% select(class,bruises,gill_spacing,gill_size,stalk_shape,population,habitat,
                                      stalk_surface_above_ring,stalk_surface_below_ring,stalk_color_below_ring,
                                      ring_type,spore_print_color)
rf3_cv <- train(class~.,
                method="rf",
                data = train_set_rf3,
                tuneGrid = data.frame(mtry = seq(1,4,1)))
plot(rf3_cv)
rf3_cv
predict_rf3_cv<- predict(rf3_cv,test_set)
confusionMatrix(predict_rf3_cv,test_set$class)
 
varImp(rf3_cv)
varImp(rf3_cv)$importance %>% arrange(desc(Overall)) %>% head(10)

rf3_cv_acc <- confusionMatrix(predict_rf3_cv,test_set$class)$overall["Accuracy"] 
rf3_cv_sen <- confusionMatrix(predict_rf3_cv,test_set$class)$byClass["Sensitivity"]
rf3_cv_speci <- confusionMatrix(predict_rf3_cv,test_set$class)$byClass["Specificity"]

result <- rbind(result,data.frame(Method="Random Forest with mtry tuning(11 features)",
                                  Accuracy=rf3_cv_acc,Sensitivity=rf3_cv_sen,Specificity=rf3_cv_speci))
rownames(result) <- NULL
knitr::kable(result)

## XGBoost
# Model 1 : First group variables (>1% and significant) 
#bruises + gill_spacing + gill_size + stalk_shape + population + habitat
set.seed(1, sample.kind = "Rounding")
train_set_xg1 <- train_set %>% select(class,bruises,gill_spacing,gill_size,stalk_shape,population,habitat)
xg1 <- train(class~.,
             method="xgbTree",
             data = train_set_xg1)
xg1             
predict_xg1 <- predict(xg1,test_set)
confusionMatrix(predict_xg1,test_set$class)

xg1_acc <- confusionMatrix(predict_xg1,test_set$class)$overall["Accuracy"] 
xg1_sen <- confusionMatrix(predict_xg1,test_set$class)$byClass["Sensitivity"]
xg1_speci <- confusionMatrix(predict_xg1,test_set$class)$byClass["Specificity"]

result <- rbind(result,data.frame(Method="XgBoost(5 features)",
                                  Accuracy=xg1_acc,Sensitivity=xg1_sen,Specificity=xg1_speci))
rownames(result) <- NULL
knitr::kable(result)

# Model 2 : Second group variable(including <1% and significant)
#bruises + gill_spacing + gill_size + stalk_shape + population + habitat +
#cap_surface+cap_color + odor + gill_color + stalk_surface_above_ring + stalk_surface_below_ring +
#stalk_color_above_ring + stalk_color_below_ring + ring_type + spore_print_color
set.seed(1, sample.kind = "Rounding")
train_set_xg2 <- train_set %>% select(class,bruises,gill_spacing,gill_size,stalk_shape,population,habitat,
                                      cap_surface,cap_color,odor,gill_color,stalk_surface_above_ring,stalk_surface_below_ring,
                                      stalk_color_above_ring,stalk_color_below_ring,ring_type,spore_print_color)
xg2 <- train(class~.,
             method="xgbTree",
             data = train_set_xg2)
xg2
plot(xg2)
varImp(xg2)

# Model 3 : adjust from model 2 with top 20 variable important
#bruises + gill_spacing + gill_size + population + habitat+stalk_shape
#cap_color+odor + stalk_surface_above_ring + stalk_surface_below_ring +
#spore_print_color+ring_type
# Cut odor due to odorp and odorc are not the big portion of data (<5%)
# Cut spore print color due to spore_print_coloru
# Cut population due to populationc
# Cut habitat due to habitatm
# Cut stalk surface below ring due to stalk_surface_below_ringy
set.seed(1, sample.kind = "Rounding")
train_set_xg3 <- train_set %>% select(class,bruises,gill_spacing,gill_size,stalk_shape,
                                      cap_color,stalk_surface_above_ring,ring_type)
xg3 <- train(class~.,
             method="xgbTree",
             data = train_set_xg3)
xg3
plot(xg3)
predict_xg3 <- predict(xg3,test_set)
confusionMatrix(predict_xg3,test_set$class)  
varImp(xg3)
varImp(xg3)$importance %>% arrange(desc(Overall)) %>% head(10)

xg3_acc <- confusionMatrix(predict_xg3,test_set$class)$overall["Accuracy"] 
xg3_sen <- confusionMatrix(predict_xg3,test_set$class)$byClass["Sensitivity"]
xg3_speci <- confusionMatrix(predict_xg3,test_set$class)$byClass["Specificity"]

result <- rbind(result,data.frame(Method="XgBoost(7 features)",
                                  Accuracy=xg3_acc,Sensitivity=xg3_sen,Specificity=xg3_speci))
rownames(result) <- NULL
knitr::kable(result)

################################### END #################################################