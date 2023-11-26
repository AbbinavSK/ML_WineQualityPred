## Classification of wine
library(ggcorrplot)
library(mosaic)
library(car)
library(MASS)

# Loading winequality-red.csv and winequality-white.csv
red_data = read.csv2("data\\winequality-red.csv")

# Convert datatype of all variables as numeric
red_data <- data.frame(lapply(lapply(red_data, as.character), as.numeric))

# Standardizing the input variables to mean=0, sd=1
data <- red_data %>% mutate_at(c('fixed_acidity', 'volatile_acidity', 'citric_acid', 'residual_sugar', 'chlorides', 'free_sulfur_dioxide', 'total_sulfur_dioxide', 'density', 'pH', 'sulphates', 'alcohol'), ~(scale(.) %>% as.vector))

head(data)
str(data) # 1 target variable and 11 independent variables

### Exploratory Data Analysis
# Correlation matrix of data 
ggcorrplot(cor(data))
ggcorrplot(cor(data, data$quality))

cor(data$fixed_acidity, data$density)

# barplot to determine frequency of quality variable
barplot(table(data$quality))

## Relation between target variable "Quality" and independent variables
# positive sloping relation
boxplot(fixed_acidity~quality,data=data
        ,col=2:8,pch=20)

boxplot(citric_acid~quality,data=data
        ,col=2:8,pch=20)

boxplot(residual_sugar~quality,data=data
        ,col=2:8,pch=20)

boxplot(sulphates~quality,data=data
        ,col=2:8,pch=20)

boxplot(alcohol~quality,data=data
        ,col=2:8,pch=20)

#negative sloping relation

boxplot(volatile_acidity~quality,data=data
        ,col=2:8,pch=20)

boxplot(chlorides~quality,data=data
        ,col=2:8,pch=20)

boxplot(free_sulfur_dioxide~quality,data=data
        ,col=2:8,pch=20)

boxplot(total_sulfur_dioxide~quality,data=data
        ,col=2:8,pch=20)

boxplot(density~quality,data=data
        ,col=2:8,pch=20)

boxplot(pH~quality,data=data
        ,col=2:8,pch=20)

## Relation between independent variables

plot(data$fixed_acidity, data$density, pch=20)
plot(data$alcohol, data$fixed_acidity, pch=20)
plot(data$fixed_acidity, data$pH, pch=20)

# Train - Test split

n=nrow(data)
m=floor(n*0.7)

set.seed(42)
id = sort(sample(1:n,m,replace = F))

train= data[id,]
test = data[-id,]

#-------------------------------------------------------------------------------
## Exp1: Regression and LDA with all input variables
# Regression

fit = lm(quality~.,data=train)

sum = summary(fit) #Summary of fit
coef = sum$coefficients

vif(fit)

test$pred1=predict(fit,newdata = test) #Test input data prediction
test$pred1 = round(test$pred1, digit=0)

confusion_mat = as.matrix(table(Actual_Values = test$quality, Predicted_Values = test$pred1)) 
confusion_mat

#Accuracy calculated using mean of correct predictions
accuracy = sum(test$pred1==test$quality)/nrow(test)
accuracy

#Root mean square error score
RMSE = sqrt(mean((test$pred1 - test$quality)^2))
RMSE

#Mean absolute deviation
MAD = mean(abs(test$pred1 - test$quality))
MAD

#Linear Discriminant analysis of Quality

fit2 = lda(quality~., data=train)
fit2

pred2=predict(fit2,newdata = test)
class(pred2)

data.frame(pred2)[1:5,]

confusion_mat = as.matrix(table(Actual_Values = test$quality, Predicted_Values = pred2$class)) 
confusion_mat

accuracy = sum(pred2$class==test$quality)/nrow(test)
accuracy

#-------------------------------------------------------------------------------
## Exp2: Regression and LDA, input data without alcohol variable
#Regression
fit3 = lm(quality~sulphates+pH+density+total_sulfur_dioxide+
            free_sulfur_dioxide+chlorides+residual_sugar+
            citric_acid+volatile_acidity+fixed_acidity,data=train)

sum3 = summary(fit3) #Summary of fit
coef3 = sum3$coefficients

vif(fit3)

test$pred3=predict(fit3,newdata = test) #Test input data prediction
test$pred3 = round(test$pred3, digit=0)

confusion_mat = as.matrix(table(Actual_Values = test$quality, Predicted_Values = test$pred3)) 
confusion_mat

#Accuracy calculated using mean of correct predictions
accuracy = sum(test$pred3==test$quality)/nrow(test)
accuracy

#Root mean square error score
RMSE = sqrt(mean((test$pred3 - test$quality)^2))
RMSE

#Mean absolute deviation
MAD = mean(abs(test$pred3 - test$quality))
MAD

#Linear Discriminant Analysis
fit4 = lda(quality~sulphates+pH+density+total_sulfur_dioxide+
             free_sulfur_dioxide+chlorides+residual_sugar+
             citric_acid+volatile_acidity+fixed_acidity, data=train)
fit4

pred4=predict(fit4,newdata = test)

data.frame(pred4)[1:5,]

confusion_mat = as.matrix(table(Actual_Values = test$quality, Predicted_Values = pred4$class)) 
confusion_mat

accuracy = sum(pred4$class==test$quality)/nrow(test)
accuracy
#-------------------------------------------------------------------------------

## Exp3: Regression and LDA, input data with alcohol, sulphates, citric acid and,
## Residual sugar

#Regression
fit5 = lm(quality~alcohol+sulphates+residual_sugar+citric_acid, data=train)

sum5 = summary(fit5) #Summary of fit
coef5 = sum5$coefficients

vif(fit5)

test$pred5=predict(fit5,newdata = test) #Test input data prediction
test$pred5 = round(test$pred5, digit=0)

confusion_mat = as.matrix(table(Actual_Values = test$quality, Predicted_Values = test$pred5)) 
confusion_mat

#Accuracy calculated using mean of correct predictions
accuracy = sum(test$pred5==test$quality)/nrow(test)
accuracy

#Root mean square error score
RMSE = sqrt(mean((test$pred5 - test$quality)^2))
RMSE

#Mean absolute deviation
MAD = mean(abs(test$pred5 - test$quality))
MAD

#Linear Discriminant Analysis
fit6 = lda(quality~alcohol+sulphates+residual_sugar+citric_acid, data=train)
fit6

pred6=predict(fit6,newdata = test)

data.frame(pred6)[1:5,]

confusion_mat = as.matrix(table(Actual_Values = test$quality, Predicted_Values = pred6$class)) 
confusion_mat

accuracy = sum(pred6$class==test$quality)/nrow(test)
accuracy
