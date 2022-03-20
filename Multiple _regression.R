house_data=read.csv("C:/Users/SOSU/Dropbox/Machine_learning/Multiple regression/kc_house_data.csv")
summary(house_data)
nrow(house_data)
ncol(house_data)
str(house_data)
H.date1 <- as.Date(as.character(house_data$date),format = "%Y%m%d")

## see the multivariate scatter plot
#install.packages("ggplot2")            # Packages need to be installed only once
#install.packages("GGally")
library("ggplot2")                     # Load ggplot2 package
library("GGally")                      # Load GGally package
ggpairs(house_data[,3:21])

## bedroom vs price plot
plot(house_data$bedrooms,house_data$price)
library(ggplot2)
# Basic scatter plot
library(ggplot2)
ggplot(house_data, aes(x=bedrooms, y=price)) + geom_point()

cor_mat=cor(house_data[,3:21],use="complete.obs") #here i used complete.obs for removing any 
#rows if it contains any of the NA values corresponding to a column


##data partion
library(purrr)
part1=runif(nrow(house_data))  #generate random number 
part1_index=order(part1)
train_data=house_data[part1_index[1:15000],]  #in trainn dataset have 15000 observation
test_data=house_data[part1_index[15001:21613],] #in test dataset have 6613 observation

## now I will fit simple multiple linear regression
house_data_regre=lm(price~bedrooms,data=train_data)
summary(house_data_regre)

pred_value=predict(house_data_regre,newdata=test_data)
test_data$predicted_price=pred_value  #add a new variable to the test data




###multiple variable regression
kc_all_regre=lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition
                +grade+sqft_above+sqft_basement+yr_built+yr_renovated+zipcode+lat+long+sqft_living15
                +sqft_lot15,data=train_data)
summary(kc_all_regre)
pred_values_all=predict(kc_all_regre,newdata = test_data)
test_data$predicted=pred_values_all  #predict when consider all variables
error=test_data$price-test_data$predicted  #difference of orginal price and predict values


library(ggplot2)
t_d=data.frame(original_price=test_data$price,predicted_value=test_data$predicted,x=test_data$id)
ggplot(t_d,aes(x))+
  geom_line(aes(y=original_price,colour="original_price"))+
  geom_line(aes(y=predicted_value,colour="predicted_value"))
