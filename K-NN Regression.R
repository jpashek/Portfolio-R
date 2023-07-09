library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(forecast)
library(dplyr)
library(FNN)
library("FNN")

# read csv (modified to have mpg data usable)
cars_c = read_csv("cars_c.csv")

#filter manufacturer, fuel type, and price
cars_c = cars_c %>% 
  filter(manufacturer == "BMW" | manufacturer == "Audi" | 
           manufacturer == "Mercedes-Benz" | manufacturer == "Volkswagen" | 
           manufacturer == "Porsche") %>% 
  filter(fuel_type=="Gasoline"| fuel_type=="Hybrid") %>% 
  filter(between(price, 1000, 300000))

#Make years a numeric data point from year 2023 for age)
cars_c = cars_c %>%
  rename(years_old = year) %>%
  mutate(years_old = 2023 - years_old)

#renaming mpg data
cars_c = cars_c %>%
  rename(mpg_city = mpg.1, mpg_highway = mpg.2)

auto_trans = grepl("Automatic", cars_c$transmission, ignore.case = TRUE)
manual_trans = grepl("Manual", cars_c$transmission, ignore.case = TRUE)
auto_trans = as.numeric(auto_trans)
manual_trans = as.numeric(manual_trans)

cars_c$is_automatic = auto_trans
cars_c$is_manual = manual_trans

#selected data

cars_c = cars_c %>%
  mutate(fuel_type_gasoline= if_else(fuel_type=="Gasoline",1,0),
         fuel_type_hybrid= if_else(fuel_type=="Hybrid",1,0),
         manufacturer_BMW = if_else(manufacturer=="BMW",1,0),
         manufacturer_Audi= if_else(manufacturer=="Audi",1,0),
         manufacturer_Volkswagen= if_else(manufacturer=="Volkswagen",1,0),
         manufacturer_Porsche= if_else(manufacturer=="Porsche",1,0),
         manufacturer_Mercedes_Benz= if_else(manufacturer=="Mercedes-Benz",1,0),
         drivetrain_All_wheel_Drive = if_else(drivetrain == "All-wheel Drive", 1, 0),
         drivetrain_Rear_wheel_Drive = if_else(drivetrain == "Rear-wheel Drive", 1, 0), 
         drivetrain_Front_wheel_Drive = if_else(drivetrain == "Front-wheel Drive", 1, 0))

cars_selected = cars_c %>%
  select(price, years_old, mileage, mpg_city, mpg_highway, 
         accidents_or_damage, one_owner, personal_use_only, seller_rating, 
         driver_rating,is_automatic, is_manual, fuel_type_gasoline,
         fuel_type_hybrid, drivetrain_All_wheel_Drive, 
         drivetrain_Rear_wheel_Drive,drivetrain_Front_wheel_Drive, manufacturer_BMW, 
         manufacturer_Audi, manufacturer_Volkswagen, manufacturer_Porsche, 
         manufacturer_Mercedes_Benz)

cars_selected = cars_selected %>%
  drop_na()


cars_selected = cars_selected %>%
  mutate(id = 1:nrow(cars_selected)) %>%
  rename(price_actual = price)

set.seed(30)
train = cars_selected %>%
  sample_frac(0.7)
validation = cars_selected %>%
  slice(setdiff(cars_selected$id, train$id))

a1 = mean(train$years_old)
a2 = sd(train$years_old)
b1 = mean(train$mileage)
b2 = sd(train$mileage)
c1 = mean(train$mpg_city)
c2 = sd(train$mpg_city)
d1 = mean(train$mpg_highway)
d2 = sd(train$mpg_highway)
e1 = mean(train$seller_rating)
e2 = sd(train$seller_rating)
f1 = mean(train$driver_rating)
f2 = sd(train$driver_rating)

train = train %>%
  mutate(years_old_norm = (years_old - a1)/a2,mileage_norm = (mileage - b1)/b2, 
         mpg_city_norm=(mpg_city - c1)/c2, mpg_highway_norm=(mpg_highway-d1)/d2, 
         seller_rating_norm=(seller_rating-e1)/e2,driver_rating_norm=(driver_rating-f1)/f2)

train_input_norm = train %>%
  select(years_old_norm, mileage_norm, mpg_city_norm, mpg_highway_norm, seller_rating_norm,
         driver_rating_norm, accidents_or_damage,
         one_owner,personal_use_only,is_automatic, is_manual,fuel_type_gasoline, 
         fuel_type_hybrid, drivetrain_All_wheel_Drive, drivetrain_Rear_wheel_Drive, 
         drivetrain_Front_wheel_Drive, manufacturer_BMW, manufacturer_Audi, 
         manufacturer_Volkswagen, manufacturer_Porsche, manufacturer_Mercedes_Benz) 

validation = validation %>%
  mutate(years_old_norm = (years_old - a1)/a2,mileage_norm = (mileage - b1)/b2, 
         mpg_city_norm=(mpg_city - c1)/c2, mpg_highway_norm=(mpg_highway-d1)/d2, 
         seller_rating_norm=(seller_rating-e1)/e2,driver_rating_norm=(driver_rating-f1)/f2)

validation_input_norm = validation %>%
  select(years_old_norm, mileage_norm, mpg_city_norm, mpg_highway_norm, seller_rating_norm,
         driver_rating_norm, accidents_or_damage,
         one_owner,personal_use_only,is_automatic, is_manual,fuel_type_gasoline, 
         fuel_type_hybrid, drivetrain_All_wheel_Drive, drivetrain_Rear_wheel_Drive, 
         drivetrain_Front_wheel_Drive, manufacturer_BMW, manufacturer_Audi, 
         manufacturer_Volkswagen, manufacturer_Porsche, manufacturer_Mercedes_Benz) 

train_output = train$price_actual
validation_outputs = validation$price_actual

##RMSE
k_value = c()
rmse = c()
for (k in 1:50) {
  print(k)
  knn_model = knn.reg(train_input_norm, validation_input_norm, train_output, k)
  validation_predict = as.data.frame(knn_model[["pred"]])
  k_value = append(k_value, k)
  rmse = append(rmse, accuracy(validation_predict$`knn_model[["pred"]]`, validation_outputs)[2])
}

print(rmse)

which(rmse == min(rmse))

# Plot rmse vs k

plot(k_value, rmse)
lines(k_value, rmse)

## accuracy for k=3
k3 = knn.reg(train_input_norm, validation_input_norm, train_output, 3)
validation_k3 = as.data.frame(k3[["pred"]])
accuracy(validation_k3$`k3[["pred"]]`, validation_outputs)

##MAPE
k_value = c()
mape = c()
for (k in 1:50) {
  print(k)
  knn_model = knn.reg(train_input_norm, validation_input_norm, train_output, k)
  validation_predict = as.data.frame(knn_model[["pred"]])
  k_value = append(k_value, k)
  mape = append(mape, accuracy(validation_predict$`knn_model[["pred"]]`, validation_outputs)[5])
}

print(mape)

which(mape == min(mape))

# Plot mape vs k

plot(k_value, mape)
lines(k_value, mape)

## accuracy for k=6
k6 = knn.reg(train_input_norm, validation_input_norm, train_output, 6)
validation_k6 = as.data.frame(k6[["pred"]])
accuracy(validation_k6$`k6[["pred"]]`, validation_outputs)

##Predicting price

newdata = newdata %>%
  mutate(years_old_norm = (years_old - a1)/a2,mileage_norm = (mileage - b1)/b2, 
         mpg_city_norm=(mpg_city - c1)/c2, mpg_highway_norm=(mpg_highway-d1)/d2, 
         seller_rating_norm=(seller_rating-e1)/e2,driver_rating_norm=(driver_rating-f1)/f2)

newdata_input_norm = newdata %>%
  select(years_old_norm, mileage_norm, mpg_city_norm, mpg_highway_norm, seller_rating_norm,
         driver_rating_norm, accidents_or_damage,
         one_owner,personal_use_only,is_automatic, is_manual,fuel_type_gasoline, 
         fuel_type_hybrid, drivetrain_All_wheel_Drive, drivetrain_Rear_wheel_Drive, 
         drivetrain_Front_wheel_Drive, manufacturer_BMW, manufacturer_Audi, 
         manufacturer_Volkswagen, manufacturer_Porsche, manufacturer_Mercedes_Benz) 

cars1 = cars_selected %>%
  mutate(years_old_norm = (years_old - a1)/a2,mileage_norm = (mileage - b1)/b2, 
         mpg_city_norm=(mpg_city - c1)/c2, mpg_highway_norm=(mpg_highway-d1)/d2, 
         seller_rating_norm=(seller_rating-e1)/e2,driver_rating_norm=(driver_rating-f1)/f2)

cars_input_norm = cars1 %>%
  select(years_old_norm, mileage_norm, mpg_city_norm, mpg_highway_norm, seller_rating_norm,
         driver_rating_norm, accidents_or_damage,
         one_owner,personal_use_only,is_automatic, is_manual,fuel_type_gasoline, 
         fuel_type_hybrid, drivetrain_All_wheel_Drive, drivetrain_Rear_wheel_Drive, 
         drivetrain_Front_wheel_Drive, manufacturer_BMW, manufacturer_Audi, 
         manufacturer_Volkswagen, manufacturer_Porsche, manufacturer_Mercedes_Benz) 

cars_output = cars1$price_actual

knn.reg(cars_input_norm, newdata_input_norm, cars_output, 3)

knn.reg(cars_input_norm, newdata_input_norm, cars_output, 6)
