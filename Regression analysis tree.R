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
  mutate(id = 1:nrow(cars_selected))

set.seed(30)

train = cars_selected %>%
  sample_frac(0.7)

validation = cars_selected %>%
  slice(setdiff(cars_selected$id, train$id))

cars.rt = rpart(price~ years_old+ mileage+ mpg_city+ mpg_highway+ 
                  accidents_or_damage+ one_owner+ personal_use_only+ seller_rating+ 
                  driver_rating+is_automatic+ is_manual+ fuel_type_gasoline+ 
                  + fuel_type_hybrid+ drivetrain_All_wheel_Drive+ 
                  drivetrain_Rear_wheel_Drive+drivetrain_Front_wheel_Drive+ manufacturer_BMW+ 
                  manufacturer_Audi+ manufacturer_Volkswagen+ manufacturer_Porsche+ 
                  manufacturer_Mercedes_Benz, 
                data = train, method = "anova",
                cp = 0.001, minsplit = 10, xval = 10)

plotcp(cars.rt)

cp.table = as_tibble(cars.rt$cptable)

optimal.cp = cp.table %>%
  filter(nsplit == 20)

pruned.ct = prune(cars.rt, cp = optimal.cp$CP)

prp(pruned.ct, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = -10)

prediction = predict(pruned.ct, validation, type = "vector")

validation = validation %>%
  mutate(price_prediction = prediction)

accuracy(validation$price_prediction, validation$price)
