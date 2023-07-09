rm(list = ls())
library("tidyverse")
library("forecast")
library("tidyr")
library("stringr")
library("dplyr")
library("FNN")
library("lattice")
library("caret")

#need to change mpg to int

cars_c = read_csv("cars_c.csv")

cars_c = cars_c %>% 
  filter(manufacturer == "BMW" | manufacturer == "Audi" | 
           manufacturer == "Mercedes-Benz" | manufacturer == "Volkswagen" | 
           manufacturer == "Porsche" | manufacturer == "Volvo") %>% 
  filter(fuel_type=="Gasoline" | fuel_type==" Diesel"| fuel_type=="Hybrid"| fuel_type=="Electric")



cars_c = cars_c %>%
  rename(years_old = year) %>%
  mutate(years_old = 2023 - years_old)

cars_c = cars_c %>%
  mutate(price_drop = ifelse(is.na(price_drop), 0, price_drop))
  
cars_c = cars_c %>%
  mutate(final_price = price - price_drop)

cars_c = cars_c %>%
  rename(mpg_city = mpg.1, mpg_highway = mpg.2)

cars_c = cars_c %>%
  filter(between(final_price, 20000, 50000))

auto_trans = grepl("Automatic", cars_c$transmission, ignore.case = TRUE)
manual_trans = grepl("Manual", cars_c$transmission, ignore.case = TRUE)
auto_trans = as.numeric(auto_trans)
manual_trans = as.numeric(manual_trans)

cars_c$is_automatic <- auto_trans
cars_c$is_manual <- manual_trans


#fuck that now we do our mlr!

cars_c = cars_c %>%
  mutate(fuel_type_gasoline= if_else(fuel_type=="Gasoline",1,0),
         fuel_type_electric= if_else(fuel_type=="Electric",1,0),
         fuel_type_hybrid= if_else(fuel_type=="Hybrid",1,0),
         manufacturer_BMW = if_else(manufacturer=="BMW",1,0),
         manufacturer_Audi= if_else(manufacturer=="Audi",1,0),
         manufacturer_Volkswagen= if_else(manufacturer=="Volkswagen",1,0),
         manufacturer_Porsche= if_else(manufacturer=="Porsche",1,0),
         manufacturer_Mercedes_Benz= if_else(manufacturer=="Mercedes-Benz",1,0), manufacturer_Volvo = ifelse(manufacturer == "Volvo",1,0),
         drivetrain_All_wheel_Drive = if_else(drivetrain == "All-wheel Drive", 1, 0),
         drivetrain_Rear_wheel_Drive = if_else(drivetrain == "Rear-wheel Drive", 1, 0), 
         drivetrain_Front_wheel_Drive = if_else(drivetrain == "Front-wheel Drive", 1, 0))

cars_selected = cars_c %>%
  select(manufacturer,final_price, years_old, mileage, mpg_city, mpg_highway, 
         accidents_or_damage, one_owner, fuel_type, personal_use_only, seller_rating, 
         driver_rating, price, price_drop, 
         is_automatic, is_manual, fuel_type_gasoline, drivetrain_All_wheel_Drive, drivetrain_Rear_wheel_Drive, 
         drivetrain_Front_wheel_Drive, manufacturer_BMW, manufacturer_Audi, 
         manufacturer_Volkswagen, manufacturer_Porsche, manufacturer_Mercedes_Benz, manufacturer_Volvo)


cars_selected = cars_selected %>%
  drop_na()

cars_selected  = cars_selected %>%
  mutate(id = 1:nrow(cars_selected))

cars_selected.mlr = lm(final_price ~ years_old + mileage + mpg_city + mpg_highway + accidents_or_damage + one_owner + personal_use_only + seller_rating + driver_rating+ price_drop + is_automatic + is_manual + fuel_type_gasoline + drivetrain_All_wheel_Drive + drivetrain_Rear_wheel_Drive + drivetrain_Front_wheel_Drive + manufacturer_BMW + manufacturer_Audi + manufacturer_Volkswagen + manufacturer_Porsche + manufacturer_Mercedes_Benz, cars_selected)

summary(cars_selected.mlr)

#Train and Validation data (Accuracy Measures)

set.seed(30)

cars_selected_acc = cars_selected %>%
  select(final_price, years_old, mileage, mpg_city, mpg_highway, 
         accidents_or_damage, one_owner, personal_use_only, seller_rating, 
         driver_rating, price_drop, 
         is_automatic, is_manual, fuel_type_gasoline, drivetrain_All_wheel_Drive, drivetrain_Rear_wheel_Drive, 
         drivetrain_Front_wheel_Drive, manufacturer_BMW, manufacturer_Audi, 
         manufacturer_Volkswagen, manufacturer_Porsche, id)

train = cars_selected_acc %>%
  sample_frac(0.7)

validation = cars_selected_acc %>%
  slice(setdiff(cars_selected_acc$id, train$id))

train.mlr = lm(final_price ~ years_old+ mileage + mpg_city + mpg_highway + accidents_or_damage + one_owner + personal_use_only + seller_rating + driver_rating + price_drop + is_automatic + is_manual + fuel_type_gasoline+ drivetrain_All_wheel_Drive + drivetrain_Rear_wheel_Drive + drivetrain_Front_wheel_Drive + manufacturer_BMW + manufacturer_Audi + manufacturer_Volkswagen + manufacturer_Porsche, train)

summary(train.mlr)

validation = validation %>%
  mutate(prediction_price = predict(train.mlr, validation))

accuracy(validation$prediction_price, validation$final_price)

train = train %>%
  mutate(price_prediction = predict(train.mlr, train))

accuracy(train$price_prediction, train$final_price)

newdata = as_tibble(list(years_old= c(1, 3, 5, 7, 10), mileage=c(16000, 36000, 50000, 80000, 110000) ,mpg_city=c(26, 27, 30, 29, 32), mpg_highway=c(29, 30, 31, 32, 35),
                         accidents_or_damage=c(0, 0, 0, 0 ,0), one_owner=c(1, 1, 0, 0, 0), personal_use_only=c(1, 0, 1, 0, 1) ,seller_rating=c(4.9, 4.4, 4.6, 4.8, 5),
                         driver_rating= c(5, 4.2, 4.5, 4.7, 4.8), price_drop=c(1000, 3000, 5000, 6000, 6500),
                         is_automatic=c(1,1,1, 1, 1), is_manual=c(0,0,0, 0, 0), fuel_type_gasoline=c(1, 1, 1, 1,0), 
                         fuel_type_hybrid=c(0, 0 ,0, 0, 1), drivetrain_All_wheel_Drive=c(0,0,1, 0, 1), drivetrain_Rear_wheel_Drive=c(0,1,0, 1, 0), 
                         drivetrain_Front_wheel_Drive=c(1,0,0, 0, 0), manufacturer_BMW=c(1,0, 0, 0 , 0), manufacturer_Audi=c(0, 0, 0, 1,0), 
                         manufacturer_Volkswagen = c(0,2,0, 0, 0), manufacturer_Porsche=c(0,0,1, 0 , 0), manufacturer_Mercedes_Benz = c(0,0,0, 0,1)))

predict(cars_selected.mlr, newdata)


