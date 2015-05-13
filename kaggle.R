# copying kaggle ----------------------------------------------------------

# For each location, the dots show the date of each record we have in the data.
# For the training data, this shows us when/where measurements were actually taken.
# But for the test data, we see a very regular set of locations/dates.  
# (This is because the test data has been filled in with many rows that don't always correspond to real measurements. 
# We need to make predictions for all of these rows, but only the ones corresponding to real measurements will be 
# used in scoring.)

library(dplyr)
library(readr)
library(ggplot2)

data_dir <- "C:/Users/jbest/Box Sync/Personal (Private) - jbest/R/kaggle/westNile"
train <- read.csv(file.path(data_dir, "train.csv"))
test <- read.csv(file.path(data_dir, "test.csv"))
train$Usage <- "Train"
test$Usage <- "Test"

train_and_test <- bind_rows(train, test)

train_and_test$Date <- as.Date(train_and_test$Date)
locations_and_dates <- train_and_test %>% select(AddressNumberAndStreet, Longitude, Latitude, Date, Usage) %>% unique

theme_set(theme_gray(base_size = 20))
png(height=1920, width=1920)
ggplot(locations_and_dates) + 
    geom_point(aes(x=Date, y=AddressNumberAndStreet, color=Usage), size=1) +
    scale_x_date(breaks = "1 year")
dev.off()

