# Code associated with "Data manipulation tutorial"

###############################################
## Setup
###############################################
### Download data
write.csv(
  read.csv("https://raw.githubusercontent.com/EDUCE-UBC/educer/main/data-raw/data_tidyverse_ws.csv"),
  "Saanich_Data.csv", row.names=FALSE)

###############################################
## The R tidyverse
###############################################
### Base R example
dat[apply(!is.na(dat[,"WS_O2"]), 1, any), 
    c("Cruise", "Date", "Depth", "WS_O2", "WS_NO3", "WS_H2S")]

### Install `tidyverse`
install.packages("tidyverse")

### Load `tidyverse`
### R v3.4 or newer
library(tidyverse)

### R v3.3 or older**
library(readr)
library(dplyr)
library(tidyr)

###############################################
## Load data
###############################################
### Read in tabular data
read_delim(file="Saanich_Data.csv") # Gives error

read_delim(file="/Users/kim/Desktop/Saanich_Data.csv") # Gives error

### Add arugments
read_delim(file="Saanich_Data.csv", delim=",")

### Argument names and order
read_delim("Saanich_Data.csv", ",") # Default order

read_delim("Saanich_Data.csv", TRUE, ",") # Gives error

read_delim("Saanich_Data.csv", col_names=TRUE, delim=",")

### Save data to the Environment
dat = read_delim("Saanich_Data.csv", delim=",", col_names=TRUE)

#Which is equivalent to

dat <- read_delim("Saanich_Data.csv", delim=",", col_names=TRUE)

###############################################
## Subset and clean data
###############################################
### Filter
dat <- filter(dat, !is.na(WS_O2))

### Select
dat <- select(dat, 
              Cruise, Date, Depth,
              WS_O2, WS_NO3, WS_H2S)

### Rename
dat <- rename(dat, O2_uM=WS_O2, NO3_uM=WS_NO3, H2S_uM=WS_H2S)

### Arrange
dat <- arrange(dat, H2S_uM)

### Mutate
dat <- mutate(dat, Depth_m=Depth*1000)

###############################################
## Link functions with %>%
###############################################
dat <- rename(dat, O2_uM=WS_O2, NO3_uM=WS_NO3, H2S_uM=WS_H2S) # Gives error

## Example without pipes
dat <- read_delim("Saanich_Data.csv", delim=",", col_names=TRUE)

dat <- filter(dat, !is.na(WS_O2))

# Example with pipes
dat <- read_delim("Saanich_Data.csv", delim=",", col_names=TRUE) %>% 
  filter(!is.na(WS_O2))

dat <- 
  read_delim("Saanich_Data.csv", delim=",", col_names=TRUE) %>%
  filter(!is.na(WS_O2)) %>% 
  select(Cruise, Date, Depth, WS_O2, WS_NO3, WS_H2S) %>% 
  rename(O2_uM=WS_O2, NO3_uM=WS_NO3, H2S_uM=WS_H2S) %>% 
  arrange(H2S_uM) %>% 
  mutate(Depth_m=Depth*1000)

dat <- 
  read_csv("https://raw.githubusercontent.com/EDUCE-UBC/educer/main/data-raw/data_tidyverse_ws.csv") %>%
  filter(!is.na(WS_O2)) %>% 
  select(Cruise, Date, Depth, WS_O2, WS_NO3, WS_H2S) %>% 
  rename(O2_uM=WS_O2, NO3_uM=WS_NO3, H2S_uM=WS_H2S) %>% 
  arrange(H2S_uM) %>% 
  mutate(Depth_m=Depth*1000)

###############################################
## Summarize data
###############################################
dat %>%
  group_by(Depth_m) %>%
  summarise(Mean_O2=mean(O2_uM),
            SD_O2=sd(O2_uM),
            n=n())

###############################################
## Transform data frames
###############################################

###############################################
### Gather and spread
###############################################
# Example wide data
set.seed(123)
wide = data.frame(
  sample_ID = c(1,2,3,4),
  year_2015 = runif(4, 0, 1) %>% round(3), 
  year_2016 = runif(4, 0.2, 1.2) %>% round(3),
  year_2017 = runif(4, 0.5, 1.5) %>% round(3)
)

wide

# Example long data
gather(wide, key="Year", value="Value", -sample_ID)

# Gather dat
dat

dat2 <- gather(dat, key="Key", value="Value", O2_uM, NO3_uM, H2S_uM)  

dat2

## Compare
dim(dat)

dim(dat2)

unique(dat2$Key)

# Spread dat
dat2 <- spread(dat2, key="Key", value="Value")
dat2

## Compare
dim(dat)

dim(dat2)

## New names
gather(dat, key="Geochemical", value="uM", O2_uM, NO3_uM, H2S_uM)  

###############################################
### *_join
###############################################
# Make separate data
dat_O2 <- dat %>% 
  select(Cruise, Date, Depth_m, O2_uM) %>% 
  arrange(O2_uM) %>% 
  filter(O2_uM != 0)

dat_NO3 <- dat %>% 
  select(Cruise, Date, Depth_m, NO3_uM) %>% 
  arrange(NO3_uM) %>% 
  filter(NO3_uM != 0)

dat_O2
dat_NO3

# Join
full_join(dat_O2, dat_NO3)

full_join(dat_O2, dat_NO3, by=c("Cruise", "Date", "Depth_m"))

###############################################
## End
###############################################