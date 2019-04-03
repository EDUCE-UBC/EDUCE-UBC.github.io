# Code associated with "Base R tutorial"
###############################################
## Setup
###############################################
### Download data
write.csv(
  read.csv("https://raw.githubusercontent.com/EDUCE-UBC/workshop_data/master/data.csv"),
  "data.csv", row.names=FALSE)

###############################################
## Load data
###############################################
### Read in tabular data
read.table(file="data.csv")

### Add arugments
read.table(file="data.csv", header=TRUE, sep=",")

### Argument names and order
read.table("data.csv", TRUE, ",")

read.table("data.csv", ",", TRUE) # Gives an error

read.table("data.csv", sep=",", header=TRUE)

### Save data to the Environment
dat = read.table(file="data.csv", header=TRUE, sep=",")

# Which is equivalent to

dat <- read.table(file="data.csv", header=TRUE, sep=",")

###############################################
## Access data
###############################################
### Columns
dat$O2_uM

### Rows and/or columns
dat[4, 3]

dat[4, "O2_uM"]

dat$O2_uM[4]

###############################################
## Basic calculations 
###############################################
# Compute mean Oxygen
mean(dat$O2_uM)

# Compute variance for Oxygen
var(dat$O2_uM)

###############################################
## Subset data
###############################################
### Conditional statements
dat$Depth_m == 200

dat$Depth_m > 100

80 %in% dat$Depth_m

is.na(dat$Depth_m)

### Logical operators
dat$Depth_m > 50 & dat$Depth_m < 150

dat$Depth_m < 50 | dat$Depth_m > 150

dat$Season == "Fall" & dat$Depth_m > 150 & dat$O2_uM == 0

### Use conditional statements and logical operators to subset data
dat$Depth_m == 200

dat[dat$Depth_m == 200, ]

subset_statement <- dat$Season == "Fall" & dat$Depth_m > 150 & dat$O2_uM == 0

subset_statement

dat[subset_statement, ]

###############################################
## End
###############################################