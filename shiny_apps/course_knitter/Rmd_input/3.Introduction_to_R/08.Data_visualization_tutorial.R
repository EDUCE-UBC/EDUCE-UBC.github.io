# Code associated with "Data visualization tutorial"

###############################################
## Setup
###############################################
### Load data
dat <- read.csv(
  "https://raw.githubusercontent.com/EDUCE-UBC/educer/main/data-raw/data_intermediate_ws.csv")

### Load packages
### R v3.4 or newer
library(tidyverse)

### R v3.3 or older
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

###############################################
## Graphics with `ggplot2`
###############################################
### Building a plot
ggplot(dat)
# equivalent to
dat %>% ggplot()

### Define axes
dat %>%
ggplot(aes(x=O2_uM, y=NO3_uM))

### Map data points
dat %>%
  ggplot(aes(x=O2_uM, y=NO3_uM)) +
  geom_point()

### Alternate aesthetic defintion
dat %>%
  ggplot() +
  geom_point(aes(x=O2_uM, y=NO3_uM))

###############################################
## Add color
###############################################
dat %>%
  ggplot(aes(x=O2_uM, y=NO3_uM)) +
  geom_point(color="purple")

### Incorrect color defintitions
dat %>%
  ggplot(aes(x=O2_uM, y=NO3_uM), color="purple") +
  geom_point()

dat %>%
  ggplot(aes(x=O2_uM, y=NO3_uM)) +
  geom_point(aes(color="purple"))

###############################################
## Add shape
###############################################
dat %>%
  ggplot(aes(x=O2_uM, y=NO3_uM)) +
  geom_point(color="purple", shape=17)

###############################################
## Mapping variables to color
###############################################
dat %>%
  ggplot(aes(x=O2_uM, y=NO3_uM)) +
  geom_point(aes(color=H2S_uM)) 

## Arrange data to better see color
dat %>%
  arrange(H2S_uM) %>%  
  filter(!is.na(H2S_uM)) %>%
  
  ggplot(aes(x=O2_uM, y=NO3_uM)) +
  geom_point(aes(color=H2S_uM)) 

###############################################
## Mapping variables to shape
###############################################
### Failed shape mapping
dat %>%
  arrange(H2S_uM) %>%  
  filter(!is.na(H2S_uM)) %>%
  
  ggplot(aes(x=O2_uM, y=NO3_uM)) +
  geom_point(aes(shape=H2S_uM)) 

### Gather data from plotting
dat %>%
  select(Depth_m, O2_uM, NO3_uM, H2S_uM) %>% 
  gather(key="Chemical", value="Concentration", O2_uM, NO3_uM, H2S_uM)

### Use gathered data in a plot
dat %>%
  select(Depth_m, O2_uM, NO3_uM, H2S_uM) %>% 
  gather(key="Chemical", value="Concentration", O2_uM, NO3_uM, H2S_uM) %>% 
  
  ggplot(aes(x=Concentration, y=Depth_m)) +
  geom_point(aes(shape=Chemical))

### Further customize
dat %>%
  select(Depth_m, O2_uM, NO3_uM, H2S_uM) %>% 
  gather(key="Chemical", value="Concentration", O2_uM, NO3_uM, H2S_uM) %>% 
  
  ggplot(aes(x=Concentration, y=Depth_m)) +
  geom_point(aes(shape=Chemical, color=Chemical)) +
  scale_y_reverse(limits=c(200, 0))

###############################################
## Facets
###############################################
### Return to this plot
dat %>%
select(Depth_m, O2_uM, NO3_uM, H2S_uM) %>% 
gather(key="Chemical", value="Concentration", O2_uM, NO3_uM, H2S_uM) %>% 

ggplot(aes(x=Concentration, y=Depth_m)) +
geom_point(aes(shape=Chemical, color=Chemical)) +
scale_y_reverse(limits=c(200, 0))

### Facet by depth
dat %>%
select(Depth_m, O2_uM, NO3_uM, H2S_uM) %>% 
gather(key="Chemical", value="Concentration", O2_uM, NO3_uM, H2S_uM) %>% 

ggplot(aes(x=Concentration, y=Depth_m)) +
geom_point(aes(shape=Chemical, color=Chemical)) +
scale_y_reverse(limits=c(200, 0)) +
facet_wrap(~Chemical) ## New facets

### Free scale axes
select(Depth_m, O2_uM, NO3_uM, H2S_uM) %>% 
gather(key="Chemical", value="Concentration", O2_uM, NO3_uM, H2S_uM) %>% 

ggplot(aes(x=Concentration, y=Depth_m)) +
geom_point(aes(shape=Chemical, color=Chemical)) +
scale_y_reverse(limits=c(200, 0)) +
facet_wrap(~Chemical, scales="free_x")

###############################################
## End
###############################################