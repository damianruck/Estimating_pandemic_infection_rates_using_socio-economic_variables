### Load libraries
library(pacman)
pacman::p_load(tidyverse,plyr,scales,lubridate,readxl,gdata,gplots,GGally,
               ggpubr,geofacet,RColorBrewer,patchwork,ggpmisc,polynom)

### Load data
source("scripts/load_data.R")

### Create FIGURE 2
source("scripts/figure2_clean.R")
source("scripts/figure2_plot.R")

### Create FIGURE 3
source("scripts/figure3_clean.R")
source("scripts/figure3_plot.R")

