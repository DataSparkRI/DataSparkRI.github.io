library(data.table)
library(tidyverse)
library(COINr)

source("R/functions.R")

# C:\\Users\\Rockwell.Richards\\Documents\\EquityDashboard\\EquityDashboardApp\\Data\\
# purse_file <- "Data/20240308equitypurse.RData" # This purse does not use imputation
# purse_file <- "Data/20240313equitypurse.RData" # This purse uses new imputation (specific measures)
purse_file <- "Data/20240402equitypurse.RData"
equity_purse <- readRDS(purse_file)

# commenvhlt_desc <- read.csv("Data/commenvhlth_desc.csv") %>% data.table() %>%
#   .[,metric := gsub("\\n","\n",metric,fixed=T)]
# stinspnd_desc <- read.csv("Data/stinspnd_desc.csv") %>% data.table() %>%
#   .[,metric := gsub("\\n","\n",metric,fixed=T)]
# engage_desc <- read.csv("Data/engage_desc.csv") %>% data.table() %>%
#   .[,metric := gsub("\\n","\n",metric,fixed=T)]

measure_desc <- read.csv("Data/measure_desc.csv") %>% data.table() %>%
  .[,metric := gsub("\\n","\n",metric,fixed=T)]
measure_dir <- equity_purse$coin[[4]]$Meta$Ind %>% data.table() %>%
  .[Level == 1,.(iCode,Direction)] 
indicator_names <- read.csv("Data/indicator_names.csv") %>% data.table()
indicator_desc <- read.csv("Data/indicator_desc.csv") %>% data.table()
index_names <- read.csv("Data/index_names.csv") %>% data.table()
index_maintable <- read.csv("Data/index_maintable.csv") %>% data.table()
indicator_status <- read.csv("Data/indicator_status.csv") %>% data.table()
index_flowchart <- read.csv("Data/index_flowchart.csv") %>% data.table()
sch_type_CW <- read.csv("Data/SchTypeCW.csv") %>% data.table() %>%
  .[,code := str_pad(code,side="left",width=5,pad=0)]

peers <- find_closest_conditions(equity_purse)

