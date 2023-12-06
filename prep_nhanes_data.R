rm(list=ls())

# load packages
library(utils)
library(data.table)
library(foreign)

# save file names and variables of interest
files_to_download <- c('P_DEMO', 'P_BPXO', 
                       'P_ALQ', 'P_BPQ', 'P_DIQ', 'P_HIQ', 'P_HUQ', 'P_INQ', 
                       'P_PAQ', 'P_FSQ', 'P_SMQ', 'P_WHQ')
vars_of_interest <- c('RIAGENDR', 'RIDAGEYR', 'RIDRETH3', 'DMDEDUC2', 'DMDMARTZ', 'DMDBORN4', 'DMDYRUSZ', 'WTINTPRP', 'WTMECPRP',
                      'BPXOSY1', 'BPXODI1', 'BPXOSY2', 'BPXODI2', 'BPXOSY3', 'BPXODI3',
                      'ALQ130', 'BPQ020', 'BPQ040A', 'BPQ080', 'DIQ010', 'HIQ011', 'HUQ051', 'INDFMMPI',
                      'PAD615', 'PAD660', 'PAD680', 'FSDHH', 'SMQ020', 'SMQ040', 'SMQ050Q', 'WHD010', 'WHD020')

# download each data file
for(file_name in files_to_download){
  print(file_name)
  download.file(paste0("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/", file_name, ".XPT"), temp_file <- tempfile(), mode="wb")
  assign(file_name, read.xport(temp_file))
}

# merge data files together
list_df <- lapply(files_to_download, get)
data <- Reduce(function(x, y) merge(x, y, all=TRUE), list_df)
data <- setDT(data)
data <- data[, vars_of_interest, with=F]
