################################################################################
##' @title EnvLogger functions
##' @author Robin Elahi
##' @date 2022-10-04
##' @log 
################################################################################


# Parse env file name
env_file_parse <- function(x, serial_only = TRUE){
  # Extract serial number
  serial <- strsplit(x, "-")[[1]][1]
  # Extract retrieval date
  part_two <- strsplit(x, "-")[[1]][2]
  retrieval_date <- strsplit(part_two, " ")[[1]][1]
  if(serial_only == TRUE){
    return(serial)
  }
  if(serial_only == FALSE){
    return(list("serial" = serial, "retrieval_date" = retrieval_date))
  }
}

# Load a single file and modify
env_file_load <- function(i){
  require(tidyverse)
  read_csv(here("data_env", files[i]), skip = 20) %>% 
    mutate(file_i = as.character(i))
}



# Compile a set of files from a single logger
env_file_compile <- function(){
  # First file
  i <- 1
  d <- env_file_load(i = i)
  
  # Loop through other files and add rows
  for(i in 2:n_files){
    d_i <- env_file_load(i = i)
    d <- rbind(d, d_i)
  }
  
  # Add serial number
  d <- d %>% mutate(serial = serial)
  
  return(d)
}

#### FOLDER ####

# Load a single file and modify
# GZD edited with her paths
env_file_load2 <- function(i, folder){
  require(tidyverse)
  file_path <- file.path(folder, files[i])
  read_csv(file.path(folder, files[i]), skip = 20) %>% 
    mutate(file_i = as.character(i))
}

# Compile a set of files from a single folder
env_file_compile2 <- function(folder = folder){
  require(tidyverse)
  # First file
  i <- 1
  d <- env_file_load2(i = i, folder = folder)
  serial <- env_file_parse(files[i])
  # Add serial number
  d <- d %>% mutate(serial = serial)
  
  # Loop through other files and add rows
  for(i in 2:n_files){
    d_i <- env_file_load2(i = i, folder = folder)
    serial <- env_file_parse(files[i])
    # Add serial number
    d_i <- d_i %>% mutate(serial = serial)
    d <- rbind(d, d_i)
  }
  return(d)
}
