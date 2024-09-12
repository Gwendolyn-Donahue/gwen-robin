################################################################################
##' @title EnvLogger processing - testing new loggers in ice bath
##' @author Robin Elahi #edited Aug 22, 2024 for GZD pathnames
##' @date 2022-10-17
##' @log 
################################################################################

#### PACKAGES, DATA ####
library(here)
library(tidyverse)
library(lubridate)

theme_set(theme_bw(base_size = 10) + 
            theme(panel.grid.minor = element_blank(), 
                  strip.background = element_blank()))

##### Robin paths: ##### 
source("env_logger/env_logger_functions.R")

folder <- "env_test_data_221011"

#Test data 

# Get list of files
files <- list.files(path = here("data_env", folder))
files

#####  GZD paths: ##### 
source("/Users/gwendolyndonahue/Desktop/temp_loggers_aug22_2024/env_logger_functions.R")

folder <- "/Users/gwendolyndonahue/Desktop/temp_loggers_aug22_2024/envloggers_240822"

files <- list.files(path = folder, recursive = TRUE)
files
#################

# create DF
d_serial <- tibble(
  serial = sapply(files, env_file_parse))
#d_serial <- d_serial %>%
  #mutate(letter = c("F", "C", "B", "E", "D", "A"))

# create column for logger name; rename serial number to remove this prefix


# Compile files
n_files <- length(files)
i <- 1
d <- env_file_compile2(folder = folder)

## Quality control
# Check for duplicates
d %>% count(file_i)

d %>% 
  ggplot(aes(time, temp, color = file_i)) + 
  geom_line() + 
  facet_wrap(~ file_i)

d2 <- d %>% 
  filter(time > "2024-08-21 21:53:01" & time < "2024-08-21 23:59:59")

d2 <- d %>% 
  filter(time < "2024-08-21 21:53:01")

d2 <- d %>%
  filter(temp < 10)

d2 %>% 
  ggplot(aes(time, temp, color = file_i)) + 
  #geom_point() + 
  geom_line() + 
  facet_wrap(~ file_i)

## Write file
write_csv(d2, here("data_env_processed", paste(folder, ".csv", sep = "")))

          