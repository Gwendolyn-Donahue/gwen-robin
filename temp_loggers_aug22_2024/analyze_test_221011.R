################################################################################
##' @title Analyze envlogger data, ice bath data
##' @author Robin Elahi
##' @date 2022-10-11
##' @log 
################################################################################

#### PACKAGES, DATA ####
library(here)
library(tidyverse)
library(lubridate)

theme_set(theme_bw(base_size = 10) + 
            theme(panel.grid.minor = element_blank(), 
                  strip.background = element_blank()))

d <- read_csv(here("data_env_processed/env_test_data_221011.csv"))

d <- d %>% 
  mutate(date_time = with_tz(time, tzone = "US/Pacific"))

# Check for duplicates
d %>% count(file_i)

d %>% 
  ggplot(aes(date_time, temp, color = file_i)) + 
  geom_line() + 
  facet_wrap(~ file_i)

d %>% 
  filter(!is.na(temp)) %>% 
  filter(temp < 17) %>% 
  group_by(serial) %>% 
  summarise(mean = mean(temp),
            sd = sd(temp), 
            n = n(), 
            se = sd / sqrt(n)) %>% 
  as.data.frame()

#### Filter data ####

d2 <- d %>% 
  filter(date_time > "2022-10-11 11:35:00.00" & date_time < "2022-10-11 14:40:00.00")

d2 %>% 
  ggplot(aes(date_time, temp, color = file_i)) + 
  geom_line() + 
  geom_point() + 
  facet_wrap(~ file_i)

serial_summary <- d2 %>% 
  filter(!is.na(temp)) %>% 
  group_by(serial) %>% 
  summarise(mean = mean(temp),
            sd = sd(temp), 
            n = n(), 
            se = sd / sqrt(n), 
            CI = 2 * se) 

d_summary <- d2 %>% 
  filter(!is.na(temp)) %>% 
  summarise(mean = mean(temp),
            sd = sd(temp), 
            n = n(), 
            se = sd / sqrt(n), 
            CI = 2 * se) 

n_per_logger <- d_summary$n / 6

serial_summary %>% 
  ggplot(aes(serial, mean)) + 
  geom_hline(data = d_summary, aes(yintercept = mean), color = "red", 
             linetype = "solid", lwd = 1.1) +
  geom_hline(data = d_summary, aes(yintercept = mean + CI), color = "red", 
             linetype = "dashed", lwd = 1.1) + 
  geom_hline(data = d_summary, aes(yintercept = mean - CI), color = "red", 
             linetype = "dashed", lwd = 1.1) +
  geom_hline(data = d_summary, aes(yintercept = 0), color = "black", 
             linetype = "solid", lwd = 1) +
  geom_point() + 
  geom_errorbar(aes(ymin = mean - CI, ymax = mean + CI), width = 0.2) + 
  labs(x = "Serial number", y = "Temperature (C)",
       caption = "mean +- 95% CI") + 
  coord_flip() + 
  ggtitle(paste("Ice bath, frequency = 10min, 11 Oct 2022, n = ", n_per_logger, sep = ""))

ggsave(here("figs", "env_test_221011.pdf"), height = 3.5, width = 6)

#### Minidot ####

# Need to skip the 2nd line
col_names <- c("time_sec", "bv", "temp", "do", "q")

md <- read_csv(here("data_minidot/2022-10-11 182800Z.txt"), col_names = col_names, 
               skip = 3) 

md <- md %>% 
  mutate(date_time = as_datetime(time_sec, tz = "US/Pacific"))

md %>% 
  ggplot(aes(date_time, temp)) + 
  geom_line()

md %>% 
  ggplot(aes(date_time, do)) + 
  geom_line()

# Filter to match env loggers
md2 <- md %>% 
  filter(date_time > "2022-10-11 11:30:00.00" & date_time < "2022-10-11 14:40:00.00")

md2 %>% 
  ggplot(aes(date_time, temp)) + 
  geom_line()

md2 %>% 
  ggplot(aes(date_time, do)) + 
  geom_line() + 
  geom_point() + 
  labs(x = "Date", y = "Dissolved oxygen (mg/l)")

#### Plot temperature and do on same plot ####

# 14.6 mg/l is maximum for distilled water
pdf(here("figs", "minidot_ice_test_221011.pdf"), width = 6, height = 5)
par(mar = c(5, 4, 4, 4) + 0.3)                     
with(md, plot(date_time, do, type = "b", ylim = c(6, 14.6), las = 1,
              xlab = "Time", ylab = "Dissolved oxgen (mg/l)"))  
abline(h = 14.6, col = "gray")
par(new = TRUE)                             
with(md, plot(date_time, temp, type = "b", pch = 17, col = 1, 
              axes = FALSE, xlab = "", ylab = "")) 
abline(h = 0, col = "gray")
with(md, points(date_time, temp, type = "b", pch = 17, col = 1))
axis(side = 4, at = pretty(c(0, 3), n = 10), las = 1)  
mtext("Temperature (C)", side = 4, line = 3) 
title(main = "Minidot in ice bath, 11 Oct 2022", line = 1)
dev.off()

#### Plot both loggers ####

d %>% 
  filter(date_time < "2022-10-11 14:40:00.00") %>% 
  ggplot(aes(date_time, temp, color = serial)) + 
  geom_line() + 
  geom_point() + 
  geom_line(data = md, aes(date_time), color = "black") +
  geom_point(data = md, aes(date_time), color = "black") +
  labs(x = "Date", y = "Temperature (C)")

ggsave(here("figs", "env_minidot_test_time_series_221011.pdf"), height = 3.5, width = 6)

d2 %>% 
  ggplot(aes(date_time, temp, color = serial)) + 
  geom_hline(aes(yintercept = 0), color = "gray") + 
  geom_line() + 
  geom_point() + 
  geom_line(data = md2, aes(date_time), color = "black") +
  geom_point(data = md2, aes(date_time), color = "black") +
  labs(x = "Date", y = "Temperature (C)")

ggsave(here("figs", "env_minidot_test_time_series_221011_b.pdf"), height = 3.5, width = 6)
