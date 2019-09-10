## Prediction
## Edward J. Xu
## Sept 8, 2019

rm(list = ls())

library(tidyverse)
library(tidymodels)
library(magrittr)
library(tsibble)
library(ggplot2)
library(bbplot)
library(purrr)
library(lubridate)
library(stringr)
setwd("~/GitHub/tidynamics")
sapply(
  dir("./examples/3-pred/funcs", full.names = TRUE), source
  )

#### 1, Data Process ####
## The data is stored in list
li <- readRDS("./data/data_soenderborg.RDS")

## Change the column names in `li$Gnwp` like "k1" to "t1"
for (i in names(li$Gnwp)) {
  li$Gnwp[paste0("t", str_sub(i, 2, -1))] = li$Gnwp[i]
}


#### 2, Relate Tibbles ####

#' Get the first character in the string
str_sub_1 <- function(chr){
  i <- NA
  if (str_sub(chr, 1, 1) == "k") {
    i <- "t_a_p"
  } else {
    i <- "g_p"
  }
  return(i)
}

#' Get the value of step ahead
get_ahead <- function(chr){
  return(strtoi(str_sub(chr, 2, -1)))
}

ti <- as_tibble(
    cbind(
      data.frame(
        "time" = li$t, "t_a" = li$Ta, "g" = li$G), li$Tanwp, li$Gnwp[, 50:98]
    )
  )

ti %>%  # Check if `time` is the primary key
  count(time) %>%
  {nrow(filter(., n > 1)) == 0}

ti_time <- ti %>%
  mutate(at = 1:nrow(.)) %>%
  mutate(fo = 1:nrow(.)) %>%
  select(at, fo, time) %>%
  print()

ti_obs <- ti %>%
  mutate(fo = 1:nrow(.)) %>%
  select(fo, t_a, g) %>%
  print()

ti_pred <- ti %>%
  mutate(at = 1:nrow(.)) %>%
  select(at, 4:90) %>%
  gather(-at, key = "ahead_chr", value = "pred") %>%
  mutate(whi = map_chr(ahead_chr, str_sub_1)) %>%
  mutate(ahead = map_int(ahead_chr, get_ahead)) %>%
  select(-ahead_chr) %>%
  mutate(fo = at + ahead) %>%
  spread(key = whi, value = pred) %>%
  arrange(at) %>%
  print()

saveRDS(list("time" = ti_time, "obs" = ti_obs, "pred" = ti_pred),
        "./data/data_soenderborg_tidy.RDS")

#### 2, Visualize Forecast ####

ti_pred %>%
  left_join(ti_obs, by = "fo") %>%
  filter(ahead %in% c(1, 24)) %>%
  ggplot() +
    geom_point(mapping = aes(x = t_a, y = t_a_p, color = ahead)) +
    geom_abline(aes(intercept = 0, slope = 1), color = "red") +
    labs(
      title = "Obs, 1 and 24 Step Ahead Pred of Ambient Temp",
      x = "Observation (Celsius Degree)", y = "Prediction (Celsius Degree)"
      )

ti_obs %>%
  left_join(ti_time, by = "fo") %>%
  ggplot() +
  geom_line(mapping = aes(x = time, y = t_a)) +
  labs(
    title = "Obs of Ambient Temp",
    x = "Time (Day)", y = "Temp (Celsius Degree)"
  )





