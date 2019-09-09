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
  dir("./vignettes/funcs-pred", full.names = TRUE), source
  )

#### 1, Data Process ####
## The data is stored in list

li <- readRDS("./data/data_soenderborg.RDS")

#### 2, Understand Forecast ####

get_ahead <- function(chr){
  return(strtoi(str_sub(chr, 2, -1)))
}

## Get the tibble with prediction and observation
ti <-
  as_tibble(
    cbind(
      data.frame("t" = li$t, "s" = 1:length(li$t), "obs" = li$Ta), li$Tanwp
      )
  ) %>%
  mutate(k0 = NA) %>%
  gather(-t, -s, -obs, key = "ahead_chr", value = "pred") %>%
  mutate(ahead = map_int(ahead_chr, get_ahead)) %>%
  mutate(w = s + ahead) %>%
  select(w, ahead, pred, obs, s, t) %>%
  arrange(w, ahead)

ti %>%
  filter(ahead == 0) %>%
  ggplot() +
  geom_line(mapping = aes(x = t, y = obs)) +
  labs(
    title = "Obs of Ambient Temp",
    x = "Time (Day)", y = "Temp (Celsius Degree)"
  )

## It's obvious that the 1 step ahead prediciton is more accurate than
##   that in 24 step.
ti %>%
  filter(ahead %in% c(1, 24)) %>%
  ggplot() +
    geom_point(mapping = aes(x = obs, y = pred, color = ahead)) +
    geom_abline(aes(intercept = 0, slope = 1), color = "red") +
    labs(
      title = "Obs, 1 and 24 Step Ahead Pred of Ambient Temp",
      x = "Observation (Celsius Degree)", y = "Prediction (Celsius Degree)"
      )




