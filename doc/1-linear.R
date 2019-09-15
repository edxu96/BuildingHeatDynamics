## ---- include = FALSE----------------------------------------------------
rm(list = ls())
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.asp = 9/16,
  fig.width = 7,
  warning = FALSE
)

## ---- message=FALSE------------------------------------------------------
library(tidyverse)
library(broom)
library(magrittr)
library(forecast)
library(ggfortify)

## ---- message=FALSE------------------------------------------------------
ti <- 
  read_csv("~/GitHub/tidynamics/data/soenderborg_2day.csv") %>%
  mutate(
    "is_win" = as.POSIXlt("2010-11-01") <= as.POSIXlt(.$t) &
      as.POSIXlt(.$t) < as.POSIXlt("2011-02-01")
    ) %>%
  select(t, P3, Te, G, Ws, is_win) %>%
  drop_na()

## ------------------------------------------------------------------------
li_mod <- list(
  "entire" = lm(P3 ~ Te, ti),
  "winter" = lm(P3 ~ Te, filter(ti, is_win))
)

## ------------------------------------------------------------------------
li_mod %>%
  map_df(tidy, .id = "period")

## ------------------------------------------------------------------------
li_mod %>%
  map_df(glance, .id = "period")

## ---- fig.cap="Linear Reg of Heating Load (y) and Ext Temp (x) in Entire Period"----
autoplot(li_mod$entire, data = ti, colour = "is_win")

## ---- fig.cap="Linear Reg of Heat Load (y) and Ext Temp (x) in Winter"----
autoplot(li_mod$winter)

## ---- fig.cap="ACF of Residuals from Linear Reg Model `lm_winter_3`"-----
forecast::ggtsdisplay(li_mod$entire$residuals, lag.max = 30)

## ---- fig.cap="ACF of Residuals from Linear Reg Model `lm_3`"------------
forecast::ggtsdisplay(li_mod$winter$residuals, lag.max = 30)

