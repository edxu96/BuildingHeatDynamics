```{r, warning=FALSE, fig.height = 11, fig.width = 11}
mod_2 <- arima(dat.f$heating, order = c(0, 0, 0), include.mean = F)
mod_2
plotTimeSeriesResidual(mod_2, num_lag = 100, "Model 2")
```


## Model 1: SARIMA(1, 0, 0)(0, 0, 0)

```{r, warning=FALSE, fig.height = 11, fig.width = 11}
(mod_1 <- arima(dat.f$heating, order = c(2, 0, 0)))
plotTimeSeriesResidual(mod_1, num_lag = 100, "Model 1")
```


```{r, warning=FALSE, fig.height = 11, fig.width = 11}
iSolar.pw <- filter(dat.f$iSolar, filter = c(ar = c(0.7119, 0.1020, -0.1685, 0.1465), 
                                             ma = c(rep(0, 47), 0.2152 - 0.1809, rep(0, 47), - 0.2152 * 0.1809)), sides = 1)
heating.pw <- filter(dat.f$iSolar, filter = c(ar = c(0.7119, 0.1020, -0.1685, 0.1465), 
                                              ma = c(rep(0, 47), 0.2152 - 0.1809, rep(0, 47), - 0.2152 * 0.1809)), sides = 1)
ccf(mod_1.1.2$residuals, heating.pw, type = "correlation", na.action = na.pass)
ccf(iSolar.pw, heating.pw, type = "correlation", na.action = na.pass)
```

```{r, warning=FALSE, fig.height = 5, fig.width = 11}
tempExternal.pw <- filter(dat.f$tempExternal, filter = c(ar = 1, ma = c(0.2873, rep(0, 46), 0.0458, 0.2873 * 0.0458)), 
                          sides = 1)
heating.pw <- filter(dat.f$heating, filter = c(ar = 1, ma = c(0.2873, rep(0, 46), 0.0458, 0.2873 * 0.0458)), sides = 1)
ccf(mod_1.1.1$residuals, heating.pw, na.action = na.pass)
ccf(tempExternal.pw, heating.pw, na.action = na.pass)
```