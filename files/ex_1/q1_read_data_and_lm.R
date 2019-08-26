







## ----Both summer and winter-------------------------------------
## But what if we don't remove the summer period?
df_x <- df_xorig
plot(df_x$Te, df_x$P)

## Simplest linear model
fit <- lm(P ~ Te, df_x)
summary(fit)
abline(fit)

## Model validation (check i.i.d. and distribution of residuals)
par(mfrow = c(2,2))
## Plot residuals
plot(fit$residuals)
## Residuals vs. input Te: There shouldn't be any relation
ival <- as.integer(names(fit$residuals))
plot(df_x$Te[ival], fit$residuals)
## Check the distribution: Should not be too skewed
hist(fit$residuals)
qqnorm(fit$residuals)
qqline(fit$residuals)

## And of course ACF
par(mfrow = c(1,1))
acf(fit_winter$residuals)

## Another great plot to check residuals
df_x$residuals <- NA
df_x$residuals[ival] <- fit$residuals
pairs(df_x[c("residuals","t","Te")], panel = panel.smooth, lower.panel = NULL)
