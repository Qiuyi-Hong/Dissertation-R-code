library('rugarch')
library('rmgarch')
library('quantmod')

#scrips#
##crude oil##
crude_oil <- readxl::read_xlsx("Documents/dissertation/crude oil.xlsx")
plot(crude_oil, type = 'l', ylab = 'prices', main = 'crude oil daily prices')
crude_oil_returns <- diff(log(crude_oil$PX_LAST))
plot(x = crude_oil$Dates[2:5217], y = crude_oil_returns, type = 'l', xlab = 'dates', ylab = 'crude oil daily returns')
ugfit_crude_oil_returns <- ugarchfit(spec = ug_spec, data = crude_oil_returns)
ug_var_crude_oil_returns <- ugfit_crude_oil_returns@fit$var
ug_vol_crude_oil_returns <- sqrt(ug_var_crude_oil_returns)
plot(x = crude_oil$Dates[2:5217], y = ug_vol_crude_oil_returns, type = 'l', xlab = 'dates', ylab = 'volatility', main = 'volatility of crude oil returns')

ug_spec <- ugarchspec(variance.model = list(model = 'gjrGARCH', garchOrder = c(1,1)), mean.model = list(armaOrder = c(1,1), include.mean = TRUE), distribution.model = 'norm')

par(mfrow = c(2,1))
plot(crude_oil, type = 'l', ylab = 'prices', main = 'crude oil daily prices')
plot(gold, type = 'l', ylab = 'prices', main = 'gold daily prices')

plot(crude_oil, type = 'l', col = 'red')
par(new = TRUE)
plot(gold, type = 'l', col = 'green', axes = FALSE)
legend(x = 2004,y = 120,legend = c('crude oil', 'gold'), fill = c('red', 'green'), lty = 1)

#multivariate GARCH for crude oil and gold#
rx <- data.frame(crude_oil_returns, gold_returns)
names(rx)[1] <- 'crude oil returns'
names(rx)[2] <- 'gold returns'

uspec.n <- multispec(replicate(2, ugarchspec(mean.model = list(armaOrder = c(1,0)))))
multif <- multifit(uspec.n, rx)
spec1 <- dccspec(uspec = uspec.n, dccOrder = c(1,1), distribution = 'mvnorm')
fit1 <- dccfit(spec1, data = rx, fit.control = list(eval.se = TRUE), fit = multif)
cor1 <- rcor(fit1)
cor_crude_oil_gold <- cor1[1,2,]
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_gold, type = 'l', xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and gold')



#natural gas#
natural_gas <- readxl::read_xlsx('documents/dissertation/natural gas.xlsx')
plot(natural_gas, type = 'l',ylab = 'prices', main = 'natural gas daily prices')
natural_gas_returns <- diff(log(natural_gas$PX_LAST))
plot(x = natural_gas$Dates[2:5217], y = natural_gas_returns, type = 'l', xlab = 'dates', ylab = 'natural gas daily returns')
ugfit_natural_gas_returns <- ugarchfit(spec = ug_spec, data = natural_gas_returns)
ug_var_natural_gas_returns <- ugfit_natural_gas_returns@fit$var
ug_vol_natural_gas_returns <- sqrt(ug_var_natural_gas_returns)
plot(x = natural_gas$Dates[2:5217], y = ug_vol_natural_gas_returns, type = 'l', xlab = 'dates', ylab = 'volatility', main = 'volatility of natural gas returns')

#mulitvariate garch model of crude oil and lean hogs#
rx2 <- data.frame(crude_oil_returns, lean_hogs_returns)
names(rx2)[1] <- 'crude oil returns'
names(rx2)[2] <- 'lean hogs returns'
multif2 <- multifit(uspec.n, rx2)
fit2 <- dccfit(spec1, data = rx2, fit.control = list(eval.se = TRUE), fit = multif2)
cor2 <- rcor(fit2)
cor_crude_oil_lean_hogs <- cor2[1,2,]
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_lean_hogs, type = 'l', xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and lean hogs')

par(mfrow = c(2,1))
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_gold, type = 'l', xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and gold')
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_natural_gas, type = 'l', xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and natural gas')
par(mfrow = c(1,1))
#######################
##agricultural market##
#######################
#corn#
corn <- readxl::read_xlsx('documents/dissertation/corn.xlsx')
plot(corn, type = 'l',ylab = 'prices', main = 'corn daily prices')
corn_returns <- diff(log(corn$PX_LAST))
plot(x = corn$Dates[2:5217], y = corn_returns, type = 'l', xlab = 'dates', ylab = 'corn daily returns')
ugfit_corn_returns <- ugarchfit(spec = ug_spec, data = corn_returns)
ug_var_corn_returns <- ugfit_corn_returns@fit$var
ug_vol_corn_returns <- sqrt(ug_var_corn_returns)
plot(x = corn$Dates[2:5217], y = ug_vol_corn_returns, type = 'l', xlab = 'dates', ylab = 'volatility', main = 'volatility of corn returns')

#soybeans#
soybeans <- readxl::read_xlsx('documents/dissertation/soybeans.xlsx')
plot(soybeans, type = 'l',ylab = 'prices', main = 'soybeans daily prices')
soybeans_returns <- diff(log(soybeans$PX_LAST))
plot(x = soybeans$Dates[2:5217], y = soybeans_returns, type = 'l', xlab = 'dates', ylab = 'soybeans daily returns')
ugfit_soybeans_returns <- ugarchfit(spec = ug_spec, data = soybeans_returns)
ug_var_soybeans_returns <- ugfit_soybeans_returns@fit$var
ug_vol_soybeans_returns <- sqrt(ug_var_soybeans_returns)
plot(x = soybeans$Dates[2:5217], y = ug_vol_soybeans_returns, type = 'l', xlab = 'dates', ylab = 'volatility', main = 'volatility of soybeans returns')

#wheat#
wheat <- readxl::read_xlsx('documents/dissertation/wheat.xlsx')
plot(wheat, type = 'l',ylab = 'prices', main = 'wheat daily prices')
wheat_returns <- diff(log(wheat$PX_LAST))
plot(x = wheat$Dates[2:5217], y = wheat_returns, type = 'l', xlab = 'dates', ylab = 'wheat daily returns')
ugfit_wheat_returns <- ugarchfit(spec = ug_spec, data = wheat_returns)
ug_var_wheat_returns <- ugfit_wheat_returns@fit$var
ug_vol_wheat_returns <- sqrt(ug_var_wheat_returns)
plot(x = wheat$Dates[2:5217], y = ug_vol_wheat_returns, type = 'l', xlab = 'dates', ylab = 'volatility', main = 'volatility of wheat returns')

#multivariate garch model#
ragricultural <- data.frame(crude_oil_returns, corn_returns, soybeans_returns, wheat_returns)
names(ragricultural)[1] <- 'crude oil returns'
names(ragricultural)[2] <- 'corn returns'
names(ragricultural)[3] <- 'soybeans returns'
names(ragricultural)[4] <- 'wheat returns'
uspec.4 <- multispec(replicate(4, ugarchspec(variance.model = list(model = 'gjrGARCH', garchOrder = c(1,1)), mean.model = list(armaOrder = c(1,1), include.mean = TRUE), distribution.model = 'norm')))
multif_agricultural <- multifit(uspec.4, ragricultural)
spec_agricultural <- dccspec(uspec = uspec.4, dccOrder = c(1,1), distribution = 'mvnorm')
fit_agricultural <- dccfit(spec_agricultural, data = ragricultural, fit.control = list(eval.se = TRUE), fit = multif_agricultural)
cor_agricultural <- rcor(fit_agricultural)
cor_crude_oil_corn <- cor_agricultural[1,2,]
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_corn, type = 'l', xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and corn')

cor_crude_oil_soybeans <- cor_agricultural[1,3,]
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_soybeans, type = 'l', xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and soybeans')

cor_crude_oil_wheat <- cor_agricultural[1,4,]
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_wheat, type = 'l', xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and wheat')

par(mfrow = c(2,2))
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_corn, type = 'l', xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and corn')
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_soybeans, type = 'l', xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and soybeans')
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_wheat, type = 'l', xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and wheat')
par(mfrow = c(1,1))

##forecasting##
dccforecast_agricultural <- dccforecast(fit_agricultural, n.ahead = 100)
correlation_forecast_agricultural <- dccforecast_agricultural@mforecast$R
str(correlation_forecast_agricultural)
correlation_forecast_crude_oil_corn <- correlation_forecast_agricultural[[1]][1,2,]
correlation_forecast_crude_oil_soybeans <- correlation_forecast_agricultural[[1]][1,3,]
correlation_forecast_crude_oil_wheat <- correlation_forecast_agricultural[[1]][1,4,]


tail_correlation_crude_oil_corn <- c(tail(cor_crude_oil_corn,200), rep(NA, 100))
correlation_forecast_crude_oil_corn_joint <- c(rep(NA, 200), correlation_forecast_crude_oil_corn)
plot(tail_correlation_crude_oil_corn, type = 'l', ylab = 'correlations', main = 'correlation forecasting crude oil and corn')
lines(correlation_forecast_crude_oil_corn_joint, type = 'l', col = 'red')

tail_correlation_crude_oil_soybeans <- c(tail(cor_crude_oil_soybeans,200), rep(NA, 100))
correlation_forecast_crude_oil_soybeans_joint <- c(rep(NA, 200), correlation_forecast_crude_oil_soybeans)
plot(tail_correlation_crude_oil_soybeans, type = 'l', ylab = 'correlations', main = 'correlation forecasting crude oil and soybeans')
lines(correlation_forecast_crude_oil_soybeans_joint, type = 'l', col = 'red')

tail_correlation_crude_oil_wheat <- c(tail(cor_crude_oil_wheat,200), rep(NA, 100))
correlation_forecast_crude_oil_wheat_joint <- c(rep(NA, 200), correlation_forecast_crude_oil_wheat)
plot(tail_correlation_crude_oil_wheat, type = 'l', ylab = 'correlations', main = 'correlation forecasting crude oil and wheat')
lines(correlation_forecast_crude_oil_wheat_joint, type = 'l', col = 'red')




################
##metal market##
################

##gold##
gold <- readxl::read_xls("Documents/dissertation/gold.xls")
plot(gold, type = 'l', ylab = 'prices', main = 'gold daily prices')
gold_returns <- diff(log(gold$PX_LAST))
plot(x = gold$Dates[2:5217], y = gold_returns, type = 'l', xlab = 'dates', ylab = 'gold daily returns')
ugfit_gold_returns <- ugarchfit(spec = ug_spec, data = gold_returns)
ug_var_gold_returns <- ugfit_gold_returns@fit$var
ug_vol_gold_returns <- sqrt(ug_var_gold_returns)
plot(x = gold$Dates[2:5217], y = ug_vol_gold_returns, type = 'l', xlab = 'dates', ylab = 'volatility', main = 'volatility of gold returns')

##silver##
silver <- readxl::read_xlsx('documents/dissertation/silver.xlsx')
plot(silver, type = 'l',ylab = 'prices', main = 'silver daily prices')
silver_returns <- diff(log(silver$PX_LAST))
plot(x = silver$Dates[2:5217], y = silver_returns, type = 'l', xlab = 'dates', ylab = 'silver daily returns')
ugfit_silver_returns <- ugarchfit(spec = ug_spec, data = silver_returns)
ug_var_silver_returns <- ugfit_silver_returns@fit$var
ug_vol_silver_returns <- sqrt(ug_var_silver_returns)
plot(x = silver$Dates[2:5217], y = ug_vol_silver_returns, type = 'l', xlab = 'dates', ylab = 'volatility', main = 'volatility of silver returns')

##copper##
copper <- readxl::read_xlsx('documents/dissertation/copper.xlsx')
plot(copper, type = 'l',ylab = 'prices', main = 'copper daily prices')
copper_returns <- diff(log(copper$PX_LAST))
plot(x = copper$Dates[2:5217], y = copper_returns, type = 'l', xlab = 'dates', ylab = 'copper daily returns')
ugfit_copper_returns <- ugarchfit(spec = ug_spec, data = copper_returns)
ug_var_copper_returns <- ugfit_copper_returns@fit$var
ug_vol_copper_returns <- sqrt(ug_var_copper_returns)
plot(x = copper$Dates[2:5217], y = ug_vol_copper_returns, type = 'l', xlab = 'dates', ylab = 'volatility', main = 'volatility of copper returns')

#multivariate garch model#
rmetal <- data.frame(crude_oil_returns, gold_returns, silver_returns, copper_returns)
names(rmetal)[1] <- 'crude oil returns'
names(rmetal)[2] <- 'gold returns'
names(rmetal)[3] <- 'silver returns'
names(rmetal)[4] <- 'copper returns'
multif_metal <- multifit(uspec.4, rmetal)
spec_metal <- dccspec(uspec = uspec.4, dccOrder = c(1,1), distribution = 'mvnorm')
fit_metal <- dccfit(spec_metal, data = rmetal, fit.control = list(eval.se = TRUE), fit = multif_metal)
cor_metal <- rcor(fit_metal)
cor_crude_oil_gold <- cor_metal[1,2,]
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_gold, type = 'l', xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and gold')

cor_crude_oil_silver <- cor_metal[1,3,]
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_silver, type = 'l', xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and silver')

cor_crude_oil_copper <- cor_metal[1,4,]
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_copper, type = 'l', xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and copper')

par(mfrow = c(3,1))
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_gold, type = 'l', xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and gold')
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_silver, type = 'l', xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and silver')
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_copper, type = 'l', xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and copper')
par(mfrow = c(1,1))

##forecasting##
dccforecast_metal <- dccforecast(fit_metal, n.ahead = 100)
correlation_forecast_metal <- dccforecast_metal@mforecast$R
str(correlation_forecast_metal)
correlation_forecast_crude_oil_gold <- correlation_forecast_metal[[1]][1,2,]
correlation_forecast_crude_oil_silver <- correlation_forecast_metal[[1]][1,3,]
correlation_forecast_crude_oil_copper <- correlation_forecast_metal[[1]][1,4,]

tail_correlation_crude_oil_gold <- c(tail(cor_crude_oil_gold,200), rep(NA, 100))
correlation_forecast_crude_oil_gold_joint <- c(rep(NA, 200), correlation_forecast_crude_oil_gold)
plot(tail_correlation_crude_oil_gold, type = 'l', ylab = 'correlations', main = 'correlation forecasting crude oil and gold')
lines(correlation_forecast_crude_oil_gold_joint, type = 'l', col = 'red')

tail_correlation_crude_oil_silver <- c(tail(cor_crude_oil_silver,200), rep(NA, 100))
correlation_forecast_crude_oil_silver_joint <- c(rep(NA, 200), correlation_forecast_crude_oil_silver)
plot(tail_correlation_crude_oil_silver, type = 'l', ylab = 'correlations', main = 'correlation forecasting crude oil and silver')
lines(correlation_forecast_crude_oil_silver_joint, type = 'l', col = 'red')

tail_correlation_crude_oil_copper <- c(tail(cor_crude_oil_copper,200), rep(NA, 100))
correlation_forecast_crude_oil_copper_joint <- c(rep(NA, 200), correlation_forecast_crude_oil_copper)
plot(tail_correlation_crude_oil_copper, type = 'l', ylab = 'correlations', main = 'correlation forecasting crude oil and copper')
lines(correlation_forecast_crude_oil_copper_joint, type = 'l', col = 'red')

####################
##livestock market##
####################

##lean hogs##
lean_hogs <- readxl::read_xlsx('documents/dissertation/lean hogs.xlsx')
plot(lean_hogs, type = 'l',ylab = 'prices', main = 'lean hogs daily prices')
lean_hogs_returns <- diff(log(lean_hogs$PX_LAST))
plot(x = lean_hogs$Dates[2:5217], y = lean_hogs_returns, type = 'l', xlab = 'dates', ylab = 'lean_hogs daily returns')
ugfit_lean_hogs_returns <- ugarchfit(spec = ug_spec, data = lean_hogs_returns)
ug_var_lean_hogs_returns <- ugfit_lean_hogs_returns@fit$var
ug_vol_lean_hogs_returns <- sqrt(ug_var_lean_hogs_returns)
plot(x = lean_hogs$Dates[2:5217], y = ug_vol_lean_hogs_returns, type = 'l', xlab = 'dates', ylab = 'volatility', main = 'volatility of lean hogs returns')

##feeder cattle##
feeder_cattle <- readxl::read_xlsx('documents/dissertation/feeder cattle.xlsx')
plot(feeder_cattle, type = 'l',ylab = 'prices', main = 'feeder cattle daily prices')
feeder_cattle_returns <- diff(log(feeder_cattle$PX_LAST))
plot(x = feeder_cattle$Dates[2:5217], y = feeder_cattle_returns, type = 'l', xlab = 'dates', ylab = 'feeder cattle daily returns')
ugfit_feeder_cattle_returns <- ugarchfit(spec = ug_spec, data = feeder_cattle_returns)
ug_var_feeder_cattle_returns <- ugfit_feeder_cattle_returns@fit$var
ug_vol_feeder_cattle_returns <- sqrt(ug_var_feeder_cattle_returns)
plot(x = feeder_cattle$Dates[2:5217], y = ug_vol_feeder_cattle_returns, type = 'l', xlab = 'dates', ylab = 'volatility', main = 'volatility of feeder cattle returns')

##live cattle##
live_cattle <- readxl::read_xlsx('documents/dissertation/live cattle.xlsx')
plot(live_cattle, type = 'l',ylab = 'prices', main = 'live cattle daily prices')
live_cattle_returns <- diff(log(live_cattle$PX_LAST))
plot(x = live_cattle$Dates[2:5217], y = live_cattle_returns, type = 'l', xlab = 'dates', ylab = 'live cattle daily returns')
ugfit_live_cattle_returns <- ugarchfit(spec = ug_spec, data = live_cattle_returns)
ug_var_live_cattle_returns <- ugfit_live_cattle_returns@fit$var
ug_vol_live_cattle_returns <- sqrt(ug_var_live_cattle_returns)
plot(x = live_cattle$Dates[2:5217], y = ug_vol_live_cattle_returns, type = 'l', xlab = 'dates', ylab = 'volatility', main = 'volatility of live_cattle returns')

#multivariate garch model#
rlivestock <- data.frame(crude_oil_returns, lean_hogs_returns, feeder_cattle_returns, live_cattle_returns)
names(rlivestock)[1] <- 'crude oil returns'
names(rlivestock)[2] <- 'lean hogs returns'
names(rlivestock)[3] <- 'feeder cattle returns'
names(rlivestock)[4] <- 'live cattle returns'
multif_livestock <- multifit(uspec.4, rlivestock)
spec_livestock <- dccspec(uspec = uspec.4, dccOrder = c(1,1), distribution = 'mvnorm')
fit_livestock <- dccfit(spec_livestock, data = rlivestock, fit.control = list(eval.se = TRUE), fit = multif_livestock)
cor_livestock <- rcor(fit_livestock)
cor_crude_oil_lean_hogs <- cor_livestock[1,2,]
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_lean_hogs, type = 'l', xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and lean hogs')

cor_crude_oil_feeder_cattle <- cor_livestock[1,3,]
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_feeder_cattle, type = 'l', xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and feeder cattle')

cor_crude_oil_live_cattle <- cor_livestock[1,4,]
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_live_cattle, type = 'l', xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and live cattle')

par(mfrow = c(3,1))
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_lean_hogs, type = 'l', xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and lean hogs')
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_feeder_cattle, type = 'l', xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and feeder cattle')
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_live_cattle, type = 'l', xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and live cattle')
par(mfrow = c(1,1))

##forecasting##
dccforecast_livestock <- dccforecast(fit_livestock, n.ahead = 100)
correlation_forecast_livestock <- dccforecast_livestock@mforecast$R
str(correlation_forecast_livestock)
correlation_forecast_crude_oil_lean_hogs <- correlation_forecast_livestock[[1]][1,2,]
correlation_forecast_crude_oil_feeder_cattle <- correlation_forecast_livestock[[1]][1,3,]
correlation_forecast_crude_oil_live_cattle <- correlation_forecast_livestock[[1]][1,4,]

tail_correlation_crude_oil_lean_hogs <- c(tail(cor_crude_oil_lean_hogs,200), rep(NA, 100))
correlation_forecast_crude_oil_lean_hogs_joint <- c(rep(NA, 200), correlation_forecast_crude_oil_lean_hogs)
plot(tail_correlation_crude_oil_lean_hogs, type = 'l', ylab = 'correlations', main = 'correlation forecasting crude oil and lean hogs')
lines(correlation_forecast_crude_oil_lean_hogs_joint, type = 'l', col = 'red')

tail_correlation_crude_oil_feeder_cattle <- c(tail(cor_crude_oil_feeder_cattle,200), rep(NA, 100))
correlation_forecast_crude_oil_feeder_cattle_joint <- c(rep(NA, 200), correlation_forecast_crude_oil_feeder_cattle)
plot(tail_correlation_crude_oil_feeder_cattle, type = 'l', ylab = 'correlations', main = 'correlation forecasting crude oil and feeder cattle')
lines(correlation_forecast_crude_oil_feeder_cattle_joint, type = 'l', col = 'red')

tail_correlation_crude_oil_live_cattle <- c(tail(cor_crude_oil_live_cattle,200), rep(NA, 100))
correlation_forecast_crude_oil_live_cattle_joint <- c(rep(NA, 200), correlation_forecast_crude_oil_live_cattle)
plot(tail_correlation_crude_oil_live_cattle, type = 'l', ylab = 'correlations', main = 'correlation forecasting crude oil and live cattle')
lines(correlation_forecast_crude_oil_live_cattle_joint, type = 'l', col = 'red')

#################
##energy market##
#################

#natural gas#
natural_gas <- readxl::read_xlsx('documents/dissertation/natural gas.xlsx')
plot(natural_gas, type = 'l',ylab = 'prices', main = 'natural gas daily prices')
natural_gas_returns <- diff(log(natural_gas$PX_LAST))
plot(x = natural_gas$Dates[2:5217], y = natural_gas_returns, type = 'l', xlab = 'dates', ylab = 'natural gas daily returns')
ugfit_natural_gas_returns <- ugarchfit(spec = ug_spec, data = natural_gas_returns)
ug_var_natural_gas_returns <- ugfit_natural_gas_returns@fit$var
ug_vol_natural_gas_returns <- sqrt(ug_var_natural_gas_returns)
plot(x = natural_gas$Dates[2:5217], y = ug_vol_natural_gas_returns, type = 'l', xlab = 'dates', ylab = 'volatility', main = 'volatility of natural gas returns')

##heating oil##
heating_oil <- readxl::read_xlsx('documents/dissertation/heating oil.xlsx')
plot(heating_oil, type = 'l',ylab = 'prices', main = 'heating oil daily prices')
heating_oil_returns <- diff(log(heating_oil$PX_LAST))
plot(x = heating_oil$Dates[2:5217], y = heating_oil_returns, type = 'l', xlab = 'dates', ylab = 'heating oil daily returns')
ugfit_heating_oil_returns <- ugarchfit(spec = ug_spec, data = heating_oil_returns)
ug_var_heating_oil_returns <- ugfit_heating_oil_returns@fit$var
ug_vol_heating_oil_returns <- sqrt(ug_var_heating_oil_returns)
plot(x = heating_oil$Dates[2:5217], y = ug_vol_heating_oil_returns, type = 'l', xlab = 'dates', ylab = 'volatility', main = 'volatility of heating oil returns')

##gasoil##
gasoil <- readxl::read_xlsx('documents/dissertation/gasoil.xlsx')
plot(gasoil, type = 'l',ylab = 'prices', main = 'gasoil daily prices')
gasoil_returns <- diff(log(gasoil$PX_LAST))
plot(x = gasoil$Dates[2:5217], y = gasoil_returns, type = 'l', xlab = 'dates', ylab = 'gasoil daily returns')
ugfit_gasoil_returns <- ugarchfit(spec = ug_spec, data = gasoil_returns)
ug_var_gasoil_returns <- ugfit_gasoil_returns@fit$var
ug_vol_gasoil_returns <- sqrt(ug_var_gasoil_returns)
plot(x = gasoil$Dates[2:5217], y = ug_vol_gasoil_returns, type = 'l', xlab = 'dates', ylab = 'volatility', main = 'volatility of gasoil returns')

#multivariate garch model#
renergy <- data.frame(crude_oil_returns, natural_gas_returns, heating_oil_returns, gasoil_returns)
names(renergy)[1] <- 'crude oil returns'
names(renergy)[2] <- 'natural gas returns'
names(renergy)[3] <- 'heating oil returns'
names(renergy)[4] <- 'gasoil returns'
multif_energy <- multifit(uspec.4, renergy)
spec_energy <- dccspec(uspec = uspec.4, dccOrder = c(1,1), distribution = 'mvnorm')
fit_energy <- dccfit(spec_energy, data = renergy, fit.control = list(eval.se = TRUE), fit = multif_energy)
cor_energy <- rcor(fit_energy)
cor_crude_oil_natural_gas <- cor_energy[1,2,]
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_natural_gas, type = 'l', xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and natural gas')
lines(mean(cor_crude_oil_natural_gas), type = 'l')

cor_crude_oil_heating_oil <- cor_energy[1,3,]
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_heating_oil, type = 'l', xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and heating oil')

cor_crude_oil_gasoil <- cor_energy[1,4,]
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_gasoil, type = 'l',xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and gasoil')

par(mfrow = c(3,1))
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_natural_gas, type = 'l', xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and natural gas')
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_heating_oil, type = 'l', xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and heating oil')
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_gasoil, type = 'l',xlab = 'dates', ylab = 'correlations', main = 'correlations between crude oil and gasoil')
par(mfrow = c(1,1))

plot(renergy)

##forecasting##
dccforecast_energy <- dccforecast(fit_energy, n.ahead = 100)
correlation_forecast_energy <- dccforecast_energy@mforecast$R
str(correlation_forecast_energy)
correlation_forecast_crude_oil_natural_gas <- correlation_forecast_energy[[1]][1,2,]
correlation_forecast_crude_oil_heating_oil <- correlation_forecast_energy[[1]][1,3,]
correlation_forecast_crude_oil_gasoil <- correlation_forecast_energy[[1]][1,4,]

tail_correlation_crude_oil_natural_gas <- c(tail(cor_crude_oil_natural_gas,200), rep(NA, 100))
correlation_forecast_crude_oil_natural_gas_joint <- c(rep(NA, 200), correlation_forecast_crude_oil_natural_gas)
plot(tail_correlation_crude_oil_natural_gas, type = 'l', ylab = 'correlations', main = 'correlation forecasting crude oil and natural_gas')
lines(correlation_forecast_crude_oil_natural_gas_joint, type = 'l', col = 'red')

tail_correlation_crude_oil_heating_oil <- c(tail(cor_crude_oil_heating_oil,200), rep(NA, 100))
correlation_forecast_crude_oil_heating_oil_joint <- c(rep(NA, 200), correlation_forecast_crude_oil_heating_oil)
plot(tail_correlation_crude_oil_heating_oil, type = 'l', ylab = 'correlations', main = 'correlation forecasting crude oil and heating_oil')
lines(correlation_forecast_crude_oil_heating_oil_joint, type = 'l', col = 'red')

tail_correlation_crude_oil_gasoil <- c(tail(cor_crude_oil_gasoil,200), rep(NA, 100))
correlation_forecast_crude_oil_gasoil_joint <- c(rep(NA, 200), correlation_forecast_crude_oil_gasoil)
plot(tail_correlation_crude_oil_gasoil, type = 'l', ylab = 'correlations', main = 'correlation forecasting crude oil and gasoil')
lines(correlation_forecast_crude_oil_gasoil_joint, type = 'l', col = 'red')


##Figure 1##
par(mfrow = c(1,1))
#agricultural market#
plot(crude_oil, type = 'l', ylab = 'prices', main = 'crude oil & agricultural market daily prices')
par(new = TRUE)
plot(corn, type = 'l', col = 'red', ylab = 'prices', axes = FALSE)
par(new = TRUE)
plot(soybeans, type = 'l',col = 'green', ylab = 'prices',axes = FALSE)
par(new = TRUE)
plot(wheat, type = 'l',col = 'blue', ylab = 'prices',axes = FALSE)
legend(x = 'topleft',y = 100,cex = 0.7, legend = c('crude oil', 'corn', 'soybeans', 'wheat'), fill = c('black', 'red', 'green','blue'), lty = 1)

#metal market#
plot(crude_oil, type = 'l', ylab = 'prices', main = 'crude oil & metal market daily prices')
par(new = TRUE)
plot(gold, type = 'l', col = 'red', ylab = 'prices', axes = FALSE)
par(new = TRUE)
plot(silver, type = 'l',col = 'green', ylab = 'prices',axes = FALSE)
par(new = TRUE)
plot(copper, type = 'l',col = 'blue', ylab = 'prices',axes = FALSE)
legend(x = 'topleft',y = 100,cex = 0.7, legend = c('crude oil', 'gold', 'silver', 'copper'), fill = c('black', 'red', 'green','blue'), lty = 1)

#livestock market#
plot(crude_oil, type = 'l', ylab = 'prices', main = 'crude oil & livestock market daily prices')
par(new = TRUE)
plot(lean_hogs, type = 'l', col = 'red', ylab = 'prices', axes = FALSE)
par(new = TRUE)
plot(feeder_cattle, type = 'l',col = 'green', ylab = 'prices',axes = FALSE)
par(new = TRUE)
plot(live_cattle, type = 'l',col = 'blue', ylab = 'prices',axes = FALSE)
legend(x = 'topleft',y = 100,cex = 0.7, legend = c('crude oil', 'lean_hogs', 'feeder_cattle', 'live_cattle'), fill = c('black', 'red', 'green','blue'), lty = 1)

#energy market#
plot(crude_oil, type = 'l', ylab = 'prices', main = 'crude oil & energy market daily prices')
par(new = TRUE)
plot(natural_gas, type = 'l', col = 'red', ylab = 'prices', axes = FALSE)
par(new = TRUE)
plot(heating_oil, type = 'l',col = 'green', ylab = 'prices',axes = FALSE)
par(new = TRUE)
plot(gasoil, type = 'l',col = 'blue', ylab = 'prices',axes = FALSE)
legend(x = 'topleft',y = 100,cex = 0.7, legend = c('crude oil', 'natural_gas', 'heating_oil', 'gasoil'), fill = c('black', 'red', 'green','blue'), lty = 1)






temp <- data.frame(index(crude_oil$Dates), stack(as.data.frame(crude_oil$PX_LAST, corn$PX_LAST, soybeans$PX_LAST, wheat$PX_LAST)))


##figure 2##

plot(x = crude_oil$Dates[2:5217], y = crude_oil_returns, type = 'l', xlab = 'dates', ylab = 'crude oil daily returns')

#agricultural market#
par(mfrow = c(3,1))
plot(x = corn$Dates[2:5217], y = corn_returns, type = 'l', xlab = 'dates', ylab = 'corn daily returns', main = 'agricultural market')
plot(x = soybeans$Dates[2:5217], y = soybeans_returns, type = 'l', xlab = 'dates', ylab = 'soybeans daily returns')
plot(x = wheat$Dates[2:5217], y = wheat_returns, type = 'l', xlab = 'dates', ylab = 'wheat daily returns')

#metal market#
par(mfrow = c(3,1))
plot(x = gold$Dates[2:5217], y = gold_returns, type = 'l', xlab = 'dates', ylab = 'gold daily returns', main = 'metal market')
plot(x = silver$Dates[2:5217], y = silver_returns, type = 'l', xlab = 'dates', ylab = 'silver daily returns')
plot(x = copper$Dates[2:5217], y = copper_returns, type = 'l', xlab = 'dates', ylab = 'copper daily returns')

#livestock market#
par(mfrow = c(3,1))
plot(x = lean_hogs$Dates[2:5217], y = lean_hogs_returns, type = 'l', xlab = 'dates', ylab = 'lean_hogs daily returns', main = 'livestock market')
plot(x = feeder_cattle$Dates[2:5217], y = feeder_cattle_returns, type = 'l', xlab = 'dates', ylab = 'feeder_cattle daily returns')
plot(x = live_cattle$Dates[2:5217], y = live_cattle_returns, type = 'l', xlab = 'dates', ylab = 'live_cattle daily returns')

#energy market and crude oil#
par(mfrow = c(3,1))
plot(x = natural_gas$Dates[2:5217], y = natural_gas_returns, type = 'l', xlab = 'dates', ylab = 'natural_gas daily returns', main = 'energy market')
plot(x = heating_oil$Dates[2:5217], y = heating_oil_returns, type = 'l', xlab = 'dates', ylab = 'heating_oil daily returns')
plot(x = gasoil$Dates[2:5217], y = gasoil_returns, type = 'l', xlab = 'dates', ylab = 'gasoil daily returns')
plot(x = crude_oil$Dates[2:5217], y = crude_oil_returns, type = 'l', xlab = 'dates', ylab = 'crude oil daily returns', main = 'crude oil ')

#table 2#
ragricultural_matrix <- as.matrix(ragricultural)
                                
##figure 3##
#agricultural market#
par(mfrow = c(3,1))
plot(x = corn$Dates[2:5217], y = ug_vol_corn_returns, type = 'l', xlab = 'dates', ylab = 'volatility of corn returns', main = 'agricultural market')
plot(x = soybeans$Dates[2:5217], y = ug_vol_soybeans_returns, type = 'l', xlab = 'dates', ylab = 'volatility of soybeans returns')
plot(x = wheat$Dates[2:5217], y = ug_vol_wheat_returns, type = 'l', xlab = 'dates', ylab = 'volatility of wheat returns')

#metal market#
par(mfrow = c(3,1))
plot(x = gold$Dates[2:5217], y = ug_vol_gold_returns, type = 'l', xlab = 'dates', ylab = 'volatility of gold returns', main = 'metal market')
plot(x = silver$Dates[2:5217], y = ug_vol_silver_returns, type = 'l', xlab = 'dates', ylab = 'volatility of silver returns')
plot(x = copper$Dates[2:5217], y = ug_vol_copper_returns, type = 'l', xlab = 'dates', ylab = 'volatility of copper returns')

#liverstock market#
par(mfrow = c(3,1))
plot(x = lean_hogs$Dates[2:5217], y = ug_vol_lean_hogs_returns, type = 'l', xlab = 'dates', ylab = 'volatility of lean hogs returns', main = 'livestock market')
plot(x = feeder_cattle$Dates[2:5217], y = ug_vol_feeder_cattle_returns, type = 'l', xlab = 'dates', ylab = 'volatility of feeder cattle returns')
plot(x = live_cattle$Dates[2:5217], y = ug_vol_live_cattle_returns, type = 'l', xlab = 'dates', ylab = 'volatility of live cattle returns')

#energy market#
par(mfrow = c(3,1))
plot(x = natural_gas$Dates[2:5217], y = ug_vol_natural_gas_returns, type = 'l', xlab = 'dates', ylab = 'volatility of natural gas returns', main = 'energy market')
plot(x = heating_oil$Dates[2:5217], y = ug_vol_heating_oil_returns, type = 'l', xlab = 'dates', ylab = 'volatility of heating oil returns')
plot(x = gasoil$Dates[2:5217], y = ug_vol_gasoil_returns, type = 'l', xlab = 'dates', ylab = 'volatility of gasoil returns')
plot(x = crude_oil$Dates[2:5217], y = ug_vol_crude_oil_returns, type = 'l', xlab = 'dates', ylab = 'volatility of crude oil returns', main = 'crude oil daily volatility')


##figure 4##
#agricultural market#
par(mfrow = c(3,1))
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_corn, type = 'l', xlab = 'dates', ylab = 'correlations', sub = 'crude oil and corn', main = 'agricultural market')
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_soybeans, type = 'l', xlab = 'dates', ylab = 'correlations', sub = 'crude oil and soybeans')
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_wheat, type = 'l', xlab = 'dates', ylab = 'correlations', sub = 'crude oil and wheat')

#metal market#
par(mfrow = c(3,1))
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_gold, type = 'l', xlab = 'dates', ylab = 'correlations', sub = 'crude oil and gold', main = 'metal market')
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_silver, type = 'l', xlab = 'dates', ylab = 'correlations', sub = 'crude oil and silver')
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_copper, type = 'l', xlab = 'dates', ylab = 'correlations', sub = 'crude oil and copper')

#livestock market#
par(mfrow = c(3,1))
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_lean_hogs, type = 'l', xlab = 'dates', ylab = 'correlations', sub = 'crude oil and lean_hogs', main = 'livestock market')
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_feeder_cattle, type = 'l', xlab = 'dates', ylab = 'correlations', sub = 'crude oil and feeder_cattle')
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_live_cattle, type = 'l', xlab = 'dates', ylab = 'correlations', sub = 'crude oil and live_cattle')

#enegy market#
par(mfrow = c(3,1))
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_natural_gas, type = 'l', xlab = 'dates', ylab = 'correlations', sub = 'crude oil and natural_gas', main = 'energy market')
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_heating_oil, type = 'l', xlab = 'dates', ylab = 'correlations', sub = 'crude oil and heating_oil')
plot(x = crude_oil$Dates[2:5217], y = cor_crude_oil_gasoil, type = 'l', xlab = 'dates', ylab = 'correlations', sub = 'crude oil and gasoil')

##figure 5##
#agricultural market#
par(mfrow = c(3,1))
tail_correlation_crude_oil_corn <- c(tail(cor_crude_oil_corn,200), rep(NA, 100))
correlation_forecast_crude_oil_corn_joint <- c(rep(NA, 200), correlation_forecast_crude_oil_corn)
plot(tail_correlation_crude_oil_corn, type = 'l', ylab = 'correlations crude oil and corn', main = 'DCC forecasting-agricultural market')
lines(correlation_forecast_crude_oil_corn_joint, type = 'l', col = 'red')

tail_correlation_crude_oil_soybeans <- c(tail(cor_crude_oil_soybeans,200), rep(NA, 100))
correlation_forecast_crude_oil_soybeans_joint <- c(rep(NA, 200), correlation_forecast_crude_oil_soybeans)
plot(tail_correlation_crude_oil_soybeans, type = 'l', ylab = 'correlations crude oil and soybeans')
lines(correlation_forecast_crude_oil_soybeans_joint, type = 'l', col = 'red')

tail_correlation_crude_oil_wheat <- c(tail(cor_crude_oil_wheat,200), rep(NA, 100))
correlation_forecast_crude_oil_wheat_joint <- c(rep(NA, 200), correlation_forecast_crude_oil_wheat)
plot(tail_correlation_crude_oil_wheat, type = 'l', ylab = 'correlations crude oil and wheat')
lines(correlation_forecast_crude_oil_wheat_joint, type = 'l', col = 'red')

#metal market#
tail_correlation_crude_oil_gold <- c(tail(cor_crude_oil_gold,200), rep(NA, 100))
correlation_forecast_crude_oil_gold_joint <- c(rep(NA, 200), correlation_forecast_crude_oil_gold)
plot(tail_correlation_crude_oil_gold, type = 'l', ylab = 'correlations crude oil and gold', main = 'DCC forecasting-metal market')
lines(correlation_forecast_crude_oil_gold_joint, type = 'l', col = 'red')

tail_correlation_crude_oil_silver <- c(tail(cor_crude_oil_silver,200), rep(NA, 100))
correlation_forecast_crude_oil_silver_joint <- c(rep(NA, 200), correlation_forecast_crude_oil_silver)
plot(tail_correlation_crude_oil_silver, type = 'l', ylab = 'correlations crude oil and silver')
lines(correlation_forecast_crude_oil_silver_joint, type = 'l', col = 'red')

tail_correlation_crude_oil_copper <- c(tail(cor_crude_oil_copper,200), rep(NA, 100))
correlation_forecast_crude_oil_copper_joint <- c(rep(NA, 200), correlation_forecast_crude_oil_copper)
plot(tail_correlation_crude_oil_copper, type = 'l', ylab = 'correlations crude oil and copper')
lines(correlation_forecast_crude_oil_copper_joint, type = 'l', col = 'red')


#livestock market#
tail_correlation_crude_oil_lean_hogs <- c(tail(cor_crude_oil_lean_hogs,200), rep(NA, 100))
correlation_forecast_crude_oil_lean_hogs_joint <- c(rep(NA, 200), correlation_forecast_crude_oil_lean_hogs)
plot(tail_correlation_crude_oil_lean_hogs, type = 'l', ylab = 'correlations crude oil&lean hogs', main = 'DCC forecasting-livestock market')
lines(correlation_forecast_crude_oil_lean_hogs_joint, type = 'l', col = 'red')

tail_correlation_crude_oil_feeder_cattle <- c(tail(cor_crude_oil_feeder_cattle,200), rep(NA, 100))
correlation_forecast_crude_oil_feeder_cattle_joint <- c(rep(NA, 200), correlation_forecast_crude_oil_feeder_cattle)
plot(tail_correlation_crude_oil_feeder_cattle, type = 'l', ylab = 'correlations crude oil&feeder cattle')
lines(correlation_forecast_crude_oil_feeder_cattle_joint, type = 'l', col = 'red')

tail_correlation_crude_oil_live_cattle <- c(tail(cor_crude_oil_live_cattle,200), rep(NA, 100))
correlation_forecast_crude_oil_live_cattle_joint <- c(rep(NA, 200), correlation_forecast_crude_oil_live_cattle)
plot(tail_correlation_crude_oil_live_cattle, type = 'l', ylab = 'correlations crude oil&live cattle')
lines(correlation_forecast_crude_oil_live_cattle_joint, type = 'l', col = 'red')

#energy market#
tail_correlation_crude_oil_natural_gas <- c(tail(cor_crude_oil_natural_gas,200), rep(NA, 100))
correlation_forecast_crude_oil_natural_gas_joint <- c(rep(NA, 200), correlation_forecast_crude_oil_natural_gas)
plot(tail_correlation_crude_oil_natural_gas, type = 'l', ylab = 'correlations crude oil&natural gas', main = 'DCC forecasting-energy market')
lines(correlation_forecast_crude_oil_natural_gas_joint, type = 'l', col = 'red')

tail_correlation_crude_oil_heating_oil <- c(tail(cor_crude_oil_heating_oil,200), rep(NA, 100))
correlation_forecast_crude_oil_heating_oil_joint <- c(rep(NA, 200), correlation_forecast_crude_oil_heating_oil)
plot(tail_correlation_crude_oil_heating_oil, type = 'l', ylab = 'correlations crude oil&heating oil')
lines(correlation_forecast_crude_oil_heating_oil_joint, type = 'l', col = 'red')

tail_correlation_crude_oil_gasoil <- c(tail(cor_crude_oil_gasoil,200), rep(NA, 100))
correlation_forecast_crude_oil_gasoil_joint <- c(rep(NA, 200), correlation_forecast_crude_oil_gasoil)
plot(tail_correlation_crude_oil_gasoil, type = 'l', ylab = 'correlations crude oil&gasoil')
lines(correlation_forecast_crude_oil_gasoil_joint, type = 'l', col = 'red')










