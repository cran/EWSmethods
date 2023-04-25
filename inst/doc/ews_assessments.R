## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")


## ---- message=FALSE-----------------------------------------------------------
set.seed(123) #to ensure reproducible data

library(EWSmethods)

## ----load_data, eval=T--------------------------------------------------------
#Load the two datasets in to the session
data("simTransComms")

data("CODrecovery")

## ----vis_data,dpi=144---------------------------------------------------------
matplot(simTransComms$community1[,3:7], type = "l", xlab = "Time", ylab = "Density",  main = "Transitioning five species community")

plot(x = CODrecovery$scenario2$time, y = CODrecovery$scenario2$biomass, type = "l", xlab = "Year", ylab = "Abundance", main = "Recovering cod population")

## ----inflec_data,echo=F,fig.align='center'------------------------------------
knitr::kable(data.frame("simTransComms" = simTransComms$community1$inflection_pt[1],
"CODrecovery" = 
CODrecovery$scenario2$inflection_pt[1]))

## ----trunc_data---------------------------------------------------------------
pre_simTransComms <- subset(simTransComms$community1,time < inflection_pt)

pre_CODrecovery <- subset(CODrecovery$scenario2,time < inflection_pt)


## ----figure, echo=F,fig.cap = "Rolling (A) vs expanding (C) window concept diagram. Panels B and D introduce the circumstance when a 'warning' is signalled in both approaches"----
knitr::include_graphics("ewsmethods_eg_fig.png", dpi = 144)

## ----rolling_ews--------------------------------------------------------------
rolling_ews_eg <- uniEWS(data = pre_simTransComms[,c(2,5)],
                         metrics = c("ar1","SD","skew"),
                         method = "rolling",
                         winsize = 50)

## ----rolling_ews_fig,dpi=144,fig.height=6-------------------------------------
plot(rolling_ews_eg,  y_lab = "Density")

## ----expanding_ews,fig.keep = "none"------------------------------------------
expanding_ews_eg <- uniEWS(data = pre_simTransComms[,c(2,5)],
                         metrics = c("ar1","SD","skew"),
                         method = "expanding",
                         burn_in = 50,
                         threshold = 2)

## ----expanding_ews_fig,dpi=144,fig.height=6-----------------------------------
plot(expanding_ews_eg, y_lab = "Density")

## ----trait_ews,fig.keep = "none"----------------------------------------------
trait_ews_eg <- uniEWS(data = pre_CODrecovery[,c(2,3)],
                         metrics = c("ar1","SD","trait"), #note "trait" is provided here
                         method = "expanding",
                         trait = pre_CODrecovery$mean.size, #and here
                         burn_in = 15, #small burn_in due to shorter time series
                         threshold = 2)

## ----trait_ews_fig,dpi=144----------------------------------------------------
plot(trait_ews_eg, y_lab = "Density", trait_lab = "Mean size (g)")

## ----multi_ews, fig.keep = "none"---------------------------------------------
multi_ews_eg <- multiEWS(data = pre_simTransComms[,2:7],
                         metrics = c("meanAR","maxAR","meanSD","maxSD","eigenMAF","mafAR","mafSD","pcaAR","pcaSD","eigenCOV","maxCOV","mutINFO"),
                         method = "rolling",
                         winsize = 50)

## ----multi_ews_fig,dpi=144,fig.height=6---------------------------------------
plot(multi_ews_eg)

## ----multi_ews2, fig.keep = "none"--------------------------------------------
multi_ews_eg2 <- multiEWS(data = pre_simTransComms[,2:7],
                         method = "expanding",
                         burn_in = 50,
                         threshold = 2)

## ----multi_ews2_fig,dpi=144,fig.height=6--------------------------------------
plot(multi_ews_eg2)

## ----detrend,dpi=144----------------------------------------------------------
detrend_dat <- detrend_ts(data =  pre_simTransComms[,2:7],  method = "loess", span = 0.75, degree = 2)

matplot(x = detrend_dat$time, y = detrend_dat[,2:6], type = "l", xlab = "Date", ylab = "Density",  main = "LOESS detrended five species community")

## ----create_seasonal,dpi=144--------------------------------------------------
spp_data <- matrix(nrow = 5*12, ncol = 5)

seasonal_cycle <-  20*sin(2*pi*(1:5*12)/12)

spp_data <- sapply(1:dim(spp_data)[2], function(x){

  spp_data[,x] <- ts(rnorm(5*12,mean = 20, sd = 3) + seasonal_cycle, freq = 12, start = c(2000, 1)) #add seasonal cycle to random noise
  
  })

multi_spp_data <- cbind("time" = base::seq(base::as.Date('2000/01/01'), base::as.Date('2004/12/01'), by = "month"), as.data.frame(spp_data))
  
matplot(x = multi_spp_data$time, y = multi_spp_data[,2:6], type = "l", xlab = "Date", ylab = "Density",  main = "Seasonal five species community")


## ----deseason,dpi=144---------------------------------------------------------
deseas_dat <- deseason_ts(data = multi_spp_data, increment = "month", method = "average", order = "ymd")

matplot(x = deseas_dat$date, y = deseas_dat[,2:6], type = "l", xlab = "Date", ylab = "Density",  main = "Deseasoned five species community")

