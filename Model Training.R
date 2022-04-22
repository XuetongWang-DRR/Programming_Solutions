########Load all packages#############################################################
library(gamlss) #This package is used for modeling ZAGA distribution (parametric model)
library(dplyr) #This package is used for some data manipulation
library(scoringRules) #This package is used for simulated forecast distributions by applying scoring rules, and for calculate CRPS
library(quantregForest) #This package is used for modeling QRF (nonparametric model)
library(lubridate) #This package is used for transfer time related information to a easy way
library(verification) #This package is used for calculate brier score and make reliability plot
library(ggplot2) #This package is used for making graphs
library(ggpubr)  #This package is used for arranging graphs madde by ggplot2
########Reading Data####################################################################
final = read.table("E:/output/new_model/24hours/24hour_forecast_alldata.txt", sep = "\t", header = T)
final = na.omit(final)
final$R4final1 = as.numeric(final$R4final1)
########Separate data to training and testing group##################################################
finaltest = final[164127:371380,] #Those are rows of the data for the second year, and they are used for testing
finaltrain = setdiff(final,finaltest) #Those are rows of the data for the first year and the third year, and they are used for training
#######Parametric model (ensemble model output statistics) with ZAGA distribution##############################################################
#Model training
mod0<-gamlss(R4final1~1, data=finaltrain, family=ZAGA)
mod1 = stepGAICAll.A(mod0, scope=list(lower=~1,upper=~RRmeanfinal1+RRsdfinal1+ RRmedianfinal1+ RRq10final1+RRq25final1+RRq75final1+RRq90final1+ 
                                        RRP0.05final1+RRP0.3final1+RRP10final1+RRP20final1+RRminfinal1+RRmaxfinal1+RRHRESfinal1+
                                        CONRRmeanfinal1+CONRRsdfinal1+CONRRmedianfinal1+CONRRq10final1+CONRRq25final1+
                                        CONRRq75final1+CONRRq90final1+CONRRP0.05final1+CONRRP0.3final1+CONRRP10final1+
                                        CONRRP20final1+CONRRminfinal1+CONRRmaxfinal1+CONRRHRESfinal1+
                                        WSPDmeanfinal1+WSPDsdfinal1+WSPDmedianfinal1+WSPDq10final1+WSPDq25final1+WSPDq75final1+
                                        WSPDq90final1+WSPDminfinal1+WSPDmaxfinal1+WSPDHRESfinal1+
                                        DPTmeanfinal1+DPTsdfinal1+DPTmedianfinal1+DPTq10final1+DPTq25final1+DPTq75final1+
                                        DPTq90final1+DPTminfinal1+DPTmaxfinal1+DPTHRESfinal1+
                                        T2Mmeanfinal1+T2Msdfinal1+T2Mmedianfinal1+T2Mq10final1+T2Mq25final1+T2Mq75final1+
                                        T2Mq90final1+T2Mminfinal1+T2Mmaxfinal1+T2MHRESfinal1+
                                        EVAmeanfinal1+EVAsdfinal1+EVAmedianfinal1+EVAq10final1+EVAq25final1+EVAq75final1+
                                        EVAq90final1+EVAminfinal1+EVAmaxfinal1+EVAHRESfinal1+
                                        CCmeanfinal1+CCsdfinal1+CCmedianfinal1+CCq10final1+CCq25final1+CCq75final1+
                                        CCq90final1+CCminfinal1+CCmaxfinal1+CCHRESfinal1+
                                        CAPEmeanfinal1+CAPEsdfinal1+CAPEmedianfinal1+CAPEq10final1+CAPEq25final1+CAPEq75final1+
                                        CAPEq90final1+CAPEminfinal1+CAPEmaxfinal1+CAPEHRESfinal1+
                                        CAPESmeanfinal1+CAPESsdfinal1+CAPESmedianfinal1+CAPESq10final1+CAPESq25final1+CAPESq75final1+
                                        CAPESq90final1+CAPESminfinal1+CAPESmaxfinal1+CAPESHRESfinal1), 
                     sigma.scope=list(lower=~1,upper=~RRmeanfinal1+RRsdfinal1+ RRmedianfinal1+ RRq10final1+RRq25final1+RRq75final1+RRq90final1+ 
                                        RRP0.05final1+RRP0.3final1+RRP10final1+RRP20final1+RRminfinal1+RRmaxfinal1+RRHRESfinal1+
                                        CONRRmeanfinal1+CONRRsdfinal1+CONRRmedianfinal1+CONRRq10final1+CONRRq25final1+
                                        CONRRq75final1+CONRRq90final1+CONRRP0.05final1+CONRRP0.3final1+CONRRP10final1+
                                        CONRRP20final1+CONRRminfinal1+CONRRmaxfinal1+CONRRHRESfinal1+
                                        WSPDmeanfinal1+WSPDsdfinal1+WSPDmedianfinal1+WSPDq10final1+WSPDq25final1+WSPDq75final1+
                                        WSPDq90final1+WSPDminfinal1+WSPDmaxfinal1+WSPDHRESfinal1+
                                        DPTmeanfinal1+DPTsdfinal1+DPTmedianfinal1+DPTq10final1+DPTq25final1+DPTq75final1+
                                        DPTq90final1+DPTminfinal1+DPTmaxfinal1+DPTHRESfinal1+
                                        T2Mmeanfinal1+T2Msdfinal1+T2Mmedianfinal1+T2Mq10final1+T2Mq25final1+T2Mq75final1+
                                        T2Mq90final1+T2Mminfinal1+T2Mmaxfinal1+T2MHRESfinal1+
                                        EVAmeanfinal1+EVAsdfinal1+EVAmedianfinal1+EVAq10final1+EVAq25final1+EVAq75final1+
                                        EVAq90final1+EVAminfinal1+EVAmaxfinal1+EVAHRESfinal1+
                                        CCmeanfinal1+CCsdfinal1+CCmedianfinal1+CCq10final1+CCq25final1+CCq75final1+
                                        CCq90final1+CCminfinal1+CCmaxfinal1+CCHRESfinal1+
                                        CAPEmeanfinal1+CAPEsdfinal1+CAPEmedianfinal1+CAPEq10final1+CAPEq25final1+CAPEq75final1+
                                        CAPEq90final1+CAPEminfinal1+CAPEmaxfinal1+CAPEHRESfinal1+
                                        CAPESmeanfinal1+CAPESsdfinal1+CAPESmedianfinal1+CAPESq10final1+CAPESq25final1+CAPESq75final1+
                                        CAPESq90final1+CAPESminfinal1+CAPESmaxfinal1+CAPESHRESfinal1), 
                     nu.scope=list(lower=~1,upper=~RRmeanfinal1+RRsdfinal1+ RRmedianfinal1+ RRq10final1+RRq25final1+RRq75final1+RRq90final1+ 
                                     RRP0.05final1+RRP0.3final1+RRP10final1+RRP20final1+RRminfinal1+RRmaxfinal1+RRHRESfinal1+
                                     CONRRmeanfinal1+CONRRsdfinal1+CONRRmedianfinal1+CONRRq10final1+CONRRq25final1+
                                     CONRRq75final1+CONRRq90final1+CONRRP0.05final1+CONRRP0.3final1+CONRRP10final1+
                                     CONRRP20final1+CONRRminfinal1+CONRRmaxfinal1+CONRRHRESfinal1+
                                     WSPDmeanfinal1+WSPDsdfinal1+WSPDmedianfinal1+WSPDq10final1+WSPDq25final1+WSPDq75final1+
                                     WSPDq90final1+WSPDminfinal1+WSPDmaxfinal1+WSPDHRESfinal1+
                                     DPTmeanfinal1+DPTsdfinal1+DPTmedianfinal1+DPTq10final1+DPTq25final1+DPTq75final1+
                                     DPTq90final1+DPTminfinal1+DPTmaxfinal1+DPTHRESfinal1+
                                     T2Mmeanfinal1+T2Msdfinal1+T2Mmedianfinal1+T2Mq10final1+T2Mq25final1+T2Mq75final1+
                                     T2Mq90final1+T2Mminfinal1+T2Mmaxfinal1+T2MHRESfinal1+
                                     EVAmeanfinal1+EVAsdfinal1+EVAmedianfinal1+EVAq10final1+EVAq25final1+EVAq75final1+
                                     EVAq90final1+EVAminfinal1+EVAmaxfinal1+EVAHRESfinal1+
                                     CCmeanfinal1+CCsdfinal1+CCmedianfinal1+CCq10final1+CCq25final1+CCq75final1+
                                     CCq90final1+CCminfinal1+CCmaxfinal1+CCHRESfinal1+
                                     CAPEmeanfinal1+CAPEsdfinal1+CAPEmedianfinal1+CAPEq10final1+CAPEq25final1+CAPEq75final1+
                                     CAPEq90final1+CAPEminfinal1+CAPEmaxfinal1+CAPEHRESfinal1+
                                     CAPESmeanfinal1+CAPESsdfinal1+CAPESmedianfinal1+CAPESq10final1+CAPESq25final1+CAPESq75final1+
                                     CAPESq90final1+CAPESminfinal1+CAPESmaxfinal1+CAPESHRESfinal1),
                     #direction = "forward",
                     steps = 3) #Selected three predictors for each parameter (mu,sigma,nu)
#Using the trained model to predict results
prd  <- predictAll(mod1,newdata=finaltest)
prob <- 1:51 / 52
new_data = as.data.frame(matrix(NA,nrow = 51, ncol = dim(finaltest)[1]))
#Calculate values for different quantiles
for (i in 1:dim(finaltest)[1]){
  new_data[,i]=sapply(prob, function(u) qZAGA(u, mu=prd$mu[i], sigma=prd$sigma[i], nu=prd$nu[i]))
}
new_data = as.data.frame(t(new_data))
new_data$observation = finaltest$R4final1
#Calculate CRPS and CRPSS
new_data$crps=apply(new_data,1,function(x) crps_sample(as.numeric(x['observation']),as.numeric(x[1:51]))) 
clim=new_data$observation
new_data$crps_clim=apply(new_data,1,function(x) crps_sample(as.numeric(x['observation']),clim))
new_data$crpss=(mean(new_data$crps)-mean(new_data$crps_clim))/(-mean(new_data$crps_clim))
new_data = new_data[,53:55]
#write.table(new_data, "crps_24h_3steps.txt", sep = "\t", col.names = T, row.names = F)
#Making CDF dataframe for multiple thresholds
CDF = as.data.frame(matrix(NA,nrow = 15, ncol = dim(finaltest)[1]))
for (i in 1:dim(finaltest)[1]){
  CDF[1,i]=sapply(0.05, function(u) pZAGA(u, mu=prd$mu[i], sigma=prd$sigma[i], nu=prd$nu[i]))
  CDF[2,i]=sapply(0.1, function(u) pZAGA(u, mu=prd$mu[i], sigma=prd$sigma[i], nu=prd$nu[i]))
  CDF[3,i]=sapply(0.2, function(u) pZAGA(u, mu=prd$mu[i], sigma=prd$sigma[i], nu=prd$nu[i]))
  CDF[4,i]=sapply(0.5, function(u) pZAGA(u, mu=prd$mu[i], sigma=prd$sigma[i], nu=prd$nu[i]))
  CDF[5,i]=sapply(0.8, function(u) pZAGA(u, mu=prd$mu[i], sigma=prd$sigma[i], nu=prd$nu[i]))
  CDF[6,i]=sapply(1, function(u) pZAGA(u, mu=prd$mu[i], sigma=prd$sigma[i], nu=prd$nu[i]))
  CDF[7,i]=sapply(2, function(u) pZAGA(u, mu=prd$mu[i], sigma=prd$sigma[i], nu=prd$nu[i]))
  CDF[8,i]=sapply(3, function(u) pZAGA(u, mu=prd$mu[i], sigma=prd$sigma[i], nu=prd$nu[i]))
  CDF[9,i]=sapply(5, function(u) pZAGA(u, mu=prd$mu[i], sigma=prd$sigma[i], nu=prd$nu[i]))
  CDF[10,i]=sapply(7.5, function(u) pZAGA(u, mu=prd$mu[i], sigma=prd$sigma[i], nu=prd$nu[i]))
  CDF[11,i]=sapply(10, function(u) pZAGA(u, mu=prd$mu[i], sigma=prd$sigma[i], nu=prd$nu[i]))
  CDF[12,i]=sapply(12.5, function(u) pZAGA(u, mu=prd$mu[i], sigma=prd$sigma[i], nu=prd$nu[i]))
  CDF[13,i]=sapply(15, function(u) pZAGA(u, mu=prd$mu[i], sigma=prd$sigma[i], nu=prd$nu[i]))
  CDF[14,i]=sapply(17.5, function(u) pZAGA(u, mu=prd$mu[i], sigma=prd$sigma[i], nu=prd$nu[i]))
  CDF[15,i]=sapply(20, function(u) pZAGA(u, mu=prd$mu[i], sigma=prd$sigma[i], nu=prd$nu[i]))
}
CDF = as.data.frame(t(CDF))
#write.table(CDF, "NCDF_24hour_3steps.txt", sep = "\t", col.names = T, row.names = F)
############Non-parametric modeling with quantile regression forest##########################################
#Train with qrf model
qrF_model <- quantregForest(x = finaltrain[, !(names(finaltrain) %in% c("R4final1", "EC4time1", "EC4ID1","R4ID1"))], 
                            y = finaltrain$R4final1,
                            nodesize = 5,
                            mtry = 3,
                            ntree = 500)
#Predict values with different quantiles (prob)
qrF_prediction <-   predict(qrF_model,
                            newdata = finaltest[, !(names(finaltest) %in% c("R4final1", "EC4time1", "EC4ID1","R4ID1"))],
                            what = prob,
                            all = TRUE)
qrF_prediction = as.data.frame(qrF_prediction)
colnames(qrF_prediction) = c(1:51)
qrF_prediction$observation = finaltest$R4final1

#Calculate CRPS and CRPSS
qrF_prediction$crps=apply(qrF_prediction,1,function(x) crps_sample(as.numeric(x['observation']),as.numeric(x[1:51]))) 
clim=qrF_prediction$observation
qrF_prediction$crps_clim=apply(qrF_prediction,1,function(x) crps_sample(as.numeric(x['observation']),clim))
qrF_prediction$crpss=(mean(qrF_prediction$crps)-mean(qrF_prediction$crps_clim))/(-mean(qrF_prediction$crps_clim))
qrF_prediction = qrF_prediction[,53:55]
#write.table(qrF_prediction, "crps_24h_ALLsample_5_3_500.txt", sep = "\t", col.names = T, row.names = F)
#Making CDF dataframe and ccalculate probabilities for multiple thresholds
condEcdf <-   predict(qrF_model,
                      newdata = finaltest[, !(names(finaltest) %in% c("R4final1", "EC4time1", "EC4ID1","R4ID1"))],
                      what = ecdf,
                      all = TRUE)

CDF = as.data.frame(matrix(NA,nrow = dim(finaltest)[1], ncol = 15))
for (i in 1:164126){
  CDF[i,1] = condEcdf[[i]](0.05)
  CDF[i,2] = condEcdf[[i]](0.1)
  CDF[i,3] = condEcdf[[i]](0.2)
  CDF[i,4] = condEcdf[[i]](0.5)
  CDF[i,5] = condEcdf[[i]](0.8)
  CDF[i,6] = condEcdf[[i]](1)
  CDF[i,7] = condEcdf[[i]](2)
  CDF[i,8] = condEcdf[[i]](3)
  CDF[i,9] = condEcdf[[i]](5)
  CDF[i,10] = condEcdf[[i]](7.5)
  CDF[i,11] = condEcdf[[i]](10)
  CDF[i,12] = condEcdf[[i]](12.5)
  CDF[i,13] = condEcdf[[i]](15)
  CDF[i,14] = condEcdf[[i]](17.5)
  CDF[i,15] = condEcdf[[i]](20)
}  
#write.table(CDF, "NCDF_24hour_ALLsample_5_3_500_qrf.txt", sep = "\t", col.names = T, row.names = F)
#########Check with original ensemble forecast##############################################
ensemble_24hours_1 = read.table("E:/output/new_model/24hours/ensemble_24hours.txt", sep = "\t", header = T)
finaltest = final[164127:371380,] 
finaltrain = setdiff(final,finaltest)
#Add labels to the original ensemble forecasts
colnames(finaltest) = c("final","dftime","grid","rID")
ensemble_24hours_1$grid = as.numeric(substr(ensemble_24hours_1$ensemble_24hours_0.second_category,12,15))
ensemble_24hours_1$dftime = as.POSIXct("2000-1-1 01:00:00", tz = "UTC")
year(ensemble_24hours_1$dftime) = as.numeric(substr(ensemble_24hours_1$ensemble_24hours_0.first_category,10,13))
month(ensemble_24hours_1$dftime) = as.numeric(substr(ensemble_24hours_1$ensemble_24hours_0.first_category,14,15))
mday(ensemble_24hours_1$dftime) = as.numeric(substr(ensemble_24hours_1$ensemble_24hours_0.first_category,16,17))
hour(ensemble_24hours_1$dftime) = as.numeric(substr(ensemble_24hours_1$ensemble_24hours_0.first_category,18,19))
minute(ensemble_24hours_1$dftime) = 00
second(ensemble_24hours_1$dftime) = 00
ensemble_24hours_1$dftime = as.character(ensemble_24hours_1$dftime)
check = left_join(finaltest,ensemble_24hours_1,by = c("dftime","grid"))
check = check[,-57]
check = check[,-56]
#Calculate CRPS and CRPSS for the original ensemble forecast
check$crps=apply(check,1,function(x) crps_sample(as.numeric(x['final']),as.numeric(x[5:55]))) 
clim=check$final
check$crps_clim=apply(check,1,function(x) crps_sample(as.numeric(x['final']),clim))
check$crpss=(mean(check$crps)-mean(check$crps_clim))/(-mean(check$crps_clim))
check = check[,56:58]


