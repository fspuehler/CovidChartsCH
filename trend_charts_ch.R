rm(list=ls())

library(tidyverse)
library(zoo)
library(lubridate)
library(httr)
library(rjson)
library("SciViews")
library(foreach)
library(future)
library(doParallel)

n.cores <- parallel::detectCores() - 1

my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)

print(my.cluster)

doParallel::registerDoParallel(cl = my.cluster)
foreach::getDoParRegistered()


## downloaded and extraced files

path <- "~/documents/download_data/data/"


path_export <- "~/documents/CovidTrendsCH/charts/"




## how old is data in -days
## wednesday -1, thursday -2, friday -3,...
data_age <- -0

## start
start <- as.Date("2022/03/01")
date_d <- seq.Date(start, Sys.Date()-1+data_age, 1)

age <- c("0 - 9","10 - 19","20 - 29","30 - 39", "40 - 49", "50 - 59", "60 - 69", "70 - 79", "80+")





###### daily data

file_1 <- "Cases"
file_2 <- "HospCapacity"
file_3 <- "Death"
file_4 <- "Hosp"
file_5 <- "CasesRawData"


filename_1 <- paste("",path,"COVID19",file_1,"_geoRegion.csv",sep="")
filename_2 <- paste("",path,"COVID19",file_2,"_geoRegion.csv",sep="")
filename_3 <- paste("",path,"COVID19",file_3,"_geoRegion.csv",sep="")
filename_4 <- paste("",path,"COVID19",file_4,"_geoRegion.csv",sep="")
filename_10 <- paste("",path,"COVID19",file_5,"_AKL10_d.csv",sep="")


data_1 <- read.table(filename_1, sep=",", header=TRUE)
data_2 <- read.table(filename_2, sep=",", header=TRUE)
data_3 <- read.table(filename_3, sep=",", header=TRUE)
data_4 <- read.table(filename_4, sep=",", header=TRUE)
data_10 <- read.table(filename_10, sep=",", header=TRUE)

moving_average7 <- function(x, n = 7) {             
  stats::filter(x, rep(1 / n, n), sides = 2)
}

moving_average5 <- function(x, n = 5) {             
  stats::filter(x, rep(1 / n, n), sides = 2)
}

moving_average10 <- function(x, n = 10) {             
  stats::filter(x, rep(1 / n, n), sides = 2)
}

moving_average14 <- function(x, n = 14) {             
  stats::filter(x, rep(1 / n, n), sides = 2)
}

moving_average21 <- function(x, n = 21) {             
  stats::filter(x, rep(1 / n, n), sides = 2)
}

moving_average28 <- function(x, n = 28) {             
  stats::filter(x, rep(1 / n, n), sides = 2)
}

##############################
###       read data d      ###
##############################

cases_ch <- NULL
hosp_full_ch <- NULL
hosp_adm_ch <- NULL
icu_full_ch <- NULL
death_ch <- NULL

a <- 1
for(a in 1:length(date_d)) {
  
  cases_ch <-  c(cases_ch, subset(data_1, datum == (date_d[a]) & geoRegion == "CH" )[1,3])
  hosp_full_ch <-  c(hosp_full_ch, subset(data_2, date == (date_d[a]) & geoRegion == "CH" )[1,7])
  hosp_adm_ch <-  c(hosp_adm_ch, subset(data_4, datum == (date_d[a]) & geoRegion == "CH" )[1,3])
  icu_full_ch <-  c(icu_full_ch, subset(data_2, date == (date_d[a]) & geoRegion == "CH" )[1,4])
  death_ch <-  c(death_ch, subset(data_3, datum == (date_d[a]) & geoRegion == "CH" )[1,3])
  
  a <- a+1
}


data_10_ch <- subset(data_10, date >  (start-10) & geoRegion == "CH")


cases_covid_age <- data.frame(date_d)
aaa <- 1
for(aaa in 1:length(age)) {
  
  cases_covid_age_1 <- NULL
  aa <- 1
  for(aa in 1:length(date_d)){
    cases_covid_age_1 <- c(cases_covid_age_1,subset(data_10_ch, date ==  (date_d[aa]) & ageRange == age[aaa] )[1,4])
    aa <- aa+1
  }
  
  cases_covid_age[age[aaa]] <- cases_covid_age_1
  aaa <- aaa+1
}




covid_data_d <- data.frame(date_d, cases_ch,hosp_full_ch,icu_full_ch,death_ch,hosp_adm_ch)






######## WASTEWATER !!! EXCLUDED











###### PLOT

filename_chart <- paste("",path_export,"CHgraphs.png",sep="")
png(file=filename_chart , 
    width=3200, height=2400)
par(mar = c(5, 20, 5, 20),mfrow=c(2,2),oma=c(4,1,9,1))




####### GRAPH 1

dummy_x <- seq(100000, 100000+length(covid_data_d[,1])-1, 1)

plot(covid_data_d[,1], -covid_data_d[,2], xaxs="i",xaxt="n",yaxt="n",xlab = "",ylab = "", main = "", ylim=c(0,20000), 
     pch=2, lwd = 0, col = "white", cex.main=1, xlim = c(covid_data_d[8,1], Sys.Date()-3), frame.plot = FALSE,  yaxs="i")

mtext("Covid: Cases, Hospital, ICU and Death (7d, 14d, 21d and 28d)", side = 3, line = 1, at = NA,
      adj = NA, padj = NA, cex = 3)

abline(h=10000, col ="grey")
abline(h=20000, col ="grey")
abline(h=30000, col ="grey")
abline(h=40000, col ="grey")

scale_factor <- c(1,20,200,2000)
color_cat <- c("orange", "skyblue", "purple", "red")
labels_cat <- c("Cases", "Beds full", "ICU full", "Deaths (reporting delay)")




lines(covid_data_d[,1]+3,  moving_average7(covid_data_d[,2])*scale_factor[1], col = color_cat[1], lwd=4)
axis(side = 2, las = 2, mgp = c(3, 0.7, 0), cex.axis=3,lwd.ticks = 2, col.axis = color_cat[1])

mtext(labels_cat[1], side = 2, line = 8.2, at = NA,
      adj = NA, padj = NA, cex = 2.5, col = color_cat[1])

par(new=TRUE)
plot(covid_data_d[,1]+3, -covid_data_d[,3], xaxs="i", xaxt="n",yaxt="n", xlab = "",ylab = "",ylim=c(0,40000/scale_factor[2]), 
     pch=1, lwd = 2, col = "white", cex.main=2, xlim = c(covid_data_d[8,1], Sys.Date()-3), frame.plot = FALSE,  yaxs="i")
axis(side = 4, line = 9,las = 2, mgp = c(3, 0.7, 0), cex.axis=3, lwd.ticks = 2, col.axis = color_cat[2])
lines(covid_data_d[,1]+3,  moving_average14(covid_data_d[,3]), col = color_cat[2], lwd=4)
mtext(labels_cat[2], side = 4, line = 17, at = NA,
      adj = NA, padj = NA, cex = 2.5, col = color_cat[2])
par(new=TRUE)
plot(covid_data_d[,1]+3, -covid_data_d[,4], xaxs="i", xaxt="n",yaxt="n", xlab = "",ylab = "",ylim=c(0,40000/scale_factor[3]), 
     pch=1, lwd = 1, col = "white", xlim = c(covid_data_d[8,1], Sys.Date()-3), frame.plot = FALSE, yaxs="i")
axis(side = 4, las = 2, mgp = c(3, 0.7, 0), cex.axis=3, lwd.ticks = 2, col.axis = color_cat[3])
lines(covid_data_d[,1]+3,  moving_average21(covid_data_d[,4]), col = color_cat[3], lwd=4)
mtext(labels_cat[3], side = 4, line = 7, at = NA,
      adj = NA, padj = NA, cex = 2.5, col = color_cat[3])
par(new=TRUE)
plot(covid_data_d[,1]+3, -covid_data_d[,5], xaxs="i", xaxt="n",yaxt="n", xlab = "",ylab = "",ylim=c(0,40000/scale_factor[4]), 
     pch=1, lwd = 2, col = "white", xlim = c(covid_data_d[8,1], Sys.Date()-3), frame.plot = FALSE, yaxs="i")
axis(side = 2, line = 12, las = 2, mgp = c(3, 0.7, 0), cex.axis=3, lwd.ticks = 2, col.axis = color_cat[4])

mtext(labels_cat[4], side = 2, line = 16.5, at = NA,
      adj = NA, padj = NA, cex = 2.5, col = color_cat[4])

lines(covid_data_d[1:(length(date_d)-30),1]+3, moving_average28(covid_data_d[1:(length(date_d)-30),5]), col = color_cat[4], lwd=4)
lines(covid_data_d[(length(date_d)-54):(length(date_d)),1]+3, moving_average28(covid_data_d[(length(date_d)-54):(length(date_d)),5]), col = color_cat[4], lwd=4, lty=2)

axis(side = 1, line = 0.5, mgp = c(3, 2, 0),las=0.5, cex.axis=3, lwd.ticks = 2, at=seq(covid_data_d[1,1], Sys.Date()-2,by = "months"), labels=format(seq(covid_data_d[1,1], Sys.Date()-2,by = "months"), format="%b %y"))





####### GRAPH 2
par(mar = c(5, 8, 3.5, 1.5))


ratio_x222 <- moving_average7(covid_data_d[(8:length(covid_data_d[,1])),2])/moving_average7(covid_data_d[(1:(length(covid_data_d[,1])-7)),2])
ratio_x333 <- moving_average14(covid_data_d[(8:length(covid_data_d[,1])),3])/moving_average14(covid_data_d[(1:(length(covid_data_d[,1])-7)),3])
ratio_x444 <- moving_average21(covid_data_d[(8:length(covid_data_d[,1])),4])/moving_average21(covid_data_d[(1:(length(covid_data_d[,1])-7)),4])
ratio_x555 <- moving_average28(covid_data_d[(8:length(covid_data_d[,1])),5])/moving_average28(covid_data_d[(1:(length(covid_data_d[,1])-7)),5])


plot(covid_data_d[8:length(covid_data_d[,1]-7),1], ratio_x222, xaxt="n",yaxt="n", xlab = "", ylab = "",
     ylim=c(0.5,2.0),xlim=c(covid_data_d[15,1],Sys.Date()-5), log = "y", 
     pch=1, lwd = 2,cex=2, col = "white", main = "", cex.main=2)
mtext("Ratio", side = 3, line = 1.2, at = NA,
      adj = NA, padj = NA, cex = 4)

axis(side = 2,   las = 2, mgp = c(3, 0.7, 0), cex.axis=3,lwd.ticks = 2)
axis(side = 1,   las = 1, mgp = c(3, 2, 0), cex.axis=3, at= seq(as.Date("2022-03-01"), Sys.Date()-1,by = "months"), labels=format(seq(as.Date("2022-03-01"), Sys.Date()-1,by = "months"), format="%b %y"),lwd.ticks = 2)




mtext("Ratio (#/#_7d_before)", side = 2, line = 4.8, at = NA,
      adj = NA, padj = NA, cex = 3)


abline(h=1, col="black", lwd = 4)
abline(h=2^(0.5), col="gray", lty="dotted", lwd = 4)
abline(h=2^(0.25), col="lightgray", lty="dotted", lwd = 4)
abline(h=0.5^(0.5), col="lightgray", lty="dotted", lwd = 4)
abline(h=0.5^(0.25), col="lightgray", lty="dotted", lwd = 4)


lines(covid_data_d[8:length(covid_data_d[,1]-7),1], moving_average5(ratio_x222), col = "orange", lwd=5)
lines(covid_data_d[8:length(covid_data_d[,1]-7),1]-5, moving_average5(ratio_x333), col = "skyblue", lwd=5)
lines(covid_data_d[8:length(covid_data_d[,1]-7),1]-7, moving_average5(ratio_x444), col = "purple", lwd=5)
lines(covid_data_d[8:(length(covid_data_d[,1]-10)-30),1]-12, moving_average5(ratio_x555)[1:(length(date_d)-37)], col = "red", lwd=5)




lines(covid_data_d[(length(covid_data_d[,1]-10)-30):(length(covid_data_d[,1])),1]-12, moving_average5(ratio_x555)[(length(date_d)-37):(length(date_d)-7)], col = "red", lwd=5, lty=2)

growth2 <- moving_average5(ratio_x222)
growth3 <- moving_average5(ratio_x333)
growth4 <- moving_average5(ratio_x444)

growth4

growth_date2 <- growth2[length(growth2)-6]
growth_date3 <- growth3[length(growth3)-9]
growth_date4 <- growth4[length(growth4)-12]

weekly_change2 <- growth2[length(growth2)-6]-1
weekly_change3 <- growth3[length(growth3)-9]-1
weekly_change4 <- growth4[length(growth4)-12]-1


weekly_change_prc2 <- weekly_change2*100
weekly_change_prc3 <- weekly_change3*100
weekly_change_prc4 <- weekly_change4*100


points(covid_data_d[length(growth2)+1,1] ,growth2[length(growth2)-6], cex = 3, lwd = 4, col = "orange")
points(covid_data_d[length(growth3)-7,1] ,growth3[length(growth3)-9], cex = 3, lwd = 4, col = "skyblue")
points(covid_data_d[length(growth4)-12,1] ,growth4[length(growth4)-12], cex = 3, lwd = 4, col = "purple")




text(y= 2^(0.5), x = covid_data_d[21,1], labels = "Two week doubling", col = "black", cex = 4, pos = 4)
text(y= 2^(0.25), x = covid_data_d[21,1], labels = "Four week doubling", col = "black", cex = 4, pos = 4)
text(y= 0.5^(0.5), x = covid_data_d[21,1], labels = "Two week halving", col = "black", cex = 4, pos = 4)
text(y= 0.5^(0.25), x = covid_data_d[21,1], labels = "Four week halving", col = "black", cex = 4, pos = 4)



if (weekly_change2 < 0){
  text_rate2 <- paste("",round(weekly_change_prc2, 2),"%, halving ",round(log(0.5, base = growth_date2 ), 1)," week(s)", sep = "")
}
if (weekly_change2 > 0){
  text_rate2 <- paste("+",round(weekly_change_prc2, 2),"%, doubling ",round(log(2, base = growth_date2 ), 1)," week(s)", sep = "")
}

if (weekly_change3 < 0){
  text_rate3 <- paste("",round(weekly_change_prc3, 2),"%, halving ",round(log(0.5, base = growth_date3 ), 1)," week(s)", sep = "")
}
if (weekly_change3 > 0){
  text_rate3 <- paste("+",round(weekly_change_prc3, 2),"%, doubling ",round(log(2, base = growth_date3 ), 1)," week(s)", sep = "")
}

if (weekly_change4 < 0){
  text_rate4 <- paste("",round(weekly_change_prc4, 2),"%, halving ",round(log(0.5, base = growth_date4 ), 1)," week(s)", sep = "")
}
if (weekly_change4 > 0){
  text_rate4 <- paste("+",round(weekly_change_prc4, 2),"%, doubling ",round(log(2, base = growth_date4 ), 1)," week(s)", sep = "")
}

legend(x = "top",inset = c(0, 0.01), legend= c("Cases", "Beds-full (t-5)", "ICU-full (t-7)","Death (t-12)", text_rate2, text_rate3, text_rate4, ""),
       col= c("orange", "skyblue", "purple", "red","orange", "skyblue", "purple", "white"), pch = c(NA, NA, NA,NA, 1,1,1, NA), lwd=4, cex=3, ncol = 2, bty = "n")




####### GRAPH 3


par(mar = c(5, 8, 5, 1.5))
plot(covid_data_d[8:length(covid_data_d[,1]-7),1], ratio_x222, xaxt="n",yaxt="n", xlab = "", ylab = "",
     ylim=c(0.5,2.0),xlim=c(covid_data_d[15,1],Sys.Date()-5), log = "y", 
     pch=1, lwd = 1,cex=1, col = "white", main = "", cex.main=1)
mtext("Case Ratio age-specific", side = 3, line = 1.2, at = NA,
      adj = NA, padj = NA, cex = 4)


axis(side = 2,   las = 2, mgp = c(3, 0.7, 0), cex.axis=3,lwd.ticks = 2)
axis(side = 1,   las = 1, mgp = c(3, 2, 0), cex.axis=3, at= seq(as.Date("2022-03-01"), Sys.Date()-1,by = "months"), labels=format(seq(as.Date("2022-03-01"), Sys.Date()-1,by = "months"), format="%b %y"),lwd.ticks = 2)



mtext("Case Ratio (#Cases/#Cases_7d_before)", side = 2, line = 4.8, at = NA,
      adj = NA, padj = NA, cex = 3)



abline(h=1, col="black", lwd = 4)
abline(h=2^(0.5), col="gray", lty="dotted", lwd = 4)
abline(h=2^(0.25), col="lightgray", lty="dotted", lwd = 4)
abline(h=0.5^(0.5), col="lightgray", lty="dotted", lwd = 4)
abline(h=0.5^(0.25), col="lightgray", lty="dotted", lwd = 4)

color_age <- c("yellow","orange","red","brown","pink","green","skyblue","blue","black")
label_age <- c("0 - 9","10 - 19","20 - 29","30 - 39", "40 - 49", "50 - 59", "60 - 69", "70 - 79", "80+")


dd <- 1
for(dd in 1:length(label_age)) {
  ratio_x222 <- moving_average7(cases_covid_age[(8:length(covid_data_d[,1])),dd+1])/moving_average7(cases_covid_age[(1:(length(covid_data_d[,1])-7)),dd+1])
  lines(cases_covid_age[8:length(covid_data_d[,1]-7),1], moving_average5(ratio_x222), col = color_age[dd], lwd=4)
  
  dd <- dd+1
}


cases_covid_age[(8:length(covid_data_d[,1])),dd+1]

legend(covid_data_d[22,1], 1.9, legend= rev(label_age[1:9]),
       col= rev(color_age[1:9]), lwd=4, cex=3)


##### GRAPH 4 !!! EXCLUDED

main_title2 <- paste("Covid Switzerland",sep="")

subtitle_fs <- paste("data: FOPH - ",Sys.Date()+data_age,", Mastodon: @CovidChartsCH@chaos.social",sep="")

mtext(main_title2, side = 3, line = 2.5, outer = TRUE, cex = 7)

mtext(subtitle_fs, side = 1, line = 2, outer = TRUE, cex = 3)

dev.off()


