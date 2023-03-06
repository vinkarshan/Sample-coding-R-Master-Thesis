tq<-read.csv("Desktop/RFolder/SXT_Mar2020.csv", header=TRUE)


##PLOT SOME STUFF (not needed)
#head(tq)
#head(tq, 25)
#table(tq$Type)
#table(tq$Date.Time, tq$Bid.Size)
#table(tq$Type)
#plot(tq$Price)
#plot(tq$Price, type = "l")
#plot(tq$Price[tq$Type == "Trade"], type = "l")
#plot(tq$Bid.Price, type = "l")
#plot(tq$Ask.Price, type = "l")

#Cleanse the data with ask and bid of 0
#install.packages("dplyr")     # Install dplyr package
library("dplyr")              # Load dplyr package

td <- tq

#Cleanse the data with ask and bid of 0 (can be used for D as well)
tq <- tq %>% mutate_at(c('Ask.Price'), ~na_if(., 0))
tq <- tq %>% mutate_at(c('Bid.Price'), ~na_if(., 0))


#Calculate midprice
tq$Mid.Price = (tq$Bid.Price+tq$Ask.Price) / 2
head(tq$Mid.Price)

#load package zoo
library(zoo)

#Last observation carry forward
tq$Mid.Price = na.locf(tq$Mid.Price, FALSE)
#Direction of trade dummy
tq$D = sign(tq$Price-tq$Mid.Price)

#More on -1 is shown in the below table, thus a bit more seller initiated
table(tq$D)

#Separate date from Date.Time
tq$Date<-(as.Date(tq$Date.Time))

#Convert Exch.Time to time
time = strptime(tq$Exch.Time, format = "%H:%M:%OS")
#Convert time to new column with seconds
tq$Seconds = time$hour * 3600 + time$min * 60 + time$sec
#Convert tq$Seconds column to offset GMT
tq$Seconds = tq$Seconds + tq$GMT.Offset*60*60
#Cleanse the data to fit openings hour 14:35 (13:35) - 20:55 (19:55)
tq = tq[tq$Seconds > (9*3600 + 35*60) & tq$Seconds < (16*3600 - 5*60), ]

head(tq$Exch.Time)
tail(tq$Exch.Time)

#To save
#save(tq, file = "tqData")

#To load
#load(tq, file = "tqData")

####CLEANSE TWO REMOVE WHEN D = 0 to NA
tq <- tq %>% mutate_at(c('D'), ~na_if(., 0))

#More on -1 is shown in the below table, thus a bit more seller initiated
table(tq$D)

#Adapt data to circuit breaker

tq_cb <- tq
tq_ca <- tq

#Circuit breaker S&P500 at 12:56:00 p.m. Reopening auction began at 1:14:00 p.m.
tq_cb = tq[tq$Seconds >= (12*3600 + 26*60 + 0*1) & 
             tq$Seconds < (12*3600 + 56*60 + 0*1), ]
tq_ca = tq[tq$Seconds > (13*3600 + 14*60 + 0*1) & 
             tq$Seconds <= (13*3600 + 45*60 + 0*1), ]


#Circuit break window for testing
#tq_ca = tq[tq$Seconds > (13*3600 + 1*60 + 0*1) & 
#             tq$Seconds <= (13*3600 + 41*60 + 0*1), ]

#To save
#save(tq, file = "tqData")

#To load
#load(tq, file = "tqData")

#Total trading volume USD BEFORE
TradeVolume_cb <- aggregate(x = tq_cb$Volume*tq_cb$Price,         # Specify data column
                            by = list(tq_cb$Date==2020-03-18),              # Specify group indicator
                            FUN = sum, na.rm = TRUE)           # Specify function (i.e. sum)
print(TradeVolume_cb)

#TradeVolume_cb <- aggregate(x = tq_cb$Volume*tq_cb$Price,         # Specify data column
#                            by = list(tq_cb$Date),              # Specify group indicator
#                            FUN = sum, na.rm = TRUE) 
#print(TradeVolume_cb)


#Total trading volume USD AFTER

TradeVolume_ca <- aggregate(x = tq_ca$Volume*tq_ca$Price,         # Specify data column
                            by = list(tq_ca$Date==2020-03-18),              # Specify group indicator
                            FUN = sum, na.rm = TRUE)           # Specify function (i.e. sum)
print(TradeVolume_ca)




#TradeVolume_ca <- aggregate(x = tq_ca$Volume*tq_ca$Price,         # Specify data column
#                            by = list(tq_ca$Date),              # Specify group indicator
#                            FUN = sum, na.rm = TRUE)           # Specify function (i.e. sum)
#print(TradeVolume_ca
#Calculate relative quoted spread AFTER per day
RQS_ca <- aggregate(x = 10000*(tq_ca$Ask.Price - tq_ca$Bid.Price)/tq_ca$Mid.Price,         # Specify data column
                    by = list(tq_ca$Date==2020-03-18),              # Specify group indicator
                    FUN = mean, na.rm = TRUE)           # Specify function (i.e. sum)

#RQS_ca <- aggregate(x = 10000*(tq_ca$Ask.Price - tq_ca$Bid.Price)/tq_ca$Mid.Price,         # Specify data column
#                    by = list(tq_ca$Date),              # Specify group indicator
#                    FUN = mean, na.rm = TRUE)           # Specify function (i.e. sum)




#Print the relative quoted spread AFTER in BPS per day
print(RQS_ca)

#Calculate relative quoted spread BEFORE per day
RQS_cb <- aggregate(x = 10000*(tq_cb$Ask.Price - tq_cb$Bid.Price)/tq_cb$Mid.Price,         # Specify data column
                    by = list(tq_cb$Date==2020-03-18),         # Specify function (i.e. sum)),              # Specify group indicator
                    FUN = mean, na.rm = TRUE)           # Specify function (i.e. sum)


#RQS_cb <- aggregate(x = 10000*(tq_cb$Ask.Price - tq_cb$Bid.Price)/tq_cb$Mid.Price,         # Specify data column
#                    by = list(tq_cb$Date)         # Specify function (i.e. sum)),              # Specify group indicator
#                    FUN = mean, na.rm = TRUE)           # Specify function (i.e. sum)



#Print the relative quoted spread BEFORE in BPS per day
print(RQS_cb)

#To make a diff column for seconds tq_cb
Seconds.diff <- diff(tq_cb$Seconds)
Seconds.diff <- c(0, Seconds.diff)
#Remove negative values (first trade for each day)
tq_cb$Seconds.diff <- pmax(Seconds.diff,0)
head(tq_cb$Seconds.diff)
#Change outlier values (>1) for column Seconds.diff to median for Seconds.diff
tq_cb$Seconds.diff[tq_cb$Seconds.diff > 1] <- median(tq_cb$Seconds.diff)

#To make a diff column for seconds for tq_ca
Seconds.diff <- diff(tq_ca$Seconds)
Seconds.diff <- c(0, Seconds.diff)
#Remove negative values (first trade for each day)
tq_ca$Seconds.diff <- pmax(Seconds.diff,0)
head(tq_ca$Seconds.diff)
#Change outlier values (>1) for column Seconds.diff to median for Seconds.diff
tq_ca$Seconds.diff[tq_ca$Seconds.diff > 1] <- median(tq_ca$Seconds.diff)

#Calculate weighted relative quoted spread per day
#Weighted mean for RQS BEFORE is based on column tq$Seconds.diff
x1 <- c(10000*(tq_cb$Ask.Price - tq_cb$Bid.Price)/tq_cb$Mid.Price) # Create vector with
w1 <- c(tq_cb$Seconds.diff)           # Extend weights vector
group <- c(tq_cb$Date==2020-03-18)           # Create group indicator
data <- data.frame(x1, w1, group)                  # Create data frame
data %>%                                           # Weighted mean by group
  group_by(group) %>% 
  summarise(weighted.mean(x1, w1, na.rm = TRUE))

#Calculate weighted relative quoted spread per day
#Weighted mean for RQS AFTER is based on column tq$Seconds.diff
x1 <- c(10000*(tq_ca$Ask.Price - tq_ca$Bid.Price)/tq_ca$Mid.Price) # Create vector with
w1 <- c(tq_ca$Seconds.diff)           # Extend weights vector
group <- c(tq_ca$Date==2020-03-18)           # Create group indicator
data <- data.frame(x1, w1, group)                  # Create data frame
data %>%                                           # Weighted mean by group
  group_by(group) %>% 
  summarise(weighted.mean(x1, w1, na.rm = TRUE))

#Effective bid-ask spread

#Calculate effective quoted spread per day (quoted removed)
EQS_cb <- aggregate(x = 2*10000*(tq_cb$D * (tq_cb$Price - tq_cb$Mid.Price)/tq_cb$Mid.Price),         # Specify data column
                    by = list(tq_cb$Date),              # Specify group indicator
                    FUN = mean, na.rm = TRUE)           # Specify function (i.e. sum)
#Print the effective quoted spread in BPS per day (quoted removed)
print(EQS_cb)

#Calculate effective quoted spread per day (quoted removed)
EQS_ca <- aggregate(x = 2*10000*(tq_ca$D * (tq_ca$Price - tq_ca$Mid.Price)/tq_ca$Mid.Price),         # Specify data column
                    by = list(tq_ca$Date),              # Specify group indicator
                    FUN = mean, na.rm = TRUE)           # Specify function (i.e. sum)
#Print the effective quoted spread in BPS per day (quoted removed)
print(EQS_ca)

#Calculate weighted effective quoted spread in BPS per day
#Weighted mean for EQS BEFORE is based on column tq$Volume
x2 <- c(2*10000*(tq_cb$D * (tq_cb$Price - tq_cb$Mid.Price)/tq_cb$Mid.Price)) # Create vector with
w2 <- c(tq_cb$Volume)           # Extend weights vector
group2 <- c(tq_cb$Date)           # Create group indicator
data2 <- data.frame(x2, w2, group2)                  # Create data frame
data2 %>%                                           # Weighted mean by group
  group_by(group2) %>% 
  summarise(weighted.mean(x2, w2, na.rm = TRUE))

#Calculate weighted effective quoted spread in BPS per day
#Weighted mean for EQS AFTER is based on column tq$Volume
x2 <- c(2*10000*(tq_ca$D * (tq_ca$Price - tq_ca$Mid.Price)/tq_ca$Mid.Price)) # Create vector with
w2 <- c(tq_ca$Volume)           # Extend weights vector
group2 <- c(tq_ca$Date)           # Create group indicator
data2 <- data.frame(x2, w2, group2)                  # Create data frame
data2 %>%                                           # Weighted mean by group
  group_by(group2) %>% 
  summarise(weighted.mean(x2, w2, na.rm = TRUE))

#Quote-based market efficiency measures at stock-date frequency

#Let us extract just quotes for the coming measurements
qq_ca = tq_ca[tq_ca$Type== "Quote", c("Date", "Seconds", "Mid.Price")]
qq_cb = tq_cb[tq_cb$Type== "Quote", c("Date", "Seconds", "Mid.Price")]

require(zoo)

dates_ca = unique(qq_ca$Date)
efficiency_ca = matrix(nrow = length(dates_ca), ncol = 7, dimnames = 
                         list(as.character(dates_ca), c("var1min", "ac1min", "rv1min", 
                                                        "var10sec", "ac10sec", "rv10sec", 
                                                        "vr_1min_10sec")))

dates_cb = unique(qq_cb$Date)
efficiency_cb = matrix(nrow = length(dates_cb), ncol = 7, dimnames = 
                         list(as.character(dates_cb), c("var1min", "ac1min", "rv1min", 
                                                        "var10sec", "ac10sec", "rv10sec", 
                                                        "vr_1min_10sec")))

#Obtaining returns at one-minute frequency
breakpoint1min = seq(9+35/60, 15+55/60, 1/60)*3600
qq_ca$interval1min = seq(9+35/60, 15+55/60, 1/60)[findInterval(qq_ca$Seconds,
                                                               breakpoint1min)]
qq_ca$lastobs1min = c(diff(qq_ca$interval1min)>0, T)
qq_cb$interval1min = seq(9+35/60, 15+55/60, 1/60)[findInterval(qq_cb$Seconds, 
                                                               breakpoint1min)]
qq_cb$lastobs1min = c(diff(qq_cb$interval1min)>0, T)

for(d in 1:length(dates_ca)){
  equispaced1min = qq_ca[qq_ca$lastobs1min & qq_ca$Date==dates_ca[d], c("interval1min",
                                                                        "Mid.Price")]
  equispaced1min$Mid.Price = na.locf(equispaced1min$Mid.Price)
  equispaced1min$Ret = 100 * c(NA, diff(log(equispaced1min$Mid.Price)))
  
  #Efficiency measures one-minute frequency
  efficiency_ca[d,"var1min"] = var(equispaced1min$Ret, na.rm = TRUE)
  efficiency_ca[d, "ac1min"] = cor(equispaced1min$Ret[-length(equispaced1min$Ret)],
                                   equispaced1min$Ret[-1], use = "complete.obs")
  efficiency_ca[d, "rv1min"] = 100*mean(equispaced1min$Ret^2, na.rm = TRUE)
}

for(d in 1:length(dates_cb)){
  equispaced1min = qq_cb[qq_cb$lastobs1min & qq_cb$Date==dates_cb[d], c("interval1min",
                                                                        "Mid.Price")]
  equispaced1min$Mid.Price = na.locf(equispaced1min$Mid.Price)
  equispaced1min$Ret = 100 * c(NA, diff(log(equispaced1min$Mid.Price)))
  
  #Efficiency measures one-minute frequency
  efficiency_cb[d,"var1min"] = var(equispaced1min$Ret, na.rm = TRUE)
  efficiency_cb[d, "ac1min"] = cor(equispaced1min$Ret[-length(equispaced1min$Ret)],
                                   equispaced1min$Ret[-1], use = "complete.obs")
  efficiency_cb[d, "rv1min"] = 100*mean(equispaced1min$Ret^2, na.rm = TRUE)
}

#Obtaining returns at 10-seconds frequency
breakpoint10sec = seq(9+35/60, 15+55/60, (1/6)/60)*3600
qq_ca$interval10sec = seq(9+35/60, 15+55/60, (1/6)/60)[findInterval(qq_ca$Seconds, 
                                                                    breakpoint10sec)]
qq_ca$lastobs10sec = c(diff(qq_ca$interval10sec)>0, T)
qq_cb$interval10sec = seq(9+35/60, 15+55/60, (1/6)/60)[findInterval(qq_cb$Seconds, 
                                                                    breakpoint10sec)]
qq_cb$lastobs10sec = c(diff(qq_cb$interval10sec)>0, T)

for(d in 1:length(dates_ca)){
  equispaced10sec = qq_ca[qq_ca$lastobs10sec & qq_ca$Date==dates_ca[d], c("interval10sec",
                                                                          "Mid.Price")]
  equispaced10sec$Mid.Price = na.locf(equispaced10sec$Mid.Price)
  equispaced10sec$Ret = 100 * c(NA, diff(log(equispaced10sec$Mid.Price)))
  
  #Efficiency measures 10-seconds frequency per day
  efficiency_ca[d,"var10sec"] = var(equispaced10sec$Ret, na.rm = TRUE)
  efficiency_ca[d, "ac10sec"] = cor(equispaced10sec$Ret[-length(equispaced10sec$Ret)],
                                    equispaced10sec$Ret[-1], use = "complete.obs")
  efficiency_ca[d, "rv10sec"] = 100*mean(equispaced10sec$Ret^2, na.rm = TRUE)
  
}

for(d in 1:length(dates_cb)){
  equispaced10sec = qq_cb[qq_cb$lastobs10sec & qq_cb$Date==dates_cb[d], c("interval10sec",
                                                                          "Mid.Price")]
  equispaced10sec$Mid.Price = na.locf(equispaced10sec$Mid.Price)
  equispaced10sec$Ret = 100 * c(NA, diff(log(equispaced10sec$Mid.Price)))
  
  #Efficiency measures 10-seconds frequency per day
  efficiency_cb[d,"var10sec"] = var(equispaced10sec$Ret, na.rm = TRUE)
  efficiency_cb[d, "ac10sec"] = cor(equispaced10sec$Ret[-length(equispaced10sec$Ret)],
                                    equispaced10sec$Ret[-1], use = "complete.obs")
  efficiency_cb[d, "rv10sec"] = 100*mean(equispaced10sec$Ret^2, na.rm = TRUE)
  
}

#Variance ratio and results
efficiency_ca[,"vr_1min_10sec"] = efficiency_ca[,"var1min"] / (efficiency_ca[,"var10sec"]*6)

round(efficiency_ca,5)

#Variance ratio and results
efficiency_cb[,"vr_1min_10sec"] = efficiency_cb[,"var1min"] / (efficiency_cb[,"var10sec"]*6)

round(efficiency_cb,5)

