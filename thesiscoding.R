#install.packages("dplyr")     # Install dplyr package
library("dplyr")              # Load dplyr package

#install.packages("readxl")     # Install readxl package
library("readxl")              # Load readxl package

install.packages("zoo")     # Install zoo package
library("zoo")                 # Load zoo package

#install.packages("ggplot2") # Install ggplot2 package
#library("ggplot2")            # Load ggplot2 package

#install.packages("Hmisc") # Install Hmisc package
#library("Hmisc")            # Load Hmisc package

#install.packages("quantmod") # Install quantmod package
#library(quantmod)             # Load quantmod package

#https://www.rdocumentation.org/packages/PerformanceAnalytics/versions/2.0.4/topics/PerformanceAnalytics-package
#https://cran.r-project.org/web/packages/PerformanceAnalytics/
#install.packages("PortfolioAnalytics") # Install PortfolioAnalytics package
#library (PortfolioAnalytics)  # Load PortfolioAnalytics package

#install.packages("xts") # Install quantmod package
library(xts)                  # Load xts package

#install.packages("tidyquant") # Install tidyquant package
#library(tidyquant)             # Load tidyquant package

#install.packages("openxlsx") # Install openxlsx package
#library(openxlsx)             # Load openxlsx package

#library(MASS)              # Load MASS package

#install.packages("nor1mix") # Install nor1mix package
#library(nor1mix)              # Load nor1mix package

#install.packages("sn") # Install sn package
#library(sn)

install.packages("lmtest") # Install lmtest package
library("lmtest")

install.packages("sandwich") # Install sandwich package
library("sandwich")

install.packages("car") # Install sandwich package
library("car")

install.packages("moments") # Install sandwich package
library("moments")

install.packages("tidyr") 
library("tidyr")

#remove.packages("dplyr") # Remove Package
#################################################################

tq<-read_xlsx("/Users/vinay/Desktop/SUMasters/Semester4/PythonRstudio/closingprices.xlsx")

tq

#sum of each column output
library(plyr)
numcolwise(sum)(tq)

#Sum of rows excluding first column
tq$sum<-rowSums(tq[,-1])
tq$sum

# drop last column with sum values to restore to basic DF
tq<-tq[,-59]

# number of columns count
n=ncol(tq)
n

##########
#STAGE1 : Market weight portfolio
##########


#sum of month end market cap
mrktcap=0
for (d in 3:49){
  mrktcap =mrktcap+tq[,c(d)]
  d=d+2
  
}
# Print marketCap column
mrktcap

# add mktcap column to dataframe
tq<-cbind(tq,mrktcap)

# change column name
colnames(tq)[59]<- "mrktcap"

# Convert 'Non month end' Market cap row values to zero
for (c in 2:6655){
  if (tq$mrktcap[c]< 10^10){
    tq$mrktcap[c] = 0  
  }
}


#New data frame of market cap columns
tq_new <- tq[, -c(1,52,53,54,55,56,57,58,59)]


# market weights calculation based on month end data
mktcapratio = 0
even_cols <-seq_len(50) %% 2 # select even columns in new dataframe
data_mod<- tq_new[, even_cols ==0] # create new dataframe with mrktcap values
mktcapratio =data_mod/tq$mrktcap # weights calculation
#tq<-cbind(tq,mktcapratio) # addition of weights column to original dataFrame

# creation of daily return matrix
odd_cols<-seq_len(50) %% 2 # select odd columns in new dataframe
data_ret<- tq_new[, odd_cols==1] # create new dataframe with return values


# creation of mkt weight marix and return matrix
mat_mktcapratio<-data.matrix(mktcapratio)
mat_data_ret<-data.matrix(data_ret)


# market weights data populated into daily return rows
temprow_val = 0
for (c in 1:6655){
  if (mktcapratio$indu_a_me[c]!='NaN'){
    temprow_val=mktcapratio[c,]

  } else {
    mktcapratio[c,] = temprow_val
   
  }

}

# addition of weights column to original dataFrame
tq<-cbind(tq,mktcapratio) 


# return in percentage terms
data_ret_per =0

#missing value data population

#data_ret<-transform(data_ret, svol_b = na.locf(svol_b))


for (d in 1:25) {
  data_ret[,c(d)][data_ret[,c(d)] ==0]<-NA
  na.locf(data_ret[,c(d)])
}

data_ret<-transform(data_ret, indu_a = na.locf(indu_a))
data_ret<-transform(data_ret, inve_a = na.locf(inve_a))
data_ret<-transform(data_ret, ores = na.locf(ores))
data_ret<-transform(data_ret, svol_b = na.locf(svol_b))
data_ret<-transform(data_ret, bure = na.locf(bure))
data_ret<-transform(data_ret, kinv_b = na.locf(kinv_b))
data_ret<-transform(data_ret, zete_b = na.locf(zete_b))
data_ret<-transform(data_ret, abu = na.locf(abu))
data_ret<-transform(data_ret, trac_b = na.locf(trac_b))
data_ret<-transform(data_ret, pwt_b = na.locf(pwt_b))
data_ret<-transform(data_ret, svol_b = na.locf(svol_b))
data_ret<-transform(data_ret, svol_b = na.locf(svol_b))
data_ret<-transform(data_ret, svol_b = na.locf(svol_b))
data_ret<-transform(data_ret, svol_b = na.locf(svol_b))
data_ret<-transform(data_ret, svol_b = na.locf(svol_b))
data_ret<-transform(data_ret, svol_b = na.locf(svol_b))
data_ret<-transform(data_ret, svol_b = na.locf(svol_b))
data_ret<-transform(data_ret, svol_b = na.locf(svol_b))
data_ret<-transform(data_ret, svol_b = na.locf(svol_b))
data_ret<-transform(data_ret, svol_b = na.locf(svol_b))
data_ret<-transform(data_ret, svol_b = na.locf(svol_b))
data_ret<-transform(data_ret, svol_b = na.locf(svol_b))
data_ret<-transform(data_ret, svol_b = na.locf(svol_b))
data_ret<-transform(data_ret, svol_b = na.locf(svol_b))
data_ret<-transform(data_ret, svol_b = na.locf(svol_b))




for (c in 2:6655) {
  ret_row_per=(data_ret[c,]/data_ret[c-1,])-1
  data_ret_per<-rbind(data_ret_per,ret_row_per)
}



#replace inf with NaN
data_ret_per<-rapply(data_ret_per, f=function(x) ifelse(is.infinite(x),0,x), how = "replace")

# replace NaN with 0
data_ret_per<-rapply(data_ret_per, f=function(x) ifelse(is.nan(x),0,x), how = "replace")
print(data_ret_per)




#Daily market weighted portfolio return attempt2
port_ret_mw = 0
port_ret_df_mw = 0
for (c in 1:6655) {
  ret_row =data_ret_per[c,]                     # pick a row from return df
  t_ret_row = t(ret_row)                    # transpose row to column
  wgt_row = mktcapratio[c,]                 # pick a column from mkt wgt df
  t_wgt_row = t(wgt_row)                    # transpose row to col
  split_port_ret<- t_ret_row*t_wgt_row      # df with portfolio returns of each stock
  port_ret_mw= sum(split_port_ret)             # daily port return
  port_ret_df_mw <- cbind(port_ret_df_mw, port_ret_mw) # portfolio daily return df 
}

port_ret_df_mw      # mkt weight portfolio return value in row
port_rt_col<-t(port_ret_df_mw)  # mkt weight portfolio return value in column df

#dropping extra row 
port_rt_col<-port_rt_col[-1,]

#addition of market wirghted portfolio return column to original df
tq<-cbind(tq,port_rt_col)

# change column name
colnames(tq)[85]<- "mkt_wt_port_rt"
colnames(tq)[86]<- "eql_wt_port_rt"






##########
#STAGE2 : creation of equal weightage portfolio 
##########


# count of number of investments companies
eq_wt_port_ret = 0
for (c in 2:6655){
  ret_row =data_ret_per[c,] 
  n_non_zero_elements= sum(ret_row !=0)
  return_sum_daily= rowSums(ret_row)
  avg_port_ret = return_sum_daily/n_non_zero_elements
  eq_wt_port_ret<- cbind(eq_wt_port_ret, avg_port_ret)
  
}

print(eq_wt_port_ret) # equal wt portfolio return

eq_wt_port_ret_t <-t(eq_wt_port_ret) # equal weight portflio retn value in column df
print(eq_wt_port_ret_t)


#addition of equal weighted portfolio return column to original df
tq<-cbind(tq,eq_wt_port_ret_t)



###########
#STAGE3: Regression on return and factor analysis
##########

# daily risk free rate
rfd<-tq$ref/365
tq<-cbind(tq,rfd)

#rm-rf market port
rmwp = tq$mkt_wt_port_rt-tq$rfd # rm-rf mkt wgt prtflo
rewp = tq$eql_wt_port_rt-tq$rfd  # rm-rf eql wgt prtflo

# adding rm-rf column to df tq
tq<-cbind(tq,rmwp)
tq<-cbind(tq,rewp)



# run multiple linear regression mkt wtg
model<- lm(rmwp~tq$MKT+tq$QMJ+tq$BAB+tq$SMB+tq$HML+tq$UMD)
print(model)


# run multiple linear regression equal wtg
model<- lm(rewp~tq$MKT+tq$QMJ+tq$BAB+tq$SMB+tq$HML+tq$UMD)
print(model)




















