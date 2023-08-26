### Importing the libraries 

library(dplyr)
library(tidyverse)
library(tidyr)
library(readxl)
library(openxlsx)
library(readr)
library(lubridate)
library(glue)
library(gmailr)
library(leaflet)
library(ggmap)
library(cluster)
library(factoextra)
library(plotly)
library(dbscan)
library(stats)
library(purrr)
library(cluster)
library(mvoutlier)
library(EnvStats)
library(ggdark)
library(stats)
library(RBCOMClient) ## Library of the sending emails 
options(scipen=999)


##############################################################################

### Reading the transaction data 
transactions <- read_excel("data/transactions.xlsx")

cc_info <- read_excel("data/cc_info.xlsx")




### Formulating the data in which the script will be run 

Reporting_Date <- format(Sys.Date(), "%y_%b_%d")


##############################################################################


### Starting with some data wrangling 


########################################################################################

##### Computing the list of customers that never went above their monthly limit  ##### 



Customers_Not_Excceding_Limit <- transactions %>% 
  ## Wrangling the transaction to become by year and month
  mutate(Transaction_Year_Month = paste0(year(as.Date(date)),str_pad(month(as.Date(date)),2,"left", 
                                                                     pad = 0))) %>%
  group_by(credit_card,Transaction_Year_Month) %>% 
  ## aggregating over each credit card and its year and month to calculate the monthly transaction dollar
  summarise(Monthly_Transaction_Amount = sum(transaction_dollar_amount, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(cc_info, by = "credit_card") %>%  ## Joining over the Credit Card info, getting the limit 
  ## per each credit card 
  mutate(Limit_Exceeding_Flag = if_else(Monthly_Transaction_Amount > credit_card_limit ,1,0)) %>% 
  ## Flagging the customers who exceeded their monthly credit limit 
  group_by(credit_card) %>% 
  summarize(Max_Exceeding_Flag = max(Limit_Exceeding_Flag)) %>% 
  ## grouping over the credit card so if the customer once exceeded his monthly limit will be flagged with 1 
  ## 0 implies that the customer never exceeded his credit limit
  ungroup() %>% 
  filter(Max_Exceeding_Flag ==0) %>% 
  ## filtering over the customers who never exceeded their monthly credit limit 
  select(credit_card) %>% 
  writexl::write_xlsx("output//Customers_Never_Exceeded_Limit.xlsx")



##############################################################################################


### Filtering the customer who went above their monthly limit 

Customers_Exceeding_Limit <- transactions %>% 
  mutate(Transaction_Year_Month = paste0(year(as.Date(date)),str_pad(month(as.Date(date)),2,"left", 
                                                                     pad = 0))) %>% ## Creating a new 
  ## Transaction flag using the year month
  group_by(credit_card,Transaction_Year_Month) %>% ## grouping by the credit card and the time in which 
  ## customers exceeded their monthly credit limit 
  summarise(Monthly_Transaction_Amount = sum(transaction_dollar_amount, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(cc_info, by = "credit_card") %>% ## joining with the credit card info, getting the monthly 
  ## credit card limit 
  mutate(Limit_Exceeding_Flag = if_else(Monthly_Transaction_Amount > credit_card_limit ,1,0)) %>% 
  ## flagging the customers who exceeded their limit 
  left_join(transactions %>% 
              group_by(credit_card) %>% 
              summarize(max_transaction_date = as.Date(max(date))) %>% 
              ungroup(), by = "credit_card") %>%
  mutate(Reporting_Date = Sys.Date()) %>%
  filter(Limit_Exceeding_Flag == 1) %>% 
  mutate(Message = paste("Passing the Monthly limit at",max_transaction_date)) %>%
  ## however, at this part we want to provide only the new customers who passed their monthly transaction 
  ## so the maximum transaction date per each credit card will be equalized with the reporting date, making 
  ## sure that the only new customer whoe exceeded their limits will be reported 
  filter(Reporting_Date == max_transaction_date) %>% 
  dplyr::select(Reporting_Date,Message,credit_card)



### saving the list of the customers exceeding their monthly limit 

## Renaming the file of the exceeded customers list with the time in which they were observed

## ****** Notice that ****** --> The provided list only includes  the customers who excceded their limits 
writexl::write_xlsx(Customers_Exceeding_Limit , paste0("output//Customers_Exceed_Limit_",Reporting_Date,".xlsx"))




#############################################################################################

####################################################################################

###### One of the effective approaches is to examine the outliers in the data 
## since the outliers can reveal the fact that some transaction when it gets into 
## amount or longitude and latitude is different . 
## it seems that examining outliers per each customer is an effective approach since each customer 

### has his own/ her own behavior. 

## for detecting the outliers the following will be implemented 
## the values of the transaction amount in dollar compared to the Q3+ 1.5 IRQ and Q3 - 1.5 IQR 
## Additionally, a further oultier detection method will be used to beat the problem 
## of the masked outliers, namely, Rosner test, getting the susbeted outlier observations per each credit card 
## the outliers will be computed based the transaction dollar amount, the latitude and langtitude and 
## then the common transaction will be computed, avoding the duplication in the transactions 
##

#############################################################################


## First of all, the outliers will be detected using IQR method for the transaction amount 
Doubted_Amount_Outliers <-  transactions %>% 
  left_join(transactions %>% 
              mutate(Transaction_Year_Month = paste0(year(as.Date(date)),str_pad(month(as.Date(date)),2,"left", 
                                                                                 pad = 0))) %>%
              group_by(credit_card) %>% 
              summarize(Transaction_Amount_Q1 = quantile(transaction_dollar_amount)[2], 
                        Transaction_Amount_Q3 = quantile(transaction_dollar_amount)[4]) %>% 
              ungroup(), by = "credit_card")  %>% 
  mutate(upper_boundary = Transaction_Amount_Q3 + 1.5 *(Transaction_Amount_Q3 - Transaction_Amount_Q1), 
         Lower_boundary = Transaction_Amount_Q1 - 1.5 *(Transaction_Amount_Q3 - Transaction_Amount_Q1)) %>% 
  filter( (transaction_dollar_amount < Lower_boundary | transaction_dollar_amount > upper_boundary))
nrow(Doubted_Amount_Outliers)





### getting the distinct credit cards by amounts 
distinct_suspected_amounts <- Doubted_Amount_Outliers %>% 
  distinct(credit_card, .keep_all = TRUE)

## computing the aniticpated outliers observations per each credit card 
## the K here is used to fed the method of rosner test, to have a further analysis whether those points 
## are truly outliers and if there are other masked outliers or not 

suspected_amount_k <- Doubted_Amount_Outliers %>% 
  group_by(credit_card) %>% 
  summarise(Number_Suspected_Transaction = n(), .groups = "drop") %>% 
  select(Number_Suspected_Transaction)


### Examining the distribution of the transaction dollar amount  

ggplotly(transactions %>%
           dplyr::select(transaction_dollar_amount) %>%
           ggplot(aes(x=transaction_dollar_amount)) +
           geom_histogram(aes(y=..density..), colour = "black", fill = "white", bins = 50)  +
           geom_density(alpha = .2, fill = "#FF6666") +
           dark_mode())


## Computing the rosner test results fpr the transacted amount 

Amount_Ronser_Test <- list()  ## formulating an empty list to store the statistical results of the rosner test
Suspected_Amount_Transaction <- list() ## formulating an empty list to store the outliers points 
Credit_Card_List <- list() ## formulatin an empty list per each credit card to filter over the outliers points per each credit card 
Detected_Transaction_Amounts <-  data.frame()
for ( i in  1:nrow(distinct_suspected_amounts)) {
  
  Data_Filtered <- transactions %>% 
    filter(credit_card == distinct_suspected_amounts[["credit_card"]][i]) 
  K_filtered <- suspected_amount_k[["Number_Suspected_Transaction"]][i]
  Test <- rosnerTest(Data_Filtered$transaction_dollar_amount, k = K_filtered) ## applying the rosner test
  Amount_Ronser_Test[[i]] <-  as.data.frame(Test$all.stats)  ## storing the out come as a data frame
  Suspected_Amount_Transaction[[i]] <- Amount_Ronser_Test[[i]] %>% 
    filter(Outlier == "TRUE") %>% 
    pull(Obs.Num) ## filtering the true outlier observations and pulling the observation numbers
  Credit_Card_List[[i]] <- as.data.frame(transactions %>% 
                                           filter(credit_card == distinct_suspected_amounts[["credit_card"]][i]) %>% 
                                           filter(row_number() %in% Suspected_Amount_Transaction[[i]])) ## filtering the 
  ## suspected transactions amounts 
  
  Detected_Transaction_Amounts <- rbind(Detected_Transaction_Amounts,Credit_Card_List[[i]] )
  ## appending all the detected transaction amounts to be outliers in a dataframe 
  
}


##################################################################################################

## Detecting the outliers for the longitude 


## First of all, the outliers will be detected using IQR method
Doubted_Long_Outliers <-  transactions %>% 
  left_join(transactions %>% 
              group_by(credit_card) %>% 
              summarize(Long_Q1 = quantile(Long)[2], 
                        Long_Q3 = quantile(Long)[4]) %>% 
              ungroup(), by = "credit_card")  %>% 
  mutate(upper_boundary = Long_Q3 + 1.5 *(Long_Q3 - Long_Q1), 
         Lower_boundary = Long_Q1 - 1.5 *(Long_Q3 - Long_Q1)) %>% 
  filter( (Long < Lower_boundary | Long > upper_boundary))






### getting the distinct credit cards by amounts 
distinct_suspected_longitude <- Doubted_Long_Outliers %>% 
  distinct(credit_card, .keep_all = TRUE)

## computing the aniticpated outliers observations per each credit card 
## the K here is used to fed the method of rosner test, to have a further analysis whether those points 
## are truly outliers and if there are other masked outliers or not 

susptected_Long_K <- Doubted_Long_Outliers %>% 
  group_by(credit_card) %>% 
  summarise(Number_Suspected_Transaction = n(), .groups = "drop") %>% 
  select(Number_Suspected_Transaction)



## Computing the rosner test results
Longitude_Ronser_Test <- list()  ## formulating an empty list to store the statistical results of the rosner test
Suspected_Long_Transaction <- list() ## formulating an empty list to store the outliers points 
Credit_Card_List <- list() ## formulatin an empty list per each credit card to filter over the outliers points per each credit card 
Detected_Transaction_longitude <- data.frame()
for ( i in  1:nrow(distinct_suspected_longitude)) {
  
  Data_Filtered <- transactions %>% 
    filter(credit_card == distinct_suspected_longitude[["credit_card"]][i]) 
  K_filtered <- susptected_Long_K[["Number_Suspected_Transaction"]][i]
  Test <- rosnerTest(Data_Filtered$Long, k = K_filtered) ## applying the rosner test
  Longitude_Ronser_Test[[i]] <-  as.data.frame(Test$all.stats)  ## storing the out come as a data frame
  Suspected_Long_Transaction[[i]] <- Longitude_Ronser_Test[[i]] %>% 
    filter(Outlier == "TRUE") %>% 
    pull(Obs.Num) ## filtering the true outlier observations and pulling the observation numbers
  Credit_Card_List[[i]] <- as.data.frame(transactions %>% 
                                           filter(credit_card == distinct_suspected_longitude[["credit_card"]][i]) %>% 
                                           filter(row_number() %in% Suspected_Long_Transaction[[i]])) ## filtering the 
  ## suspected transactions 
  
  Detected_Transaction_longitude <- rbind(Detected_Transaction_longitude,Credit_Card_List[[i]] )
  
  
}



############################################################################################


### detecting the outliers for the latitude 



## First of all, the outliers will be detected using IQR method
Doubted_Lat_Outliers <-  transactions %>% 
  left_join(transactions %>% 
              group_by(credit_card) %>% 
              summarize(Lat_Q1 = quantile(Lat)[2], 
                        Lat_Q3 = quantile(Lat)[4]) %>% 
              ungroup(), by = "credit_card")  %>% 
  mutate(upper_boundary = Lat_Q3 + 1.5 *(Lat_Q3 - Lat_Q1), 
         Lower_boundary = Lat_Q1 - 1.5 *(Lat_Q3 - Lat_Q1)) %>% 
  filter( (Lat < Lower_boundary | Lat > upper_boundary))






### getting the distinct credit cards by amounts 
distinct_suspected_latitude<- Doubted_Lat_Outliers %>% 
  distinct(credit_card, .keep_all = TRUE)

## computing the aniticpated outliers observations per each credit card 
## the K here is used to fed the method of rosner test, to have a further analysis whether those points 
## are truly outliers and if there are other masked outliers or not 

susptected_lat_K <- Doubted_Lat_Outliers %>% 
  group_by(credit_card) %>% 
  summarise(Number_Suspected_Transaction = n(), .groups = "drop") %>% 
  select(Number_Suspected_Transaction)



## Computing the rosner test results
Latitude_Ronser_Test <- list()  ## formulating an empty list to store the statistical results of the rosner test
Suspected_Lat_Transaction <- list() ## formulating an empty list to store the outliers points 
Credit_Card_List <- list() ## formulatin an empty list per each credit card to filter over the outliers points per each credit card 
Detected_Transaction_latitude <- data.frame()
for ( i in  1:nrow(distinct_suspected_latitude)) {
  
  Data_Filtered <- transactions %>% 
    filter(credit_card == distinct_suspected_latitude[["credit_card"]][i]) 
  K_filtered <- susptected_lat_K[["Number_Suspected_Transaction"]][i]
  Test <- rosnerTest(Data_Filtered$Long, k = K_filtered) ## applying the rosner test
  Latitude_Ronser_Test[[i]] <-  as.data.frame(Test$all.stats)  ## storing the out come as a data frame
  Suspected_Lat_Transaction[[i]] <- Latitude_Ronser_Test[[i]] %>% 
    filter(Outlier == "TRUE") %>% 
    pull(Obs.Num) ## filtering the true outlier observations and pulling the observation numbers
  Credit_Card_List[[i]] <- as.data.frame(transactions %>% 
                                           filter(credit_card == distinct_suspected_latitude[["credit_card"]][i]) %>% 
                                           filter(row_number() %in% Suspected_Lat_Transaction[[i]])) ## filtering the 
  ## suspected transactions 
  
  Detected_Transaction_latitude <- rbind(Detected_Transaction_latitude,Credit_Card_List[[i]] )
  
  
}


########## Joining the outliers across the transaction amount, longitude and latitude  ########## 


Outliers_long_lat <- Detected_Transaction_longitude %>% 
  inner_join(Detected_Transaction_latitude, by = c("credit_card", "date"))

Outliers_long <- Detected_Transaction_longitude %>% 
  anti_join(Detected_Transaction_latitude, by = c("credit_card", "date"))

Outliers_across_amounts_only <- Detected_Transaction_Amounts %>% 
  anti_join(Outliers_long_lat , by = c("credit_card", "date")) %>% 
  anti_join(Outliers_long, by = c("credit_card", "date"))


Outliers_long_only <- Detected_Transaction_longitude %>% 
  anti_join(Detected_Transaction_Amounts , by = c("credit_card", "date")) %>% 
  anti_join(Detected_Transaction_latitude, by = c("credit_card", "date"))



## computing the latitude since the longitude are common with the latitude, 
## the anti join will have the latitude only 
Outliers_lat_only <- Detected_Transaction_latitude %>% 
  anti_join(Detected_Transaction_Amounts , by = c("credit_card", "date")) 

### By combining the outliers across the amount, longitude and the latitude 
Outliers_across_amounts_long_lat <-Detected_Transaction_Amounts %>% 
  select(credit_card, date,transaction_dollar_amount) %>%
  inner_join(Detected_Transaction_longitude %>% 
               select(credit_card, date, Long), by = c("credit_card", "date")) %>% 
  inner_join(Detected_Transaction_latitude %>% 
               select(credit_card, date, Lat), by = c("credit_card", "date"))

## The best representation of the outliers method is detecting the maps of the outlier latitude and Longatidue 


Outliers_Lang_Lat <- Outliers_long %>% 
  select(credit_card,Long,Lat) %>%
  rbind(Outliers_long_lat %>% 
          dplyr::select(credit_card,Long.x,Lat.x) %>% 
          rename(Long = Long.x, 
                 Lat = Lat.x))


## Examining the distinct credit cards with outliers in longitude and latitude 

Distinct_Credit_Cards_outliers_Long_Lat <- Outliers_Lang_Lat %>% 
  distinct(credit_card, .keep_all = TRUE)

## ************ To be run for generating automatic Maps ************  ##
# for(i in 1:nrow(Distinct_Credit_Cards_outliers_Long_Lat)) {
#   credit_card <- transactions%>%
#     filter(credit_card == Distinct_Credit_Cards_outliers_Long_Lat[["credit_card"]][i])
#   graph <- qmplot(Long, Lat,data = credit_card, colour = I("red"), size = I(3),
#             darken = 0.3)
#   png(filename = paste0("fig/",Distinct_Credit_Cards_outliers_Long_Lat[["credit_card"]][i],".png"), width= 700, height = 600,units = "px", bg = "white")
#   plot(graph)
#   dev.off()
# }

### Combining all the transactions that are worth invetsigating 

Outliers <- Outliers_across_amounts_only %>%
  rbind(Outliers_long_only) %>% 
  rbind(Outliers_across_amounts_long_lat) %>% 
  rbind(Outliers_lat_only)


### writing the suspected trasactions that are worth investigation 

writexl::write_xlsx(Outliers, "output//Transactions_worth_investigation.xlsx")




########################################################################################


#### Applying clustering techniques 

## Before fitting any cluster algorithm, it is advisable to scale the data first 

## it seems a good approach to have the credit limit as it can indicate similar spending behavior 
## therefore, if one of the customer had a susbected transaction , this may give a further insight 


### scaling the data lets the feature are measurable 

Scaled_transaction_data <- scale(transactions %>% 
                                   left_join(cc_info %>% select(credit_card, credit_card_limit), 
                                             by = "credit_card") %>%
                                   select(transaction_dollar_amount,credit_card_limit))



### selecting the k clusters from 1 to 10 


tot_withinss <- map_dbl(1:10, function(k) {
  model = kmeans(x = Scaled_transaction_data, centers = k)
  model$tot.withinss
  
})

## combining the K cluster against the total with in distance 
elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

print(elbow_df)

## Plotting the different K clusters against the total within distance 
ggplot(elbow_df, aes(x=k, y = tot_withinss)) + 
  geom_line() + 
  scale_x_continuous(breaks = 1:10) +
  dark_mode()

## it can be implied that the most suitable K is 3 


## Silhouette analysis aims for making sure that each point fits into their corresponding clusters 
## silhouette cluster distance works on both the within cluster distance and the closest neighbours distance 
## the within cluster distance is the average euclidean distance compared to the group that this point is included in 
## while the closest neighbor distance is the average euclidean distance to the other nearest neighbors 
sil_width <- map_dbl(2:10, function(k)  {
  model = pam(x = Scaled_transaction_data, k = k)
  model$silinfo$avg.width
  
})



sil_df <- data.frame(
  k = 2:10, 
  sil_width = sil_width
)


print(sil_df)



ggplot(sil_df, aes(x=k, y = sil_width)) + 
  geom_line() + 
  scale_x_continuous(breaks = 2:10) +
  dark_mode()


#### Fitting the unsupervised machine learning algorithm using the kmeans with 3 number of clusters 
### Fitting over the data using 3 centeriods 

km_3 <- kmeans(Scaled_transaction_data, centers = 3, nstart = 100)
print(km_3)
km_3_clusters <-km_3$cluster




