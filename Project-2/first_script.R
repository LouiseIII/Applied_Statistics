# Traitement de données / First Exploration
load("~/Google Drive/EPFL/Applied Stat/StatApp-314577/Project-2/2_online_shopping.RData")
path_to_data <- "~/Google Drive/EPFL/Applied Stat/StatApp-314577/Project-2/"
data_file <- "2_online_shopping.RData"
load(paste(path_to_data, data_file, sep=""))


# Rename the feature's Data
names(Data)[1:6] <- c("n_admin_page", "time_admin_page", "n_info_page", "time_info_page",
                      "n_product_page", "time_product_page")
names(Data)[c(10:11, 17)] <- c("special_day", "month", "weekend")
names(Data)[7:9] <- c("bounce_rates", "exit_rates", "page_values")
names(Data)[12:16] <- c("operating_sys", "browser", "region", "traffic_type", "visitor_type")
names(Data)[18] <- "Purchase"

# Put some in Factor 
Data$operating_sys <- as.factor(Data$operating_sys)
Data$browser <- as.factor(Data$browser)
Data$region <- as.factor(Data$region)
Data$traffic_type <- as.factor(Data$traffic_type)



gm <- glm(Revenue~., data=Data2, family="binomial")

#operating systems as factor 
# idem Browser / Region / Traffic Type 

Data2%>%
 mutate(res=resid(gm))%>% pivot_longer(-res)%>%
  ggplot(aes(y=res, x=value))+
  facet_wrap(~ name, scales="free")+
  geom_point()+
  geom_smooth()


#_____________________________________________________________________________
model_entire <- glm(purchase~., data=Data, family="binomial")
plot(model_entire)

Data%>%
  mutate(res=resid(model_entire), operating_sys=as.numeric(operating_sys),
         browser=as.numeric(browser),
         region=as.numeric(region),
         traffic_type=as.numeric(traffic_type),
         weekend=as.numeric(weekend), 
         visitor_type = as.numeric(visitor_type),
         month = as.numeric(month))%>% pivot_longer(-res)%>%
  ggplot(aes(y=res, x=value))+
  facet_wrap(~ name, scales="free")+
  geom_point()+
  geom_smooth()

library(tidyverse)

#_____________________________________________________________________________

Data2 <- Data
Data2$exit_rates <- log(Data2$exit_rates+1)
Data2$bounce_rates <- log(Data2$bounce_rates +1)
Data2$n_product_page <- log(Data2$n_product_page +1)
Data2$page_values <- log(Data2$page_values+1)
Data2$time_admin_page <- log(Data2$time_admin_page +1)
Data2$time_info_page <- log(Data2$time_info_page +1)
Data2$time_product_page <- log(Data2$time_product_page +1)

model_entire2 <- glm(purchase~.+page_values*exit_rates + page_values*bounce_rates +
                       time_product_page**2, data=Data2, family="binomial")

Data2%>%
  mutate(res=resid(model_entire2), operating_sys=as.numeric(operating_sys),
         browser=as.numeric(browser),
         region=as.numeric(region),
         traffic_type=as.numeric(traffic_type),
         weekend=as.numeric(weekend), 
         visitor_type = as.numeric(visitor_type),
         month = as.numeric(month))%>% pivot_longer(-res)%>%
  ggplot(aes(y=res, x=value))+
  facet_wrap(~ name, scales="free")+
  geom_point()+
  geom_smooth()

submodel <- glm(formula = purchase ~ n_admin_page + n_product_page + bounce_rates + 
      exit_rates + page_values + month  + visitor_type, 
    family = "binomial", data = Data2) #traffic_type

submodel <- glm(formula = purchase ~ n_admin_page + n_product_page + bounce_rates + 
                  exit_rates + page_values + month  + visitor_type, 
                family = "binomial", data = Data2) #traffic_type

#_____________________________________________________________________________

Data3 <- Data2
Data3$page_values2 <- Data3$page_values**2

model_entire3 <- glm(purchase~., data=Data3, family="binomial")

Data3%>%
  mutate(res=resid(model_entire3), operating_sys=as.numeric(operating_sys),
         browser=as.numeric(browser),
         region=as.numeric(region),
         traffic_type=as.numeric(traffic_type),
         weekend=as.numeric(weekend), 
         visitor_type = as.numeric(visitor_type),
         month = as.numeric(month))%>% pivot_longer(-res)%>%
  ggplot(aes(y=res, x=value))+
  facet_wrap(~ name, scales="free")+
  geom_point()+
  geom_smooth() 

#______________________________________________________________________
# Calcul of the AUC
library(pROC)
AUC_eval <- function(gmodel,Data){
  set.seed(517)
  Folds <- matrix(sample(1:dim(Data)[1]), ncol=5)
  AUC <- rep(0,5)
  for(k in 1:5){
    train <- Data[-Folds[,k],]
    test <- Data[Folds[,k],]
    my_gm <- glm(gmodel$formula, family="binomial", data=train)
    test_pred <- predict(my_gm, newdata = test, type="response")
    AUC[k] <- auc(test$purchase,test_pred)
  }
  return(mean(AUC))
}

gl <- glm(formula = purchase ~ n_admin_page + n_product_page + bounce_rates + 
            exit_rates + page_values + month + visitor_type  + 
            +page_values * exit_rates + page_values * bounce_rates, 
          family = "binomial", data = Data2)

gl2 <- glm(formula = purchase ~ n_admin_page + n_product_page + bounce_rates + 
             exit_rates + page_values + month  + visitor_type + 
             exit_rates:page_values + bounce_rates:page_values, family = "binomial", 
           data = Data2) # traffic_type

Data3 <- Data
Data3$pages <- Data$n_admin_page + Data$n_info_page + Data$n_product_page
gl <- glm(formula = purchase ~ . +page_values * exit_rates + 
            page_values * bounce_rates + pages * bounce_rates + 
            pages * exit_rates + pages * page_values, 
          family = "binomial", data = Data3)
# After AIC

gl2 <- glm(formula = purchase ~ n_info_page + time_product_page + bounce_rates + 
             exit_rates + page_values + month  + visitor_type + 
             pages + exit_rates:page_values + bounce_rates:page_values + 
             page_values:pages, family = "binomial", data = Data3) #traffic_type

ggplot(Data)+
  geom_point(aes(x=log(bounce_rates+0.00001), y=exit_rates))
















#---------------------------------------------------
# Exploration of the data (one variable at a time)
#---------------------------------------------------

  #1) Administrative
ggplot(Data)+
  geom_histogram(aes(x=n_admin_page, color=purchase))

ggplot(data=Data)+
  geom_point(aes(x=time_admin_page, y=Revenue))

ggplot(Data)+
  geom_point(aes(x=time_admin_page, y=n_admin_page, color=Revenue))

  #2) Info
Data2 <- Data[c(Data$n_info_page<10),]

ggplot(Data2)+
  geom_histogram(aes(x=n_info_page, color=Revenue))

ggplot(Data)+
  geom_point(aes(x=time_info_page, y=n_info_page, color=purchase))

    #si on regroupe ceux qui en ont consulté et ceux qui n'en ont pas 
Data2 <- Data
Data2$Informational <- as.numeric(Data2$Informational==0)
ggplot(Data2)+
  geom_histogram(aes(x=Informational, color=Revenue))


  #3) Product
ggplot(Data)+
  geom_histogram(aes(x=ProductRelated, color=Revenue))

ggplot(Data)+
  geom_point(aes(x=ProductRelated_Duration, y=ProductRelated, color=Revenue))

  #4) Months  
ggplot(Data)+
  geom_histogram(aes(x=Month, color=Revenue), stat="count")

ggplot(Data)+
  geom_histogram(aes(x=Weekend, color=Revenue), stat="count")

Data2 <- Data[c(Data$SpecialDay>0),]
ggplot(Data2)+
  geom_histogram(aes(x=SpecialDay, color=Weekend))

  #5) PAges Visited 
ggplot(Data)+
  geom_point(aes(x=BounceRates, y=Revenue))

ggplot(Data)+
  geom_histogram(aes(x=BounceRates, color=Revenue))


ggplot(Data)+
  geom_point(aes(x=ExitRates, y=Revenue))

ggplot(Data)+
  geom_point(aes(x=BounceRates, y=ExitRates, color=Revenue))


ggplot(Data)+
  geom_point(aes(x=PageValues, y=Revenue))

ggplot(Data)+
  geom_point(aes(x=PageValues, y=BounceRates, color=Revenue))

  #6) OperatingSystems
ggplot(Data)+
  geom_histogram(aes(x=OperatingSystems, color=Revenue))

index <- c(which(Data$OperatingSystems==4), 
          which(Data$OperatingSystems==5),
          which(Data$OperatingSystems==6),
          which(Data$OperatingSystems==7),
          which(Data$OperatingSystems==8))
Data$OperatingSystems[index]=4

ggplot(Data)+
  geom_histogram(aes(x=browser, color=purchase), stat="count")

index <- c(which(Data$Browser==3), 
           which(Data$Browser==4), 
           which(Data$Browser==5), 
           which(Data$Browser==6), 
           which(Data$Browser==7), 
           which(Data$Browser==8), 
           which(Data$Browser==9), 
           which(Data$Browser==10), 
           which(Data$Browser==11), 
           which(Data$Browser==12), 
           which(Data$Browser==13))
Data$Browser[index]=3
  
ggplot(Data)+
  geom_histogram(aes(x=Region, color=Revenue))

ggplot(Data)+
  geom_histogram(aes(x=TrafficType, color=Revenue))

ggplot(Data)+
  geom_histogram(aes(x=VisitorType, color=Revenue), stat="count")

x <- as.numeric(Data$month)

