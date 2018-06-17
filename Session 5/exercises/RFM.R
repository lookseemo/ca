#' RFM
#' @description  RFM (Recency, Frequency, Monetary ) analysis
#' @param data Dataset for analysis.
#' Should contain at least three columns : customer name, date of purchase and amount
#' @param analysisDate Date used of recency calculations. Defaults to current date.
#' @param customer Name of column containing name of customer. Default : customer
#' @param date Name of column containing date of transaction. Default : date
#' @param revenue Name of column containing the amount of transaction. Default : amount
#' @keywords RFM
#' @export
#' @examples
#' rfm(sales)
#' @author Syam Murali

rfm <- function(data, analysisDate = Sys.Date(), customer ="customer",
               date = "date", revenue  ="amount")
{
  
  if(ncol(data) != 3){
    stop("Data should have customer, amount and date columns")
  }
  
  data = data[,c(customer, date, revenue)]
  names(data) = c("customer", "date", "revenue")
  
  if(length(data$customer) == 0 | length(data$date) == 0 | length(data$revenue) == 0){
    stop("customer, amount or date column not specified")
  }
  
  recency = function(data){
    aggregate(date ~ customer, data , min)
  }
  
  frequency = function(data){
    aggregate(date ~ customer, data[,1:2] , length)
  }
  
  monetary = function(data){
    aggregate(revenue ~ customer, data[,c(1,3)] , sum)
  }
  
  score = function(scoreData){
    
    maxQuartile = length(unique(scoreData))
    
    if (maxQuartile > 5){
      maxQuartile = 5
    }
    lowerLimit = (5 - maxQuartile) + 1
    scoreLabel = as.character(cut(scoreData,
                                  breaks = c(quantile(unique(scoreData),probs= seq(0,1,length.out = maxQuartile + 1))),
                                  labels = seq(lowerLimit,5),
                                  include.lowest = TRUE))
    as.numeric(scoreLabel)
  }
  
  
  # Formatting data fields
  data$customer = as.factor(data$customer)
  data$date = as.Date(data$date, '%d/%m/%Y')
  data$revenue = as.numeric(data$revenue)
  
  # Set Analysis Date
  analysisDate = as.Date(analysisDate)
  data$date = as.numeric(difftime(analysisDate,  data$date, units = "days"))
  
  # Calculating Recency, Frequency and Monetary values for each customer
  Recency   = recency(data)
  Frequency = frequency(data)[,2]
  Monetary  = monetary(data)[,2]
  
  rfm_data = cbind(Recency, Frequency, Monetary)
  names(rfm_data)[2] = "Recency"
  
  RFM_Score = do.call(cbind, lapply(rfm_data[,-1], function(x) score(x)))
  RFM_Score[,1] = 6 - RFM_Score[,1]
  RFM_Score = as.numeric(apply(RFM_Score,1,paste, collapse = ''))
  
  data = cbind(rfm_data, RFM_Score)
  
  
  # Sorting data frame in decreasing order of RFM value
  data = data[order(data$RFM_Score, decreasing = TRUE),]
  
  return(data)
}

#' get_CLV_RFM
#' @description  Calculate CLV for RFM
#' @param r Recency value
#' @param f Frequency value
#' @param m the profit a customer can contribute
#' @param n Number of the customer who have the same Recency and Frequency value
#' @param cost The cost accured in each purchasing period to every potential customers who would buy or not buy in the future
#' @param periods How many periods the customer will stay before he/she churn
#' @param dr Discount rate
#' @param pModel The regression model which is used to predict the "buy" rate based on Recency,Frequency and/or Monetary
#' @keywords RFM, CLV
#' @export
#' @examples
#' getCLV(r,f,m,n,cost,periods,dr,pModel)
#' @author Jack Han

get_CLV_RFM <-function(r,f,m,n,cost,periods,dr,pModel){
  
  df<-data.frame(period=c(0),r=c(r),f=c(f),n=c(n),value=c(0))
  
  for(i in 1:periods){
    backstep<-df[df$period==i-1,]
    nrow<-nrow(backstep)
    for(j in 1:nrow){
      r<-backstep[j,]$r
      f<-backstep[j,]$f
      n<-backstep[j,]$n
      p<-predict(pModel,data.frame(Recency=r,Frequency=f),type='response')[1]
      buyers<-n*p
      df<-rbind(df,c(i,0,f+1,buyers,buyers*(m-cost) / (1+dr)^i))
      df<-rbind(df,c(i,r+1,f,n-buyers,(n-buyers)*(-cost)  / (1+dr)^i ))
    }
  }
  
  return(sum(df$value))
}