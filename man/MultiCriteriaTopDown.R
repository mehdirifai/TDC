#TDC_tools

# A function for data segmentation 

MultiCriteriaTopDown(data,
                      var,
                      metric,
                      metric_variable,
                      opt_type,
                      dim_limit,
                      cut_size)
  

# data : A data frame. The dataset to be analyzed and where the cuts are found.
  
# var : A vector. list of ranked variables. 
  
# metric : A vector. list of metrics we want to monitor during the binning. Possible metrics:mean, ratio ("sum(var1)/sum(var2)"), weighted.mean ("weighted.mean(var1,var2)"), crossratio ("sum(var1*var2)/sum(var3*var4)")

# metric_variable : A matrix. matrix of variables used by the metrics. Every row of the matrix is used by one metric. The jth metric uses the jth row of the matrix.   

#opt_type: A vector. Either 'max' or 'min' to specifiy if we are trying to minimize or maximize the metric.

#dim_limit: An integer. Refers to the lower bound dimension of the output dataset.

#cut_size: A float. Number between 0 and 0.5 that refers to the size of the quantiles. 0.1 leading to 10% quantile cuts.


#This function creates segments using a top down algorithm for variables using them in the very specific order they are entered in the fucntion. 

library(tdctools) # load the package
  
  ############
  ## EXAMPLE 1
  ############
  ## Read example data from the package
  data <- read.csv(system.file('extdata', 'sampletest.csv', package="binhypo"), header=TRUE)
  
  ## Input variables
  var <- c('PMI6_1','FICO','REV201')
  metric <- c('mean','weighted.mean')
  metric_variable <- rbind(c('BAE201405ProjLoss',1),c('BAE201405ProjLoss','SampleWeight'))
  opt_type <- c('max','max')
  dim_limit<-100
  cut_size<-0.1
  
  ## Run the function
  MultiCriteriaTopDown(df,var,metric,metric_variable,opt_type,dim_limit,cut_size)
  
  
  ############
  ## EXAMPLE 2
  ############
  ## Read example data from the package
  data <- read.csv(system.file('extdata', 'sampletest.csv', package="binhypo"), header=TRUE)
  
  ## Input variables
  var <- c('PMI6_1','FICO','REV201')
  metric <- c('ratio','crossratio')
  metric_variable <- rbind(c('DTIwoProspLoanAllEmp','REV201'),c('BAE201405ProjLoss','SampleWeight','EALR','SampleWeight'))
  opt_type <- c('min','max')
  dim_limit<-100
  cut_size<-0.1
  
  ## Run the function
  MultiCriteriaTopDown(df,var,metric,metric_variable,opt_type,dim_limit,cut_size)

