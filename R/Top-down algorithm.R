

#Input: dataset (dataframe), list of ranked variables (vector of characters), list of functions (vector of characters), 
#variables used by the functions (matrix or dataframe of characters), problem type (vector of characters),threshold or the dimensional limit of the output dataframe (integer), size of the percentile (float between 0 and 0.5)


#Output:
#outcome: optimized values of the functions
#df: the corresponding dataframe
#var_name: the variables used in the segmentation
#var_cut: values of the corresponding cuts
#direction_cut: direction of the cut (greater or smaller than the cut)

# variable used by the functions should be in a matrix. Row i is target by function i. For every row column j is var j. 

# Possible functions mean, ratio ("sum(var1)/sum(var2)"), weighted.mean ("weighted.mean(var1,var2)"), crossratio ("sum(var1*var2)/sum(var3*var4)")

MultiCriteriaTopDown<-function(data,var,metric,metric_variable,opt_type,dim_limit,cut_size){

df<-data

# compute the global performance and apply the functions over the whole dataset 
outcome=numeric()

for (k in 1:length(metric)){
  
  if (metric[k]=='mean'){
    outcome=c(outcome,mean(df[,metric_variable[k,1]]))}
  
  else if (metric[k]=='ratio'){
    outcome=c(outcome,sum(df[,metric_variable[k,1]])/sum(df[,metric_variable[k,2]]))}
  
  else if (metric[k]=='weighted.mean'){
    outcome=c(outcome,weighted.mean(df[,metric_variable[k,1]],df[,metric_variable[k,2]]))} 
  
  else if (metric[k]=='crossratio'){
    outcome=c(outcome,sum(df[,metric_variable[k,1]]*df[,metric_variable[k,2]])/sum(df[,metric_variable[k,3]]*df[,metric_variable[k,4]]))}}

var_cut<-numeric()
direction_cut<-numeric()
var_name<-numeric()
var_check<-NULL   
for (i in 1:length(var)){
q <- quantile(df[,var[i]],probs = seq(0+cut_size, 1-cut_size, cut_size))
    for (j in 1:length(q)){
      q <- quantile(df[,var[i]],probs = seq(0+cut_size, 1-cut_size, cut_size))  
      outcome_up<-numeric()
      outcome_down<-numeric()
      df_copy_up<-df[df[,var[i]]>=q[[j]],]   #segment the data by applying succesive quantile cuts
      df_copy_down<-df[df[var[i]]<q[[j]],]    #parse the upper and lower part of the split 
    
    for (k in 1:length(metric)){
      if (metric[k]=='mean'){  if (dim(df_copy_up)[1]>=dim_limit){      #check the nature of the function
        result_up=mean(df_copy_up[,metric_variable[k,1]]) }
        if (dim(df_copy_down)[1]>=dim_limit){  
          result_down=mean(df_copy_down[,metric_variable[k,1]])}}
      else if (metric[k]=='ratio'){ 
        if (dim(df_copy_up)[1]>=dim_limit){
        result_up=sum(df_copy_up[,metric_variable[k,1]])/sum(df_copy_up[,metric_variable[k,2]])}
        if (dim(df_copy_down)[1]>=dim_limit){  
        result_down=sum(df_copy_down[,metric_variable[k,1]])/sum(df_copy_down[,metric_variable[k,2]])}}
      else if (metric[k]=='weighted.mean'){
        if (dim(df_copy_up)[1]>=dim_limit){  
        result_up=weighted.mean(df_copy_up[,metric_variable[k,1]],df_copy_up[,metric_variable[k,2]])}
        if (dim(df_copy_down)[1]>=dim_limit){  
        result_down=weighted.mean(df_copy_down[,metric_variable[k,1]],df_copy_down[,metric_variable[k,2]])}}
      else if (metric[k]=='crossratio'){
        if (dim(df_copy_up)[1]>=dim_limit){  
        result_up=sum(df_copy_up[metric_variable[k,1]]*df_copy_up[metric_variable[k,2]])/sum(df_copy_up[metric_variable[k,3]]*df_copy_up[metric_variable[k,4]])}
        if (dim(df_copy_down)[1]>=dim_limit){  
        result_down=sum(df_copy_down[metric_variable[k,1]]*df_copy_down[metric_variable[k,2]])/sum(df_copy_down[metric_variable[k,3]]*df_copy_down[metric_variable[k,4]])}}
      if (opt_type[k]=='max'){  #check the nature of the problem
        if (outcome[k]<=result_up & result_down<=result_up){    
          if (dim(df_copy_up)[1]>=dim_limit){   
            outcome_up=c(outcome_up,result_up)   #record the result only if the performance is better over that segment 
              }}
      else if (outcome[k]<=result_down) { 
          if (dim(df_copy_down)[1]>=dim_limit){
            outcome_down=c(outcome_down,result_down)
          }}}
      else if (opt_type[k]=='min'){
          if (outcome[k]>=result_up & result_down>=result_up){
            if (dim(df_copy_up)[1]>=dim_limit){   
              outcome_up=c(outcome_up,result_up)
            }}
          else if (outcome[k]>=result_down) { 
            if (dim(df_copy_down)[1]>=dim_limit){
            outcome_down=c(outcome_down,result_up)
          }}
       
      }} 
      if (length(outcome_up)==length(metric)){   
        outcome<-outcome_up      #if the overall performance over the segment is better then we record the result as the new value to hit
        var_name<-c(var_name,var[i])  # we also keep track of the modifications made
        var_cut<-c(var_cut,q[[j]])
        direction_cut<-c(direction_cut,'+')
        var_check<-var[i]
      }
    else if (length(result_down)==length(metric)){
      outcome<-outcome_down     
      var_name<-c(var_name,var[i])
      var_cut<-c(var_cut,q[[j]])
      direction_cut<-c(direction_cut,'-')
      var_check<-var[i]
    }}
  if (length(direction_cut)>0) {
    if (direction_cut[length(direction_cut)]=='+' & var_check==var[i]){           #the new segment replaces the dataset 
    df<-df[df[var[i]]>=var_cut[length(var_cut)],]}
    else if (direction_cut[length(direction_cut)]=='-' & var_check==var[i]){
    df<-df[df[var[i]]<var_cut[length(var_cut)],]
    }
  }    
   print(dim(df))
} 

return(list('outcome'=outcome,'var_name'=var_name,'var_cut'=var_cut,'direction_cut'=direction_cut,'df'=df))}


df<-data[,c('FICO','BAE201405ProjLoss','PMI6_1','DTIwoProspLoanAllEmp','REV201','SampleWeight')]

var <- c('PMI6_1','FICO','REV201')
metric <- c('mean','weighted.mean')
metric_variable <- rbind(c('BAE201405ProjLoss',1),c('BAE201405ProjLoss','SampleWeight'))
opt_type <- c('max','max')
dim_limit<-100
cut_size<-0.1

t<-MultiCriteriaTopDown(df,var,metric,metric_variable,opt_type,dim_limit,cut_size)

