library(BART)
library(survey)
library(pps)
library(arules)  ### functions: discretize
library(CHAID)
library(mice)
library(splitstackshape)
library(TruncatedDistributions)
library(glmnet)
library(dbarts)  ## rbart_vi
library(tidyverse)
library(dplyr)
disvretize_fun=function(x,breaks=5){
  x1=discretize(x, method="frequency",breaks = breaks)
  return(x1)
}
## type=0 means norm type=1 means binary
generate_variable_name=function(num=10,type=0){
  variable_name=NULL
  if(type==0){
    for (i in 1:num) {
      variable_name=c(variable_name,paste("x",i,sep = "_"))
    }
  }else{
    for (i in 1:num) {
      variable_name=c(variable_name,paste("z",i,sep = "_"))
    }
  }
  return(variable_name)
}
generate_pop=function(seed=1,percentage=c(0.25,3),y_type=1,n=c(10,8,6,4),num_bin=10,num_norm=10){
  set.seed(seed)
  ## number of strata 
  strata_num=4
  ## strata size 
  n_strata=c(25,20,15,10)
  ## cluster size
  number_of_individual=round(rtexp(sum(n_strata),rate = 1/200,a=100,b=300))
  print(number_of_individual)
  ## total number of individual
  sum_number_of_individual=sum(number_of_individual)
  ## generate strata id and cluster id
  strata_id=NULL
  cluster_id=NULL
  for (i in 1:strata_num) {
    strata_id=c(strata_id,rep(i,n_strata[i]))
  }
  cluster_id=1:sum(n_strata)
  dat=data.frame(strata_id,cluster_id,number_of_individual)
  
  ## generate individual id
  dat=expandRows(dat, "number_of_individual", drop = FALSE) %>% 
    group_by(strata_id,cluster_id) %>%
    dplyr::mutate(individual_id = row_number()) %>% ungroup() 
  
  dat=dat %>%  
    mutate(individual_id=1:nrow(dat))
  
  variable_norm_name=generate_variable_name(num = num_norm,type=0)
  variable_bin_name=generate_variable_name(num = num_bin,type=1)
  for (i in 1:num_norm) {
    x=variable_norm_name[i]
    dat[,x]=round(rnorm(nrow(dat),0,1),3)
  }
  binom_p=runif(num_bin,min=0.4,max=0.6)
  for (i in 1:num_bin) {
    x=variable_bin_name[i]
    dat[,x]=rbinom(nrow(dat),size=1,prob = binom_p[i])
  }
  
  dat[,"strata_y"]=ifelse((dat[,"strata_id"]==1 | dat[,"strata_id"]==2),1,0)
  dat[,"strata_cohort"]=ifelse((dat[,"strata_id"]==2 | dat[,"strata_id"]==3),1,0)
  dat[,"strata_sub"]=ifelse((dat[,"strata_id"]==2 | dat[,"strata_id"]==3),1,0)
  
  s_size=NULL
  for (i in 1:strata_num) {
    s_size[i] = nrow(dat[which(dat$strata_id==i),])
  }
  dat_1 = data.frame(strata_id=1:strata_num, strata_size=s_size)
  
  base_weight=NULL
  for(i in 1:strata_num){
    cluster_size=dat[which(dat$strata_id==i),]$number_of_individual
    selected_cluster_num=n[i]
    str_size=dat_1$strata_size[i]
    base_weight=c(base_weight,1/(cluster_size*selected_cluster_num/str_size))
  }
  
  dat_2=dat %>% mutate(
    base_weight=base_weight)
  
  if(y_type==0){
    coef_intercpt=0
    dat_3=dat_2 %>% 
      mutate(
        z=coef_intercpt-2*x_1+(x_2)^2+2*z_1-z_2-2*z_3+x_1*z_1)
    
    dat_4=unique(dat_3[,c("strata_id","cluster_id")]) %>% 
      mutate(ran_intercept=rnorm(nrow(unique(dat_3[,c("strata_id","cluster_id")])),0,0.5))
    dat_5=inner_join(dat_3,dat_4,by=c("strata_id","cluster_id"))
    
    dat_5=dat_5 %>% 
      mutate(
        z=z+ran_intercept,
        pr=1/(1+exp(-z)),
        y = rbinom(nrow(dat_5),size=1,prob=pr)
      )
    perc=sum(dat_5$y)/nrow(dat_5)
    
    while( abs(perc-percentage[1]) > 0.001 ){
      if( (perc-percentage[1])>0.001 ) {
        coef_intercpt=coef_intercpt-0.1
        dat_3=dat_2 %>% 
          mutate(
            z=coef_intercpt-2*x_1+(x_2)^2+2*z_1-z_2-2*z_3+x_1*z_1)
        
        dat_4=unique(dat_3[,c("strata_id","cluster_id")]) %>% 
          mutate(ran_intercept=rnorm(nrow(unique(dat_3[,c("strata_id","cluster_id")])),0,0.5))
        dat_5=inner_join(dat_3,dat_4,by=c("strata_id","cluster_id"))
        
        dat_5=dat_5 %>% 
          mutate(
            z=z+ran_intercept,
            pr=1/(1+exp(-z)),
            y = rbinom(nrow(dat_5),size=1,prob=pr)
          )
        perc=sum(dat_5$y)/nrow(dat_5)
        print(c(perc,coef_intercpt))
      } else
      {
        coef_intercpt=coef_intercpt+0.1
        
        dat_3=dat_2 %>% 
          mutate(
            z=coef_intercpt-2*x_1+(x_2)^2+2*z_1-z_2-2*z_3+x_1*z_1)
        
        dat_4=unique(dat_3[,c("strata_id","cluster_id")]) %>% 
          mutate(ran_intercept=rnorm(nrow(unique(dat_3[,c("strata_id","cluster_id")])),0,0.5))
        dat_5=inner_join(dat_3,dat_4,by=c("strata_id","cluster_id"))
        
        dat_5=dat_5 %>% 
          mutate(
            z=z+ran_intercept,
            pr=1/(1+exp(-z)),
            y = rbinom(nrow(dat_5),size=1,prob=pr)
          )
        perc=sum(dat_5$y)/nrow(dat_5)
        print(c(perc,coef_intercpt))
      }
    }
    print(c(perc,coef_intercpt))
    dat_5=dat_5 %>% dplyr::select(-z,-ran_intercept)
  }
  else{
    coef_intercpt=-2
    dat_3=dat_2 %>% 
      mutate(
        z=coef_intercpt-2*x_1+(x_2)^2+2*z_1-z_2-2*z_3+x_1*z_1)
    
    dat_4=unique(dat_3[,c("strata_id","cluster_id")]) %>% 
      mutate(ran_intercept=rnorm(nrow(unique(dat_3[,c("strata_id","cluster_id")])),0,1))
    dat_5=inner_join(dat_3,dat_4,by=c("strata_id","cluster_id"))
    
    dat_5=dat_5 %>% 
      mutate(
        z=z+ran_intercept,
        pr=1/(1+exp(-z)),
        y = rnorm(nrow(dat_5),z,1)
      )
    perc=sum(dat_5$y)/nrow(dat_5)
    while (abs(perc-percentage[2])>0.01) {
      if((perc-percentage[2])>0.01) {
        coef_intercpt=coef_intercpt-0.01
        dat_3=dat_2 %>% 
          mutate(
            z=coef_intercpt-2*x_1+(x_2)^2+2*z_1-z_2-2*z_3+x_1*z_1)
        
        dat_4=unique(dat_3[,c("strata_id","cluster_id")]) %>% 
          mutate(ran_intercept=rnorm(nrow(unique(dat_3[,c("strata_id","cluster_id")])),0,1))
        dat_5=inner_join(dat_3,dat_4,by=c("strata_id","cluster_id"))
        
        dat_5=dat_5 %>% 
          mutate(
            z=z+ran_intercept,
            pr=1/(1+exp(-z)),
            y = rnorm(nrow(dat_5),z,1)
          )
        perc=sum(dat_5$y)/nrow(dat_5)
        print(c(perc,coef_intercpt))
      } else
      {
        coef_intercpt=coef_intercpt+0.01
        dat_3=dat_2 %>% 
          mutate(
            z=coef_intercpt-2*x_1+(x_2)^2+2*z_1-z_2-2*z_3+x_1*z_1)
        
        dat_4=unique(dat_3[,c("strata_id","cluster_id")]) %>% 
          mutate(ran_intercept=rnorm(nrow(unique(dat_3[,c("strata_id","cluster_id")])),0,1))
        dat_5=inner_join(dat_3,dat_4,by=c("strata_id","cluster_id"))
        
        dat_5=dat_5 %>% 
          mutate(
            z=z+ran_intercept,
            pr=1/(1+exp(-z)),
            y = rnorm(nrow(dat_5),z,1)
          )
        perc=sum(dat_5$y)/nrow(dat_5)
        print(c(perc,coef_intercpt))
      }
    }
    
    dat_5=dat_5 %>% dplyr::select(-z,-ran_intercept)
  }
  
  dat_5$SEQN=1:nrow(dat_5)
  
  return(dat_5)
}
generate_cohort=function(dat,n=c(10,8,6,4),percentage=0.6,variable_norm_name,variable_bin_name){
  dat_1=unique(dat %>% dplyr::select(strata_id,cluster_id,number_of_individual))
  dat_1=dat_1%>% 
    mutate(order=1:nrow(dat_1))
  ## select mh EAs  from  each  stratum by PPS
  selected_cluster=ppssstrat(dat_1$number_of_individual,dat_1$strata_id,n)
  
  dat_2=dat_1[which(dat_1$order %in% selected_cluster),] %>% dplyr::select(-order)
  dat_3=left_join(dat_2,dat,by=c("strata_id","cluster_id","number_of_individual"))
  
  coef_intercpt=-1
  dat_1=dat_3 %>% 
    mutate(
      z=coef_intercpt+2*z_1+2*z_2-1*z_3)
  
  ##  dat_2=unique(dat_3[,c("strata_id","cluster_id")]) %>% 
  ##    mutate(ran_intercept=rnorm(nrow(unique(dat_3[,c("strata_id","cluster_id")])),0,1))
  ##  dat_1=inner_join(dat_1,dat_2,by=c("strata_id","cluster_id"))
  
  dat_1=dat_1 %>% 
    mutate(
      ##  z=z+ran_intercept,
      pr=1/(1+exp(-z)),
      res_cohort = rbinom(nrow(dat_1),1,pr)
    )
  
  perc=sum(dat_1$res_cohort)/nrow(dat_1)
  
  dat_1=dat_1 %>% dplyr::select(-z,-pr)  
  dim(unique(dat_1[which(dat_1$res_cohort==1),c("strata_id","cluster_id")]))
  cohort_data=dat_1[which(dat_1$res_cohort==1),] %>% dplyr::select(-number_of_individual,-res_cohort)
  dat_2=dat_1 %>% 
    dplyr::select("z_1","z_2","z_3","base_weight","res_cohort") %>% 
    mutate(base_weight = log(base_weight)) %>% 
    mutate_at("base_weight",list(~disvretize_fun(.))) %>% 
    mutate_at(c("z_1","z_2","z_3","base_weight","res_cohort"),list(~as.factor(.)))
  
  ctrl <- chaid_control(minsplit = 200, minprob = 1,minbucket = 100)
  chaidsurv <- chaid(res_cohort~., data = dat_2, control = ctrl)
  dat_3=dat_2[which(dat_1$res_cohort==1),]
  res_rate=predict.party(chaidsurv,newdata = dat_3,type="prob")[,2]
  cohort_data=cohort_data %>% mutate(cohort_weight=base_weight*(1/res_rate)) 
  return(cohort_data)}
extract_str=function(naming){
  value1="x"
  value2="z"
  value3="strata"
  value4="base_weight"
  value5="weight_predictor"
  if(grepl(value1,naming,fixed = TRUE)){
    val=naming}else
      if(grepl(value2,naming,fixed = TRUE)){
        val=substr(naming,1,nchar(naming)-1)}else
          if(grepl(value3,naming,fixed = TRUE)){
            val=substr(naming,1,nchar(naming)-1)}else
              if(grepl(value4,naming,fixed = TRUE)){
                val=naming}else
                  if(grepl(value5,naming,fixed = TRUE)){
                    val=naming}else{
                      val=NULL
                    }
  return(val=val)
}
test_con=function(selected_var){
  con_seq=NULL
  for (i in 1:length(selected_var)) {
    if(grepl("x",selected_var[i],fixed=TRUE)|grepl("base_weight",selected_var[i],fixed=TRUE)|grepl("weight_predictor",selected_var[i],fixed=TRUE)){
      con=TRUE
      con_seq=c(con_seq,i)}
    if(grepl("z",selected_var[i],fixed=TRUE)|grepl("strata",selected_var[i],fixed=TRUE)){con=FALSE}
  }
  return(con_seq)
}
generate_sub_cohort=function(cohort_data,percentage=0.4,weight_method="LGM",scenario,select_rate=0.5,variable_norm_name,variable_bin_name){
  coef_intercpt=1
  ## onlye 50% of the cohort_data are selected
  selected_cohort=sample(1:nrow(cohort_data),ceiling(0.5*nrow(cohort_data)))
  cohort_data_2=cohort_data[selected_cohort,]
  if(scenario==1|scenario==2){
    dat_1=cohort_data_2 %>% 
      mutate(
        z=coef_intercpt + 2*x_1 + 1.5*(x_2)^2 + 2*z_1 + 1*z_2 - 2*z_3 - x_1*z_1)
  }
  if(scenario==3){
    dat_1=cohort_data_2 %>% 
      mutate(
        z=coef_intercpt + 2*x_1 - 1.5*(x_2)^2 + 2*z_1 + 1*z_2 - 2*z_3 - x_1*z_1)
  }  
  if(scenario==4){
    dat_1=cohort_data_2 %>% 
      mutate(
        z=coef_intercpt + 2*x_1 - 1.5*(x_3)^2 + 2*z_1 + 1*z_2 - 2*z_3 - x_1*z_1)
  }
  
  dat_1=dat_1 %>% 
    mutate(
      pr=1/(1+exp(-z)),
      res_sub = rbinom(nrow(dat_1),1,pr)
    )
  
  perc=sum(dat_1$res_sub)/nrow(dat_1)
  dat_1=dat_1 %>% dplyr::select(-z,-pr)
  dim(unique(dat_1[which(dat_1$res_sub==1),c("strata_id","cluster_id")]))
  sub_cohort=dat_1[which(dat_1$res_sub==1),] %>% dplyr::select(-res_sub)
  
  if(weight_method=="LGM"){
    dat_2=dat_1 %>% 
      dplyr::select(all_of(variable_norm_name),all_of(variable_bin_name),"base_weight","res_sub") %>% 
      mutate_at(c(variable_bin_name),list(~ as.factor(.))) %>% 
      mutate(base_weight=log(base_weight)) 
    
    ## when number of variables is large, use lasso to select variables
      x <- model.matrix(res_sub~., dat_2)[,-1]
      y <- dat_2$res_sub
      cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
      model <- glmnet(x, y, alpha = 1, family = "binomial",
                      lambda = cv.lasso$lambda.min)
      
      x_new = model.matrix(~., cohort_data %>% 
                             dplyr::select(all_of(variable_norm_name),all_of(variable_bin_name),"base_weight") %>% 
                             mutate_at(c(variable_bin_name),list(~ as.factor(.))) %>% 
                             mutate(base_weight=log(base_weight)))[,-1]
      
      res_rate_cohort <- model %>% predict(newx = x_new,type="response")
      res_rate = model %>% predict(newx = x,type="response")
    
    sub_cohort_notrim=sub_cohort %>% mutate(sub_cohort_weight=cohort_weight*(1/res_rate[which(dat_2$res_sub==1)])) 
    
    dat_3=sub_cohort_notrim %>% mutate(weight=sub_cohort_weight/cohort_weight)
    
    survey_1<-svydesign(id=~cluster_id, weights=~weight, data=dat_3,strata = ~strata_id,nest = TRUE) 
    
    ## trim weights
    pre_sub_weights=weights(survey_1)
    bound= median(pre_sub_weights)+4*IQR(pre_sub_weights)
    dclus1t<-trimWeights(survey_1,lower=0, upper=bound)
    res_rate_trim=weights(dclus1t)
    sub_cohort_trim=sub_cohort %>% mutate(sub_cohort_weight=res_rate_trim*cohort_weight) 
  }
  else if(weight_method=="BART"){
    dat_2=as.data.frame(dat_1 %>% dplyr::select(c(all_of(variable_norm_name),all_of(variable_bin_name),"strata_id","base_weight"))%>% 
                          mutate_at("strata_id",list(~ as.factor(.)))) %>% 
      mutate(base_weight=log(base_weight))
    
    test_dat = as.data.frame(cohort_data %>% dplyr::select(c(all_of(variable_norm_name),all_of(variable_bin_name),"strata_id","base_weight"))%>% 
                               mutate_at("strata_id",list(~ as.factor(.)))) %>% 
      mutate(base_weight=log(base_weight))
    
    bartFit = gbart(dat_2,dat_1$res_sub,test_dat,type = 'pbart',printevery = 10000,mc.cores = 4)
    res_rate_cohort=bartFit$prob.test.mean
    res_rate=res_rate_cohort[selected_cohort]
    
    sub_cohort_notrim=sub_cohort %>% mutate(sub_cohort_weight=cohort_weight*(1/res_rate[which(dat_1$res_sub==1)])) 
    dat_3=sub_cohort_notrim %>% mutate(weight=sub_cohort_weight/cohort_weight)
    
    survey_1<-svydesign(id=~cluster_id, weights=~weight, data=dat_3,strata = ~strata_id,nest = TRUE) 
    
    ## trim weights
    pre_sub_weights=weights(survey_1)
    bound= median(pre_sub_weights)+4*IQR(pre_sub_weights)
    dclus1t<-trimWeights(survey_1,lower=0, upper=bound)
    res_rate_trim=weights(dclus1t)
    sub_cohort_trim=sub_cohort %>% mutate(sub_cohort_weight=res_rate_trim*cohort_weight) 
  }
  else if(weight_method=="RBART"){
    dat_2=dat_1 %>% dplyr::select(c(all_of(variable_norm_name),all_of(variable_bin_name),"strata_id","res_sub","base_weight"))%>% 
      mutate_at("strata_id",list(~ as.factor(.))) %>% 
      mutate(base_weight=log(base_weight))
    
    rbartFit <- rbart_vi(res_sub ~ . - strata_id, dat_2, group.by = dat_2$strata_id,
                         n.samples = 1000L, n.burn = 100L, n.chains = 1L,
                         n.trees = 100L, keepTrees = TRUE,n.thin = 1)
    dat_3=dat_2[which(dat_1$res_sub==1),]
    
    test_dat=cohort_data %>% dplyr::select(c(all_of(variable_norm_name),all_of(variable_bin_name),"strata_id","base_weight"))%>% 
      mutate_at("strata_id",list(~ as.factor(.))) %>% 
      mutate(base_weight=log(base_weight))
    
    pred=predict(rbartFit,newdata=test_dat,group.by=test_dat$strata_id)
    dat_4=as.data.frame(pred)
    res_rate_cohort=apply(dat_4,2,mean)
    res_rate=res_rate_cohort[selected_cohort]
    sub_cohort_notrim=sub_cohort %>% mutate(sub_cohort_weight=cohort_weight*(1/res_rate[which(dat_2$res_sub==1)])) 
    dat_3=sub_cohort_notrim %>% mutate(weight=sub_cohort_weight/cohort_weight)
    
    survey_1<-svydesign(id=~cluster_id, weights=~weight, data=dat_3,strata = ~strata_id,nest = TRUE) 
    
    ## trim weights
    pre_sub_weights=weights(survey_1)
    bound= median(pre_sub_weights)+4*IQR(pre_sub_weights)
    dclus1t<-trimWeights(survey_1,lower=0, upper=bound)
    res_rate_trim=weights(dclus1t)
    sub_cohort_trim=sub_cohort %>% mutate(sub_cohort_weight=res_rate_trim*cohort_weight) 
  }
  else if(weight_method=="CHAID"){
    dat_2=dat_1 %>% 
      dplyr::select(all_of(variable_norm_name),all_of(variable_bin_name),"base_weight","res_sub") %>% 
      mutate_at(c(variable_bin_name),list(~ as.factor(.)))  %>% 
      mutate(base_weight=log(base_weight))
    # Dumy code categorical predictor variables
    x <- model.matrix(res_sub~., dat_2)[,-1]
    # Convert the outcome (class) to a numerical variable
    y <- dat_2$res_sub
    cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
    coef_dat = as.data.frame(as.matrix(coef(cv.lasso,s='lambda.min',exact=TRUE)))
    colnames(coef_dat)="coef"
    coef_dat = coef_dat %>% filter(coef!=0) 
    x1=rownames(coef_dat)
    selected_var=NULL
    for (var_num in 1:length(x1)) {
      selected_var=c(selected_var,extract_str(x1[var_num]))
    }
    x2=unique(selected_var)
    dat_3=cohort_data %>% dplyr::select(c(all_of(selected_var)))%>% 
      mutate_at(all_of(selected_var[test_con(selected_var)]),list(~disvretize_fun(.))) %>% 
      mutate_at(c(selected_var),list(~as.factor(.)))
    dat_4=dat_3[selected_cohort,] %>% mutate(
      res_sub=as.factor(dat_2$res_sub)
    )
    ctrl <- chaid_control(minsplit = 50, minprob = 1,minbucket = 25)
    chaidsurv <- chaid(res_sub~., data = dat_4, control = ctrl)
    
    res_rate_cohort=predict.party(chaidsurv,newdata = dat_3,type="prob")[,2]
    res_rate=res_rate_cohort[selected_cohort]
    sub_cohort_notrim=sub_cohort %>% mutate(sub_cohort_weight=cohort_weight*(1/res_rate[which(dat_1$res_sub==1)])) 
    sub_cohort_trim=NULL
    res_rate_trim=NULL
  }
  return(list(sub_cohort_notrim=sub_cohort_notrim,sub_cohort_trim=sub_cohort_trim,
              predicted_cohort_res=res_rate_cohort,
              predicted_sub_res_notrim=res_rate[which(dat_1$res_sub==1)],
              predicted_sub_res_trim=res_rate_trim[which(dat_1$res_sub==1)],select_cohort=cohort_data_2))
}
## bayesian bart sub to cohort
logistic <- function(x) {log(x/(1-x)) }
## bart_type==0 bart 1 sbart 2 rbart
#### when the outcome is continuous, the code is not correct
bayesian_bart_sub_cohort=function(sub_cohort,cohort_data,y_type=0,bart_type=0,weights=FALSE,
                                  predicted_cohort_res=NULL,predicted_sub_res=NULL,multiple_num=multiple_num,
                                  variable_norm_name = variable_norm_name, variable_bin_name = variable_bin_name){
  dat_1=sub_cohort
  dat_1$propensity_predictor=1/predicted_sub_res
  
  dat_1=dat_1 %>% dplyr::select(c(all_of(variable_norm_name),all_of(variable_bin_name),
                                  "cluster_id","y","strata_id","propensity_predictor","cohort_weight")) %>% 
    mutate_at(c("cluster_id","strata_id"),list(~as.factor(as.numeric(as.factor(.))))) %>% 
    mutate(propensity_predictor=log(propensity_predictor),
           cohort_weight=log(cohort_weight)) 
  
  
  dat_2=cohort_data
  dat_2$propensity_predictor=1/predicted_cohort_res
  dat_2=as.data.frame(dat_2 %>% dplyr::select(c(all_of(variable_norm_name),all_of(variable_bin_name),
                                                "cluster_id","strata_id","propensity_predictor","cohort_weight")) %>% 
                        mutate_at(c("cluster_id","strata_id"),list(~as.factor(as.numeric(as.factor(.))))))  %>% 
    mutate(propensity_predictor=log(propensity_predictor),
           cohort_weight=log(cohort_weight)) 
  if(y_type==0){
    if(bart_type==0){
      bartFit = gbart(x.train=as.data.frame(dat_1 %>% dplyr::select(-y)),y.train=dat_1$y,x.test=dat_2,type = 'pbart',printevery = 10000,mc.cores = 4)
      dat_3=as.data.frame(bartFit$prob.test)
    }
    else if(bart_type==2){
      rbartFit <- rbart_vi(y ~ . - cluster_id, dat_1, group.by = dat_1$cluster_id,
                           n.samples = 10000L, n.burn = 100L, n.chains = 1L,
                           n.trees = 50L, n.threads = 4L,keepTrees = TRUE,printEvery = 10000,n.thin = 10)
      pred=predict(rbartFit,newdata=dat_2,group.by=dat_2$cluster_id)
      dat_3=as.data.frame(pred)
    }
  }
  else if(y_type==1){
    if(bart_type==0){
      bartFit = gbart(as.data.frame(dat_1 %>% dplyr::select(-y)),dat_1$y,as.data.frame(dat_2),type = 'wbart',
                      mc.cores = 4,keepevery=1,ntree = 100)
      dat_3=as.data.frame(bartFit$yhat.test)
    }
    else if(bart_type==2){
      rbartFit <- rbart_vi(y ~ . - cluster_id, dat_1, group.by = dat_1$cluster_id,
                           n.samples = 1000L, n.burn = 100L, n.chains = 1,
                           n.trees = 100L, keepTrees = TRUE,n.thin = 1)
      pred=predict(rbartFit,newdata=dat_2,group.by=dat_2$cluster_id)
      dat_3=as.data.frame(pred)
    }
  }
  mean_y_post=NULL
  y_post=matrix(rep(NA,nrow(dat_2)*nrow(dat_3)),nrow = nrow(dat_2),ncol = nrow(dat_3))
  col_names=NULL
  for (i in seq_len(nrow(dat_3))) {
    if(y_type==0){
      dat=dat_2 %>% 
        mutate(
          y=rbinom(nrow(dat_2),size=1,prob=as.numeric(dat_3[i,])),
          strata_id=cohort_data$strata_id,
          cluster_id=cohort_data$cluster_id,
          individual_id=cohort_data$individual_id,
          number_id=paste(strata_id,cluster_id,individual_id,sep="_")
        )
      
      dat_1=sub_cohort %>% dplyr::select(strata_id,cluster_id,individual_id,y) %>% 
        mutate(number_id=paste(strata_id,cluster_id,individual_id,sep="_"))
      
      dat[which(dat$number_id %in% dat_1$number_id),"y"] <- dat_1$y
    }
    else{
      dat=dat_2 %>% 
        mutate(
          y=as.numeric(dat_3[i,]),
          strata_id=cohort_data$strata_id,
          cluster_id=cohort_data$cluster_id,
          individual_id=cohort_data$individual_id,
          number_id=paste(strata_id,cluster_id,individual_id,sep="_")
        )
      dat_1=sub_cohort %>% dplyr::select(strata_id,cluster_id,individual_id,y) %>% 
        mutate(number_id=paste(strata_id,cluster_id,individual_id,sep="_")) %>% 
        arrange(individual_id)
      
      dat[which(dat$number_id %in% dat_1$number_id),"y"] <- dat_1$y
    }
    mean_y_post=c(mean_y_post,mean(dat$y))
    y_post[,i]=dat$y
    col_names=c(col_names,paste("y",i,sep = "_"))
  }
  colnames(y_post)=col_names
  
  bart_cohort_data=dat %>% dplyr::select(strata_id,cluster_id,individual_id) %>% mutate(cohort_weight=cohort_data$cohort_weight)
  
  bart_cohort_data=cbind(bart_cohort_data,y_post)
  return(list(bart_cohort_data=bart_cohort_data))
}
## MI BART sub to coh
MI_bart_sub_cohort=function(sub_cohort,cohort_data,cohort_data_list,multiple_num=10){
  dat_2=cohort_data %>% dplyr::select(c(all_of(variable_norm_name),all_of(variable_bin_name)),"strata_id","cluster_id","individual_id","cohort_weight")
  dat_4=cohort_data_list[,-(1:4)]
  multiple_y=dat_4[,seq(1,ncol(dat_4),length.out = multiple_num)]
  dat_3=list()
  for(i in 1:multiple_num){dat_3[[i]]=cbind(dat_2,y=multiple_y[,i])}
  
  estimate=summary(pool(map(dat_3,function(x){fit=lm(y~1,data = x)
  return(fit)})))
  
  mean_predicted_y=estimate$estimate
  confidence_interval=c(mean_predicted_y-qt(0.975,estimate$df)*estimate$std.error,mean_predicted_y+qt(0.975,estimate$df)*estimate$std.error)
  mean_y=mean(cohort_data$y)
  cover=as.numeric(mean_y>confidence_interval[1] & mean_y<confidence_interval[2])
  square_error=(mean_predicted_y-mean_y)^2
  bias=mean_predicted_y-mean_y
  relative_bias=abs((mean_predicted_y-mean_y)/mean_y)
  width_conf=confidence_interval[2]-confidence_interval[1]
  
  return(list(mean_pre=mean_predicted_y,cover=cover,square_error=square_error,width_conf=width_conf,
              mean_real=mean_y,bias=bias,relative_bias=relative_bias,true_mean_y=mean_y))
}
# cohort to population
### weight true cohort to population 
weight_cohort_pop=function(cohort_data,finite_pop,y_type=0){
  survey_1<-svydesign(id=~cluster_id, weights=~cohort_weight, data=cohort_data,strata = ~strata_id,nest = TRUE) 
  ### using replicate weights
  if(y_type==0){
    outcome_1=svyciprop(~y, survey_1)
    mean_pre_y=as.vector(outcome_1)
    ci_1=as.vector(attr(outcome_1, "ci"))
  }else{
    mean_pre_y=svymean(~y,survey_1)
    ci_1=confint(mean_pre_y)
  }
  mean_y=mean(finite_pop$y)
  cover=as.numeric(mean_y>ci_1[1] & mean_y<ci_1[2])
  square_error=(mean_pre_y[1]-mean_y)^2
  bias=(mean_pre_y[1]-mean_y)
  relative_bias=abs((mean_pre_y[1]-mean_y)/mean_y)
  width_conf=ci_1[2]-ci_1[1]
  return(list(mean_pre=mean_pre_y[1],cover=cover,square_error=square_error,width_conf=width_conf, 
              bias= bias,relative_bias=relative_bias,true_mean_y=mean_y))
}
### bayesian bart weight cohort
MI_bart_sub_pop=function(cohort_data_list,finite_pop,multiple_num=10,y_type=0){
  mean_y_post=NULL
  variance_post=NULL
  selected_y=sample(1:(ncol(cohort_data_list)-4),multiple_num)
  for (i in 1:multiple_num) {
    ord_y=paste("y",selected_y[i],sep = "_")
    dat=cohort_data_list %>% dplyr::select("strata_id","cluster_id","individual_id","cohort_weight",all_of(ord_y))
    colnames(dat)=c("strata_id","cluster_id","individual_id","cohort_weight","y")
    survey_1<-svydesign(id=~cluster_id, weights=~cohort_weight, data=dat,strata = ~strata_id,nest = TRUE) 
    ### 
    if(y_type==0){
      mean_y_post[i]=as.vector(svyciprop(~y,survey_1))
      variance_post[i]=vcov(svyciprop(~y,survey_1))
    }else{
      mean_y_post[i]=coef(svymean(~y,survey_1))
      variance_post[i]=vcov(svymean(~y,survey_1))
    }
  }
  mean_pre=mean(mean_y_post)
  v_bar=mean(variance_post)
  b=var(mean_y_post)
  var_multiple=v_bar+(1+1/multiple_num)*b
  d=(multiple_num-1)*(1+multiple_num*v_bar/((multiple_num+1)*b))^2
  ci_1=c(mean_pre-qt(0.975,d)*sqrt(var_multiple),mean_pre+qt(0.975,d)*sqrt(var_multiple))
  mean_y=mean(finite_pop$y)
  cover=as.numeric(mean_y>ci_1[1] & mean_y<ci_1[2])
  square_error=(mean_pre-mean_y)^2
  bias=(mean_pre-mean_y)
  width_conf=ci_1[2]-ci_1[1]
  relative_bias=abs((mean_pre-mean_y)/mean_y)
  return(list(mean_pre=mean_pre,cover=cover,square_error=square_error,width_conf=width_conf,
              bias=bias,relative_bias=relative_bias,true_mean_y=mean_y,withinvariance=v_bar,
              betweenvariance=(1+1/multiple_num)*b))
}
## sub to population
weight_sub_pop=function(sub_cohort,finite_pop,y_type=0,cohort_data){
  dat_1=sub_cohort %>% mutate(in_sub_cohort=1)
  One=left_join(cohort_data,dat_1[,c("SEQN","in_sub_cohort","sub_cohort_weight")],by="SEQN")
  One$inAnalysis=ifelse(is.na(One$in_sub_cohort),0,1)
  One$sub_cohort_weight=ifelse(is.na(One$sub_cohort_weight),0,One$sub_cohort_weight)
  
  svy_1=svydesign(data=One, id=~cluster_id, strata=~strata_id, weights=~sub_cohort_weight, nest=TRUE) 
  svy_2 <- subset(svy_1, inAnalysis==1)
  
  ### using replicate weights
  if(y_type==0){
    outcome_1=svyciprop(~y, svy_2)
    mean_pre_y=as.vector(outcome_1)
    ci_1=as.vector(attr(outcome_1, "ci"))
  }else{
    mean_pre_y=svymean(~y,svy_2)
    ci_1=confint(mean_pre_y)
  }
  
  mean_y=mean(finite_pop$y)
  cover=as.numeric(mean_y>ci_1[1] & mean_y<ci_1[2])
  square_error=(mean_pre_y[1]-mean_y)^2
  bias=(mean_pre_y[1]-mean_y)
  width_conf=ci_1[2]-ci_1[1]
  relative_bias=abs((mean_pre_y[1]-mean_y)/mean_y)
  return(list(mean_pre=mean_pre_y[1],cover=cover,square_error=square_error,
              width_conf=width_conf,bias=bias,relative_bias=relative_bias,true_mean_y=mean_y))
}










