library(dplyr)
library(survey)
library(nhanesA)
library(mice)
library(tidyverse)
library(BART)

## load data
{
  demo <-read.csv("./application/data/demo.csv")
  demo_1 = demo %>% 
    mutate(
      gender_group=factor(RIAGENDR),
      age_group = as.factor(cut(RIDAGEYR, breaks=c(-1,11,19,39,59,Inf),labels=c(1,2,3,4,5))), ##(0,80)
      race_group= as.factor(RIDRETH3),  ## 1: Mexican American 2: Other Hispanic 3: Non-Hispanic White 4: Non-Hispanic Black 6:Non-Hispanic Asian 7:Other Race - Including Multi-Racial
      citizen_group=ifelse(is.na(DMDCITZN),9,DMDCITZN),
      citizen_group=factor(citizen_group,levels=c(1,2,7,9),labels=c(1,2,3,3)), ## 1; Citizen by birth or naturalization 2:Not a citizen of the US 3: Refused Don't Know or Missing
      length_inUSA_group=ifelse(is.na(DMDYRSUS),99,DMDYRSUS), 
      length_inUSA_group=factor(length_inUSA_group,levels=c(1,2,3,4,5,6,7,8,9,77,99),labels = c(1,1,1,1,2,2,3,3,3,4,4)), ## 1:<=15 years 2:15-30 years 3:30-50 years 4: refused or missing or don't know
      educ_group = ifelse(is.na(DMDEDUC2),9,DMDEDUC2),
      educ_group = factor(educ_group,levels = c(1,2,3,4,5,7,9),labels = c(1,1,1,2,2,3,3)), ## 1: <= high school graduate or less 2: college graduate or above 3: refused don't know or missing
      marital_group=as.factor(ifelse(is.na(DMDMARTL)|DMDMARTL==77|DMDMARTL==99,7,DMDMARTL)), ##1	Married	2	Widowed	3	Divorced	4	Separated	5	Never married	6	Living with partner	7	Refused	Don't Know Missing
      HH_size_group=as.factor(DMDFMSIZ), 
      cohort_weight=WTMEC2YR,
      ratio_family_income_group = ifelse(is.na(INDFMPIR),9,INDFMPIR),
      ratio_family_income_group=factor(cut(ratio_family_income_group, breaks=c(-1,1,2,3,4,5,9),labels=c(1,2,3,4,5,6))),
      cluster=SDMVPSU,
      strata=SDMVSTRA,
      SEQN=as.numeric(SEQN)
    ) %>% 
    dplyr::select("SEQN","strata","cluster",ends_with("group"),"cohort_weight")

  ## BMI
  body_measures=read_csv("./application/data/body_measures.csv")
  body_measures=body_measures%>% 
    dplyr::select(SEQN,BMXBMI) %>% 
    mutate(SEQN=as.numeric(SEQN))
  
  dia <- read_csv("./application/data/dia.csv")
  dia = dia %>% dplyr::select(SEQN,DIQ010,DIQ160,DIQ170,DIQ172) 
  ## DIQ010 - Doctor told you have diabetes 
  #DIQ160 - Ever told you have prediabetes 
  #DIQ170 - Ever told have health risk for diabetes 
  #DIQ172 Feel could be at risk for diabetes
  dia = dia %>% 
    mutate(
      DIQ160=as.factor(ifelse(is.na(DIQ160),9,DIQ160)),
      DIQ160=factor(DIQ160,levels = c(1,2,7,9),labels = c(1,0,2,2)),
      DIQ170=as.factor(ifelse(is.na(DIQ170),9,DIQ170)),
      DIQ170=factor(DIQ170,levels = c(1,2,7,9),labels = c(1,0,2,2)),
      DIQ172=as.factor(ifelse(is.na(DIQ172),9,DIQ172)),
      DIQ172=factor(DIQ172,levels = c(1,2,7,9),labels = c(1,0,2,2)),
      diagnosed_diabetes=ifelse(is.na(DIQ010),9,DIQ010),
      diagnosed_diabetes=ifelse(diagnosed_diabetes==1,1,0),
      SEQN=as.numeric(SEQN)
    ) %>% dplyr::select(-DIQ010)
  
  
  hem=read_csv("./application/data/hem.csv")
  hem=hem%>% 
    mutate(SEQN=as.numeric(SEQN))
  
  cho=read_csv("./application/data/cho.csv")
  cho=cho %>% 
    select(SEQN,LBDHDDSI) %>% 
    mutate(SEQN=as.numeric(SEQN))
 
  blood <- read_csv("./application/data/blood.csv")
  ## BPQ020:Ever told you had high blood pressure 
  ## BPQ080:Doctor told you - high cholesterol level
  blood=blood %>% 
    select(SEQN,BPQ020) %>% 
    mutate(
      BPQ020=as.factor(ifelse(is.na(BPQ020),9,BPQ020)),
      BPQ020=factor(BPQ020,levels = c(1,2,7,9),labels = c(1,0,2,2)),
      SEQN=as.numeric(SEQN))
  
  alc=read_csv("./application/data/alc.csv")
  ## ALQ151 - Ever have 4/5 or more drinks every day? 1: yes 2: no 7/9: don't know refused or missing
  alc=alc %>% 
    select(SEQN,ALQ151) %>% 
    mutate(
      ALQ151=as.factor(ifelse(is.na(ALQ151),9,ALQ151)),
      ALQ151=factor(ALQ151,levels = c(1,2,7,9),labels = c(1,0,2,2)),
      SEQN=as.numeric(SEQN)
    ) 
  
  smoke=read_csv("./application/data/smoke.csv")
  ## SMQ040 - Do you now smoke cigarettes? # 1 Every day 2 someday 3 not at all 7 refused 9 don't know
  smoke=smoke %>% dplyr::select(SEQN,SMQ040,SMQ020) %>% 
    mutate(
      SMQ040=ifelse(SMQ020==2,3,SMQ040),
      SMQ040=ifelse(is.na(SMQ040),9,SMQ040),
      SMQ040=factor(SMQ040,levels = c(1,2,3,7,9),labels = c(1,2,3,4,4)),
      SEQN=as.numeric(SEQN)
    )

  ##### fasting glucose level
  glu = read_csv("./application/data/glu.csv")
  glu=glu%>% mutate(
    sub_cohort_weight=WTSAF2YR,
    SEQN=as.numeric(SEQN)
  ) %>% 
    dplyr::select(SEQN,sub_cohort_weight,LBXGLU) 
}

## cohort data
{
  cohort_data=left_join(demo_1,body_measures,by="SEQN") %>% 
    left_join(.,hem) %>% 
    left_join(.,dia,by="SEQN") %>% 
    left_join(.,cho,by="SEQN") %>% 
    left_join(.,smoke,by="SEQN") %>% 
    left_join(.,alc,by="SEQN") %>% 
    left_join(.,blood,by="SEQN") %>%
    filter(cohort_weight!=0) %>% 
    filter(age_group!=1 & age_group!=2)
  md.pattern(cohort_data)
  set.seed(1)
  imp=mice(cohort_data)
  cohort_data=complete(imp)
  (nrow(cohort_data)-nrow(drop_na(cohort_data)))/nrow(cohort_data)  
  
  sub_cohort=left_join(glu,cohort_data,by="SEQN") %>% 
    filter(sub_cohort_weight!=0)%>% 
    filter(age_group!=1 & age_group!=2)
  
  sub_cohort= sub_cohort %>% 
    mutate(diabetes=ifelse((LBXGLU>=126)|LBXGH>=6.5,1,0)) 
}

multiple_num=50

######################## for the undiagnosed_diabetes    #########################

## weights from sub-cohort to population
{
  svy_1=svydesign(data=sub_cohort, id=~cluster, strata=~strata, weights=~sub_cohort_weight, nest=TRUE) 
  svymean(~diabetes,svy_1)
  
  x1=svyciprop(~diabetes,svy_1)
  mean_weight=as.vector(x1)
  ci_weight=as.vector(attr(x1, "ci"))
  width_weight=ci_weight[2]-ci_weight[1]
}

dat_1=sub_cohort %>% 
  dplyr::select("age_group","gender_group","race_group",
                "citizen_group","BMXBMI","LBXGH","LBXGLU","LBDHDDSI",contains("DIQ"),
                contains("BPQ"),contains("ALQ"),contains("SMQ"),"strata","cluster","diagnosed_diabetes","cohort_weight") %>% 
  mutate(stra_clus=as.factor(as.numeric(as.factor(paste(strata,cluster,sep = "_"))))) %>% 
  dplyr::select(-cluster) %>% 
  mutate(strata=as.factor(strata)) %>% 
  mutate(cohort_weight=log(cohort_weight),
         BMXBMI=log(BMXBMI),
         LBXGH=log(LBXGH),
         LBDHDDSI=log(LBDHDDSI),
         LBXGLU=log(LBXGLU)) 


dat_2=cohort_data %>% 
  dplyr::select("age_group","gender_group","race_group",
                "citizen_group","BMXBMI","LBXGH","LBDHDDSI",
                contains("DIQ"),contains("BPQ"),contains("ALQ"),contains("SMQ"),"strata","cluster","diagnosed_diabetes","cohort_weight") %>% 
  mutate(stra_clus=as.factor(as.numeric(as.factor(paste(strata,cluster,sep = "_"))))) %>% 
  dplyr::select(-cluster)%>% 
  mutate(strata=as.factor(strata)) %>% 
  mutate(cohort_weight=log(cohort_weight),
         BMXBMI=log(BMXBMI),
         LBXGH=log(LBXGH),
         LBDHDDSI=log(LBDHDDSI))

### predict response rate
in_sub=ifelse(cohort_data$SEQN %in% sub_cohort$SEQN,1,0)

bartFit = gbart(dat_2,in_sub,dat_2,type = 'pbart',printevery = 10000,mc.cores = 4)
res_rate=bartFit$prob.test.mean
##############################################################################

dat_3=dat_1 %>% dplyr::select(-LBXGLU) %>% 
  mutate(nr_adjustment=log(1/res_rate[which(in_sub==1)]))

dat_2=dat_2 %>% mutate(
  nr_adjustment=log(1/res_rate)
)

## bart
{
  set.seed(1)
  bartFit = gbart(dat_3,dat_1$LBXGLU,dat_2,type = 'wbart',mc.cores = 4,
                  nskip = 100,ntree = 100,keepevery = 1)
  dat_4 = as.data.frame(bartFit$yhat.test)
  
  mean_y_post=NULL
  variance_post=NULL
  multiple_y=seq(1,nrow(dat_4),length.out = multiple_num)
  dat_5=dat_2
  for (j in 1:multiple_num) {
    dat_5$LBXGLU=exp(as.numeric(dat_4[multiple_y[j],]))
    dat_5$SEQN=cohort_data$SEQN
    dat_5[which(dat_5$SEQN %in% sub_cohort$SEQN),"LBXGLU"] <- sub_cohort$LBXGLU
    dat_5$cluster=cohort_data$cluster
    dat_5$diabetes=ifelse(((dat_5$LBXGLU>=126)|(dat_5$LBXGH>=6.5)),1,0)
    survey_1<-svydesign(id=~cluster, weights=~cohort_weight, data=dat_5,strata = ~strata,nest = TRUE) 
    ### using replicate weights
    variance_post[j]=vcov(svyciprop(~diabetes,survey_1))
    mean_y_post[j]=as.vector(svyciprop(~diabetes,survey_1))
  }
  
  mean_bart=mean(mean_y_post)
  v_bar=mean(variance_post)
  b=var(mean_y_post)
  var_multiple=v_bar+(1+1/multiple_num)*b
  d=(multiple_num-1)*(1+multiple_num*v_bar/((multiple_num+1)*b))^2
  ci_bart=c(mean_bart-qt(0.975,d)*sqrt(var_multiple),mean_bart+qt(0.975,d)*sqrt(var_multiple))
  mean_bart
  ci_bart
  width_bart=ci_bart[2]-ci_bart[1]
}

## rbart prediction of total diabetes in cohort
{
  dat_1$LBXGLU=as.numeric(dat_1$LBXGLU)
  rbartFit <- rbart_vi(LBXGLU ~ . - stra_clus, dat_1, group.by = dat_1$stra_clus,
                       n.samples = 1000L, n.burn = 100L, n.chains = 1L,
                       n.trees = 100L, keepTrees = TRUE,n.thin = 1)
  
  pred=predict(rbartFit,newdata=dat_2,group.by=dat_2$stra_clus)
  dat_4=as.data.frame(pred)

  multiple_y=seq(1,nrow(dat_4),length.out = multiple_num)
  mean_y_post=NULL
  variance_post=NULL
  dat_5=dat_2
  for (j in 1:multiple_num) {
    dat_5$LBXGLU=exp(as.numeric(dat_4[multiple_y[j],]))
    dat_5$SEQN=cohort_data$SEQN
    dat_5[which(dat_5$SEQN %in% sub_cohort$SEQN),"LBXGLU"] <- sub_cohort$LBXGLU
    dat_5$cluster=cohort_data$cluster
    dat_5$diabetes=ifelse(((dat_5$LBXGLU>=126)|(dat_5$LBXGH>=6.5)),1,0)
    survey_1<-svydesign(id=~cluster, weights=~cohort_weight, data=dat_5,strata = ~strata,nest = TRUE) 
    ### using replicate weights
    variance_post[j]=vcov(svyciprop(~diabetes,survey_1))
    mean_y_post[j]=as.vector(svyciprop(~diabetes,survey_1))
  }
  
  mean_rbart=mean(mean_y_post)
  v_bar=mean(variance_post)
  b=var(mean_y_post)
  var_multiple=v_bar+(1+1/multiple_num)*b
  d=(multiple_num-1)*(1+multiple_num*v_bar/((multiple_num+1)*b))^2
  ci_rbart=c(mean_rbart-qt(0.975,d)*sqrt(var_multiple),mean_rbart+qt(0.975,d)*sqrt(var_multiple))
  mean_rbart
  ci_rbart
  width_rbart=ci_rbart[2]-ci_rbart[1]
}

## plots of application results
{
  method=c("WT","MI-BART","MI-rBART")
  mean=c(mean_weight*100,mean_bart*100,mean_rbart*100)
  min=c(ci_weight[1]*100,ci_bart[1]*100,ci_rbart[1]*100)
  max=c(ci_weight[2]*100,ci_bart[2]*100,ci_rbart[2]*100)
  width=c(width_weight*100,width_bart*100,width_rbart*100)
  dat=data.frame(method,mean,min,max,width)
  
  g1=ggplot(dat, aes(x=method, y=mean,group=1)) + 
    geom_errorbar(aes(ymin=min, ymax=max), width=.05) +
    geom_point(size=1)+
    theme_classic()+
    scale_shape_manual(values=c(15, 16, 17, 3, 23, 25))+
    scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73",
                                "#D55E00", "#0072B2", "#D55E00", "#CC79A7"))+
    theme(plot.margin = unit(c(1,1,1,1), "cm"),
          axis.title.x = element_text(size=10),
          axis.title.y = element_text(size=10,vjust=4),
          axis.text.x = element_text(size=10,vjust = -1),
          legend.text = element_text(size=10),
          legend.title = element_text(size=10))+
    ylab("Prevalence of Diabetes (%)")+
    xlab("")
}


######################## for the diagnosed_diabetes #########################

## benchmark from true cohort to population
{
  svy_1=svydesign(data=cohort_data, id=~cluster, strata=~strata, weights=~cohort_weight, nest=TRUE) 
  
  x1=svyciprop(~diagnosed_diabetes,svy_1)
  mean_benchmark_2=as.vector(x1)
  ci_benchmark_2=as.vector(attr(x1, "ci"))
  width_benchmark_2=ci_benchmark[2]-ci_benchmark[1]
}

## weights from sub-cohort to population
{
  svy_1=svydesign(data=sub_cohort, id=~cluster, strata=~strata, weights=~sub_cohort_weight, nest=TRUE) 
  x1=svyciprop(~diagnosed_diabetes,svy_1)
  
  mean_weight_2=as.vector(x1)
  ci_weight_2=as.vector(attr(x1, "ci"))
  width_weight_2=ci_weight_2[2]-ci_weight_2[1]
}

## sub-cohort to cohort by bart
### bart predict no weights
dat_1=sub_cohort %>% 
  dplyr::select("cohort_weight","age_group","gender_group","race_group",
                "citizen_group","BMXBMI","LBXGH","LBDHDDSI",
                contains("DIQ"),"SMQ040","ALQ151","BPQ020",
                "strata","cluster","diagnosed_diabetes") %>% 
  mutate(stra_clus=as.factor(as.numeric(as.factor(paste(strata,cluster,sep = "_"))))) %>% 
  dplyr::select(-cluster) %>% 
  mutate(strata=as.factor(strata)) %>% 
  mutate(cohort_weight=log(cohort_weight),
         BMXBMI=log(BMXBMI),
         LBDHDDSI=log(LBDHDDSI),
         LBXGH=log(LBXGH))

dat_2=cohort_data %>% 
  dplyr::select("cohort_weight","age_group","gender_group","race_group",
                "citizen_group","BMXBMI","LBXGH","LBDHDDSI",contains("DIQ"),
                "SMQ040","ALQ151","BPQ020","strata","cluster") %>% 
  mutate(stra_clus=as.factor(as.numeric(as.factor(paste(strata,cluster,sep = "_"))))) %>% 
  dplyr::select(-cluster)%>% 
  mutate(strata=as.factor(strata)) %>% 
  mutate(cohort_weight=log(cohort_weight),
         BMXBMI=log(BMXBMI),
         LBDHDDSI=log(LBDHDDSI),
         LBXGH=log(LBXGH))
### predict response rate
in_sub=ifelse(cohort_data$SEQN %in% sub_cohort$SEQN,1,0)

bartFit = gbart(dat_2,in_sub,dat_2,type = 'pbart',printevery = 10000,mc.cores = 4)
res_rate=bartFit$prob.test.mean
##############################################################################

dat_3=dat_1 %>% dplyr::select(-diagnosed_diabetes) %>% 
  mutate(nr_adjustment=log(1/res_rate[which(in_sub==1)]))

dat_2=dat_2 %>% mutate(
  nr_adjustment=log(1/res_rate)
)

## bart prediction of total diabetes in cohort
{
  bartFit_2 = gbart(dat_3,dat_1$diagnosed_diabetes,dat_2,type = 'pbart',
                    mc.cores = 4,keepevery = 1,
                    nskip = 100,ntree=100)
  dat_4=as.data.frame(bartFit_2$prob.test)
  
  multiple_y=seq(1,nrow(dat_4),length.out = multiple_num)
  mean_y_post=NULL
  variance_post=NULL
  dat_5=cohort_data
  for (j in 1:multiple_num) {
    dat_5$diagnosed_diabetes=ifelse(as.numeric(dat_4[multiple_y[j],])>0.5,1,0)
    ##rbinom(nrow(dat_5),size=1,as.numeric(dat_4[multiple_y[j],]))
    dat_5$SEQN=cohort_data$SEQN
    dat_5[which(dat_5$SEQN %in% sub_cohort$SEQN),"diagnosed_diabetes"] <- sub_cohort$diagnosed_diabetes
    dat_5$cluster=cohort_data$cluster
    survey_1<-svydesign(id=~cluster, weights=~cohort_weight, data=dat_5,strata = ~strata,nest = TRUE) 
    ### using replicate weights
    variance_post[j]=vcov(svyciprop(~diagnosed_diabetes,survey_1))
    mean_y_post[j]=as.vector(svyciprop(~diagnosed_diabetes,survey_1))
  }
  
  mean_bart_2=mean(mean_y_post)
  v_bar=mean(variance_post)
  b=var(mean_y_post)
  var_multiple=v_bar+(1+1/multiple_num)*b
  d=(multiple_num-1)*(1+multiple_num*v_bar/((multiple_num+1)*b))^2
  ci_bart_2=c(mean_bart_2-qt(0.975,d)*sqrt(var_multiple),mean_bart_2+qt(0.975,d)*sqrt(var_multiple))
  width_bart_2=ci_bart_2[2]-ci_bart_2[1]
}

## rbart prediction
{
  rbartFit_2 <- rbart_vi(diagnosed_diabetes ~ . - stra_clus, dat_1, group.by = dat_1$stra_clus,
                         n.samples = 1000L, n.burn = 100L, n.chains = 1L,
                         n.trees = 100L,keepTrees = TRUE,n.thin = 1)
  pred_2=predict(rbartFit_2,newdata=dat_2,group.by=dat_2$stra_clus)
  dat_4=as.data.frame(pred_2)
  
  multiple_y=seq(1,nrow(dat_4),length.out = multiple_num)
  mean_y_post=NULL
  variance_post=NULL
  dat_5=cohort_data
  for (j in 1:multiple_num) {
    dat_5$diagnosed_diabetes=ifelse(as.numeric(dat_4[multiple_y[j],])>0.5,1,0)
    dat_5$SEQN=cohort_data$SEQN
    dat_5[which(dat_5$SEQN %in% sub_cohort$SEQN),"diagnosed_diabetes"] <- sub_cohort$diagnosed_diabetes
    dat_5$cluster=cohort_data$cluster
    survey_1<-svydesign(id=~cluster, weights=~cohort_weight, data=dat_5,strata = ~strata,nest = TRUE) 
    ### using replicate weights
    variance_post[j]=vcov(svyciprop(~diagnosed_diabetes,survey_1))
    mean_y_post[j]=as.vector(svyciprop(~diagnosed_diabetes,survey_1))
  }
  
  mean_rbart_2=mean(mean_y_post)
  v_bar=mean(variance_post)
  b=var(mean_y_post)
  var_multiple=v_bar+(1+1/multiple_num)*b
  d=(multiple_num-1)*(1+multiple_num*v_bar/((multiple_num+1)*b))^2
  ci_rbart_2=c(mean_rbart_2-qt(0.975,d)*sqrt(var_multiple),mean_rbart_2+qt(0.975,d)*sqrt(var_multiple))
  width_rbart_2=ci_rbart_2[2]-ci_rbart_2[1]
}

## plots
{
  method=c("Benchmark","WT","MI-BART","MI-rBART")
  mean=c(mean_benchmark_2*100,mean_weight_2*100,mean_bart_2*100,mean_rbart_2*100)
  min=c(ci_benchmark_2[1]*100,ci_weight_2[1]*100,ci_bart_2[1]*100,ci_rbart_2[1]*100)
  max=c(ci_benchmark_2[2]*100,ci_weight_2[2]*100,ci_bart_2[2]*100,ci_rbart_2[2]*100)
  width=c(width_benchmark_2*100,width_weight_2*100,width_bart_2*100,width_rbart_2*100)
  dat=data.frame(method,mean,min,max,width)
  
  dat$method=factor(dat$method,levels = c("Benchmark","WT","MI-BART","MI-rBART"))
  
  g2=ggplot(dat, aes(x=method, y=mean,group=1)) + 
    geom_errorbar(aes(ymin=min, ymax=max), width=.05) +
    geom_point(size=1)+
    theme_classic()+
    scale_shape_manual(values=c(15, 16, 17, 3, 23, 25))+
    scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73",
                                "#D55E00", "#0072B2", "#D55E00", "#CC79A7"))+
    theme(plot.margin = unit(c(1,1,1,1), "cm"),
          axis.title.x = element_text(size=10),
          axis.title.y = element_text(size=10,vjust=4),
          axis.text.x = element_text(size=10,vjust = -1),
          legend.text = element_text(size=10),
          legend.title = element_text(size=10))+
    ylab("Prevalence of Diagnosed Diabetes (%)")+
    xlab("")+
    geom_hline(yintercept = mean_benchmark*100,linetype=2)
}

######################## for the a1c log outcome #########################

## benchmark from true cohort to population
{
  cohort_data_1=cohort_data %>% dplyr::filter(!is.na(LBXGH))
  svy_1=svydesign(data=cohort_data_1, id=~cluster, strata=~strata, weights=~cohort_weight, nest=TRUE) 
  
  x1=svymean(~LBXGH,svy_1)
  mean_benchmark_3=as.vector(x1)
  ci_benchmark_3=confint(x1)
  width_benchmark_3=ci_benchmark_5[2]-ci_benchmark_5[1]
}

## weights from sub-cohort to population
{
  sub_cohort_1=sub_cohort %>% dplyr::filter(!is.na(LBXGH))
  svy_1=svydesign(data=sub_cohort_1, id=~cluster, strata=~strata, weights=~sub_cohort_weight, nest=TRUE) 
  
  x1=svymean(~LBXGH,svy_1)
  mean_weight_3=as.vector(x1)
  ci_weight_3=confint(x1)
  width_weight_3=ci_weight_3[2]-ci_weight_3[1]
}

dat_1=sub_cohort %>% 
  dplyr::select("cohort_weight","age_group","gender_group","race_group",
                "citizen_group","BMXBMI","LBXGH","LBDHDDSI",
                contains("DIQ"),"SMQ040","ALQ151","BPQ020","strata","cluster","diagnosed_diabetes") %>% 
  mutate(stra_clus=as.factor(as.numeric(as.factor(paste(strata,cluster,sep = "_"))))) %>% 
  dplyr::select(-cluster) %>% 
  mutate(strata=as.factor(strata)) %>% 
  mutate(cohort_weight=log(cohort_weight),
         BMXBMI=log(BMXBMI),
         LBDHDDSI=log(LBDHDDSI),
         LBXGH=log(LBXGH))

dat_2=cohort_data %>% 
  dplyr::select("cohort_weight","age_group","gender_group","race_group",
                "citizen_group","BMXBMI","LBDHDDSI",contains("DIQ"),
                "SMQ040","ALQ151","BPQ020","strata","cluster","diagnosed_diabetes") %>% 
  mutate(stra_clus=as.factor(as.numeric(as.factor(paste(strata,cluster,sep = "_"))))) %>% 
  dplyr::select(-cluster)%>% 
  mutate(strata=as.factor(strata)) %>% 
  mutate(cohort_weight=log(cohort_weight),
         BMXBMI=log(BMXBMI),
         LBDHDDSI=log(LBDHDDSI))

### predict response rate
in_sub=ifelse(cohort_data$SEQN %in% sub_cohort$SEQN,1,0)

bartFit = gbart(dat_2,in_sub,dat_2,type = 'pbart',printevery = 10000,mc.cores = 4)
res_rate=bartFit$prob.test.mean
##############################################################################

dat_3=dat_1 %>% dplyr::select(-LBXGH) %>% 
  mutate(nr_adjustment=log(1/res_rate[which(in_sub==1)]))

dat_2=dat_2 %>% mutate(
  nr_adjustment=log(1/res_rate)
)

## bart prediction of total diabetes in cohort
{
  bartFit_3 = gbart(dat_3,dat_1$LBXGH,dat_2,type = 'wbart',nskip = 100,
                    ntree = 100,mc.cores = 4)
  dat_bart_3=as.data.frame(bartFit_5$yhat.test)
  
  set.seed(2)
  multiple_y=seq(1,nrow(dat_bart_3),length.out = multiple_num)
  mean_y_post=NULL
  variance_post=NULL
  dat_5=cohort_data
  for (j in 1:multiple_num) {
    dat_5$LBXGH=exp(as.numeric(dat_bart_3[multiple_y[j],]))
    dat_5$SEQN=cohort_data$SEQN
    dat_5[which(dat_5$SEQN %in% sub_cohort$SEQN),"LBXGH"] <- sub_cohort$LBXGH
    dat_5$cluster=cohort_data$cluster
    survey_1<-svydesign(id=~cluster, weights=~cohort_weight, data=dat_5,strata = ~strata,nest = TRUE) 
    ### using replicate weights
    variance_post[j]=vcov(svymean(~LBXGH,survey_1))
    mean_y_post[j]=coef(svymean(~LBXGH,survey_1))
  }
  
  mean_bart_3=mean(mean_y_post)
  v_bar=mean(variance_post)
  b=var(mean_y_post)
  var_multiple=v_bar+(1+1/multiple_num)*b
  d=(multiple_num-1)*(1+multiple_num*v_bar/((multiple_num+1)*b))^2
  ci_bart_3=c(mean_bart_3-qt(0.975,d)*sqrt(var_multiple),mean_bart_3+qt(0.975,d)*sqrt(var_multiple))
  width_bart_3=ci_bart_3[2]-ci_bart_3[1]
}

## rbart prediction
{
  dat_1$LBXGH=as.numeric(dat_1$LBXGH)
  rbartFit_3 <- rbart_vi(LBXGH ~ . - stra_clus, dat_1, group.by = dat_1$stra_clus,
                         n.samples = 1000L, n.burn = 100L, n.chains = 1L,
                         n.trees = 100L, n.thin = 1,keepTrees = TRUE)
  pred_3=predict(rbartFit_3,newdata=dat_2,group.by=dat_2$stra_clus)
  dat_rbart_3=as.data.frame(pred_3)
  
  set.seed(2)
  multiple_y=seq(1,nrow(dat_rbart_3),length.out = multiple_num)
  mean_y_post=NULL
  variance_post=NULL
  for (j in 1:multiple_num) {
    dat_5$LBXGH=exp(as.numeric(dat_rbart_3[multiple_y[j],]))
    dat_5$SEQN=cohort_data$SEQN
    dat_5[which(dat_5$SEQN %in% sub_cohort$SEQN),"LBXGH"] <- sub_cohort$LBXGH
    dat_5$cluster=cohort_data$cluster
    survey_1<-svydesign(id=~cluster, weights=~cohort_weight, data=dat_5,strata = ~strata,nest = TRUE) 
    ### using replicate weights
    variance_post[j]=vcov(svymean(~LBXGH,survey_1))
    mean_y_post[j]=coef(svymean(~LBXGH,survey_1))
  }
  
  mean_rbart_3=mean(mean_y_post)
  v_bar=mean(variance_post)
  b=var(mean_y_post)
  var_multiple=v_bar+(1+1/multiple_num)*b
  d=(multiple_num-1)*(1+multiple_num*v_bar/((multiple_num+1)*b))^2
  ci_rbart_3=c(mean_rbart_3-qt(0.975,d)*sqrt(var_multiple),mean_rbart_3+qt(0.975,d)*sqrt(var_multiple))
  width_rbart_3=ci_rbart_3[2]-ci_rbart_3[1]
}

## plots
{
  method=c("Benchmark","WT","MI-BART","MI-rBART")
  mean=c(mean_benchmark_3,mean_weight_3,mean_bart_3,mean_rbart_3)
  min=c(ci_benchmark_3[1],ci_weight_3[1],ci_bart_3[1],ci_rbart_3[1])
  max=c(ci_benchmark_3[2],ci_weight_3[2],ci_bart_3[2],ci_rbart_3[2])
  width=c(width_benchmark_3,width_weight_3,width_bart_3,width_rbart_3)
  dat=data.frame(method,mean,min,max,width)
  
  ## print table
  dat_a1c=dat
  colnames(dat_a1c)=c("Method","Estimand","Lower bound","Upper bound","Interval Width")
  print(xtable(dat_a1c,digits = 4),include.rownames=FALSE)
  
  dat$method=factor(dat$method,levels = c("Benchmark","WT","MI-BART","MI-rBART"))
  
  g3=ggplot(dat, aes(x=method, y=mean,group=1)) + 
    geom_errorbar(aes(ymin=min, ymax=max), width=.05) +
    geom_point(size=1)+
    theme_classic()+
    scale_shape_manual(values=c(15, 16, 17, 3, 23, 25))+
    scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73",
                                "#D55E00", "#0072B2", "#D55E00", "#CC79A7"))+
    theme(plot.margin = unit(c(1,1,1,1), "cm"),
          axis.title.x = element_text(size=10),
          axis.title.y = element_text(size=10,vjust=4),
          axis.text.x = element_text(size=10,vjust = -1),
          legend.text = element_text(size=10),
          legend.title = element_text(size=10))+
    ylab("Population Mean of HbA1c")+
    xlab("")+
    geom_hline(yintercept = mean_benchmark_5,linetype=2)

  ggpubr::ggarrange(g1,g2,g3,
                    labels = c("A", "B","C"),
                    ncol = 3,nrow = 1,font.label = list(size = 10, face = "bold"))
  ggsave("application_result.png",width = 12, height = 4)
}

{
  dat_all=rbind(dat_diabetes,dat_diagnosed_diabetes,dat_a1c)
  colnames(dat_all)=c("Method","Estimand","Lower bound","Upper bound","Interval Width")
  print(xtable(dat_all,digits = 3),include.rownames=FALSE)
}