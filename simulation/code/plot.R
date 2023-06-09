library(dplyr)
library(tidyverse)
library(xtable)

formResult=function(multiple_num,scenario){
  num=which(simu_table$multiple_num==multiple_num&simu_table$scenario==scenario)
  load(paste("./simulation/results/",num,".RData",sep = ""))
  relative_bias=apply(sapply(out, function(x){
    sapply(x, function(y){
      y$relative_bias
    })
  }),1,mean)
  width_conf=apply(sapply(out, function(x){
    sapply(x, function(y){
      y$width_conf
    })
  }),1,mean)
  square_error=apply(sapply(out, function(x){
    sapply(x, function(y){
      y$square_error
    })
  }),1,mean)
  cover=apply(sapply(out, function(x){
    sapply(x, function(y){
      y$cover
    })
  }),1,mean)
  method=c("true_coh_pop","sub_pop_weight_lgm","sub_pop_weight_rbart","sub_pop_weight_bart","sub_pop_weight_chaid",
           "sub_pop_mi_rbart","sub_pop_mi_bart")
  data.frame(method,relative_bias=round(relative_bias*100,2),width_conf=round(width_conf*100,2),
             mean_square_error=round(sqrt(square_error)*100,2),cover=round(cover*100,2),
             Scenario=paste("S",scenario,sep = ""),multiple_num=multiple_num)
}

##### load results into a single table #######
multiple_num=unique(simu_table$multiple_num)
scenario=unique(simu_table$scenario)
result_dat=NULL
for (i in 1:length(multiple_num)) {
  for (j in 1:length(scenario)) {
    result_dat=rbind(result_dat,formResult(multiple_num = multiple_num[i], scenario = scenario[j]))
  }
}

result_dat=result_dat %>% 
  pivot_longer(
    cols=2:5,
    names_to = "Index",
    values_to = "value"
  )
result_dat$Index=factor(result_dat$Index,levels = c("relative_bias","mean_square_error","cover","width_conf"),
                        labels = c("Absolute Bias", "RMSE","Coverage Rate","Interval Width"))

result_dat$method=factor(result_dat$method,
                         levels = c("true_coh_pop","sub_pop_weight_lgm","sub_pop_weight_chaid","sub_pop_weight_bart","sub_pop_weight_rbart",
                                    "sub_pop_mi_bart","sub_pop_mi_rbart"),
                         labels = c("Benchmark","WT-LGM","WT-CHAID","WT-BART","WT-rBART",
                                    "MI-BART","MI-rBART" ))
result_dat$Framework=factor(ifelse(result_dat$method=="Benchmark","Benchmark",
                                   ifelse(result_dat$method %in% c(
                                     "WT-LGM","WT-CHAID","WT-BART","WT-rBART"),"Weighting","Imputation")),levels = c("Benchmark","Weighting","Imputation"))
result_dat$Scenario=as.factor(result_dat$Scenario)
result_dat$multiple_num=as.factor(result_dat$multiple_num)

plotresult = function(num){
  g1=ggplot(subset(result_dat,multiple_num==num),aes(x=Scenario,y=value,group=method)) +
    geom_point(aes(shape=method,color=method),size=2)+
    theme_bw()+
    facet_wrap(~factor(Index,levels=c("Absolute Bias", "RMSE","Coverage Rate","Interval Width")),scales = "free_y",nrow = 1)+
    ## geom_hline(data = cover_95, aes(yintercept = value),linetype=2)+
    scale_shape_manual(values=c(15,3,4,1,2,16,17))+ 
    scale_color_manual(values=c("black", "black", "black", "black",
                                "black", "black", "black"))+
    theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
          axis.title.y = element_text(size=15),
          axis.text = element_text(size=15),
          legend.text = element_text(size=15),
          legend.title = element_text(size=15),
          strip.text.x = element_text(size=15),
          legend.position = "right",
          legend.direction = "vertical")+
    ylab("Value")+
    xlab("")+
    geom_hline(data = data.frame(yint=95,Index="Coverage Rate"), aes(yintercept = yint), 
               linetype = "dotted",color="red")
  g1
  ggsave(paste("./simulation/figures/simulation_continuous_",num,".png",sep = ""),width = 11.8,height = 5)   
}

for (i in 1:length(multiple_num)) {
  plotresult(num = multiple_num[i])
}

## table
{
  scenario_num=4
  ## table
  dat_1=result_dat %>%
    pivot_wider(names_from = Index, values_from=value) %>% 
    filter(multiple_num==1000) #### specify the number of multiple imputation
  
  dat_1$Scenario=c("S1",rep("",nrow(dat_1)/scenario_num-1),"S2",rep("",nrow(dat_1)/scenario_num-1),"S3",
                   rep("",nrow(dat_1)/scenario_num-1),"S4",rep("",nrow(dat_1)/scenario_num-1))
  print(xtable(dat_1[,-3],digits = 1),include.rownames=FALSE)
}
