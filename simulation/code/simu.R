#### simulation parameters
multiple_num=c(rep(10,4),rep(20,4),rep(50,4),rep(100,4),rep(500,4),rep(1000,4))
scenario=rep(c(1,2,3,4),6)
simu_table=data.frame(multiple_num,scenario)

### source your local path for "R_pac_2.R"
source("./simulation/code/R_pac_2.R")
finite_pop_norm=read_csv("./simulation/data/finite_pop_norm.csv")

simu=function(simu_time=500,scenario,multiple_num,path,finite_pop_norm){
  if(scenario==1){
    num_norm=2;num_bin=3
  }
  if(scenario!=1){
    num_norm=num_bin=10
  }
  variable_norm_name=generate_variable_name(num = num_norm,type=0)
  variable_bin_name=generate_variable_name(num = num_bin,type=1)
  
  outcome_list=foreach(i = 1:simu_time) %dopar% {
    source(paste(path,"R_pac_2.R",sep=""))
    set.seed(i)
    cohort_data=generate_cohort(finite_pop_norm,variable_norm_name = variable_norm_name, variable_bin_name = variable_bin_name)
    
    m1=generate_sub_cohort(cohort_data,weight_method = "LGM",scenario = scenario,select_rate = 0.5,variable_norm_name = variable_norm_name, variable_bin_name = variable_bin_name)
    m2=generate_sub_cohort(cohort_data,weight_method = "RBART",scenario = scenario,select_rate = 0.5,variable_norm_name = variable_norm_name, variable_bin_name = variable_bin_name)
    m3=generate_sub_cohort(cohort_data,weight_method = "BART",scenario = scenario,select_rate = 0.5,variable_norm_name = variable_norm_name, variable_bin_name = variable_bin_name)
    m4=generate_sub_cohort(cohort_data,weight_method = "CHAID",scenario = scenario,select_rate = 0.5,variable_norm_name = variable_norm_name, variable_bin_name = variable_bin_name)
    
    ### predict outcome by rbart with weights
    z1=bayesian_bart_sub_cohort(sub_cohort = m2$sub_cohort_notrim,
                                cohort_data = cohort_data,
                                bart_type = 2,weights = TRUE,predicted_cohort_res = m2$predicted_cohort_res,
                                predicted_sub_res = m2$predicted_sub_res_notrim,y_type=1,
                                multiple_num=multiple_num,
                                variable_norm_name = variable_norm_name, variable_bin_name = variable_bin_name)
    
    ### predict outcome by bart with weights
    z2=bayesian_bart_sub_cohort(sub_cohort = m3$sub_cohort_notrim,
                                cohort_data = cohort_data,
                                bart_type = 0,weights = TRUE,predicted_cohort_res = m3$predicted_cohort_res,
                                predicted_sub_res = m3$predicted_sub_res_notrim,y_type=1,
                                multiple_num=multiple_num,
                                variable_norm_name = variable_norm_name, variable_bin_name = variable_bin_name)
    
    ## true cohort to pop
    x1=weight_cohort_pop(cohort_data = cohort_data,finite_pop = finite_pop_norm,y_type=1)
    
    ## sub to pop
    ### weight from sub to pop
    x2=weight_sub_pop(m1$sub_cohort_notrim,finite_pop_norm,cohort_data = cohort_data,y_type=1)
    x3=weight_sub_pop(m2$sub_cohort_notrim,finite_pop_norm,cohort_data = cohort_data,y_type=1)
    x4=weight_sub_pop(m3$sub_cohort_notrim,finite_pop_norm,cohort_data = cohort_data,y_type=1)
    x5=weight_sub_pop(m4$sub_cohort_notrim,finite_pop_norm,cohort_data = cohort_data,y_type=1)
    
    ### rbart from sub to coh then weight from coh to pop 
    x6=MI_bart_sub_pop(cohort_data_list = z1$bart_cohort_data,
                       finite_pop = finite_pop_norm,multiple_num = multiple_num,y_type=1)
    ### bart from sub to coh then weight from coh to pop 
    x7=MI_bart_sub_pop(cohort_data_list = z2$bart_cohort_data,
                       finite_pop = finite_pop_norm,multiple_num = multiple_num,y_type=1)
    list(x1,x2,x3,x4,x5,x6,x7)
    }
  return(outcome_list)
}
