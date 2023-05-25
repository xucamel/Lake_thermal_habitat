setwd("C:/Users/user/Desktop/OneDrive/Postdoctor/Project_3")
library(rstan)
library(dplyr)
SPM_stan = stan_model(file="Code/Stan_code/SPM.stan")

# reading input
input_df = read.csv("Data/SPM_input.csv")

# number of lakes
length(unique(input_df$wbic))

# number of stocks
Nstocks = length(unique(input_df$stock_id))

# output container 
output = array(dim=c(Nstocks,8,7))
output_quantities = data.frame(wbic=rep(NA,Nstocks), P_terminal=rep(NA,Nstocks),P_no_fishing=rep(NA,Nstocks),P_no_fishing_adjusted=rep(NA,Nstocks),P_no_env=rep(NA,Nstocks),P_one_env=rep(NA,Nstocks),N_divergent=rep(NA,Nstocks),max_f=rep(NA,Nstocks),min_f=rep(NA,Nstocks))

# run the model
for (i in 1:Nstocks){
  ## model input 
  stock_i = unique(input_df$stock_id)[i]
  data_i = input_df[input_df$stock_id==stock_i,]
  min_year_i = min(data_i$year[which(data_i$cpe>0)],data_i$year[which(data_i$rec_harvest>0)])
  max_year_i = max(data_i$year[which(data_i$cpe>0)],data_i$year[which(data_i$rec_harvest>0)])
  catch_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$rec_harvest
  cpue_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$cpe
  env_i = scale(data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$days_optimal_temp, scale = T)[,]
  stocking_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$stocking_weight
  tri_i = data_i[data_i$year>=min_year_i&data_i$year<=max_year_i,]$tri_harvest
  
  stan_data <- list(
    N_1 = max_year_i-min_year_i+1,
    Catch_1 = catch_i,
    CPUE_1 = cpue_i,
    Environment_1 = env_i,
    Stocking_1 = stocking_i,
    Tribal_1 = tri_i,
    k_1_prior = c(max(catch_i),max(catch_i*10000)),
    r_1_prior = c(0.05,0.5)
  )
  
  chain=3; iter=2000; warmup=1000; thin=1;
  
  fit_SPM_stan <- sampling(SPM_stan,
                           data=stan_data,
                           chain=chain,
                           iter=iter,
                           warmup=warmup,
                           cores = chain,
                           thin=thin,
                           control = list(adapt_delta = 0.999,max_treedepth = 12)
  ) 
  
  output[i,,1] = summary(fit_SPM_stan)$summary[1:8,"mean"]
  output[i,,2] = summary(fit_SPM_stan)$summary[1:8,"se_mean"]
  output[i,,3] = summary(fit_SPM_stan)$summary[1:8,"sd"]
  output[i,,4] = summary(fit_SPM_stan)$summary[1:8,"2.5%"]
  output[i,,5] = summary(fit_SPM_stan)$summary[1:8,"97.5%"]
  output[i,,6] = summary(fit_SPM_stan)$summary[1:8,"Rhat"]
  output[i,,7] = summary(fit_SPM_stan)$summary[1:8,"n_eff"]
  output_quantities$P_terminal[i] = summary(fit_SPM_stan)$summary["P_terminal","mean"]
  output_quantities$P_no_fishing[i] = summary(fit_SPM_stan)$summary["P_no_fishing_terminal","mean"]
  output_quantities$P_no_fishing_adjusted[i] = summary(fit_SPM_stan)$summary["P_no_fishing_terminal_adjusted","mean"]
  output_quantities$P_no_env[i] = summary(fit_SPM_stan)$summary["P_no_env_terminal","mean"]
  output_quantities$P_one_env[i] = summary(fit_SPM_stan)$summary["P_one_env_terminal","mean"]
  output_quantities$N_divergent[i] = get_num_divergent(fit_SPM_stan)
  output_quantities$max_f[i] = max(colMeans(as.data.frame(extract(fit_SPM_stan,"F_1"))))
  output_quantities$min_f[i] = min(colMeans(as.data.frame(extract(fit_SPM_stan,"F_1"))))
  output_quantities$stock_id = stock_i
  print(i)
}

saveRDS(output,"Output/WI_SPM.rda")
write.csv(output_quantities,"Output/WI_SPM.csv",row.names = FALSE)

