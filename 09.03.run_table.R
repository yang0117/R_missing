#' 09.03. an overall function to generate the result table

source("09.01.beta_matrix_generator.R")
source("09.02.result_table_generate.R")

#######################################
#'run_table: generate the table we need
#'input: n_sim=100,n=250,beta_vector,k=5,intercept=2,beta_vector=c(0.5,1,2)),method_indicator,loss_rate,penalty
#'output: a list included result table and all beta estimation

run_table <- function(n_sim=100,n=250,beta_vector=c(1.5,2,3,rep(0,5)),k=5,intercept=2,
                      filter_vec=c(0.5,1,2),method_indicator,loss_rate,penalty){
  #generate beta estimation
  beta_est_all <- beta_list_all_method(n_sim=n_sim,n=n,beta_vector=beta_vector,k=k,intercept=intercept,
                                       method_indicator=method_indicator,loss_rate=loss_rate,penalty=penalty)
  #generate result table
  result_table <- result_table_all(beta_result_list = beta_est_all, beta_vec=beta_vector, filter_vec=filter_vec)
  result <- list(result_table=result_table,beta_estimation_all=beta_est_all)
  return(result)
}


#######################################
#'run_table17: generate the table we need for all 17 settings
#'             everything is exactly same as function "run_table", except including all 17
#'             methods and add two parameters,n_iter_SCAD,n_iter_MCP to indicate iteration times for SCAD
#'             and MCP and delete penalty parameter and delete penalty paramter
#'input: n_sim=100,n=250,beta_vector,k=5,intercept=2,beta_vector=c(0.5,1,2)),method_indicator,loss_rate,
#'       n_iter_SCAD: iteration time for SCAD, if 0, no SCAD will be estimated
#'       n_iter_MCP: iteration time for MCP, if 0, no MCP will be estimated
#'       error_var: variance for error term
#'       y_logistic: F: generate y as normal distribution
#'                   T: generate y as logistic regression, it will ignore the value of method_indicator and will keep
#'                      all 1 of y and 25% 0 of y
#'output: a list included result list(it is a list and each matrix in this list corresponding to a relative ratio)
#'                    and all beta estimation

run_table17 <- function(n_sim=100,n=250,beta_vector=c(1.5,2,3,rep(0,5)),k=5,intercept=2,
                      filter_vec=c(0.5,1,2),method_indicator,loss_rate,
                      logistic_method="regular",
                      n_iter_SCAD,n_iter_MCP,error_var,y_logistic,
                      initial_true_indicator_SCAD = F,
                      initial_true_indicator_MCP = F,
                      x_missing_location=1,error_independent=F,
                      lambda_location_SCAD="all",lambda_location_MCP="all"){
  test_time <- proc.time()
  #generate beta estimation
  beta_est_all <- beta_list_all_method17(n_sim=n_sim,n=n,beta_vector=beta_vector,k=k,intercept=intercept,
                                       method_indicator=method_indicator,loss_rate=loss_rate,
                                       logistic_method=logistic_method,
                                       n_iter_SCAD=n_iter_SCAD,n_iter_MCP=n_iter_MCP,
                                       error_var=error_var,y_logistic=y_logistic,
                                       initial_true_indicator_SCAD=initial_true_indicator_SCAD,initial_true_indicator_MCP=initial_true_indicator_MCP,
                                       x_missing_location=x_missing_location,error_independent=error_independent,
                                       lambda_location_SCAD=lambda_location_SCAD,lambda_location_MCP=lambda_location_MCP)
  
  #generate result table
  result_table_list <- result_table_all(beta_result_list = beta_est_all, beta_vec=beta_vector, filter_vec=filter_vec,
                                   n_iter_SCAD=n_iter_SCAD,n_iter_MCP=n_iter_MCP)
  result_table <- result_table_list$table
  
  #put result with similar filter togather
  #row_number <- dim(result_table)[1]/(length(filter_vec)+1)
  result_table_row_index <- seq(1,dim(result_table)[1],by=(length(filter_vec)+1))
  table_original <- result_table[result_table_row_index,]
  #initial a matrix to store relative ratio index
  relative_ratio_index_matrix <- matrix(0,length(filter_vec),length(result_table_row_index))
  #assign value for relative_ratio_index_matrix
  for(i in 1:length(filter_vec)){
    relative_ratio_index_matrix[i,] <- result_table_row_index+i
  }
  #remove index for full and complete data
  relative_ratio_index_matrix <- relative_ratio_index_matrix[,-(1:6)]
  
  #assign value to result list
  result_list <- list()
  result_list[["table_original"]] <- table_original
  for(i in 1:length(filter_vec)){
    result_list[[paste("relativer_ratio_",filter_vec[i],sep = "")]] <-  result_table[relative_ratio_index_matrix[i,],]
  }
  
  result <- list(result_list=result_list,beta_diff=result_table_list$beta_diff,
                 beta_estimation_all=beta_est_all)
  test_time <- proc.time() - test_time
  print("total time is:")
  print(test_time)
  return(result)
}
