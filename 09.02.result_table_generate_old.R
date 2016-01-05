#09.02 result table generate
#'
#'structure:
#'result_table_all => result_table_single => L_inf
#'                                           relative_bias
#'                                           table_all_t0n0
#'                                           beta_est_filter
#'                                           
#'                                          
#'


####################################
#'result_table_all:define a function that generate the result table
#'input: beta_result_list from #09.01. beta_matrix_generate
#'       beta_vecort: the true value of beta, used to get the zero position and caculate L_infinity_norm
#'       filter_vec: a vector to store coeffient to muliple relative bias to filter beta estimation
#'output: the talble we need
result_table_all <- function(beta_result_list,beta_vec,filter_vec){
  #this function will use a result_table_single to generate a table contains all three method
  
  #get beta estimation table from different method and remove intercept
  beta_full <- beta_result_list$beta_method1_full[,-1]
  beta_complete <- beta_result_list$beta_method2_complete[,-1]
  beta_logistics_our <- beta_result_list$beta_method3_our[,-1]
  
  #generate result table
  table1_full <- result_table_single(beta_result_matrix=beta_full,beta_vec=beta_vec,method_name="Method1_full",filter_vec=filter_vec)
  table2_complete <- result_table_single(beta_result_matrix=beta_complete,beta_vec=beta_vec,method_name="Method2_complete",filter_vec=filter_vec)
  table3_logistic_our <- result_table_single(beta_result_matrix=beta_logistics_our,beta_vec=beta_vec,method_name="Method3_logistics_our",filter_vec=filter_vec)
  
  result <- rbind(table1_full,table2_complete,table3_logistic_our)
  return(result)
}



####################################
#'result_table_single:define a function that generate the result table
#'input: beta-result-matrix from result list from #09.01. beta_matrix_generate, can be either with
#'       intercept or without intercpt, if with intercept, the intercept should be first columns
#'       method_name: a string used to store in the result row names to indicate what method and dataset.
#'       filter_vec: a vector to store coeffient to muliple relative bias to filter beta estimation
#'output: the single table for only one method
result_table_single <- function(beta_result_matrix,beta_vec,method_name,filter_vec){
  #remove intercept column
  if (((dim(beta_result_matrix)[2])-1) == length(beta_vec)){
    beta_matrix <- beta_result_matrix[,-1]
    print(paste("for",method_name,"remove intercept column"))
    print(paste("the name in beta_vec used to calculation is",paste(colnames(beta_matrix),collapse=" "),"in",method_name))
  }else if((dim(beta_result_matrix)[2]) == length(beta_vec)){
    beta_matrix <- beta_result_matrix
    print(paste("for",method_name,"without remove intercept column"))
    print(paste("the name in beta_vec used to calculation is",paste(colnames(beta_matrix),collapse=" "),"in",method_name))
  } else stop("cannot decide whether should remove intercept")
  
  #prepare result names
  #table_col_names <- c("L_inf_norm","rho","tn0en0","tn0e0","t0e0","t0en0")
  table_col_names <- c("L_inf_norm","rho",names(table_all_t0n0(beta_matrix=beta_result_matrix,beta_vec=beta_vec)))
  table_row_names <- c(method_name)
  
  #calculate rho
  rho <- relative_bias(beta_matrix=beta_result_matrix,beta_vec=beta_vec)
  #print(rho)
  
  #first row without filter
  row_without_filter <- c(L_inf(beta_matrix=beta_result_matrix,beta_vec=beta_vec),
                          0,
                          table_all_t0n0(beta_matrix=beta_result_matrix,beta_vec=beta_vec))
  result <- row_without_filter
  #print(result)
  #ohter row with filter
  for (i in filter_vec){
    table_row_names <- c(table_row_names,paste(i,"* rho"))
    #print(i)
    current_beta_vec <- beta_est_filter(beta_matrix=beta_result_matrix,beta_vec=beta_vec,th1=rho*i)
    current_row_with_filter <- c(L_inf(beta_matrix=current_beta_vec,beta_vec=beta_vec),
                                 mean(rho*i),
                                 table_all_t0n0(beta_matrix=current_beta_vec,beta_vec=beta_vec))
    #print(current_row_with_filter)
    result <- rbind(result,current_row_with_filter)
  }
  
  if (dim(result)[1] != length(table_row_names)) stop("result row number wrong,result_table_single")
  if (dim(result)[2] != length(table_col_names)) stop("result column number wrong,result_table_single")
  colnames(result) <- table_col_names
  rownames(result) <- table_row_names
  return(result)
}

####################################
#'L_inf: calculate the L_inf value,here, this function calculate L_inf for each estimation and take average for all L_inf value
#'input: beta_estimation matrix(every row is an estimation, it does NOT inlcude intercept), 
#'       true value of beta
#'ouput: a scalar average L_inf for all the beta_estimation
L_inf <- function(beta_matrix,beta_vec){
  # check beta length match
  if ((dim(beta_matrix)[2]) != length(beta_vec)) stop("beta length not match L_inf")
  #beta_estimation - beta_true
  beta_minus_true <- t(t(beta_matrix) - beta_vec)
  #print(beta_minus_true)
  #take abs value
  beta_minus_true <- abs(beta_minus_true)
  #print(beta_minus_true)
  #find maximum in each row
  beta_max <- apply(beta_minus_true, 1, max)
  #print(beta_max)
  #average of all L_norm
  result <- mean(beta_max)
  return(result)
}

####################################
#' relative_bias: calculate relative ratio for non-zero beta for each estimation and return a vector
#' input: beta_estimation matrix(every row is an estimation, it does NOT inlcude intercept), 
#'        true value of beta,
#' output: a vector that store the relative ratio for non-zero beta for each estimation

relative_bias <- function(beta_matrix,beta_vec){
  # check beta length match
  if ((dim(beta_matrix)[2]) != length(beta_vec)) stop("beta length not match relative_bias")
  non_zero_index <- which(beta_vec != 0)
  #(beta_estimation - beta_true)/beta_true
  beta_matrix <- beta_matrix[,non_zero_index]
  beta_vec <- beta_vec[non_zero_index]
  # check beta length match
  if ((dim(beta_matrix)[2]) != length(beta_vec)) stop("beta length not match after removing zero beta relative_bias")
  beta_ratio<- t((t(beta_matrix) - beta_vec)/beta_vec)
  #print(beta_ratio)
  #take abs value
  beta_ratio <- abs(beta_ratio)
  #print(beta_ratio)
  #find maximum in each row
  beta_ratio_max <- apply(beta_ratio, 1, max)
  return(beta_ratio_max)
}


####################################
#' table_all_t0n0: define a function that calculate "t0e0","t0en0","tn0e0","tn0en0" value for each row and return
#'                 the average for all rows as a vector
#' input: beta_estimation matrix(every row is an estimation, it does NOT inlcude intercept), 
#'        true value of beta,
#' output: a vector that store the average value for all rows as "t0e0","t0en0","tn0e0","tn0en0" table

table_all_t0n0 <- function(beta_matrix,beta_vec){
  # check beta length match
  if ((dim(beta_matrix)[2]) != length(beta_vec)) stop("beta length not match table_all_t0e0")
  # matrix to store result
  res_matrix <- matrix(-99,dim(beta_matrix)[1],4)
  colnames(res_matrix) <- c("tn0en0","tn0e0","t0e0","t0en0")
  #generate the table
  nonzero_ind <- which(beta_vec != 0)
  total_covariate <- length(beta_vec)
  nonzero_num <- sum(beta_vec != 0)
  for (i in 1:dim(beta_matrix)[1]){
    beta_current <- beta_matrix[i,]
    tn0en0 <- sum(beta_current[nonzero_ind] != 0)
    tn0e0 <- nonzero_num - tn0en0
    t0en0 <- sum(beta_current[-nonzero_ind] != 0)
    t0e0 <- total_covariate - nonzero_num - t0en0
    res_matrix[i,] <- c(tn0en0,tn0e0,t0e0,t0en0)
  }
  result <- colMeans(res_matrix)
  return(result)
}

####################################
#'beta_est_filter: generate new beta_est matrix after converting some beta_est(which is 0 in true value) to
#'                zero for  every beta_est(which is 0 in true value) is less than input threshold th1
#'input: beta_estimation matrix(every row is an estimation, it does NOT inlcude intercept), 
#'       true value of beta
#'       th1: vector the threshold
#'output: a matrix is the new beta estimation based on threshold

beta_est_filter <- function(beta_matrix,beta_vec,th1){
  # check beta length match
  if ((dim(beta_matrix)[2]) != length(beta_vec)) stop("beta length not match table_all_t0e0")
  # get zero index
  zero_index <- which(beta_vec == 0)
  #result matrix
  beta_after_filter <- beta_matrix
  #change value based on the threshold
  for(i in zero_index){
    for(j in 1:dim(beta_matrix)[1]){
      beta_after_filter[j,i] <- ifelse(abs(beta_matrix[j,i])<th1[j],0,beta_matrix[j,i])
    }
  }
  return(beta_after_filter)
}



