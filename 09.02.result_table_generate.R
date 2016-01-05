#' !!! important: This version has been modified to test scenario, DO NOT use this version
#'                before you confirm everything.


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
#'result_table_all:define a function that generate the result table for all rows
#'                 c("FLASSO","FSCAD","FMCP","CLASSO","CSCAD","CMCP","PLASSO"),
#'                 PSCAD*iteration_time, PMCP*iteration_time
#'input: beta_result_list from #09.01. beta_matrix_generate
#'       beta_vecort: the true value of beta, used to get the zero position and caculate L_infinity_norm
#'       filter_vec: a vector to store coeffient to muliple relative bias to filter beta estimation
#'       n_iter_SCAD: iteration time for SCAD, if 0, no SCAD will be estimated
#'       n_iter_MCP: iteration time for MCP, if 0, no MCP will be estimated
#'output: the talble we need
result_table_all <- function(beta_result_list,beta_vec,filter_vec,n_iter_SCAD,n_iter_MCP){
  print("!!! important: This version has been modified to test scenario, 
         DO NOT use this version before you confirm everything.
        Add beta difference table")
  #this function will use a result_table_single to generate a table contains all method
  
  #get beta estimation table from different method and remove intercept
  #table_name <- c("FLASSO","FSCAD","FMCP","CLASSO","CSCAD","CMCP","PLASSO","PSCAD","PMCP")
  table_name <- c("FLASSO","FSCAD","FMCP","CLASSO","CSCAD","CMCP","PLASSO")
  #initial a list to store all the beta estimation
  print(str(beta_result_list))
  beta_list <- list()
  for(i in 1:length(table_name)){
    print(beta_result_list[[table_name[i]]])
    beta_list[[table_name[i]]] <- beta_result_list[[table_name[i]]]
  }
    
  
  # generate diff table
  beta_diff_list <- list()
  
  for(i in 1:length(table_name)){
    print(beta_result_list[[table_name[i]]])
    print(class(beta_result_list[[table_name[i]]]))
    beta_diff_list[[table_name[i]]] <- t(t(beta_result_list[[table_name[i]]]) - c(0,beta_vec)) 
  }
  # generate diff table for SCAD with n_iter_SCAD iteration
  PSCAD_name <- character(n_iter_SCAD)
  for(i in 1:n_iter_SCAD){
    PSCAD_name[i] <- paste("PSCAD",i,sep = "")
  }
  for (i in 1:n_iter_SCAD){
    beta_diff_list[[PSCAD_name[i]]] <- t(t(beta_result_list[["PSCAD"]][[i]]) - c(0,beta_vec)) 
  }
  
  #generate diff table for MCP with n_iter_MCP iteration
  PMCP_name <- character(n_iter_MCP)
  for(i in 1:n_iter_MCP){
    PMCP_name[i] <- paste("PMCP",i,sep = "")
  }
  for (i in 1:n_iter_MCP){
    beta_diff_list[[PMCP_name[i]]] <- t(t(beta_result_list[["PMCP"]][[i]]) - c(0,beta_vec))
  }
  
  nopenalty_name <- names(beta_result_list[["NOPENALTY"]])
  for(i in 1:length(nopenalty_name)){
    beta_diff_list[[nopenalty_name[i]]] <- t(t(beta_result_list[["NOPENALTY"]][[nopenalty_name[i]]]) - c(0,beta_vec))
  }
  
  
  #assign beta estimation to each beta_name
#   beta_full <- beta_result_list$beta_method1_full[,-1]
#   beta_complete <- beta_result_list$beta_method2_complete[,-1]
#   beta_logistics_our <- beta_result_list$beta_method3_our[,-1]
  
  #initial a list to store result
  table_list <- list()
  #generate table for full data with three penalty, complete data with three penalty and logistics with LASSO
  for (i in 1:(length(table_name))){
    table_list[[table_name[i]]] <- result_table_single(beta_result_matrix=beta_list[[table_name[i]]][,-1],
                                                      beta_vec=beta_vec,
                                                      method_name=table_name[i],filter_vec=filter_vec)
  }
  #generate table for SCAD with n_iter_SCAD iteration
  PSCAD_name <- character(n_iter_SCAD)
  for(i in 1:n_iter_SCAD){
    PSCAD_name[i] <- paste("PSCAD",i,sep = "")
  }
  for (i in 1:n_iter_SCAD){
    table_list[[PSCAD_name[i]]] <- result_table_single(beta_result_matrix=beta_result_list[["PSCAD"]][[i]][,-1],
                                                       beta_vec=beta_vec,
                                                       method_name=PSCAD_name[i],filter_vec=filter_vec)
  }
  
  #generate table for MCP with n_iter_MCP iteration
  PMCP_name <- character(n_iter_MCP)
  for(i in 1:n_iter_MCP){
    PMCP_name[i] <- paste("PMCP",i,sep = "")
  }
  for (i in 1:n_iter_MCP){
    table_list[[PMCP_name[i]]] <- result_table_single(beta_result_matrix=beta_result_list[["PMCP"]][[i]][,-1],
                                                       beta_vec=beta_vec,
                                                       method_name=PMCP_name[i],filter_vec=filter_vec)
  }
#   
#   #generate result table
#   table1_full <- result_table_single(beta_result_matrix=beta_full,beta_vec=beta_vec,method_name="Method1_full",filter_vec=filter_vec)
#   table2_complete <- result_table_single(beta_result_matrix=beta_complete,beta_vec=beta_vec,method_name="Method2_complete",filter_vec=filter_vec)
#   table3_logistic_our <- result_table_single(beta_result_matrix=beta_logistics_our,beta_vec=beta_vec,method_name="Method3_logistics_our",filter_vec=filter_vec)
  
  #result <- rbind(table1_full,table2_complete,table3_logistic_our)
  result <- table_list[[1]]
  for (i in 2:length(table_list)){
    result <- rbind(result,table_list[[i]])
  }
  
  result <- list(table=result,beta_diff=beta_diff_list)
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
  table_col_names <- c("rho","r_sd","L_inf","L_sd","L_1","L_1_sd","L_2","L_2_sd",
                       names(table_all_t0n0(beta_matrix=beta_result_matrix,beta_vec=beta_vec)))
  table_row_names <- c(method_name)
  
  #calculate rho
  rho_list <- relative_bias(beta_matrix=beta_result_matrix,beta_vec=beta_vec)
  rho <- rho_list$beta_ratio_max
  rho_sd <- rho_list$beta_ratio_max_sd
  #print(rho)
  
  #first row without filter
  L_inf_vec <- L_inf(beta_matrix=beta_result_matrix,beta_vec=beta_vec)
  L_1_vec <- L_1(beta_matrix=beta_result_matrix,beta_vec=beta_vec)
  L_2_vec <- L_2(beta_matrix=beta_result_matrix,beta_vec=beta_vec)
  row_without_filter <- c(0,0,
                          L_inf_vec[1],L_inf_vec[2],L_1_vec[1],L_1_vec[2],L_2_vec[1],L_2_vec[2],
                          table_all_t0n0(beta_matrix=beta_result_matrix,beta_vec=beta_vec))
  result <- row_without_filter
  #print(result)
  #ohter row with filter
  for (i in filter_vec){
    if(i<0.1){
      table_row_names <- c(table_row_names,paste(method_name," ",i,sep = ""))
      #print(i)
      current_beta_vec <- beta_est_filter_bias(beta_matrix=beta_result_matrix,beta_vec=beta_vec,th1=i)
      L_inf_vec_rho <- L_inf(beta_matrix=current_beta_vec,beta_vec=beta_vec)
      L_1_vec_rho <- L_1(beta_matrix=current_beta_vec,beta_vec=beta_vec)
      L_2_vec_rho <- L_2(beta_matrix=current_beta_vec,beta_vec=beta_vec)
      current_row_with_filter <- c(mean(i),sd(i),
                                   L_inf_vec_rho[1],L_inf_vec_rho[2],
                                   L_1_vec_rho[1],L_1_vec_rho[2],
                                   L_2_vec_rho[1],L_2_vec_rho[2],
                                   table_all_t0n0(beta_matrix=current_beta_vec,beta_vec=beta_vec))
    }else{
      table_row_names <- c(table_row_names,paste(method_name," ",i,"*rho",sep = ""))
      #print(i)
      current_beta_vec <- beta_est_filter(beta_matrix=beta_result_matrix,beta_vec=beta_vec,th1=rho*i)
      L_inf_vec_rho <- L_inf(beta_matrix=current_beta_vec,beta_vec=beta_vec)
      L_1_vec_rho <- L_1(beta_matrix=current_beta_vec,beta_vec=beta_vec)
      L_2_vec_rho <- L_2(beta_matrix=current_beta_vec,beta_vec=beta_vec)
      current_row_with_filter <- c(mean(rho*i),sd(rho*i),
                                   L_inf_vec_rho[1],L_inf_vec_rho[2],
                                   L_1_vec_rho[1],L_1_vec_rho[2],
                                   L_2_vec_rho[1],L_2_vec_rho[2],
                                   table_all_t0n0(beta_matrix=current_beta_vec,beta_vec=beta_vec))
    }
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
#'L_inf: calculate the L_inf value and standard deviance, 
#'       this function calculate L_inf for each estimation and take average for all L_inf value
#'       and caculate the standard deviance of all L_inf value
#'input: beta_estimation matrix(every row is an estimation, it does NOT inlcude intercept), 
#'       true value of beta
#'ouput: a vector first element is  average L_inf for all the beta_estimation
#'                second element is standard deviance for all the beta_estiamtion
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
  result <- c(mean(beta_max),sd(beta_max))
  return(result)
}


L_1 <- function(beta_matrix,beta_vec){
  # check beta length match
  if ((dim(beta_matrix)[2]) != length(beta_vec)) stop("beta length not match L_inf")
  #beta_estimation - beta_true
  beta_minus_true <- t(t(beta_matrix) - beta_vec)
  #print(beta_minus_true)
  #take abs value
  beta_minus_true <- abs(beta_minus_true)
  #print(beta_minus_true)
  #find maximum in each row
  beta_max <- apply(beta_minus_true, 1, sum)
  #print(beta_max)
  #average of all L_norm
  result <- c(mean(beta_max),sd(beta_max))
  return(result)
}

L_2 <- function(beta_matrix,beta_vec){
  # check beta length match
  if ((dim(beta_matrix)[2]) != length(beta_vec)) stop("beta length not match L_inf")
  #beta_estimation - beta_true
  beta_minus_true <- t(t(beta_matrix) - beta_vec)
  #print(beta_minus_true)
  #take square
  beta_minus_true <- (beta_minus_true)^2
  #print(beta_minus_true)
  #find maximum in each row
  beta_max <- apply(beta_minus_true, 1, sum)
  #print(beta_max)
  #get sqrt
  beta_max <- sqrt(beta_max)
  #average of all L_norm
  result <- c(mean(beta_max),sd(beta_max))
  return(result)
}

####################################
#' relative_bias: calculate relative ratio for non-zero beta for each estimation and
#'                and standard deviance of all estimation and return a list
#' input: beta_estimation matrix(every row is an estimation, it does NOT inlcude intercept), 
#'        true value of beta,
#' output: a list that store the relative ratio for non-zero beta for each estimation
#'                            and standard deviance of all estimation

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
  beta_ratio_max_sd <- sd(beta_ratio_max)
  result <- list(beta_ratio_max=beta_ratio_max,
                 beta_ratio_max_sd=beta_ratio_max_sd)
  return(result)
}


####################################
#' table_all_t0n0: define a function that calculate "t0e0","t0en0","tn0e0","tn0en0" value for each row and return
#'                 the average and stand deviance for all rows as a vector
#' input: beta_estimation matrix(every row is an estimation, it does NOT inlcude intercept), 
#'        true value of beta,
#' output: a vector that store the average value for all rows as "t0e0","t0en0","tn0e0","tn0en0" table
#'                            and standard deviance as "t0e0_sd","t0en0_sd","tn0e0_sd","tn0en0_sd"

table_all_t0n0 <- function(beta_matrix,beta_vec){
  # check beta length match
  if ((dim(beta_matrix)[2]) != length(beta_vec)) stop("beta length not match table_all_t0e0")
  # matrix to store result
  res_matrix <- matrix(-99,dim(beta_matrix)[1],4)
  #colnames(res_matrix) <- c("tn0en0","tn0e0","t0e0","t0en0","tn0en0_sd","tn0e0_sd","t0e0_sd","t0en0_sd")
  colnames(res_matrix) <- c("tn0e0","t0en0","tn0e0_sd","t0en0_sd")
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
    res_matrix[i,] <- c(tn0e0,t0en0,rep(-99,2))
  }
  result <- colMeans(res_matrix)
  sd <- apply(res_matrix[,1:2],2,sd)
  result[3:4] <- sd
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

####################################
#'beta_est_filter_bias: generate new beta_est matrix after converting some beta_est(which is 0 in true value) to
#'                      zero for every beta_est(which is 0 in true value) which its bias(abs(beta_hat-beta)) is less than threshold
#'input: beta_estimation matrix(every row is an estimation, it does NOT inlcude intercept), 
#'       true value of beta
#'       th1: scalar, the threshold
#'output: a matrix is the new beta estimation based on threshold

beta_est_filter_bias <- function(beta_matrix,beta_vec,th1){
  # check beta length match
  if ((dim(beta_matrix)[2]) != length(beta_vec)) stop("beta length not match table_all_t0e0")
  # get zero index
  zero_index <- which(beta_vec == 0)
  #result matrix
  beta_after_filter <- beta_matrix
  #bias matrix
  beta_bias <- t(t(beta_matrix)-beta_vec)
  beta_bias <- abs(beta_bias)
  print(beta_bias)
  #change value based on the threshold
  for(i in zero_index){
    for(j in 1:dim(beta_matrix)[1]){
      beta_after_filter[j,i] <- ifelse(abs(beta_bias[j,i])<th1,0,beta_matrix[j,i])
    }
  }
  return(beta_after_filter)
}




