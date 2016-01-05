#' After loading .Rdata file, the object name is table_res1
#' Basically speaking, table_res1 consists of two list,
#' first list is called result_list, it stored all the table I showed in the report,
#' second list is called beta_estimation_all, it stored all the beta estimation for each simulation
#'
#'

# load R object
load("Scenario_A.Rdata")

##############################
# get first list result_list
##############################
# get all the result
# all the table are stored in a list
# the table here means what I showed in the report
table_list <- table_res1$result_list
# print out each table
for(i in 1:length(table_list)){
  print(names(table_list)[i])
  print(round(table_list[[i]],4))
}

##############################
# get second list beta_estimation_all
##############################
# get all the estimation and some parameter about details of setting up the simulation
# all the estimation are stored in a list
est_list <- table_res1$beta_estimation_all
# get estimation for FLASSO, it is a matrix
FLASSO_matrix <- est_list$FLASSO
# get estimation for PSCAD for all iteration,it is a list
PSCAD_list <- est_list$PSCAD
# get the estimation of SCAD for first iteration, it is a matrix
PSCAD1_matrix <- PSCAD_list[[1]]
# get the estimation of SCAD for second iteration, it is a matrix
PSCAD2_matrix <- PSCAD_list[[2]]
