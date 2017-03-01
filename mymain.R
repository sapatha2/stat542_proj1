# Main file, coded by Shivesh Pathak and Jean Liu
RMSE <- function(x,y){
  a <- sqrt(sum((log(x)-log(y))^2)/length(y))
  return(a)
}
source('./prep_data.R')
source('./simple_lin.R')
source('./ridge_lasso_forest.R')
print("Completed Run")