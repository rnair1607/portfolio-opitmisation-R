library(quantmod)
myStocks <- c("TSLA", "IBM", "INTC","NVDA","GOOGL","COP","XOM","HPQ","SONY","ORCL")
stock_pool <- sort(c("AAPL","MSFT","GOOG","GOOGL","AMZN","TSLA","NVDA","META","ASML","AVGO","PEP",
                     "COST","AZN","XOM","TMUS","ADBE","NFLX","CMCSA","TXN","QCOM","AMD","HON","AMGN",
                     "SBUX","PDD","INTU","INTC","GILD","AMAT","ADP","BKNG","ADI","PYPL","MDLZ","HPQ",
                     "REGN","IBM","VRTX","FISV","LRCX","MU","CSX","SONY","CHTR","ORCL","COP",
                     "KLAC","SNPS","MAR","MNST"))

getSymbols(myStocks, from="2019-01-01",to="2020-01-01")
genarate_population <- function (pop_size) {
  for(i in 1:pop_size){
    wts <- runif(n=length(myStocks))
    
    wts <- wts/sum(wts)
    if(i == 1){
      pop_mat <- rbind(wts)  
    }
    else{
      pop_mat <- rbind(pop_mat,wts)
    }
   
  }
  return(pop_mat)
}

constraint = function(x) {
  boundary_constr = (sum(x)-1)**2   # "sum x = 1" constraint
  
  for (i in 1:length(x)) {
    boundary_constr = boundary_constr + max(c(0,x[i]-1))**2 + max(c(0,-x[i]))**2     
  }
  
  return (boundary_constr)
}

create_data <- function(stocks){
  prices <- lapply(stocks,function(x) {Ad(get(x))})
  names(prices) <- c(stocks)
  
  
  
  # Daily returns
  daily_ret <- lapply(prices,dailyReturn)
  daily_ret <- do.call(merge,daily_ret)
  colnames(daily_ret) <- stocks
  
  
  
  
  
  
  # Annualized returns
  annual_ret <- lapply(prices,yearlyReturn)
  annual_ret <- do.call(merge,annual_ret)
  colnames(annual_ret) <- stocks
  
  
  
  
  # Covariance matrix
  cov_mat <- cov(daily_ret)
  
  return(list("prices" = prices, 
              "daily_ret" = daily_ret, 
              "annual_ret" = annual_ret, 
              "cov_mat" =   cov_mat))
}



obj_opt_func <- function(wts,data,fast){
  days = 252
  cov_mat <- data$cov_mat
  
  #Risk
  daily_risk <- t(wts) %*% cov_mat %*% wts 
  yearly_risk <- ((1 + daily_risk)^days)-1
  yearly_risk <- sqrt(yearly_risk) 
  
  #Return
  yearly_return <- sum(data$annual_ret * wts)

  #Sharpe ratio
  sharpe <- yearly_return/yearly_risk
  
  
  
  if (fast) {
    return(sharpe-100*constraint(wts))
    }
  else {
    return(list("return" = yearly_return, 
                "risk" = yearly_risk, 
                "fitness" = sharpe-100*constraint(wts)))
  }
  
  
  
}


library("GA")

optimal_returns = function(stock){
  data_mat <- create_data(stock)
  ga_res <- ga(
    type="real-valued", 
    fitness = function(x){obj_opt_func(x,data_mat,fast=TRUE)}, 
    lower = rep(0,length(myStocks)), 
    upper = rep(1,length(myStocks)),  
    maxiter = 7000, 
    run=300, 
    monitor=TRUE,
    seed=1,
    popSize = 300
  )
  
  return(ga_res)
}

sol = optimal_returns(myStocks)
print(summary(sol))
sum(summary(sol)$solution)
evolved <- c(summary(sol)$solution)
class(evolved)
plot(sol)

###################################################################################################################################
#Comparison with different weights
###################################################################################################################################
eq_wts <- c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1) 
rnd_wts <- runif(10)
rnd_wts <- rnd_wts/sum(rnd_wts)

data_mat <- create_data(myStocks)
eq_data <- obj_opt_func(eq_wts,data_mat,fast=FALSE)
rnd_data <- obj_opt_func(rnd_wts,data_mat,fast=FALSE)
evolved_data <- obj_opt_func(evolved,data_mat,fast=FALSE)
rnd_data
eq_data
evolved_data

###################################################################################################################################
# Future unseen data
###################################################################################################################################
getSymbols(myStocks, from="2020-01-01",to="2021-01-01")
data_mat <- create_data(myStocks)
eq_future_data <- obj_opt_func(eq_wts,data_mat,fast=FALSE)
evolved_future_data <- obj_opt_func(evolved,data_mat,fast=FALSE)
rnd_future_data <- obj_opt_func(rnd_wts,data_mat,fast=FALSE)
eq_future_data
evolved_future_data
rnd_future_data

###################################################################################################################################
#Max risk portfolio
###################################################################################################################################
getSymbols(myStocks, from="2019-01-01",to="2020-01-01")
obj_max_risk_func <- function(wts,data,fast){
  days = 252
  
  #Risk
  daily_risk <- t(wts) %*% data$cov_mat %*% wts 
  yearly_risk <- ((1 + daily_risk)^days)-1
  yearly_risk <- sqrt(yearly_risk) 
  #Return
  yearly_return <- sum(data$annual_ret * wts)
  #fitness value
  fitness <- (yearly_risk-100*constraint(wts)) 
  
  if (fast) {
    return(fitness)
  }
  else {
    return(list("return" = yearly_return, 
                "risk" = yearly_risk, 
                "fitness" = fitness))
  }

}

max_risk_ga = function(stock){
  data_mat <- create_data(stock)
  ga_risk_res <- ga(
    type="real-valued", 
    fitness = function(x){obj_max_risk_func(x,data_mat,fast=TRUE)}, 
    lower = rep(0,length(myStocks)), 
    upper = rep(1,length(myStocks)),  
    maxiter = 7000, 
    run=300, 
    monitor=TRUE,
    seed=1,
    popSize = 300
  )
  
  return(ga_risk_res)
}

risk_sol = max_risk_ga(myStocks)
print(summary(risk_sol))
sum(summary(risk_sol)$solution)
risk_evolved <- c(summary(risk_sol)$solution)
data_mat <- create_data(myStocks)
risk_evolved_perf <- obj_max_risk_func(risk_evolved,data_mat,fast=FALSE)
risk_evolved_perf
plot(risk_sol)
###################################################################################################################################
#Comparison of high risk portfolio with random and even weights
###################################################################################################################################
data_mat <- create_data(myStocks)
risk_evolved_perf <- obj_max_risk_func(risk_evolved,data_mat,fast=FALSE)
risk_eq_data <- obj_opt_func(eq_wts,data_mat,fast=FALSE)
risk_rnd_data <- obj_opt_func(rnd_wts,data_mat,fast=FALSE)
risk_evolved_perf
risk_eq_data
risk_rnd_data

###################################################################################################################################
#GA to select 10 stocks from a pool of 50
###################################################################################################################################
get_stocks <- function(bit_vector, big_list)
{
  selected <- c()
  for (i in 1:length(bit_vector))
  {
    if (bit_vector[i]==1) { 
      
      selected <- append(selected,big_list[i]) 
      }
  }
  return (selected)
}


bit_port_obj <- function(bit_vector, big_list, in_GA)
{
  
  std_vector <- c(1,1,1,1,1,1,1,1,1,1)
  #std_vector <- std_vector/sum(std_vector)
  
  penalty <- (2 * (sum(bit_vector)-10)^2) 
  
  #Get data for the selected stocks  
  stock_list <- get_stocks(bit_vector, big_list)
  #print(stock_list)
  data_mat <- create_data(stock_list)
  
  daily_ret <- data_mat$daily_ret
  yearly_ret <- data_mat$yearly_ret
  cov_mat <- data_mat$cov_mat
  
  
  
  #Return
  yearly_return <- sum(std_vector * yearly_ret)
  
  #Performance adjusted by penalty if incorrect number of assets in the portfolio
  final_fitness <- yearly_ret - penalty
  
  if (in_GA) {
    return(final_fitness)
    }
  else {
    return(list(
                "yearly_return" = yearly_ret, 
                
                "fitness" = final_fitness))
  }
}

random_50bits <- function(x) {
 
  for (r in 1:100) {
    
    random_assets <-sample(1:50,10)
    empty_port <- rep(0,50) #empty vector with just ones
    chosen<- replace(empty_port,random_assets,1)#replace 10 positions randomly with 1
    if (r == 1) {
      initialPop <- rbind(chosen)
    }
    else
    {
      initialPop <- rbind(initialPop,chosen) #append each row to the matrix
    }
  }
  return(initialPop) #return matrix of population to the GA function
}


bit_portfolio_GA <- function(big_list, in_GA=TRUE) {
  GA_sol <- ga(type="binary",
             nBits = 50,
             maxiter = 7000,
             fitness = function(x) {bit_port_obj(x, big_list, in_GA)},
             popSize = 100,
             population = random_50bits
  )
  
  return(GA_sol)
}

GA_port_sol = bit_portfolio_GA(stock_pool,TRUE)
print(summary(GA_port_sol))
sum(summary(GA_port_sol)$solution)
plot(GA_port_sol)

bit_port_obj(GA_port_sol@solution, stock_pool, FALSE)

