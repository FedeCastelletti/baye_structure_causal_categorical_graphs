library(pcalg)
library(gRbase)

gen_dag_data = function(i, n, q, prob, delta = NULL){
  
  # This function randomly generates a DAG and a categorical binary dataset
  
  ###########
  ## INPUT ##
  ###########
  
  # i     : a numerical seed, required for reproducibility
  # n     : the number of observations
  # q     : the number of nodes (variables)
  # prob  : a probability of edge inclusion for the DAG random generation
  # delta : a (q,1) vector of cut-offs for categorical-data generation from latent Normal draws
  
  ############
  ## OUTPUT ##
  ############
  
  # dag : the (q,q) adjacency matrix of the generated DAG
  # Y   : the (n,q) generated categorical (binary) dataset
  # mu, Sigma : parameters of the underlying (latent) multivariate Normal for categorical data generation
  
  # If not specified otherwise, cut-offs are set equal to zero for each variable
  
  if(is.null(delta)){
    delta = rep(0, q)
  }
  
  set.seed(i)
  
  dag = t(as(randomDAG(q, prob = prob), "matrix") != 0)*1
  
  B = dag*matrix(runif(q*q, 0.1, 1), q, q)*sample(c(-1, 1), size = q*q, replace = TRUE); diag(B) = 1
  
  Sigma_cond = diag(rep(1, q))
  
  Sigma = solve(t(B))%*%Sigma_cond%*%solve(B); mu = c(rep(0, q))
  mu    = c(rep(0, q))
  
  library(mvtnorm)
  
  Z = data.frame(rmvnorm(n, mu, Sigma))
  Y = Z
  
  for(j in 1:q){
    
    Y[,j] = ifelse(Z[,j] > delta[j], 1, 0)
    
  }
  
  return(list(dag = dag, Y = Y, mu = mu, Sigma = Sigma))
  
}
