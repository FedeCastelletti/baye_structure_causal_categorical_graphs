## Generate DAG and data

q = 10
n = 1000

library(pcalg)
library(plyr)
library(gtools)
library(devtools)

install_github("mldv/bnlearn")

library(bnlearn)

source("gen_dag_data.R")
source("mcmc_dag_theta.R")
source("gamma_causal.R")

## Simulate a DAG (dag) and generate a categorical dataset Y

data_sim = gen_dag_data(i = 1, n = n, q = q, prob = 2/q)

dag = data_sim$dag
Y   = data_sim$Y

## Set prior hyperparameters

a = 1 # Dirichlet prior

a_pi = 1 # Beta Binomial on DAG, p(D)
b_pi = q

## Run MCMC scheme

S    = 12000
burn = 2000

t0 = proc.time()
out = mcmc_dag_param(Y, S, a, a_pi, b_pi, ne = 4, verbose = FALSE, vset = 5)
t1 = proc.time() - t0

## Posterior draws from DAGs

out$Dags[,,1]
out$Dags[,,S]

## Posterior draws from DAG-parameter theta

out$Theta[[1]]
out$Theta[[S]]

## Recover posterior probabilities of edge inclusion

probs = apply(X = out$Dags, MARGIN = c(1,2), FUN = mean)
round(probs, 2)

## Posterior distribution of causal-effect parameter corresponding to do(X3 = x) on Y = 1

summary(out$Out_causal)
