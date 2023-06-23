These R codes implement our Bayesian methodology for posterior inference and causal effect estimation in categorical DAGs

Specifically:

gen_data_data.R : randomly generates a DAG structure on q nodes with a given probability of edge inclusion and generates a categorical binary dataset

mcmc_dag_theta.R : contains the main MCMC algorithm for joint posterior inference on DAGs, DAG-parameters and causal effects

gamma_causal.R : computes causal effects from DAG-parameters

move_dag.R : performs one move from a DAG to an adjacent DAG (implements the proposal distribution over the space of DAGs)

marg_dag.R : computes the (log)marginal likelihood of a DAG model relative to a node-component of the DAG

example.R : implements mcmc_dag_theta.R on a simulated categorical dataset generated using gen_dag_data.R