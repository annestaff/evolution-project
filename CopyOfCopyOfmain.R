library(tidyverse)

offspring <- function(individual) {
  n_offspring <- rpois(1, individual[4])
  if (is.na(n_offspring)) {
    return(vector())
  } else {
    offspring <- data.frame(K=rep(individual[1], n_offspring), lambda=rep(individual[2], n_offspring), c=rep(individual[3], n_offspring), fitness=rep(individual[4], n_offspring))
    return(offspring)
  }
}

next_generation <- function(population){
  next_gen <- apply(population, 1, offspring)
  next_gen <- bind_rows(next_gen)
  next_gen$fitness <- apply(next_gen, 1, vector_fitness, N=nrow(next_gen))
  return(next_gen)
}

next_generation_with_mutation <- function(population){
  next_gen <- next_generation(population)
  n_mutations <- rpois(1, nrow(next_gen)*mu)
  mutants <- sample(nrow(next_gen), n_mutations, replace=FALSE)
  if(length(mutants) > 0) {
    for(i in 1:(ncol(next_gen)-1)) {
      next_gen[mutants,i] <- next_gen[mutants, i] + rnorm(1, 0, i*0.04)
    }
    next_gen[mutants,4] <- with(next_gen[mutants,], calc_fitness(nrow(next_gen), K, lambda, c))
  }
  return(next_gen)
}

run_infinitely_while_saving_to_file()
