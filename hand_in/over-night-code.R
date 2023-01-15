library(tidyverse)
set.seed(42)
# initial values
K <- 950 # equilibrium density
lambda <- 1.5 # growth rate
N_init <- 1000 # intial total density
c <- -2 # complexity
mu <- 0.01 # mutation rate
# we will define later r as the relative density à savoir N/mean(K)

# population initiale
population <- data.frame(K=rep(K, N_init), lambda=rep(lambda, N_init), c=rep(c, N_init))


# calculate fitnesses
calc_fitness <- function(N, K, lambda, c) {
  # make sure we're not accidentally dividing by 0
  if (any(lambda == 1)) {
    lambda <- 1.0000000001
  }
  res <- lambda / (1 + (lambda - 1)*((N/K)**((1-c)*lambda/(lambda-1))))
  return(res)
}
vector_fitness <- function(input, N) {
  lambda = input[2]
  if (any(lambda == 1)) {
    lambda <- 1.0000000001
  }
  res <- lambda / (1 + (lambda - 1)*((N/input[1])**((1-input[3])*lambda/(lambda-1))))
  return(res)
}

# run
fitness <- with(population[1,], calc_fitness(N_init, K, lambda, c))
population$fitness <- rep(fitness, N_init)

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

run_infinitely_while_saving_to_file <- function() {
  population_with_mutation <- read.table("population.txt")
  generations_with_mutation <- read.table("generations.txt")
  cnt <- nrow(generations_with_mutation)
  while (TRUE) {
    population_with_mutation <- next_generation_with_mutation(population_with_mutation)
    generations_with_mutation <- rbind(generations_with_mutation, 
                                       c(nrow(population_with_mutation), 
                                         mean(population_with_mutation$K), 
                                         sd(population_with_mutation$K), 
                                         mean(population_with_mutation$lambda), 
                                         sd(population_with_mutation$lambda), 
                                         mean(population_with_mutation$c), 
                                         sd(population_with_mutation$c), 
                                         mean(population_with_mutation$fitness), 
                                         sd(population_with_mutation$fitness),
                                         nrow(population_with_mutation)/mean(population_with_mutation$K)))
    write.table(population, file="population.txt")
    write.table(generations_with_mutation, file="generations.txt")
    cnt <- cnt + 1
    print(cnt)
  }
}

generations <- data.frame(N=nrow(population_with_mutation), K_means=mean(population_with_mutation$K), K_sd=sd(population_with_mutation$K), lambda_means=mean(population_with_mutation$lambda), lambda_sd=sd(population_with_mutation$lambda), c_means=mean(population_with_mutation$c), c_sd=sd(population_with_mutation$c), fitness_means=mean(population_with_mutation$fitness), fitness_sd=sd(population_with_mutation$fitness))

write.table(generations, file="generations.txt")
write.table(population, file="population.txt")

set.seed(420)
run_infinitely_while_saving_to_file()
