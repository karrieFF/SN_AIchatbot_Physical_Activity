% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_simulation.R
\name{run_simulation}
\alias{run_simulation}
\title{Run Simulation}
\usage{
run_simulation(
  num_agents,
  n_simulations,
  methods,
  stages,
  stages_name,
  p_prior,
  ps_theory,
  adoption_efficacy,
  non_adoption_efficacy
)
}
\arguments{
\item{num_agents}{Number of agents}

\item{n_simulations}{Number of simulations}

\item{methods}{Methods for comparison}

\item{stages}{Stages of the process}

\item{stages_name}{Names of the stages}

\item{p_prior}{Prior probability of adoption}

\item{ps_theory}{Theoretical probabilities}

\item{adoption_efficacy}{Adoption efficacy values}

\item{non_adoption_efficacy}{Non-adoption efficacy value}
}
\value{
A list of final probabilities
}
\description{
Run Simulation
}
