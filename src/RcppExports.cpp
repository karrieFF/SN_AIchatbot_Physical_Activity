// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// run_simulation_cpp
List run_simulation_cpp(int num_agents, int n_simulations, Rcpp::CharacterVector methods, Rcpp::IntegerVector stages, Rcpp::CharacterVector stages_name, double p_prior, Rcpp::NumericVector ps_theory, Rcpp::NumericVector adoption_efficacy, double non_adoption_efficacy);
RcppExport SEXP _doipkg_run_simulation_cpp(SEXP num_agentsSEXP, SEXP n_simulationsSEXP, SEXP methodsSEXP, SEXP stagesSEXP, SEXP stages_nameSEXP, SEXP p_priorSEXP, SEXP ps_theorySEXP, SEXP adoption_efficacySEXP, SEXP non_adoption_efficacySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type num_agents(num_agentsSEXP);
    Rcpp::traits::input_parameter< int >::type n_simulations(n_simulationsSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type methods(methodsSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type stages(stagesSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type stages_name(stages_nameSEXP);
    Rcpp::traits::input_parameter< double >::type p_prior(p_priorSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type ps_theory(ps_theorySEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type adoption_efficacy(adoption_efficacySEXP);
    Rcpp::traits::input_parameter< double >::type non_adoption_efficacy(non_adoption_efficacySEXP);
    rcpp_result_gen = Rcpp::wrap(run_simulation_cpp(num_agents, n_simulations, methods, stages, stages_name, p_prior, ps_theory, adoption_efficacy, non_adoption_efficacy));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_doipkg_run_simulation_cpp", (DL_FUNC) &_doipkg_run_simulation_cpp, 9},
    {NULL, NULL, 0}
};

RcppExport void R_init_doipkg(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
