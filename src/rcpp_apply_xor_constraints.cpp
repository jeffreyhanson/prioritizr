#include "package.h"
#include "optimization_problem.h"
#include "functions.h"

// [[Rcpp::export]]
bool rcpp_apply_xor_constraints(SEXP x, const Rcpp::List cliques) {

  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t A_original_nrow = ptr->_rhs.size();

  // define variables
  std::size_t r = A_original_nrow;
  std::size_t n_cliques = cliques.size();
  Rcpp::IntegerVector curr_clique;
  std::size_t curr_clique_size;

  // add constraints based on cliques
  for (std::size_t j = 0; j < n_cliques; ++j) {
    // extract y'th zone clique
    curr_clique = cliques[j];
    curr_clique_size = static_cast<std::size_t>(curr_clique.size());
    // add row numbers for constraint matrix values
    for (std::size_t i = 0; i < curr_clique_size; ++i) {
      ptr->_A_i.push_back(r);
    }
    // add column numbers for constraint matrix values
    for (std::size_t i = 0; i < curr_clique_size; ++i) {
      ptr->_A_j.push_back(curr_clique[i] - 1);
    }
    // add coefficients for constraint matrix values
    for (std::size_t i = 0; i < curr_clique_size; ++i) {
      ptr->_A_x.push_back(1.0);
    }
    // add remaining constraint information
    ptr->_sense.push_back("<=");
    ptr->_rhs.push_back(1.0);
    ptr->_row_ids.push_back("x");
    // increment row counter
    ++r;
  }

  // return result
  return true;
}
