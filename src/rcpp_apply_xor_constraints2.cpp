#include "package.h"
#include "optimization_problem.h"
#include "functions.h"

// [[Rcpp::export]]
bool rcpp_apply_xor_constraints2(
  SEXP x, const Rcpp::List z_cliques, const Rcpp::List pu_cliques
) {

  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t A_original_nrow = ptr->_rhs.size();

  // define variables
  std::size_t r = A_original_nrow;
  std::size_t n_z_cliques = z_cliques.size();
  std::size_t n_pu_cliques = pu_cliques.size();
  Rcpp::IntegerVector curr_z_clique;
  Rcpp::IntegerVector curr_pu_clique;
  std::size_t n_curr_z_clique;
  std::size_t n_curr_pu_clique;

  // add constraints
  for (std::size_t y = 0; y < n_z_cliques; ++y) {
    // extract y'th zone clique
    curr_z_clique = z_cliques[y];
    n_curr_z_clique = static_cast<std::size_t>(curr_z_clique.size());
    for (std::size_t j = 0; j < n_pu_cliques; ++j) {
      // extract j'th planning unit clique
      curr_pu_clique = pu_cliques[j];
      n_curr_pu_clique = static_cast<std::size_t>(curr_pu_clique.size());
      // add row numbers for constraint matrix values
      for (std::size_t i = 0; i < n_curr_z_clique * n_curr_pu_clique; ++i) {
        ptr->_A_i.push_back(r);
      }
      // add column numbers for constraint matrix values
      for (std::size_t z = 0; z < n_curr_z_clique; ++z) {
        for (std::size_t i = 0; i < n_curr_pu_clique; ++i) {
          ptr->_A_j.push_back(
            ((curr_z_clique[z] - 1) * ptr->_number_of_planning_units) +
            curr_pu_clique[i] - 1
          );
        }
      }
      // add coefficients for constraint matrix values
      for (std::size_t i = 0; i < n_curr_z_clique * n_curr_pu_clique; ++i) {
        ptr->_A_x.push_back(1.0);
      }
      // add remaining constraint information
      ptr->_sense.push_back("<=");
      ptr->_rhs.push_back(1.0);
      ptr->_row_ids.push_back("x2");
      // increment row counter
      ++r;
    }
  }

  // return result
  return true;
}
