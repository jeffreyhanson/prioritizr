#include "package.h"
#include "optimization_problem.h"
#include "functions.h"
#include <map>
#include <set>
#include <array>

// [[Rcpp::export]]
bool rcpp_apply_xor_constraints2(
  SEXP x, const Rcpp::List connected_data
) {

  /* The following code makes the following critical assumptions
   *
   * 1. connected_data is a list of lists containing sparse matrices. Each
   *    matrix has the same number of rows and columns, and these are
   *    equal to the number of planning units in x. The nested list
   *    structure of the object is
   *    connected_data[[1:ptr->_number_of_zones]][[1:ptr->_number_of_zones]]
   *
   * 2. the problem already contains constraints so that the total
   *    allocation of each planning unit across all zones is at most one
   *    (see rcpp_add_zones_constraints). This means that, for a given
   *    planning unit j, at most one of the decision variables for j can be
   *    selected, so they can be summed in a single constraint.
   *
   */

  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t A_original_nrow = ptr->_rhs.size();
  std::size_t n_pu = ptr->_number_of_planning_units;
  std::size_t n_z = ptr->_number_of_zones;

  // convert the list of list of sparseMatrix objects to a Rcpp classes
  std::vector<std::vector<arma::sp_mat>> data;
  import_connectivity_matrix_list(connected_data, data, true);

  // create a map to store each conflicts between planning unit allocation
  // variables.
  //
  // for example, consider the following conflicts:
  // (pu_i zone_a) cannot be allocated if (pu_j zone_b)
  // (pu_i zone_a) cannot be allocated if (pu_j zone_c)
  //
  // this map stores these conflicts with the following structure:
  // conflicts[{pu_i_zone_a, pu_j}] = {pu_j_zone_b, pu_j_zone_c}
  std::map<std::array<std::size_t, 2>, std::set<std::size_t>> conflicts;
  for (std::size_t z1 = 0; z1 < n_z; ++z1) {
    for (std::size_t z2 = 0; z2 < n_z; ++z2) {
      for (
        arma::sp_mat::const_iterator itr = data[z1][z2].begin();
        itr != data[z1][z2].end(); ++itr
      ) {
        // only add conflict if the cell indicates a connection with
        // a (meaningfully) non-zero value AND the connection is between
        // different planning units
        if (!((*itr < 1.0e-15) || (itr.row() == itr.col()))) {
          // store the conflict
          conflicts[{(z1 * n_pu) + itr.row(), itr.col()}].insert(
            (z2 * n_pu) + itr.col()
          );
        }
      }
    }
  }

  // prepare data for adding conflicts as constraints.
  // since conflicts may store entries for multiple conflicts that
  // describe the same relationship and the order of the planning units is
  // simply swapped, we will process these data to retain data for building
  // non-duplicate constraints.
  //
  // for example, consider the following entries in conflicts:
  // (pu_i_zone_a) cannot be allocated if (pu_j_zone_a),
  // (pu_j_zone_a) cannot be allocated if (pu_i_zone_a).
  //
  // to resolve this, constraints will contain only the following information:
  // [(pu_i_zone_a), (pu_j_zone_a)]
  //
  // note that we must use std::set to ensure that only non-duplicate
  // information is retained
  std::set<std::set<std::size_t>> constraints;
  std::set<std::size_t> curr_conflict;
  for (auto itr = conflicts.begin(); itr != conflicts.end(); ++itr) {
    curr_conflict = itr->second;
    curr_conflict.insert(itr->first[0]);
    constraints.insert(curr_conflict);
  }

  // add constraints to problem formulation
  std::size_t r = A_original_nrow;
  for (auto itr = constraints.cbegin(); itr != constraints.cend(); ++itr) {
    for (auto itr2 = itr->cbegin(); itr2 != itr->cend(); ++itr2) {
      ptr->_A_i.push_back(r);
      ptr->_A_j.push_back(*itr2);
      ptr->_A_x.push_back(1.0);
    }
    ptr->_sense.push_back("<=");
    ptr->_rhs.push_back(1.0);
    ptr->_row_ids.push_back("x2");
    ++r;
  }

  // return result
  return true;
}
