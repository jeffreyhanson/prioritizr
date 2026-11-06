#include "package.h"
#include "optimization_problem.h"
#include "functions.h"
#include <boost/unordered_map.hpp>

// [[Rcpp::export]]
bool rcpp_apply_xor_constraints3(
  SEXP x, const Rcpp::List connected_data, std::size_t max_n
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
  std::size_t n_dv = n_pu * n_z;

  // convert the list of list of sparseMatrix objects to a Rcpp classes
  std::vector<std::vector<arma::sp_mat>> data;
  import_connectivity_matrix_list(connected_data, data, true);

  // calculate total number of non-zero elements in data
  std::size_t data_nonzero = 0;
  for (std::size_t z1 = 0; z1 < ptr->_number_of_zones; ++z1)
    for (std::size_t z2 = 0; z2 < ptr->_number_of_zones; ++z2)
      data_nonzero += data[z1][z2].n_nonzero;

  // declare variables
  boost::unordered_map<std::size_t, std::vector<std::size_t>> conflicts;
  conflicts.reserve(n_dv);
  std::size_t curr_i;
  std::size_t curr_j;

  // extract data from the connected matrix
  for (std::size_t z1 = 0; z1 < ptr->_number_of_zones; ++z1) {
    for (std::size_t z2 = 0; z2 < ptr->_number_of_zones; ++z2) {
      for (arma::sp_mat::const_iterator itr = data[z1][z2].begin();
           itr != data[z1][z2].end(); ++itr) {
        // store conflict if cell value is not zero, or not a diagonal value
        if (!(*itr < 1.0e-15) && (itr.row() != itr.col())) {
          // extract cell data
          curr_i = (z1 * ptr->_number_of_planning_units) + itr.row();
          curr_j = (z2 * ptr->_number_of_planning_units) + itr.col();
          // store connection
          if (conflicts.find(curr_i) != conflicts.end()) {
            // if map already has connection for curr_i...
            conflicts[curr_i].push_back(curr_j);
          } else {
            // otherwise...
            conflicts[curr_i] = {curr_j};
            conflicts.reserve(max_n);
          }
        }
      }
    }
  }

  // add constraints
  std::size_t r = A_original_nrow;
  auto itr2 = conflicts.begin();
  bool is_sym;
  for (auto itr = conflicts.cbegin(); itr != conflicts.cend(); ++itr) {
    // if conflict involves just two planning
    // unit decision variables, then it can be accounted for with the
    // simple constraint:
    // x + y <= 1, where x and y are two different planning units
    if (itr->second.size() == 1) {
      // determine if the constraint is symmetric
      itr2 = conflicts.find(itr->second[0]);
      is_sym =
        (itr2 != conflicts.end()) &&
        (itr2->second.size() == 1) &&
        (itr2->second[0] == itr->first);
      // if conflict is symmetric,
      // then don't add constraint if index for x is greater than y to
      // avoid adding constraints for x + y <= 1 and also y + x <= 1
      if (!(is_sym && (itr->first > itr->second[0]))) {
        ptr->_A_i.push_back(r);
        ptr->_A_j.push_back(itr->first);
        ptr->_A_x.push_back(1.0);
        ptr->_A_i.push_back(r);
        ptr->_A_j.push_back(itr->second[0]);
        ptr->_A_x.push_back(1.0);
        ptr->_sense.push_back("<=");
        ptr->_rhs.push_back(1.0);
        ptr->_row_ids.push_back("x1");
        // increment row counter
        ++r;
      }
    } else {
      // otherwise if conflict involves multiple planning units
      // , add in more complex constraint to account for
      // multiple planning units:
      // (n * x) + y1 + y2 + y3 <= n, where x is a planning unit
      // and y1, y2, and y3 are also planning units, and
      // none of y1, y2, or y3 should not be selected if x is selected
      ptr->_A_i.push_back(r);
      ptr->_A_j.push_back(itr->first);
      ptr->_A_x.push_back(itr->second.size());
      for (
        auto itr3 = itr->second.cbegin(); itr3 != itr->second.cend(); ++itr3
      ) {
        ptr->_A_i.push_back(r);
        ptr->_A_j.push_back(*itr3);
        ptr->_A_x.push_back(1.0);
      }
      ptr->_sense.push_back("<=");
      ptr->_rhs.push_back(itr->second.size());
      ptr->_row_ids.push_back("x2");
      // increment row counter
      ++r;
    }
  }

  // return result
  return true;
}
