#include <Rcpp.h>
using namespace Rcpp;
#include <vector>
#include <algorithm> // for std::find

// [[Rcpp::export]]
std::vector<double> getTraitLearningProbability(Rcpp::IntegerMatrix repertoires,
                                                int ind,
                                                Rcpp::List requirements,
                                                std::vector<int> learnableTraits) {
  // Initialize a result vector with 0.0 for each element in learnableTraits
  std::vector<double> pList(learnableTraits.size(), 0.0);

  // If there are no learnable traits, return an empty probability list
  if (learnableTraits.empty()) {
    return pList; // Return an empty vector if no learnable traits
  }

  // Collect the indices of known traits for the given individual
  std::vector<int> knownTraits;
  for (int j = 0; j < repertoires.ncol(); ++j) {
    if (repertoires(ind - 1, j) == 1) { // Convert ind to 0-based index
      knownTraits.push_back(j + 1); // Store as 1-based index
    }
  }

  // Loop through each learnable trait to determine the learning probability
  for (size_t i = 0; i < learnableTraits.size(); ++i) {
    int targetTrait = learnableTraits[i];
    Rcpp::List requirement = requirements[targetTrait - 1]; // Convert to 0-based index

    // Check against the requirements for the target trait
    if (requirement.size() >= 2) {
      bool allReq1Known = true;
      bool allReq2Known = true;

      // Check all elements of the first requirement list
      Rcpp::IntegerVector req1 = requirement[0];
      for (int req : req1) {
        if (std::find(knownTraits.begin(), knownTraits.end(), req) == knownTraits.end()) {
          allReq1Known = false;
          break;
        }
      }

      // Check all elements of the second requirement list
      Rcpp::IntegerVector req2 = requirement[1];
      for (int req : req2) {
        if (std::find(knownTraits.begin(), knownTraits.end(), req) == knownTraits.end()) {
          allReq2Known = false;
          break;
        }
      }

      // If either set of requirements is fully known, set the probability to 1
      if (allReq1Known || allReq2Known) {
        pList[i] = 1.0;
      }
    } else if (requirement.size() >= 1) {
      bool allReq1Known = true;

      // Check all elements of the single requirement list
      Rcpp::IntegerVector req1 = requirement[0];
      for (int req : req1) {
        if (std::find(knownTraits.begin(), knownTraits.end(), req) == knownTraits.end()) {
          allReq1Known = false;
          break;
        }
      }

      // If the single set of requirements is fully known, set the probability to 1
      if (allReq1Known) {
        pList[i] = 1.0;
      }
    }
  }

  return pList; // Return the computed probabilities list
}
