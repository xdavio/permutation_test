#include <Rcpp.h>
using namespace Rcpp;

inline int randWrapper(const int n) { return floor(unif_rand()*n); }

// [[Rcpp::export]]
IntegerVector randomShuffle(IntegerVector a) {
    // clone a into b to leave a alone
    IntegerVector b = clone(a);
    std::random_shuffle(b.begin(), b.end(), randWrapper);
    return b;
}

// [[Rcpp::export]]
NumericVector c_fly(NumericMatrix df,
		    IntegerVector treat_group,
		    IntegerVector cont_group,
		    IntegerVector ind,
		    int N)
{
  // zero-index first

  NumericVector difference_mat(N);

  IntegerVector ind_shuff(ind.size());
  IntegerVector treat_shuff(treat_group.size());
  IntegerVector cont_shuff(cont_group.size());
    
  NumericVector treat(df.nrow());
  NumericVector cont(df.nrow());
  
  int z;
  for (z=0; z<N; ++z) {
    ind_shuff = randomShuffle(ind);
    treat_shuff = ind_shuff[treat_group];
    cont_shuff = ind_shuff[cont_group];

    int i, j;
    for (i=0; i<df.nrow(); ++i) {
      treat(i) = 0;
      for (j=0; j<treat_shuff.size(); ++j) {
	treat(i) += df(i, treat_shuff(j));
      }
      treat(i) = treat(i) / treat_shuff.size();

      cont(i) = 0;
      for (j=0; j<cont_shuff.size(); ++j) {
	cont(i) += df(i, cont_shuff(j));
      }
      cont(i) = cont(i) / cont_shuff.size();
      
      difference_mat(z) += std::abs(treat(i) - cont(i));
    }
  }  
  return difference_mat;
}
