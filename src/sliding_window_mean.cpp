#include <Rcpp.h>
using namespace Rcpp;

//' Sliding window mean
//'
//' Compute mean of each sliding window.
//'
//' @param x A vector of numeric values.
//' @param length An integer. Length of a window.
//' @param by An integer. Step between each window.
//' @param keep_cut A boolean. Indicate if the last uncompleted window should be kept or not.
// [[Rcpp::export]]
NumericVector sliding_window_mean(NumericVector x,
                                  int length,
                                  int by,
                                  bool keep_cut) {
  // Control argument value and identify
  //bool cut = false;
  int last_length = length;
  // the number of required windows
  int n = 1;
  // Check if the window length is higher than
  // the length of the provided vector
  if( x.length() >= length)
  {
    n = floor( (x.length() - length + by) / by );
    if( (n-1)*by+length < x.length() )
    {
      if( keep_cut )
      {
        last_length = x.length() - (n-1)*by+length;
        //cut = true;
        n++;
      }
    }
  }
  else
  {
    // There is only one cut window
    last_length = x.length();

    // Return a null vector if keep_cut is false
    if(! keep_cut)
    {
      NumericVector null(0);
      return(null);
    }
  }
  // Initialize the output vector
  NumericVector values(n);
  int i,j;

  // Count elements
  int step = 0;
  for (i=0; i<n-1 ; i++)
  {
    double value = 0.0;
    for (j=0 ; j<length ; j++)
      value += x[step+j];
    values[i] = value/length;
    step += by;
  }

  // Treat the last window
  double value = 0.0;
  for(j=0 ; j<last_length ; j++)
    value += x[step+j];
  values[n-1] = value/last_length;

  return(values);
}
