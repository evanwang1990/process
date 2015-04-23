#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

int partition(NumericVector& A, int p, int r)
{
  int i = p;
  // choose the partition number randomly
  int loc = rand() % (r - p + 1) + p;
  swap(A[loc], A[r]);
  
  float x = A[r];
  for(int j = p; j < r; j ++){
    if(A[j] < x){
      swap(A[j], A[i]);
      i ++;
    }
  }
  swap(A[i], A[r]);
  return i;
}


//rearrange the vector A to make sure that the front K elments are smaller than others
float rearrange(NumericVector& A, const int p, const int r, const int K)
{
  int q = partition(A, p, r);
  int len = q - p + 1;
  if(len == K){
		return A[q];
	}
	else if (len > K){
		return rearrange(A, p, q-1, K);
	}
	else{
		return rearrange(A, q+1, r, K - len);
	}
}


//[[Rcpp::export]]
float TrimSum (NumericVector A, int p, int r, int K)
{
  NumericVector B = clone(A);
  float q_left = rearrange(B, p, r, K);
  float q_right = rearrange(B, K + 1, r, r - 2 *K +1);
  return sum(B[seq(K, (r- K))]);
}

//[[Rcpp::export]]
float TrimMean(NumericVector A, int p, int r, int K)
{
  return TrimSum(A, p, r, K) / (r + 1 - 2.0 * K);
}

//[[Rcpp::export]]
NumericVector test(NumericVector A, int p, int r, int K)
{
  NumericVector B = clone(A);
  float q_left = rearrange(B, p, r, K);
  float q_right = rearrange(B, K + 1, r, r - 2 *K +1);
  NumericVector res = B;
  return res;
}