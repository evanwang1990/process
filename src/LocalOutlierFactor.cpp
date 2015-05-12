#include <Rcpp.h>
#include <vector>
#include <math.h>

using namespace Rcpp;
using namespace std;


//[[Rcpp::export]]
float dist(vector<float> x, vector<float> y)
{
  float res = 0;
  for(int i=0; i< x.size(); ++i)
  {
    res += pow(x[i] - y[i], 2);
  }
  res = sqrt(res);
  return res;
}

//[[Rcpp::export]]
float kDist(vector<float> dist, int k)
{
  nth_element(dist.begin(), dist.begin() + k, dist.end());
  return dist[k];
}

//[[Rcpp::export]]
int main(int m)
{
  float aa[7] = {2, 0, 3, 5, 7, 2, 6};
  vector<float> AA(aa, aa+7);
  float res = kDist(AA, m);
  for(int i=0; i<7; ++i)
  {
    cout<<AA[i]<<endl;
  }
  cout<<res<<endl;
  return 0;
}