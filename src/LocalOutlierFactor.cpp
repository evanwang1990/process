#include <vector>
#include <math.h>
#include <Rcpp.h>

using namespace Rcpp;
using namespace std;


//[[Rcpp::export]]
float dist(NumericVector x, NumericVector y)
{
  float res = sqrt(sum((x - y) * (x - y)) + 0E-5);
  return res;
}

//[[Rcpp::export]]
float kDist(NumericVector dist, int k)
{
  nth_element(dist.begin(), dist.begin() + k, dist.end());
  return dist[k];
}

//[[Rcpp::export]]
IntegerVector kNN(NumericVector dist, NumericVector k_dist, int node)
{
  IntegerVector index;
  for(int i=0; i<k_dist.size(); ++i)
  {
    if(dist[i] <= k_dist[node] & dist[i] != 0) index.push_back(i);
  }
  return index;
}

//[[Rcpp::export]]
NumericVector lofactor1(NumericMatrix data, int k)
{
  int num = data.nrow();

  NumericVector k_dist(num);
  for(int i=0; i<num; ++i)
  {
    NumericVector dist1(num);
    for(int j=0; j<num; ++j)
    {
      dist1[j] = dist(data.row(i), data.row(j));
    }
    k_dist[i] = kDist(dist1, k);
  }
  
  NumericVector lrd(num);
  for(int i=0; i<num; ++i)
  {
    NumericVector dist1(num);
    for(int j=0; j<num; ++j)
    {
      dist1[j] = dist(data.row(i), data.row(j));
    }
    
    float tmp = 0;
    int count = 0;
    for(int n=0; n<num; ++n)
    {
      if(dist1[n] != 0 & dist1[n] <= k_dist[i])
      {
        tmp += max(k_dist[n], dist1[n]);
        count ++;
      }
    }
    lrd[i] = count / tmp;
  }
  
  NumericVector lof(num);
  for(int i=0; i<num; ++i)
  {
    NumericVector dist1(num);
    for(int j=0; j<num; ++j)
    {
      dist1[j] = dist(data.row(i), data.row(j));
    }
    
    float tmp = 0;
    int count = 0;
    for(int n=0; n<num; ++n)
    {
      if(dist1[n] != 0 & dist1[n] <= k_dist[i])
      {
        tmp += lrd[n];
        count ++;
      }
    }
    lof[i] = count * lrd[i] / tmp;
  }
  return lof;
}