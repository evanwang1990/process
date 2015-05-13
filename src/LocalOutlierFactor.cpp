#include <vector>
#include <math.h>
#include <Rcpp.h>

using namespace Rcpp;
using namespace std;

struct NodeDist
{
  double distance;
  int index;
};

<<<<<<< HEAD
//[[Rcpp::export]]
float dist(NumericVector x, NumericVector y)
{
  float res = sqrt(sum((x - y) * (x - y)) + 0E-5);
  return res;
}

//[[Rcpp::export]]
float kDist(NumericVector dist, int k)
=======

int partition(vector<NodeDist> & arr, int beg, int end)
{
  int p = beg - 1;
  for(int i=beg; i<end; ++i)
  {
    if(arr[i].distance <= arr[end].distance)
    {
      p ++;
      swap(arr[i], arr[p]);
    }
  }
  p ++;
  swap(arr[p], arr[end]);
  return p;
}

int rearrange(vector<NodeDist> & arr, int beg, int end, int p)
{
  if(beg == end) return 0;
  int p0 = partition(arr, beg, end);
  if(p == p0) return 0;
  if(p0 > p) 
    return rearrange(arr, beg, p0-1, p);
  else 
    return rearrange(arr, p0+1, end, p);
}

//[[Rcpp::export]]
double Dist(NumericVector x, NumericVector y)
>>>>>>> 07013a2210d2e189fd551d0a2a7e4d743415a968
{
  double res = sqrt(sum((x - y) * (x - y)));
  return res;
}


//[[Rcpp::export]]
<<<<<<< HEAD
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
=======
NumericVector LOF(NumericMatrix data, int k)
{
  int nrow = data.nrow();
  vector<NodeDist> dist(nrow);
  NodeDist *kNN = new NodeDist [nrow * k];
  
  for(int i=0; i<nrow; ++i)
  {
    NumericVector node0 = data.row(i);
    double dist_;
    for(int j=0; j<nrow; ++j)
    {
      dist_ = Dist(node0, data.row(j));
      if(dist_ > 0)
      {
        dist[j].distance = dist_;
        dist[j].index = j;
      }
    }
    rearrange(dist, 0, nrow-1, k);
    for(int j=0; j<k; ++j)
    {
      kNN[i*k + j] = dist[j];
    }
  }
  
  NumericVector lrd(nrow);
  NodeDist neighbor_;
  for(int i=0; i<nrow; ++i)
  {
    double lrd_ = 0;
    for(int j=0; j<k; ++j)
    {
      neighbor_ = kNN[i*k + j];
      lrd_ += max(neighbor_.distance, kNN[(neighbor_.index + 1) * k - 1].distance);
    }
    lrd[i] = k / lrd_;
  }
  
  NumericVector lof(nrow);
  for(int i=0; i<nrow; ++i)
  {
    double lof_ = 0;
    for(int j=0; j<k; ++j)
    {
      lof_ += lrd[kNN[i*k + j].index];
    }
    lof[i] = lof_ / (k * lrd[i]);
>>>>>>> 07013a2210d2e189fd551d0a2a7e4d743415a968
  }
  return lof;
}