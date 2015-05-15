#include <Rcpp.h>
//[[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
#include <vector>
#include <math.h>

using namespace Rcpp;
using namespace std;
using namespace RcppParallel;

//NodeDist structure makes searching one point's
//k-neighbours much easier because it contains
//both distance and index information.
struct NodeDist
{
  double distance;
  int index;
};

//using the algorithm of finding the nth element 
//of an array in "Introduction of Algorithm".
int partition(vector<NodeDist> & arr, int beg, int end);
int rearrange(vector<NodeDist> & arr, int beg, int end, int p);
//do something to make sure all the numbers left 
//of pth number are not larger than the pth number
//and the right are larger than the pth number
int rearrange_else(vector<NodeDist> & arr, int p, int end);

//calculate k_distance and local reachable density
//in parallel way
struct parallelKNN:public Worker
{
  const unsigned int k;
  const int kmax;
  const unsigned int nrow;
  const RMatrix<double> data;
  RMatrix<double> dist;
  RMatrix<int> indx;
  RVector<int> act_k;
  
  parallelKNN(unsigned int k, int kmax, unsigned int nrow, NumericMatrix data, NumericMatrix dist, IntegerMatrix indx, IntegerVector act_k)
    : k(k), kmax(kmax), nrow(nrow), data(data), dist(dist), indx(indx), act_k(act_k) {}
  
  void operator()(size_t begin, size_t end)
  {
    vector<NodeDist> nodedist(nrow - 1);
    for(size_t i = begin; i < end; ++i)
    {
      RMatrix<double>::Row node0 = data.row(i);
      vector<NodeDist>::iterator dist_pt = nodedist.begin();
      for(unsigned int j = 0; j < nrow; ++j)
      {
        //calculate Euclidean distances
        if(i == j) continue; // don't calculate self's distance
        RMatrix<double>::Row node1 = data.row(j);
        double dist_ = 0;
        for(size_t len = 0; len < node0.length(); ++len)
        {
          dist_ += pow(node0[len] - node1[len], 2);
        }
        dist_ = sqrt(dist_);
        
        //initialize the nodedist vector
        dist_pt->distance = dist_;
        dist_pt->index = j;
        dist_pt ++;
      }
      
      //find k-neighbours to calculate k-distance
      rearrange(nodedist, 0, nrow-2, k - 1);
      act_k[i] = min(rearrange_else(nodedist, k - 1, nrow - 2), kmax - 1);
      for(int j = 0; j <= act_k[i]; ++j)
      {
        dist(i,j) = nodedist[j].distance;
        indx(i,j) = nodedist[j].index;
      }
    }
  }
};

//calculate local outlier factor in serial way
NumericVector LOF(NumericMatrix data, int k, int equal_num)
{
  int nrow = data.nrow();
  int kmax = min(nrow, k+equal_num);
  vector<NodeDist> dist(nrow-1);
  NodeDist *kNN = new NodeDist [nrow * kmax]; //the max number of objects O whose distance to object P equals k_distance
  int *act_k = new int[nrow];
  
  for(int i=0; i<nrow; ++i)
  {
    NumericVector node0 = data.row(i);
    double dist_;
    vector<NodeDist>::iterator dist_pt = dist.begin();
    for(int j=0; j<nrow; ++j)
    {
      dist_ = sqrt(sum((node0 - data.row(j)) * (node0 - data.row(j))));
      if(dist_ > 0)
      {
        dist_pt->distance = dist_;
        dist_pt->index = j;
        dist_pt ++;
      }
    }
    rearrange(dist, 0, nrow-2, k - 1);
    act_k[i] = min(rearrange_else(dist, k - 1, nrow - 2), kmax - 1);
    for(int j=0; j<=act_k[i]; ++j)
    {
      kNN[i*kmax + j] = dist[j];
    }    
  }

  NumericVector lrd(nrow);
  NodeDist neighbor_;
  for(int i=0; i<nrow; ++i)
  {
    double lrd_ = 0;
    for(int j=0; j<=act_k[i]; ++j)
    {
      neighbor_ = kNN[i*kmax + j];
      lrd_ += max(neighbor_.distance, kNN[neighbor_.index * kmax + k - 1].distance);
    }
    lrd[i] = (act_k[i] + 1) / lrd_;
  }

  NumericVector lof(nrow);
  for(int i=0; i<nrow; ++i)
  {
    double lof_ = 0;
    for(int j=0; j<=act_k[i]; ++j)
    {
      lof_ += lrd[kNN[i*kmax + j].index];
    }
    lof[i] = lof_ / ((act_k[i] + 1) * lrd[i]);
  }
  
  delete [] kNN;
  delete [] act_k;
  return lof;
}

//calculate local outlier factor in parallel way
//[[Rcpp::export]]
NumericVector parallelLOF(NumericMatrix data, unsigned int k, int equal_num)
{
  unsigned int nrow = data.nrow();
  int kmax = min(nrow, k+equal_num);
  NumericMatrix dist(nrow, kmax);
  IntegerMatrix indx(nrow, kmax);
  IntegerVector act_k(nrow);
  
  parallelKNN parallelKNN(k, kmax, nrow, data, dist, indx, act_k);
  parallelFor(0, nrow, parallelKNN);
  
  NumericVector lrd(nrow);
  for(unsigned int i=0; i<nrow; ++i)
    {
      double lrd_ = 0;
      for(int j=0; j<=act_k[i]; ++j)
      {
        lrd_ += max(dist(i, j), dist(indx(i, j), k - 1));
      }
      lrd[i] = (act_k[i] + 1) / lrd_;
    }
  

  NumericVector lof(nrow);
  for(unsigned int i=0; i<nrow; ++i)
    {
      double lof_ = 0;
      for(int j=0; j<=act_k[i]; ++j)
      {
        lof_ += lrd[indx(i,j)];
      }
      lof[i] = lof_ / ((act_k[i] + 1) * lrd[i]);
    }
  return lof;
}

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

int rearrange_else(vector<NodeDist> & arr, int p, int end)
{
  if(p + 1 <= end)
  {
    for(int i = p+1; i<=end; ++i)
    {
      if(abs(arr[p].distance - arr[i].distance) < 1e-6)
      {
        p ++;
        swap(arr[p], arr[i]);
      }
    }
  }
  return p;
}
