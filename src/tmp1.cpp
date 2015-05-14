#include <Rcpp.h>
//[[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
#include <vector>
#include <math.h>

using namespace Rcpp;
using namespace std;
using namespace RcppParallel;

struct NodeDist
{
  double distance;
  int index;
};


struct parallelKNN:public Worker
{
  const unsigned int k;
  const unsigned int kmax;
  const unsigned int nrow;
  const RMatrix<double> data;
  RMatrix<double> dist;
  RMatrix<int> indx;
  RVector<int> act_k;
  
  parallelKNN(unsigned int k, unsigned int kmax, unsigned int nrow, NumericMatrix data, NumericMatrix dist, IntegerMatrix indx, IntegerVector act_k)
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

