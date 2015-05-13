#include <Rcpp.h>
#include <vector>
#include <math.h>

using namespace Rcpp;
using namespace std;

struct NodeDist
{
  double distance;
  int index;
};


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


//[[Rcpp::export]]
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
    
    //
  cout<<"dist: ";
  for(int m=0; m<nrow-1; m++)
  {
    cout<<dist[m].distance<<" ";
  }
  cout<<endl<<"index: ";
  for(int m=0; m<nrow-1; m++)
  {
    cout<<dist[m].index<<" ";
  }
  cout<<endl<<"neigbors: "<<act_k[i]<<endl<<"kNN: ";
  for(int m=i*kmax; m<i*kmax+kmax; ++m)
  {
    cout<<kNN[m].index<<" ";
  }
  cout<<endl;
    
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
    for(int j=0; j<act_k[i]; ++j)
    {
      lof_ += lrd[kNN[i*kmax + j].index];
    }
    lof[i] = lof_ / ((act_k[i] + 1) * lrd[i]);
  }
  
  delete [] kNN;
  delete [] act_k;
  return lof;
}