#include <Rcpp.h>
#include <vector>
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


//[[Rcpp::export]]
int tmp(NumericVector dist, int p)
{
  vector<NodeDist> res(dist.size());
  for(int i=0; i<dist.size(); ++i)
  {
    res[i].distance = dist[i];
    res[i].index = i;
  }
  rearrange(res, 0, dist.size() - 1, p);
  for(int i=0; i<=p; ++i)
  {
    cout<<"distance: "<<res[i].distance<<"; index: "<<res[i].index<<endl;
  }
  return 0;
}