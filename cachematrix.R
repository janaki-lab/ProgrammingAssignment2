## program to set matrix,get matrix,set inverse and get inverse 
# and return a matrix that is the inverse of x

## 2 functions makeCacheMatrix and cacheSolve
#make cacheMatrix is a function used to set matrix,get matrix,set inverse and get inverse 

makeCacheMatrix<- function(x=matrix())
{
  inv=NULL
  set<-function(y)
  {
    x<<-y
    inv=NULL
  }
  get<- function(){x}
  setInverse<- function(inverse)
  {
    inv<<-inverse
  }
  getInverse<-function()
  {
    inv
  }
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## cacheSolve is a function used to return a matrix that is the inverse of 'x'

cacheSolve<-function(x, ...)
{
  inv<-x$getInverse()
  if(!is.null(inv))
  {
    message("getting cache data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat, ...)
  x$setInverse(inv)
  inv
}