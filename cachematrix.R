## The makeCacheMarix function creates a matrix and caches it's inverse.
## The cacheSolve function computes the inverse of the matrix.

## This function creates a special "matrix" object that can cache its inverse.
##It has four functions to get and set matix and inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
  i<<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x
  setInverse<-function(inverse) i<<-inverse
  getInverse<-function()i
  list(set=set,get=get,
       setInverse=setInverse,
       getInverse=getInverse
  )
}


## This function computes the inverse of the a matrix returned by makeCacheMatrix above.
## If the inverse has already been computed the the function returns it.
## If the inverse has not been computed, the function computes the inverse and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return (i)
  }
  data<-x$get()
  i<-solve(data)
  x$setInverse(i)
  i
}
