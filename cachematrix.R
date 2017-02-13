## To cache the inversion of a matrix, and to retrieve the
## inverse from the cache

## This function creates a special "matrix" object that 
## can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  xInverse <- NULL
  set <- function(y){
    x <<- y
    xInverse <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) xInverse <<- inverse
  getInverse <- function() xInverse
  list(set=set,get=get,
       setInverse=setInverse,
       getInverse=getInverse)

}

##This function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated 
##(and the matrix has not changed), then the 
##cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        inverse <- makeCacheMatrix(x)$getInverse()
        if (!is.null(inverse)){
          massage("getting cache data")
          return(inverse)
        }
        
        data <- makeCacheMatrix(x)$get()
        inverse <- solve(data)
        makeCacheMatrix(x)$setInverse(inverse)
        inverse
}
