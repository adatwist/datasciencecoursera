## Put comments here that give an overall description of what your
## functions do

## This function creates and stores a special "matrix" object (mtx) that can cache its inverse (inv)

makeCacheMatrix <- function(mtx = matrix()) {
 inv <- NULL
 set <- function(y) {
   mtx <<- y
   inv <<- NULL
 }
 
 get <- function() mtx
 setinverse <- function(inverse) inv <<- inverse
 getinverse <- function() inv
 list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

  
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(mtx, ...) {
        ## Return a matrix that is the inverse of 'mtx'
  inv <- mtx$getinverse()
  if(!is.null(inv)) {
     message("getting cached data")
     return(inv)
  }
  data <- mtx$get()
  inv <- solve(data, ...)
  mtx$setinverse(inv)
}


