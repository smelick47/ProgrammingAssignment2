## makecachematrix return special matrix with functions
## cacheSolve assign the cache value to the given matrix

## makeCacheMatrix takes a matrix as a argument and return special matrix with 
## set , get
## setinverse , getinverse
## functions
makeCacheMatrix <- function(x = matrix()) {
  ## init cache
  cache <- NULL
  
  ## set function take matrix and assign it to x and clear the cache for new assignment 
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  ## return the matrix
  get <- function() x
  
  ## asign the inverse matrix to cache object
  setinverse <- function(inverse) cache <<- inverse
  
  ## retun the cache object
  getinverse <- function() cache
  
  ## retun the special matrix with list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## take the makeCacheMatrix as a argument and assign the inverse to the cache
## if inversible and cache is not present
## if cache is present just return the cache

cacheSolve <- function(x, ...) {
  ## get the cacher inverse  
  inverse <- x$getinverse()
  
  if (!is.null(inverse)){
    message("getting cached inverse")
    return(inverse)
  }
  
  ## get the matrix
  m <- x$get()
  ## calculate the inverse
  tryCatch(
    {
      inv <- solve(m)
      x$setinverse(inv)
    }
    ,error = function(e) {
      message("inverse can not be calculated")
    }
  )
  
}
