## Put comments here that give an overall description of what your
## functions do

##  creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Get the matrix data
  get <- function() x
  
  ## Store the inverse value
  setinverse <- function(inverse) m <<- inverse
  
  ## REtrieve the inverse
  getinverse <- function() m
  
  ## return the function list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## check if the inverse value is already calculated
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting inverse from cache")
    ## Return the inverse from cache
    return(m)
  }
  message("Not cached. Calculating inverse")
  ## Reading the matrix
  data <- x$get()
  ## Calculating inverse
  m <- solve (data) ##(data, ...)
  ## Caching the value
  x$setinverse(m)
  ## Returning the inverse 
  return(m)
 
}
