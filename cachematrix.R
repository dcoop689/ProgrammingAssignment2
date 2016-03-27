## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## start value of inverse is NULL
  inv <- NULL
  ##Where the value will be cached. Matrix is created and changes are made to the cached matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #get is assigned the value x
  get <- function() x
  #calculates the inverse of the matrix
  setinverse <- function(solve) inv <<- solve
  #gets the inverse
  getinverse <- function() inv
  # passes value of makeCacheMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # used to get the cache of the matrix
  inv <- x$getinverse()
  # is the inverse isn't null. it will return the cached inverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #if the inverse isn't retrieved it calculates it below.
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  return(inv)
}
