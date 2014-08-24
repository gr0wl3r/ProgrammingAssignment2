## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates the matrix to hold cache information and sets out
## a list of functions to besed on that matrix
makeCacheMatrix <- function(x = matrix()) {
  ## Creates the cache matrix to be hold inverse
  s <- NULL
  
  ## Set function to move data into variable matrix 
  ## and clear the cache matrix
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  ## Get function
  get <- function() x
  
  ## SetInverse function moves solution into cache matrix
  setInverse <- function(solve) s <<- solve
  
  ## GetInverse function checks contents of cache matrix
  getInverse <- function() s
  
  list (set = set, get = get, 
        setInverse = setInverse, 
        getInverse = get Inverse)

}


## Write a short comment describing this function
## cacheSolve computes the inverse of a matrix and caches the 
## information in the CacheMAtrix. CacheSolve also checks whether
## the inverse had already bee computed and returns it if it had.
## Thus savaing computing time and resources.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getInverse()
  
  ## Check if already solved
  if(!is.null(s)){
    return s
  }
  ##Calculate the inverse
  holder <- x$get()
  s <- solve(holder, ...)
  
  ## Cache the solution
  x$setInverse(s)
  
  s
  
}

