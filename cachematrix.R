## Create two functions - makeCacheMatrix and cacheSolve that caches the inverse of a matrix
## and returns the cached result instead of performing a new inverse operation if the same input is given

## makeCacheMatrix is a function that creates a matrix object 
## that gets an input square matrix and caches its inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) m <<- mean
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function which checks if the inverse of the matrix input has alraedy been calculated 
## and returns the cached result 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m   
}
