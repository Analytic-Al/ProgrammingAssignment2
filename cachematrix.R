## Al Thompson 17-FEB-2015   Coursera Programming in R, Assignment 2
## cacheing and lexical scoping examples

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  # get the value of the matrix
  get <- function() x
  
  # set the value of the inverse
  setinv <- function(solve) m <<- solve
  
  # get the value of the inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()                      # lookup this matrix
  if(!is.null(m)) {   
    message("Getting Cached Data")
    return(m)                          # if we've done it before, return that one
  }
  data <- x$get()
  m <- solve(data, ...)                # otherwise, invert it now
  x$setinv(m)                          # and save the invert in the cache, for next time
  m
}
