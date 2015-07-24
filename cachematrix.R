## Both functions below where based on the cachemean example given by the instructor
## cacheSolve was improved to accept a normal matrix the first time is used
## and to check the input's class.


## Creates a cachematrix object that holds a matrix and its inverse cached
## How to use: cachematrix <- makeCacheMatrix(m) where m is a normal matrix
makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  
  ## cachematrix$set(newm) - resets the object with other normal matrix
  set <- function(newmatrix) {
    m <<- newmatrix
    inv <<- NULL
  }
  
  ## cachematrix$get() - returns the normal matrix
  get <- function() m
  
  ## cachematrix$setinverse(invm) - sets the inverse matrix
  setinverse <- function(inverse) inv <<- inverse
  
  ## cachematrix$getinverse() - returns the inverse matrix
  getinverse <- function() inv
  
  ## a cachematrix is a named list of the 4 functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of a cachematrix. 
## Only computes the inverse matrix if it's not in cache yet. 
## Assumes that the matrix supplied is always invertible.
cacheSolve <- function(m, ...) {
  
  ## Helping first usage only
  if (is.matrix(m)) {
    message("You can't use this function in a normal matrix.")
    if (is.null(cm)) {
      message("I'll help you this time by converting it to the cachematrix object called 'cm'.")
      message("Next time, use cachematrix <- makeCacheMatrix(matrix) and then cacheSolve(cachematrix).")
      cm <<- makeCacheMatrix(m)
    } else {
      ## If cm is not null, it's not safe to override its value with this cachematrix
      return(FALSE)
    }
  } else if (!is.function(m$get)) {
    message("Please provide a cachematrix object as input.")
    message(paste("You provided a",class(m)))
    return(FALSE)
  } else {
    inv <- m$getinverse()
    if(!is.null(inv)) {
      message("This inverse matrix has already been calculated.")
      message("Retrieving cached data.")
      return(inv)
    }
    cm <- m
  }
  
  ## Solving the inverse matrix
  inv <- solve(cm$get(), ...)
  
  ## Setting the cache to be used next time
  cm$setinverse(inv)
  
  ## Return the inverse matrix
  inv
}
