# Both functions allow the user to cache an inverse of a matrix.
# cacheSolve requires a list given by makeCacheMatrix, makeCache-
# Matrix requires a matrix as an input.
# The r.package matlib is needed for the solve function.


# makeCacheMatrix takes a given matrix, and returns a list
# of functions that:
# 1. set, creates an object with the value of the matrix and x an
# empty object for the inverse i.
# 2, get, retrieves the value of the matrix x.
# 3, setInverse, allows for a new object to be stored on the 
# variable i.
# 4. getInverse. retrieves the object saved on i, 
makeCacheMatrix <- function(x = matrix()) {
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  set(x)
  get <- function() {
    x
    }
  setInverse <- function(inverse){
    i<<- inverse
    }
  getInverse <- function(){
    i
    }
  list(set = set, get = get, setInverse = setInverse, 
	getInverse = getInverse)
}


# cacheSolve takes a list of objects created by makeCacheMatrix and
# looks if the inverse has been calculated. If that is the case, it
# just retrieves the matrix. Else it looks to calculate de matrix 
# using solve from the r.package, and set that value to i.
cacheSolve <- function(x, ...) {
  
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}