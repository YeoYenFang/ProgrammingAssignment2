## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is a function that create a 'Matrix' object that is a list containing funtion to 
## set the Matrix and cache it
## get the Matrix 
## set the Inverse Matrix and cache it
## get the Inverse Matrix 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solveMatrix) inv <<- solveMatrix
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve is a function that computes the inverse of the Matrix which is return by makeCacheMatrix.
## If matrix not changed and the inverse has already been calculated, 
## cacheSolve retrieve the inverse from the cache. 
## If Matrix is changed, cacheSolve calculates the inverse
## set the inverse Matrix in the cache via setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
