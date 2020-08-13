## Put comments here that give an overall description of what your
## functions do

## There are two functions makeCacheMatrix, makeCacheMatrix
## makeCacheMatrix consist of set, get, setInv, getInv
## Library(MASS) is used to calculate inverse for non squared as well as square matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function(){x}
      setInv <- function(inverse) {inv <<- inverse}
      getInv <- function() {inv}
      list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
## This is used to get the cache data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getInv()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setInv(inv)
      inv
}
