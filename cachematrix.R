## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates a "special" matrix,
# with x as the matrix and inv as the inverse of the matrix
# get and set are the getters and setters for the matrix x
# getInv and setInv are the getters and setters for inverse of matrix x

# The list created at the end allows for the $ operator extraction
# x$get will extract the matrix and x$getInv will extract the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

# First check if the inverse is already calculated. if it isn't,
# calculate the matrix with the built in solve() function.
# Finally, set the calculated inverse using x$setInv
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  
  if(!is.null(inv)){
    message("Getting cached inverse")
    return(inv)
  }
  
  matrix <- x$get
  inv <- solve(matrix, ...)
  x$setInv(inv)
  inv
}
