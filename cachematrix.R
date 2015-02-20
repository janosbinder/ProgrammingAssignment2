## Put comments here that give an overall description of what your
## functions do

## To test whether it is working use the following comments
## A = matrix(c(1,2,3,4), nrow=2, ncol=2, byrow = TRUE)
## m <- makeCacheMatrix(A)
## n = cacheSolve(m)
## m$get() %*% n
## The last command should return the identity matrix

## This function creates a special "vector" for makeCacheMatrix for the matrix passed as an argument
## $get() -> returns the matrix
## $set(m) -> set m matrix as the value of the vector
## $getinverse() -> returns the inverse if calculated
## $setinverse(..$get()) -> calculated the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the matrix stored in the special "vector" created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the inverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
