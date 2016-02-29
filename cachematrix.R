
# The function "makeCacheMatrix" creates a list contaning the following elements:
# 1) Set the matrix
# 2) Get the matrix
# 3) Set the inverse of the matrix
# 4) Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  B <- NULL
  set <- function(y) {
    x <<- y
    B <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) B <<- inverse
  getinverse <- function() B
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


# The function "cacheSolve" gets the inverse of the matrix from the cache
# and skips the calculation if it has been already done. If not, the inverse
# is calculated via the function "solve" and the result is store in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  B <- x$getinverse()
  if(!is.null(B)) {
    message("getting cached data")
    return(B)
  }
  data <- x$get()
  B <- solve(data, ...)
  x$setinverse(B)
  B
}
