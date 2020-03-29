#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cacheMatrix <- NULL
  set <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cacheMatrix <<- inverse
  getinverse <- function() cacheMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  cacheMatrix <- x$getinverse()
  if (!is.null(cacheMatrix)) {
    message("getting cache matrix")
    return(cacheMatrix)
  }
  dataMatrix <- x$get()
  cacheMatrix <- solve(dataMatrix)
  x$setinverse(cacheMatrix)
  cacheMatrix
}
