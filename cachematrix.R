# makeCacheMatrix
# 1. sets the value of the matrix
# 2. gets the value of the matrix
# 3. sets the value of inverse of the matrix
# 4. gets the value of inverse of the matrix

makeCacheMatrix <- function(a = matrix()) {
  inv <- NULL
  set <- function(b) {
        a <<- b
        inv <<- NULL
  }
  get <- function() a
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# cacheSolve
# 1) Returns the inverse of the matrix
# 2) Checks if the inverse has been computed
# 3) In case it has been computed, it gets the result and skips the computation
# 4) If it has not been computed, it sets the value in the cache by the setInverse() function

cacheSolve <- function(a, ...) {
  inv <- a$getInverse()
  if (!is.null(inv)) {
       message("extracting cached data")
       return(inv)
  }
  mat <- a$get()
  inv <- solve(mat, ...)
  a$setInverse(inv)
  inv
}
