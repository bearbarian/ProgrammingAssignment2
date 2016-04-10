## Oleg Vinogradov, 2016
## Couple functions for caching the intermediate calculations on matrices

##
## This function creates a matrix-like object which contains the original matrix plus the placeholder for the inverse of it
##
makeCacheMatrix <- function(m_orig = matrix()) {
  m_inv <- NULL
  set <- function(m) {
    m_orig <<- m
    m_inv <<- NULL
  }
  get <- function() m_orig
  setInverse <- function(inv) m_inv <<- inv
  getInverse <- function() m_inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##
## This function accepts a cacheMatrix object created by makeCacheMatrix function and returns an inverse matrix
## The calculated inverse matrix is memorized in cache and can be immediately returned on the consequent calls to this function 
##
cacheSolve <- function(m, ...) {
  m_inv <- m$getInverse()
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  data <- m$get()
  m_inv <- solve(data, ...)
  m$setInverse(m_inv)
  m_inv
}
