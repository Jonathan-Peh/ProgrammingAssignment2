## Functions work together to take matrices and return cached inverse matrices if possible. If not, they calculate the inverse matrices and cache it.

## makeCacheMatrix wraps input matrix to give object that can return cached inverse or be interfaced to set inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse){
    inv <<- inverse
  }
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## If possible, cacheSolve returns the cached inverse matrix of the input makeCacheMatrix object
## If the cache is empty, inverse matrix is computed, saved, and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("retrieving cached data...")
    return(inv)
  }
  else{
  data <- x$get()
  inv = solve(data)
  x$setinverse(inv)
  inv
  }
}
