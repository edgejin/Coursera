## This function will make a list about the "special" matrix
## The specical matrix includes the following 4 different function 1. set 2. get 3. setsolve 4 getsolve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The function will take the special matrix defined above as a parameter into it and calculate the inverse of the matrix
## if the matrix is the same as before, then it will output the cache, otherwise, the new inverse will be calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
