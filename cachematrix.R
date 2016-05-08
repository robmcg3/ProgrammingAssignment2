## These two functions work in tandem to cache the inverse of a matrix
## in a variable in a seperate environment
## first, cache the matrix using the makeCacheMatrix function
## then solve the matrix with the cacheSolve function
## subsequent calls to the cacheSolve function will retrieve the value
## of the inverse matrix

## The first function,  makeCacheMatrix  creates a list of functions, 
## which perform the following:
# set: set the value of the matrix in a cached variable
# get: get the value of the matrix from the cached variable
# setinverse: set the value of the inverse using solve() in a cached variable
# getinverse: get the value of the inverse from the cached variable
# 
# during the initial call, the data is cached and a null value is entered into the 
# cached inverse


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
       getinverse = getinverse)}


## The second function cacheSolve checks to see whether there is a null value 
## in the cached inverse value.
## if the value is null, the inverse is calculated
## if the value is not null, the cached inverse is returned.


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
