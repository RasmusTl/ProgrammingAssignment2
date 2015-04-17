## The below two functions return the inverse of a specified matrix. 
## It starts by checking if it has already been computed.
## If so, it gets the result and skips the computation. 
## If not, it computes the inverse, and sets the value in the cache, for future calls.


# Function below assumes that the specified matrix is always invertible.
makeCacheMatrix <- function(x = matrix()) {
  invert_matrix <- NULL
  set <- function(y) {
    x <<- y
    invert_matrix <<- NULL
  }
  get <- function() x
  setinvert_matrix <- function(invert_matrix) invert_matrix <<- invert_matrix
  getinvert_matrix <- function() invert_matrix
  list(set=set, get=get, setinvert_matrix=setinvert_matrix, getinvert_matrix=getinvert_matrix)
}

## If the cached invert_matrix is calculated, cacheSolve retrieves it, and if not, it computes, stores, and returns it.
cacheSolve <- function(x, ...) {
  invert_matrix <- x$getinvert_matrix()
  if(!is.null(invert_matrix)) {
    message("getting cached data.")
    return(invert_matrix)
  }
  data <- x$get()
  invert_matrix <- solve(data)
  x$setinvert_matrix(invert_matrix)
  invert_matrix
}

## Test to see if it works- luckily it does ;-) ##
q = matrix(data = c(10,20,40,60), nrow = 2, ncol = 2)
y = makeCacheMatrix(q)
y$get()
cacheSolve(y)
cacheSolve(y)

