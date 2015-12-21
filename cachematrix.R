## cache potentially time-consuming computations of matrix inverse computation.
## it can be looked up in the cache rather than recomputed. 


## The function makeVector creates a special "matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(X = matrix()) {
  inverseX <- NULL
  set <- function(anotherX) {
    X <<- anotherX
    inverseX <<- NULL
  }
  get <- function() X
  setinverse <- function(val) inverseX <<- val
  getinverse <- function() inverseX
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## the function cacheSolve calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the mean has already been calculated. 
# If so, it gets the mean from the cache and skips the computation. 
# Otherwise, it calculates the mean of the data and sets the value of the mean 
#   in the cache via the setmean function.

cacheSolve <- function(M, ...) {
  inverseX <- M$getinverse()
  if(!is.null(inverseX)) {
    message("getting cached data")
    return(inverseX)
  }
  X <- M$get()
  inverseX <- solve(X, ...)
  M$setinverse(inverseX)
  inverseX
}


### some testing code
set.seed(123456)
X <- matrix(rnorm(2250000), 1500, 1500)
a <- makeCacheMatrix(X)
result1 <- cacheSolve(a)
result2 <- cacheSolve(a)
all(result1 == result2)

X <- matrix(rnorm(2250000), 1500, 1500)
a$set(X)
result1 <- cacheSolve(a)
result2 <- cacheSolve(a)
all(result1 == result2)
