## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  # initiate dummy variable
  set <- function(y) { # set value of matrix
    x <<- y           
    m <<- NULL
  }
  get <- function() x # get value of matrix
  setinverse <- function(solve) m <<- solve # set inverse
  getinverse <- function() m  # get inverse
  list(set = set, get = get,  # create list object to be returned
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) { # check if inverse has been calculated
    message("getting cached data")  # print whether cached data is used
    return(m)              # return cached data
  }
  data <- x$get()     # get matrix 
  m <- solve(data, ...)  # calculate inverse matrix
  x$setinverse(m)      # cache inverse 
  m                   # return the inverse
}
