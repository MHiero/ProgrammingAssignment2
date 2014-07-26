
############### ProgrammingAssignment2 ##########################
## The following functions may be used, to compute the inverse ##
## of a matrix and store it as cached data for further use.    ##
## @author MHiero
## @version 1.3

############## makeCacheMatrix ##################################
## This function creates a special "matrix" object that can    ##
## cache its inverse.                                          ##
## @param: Call parameter x, an invertible matrix              ##
## @return:  cacheable matrix object                           ##

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

############## cacheSolve ##################################
## This function computes the inverse of the special      ##
## "matrix" returned by makeCacheMatrix above. If the     ##
## inverse has already been calculated (and the matrix    ##
## has not changed), then cacheSolve should retrieve the  ##
## inverse from the cache.                                ##
##                                                        ##
## @param: Call parameter x, a (Cache-)Matrix             ##
## @return:  Inverse matrix                               ##

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
