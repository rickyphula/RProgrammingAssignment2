## This is Peer-graded Assignment: Programming Assignment 2: Lexical Scoping 
## in Coursera R Progamming Course

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL                    ## initialize m in the parent environment so that all funtions in this environment can access it. 
  set <- function(y) {         ## reset value with a new matrix and clear the cached value of m
    x <<- y
    m <<- NULL
  }
  
  get <- function() x          # save the matrix 
  setinverse <- function(inverse) m <<- inverse     ## cache the inverse matrix 
  getinverse <- function() m                        ## save the inverse of the matrix
  list(set = set, get = get,                        ## functions and values are retuned as list
       setinverse = setinverse,
       getinverse = getinverse)
}  

## cacheSolve: This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()                               ## extract the cached matrix, if the value is not NULL, return the value and  
  if(!is.null(m)) {                                 ## "getting cached data"
    message("getting cached data")
    return(m)
  }
  data <- x$get()                                   ## if getinverse() returns NULL, get the new matrix and calculate the inverse 
  m <- solve(data, ...)                             ## then cache the inverse
  x$setinverse(m)
  m
}
