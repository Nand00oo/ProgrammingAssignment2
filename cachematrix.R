## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

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
           getinverse = getinverse)
}



## Function “cacheSolve” computes the inverse of the special “matrix” (which is the input of cachemean) 
## returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache. If the inverse has not been calculated, 
## data gets the matrix stored with makeCacheMatrix, m calculates the inverse, and x$setmean(m) stores it 
## in the object m in makeCacheMatrix.

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
            
## To test the function:
cacheSolve(makeCacheMatrix(x<-rbind(c(2,4.5),c(1,6))))

##           [,1]       [,2]
##  [1,]  0.8000000 -0.6000000
##  [2,] -0.1333333  0.2666667

## Gives the same result as:
            
## x<-rbind(c(2,4.5),c(1,6))
## solve(x)
##           [,1]       [,2]
## [1,]  0.8000000 -0.6000000
## [2,] -0.1333333  0.2666667           
            
