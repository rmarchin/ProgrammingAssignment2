## Cache the inverse of a matrix, Retrieve the inverse matrix

## Create a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinv <- function(solve) m <<- solve
     getinv <- function() m
     list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Return a matrix that is the inverse of 'x'
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

## How to execute the makeCacheMatrix and cacheSolve functions
m <- makeCacheMatrix() #create a list of the 4 functions in makeCacheMatrix
m$set(matrix(10:13, 2, 2)) #create a matrix
m$get() #retrieve the matrix
cacheSolve(m) #calculate the inverse of matrix created
cacheSolve(m) #return the inverse matrix from the cache
m$set(matrix(20:23, 2, 2)) #create a second matrix
cacheSolve(m) #calculate the inverse of second matrix 
cacheSolve(m) #return the inverse matrix from the cache