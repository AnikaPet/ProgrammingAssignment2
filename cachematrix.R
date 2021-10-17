## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# set the value of matrix x   set
# get the value of matrix x   get
# set the value of inverse matrix   setinverse
# get the value of inverse matrix   getinverse
# return value is a list

makeCacheMatrix <- function(x = matrix()) {
      
      inv_x <- NULL
      set <- function(y){
            x <<- y
            inv_x <<- NULL
      }   
      get <- function() x
      
      setinverse <- function(inv) inv_x <<- inv
      getinverse <- function() inv_x
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

# calculates inverse of matrix x 
# first checks if the inverse has already been calculated
# if yes get value from cache and return
# else find inverse and set the value in the cache and return inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      inv_x <- x$getinverse()
      if(!is.null(inv_x)){
            message("getting cached data")
            return(inv_x)
      }
      
      matrix <- x$get()
      inv_x <- solve(matrix,...)
      x$setinverse(inv_x)
      inv_x
}
