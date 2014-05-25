## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeCacheMatrix passes 1 arg: 
#1. Matrix X


makeCacheMatrix <- function(x = matrix()) {
# variable i is intialized to NULL
  i <<- NULL
  
# set function to set x the value of inverted matrix y
  
  set <- function(y){
                   x <<- y
                   i <<- NULL
    
  }

# get function to retrieve the value of inverted matrix 
  get <- function(x){

# value of the inversion is allocated to the variable i    
         setinverse <- function(inverse)
         i <<- inverse
        getinverse <- function()i
    
# The list operation yields out the list of the values in the environment

      list(set = set , get= get , setinverse= setinverse,
           getinverse= getinverse)
    
  }
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
# Variable i is allocated the value returned by getinverse function 
  i <- x$getinverse()
  
# If the value of i is not NULL , message display," getting data".

  if(!is.null(i)){
              message("getting cached data")
# return the value of the inverse residing in i.
              return(i)
  }

# set the value of data as matrix needed to be calculated
             data <- x$get()
# Put the value of calculated mean in i , ... shows the additional parameters
#depending upon Moore-Penrose generalized inverse.

             i <- inverse(data,...)
# Set the value of inverse in the cache
             x$setinverse(i)
# return the value of inverse.
             i
}
