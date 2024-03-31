## I ADMIT UPFRONT THAT MY EXPLANATION IS TAKEN FROM THE PROGRAMMING ASSIGNMENT 2
## WITH THE NECESSARY MODIFICATIONS AND A SMALL ADDITION I THINK MAKES IT EASIER
## TO UNDERSTAND. I FOUND THE DISCUSSION "Demystifying makeVector()" TO BE
## HELPFUL IN BREAKING DOWN WHAT IS ACTUALLY HAPPENING.

#############################

## In this example we introduce the `<<-` operator which can be used to
## assign a value to an object in an environment that is different from the
## current environment. Below are two functions that are used to create a
## special object that stores a matrix and caches its inverse.

## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse


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

## If output from 'makeCacheMatrix' is saved as an object, it contains not only
## the four functions above, but also the objects x (since it is passed into
## the function) and m (since it is created within the function).

## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets the value of the inverse in the cache via the
## `setinverse` function.

cacheSolve <- function(x, ...) {
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