## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    # Initialize variable for cached inverse matrix
    invMatrix <- NULL
    
    ## Setter for matrix, resets invMatrix
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    
    ## Getter for matrix
    get <- function() x
    
    ## Setter for inverse matrix
    setInverse <- function(inverse) invMatrix <<- inverse
    
    ## Getter for inverse matrix
    getInverse <- function() invMatrix
    
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special
## matrix returned by makeCacheMatrix function. If the inverse has
## already been calculated and the matrix has not changed, then
## this function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    #get cached inverse matrix
    invMatrix <- x$getInverse()
    
    #return the inverse matrix if found in cache
    if(!is.null(invMatrix)) {
        message("getting cached data")
        return(invMatrix)
    }
    
    #If inverse matrix is not in cache, solve it and save in cache
    data <- x$get()
    invMatrix <- solve(data, ...)
    x$setInverse(invMatrix)
    
    invMatrix
}

#### Test ####

hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
testMatrix <- makeCacheMatrix(hilbert(4))
print(testMatrix$get())
print(testMatrix$getInverse())
print(cacheSolve(testMatrix))
print(cacheSolve(testMatrix))

print("****new matrix****")

testMatrix$set(matrix(c(1,2,3,6,0,4,7,8,9),3,3))
#Matrix has changed, test if cacheSolve returns correct inverse
print(cacheSolve(testMatrix)) 
print(testMatrix$get())
print(testMatrix$getInverse())
print(cacheSolve(testMatrix))
print(cacheSolve(testMatrix))