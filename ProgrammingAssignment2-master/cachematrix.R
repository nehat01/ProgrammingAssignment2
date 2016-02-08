## The functions in this file are used to calculate matrix inversion which is a costly computation.

## makeCacheMatrix function is used to create a special matrix object 
## Which will cache the inverse of matrix for future use.

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set = function(y) {
        ## << symbol is used to assign value to an object in env. which is diff from current environment. 
        x <<- y 
        inv <<- NULL
    }
    get = function() x
    setinv = function(inverse) inv <<- solve
    getinv = function() inv
    list( set=set , get=get , setinv=setinv , getinv=getinv )
    
}


## cacheSolve function is used to calculate the inverse of matrix.
## In case the inverse has already been created and matrix is same, 
## It will retrieve its inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv = x$getinv()
    if (!is.null(inv))
    {
        ## If inverse exists
        message("getting cached data")
        return(inv)
    }
    ## else calculate inverse by solve method
    data = x$get()
    inv = solve(data, ... )
    x$setinv(inv)
    inv
}

## To test these methods , create a matrix and call these functions as below:
## extra steps for testing:
## craete matrix of random numbers and then calling these functions.
set.seed(1110201)
r = rnorm(1000000)
mat1 = matrix(r, nrow=1000, ncol=1000)
test = makeCacheMatrix(mat1)
cacheSolve(test)



