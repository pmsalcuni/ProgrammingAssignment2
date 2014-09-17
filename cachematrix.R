## The functions below work together to solve for the inverse of a square,
## invertible matrix. The makeCacheMatrix function stores the inverse in its
## environment once it has been calculated by the cacheSolve function, so that
## the cacheSolve function can check to see if it is already defined there 
## before calculating it again.

## the makeCacheMatrix function takes a square, invertible matrix as an argument
## x and returns a list of four elements: the functions set, get, setInv, and
## getInv. Upon calling this function, any cached inverted matrix calculated
## from cacheSolve is removed by assigning inv as NULL. The set function takes 
## an argument y and assigns it to x in the function environment. get() returns
## x. setInv() takes an argument and assigns it to the variable inv in the
## parent function environment, while getInv() returns inv.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){ 
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)    
}


## cacheSolve takes a list argument that results from makeCacheMatrix and
## returns the inverse of the matrix stored with the latter function. When inv
## is undefined in the makeCacheMatrix function (i.e. when $setInv() has not run
## yet) cacheSolve defines it by calling $setInv(). Inv will remained cached
## until makeCacheMatrix or $set() or $setInv() is run again.

cacheSolve <- function(x, ...) {
        inv <- x$getInv() # checks to see if inv has been defined in 
                          # makeCacheMatrix yet
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get() # defines cached matrix within cacheSolve environment
        inv <- solve(data) # calculates inverse matrix
        x$setInv(inv) # defines inv in makeCacheMatrix environment
        inv # returns inverse
}
