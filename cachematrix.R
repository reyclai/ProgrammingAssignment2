## Together these functions set up the environment to store the inverse of
## a matrix in the cache, and calculate the matrix

## This function sets up the environment with the objects required for cacheSolve.
## m is set as NULL
## x is set as an object defined in the external environment (y) in the set function
## get retrieves x (y from the external environment)
## setinv gets m ready to store the inverse (inv) which is calculated in casheSolve
## getinv retrieves the inverse (m)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function calculates the inverse of the matrix 'x'
## But the inverse is only calculated if it is not already in the cache
## ie the inverse is only calculated if m is NULL
## input to cachSolve must be an object including the makeCacheMatrix environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

##Running functions with a 3 by 3 matrix
set.seed(54)
numbers <- sample(1:100, 9)
mymatrix <- makeCacheMatrix(matrix(numbers, 3, 3))
cacheSolve(mymatrix)

##testing if it worked
solve(matrix(numbers, 3, 3))

## this time output should say "getting cached data" before printing inverse
cacheSolve(mymatrix)