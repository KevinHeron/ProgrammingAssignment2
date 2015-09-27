## Put comments here that give an overall description of what your
## functions do: 
## This functions limit the impact of the potentially resource-consuming
## task of matrix inversion, by cacheing the inverse of matrix. An example
## is provided after the functions that shows the advantages of these functions.
## It is to be noted that these functions assume a matrix that is able to be
## nonsingular.


## Write a short comment describing this function:
## 1) set the matrix;
## 2) get the matrix;
## 3) set the value of the inverse; and,
## 4) get the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y 
        i <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inv) i <<- inv 
    getinv <- function() i
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## This function will check to see if we have cached the matrix inverse
## using makeCacheMatrix(). If we have, it gets the cached version; if not,
## it does the inversion, sets it using `setinv`, and returns it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
        
        ## Was the matrix inverted?
        if(!is.null(inv)){
            ## If yes, get the cache
            message("getting cached data")
            return(inv)
        }
        ## else, do the inversion
        mat <- x$get()         ## get
        inv <- solve(mat, ...) ## solve/invert it 
        x$setinv(inv)          ## set it
        inv                    
}

## Example. (It is to be noted that the true advantages would be more evident
## using a larger matrix and clocking the system time or memory used, etc. I 
## thought this was decent example, nevertheless.)
set.seed(123)               ## this and next line create a `test matrix`
mat  <- replicate(5, rnorm(5))
mat2 <- replicate(6, rnorm(6)) 

mm <- makeCacheMatrix(mat)  ## mm is the `housing`
mm$get()                    ## gets the matrix
mm$setinv(solve(mat))       ## sets/caches the inverse
mm$getinv()                 ## here's the inverse, as it was, when it was set.

identical(mm$getinv(), solve(mat))

mm2 <- makeCacheMatrix(mat2)

cacheSolve(mm)  ## This should return a "getting cached" message
cacheSolve(mm2) ## This should *not* return any message

## we `get` from the same place
identical(tracemem(cacheSolve(mm)), tracemem(cacheSolve(mm))) 
## because it is not cached...
identical(tracemem(solve(mat)), tracemem(solve(mat)))
