## These functions, if I understand correctly, reduce the number of calculations that are needed, by putting results in caches. That means that since results are now stored in memory, we don't need to carry out the calculation again (in this case, calculating the inverse of a matrix), which speeds-up the performance of the program. 

# Both functions bellow are basically a repeat of Prof. Peng's functions that he provided in the assignment. This first function (makeCacheMatrix) creates a vector of functions. None of the functions in this vector calculates the inverse of the matrix, but what it does is save the data in the computer's cache using the 'list' function. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)        
}



## The second function (cacheSolve) does two things: it checks if there  is alreadyan inversed matrix (by checking if $getinverse is empty or not). If it is not empty, it prints its contents. If it is empty, it reads the matrix and calculates its inverse, and saves it in $


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}





