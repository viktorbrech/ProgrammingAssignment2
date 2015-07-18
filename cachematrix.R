## Put comments here that give an overall description of what your
## functions do

## By assigning this function to a variable, it creates a matrix
## that can contain a cached version of its own inverse. This is achieved via a
## list of functions to set/get the matrix/inverse. The matrix data passed via
## the original function call is retained in the environment of this closure,
## and set_matrix and set_inverse manipulate data in that environment via the
## "<<-" operator.

makeCacheMatrix <- function(x = matrix()) {
    i = NULL #initialize the cached inverse variable
    set_matrix = function(y) {
        x <<- y
        m <<- NULL
        ## the "<<-" operator ensures that x and m in the "makeCacheMatrix"
        ## environment are updated.
    }
    get_matrix = function() x
    set_inverse = function(inverse) i <<- inverse # this caches the inverse in i
    get_inverse = function() i
    return(list(setmatrix = set_matrix, getmatrix = get_matrix,
                setinverse = set_inverse, getinverse = get_inverse))
    #the return value is a list of the four functions defined here
}

## This function calls the "get_inverse" function of the matrix object to
## access a cached version of the inverse. If the matrix object does not contain
## a cached inverse, this function instead calculates (and caches) the inverse.

cacheSolve <- function(x) {
    i = x$getinverse()
    if (!is.null(i)) return(i) #if a cached version i exists, it is returned
    matrix = x$getmatrix() #executed if no cached version exists in i
    i = solve(matrix) #calculates the inverse and stores it in i
    x$setinverse(i) #create a cached version i of the inverse in the object
    return(i) #return the inverse that was just calculated
}
