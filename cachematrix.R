# Both the functions below together serve to cache a potentially time consuming
# computation, i.e. calculating the inverse of a matrix. 


# The function`makeCacheMatrix` serves to return an object of type makeCacheMatrix()
# (which is in fact a list)
# The set and setInv function serves to set the value of objects in the parent environment:
# "x" & "inv"
# While the get and getInv functions serves to get the values of the object: x & inv
makeCacheMatrix <- function(x){
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(i) inv <<- i
        getInv <- function(i) inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}

cacheSolve <- function(x, ...){ #where x is an object of type makeCacheMatrix()
        inverse <- x$getInv()# Due to lexical scoping, it will retrieve `inv` from the environment where this function is defined.
        if(!is.null(inverse)){ #A condition to check if the inverse has already
                message("getting cached inverse of matrix") # been computed
                return(inverse)
        }
        d <- x$get() #else we get the matrix
        inverse <- solve(d, ...)# calculate the inverse while passing `...`
        x$setInv(inverse) # and cache the inverse for future use
        return(inverse)
}