### 1. Function
### creates a set of functions with the intend of storing and changing an inverse of a matrix
### functions are set, get, set_inv, get_inv
### to use these functions create an object (e.g. mym <- makeCacheMatrix(data))
### to call the value of the functions use e.g. mym$get()

makeCacheMatrix <- function(x=numeric){
        inv <- NULL ### create empty object
        set <- function(y){ ### y as new input
                x <<- y ### overwrite x
                inv <<- NULL ### clear cache
        }
        get <- function() x ### could be read as print(x)
        set_inv <- function(inverse) inv <<- inverse ### store inverse of x
        get_inv <- function() inv ### call inverse
        list(set=set, 
             get=get, 
             set_inv=set_inv, 
             get_inv=get_inv) ### define functions
        
}

### 2. Function
### Creates and stores the inverse for 1. functions environment, caching can save time when computing big data sets
### before running 2. F. get_inv is empty (NULL)
### after runing this function mym$get_inv will display the cached inverse of x

cacheSolve <- function(x, ...){
        inv <- x$get_inv() ### calls inverse
        if(!is.null(inv)){ ### if inv exists
                message("Getting cached data") ### dispaly message
                return(inv) ### and print inverse
        }
        dat <- x$get() ### function's else part. Get data from makeCacheMatrix
        inv <- solve(dat,...) ### compute inverse of x
        x$set_inv(inv) ### store inverse in makeCacheMatrix environment
        print(inv) ### print inverse
}
