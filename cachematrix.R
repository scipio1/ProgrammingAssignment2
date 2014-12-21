## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){     ##set函数用于将y的值赋予x
                x <<- y
                m <<- NULL
        }
        get <- function() x    ##get函数用于返回x的值
        setsolve <- function(solve) m <<- solve         ##setsolve函数用于将solve的值赋予m
        getsolve <- function() m        ##get函数用于返回m的值
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)       ##返回list
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getsolve()       ##尝试读取缓存的逆矩阵
        if(!is.null(m)){        ##判断缓存的逆矩阵是否为null
                message("getting cached data")
                return(m)       ##如果不为null，直接输出缓存值
        }
        data <- x$get()         ##如果为null，则读取原矩阵
        m <- solve(data)        ##求出逆矩阵
        x$setsolve(m)           ##将值存放到缓存中
        m
}
