// Playground - noun: a place where people can play

import UIKit

var str = "Hello, playground"

var primes = [2]
for number in stride(from: 3, through: 100, by: 2) {
    var isPrime = true
    for trial in primes {
        if number % trial == 0 {
            isPrime = false
            break
        }
    }
    if isPrime {
        primes.append(number)
    }
}

primes