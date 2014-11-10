// Playground - noun: a place where people can play

import UIKit

let a = 4.0
let b = 12.0
let c = sqrt(a*a + b*b)
println("\(a), \(b), and \(c)")

class RightTriangle {
    let a: Double
    let b: Double
    let c: Double
    
    init(a: Double, b: Double) {
        self.a = a
        self.b = b
        self.c = sqrt(a*a + b*b)
    }
}
let π = 3.14159

func area (r: Double) -> Double {
    return π*r*r
}

area(10.0)

RightTriangle(a: 4.0, b: 12.0)


let array = 0..<12

for item in 0..<12 {
    println("\(item)")
}

let test_membership = [1, 3, 9, 2]
for item in test_membership {
    
}