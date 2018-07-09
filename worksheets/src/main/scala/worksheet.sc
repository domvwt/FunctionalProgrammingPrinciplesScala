
val asset = Set(1,2,3)

val basset = Set(3,4,5)

def sum(f: Int => Int, a: Int, b: Int) = {
  def loop(a: Int, acc: Int): Int =
    if(a > b) acc
    else loop(a + 1, f(a) + acc)
  loop(a, 0)
}

sum(x => x*x, 3, 5)

def product(f: Int => Int)(a: Int, b: Int): Int = {
  if(a > b) 1
  else f(a) * product(f)(a + 1, b)
}

product(x => x*x)(3, 4)

def fact(n: Int) = product(x => x)(1, n)

fact(5)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, id: Int)(a: Int, b: Int): Int = {
  if(a > b) id
  else combine(f(a), mapReduce(f, combine, id)(a + 1, b))
}

def product2(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)

product2(x => x * x)(3, 4)

