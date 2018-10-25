import patmat.Huffman._

val output = makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3)))

output match {
  case _ :: Nil => true
  case _ => false
}

val test = List(Leaf('x', 7))

test match {
  case _ :: Nil => true
  case _ => false
}