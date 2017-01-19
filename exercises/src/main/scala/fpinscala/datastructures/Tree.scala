package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {


  def size[A](root: Tree[A]): Int = root match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(root: Tree[Int]): Int = root match {
    case Leaf(a) => a
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](root: Tree[A]): Int = root match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + depth(l) max depth(r)
  }

  def map[A, B](root: Tree[A])(f: A => B): Tree[B] = root match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](root: Tree[A])(f: A => B)(g: (B, B) => B): B = root match {
    case Leaf(a) => f(a)
    case Branch(l: Tree[A], r: Tree[A]) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def foldSize[A](root: Tree[A]): Int = fold(root)(_ => 1)(1 + _ + _)

  def foldMaximum(root: Tree[Int]): Int = fold(root)(a => a)(_ max _)

  def foldDepth(root: Tree[Int]): Int = fold(root)(a => 0)( (l, r) => 1 + (l max r))

  def map[A, B](root: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](root)( a => Leaf(f(a)) ) (Branch(_, _))

}