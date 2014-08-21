package phoenix.collection.immutable.trees

import phoenix.collection.immutable.graphs.Graph

/**
 * Created by Satya Prakash on 16/08/14.
 */

abstract class BinarySearchTree[+T <% Ordered[T]] extends BinaryTree[T] {
  def left: BinarySearchTree[T]
  def right: BinarySearchTree[T]

  private final def min[T <% Ordered[T]](tree: BinarySearchTree[T]): T = tree.left match {
    case EmptyBST => tree.value.get
    case l => min(l)
  }

  private final def max[T <% Ordered[T]](tree: BinarySearchTree[T]): T = tree.right match {
    case EmptyBST => tree.value.get
    case r => max(r)
  }

  private final def isValid[T <% Ordered[T]] (e: Option[T], l: BinarySearchTree[T], r: BinarySearchTree[T]) = e match {
    case None => true
    case Some(elem) =>
      val lm = l match {
        case EmptyBST => elem
        case _ => max(l)
      }
      val rm = r match {
        case EmptyBST => elem
        case _ => min(r)
      }
      if (elem >= lm && elem <= rm) true else false
  }
  require(isValid(value, left, right), "Arguements are not valid for BinarySearchTree")
}