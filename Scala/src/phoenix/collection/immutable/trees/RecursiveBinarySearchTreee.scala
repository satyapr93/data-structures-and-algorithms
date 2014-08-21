package phoenix.collection.immutable.trees

/**
 * Created by Satya on 21/08/14.
 */

case class LeafBST[T <% Ordered[T]] (elem: T) extends BinarySearchTree[T] {
	def value = Some(elem)
  def left = EmptyBST
  def right = EmptyBST
  def inorderTraversal = Seq()
}
case object EmptyBST extends BinarySearchTree[Nothing] {
  def value = None
  def left = EmptyBST
  def right = EmptyBST
  def inorderTraversal = Seq()
}

case class RecursiveBinarySearchTree[T <% Ordered[T]] (elem: T, val left: BinarySearchTree[T], val right: BinarySearchTree[T]) extends BinarySearchTree[T] {
  def value: Option[T] = Some(this.elem)
  def inorderTraversal: Seq[T] = (if(left  == EmptyBST) Seq() else left.inorderTraversal) ++ Seq(elem) ++ (if(right == EmptyBST) Seq() else right.inorderTraversal)
}

object RecursiveBinarySearchTree {
  def buildFromSeq[T <% Ordered[T]](list: Seq[T]): BinarySearchTree[T] = list match {
    case Seq() => EmptyBST
    case _ => {
      val (lt, rt) = list.splitAt(list.length/2)
      val elem = rt.head
      val left = buildFromSeq(lt)
      val right = buildFromSeq(rt.tail)
      RecursiveBinarySearchTree(elem, left, right)
    }
  }

  def build[T <% Ordered[T]](node: T, leftTree: BinarySearchTree[T], rightTree: BinarySearchTree[T]) = {
    val l = if(leftTree == EmptyBST) Nil else leftTree.inorderTraversal
    val r = if(rightTree == EmptyBST) Nil else rightTree.inorderTraversal

    def merge(xs: Seq[T], ys: Seq[T]): Seq[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x::xs1, y::ys1) => {
        if(x < y) x +: merge(xs1, ys)
        else y +: merge(xs, ys1)
      }
    }
    val mergedSeq = merge(merge(l, r), Seq(node))
    val (lt, rt) = mergedSeq.splitAt(mergedSeq.length/2)

    val left = buildFromSeq(lt)
    val right = buildFromSeq(rt.tail)

    RecursiveBinarySearchTree(rt.head, left, right)
  }
}