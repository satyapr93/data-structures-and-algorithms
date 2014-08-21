package phoenix.search

/**
 * Created by Satya Prakash on 16/08/14.
 */

import scala.collection.mutable.Stack

trait Graph[T] {
	def neighbours(node: T) : Seq[T]
}

object BreadthFirstSearch {
	def contains[T](graph: Graph[T], root: T, node: T): Boolean = {
		val queue = Stack(root)

		def recursiveSearch(): Boolean = {		
			val top = queue.pop()
			if (top.equals(node)) return true
			graph.neighbours(top).foreach(queue.push(_))
			queue.isEmpty match {
				case true => false
				case false => recursiveSearch()
			}
		}
		recursiveSearch()
	}
}