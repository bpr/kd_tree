# KD Trees

## Introduction

From http://en.wikipedia.org/wiki/K-d_tree :

*The k-d tree is a binary tree in which every node is a k-dimensional
 point. Every non-leaf node can be thought of as implicitly generating
 a splitting hyperplane that divides the space into two parts, known
 as subspaces. Points to the left of this hyperplane represent the
 left sub-tree of that node and points right of the hyperplane are
 represented by the right sub-tree. The hyperplane direction is chosen
 in the following way: every node in the tree is associated with one
 of the k-dimensions, with the hyperplane perpendicular to that
 dimension's axis. So, for example, if for a particular split the "x"
 axis is chosen, all points in the subtree with a smaller "x" value
 than the node will appear in the left subtree and all points with
 larger "x" value will be in the right sub tree. In such a case, the
 hyperplane would be set by the x-value of the point, and its normal
 would be the unit x-axis.[1]*

## Algorithm

The algorithm was inspired by the Wikipedia entry and by Overmars computational 
geometry book.


Implementation: multidim.ml spaces.ml kd_tree.ml 

Usage:

* Compile with omake. 
* Run './repl -input_file usa_cities.csv'
* In the repl, enter lat and long separated by comma, e.g. 

  enter (lat,long) >>> -121.46,38.52       
  {name=Parkway-South Sacramento; population=40797; state=California; latlong=(-121.45,38.51)}

Todo:    
* Fix range search
* Implement algorithm in Scala, Clojure, and Haskell