
; Problem 1
; Solution recurses by splitting Tree into L m and R parts and searches each for N
; N = Number being searched for
; TREE = Ordered Tree to search
(defun TREE-CONTAINS (N TREE)
    (cond 
        ; TREE is an atom
        ((atom TREE) (= N TREE))
        ; No element
        ((null TREE) nil)
        ; One element
        ((null (cdr TREE)) (= N (car TREE)))
        
        (t (or (TREE-CONTAINS N (first TREE)) (TREE-CONTAINS N (second TREE)) (TREE-CONTAINS N (third TREE))))
    )
)

; Problem 2
; Finds the minimum value in the given tree by branching into the three parts of an ordered tree
; TREE = Tree to find the min value of
(defun TREE-MIN (TREE)
    (cond 
        ; TREE is an atom
        ((atom TREE) TREE)
        ; No element
        ((null TREE) nil)
        ; One element
        ((null (cdr TREE)) TREE)
        
        (t (min (TREE-MIN (second TREE)) (TREE-MIN (first TREE))  (TREE-MIN (third TREE))))
    )
)

; Problem 3
; Returns a list of a tree in pre order notation by branching and combining 
; the middle leftmost, and rightmost subtrees in that order
; TREE = Tree to return pre order list of
(defun TREE-ORDER (TREE)
    (cond 
        ; TREE is an atom
        ((atom TREE) (list TREE))
        ; No element
        ((null TREE) nil)
        ; One element
        ((null (cdr TREE)) TREE)
        
        (t (append (TREE-ORDER (second TREE)) (TREE-ORDER (first TREE)) (TREE-ORDER (third TREE))))
    )
)

; Problem 4
; returns the sub-list by recursing and traversing the list and appending elements until
; the sub list is achieved
; L = List
; START = starting index of list
; LEN = length of desired sub list
(defun SUB-LIST (L START LEN)
    (cond 

        ((= LEN 0) nil)
        ; No element
        ((null L) nil)
        
        ((= START 0) (append (list (car L)) (SUB-LIST (cdr L) 0 (- LEN 1))))

        (t (SUB-LIST (cdr L) (- START 1) LEN))
    )
)

; Problem 5
; splits list into two parts by using sub list on the first and second halves
; L = List to split
(defun SPLIT-LIST (L)
    (let* ((lenl (length L)) (len1 (ceiling (/ lenl 2))) (len2 (- lenl len1)) (start1 0) (start2 (+ start1 len1)))
        
            (list (SUB-LIST L start1 len1) (SUB-LIST L start2 len2))
        
    )
)

; Problem 6
; Returns height of tree by branching and returning the max depth value recursively
; TREE = Tree to find depth of
(defun BTREE-HEIGHT (TREE)
    (cond 

        ((atom TREE) 0)
        ; No element
        ((null TREE) 0)

        (t (max (+ (BTREE-HEIGHT (first TREE)) 1) (+ (BTREE-HEIGHT (second TREE)) 1)))
    )
)

; Problem 7
; Converts a list to a binary tree by using split list to pair up child nodes
; LEAVES = list to turn into leaves of binary tree
(defun LIST2BTREE (LEAVES)
    (cond 
        ((= (length LEAVES) 1) (car LEAVES))
        ((and (= (length LEAVES) 2) (atom (first LEAVES)) (atom (second LEAVES))) LEAVES)
        ; No element
        ((null LEAVES) '())

        (t (list (LIST2BTREE (first (SPLIT-LIST LEAVES))) (LIST2BTREE (second (SPLIT-LIST LEAVES)))))
    )
)

; Problem 8
; Converts a binary tree to a list by the same methods as the solution to problem 7
; TREE = tree to convert to list
(defun BTREE2LIST (TREE)
    (cond 
        ((atom TREE) (list TREE))
        ((null TREE) nil)
        ; No element
        ((null TREE) '())

        (t (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE))))
    )
)

; Problem 9
; Compares two lists by recursing in parallel and comparing
; E1 = first list to recurse
; E2 = Second list to recurse
(defun IS-SAME (E1 E2)
    (cond 

        ; E1 and E2 are different types (atoms and lists)
        ((or (and (atom E1) (not (atom E2))) (and (not (atom E1)) (atom E2))) nil)
        
        ; single element atom
        ((and (atom E1) (atom E2)) (list E1 E2))
        
        ; No element
        ((or (null E1) (null E2)) nil)
        
        ; single element list
        ((and (null (cdr E1)) (null (cdr E2))) (= (car E1) (car E2)))
        
        (t (and (IS-SAME (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2))))
    )
)