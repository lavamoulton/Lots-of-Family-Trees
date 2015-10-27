;;;takes in input from the file line by line
;;;looks at the first character which can be "E", "W", "X", "R"
;;;passes the string on to further functions that will handle each case specifically
(defun handle-input (string)
  (if (string= string "E" :start1 0 :end1 1)
      (progn
        (setf *end-of-input* nil)
        (e-input (subseq string 2))))
  (if (string= string "W" :start1 0 :end1 1)
      (progn
        (if (null *end-of-input*)
            (create-lists))
        (w-query string)))
  (if (string= string "X" :start1 0 :end1 1)
      (progn
        (if (null *end-of-input*)
            (create-lists))
        (x-query string)))
  (if (string= string "R" :start1 0 :end1 1)
      (progn
        (if (null *end-of-input*)
            (create-lists))
        (r-query string))))

;;;function to call all the methods that should be generated at the end
;;;of input to the family tree
;;;the boolean variable keeping track of whether this function is necessary
;;;is *end-of-input*
(defun create-lists ()
  (gen-ancestor)
  (gen-relative)
  (gen-cousins)
  (rem-non-cousins)
  (setf *end-of-input* t))

;;;creates the initial hash table
;;;checks to make sure one does not exist so it is not wiped through each run
(defun make-hash ()
      (defparameter *person-hash* (make-hash-table :test 'equal))
      (defparameter *end-of-input* nil))

;;;takes in the filename as string, opens the file and reads it line by line
;;;passes these lines to the handle-input function to decide how to handle the input
(defun file-read (string)
   (let ((in (open string :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
             while line do (handle-input line))
      (close in))))

(defun read-file ()
  (loop for line = (read-line *standard-input* nil :eof)
        until (eq line :eof)
        do
        (handle-input line)))

(defun family ()
  (make-hash)
  (read-file))
  ;(file-read "C:\\Users\\wildcat\\Dropbox\\Programming\\testinput2.txt"))

;;;handles the "E" input
;;;tests whether each person passed in exists in the hashtable
;;;if not, adds the person
(defun e-input (string)
  (let ((word-list (split-by-one-space string)))
    (if (null (gethash (first word-list) *person-hash*))
        (add-person (first word-list)))
    (if (null (gethash (second word-list) *person-hash*))
        (add-person (second word-list)))
    (if (null (gethash (third word-list) *person-hash*))
        (add-person (third word-list) (list (first word-list) (second word-list))))))

;;;creates a new person object and adds it to the global hashtable, *person-hash*
;;;if the query has parents provided, adds the person to their children lists
(defun add-person (name &optional (parents (list name)))
  (if (not (equal (length parents) 1))
      (handle-parents name parents))
  (setf (gethash name *person-hash*)
        (make-person name parents))
  (handle-siblings name parents))

;;;adds the name of the child to the child list of both parents
;;;adds the two parents as spouses of each other
(defun handle-parents (name parents)
  (setf (child-list (gethash (second parents) *person-hash*))
        (append (child-list (gethash (second parents) *person-hash*)) (list name)))
  (setf (child-list (gethash (first parents) *person-hash*))
        (append (child-list (gethash (first parents) *person-hash*)) (list name)))
  (setf (spouse-list (gethash (second parents) *person-hash*))
        (union (spouse-list (gethash (second parents) *person-hash*)) 
                (list (first parents)) :test 'equal))
  (setf (spouse-list (gethash (first parents) *person-hash*)) 
        (union (spouse-list (gethash (first parents) *person-hash*))
                (list (second parents)) :test 'equal)))

;;;adds current children of a recently created person's parents
;;;to their sibling list
(defun handle-siblings (name parents)
  (if (not (equal (length parents) 1))
      (progn
        (setf (sibling-list (gethash name *person-hash*))
              (remove name (get-common-children parents) :test 'equal))
        (add-siblings name parents))))

;;;adds new sibling to already existing children
(defun add-siblings (name parents)
  (loop for child in (get-common-children parents)
        do
            (setf (sibling-list (gethash child *person-hash*))
             (append (sibling-list (gethash child *person-hash*)) (list name)))))

;;;returns a list of the common children between two parents
(defun get-common-children (parents)
  (intersection (child-list (gethash (first parents) *person-hash*))
                (child-list (gethash (second parents) *person-hash*))))

;;;used to test the hash table information
(defun test-hash ()
  (format t "----------------------------------~%")
  (maphash #'(lambda (k v) (format t "~a => ~a~%=>~a~%=>~a~%=>~a~%=>~a~%=>~a~%=>~a~%~%" k 
                                   (parent-list v) (sibling-list v) (child-list v) 
                                   (spouse-list v) (ancestor-list v)
                                   (relative-list v) (cousin-list v))) *person-hash*))

;;;generate all ancestor lists for the people in the family tree
(defun gen-ancestor ()
  (loop for key in (get-hash-keys)
        do
        (setf (ancestor-list (gethash key *person-hash*))
              (rec-ancestor key))
        (if (not (equal (length (ancestor-list (gethash key *person-hash*))) 1))
            (setf (ancestor-list (gethash key *person-hash*))
                  (remove key (ancestor-list (gethash key *person-hash*))
                          :test 'equal)))))

;;;recursive function involved in finding every person's ancestor
(defun rec-ancestor (key)
  (setq final-list (list key))
  (if (equal (length (parent-list (gethash key *person-hash*))) 1)
      final-list
    (progn
      (let ((parents (parent-list (gethash key *person-hash*))))
        (setf final-list (remove-duplicates (append final-list
                                                    parents
                                                    (rec-ancestor (first parents))
                                                    (rec-ancestor (second parents))) 
                                            :test 'equal))))))

;;;returns a list of lists of the ancestors for a given person
(defun rec-ancestor-lists (parents)
  (setq final-list (list parents)) 
  (if (not (equal (length parents) 1))
      (if (and (equal (length (parent-list (gethash (first parents) *person-hash*))) 1)
               (equal (length (parent-list (gethash (second parents) *person-hash*))) 1))
          final-list
        (progn
          (setf final-list (remove-duplicates (append final-list
                                                      (list (parent-list 
                                                             (gethash (first parents)
                                                                      *person-hash*)))
                                                      (list (parent-list
                                                             (gethash (second parents)
                                                                      *person-hash*)))
                                                      (rec-ancestor-lists (parent-list
                                                                           (gethash
                                                                            (first parents)
                                                                            *person-hash*)))
                                                      (rec-ancestor-lists (parent-list
                                                                           (gethash
                                                                            (second parents)
                                                                            *person-hash*))))
                                              :test 'equal))))))

;;;finds the descendant relationships of a given person
(defun rec-descendant (key)
  (setq final-list (list key))
  (if (null (child-list (gethash key *person-hash*)))
      final-list
    (progn
      (let ((children (child-list (gethash key *person-hash*))))
        (loop for child in children
               do
               (setf final-list (remove-duplicates (append final-list
                                                           children
                                                          (rec-descendant child))
                                                  :test 'equal)))
        final-list))))

;;;generates all relative lists for the people in the hash map                 
(defun gen-relative ()
  (loop for key in (get-hash-keys)
        do
        (setf (relative-list (gethash key *person-hash*))
              (remove-duplicates (append (relative-list (gethash key *person-hash*))
                                         (rec-ancestor key)
                                         (rec-descendant key))
                                 :test 'equal))))

;;;removes all cousins who have a 0 0 relationship with the person
(defun rem-non-cousins ()
  (loop for key in (get-hash-keys)
        do
        (loop for cousin in (cousin-list (gethash key *person-hash*))
              do
              (if (and (equal (second cousin) 0) (equal (third cousin) 0))
                  (setf (cousin-list (gethash key *person-hash*))
                        (remove cousin (cousin-list (gethash key *person-hash*))
                                :test 'equal))))))

;;;generates all cousin relationships for the people in the hash map
(defun gen-cousins ()
  (loop for key in (get-hash-keys)
        do
        (loop for second-key in (get-hash-keys)
              do
              (setf (cousin-list (gethash key *person-hash*))
              (remove-duplicates (append (cousin-list (gethash key *person-hash*))
                                         (find-cousin-rel key second-key))
                                 :test 'equal)))))

;;;finds the cousin relationship between two given people in the tree
(defun find-cousin-rel (key second-key)
  (setq cousin-level 0)
  (setq remove-level 0)
  (setq common-ancestor (intersection
                         (rec-ancestor-lists (parent-list (gethash key *person-hash*)))
                         (rec-ancestor-lists (parent-list (gethash second-key 
                                                                   *person-hash*)))
                         :test 'equal))
  (cond ((equal (length common-ancestor) 0)
         (setf (cousin-list (gethash key *person-hash*))
               (append (cousin-list (gethash key *person-hash*))
                       (list (list second-key cousin-level remove-level)))))
        ((equal key second-key)
         (setf (cousin-list (gethash key *person-hash*))
               (append (cousin-list (gethash key *person-hash*))
                       (list (list second-key cousin-level remove-level)))))
        ((member second-key (parent-list (gethash key *person-hash*)) :test 'equal)
         (setf (cousin-list (gethash key *person-hash*))
               (append (cousin-list (gethash key *person-hash*))
                       (list (list second-key cousin-level remove-level)))))
        ((member second-key (child-list (gethash key *person-hash*)) :test 'equal)
         (setf (cousin-list (gethash key *person-hash*))
               (append (cousin-list (gethash key *person-hash*))
                       (list (list second-key cousin-level remove-level)))))
        (t
         (setf cousin-level (min (get-ancestry-degree key (first common-ancestor))
                                 (get-ancestry-degree second-key (first common-ancestor))))
         (setf remove-level (abs (- (get-ancestry-degree key (first common-ancestor))
                                    (get-ancestry-degree second-key 
                                                         (first common-ancestor)))))
         (if (> remove-level 4000)
             (setf (cousin-list (gethash key *person-hash*))
                   (append (cousin-list (gethash key *person-hash*))
                           (list (list second-key 0 0))))
           (setf (cousin-list (gethash key *person-hash*))
                 (append (cousin-list (gethash key *person-hash*))
                         (list (list second-key cousin-level remove-level))))))))

;;;gets the degree of ancestry between a person and a given set of ancestors
(defun get-ancestry-degree (key ancestors)
  (let ((key-ancestors (rec-ancestor-lists (parent-list (gethash key *person-hash*))))
        (count 1))
    (if (> (length key-ancestors) 1)
        (progn (setf key-ancestors (subseq key-ancestors 1))
          (loop do
                (setq 1-parents (car key-ancestors))
                (setq key-ancestors (cdr key-ancestors))
                (setq 2-parents (car key-ancestors))
                (setq key-ancestors (cdr key-ancestors))
                (if (or (equal ancestors 1-parents) (equal ancestors 2-parents))
                    count
                  (setf count (+ count 1)))
                while (not (null key-ancestors)))
          count)
      5000)))

;;;returns a list of all the keys in the hash table of people
(defun get-hash-keys ()
  (loop for key being the hash-keys of *person-hash* collect key))

;;;handles the "W" query
;;;lists everyone who is the relation provided
(defun w-query (string)
  (format t "~a~%" string)
  (cond
   ((string= string "spouse" :start1 2 :end1 8)
    (w-list (spouse-list (gethash (subseq string 9) *person-hash*))))
   ((string= string "parent" :start1 2 :end1 8)
    (w-list (parent-list (gethash (subseq string 9) *person-hash*))))
   ((string= string "sibling" :start1 2 :end1 9)
    (w-list (sibling-list (gethash (subseq string 10) *person-hash*))))
   ((string= string "ancestor" :start1 2 :end1 10)
    (w-list (ancestor-list (gethash (subseq string 11) *person-hash*))))
   ((string= string "relative" :start1 2 :end1 10)
    (w-list (relative-list (gethash (subseq string 11) *person-hash*))))
   ((string= string "cousin" :start1 3 :end1 9)
    (setf cousin-level (subseq string 10 11))
    (setf remove-level (subseq string 12 13))
    (list-cousins (subseq string 15) cousin-level remove-level))))

;;;prints out a list of the cousins of the given person alphabetically
(defun list-cousins (key cousin-level remove-level)
  (setf final-list nil)
  (loop for cousin in (cousin-list (gethash key *person-hash*))
        do
        (if (and (equal (second cousin) (parse-integer cousin-level))
                 (equal (third cousin) (parse-integer remove-level)))
            (setf final-list (append final-list (list cousin)))))
  (w-list final-list))

;;;part of the "W" query
;;;prints the list of people in alphabetical form to standard out
(defun w-list (list)
  (format t "~{~a~%~}~%" (sort list #'string-lessp)))

;;;handles the "X" query
;;;decides if the two people provided are of the relation given
(defun x-query (string)
  (format t "~a~%" string)
  (setq x-list (split-by-one-space (subseq string 2)))
  (cond
   ((string= (second x-list) "spouse")
    (if (member (first x-list) (spouse-list (gethash (third x-list) *person-hash*))
                :test 'equal)
        (format t "Yes~%~%")
      (format t "No~%~%")))
   ((string= (second x-list) "parent")
    (if (member (first x-list) (parent-list (gethash (third x-list) *person-hash*))
                :test 'equal)
        (format t "Yes~%~%")
      (format t "No~%~%")))
   ((string= (second x-list) "sibling")
    (if (member (first x-list) (sibling-list (gethash (third x-list) *person-hash*))
                :test 'equal)
        (format t "Yes~%~%")
      (format t "No~%~%")))
   ((string= (second x-list) "ancestor")
    (if (member (first x-list) (ancestor-list (gethash (third x-list) *person-hash*))
                :test 'equal)
        (format t "Yes~%~%")
      (format t "No~%~%")))
   ((string= (second x-list) "relative")
    (if (member (first x-list) (ancestor-list (gethash (third x-list) *person-hash*))
                :test 'equal)
        (format t "Yes~%~%")
      (format t "No~%~%")))
   ((string= (subseq (second x-list) 1) "cousin")
    (setq cousin-level (third x-list))
    (setq remove-level (subseq (fourth x-list) 0 1))
    (if (member (list (first x-list) (parse-integer cousin-level)
                      (parse-integer remove-level))
                (cousin-list (gethash (fifth x-list) *person-hash*)) :test 'equal)
        (format t "Yes~%~%")
      (format t "No~%~%")))))

;;;handles the "R" query
;;;lists the closest relation that two people provided share
(defun r-query (string)
  (format t "~a~%" string)
  (setq r-list (split-by-one-space (subseq string 2)))
  (cond
   ((member (first r-list) (spouse-list (gethash (second r-list) *person-hash*))
            :test 'equal)
    (format t "spouse~%~%"))
   ((member (first r-list) (parent-list (gethash (second r-list) *person-hash*))
            :test 'equal)
    (format t "parent~%~%"))
   ((member (first r-list) (sibling-list (gethash (second r-list) *person-hash*))
            :test 'equal)
    (format t "sibling~%~%"))
   ((member (first r-list) (ancestor-list (gethash (second r-list) *person-hash*))
            :test 'equal)
    (format t "ancestor~%~%"))
   ((member (first r-list) (relative-list (gethash (second r-list) *person-hash*))
            :test 'equal)
    (format t "relative~%~%"))
   (t (format t "unrelated~%~%"))))
  ;(test-hash))

;;;splits up a string into a list of substrings
;;;splits whenever there is a space, used to break a string into separate words
(defun split-by-one-space (string)
  (loop for i = 0 then (1+ j)
        as j = (position #\Space string :start i)
        collect (subseq string i j)
        while j))

;;;class representing a person object
;;;includes a way to set and access the following:
;;;person-name, parent-list, child-list, spouse-list, sibling-list, child-list
;;;spouse-list, ancestor-list, relative-list, cousin-list
(defclass person ()
  ((person-name
    :initarg :person-name
    :accessor person-name)
   (parent-list
    :initarg :parent-list
    :accessor parent-list)
   (child-list
    :initarg :child-list
    :accessor child-list)
   (spouse-list
    :initarg :spouse-list
    :accessor spouse-list)
   (sibling-list
    :initarg :sibling-list
    :accessor sibling-list)
   (ancestor-list
    :initarg :ancestor-list
    :accessor ancestor-list)
   (relative-list
    :initarg :relative-list
    :accessor relative-list)
   (cousin-list
    :initarg :cousin-list
    :accessor cousin-list)))

;;;function to instantiate a new person object
;;;takes in the name and the parent list as arguments
(defun make-person (name parent-list)
  (make-instance 'person :person-name name :parent-list parent-list 
                 :sibling-list (list name) :child-list nil :spouse-list nil
                 :ancestor-list nil :relative-list nil :cousin-list nil))

