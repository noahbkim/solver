; Logic puzzle solver
; Noah Kim



; http://stackoverflow.com/questions/15393797/lisp-splitting-input-into-separate-strings
; Lisp string parsing is garbage so using code from online will make up for it
(defun split (string &key (delimiterp #'spacep))
  "Split a string by characters that validate the delimiter function."
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))

(defun spacep (c)
  "Delimit only by spaces for input."
  (char= c #\Space))

(defun periodp (c)
  "Delimit by period for field values."
  (char= c #\.))

(defvar whitespace '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout))


; List manipulation
(defun flatten (list)
  "Flatten a list of lists."
  (apply #'append list))

(defun deep-copy (list)
  "Clone elements of a list without references."
  (loop for e in list collect (if (listp e) (deep-copy e) e)))



; Graph components
(defstruct edge a b)
(defstruct graph edges)

(defun make-node (string)
  "Make a new node based from a string."
  (split string :delimiterp #'periodp))

(defun other-node (edge node)
  "Get the adjacent node in an edge."
  (if (equalp node (get 'a edge)) (get 'b edge) (get 'a edge)))

(defun make-edge (a b)
  "Make a new edge with two nodes."
  (let ((edge (gensym "EDGE")))
    (setf (get 'a edge) a (get 'b edge) b)
    edge))

(defun contains-node (edge node)
  "Check if the edge contains the node."
  (or (equalp node (get 'a edge)) (equalp node (get 'b edge))))

(defun equals-edge (a b) 
  "Check if two edges are equivalent."
  (or (and (equalp (get 'a a) (get 'a b)) (equalp (get 'b a) (get 'b b)))
      (and (equalp (get 'a a) (get 'b b)) (equalp (get 'b a) (get 'a b)))))

(defun make-graph (&rest edges)
  "Make a graph of a list of edges."
  (let ((graph (gensym "GRAPH")))
    (setf (get 'edges graph) edges)
    graph))

(defun contains-edge (graph edge)
  "Check if a graph contains an edge."
  (find edge (get 'edges graph) :test #'equals-edge))

(defun add-edge (graph edge)
  "Add an edge to the graph if not already present."
  (if (not (contains-edge graph edge))
      (setf (get 'edges graph) (cons edge (get 'edges graph)))))

(defun get-edges (graph node)
  "Get the edges connected to a node in a graph."
  (loop for edge in (get 'edges graph) if (contains-node edge node) collect edge))

(defun get-adjacent (graph node)
  "Get adjacent nodes to a node in a graph."
  (loop for edge in (get-edges graph node) collect (other-node edge node)))

(defun remove-edge (graph edge)
  "Remove an edge from a graph if present."
  (setf (get 'edges graph) (remove edge (get 'edges graph) :test #'equals-edge))
  graph)

(defun connected-graph (graph node)
  "Get the connected graph starting at the node."
  (let ((connected (make-graph)) (frontier (get-edges graph node)))
    (loop while frontier do
      (let ((current (car frontier)))
        (setf node (other-node current node))
        (add-edge connected current)
        (setf frontier
              (append (cdr frontier) 
                      (loop for edge in (get-edges graph node) 
                        if (and (not (contains-edge connected edge)) 
                                (not (find edge frontier :test #'equals-edge)))
                        collect edge)))))
    connected))

(defun get-nodes (graph)
  "List all the nodes in a graph."
  (let ((nodes nil))
    (loop for edge in (get 'edges graph) do
      (let ((a (get 'a edge)) (b (get 'b edge)))
        (if (not (find a nodes :test #'equalp)) (setf nodes (cons a nodes)))
        (if (not (find b nodes :test #'equalp)) (setf nodes (cons b nodes)))))
    nodes))

(defun remove-graph (graph subgraph)
  "Remove a subgraph from a graph."
  (loop for edge in (get 'edges subgraph) do
    (remove-edge graph edge))
  graph)

(defun without-graph (graph subgraph)
  "Return a copy of the graph without the subgraph."
  (remove-graph (apply #'make-graph (get 'edges graph)) subgraph))

(defun all-connected-graphs (graph)
  "Get a list of disjoint subgraphs in the graph."
  (let ((graphs nil))
    (loop while (get 'edges graph) do
      (setf subgraph (connected-graph graph (get 'a (nth 0 (get 'edges graph)))))
      (setf graph (without-graph graph subgraph))
      (setf graphs (cons subgraph graphs)))
    graphs))

(defun print-graph (graph)
  "Print the contents of a graph."
  (loop for edge in (get 'edges graph) do
    (format t "~a ~a~%" (get 'a edge) (get 'b edge))))

(defun graph-size (graph)
  (length (get 'edges graph)))



; Load a logic field and parameter file
(defun parse (path) 
  (let ((fields nil) (nodes nil) (constraints nil))
    (with-open-file (stream path)
      (let ((mode 0))
        (loop for line = (read-line stream nil) while line do
          (setf line (string-trim whitespace line))
          (cond ((or (eq (length line) 0) (position #\# line)) nil)
                ((equalp line "[fields]") (setf mode 1))
                ((equalp line "[constraints]") (setf mode 2))
                ((eq mode 1)
                 (let* ((field nil)
                        (parts (split line)) 
                        (raw-name (nth 0 parts))
                        (name (subseq raw-name 0 (1- (length raw-name)))))
                   (loop for part in (cdr parts) do
                     (setf node (concatenate 'string name "." part))
                     (if (find node nodes :test #'equal)
                         (error (message "Duplicate node name")))
                     (setf field (cons (make-node node) field)))
                   (setf fields (cons field fields))))
                ((eq mode 2) 
                 (let* ((parts (split line))
                        (shuffled (list (cadr parts) (car parts) (caddr parts))))
                   (setf constraints (cons shuffled constraints))))))))
    (list fields constraints)))


; Graph manipulation
(defun field-graph (fields)
  "Generate a complete graph connecting all fields."
  (let ((graph (make-graph)))
    (loop for field in fields do
      (let ((rest (flatten (remove field fields :test #'equalp))))
        (loop for node in field do
          (loop for other in rest do
            (add-edge graph (make-edge node other))))))
    graph))
	
(defun rest-of-field (fields node)
  "Get the rest of nodes from the corresponding field."
  (loop for field in fields do
    (if (find node field :test #'equalp)
        (return (remove node field :test #'equalp)))))



; Main
(defun main (path)
  "Solve a puzzle specified in the file at path."
  
  ; Parse and set up graph
  (let ((result (parse path)))
    (setf fields (car result))
    (setf constraints (cadr result)))
  (setf search (field-graph fields))  ; Graph of nodes without disconnects or connects
  (setf connected (make-graph))  ; Graph of connected nodes
  (setf disconnected (make-graph))  ; Graph of nodes without disconnects
  
  ; Apply initial constraints
  (loop for constraint in constraints do
    (let ((a (make-node (cadr constraint))) (b (make-node (caddr constraint))))
      (cond ((equalp (car constraint) "&")
             (add-edge connected (make-edge a b))
             (remove-edge search (make-edge a b))
             (loop for not-a in (rest-of-field fields a) do
               (format t "Removing edge: ~a ~a~%" b not-a)
               (remove-edge search (make-edge not-a b))
               (add-edge disconnected (make-edge not-a b)))
             (loop for not-b in (rest-of-field fields b) do
               (format t "Removing edge: ~a ~a~%" a not-b)
               (remove-edge search (make-edge a not-b))
               (add-edge disconnected (make-edge a not-b))))
            ((equalp (car constraint) "|")
             (remove-edge search (make-edge a b))
             (add-edge disconnected (make-edge a b))))))
  
  (setf last-size (graph-size search))
  
  ; Hope for the best
  (loop while (> last-size 0) do

    (format t "Disconnected count: ~d~%" (graph-size disconnected))

    ; Iterate apply more constraints
    (loop for node in (get-nodes connected) do
      (loop for other in (get-adjacent connected node) do
        (loop for not-node in (get-adjacent disconnected node) do
          (remove-edge search (make-edge other not-node))
          (add-edge disconnected (make-edge other not-node)))))
    
    (format t "New disconnected count: ~d~%" (graph-size disconnected))
    (format t "Connected count: ~d~%" (graph-size connected))

    ; Add singly connected nodes of two fields
    (loop for node in (get-nodes search) do
      (let ((node-fields (deep-copy fields)))
        (loop for field in node-fields do
          (if (or (find node field :test #'equalp)
                  (loop for other in (get-adjacent connected node)
                    when (find other field :test #'equalp) 
                    collect other))
              (setf node-fields (remove field node-fields :test #'equalp))))
        (loop for other in (get-adjacent disconnected node) do
          (loop for i from 0 to (1- (length node-fields)) do
            (let ((field (nth i node-fields)))
              (if (find other field :test #'equalp) 
                  (setf (nth i node-fields) (remove other field :test #'equalp))))))
        (loop for field in node-fields do
          (let* ((a node) (b (car field)) (edge (make-edge a b)))
            (cond ((eq (length field) 1) 
                   (format t "Adding edge by disconnects ~a ~a~%" a b)
                   (add-edge connected edge)
                   (remove-edge search edge)
                   (loop for not-a in (rest-of-field fields a) do
                     (format t "Removing edge: ~a ~a~%" b not-a)
                     (remove-edge search (make-edge b not-a))
                     (add-edge disconnected (make-edge b not-a)))
                   (loop for not-b in (rest-of-field fields b) do
                     (format t "Removing edge: ~a ~a~%" a not-b)
                     (remove-edge search (make-edge a not-b))
                     (add-edge disconnected (make-edge a not-b)))))))))
    
    (format t "New connected count: ~d~%" (graph-size connected))

    ; Complete connected node graphs
    (loop for subgraph in (all-connected-graphs connected) do
      (let ((nodes (get-nodes subgraph)))
        (loop for i from 0 to (1- (length nodes)) do
          (loop for j from (1+ i) to (1- (length nodes)) do
            (let ((a (nth i nodes)) (b (nth j nodes)))
              (cond ((not (contains-edge connected (make-edge a b)))
                     (add-edge connected (make-edge a b))
                     (format t "Adding edge by extension: ~a ~a~%" a b)
                     (remove-edge search (make-edge a b))
                     (loop for not-a in (rest-of-field fields a) do
                       (format t "Removing edge: ~a ~a~%" b not-a)
                       (remove-edge search (make-edge b not-a))
                       (add-edge disconnected (make-edge b not-a)))
                     (loop for not-b in (rest-of-field fields b) do
                       (format t "Removing edge: ~a ~a~%" a not-b)
                       (remove-edge search (make-edge a not-b))
                       (add-edge disconnected (make-edge a not-b))))))))))


    (format t "Newer connected count: ~d~%" (graph-size connected))
    (format t "Newer disconnected count: ~d~%" (graph-size disconnected))
    (format t "---~%")
    
    ; Check change in size
    (cond ((eq last-size (setf last-size (graph-size search)))
           (format t "Failed to converge.")
           (return))))

  (loop for z in (all-connected-graphs connected) do
    (format t "~a~%" (get-nodes z)))

  (list search connected disconnected))



