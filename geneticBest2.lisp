;(load "primeiraEntrega.lisp")

(defstruct node
  (state nil)        		; a state in the domain
  (parent nil)              ; the parent node of this node
  (action nil)              ; the domain action leading to state
  (depth 0)                 ; depth of node in tree (root = 0)
  (expanded? nil)           ; any successors examined?
)

(defun accoes-realizadas(node)
  (let ((accoes (list (node-action node)))
       (current (node-parent node)))
  (loop while (node-parent current) do
    (setf accoes (append (list (node-action current)) accoes))
    (setf current (node-parent current)))
  accoes))

(defun procura-best(array pecas p)
	(let ((caminho (list))
		 (node (make-node :state (make-estado :pecas-por-colocar pecas :tabuleiro (array->tabuleiro array))))
		 (resto 0))
	
	(if (oddp (list-length pecas))
		(setf resto 1))

	(loop for i from 0 to (1- (/ (- (list-length pecas) resto) 2)) do
		(setf node (escolhe-melhor-neto (node-state node) #'meta-heuristica p))
		(setf caminho (append caminho (accoes-realizadas node))))

	(if (equalp resto 1)
		(progn (setf node (escolhe-melhor-filho (node-state node) #'meta-heuristica p))
		  	   (setf caminho (append caminho (list (node-action node))))))
	caminho))

(defun escolhe-melhor-neto(estado funcao-fitness p)
	(let ((sucessores (list))
		  (netos (list))
		  (accoes-possiveis (accoes estado))
		  (primeiro-node (make-node :state estado))
		  neto-melhor
		  node-actual
		  neto-novo 
		  accao-actual)

	(loop while accoes-possiveis do
		(setf accao-actual (pop accoes-possiveis))
		(push (make-node :state (resultado estado accao-actual) :parent primeiro-node :action accao-actual :depth 1) sucessores))
	
	(setf node-actual (first sucessores))
	(setf accoes-possiveis (accoes (node-state node-actual)))
	(setf accao-actual (first accoes-possiveis))
	(setf neto-melhor 
		(make-node :state (resultado (node-state node-actual) accao-actual) :parent node-actual :action accao-actual :depth 2))

	(loop while sucessores do
		(setf node-actual (pop sucessores))
		(setf accoes-possiveis (accoes (node-state node-actual)))
		(loop while accoes-possiveis do
			(setf accao-actual (pop accoes-possiveis))
			(setf neto-novo (make-node :state (resultado (node-state node-actual) accao-actual) 
									   :parent node-actual 
									   :action accao-actual 
									   :depth 2))
			(push neto-novo netos)
			(if (< (funcall funcao-fitness neto-novo p) (funcall funcao-fitness neto-melhor p))
				(setf neto-melhor neto-novo))))

	(random-element (remove-if #'(lambda (node) (no-pior-que-best? node neto-melhor funcao-fitness p))
	  							netos))))

(defun escolhe-melhor-filho(estado funcao-fitness p)
	(let ((sucessores (list))
		  (accoes-possiveis (accoes estado))
		  (primeiro-node (make-node :state estado))
		  filho-melhor
		  novo-no
		  accao-actual)

	(setf accao-actual (pop accoes-possiveis))
	(setf filho-melhor (make-node :state (resultado estado accao-actual) :parent primeiro-node :action accao-actual :depth 1))
	(push filho-melhor sucessores)

	(loop while accoes-possiveis do
		(setf accao-actual (pop accoes-possiveis))
		(setf novo-no (make-node :state (resultado estado accao-actual) :parent primeiro-node :action accao-actual :depth 1))
		(push novo-no sucessores)
		(if (< (funcall funcao-fitness novo-no p #'linhas-completas-1nivel) 
			   (funcall funcao-fitness filho-melhor p #'linhas-completas-1nivel))
			(setf filho-melhor novo-no)))

	(random-element (remove-if #'(lambda (node) 
								 (no-pior-que-best? node filho-melhor funcao-fitness p #'linhas-1nivel))
	  							 sucessores))))

(defun no-pior-que-best?(node best funcao-fitness p &optional (h-linhas #'linhas-completas-2niveis))
	(if (> (funcall funcao-fitness node p h-linhas) (funcall funcao-fitness best  p h-linhas))
		t
		nil))

(defun meta-heuristica(node p &optional (h-linhas #'linhas-completas-2niveis))
	(+ (* (aref p 0) (heuristica-somatorio-alturas node)) (* (aref p 1) (funcall h-linhas node))
	   (* (aref p 2) (heuristica-numero-de-buracos node)) (* (aref p 3) (heuristica-monotonia node))))

(defun heuristica-somatorio-alturas(node)
	(let ((tabuleiro (estado-tabuleiro (node-state node)))
		  (height 0))
	(dotimes (i 10)
		(setf height (+ height (tabuleiro-altura-coluna tabuleiro i))))
	height))

(defun heuristica-monotonia(node)
	(let ((tabuleiro (estado-tabuleiro (node-state node)))
		   (bumpiness 0))
	(loop for i from 0 to 8 do
		(setf bumpiness (+ bumpiness (abs (- (tabuleiro-altura-coluna tabuleiro i) (tabuleiro-altura-coluna tabuleiro (1+ i)))))))

	bumpiness))

(defun linhas-completas(node)
	(let ((pontos-node (estado-pontos (node-state node)))
		  (pontos-pai (estado-pontos (node-state (node-parent node))))
		  diferenca
		  (linhas 0))

	(setf diferenca (- pontos-node pontos-pai))
	(case diferenca
		((100) (setf linhas 1))
		((300) (setf linhas 2))
		((500) (setf linhas 3))
		((800) (setf linhas 4)))
	(* linhas -1)))

(defun linhas-completas-2niveis(node)
	(+ (linhas-completas node) (linhas-completas (node-parent node))))

(defun linhas-completas-1nivel(node)
	(linhas-completas node))

(defun heuristica-numero-de-buracos(node)
	(let ((tabuleiro (estado-tabuleiro (node-state node)))
		  (altura-coluna 0) (holes 0))
	(dotimes (j 10)
		(setf altura-coluna (tabuleiro-altura-coluna tabuleiro j))
		(dotimes (i altura-coluna)
			(if (and (null (tabuleiro-preenchido-p tabuleiro i j)) (tabuleiro-preenchido-p tabuleiro (1+ i) j))
				(incf holes))))
	holes))

