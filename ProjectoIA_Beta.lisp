;---------------------------------------------------Grupo 41 Taguspark----------------------------------------------------
;78047 Ricardo Rei
;77940 Luis Nunes 	
;77919 Luis Henriques

;-----------------------------------------------------tipo ACCAO----------------------------------------------------------
;@brief:
;	Cria uma accao (par inteiro e array)
(defun cria-accao(int array)
	(cons int array))

;@brief:
;	Retorna o primeiro elemento desse par ou seja a primeira coluna a ser ocupada 
;	pela peca a contar da esquerda
(defun accao-coluna(accao)
	(car accao))

;@brief:
;	Retorna o segundo elemento do par ou seja a configuracao da peca
(defun accao-peca(accao)
	(cdr accao))

;----------------------------------------------------tipo TABULEIRO------------------------------------------------------
;@brief:
;	Cria um tabuleiro vazio (array 18x10)
(defun cria-tabuleiro()
	(make-array '(18 10)))

;@brief:
;	Cria um tabuleiro novo e com referencia diferente que e igual ao recebido em valor 
(defun copia-tabuleiro(tabuleiro)
	(let ((newTabuleiro (make-array '(18 10))))
		(dotimes (i 18)
			(dotimes (j 10)
				(setf (aref newTabuleiro i j) (aref tabuleiro i j))))
		newTabuleiro))

;@brief:
;	Verifica se a linha i coluna j do tabuleiro esta preenchida
(defun tabuleiro-preenchido-p(tabuleiro i j)
	(if (aref tabuleiro (- 17 i) j)
		t
		nil))

;@brief:
;	Devolve a altura da coluna j ou 0 caso ela esteja toda a nil
(defun tabuleiro-altura-coluna(tabuleiro j)
	(dotimes (i 18)
		(if (tabuleiro-preenchido-p tabuleiro (- 17 i) j)
			(return-from tabuleiro-altura-coluna (- 18 i))))
	0)

;@brief:
;	Devolve true se a linha i estiver toda completa e nil caso nao esteja
(defun tabuleiro-linha-completa-p(tabuleiro i)
	(dotimes (j 10)
		(if (null (tabuleiro-preenchido-p tabuleiro i j))
			(return-from tabuleiro-linha-completa-p nil)))
	t)

;@brief:
;	Preenche o tabuleiro na linha i coluna j
(defun tabuleiro-preenche!(tabuleiro i j)
	(if (or (< 17 i) (< 9 j) (< i 0) (< j 0))
		(return-from tabuleiro-preenche! nil))
	(setf (aref tabuleiro (- 17 i) j) t))

;@brief:
;	Altera o tabuleiro removendo a linha que lhe e passada por argumento, e fazendo todas as linhas superiores descer.
;	As linhas que estao por baixo da linha respectiva nao sao alteradas. 
(defun tabuleiro-remove-linha!(tabuleiro linha)	
	(loop for i from linha to 17 do
		(if (equal i 17) 
			;then
			(dotimes (j 10)
				(setf (aref tabuleiro 0 j) nil))
			;else  
			(dotimes (j 10)
				(setf (aref tabuleiro (- 17 i) j) (aref tabuleiro (- 16 i) j))))) 
	nil)

;@brief:
;	Verifica se o topo do tabuleiro esta preenchido retorna true caso esteja e nil caso nao esteja
(defun tabuleiro-topo-preenchido-p(tabuleiro)
	(dotimes (j 10)
		(if (tabuleiro-preenchido-p tabuleiro 17 j)
			(return-from tabuleiro-topo-preenchido-p t)))
	nil)

;@brief:
;	Vai verificar se dois tabuleiros sao iguais ou nao, atraves da condicao: ((not A) /\ B) V (A /\ (not B))
(defun tabuleiros-iguais-p(tabuleiro1 tabuleiro2)
	(dotimes (i 18)
		(dotimes (j 10)
			(if (or (and (tabuleiro-preenchido-p tabuleiro1 i j) (not (tabuleiro-preenchido-p tabuleiro2 i j))) 
					(and (not (tabuleiro-preenchido-p tabuleiro1 i j)) (tabuleiro-preenchido-p tabuleiro2 i j)))
						(return-from tabuleiros-iguais-p nil))))
	t)

;@brief:
;	Devolve um array que e representa um tabuleiro, ou seja inverte o tabuleiro.
(defun tabuleiro->array(tabuleiro)
	(let ((array (cria-tabuleiro)))
		(dotimes (i 18)
			(dotimes (j 10)
				(if (tabuleiro-preenchido-p tabuleiro i j)
					(setf (aref array i j) t))))
		array))

;@brief:
;	Recebe um array e devolve um tabuleiro, que e o array invertido. 
(defun array->tabuleiro(array)
	(let ((tabuleiro (cria-tabuleiro)))
		(dotimes (i 18)
			(dotimes (j 10)
				(if (aref array i j)
					(tabuleiro-preenche! tabuleiro i j))))
		tabuleiro))

;------------------------------------------------NOTA--------------------------------------------------------------------------
;estes auxiliares sao uteis para a funcao resultado (linha 295), estao nesta seccao devido a sua funcao ser alterar o tabuleiro
;------------------------------------------------------------------------------------------------------------------------------

;@@@@ AUXILIAR @brief: 
;		Recebe um tabuleiro e uma accao e altera o tabuleiro aplicando a accao.. 
(defun actualiza-tabuleiro(tabuleiro accao)
	(let ((indice-coluna-mais-alta (accao-coluna accao))
		  (altura-coluna-mais-alta (tabuleiro-altura-coluna tabuleiro (accao-coluna accao)))
		  (dimensao-lateral-peca (second (array-dimensions (accao-peca accao))))
		  (pivot (pivot tabuleiro accao)) ;pivot e o indice do quadrado da base que vai ficar fazer o apoio
		  (nils 0)) ;caso nao seja um quadrado da base temos de contar o numero de nils entre a base e esse quadrado 

	(loop for i from (accao-coluna accao) to (+ (accao-coluna accao) (1- dimensao-lateral-peca)) do
		(if (<= (tabuleiro-altura-coluna tabuleiro indice-coluna-mais-alta) (tabuleiro-altura-coluna tabuleiro i))
				   (setf indice-coluna-mais-alta i)))
	(setf altura-coluna-mais-alta (tabuleiro-altura-coluna tabuleiro indice-coluna-mais-alta))
	;verifica se se trata de um encaixe, ou seja, se o pivot da peca nao e igual a altura da coluna mais alta
	(if (null (= (tabuleiro-altura-coluna tabuleiro (+ (accao-coluna accao) pivot)) altura-coluna-mais-alta))
	(progn 
		(setf nils (conta-nils-da-coluna accao (- indice-coluna-mais-alta (accao-coluna accao))))

		;estou a verificar se a diferenca entre a coluna mais alta e a coluna onde o pivot vai apoiar e menor que os nils
		(if (< (- altura-coluna-mais-alta (tabuleiro-altura-coluna tabuleiro (+ (accao-coluna accao) pivot))) nils) 
			(setf altura-coluna-mais-alta ;se for menor temos de subtrair essa diferenca a altura
			(- altura-coluna-mais-alta (- altura-coluna-mais-alta (tabuleiro-altura-coluna tabuleiro (+ (accao-coluna accao) pivot)))))
			;else subtraimos o numero de nils
			(setf altura-coluna-mais-alta (- altura-coluna-mais-alta nils)))))
	;apos estas verificacoes ja sabemos onde a peca vai ser desenhada
	(coloca-peca-entre tabuleiro (accao-peca accao) altura-coluna-mais-alta (accao-coluna accao))))

;@@@@ AUXILIAR @brief: 
;			 Recebe um tabuleiro uma peca e um indice para a altura e coluna e coloca a configuracao daquela peca a partir
;            daqueles indices de altura e coluna..  
(defun coloca-peca-entre(tabuleiro peca altura coluna)
	(dotimes (i (second (array-dimensions peca)))
		(dotimes (j (first (array-dimensions peca))) 
			(if (aref peca j i)
				(tabuleiro-preenche! tabuleiro (+ altura j) (+ coluna i)))))
	tabuleiro)

;@@@@ AUXILIAR @brief:
;				Recebe uma accao e uma coluna e devolve os nils que a peca tem, desde a base, nessa mesma coluna
;				Ex; conta-nils-da-coluna: peca-t2 x 2 -> 1 , pois na coluna 2 a peca t0 tem 1 nils a contar de baixo
(defun conta-nils-da-coluna(accao coluna)
	(let ((nils 0))
		(loop while (null (aref (accao-peca accao) nils coluna)) do 
			(setf nils (1+ nils)))
	nils))

;@@@@ AUXILIAR @brief:
;				Recebe uma peca e ve qual e o indice da base que vai ficar a fazer o apoio
(defun pivot(tabuleiro accao)
	(let ((indice nil))
	(loop for j from (1- (second (array-dimensions (accao-peca accao)))) downto 0 do
		;este if so inicializa a variavel, se comecasse a zero podia dar problemas e retornar 0 quando nao era suposto
		(if (and (aref (accao-peca accao) 0 j) (null indice))
			(setf indice j))
		;so mudamos o pivot se houver na base algum indice com valor de coluna mais alto
		(if (and (aref (accao-peca accao) 0 j) 
			(> (tabuleiro-altura-coluna tabuleiro (+ (accao-coluna accao) j))
				(tabuleiro-altura-coluna tabuleiro (+ (accao-coluna accao) indice))))
			(setf indice j)))
	indice))


;------------------------------------------------tipo ESTADO-----------------------------------------------------------------
(defstruct estado
	(pontos 0)     		 		 ;;pontos conseguidos ate ao momento
	(pecas-por-colocar (list))	 ;;lista de pecas que faltam colocar
	(pecas-colocadas (list)) 	 ;;pecas ja colocadas
	(tabuleiro (cria-tabuleiro)) ;;tabuleiro com as posicoes actualmente preenchidas
)

;@brief:
;	Devolve um estado igual ao estado que recebe.. alteracoes entre eles nao seram partilhadas
(defun copia-estado(estado)
	(make-estado 
		:pontos (estado-pontos estado) 
		:pecas-por-colocar (copy-list (estado-pecas-por-colocar estado)) 
		:pecas-colocadas (copy-list (estado-pecas-colocadas estado)) 
		:tabuleiro (copia-tabuleiro (estado-tabuleiro estado))))

;@brief:
;	Compara dois estados e devolve true caso sejam iguais e false caso contrario
(defun estados-iguais-p(estado1 estado2)
	(if (and (equal (estado-pontos estado1) (estado-pontos estado2))  
			 (equal (estado-pecas-por-colocar estado1) (estado-pecas-por-colocar estado2))
	 		 (equal (estado-pecas-colocadas estado1) (estado-pecas-colocadas estado2))
	 		 (tabuleiros-iguais-p (estado-tabuleiro estado1) (estado-tabuleiro estado1)))
		t nil))

;@brief:
;	Verifica se ainda ha pecas-por-colocar ou se ha pecas a ocupar a ultima linha, se alguma destas 
;	condicoes de verifica devolve true.
(defun estado-final-p(estado)
	(if (or (null (estado-pecas-por-colocar estado)) 
			(tabuleiro-topo-preenchido-p (estado-tabuleiro estado)))
		t
		nil))

;;;------------------------------------------------tipo PROBLEMA-----------------------------------------------------------
;@brief:
;	Funcao que recebe um estado e verifica se esse estado e uma solucao para o problema
(defun solucao(estado)
	(if (and (estado-final-p estado) (null (tabuleiro-topo-preenchido-p (estado-tabuleiro estado))))
		t
		nil))


;@@@@ AUXILIAR @brief:
;				Funcao que com base numa configuracao da todas as accoes possiveis para a mesma
(defun gera-accoes(configuracao)
	(let ((accoes (list)))
		(dotimes (j (- 11 (second (array-dimensions configuracao))))
			(setf accoes (append accoes (list (cria-accao j configuracao)))))
		accoes))

;@brief:
;	Recebe um estado e devolve uma lista de accoes correspondendo a todas as accoes 
;	validas que podem ser feitas com a proxima peca a ser colocada
(defun accoes(estado)
	(if (estado-final-p estado)
		(return-from accoes nil))
	
	(case (car (estado-pecas-por-colocar estado))
		((I) (append (gera-accoes peca-i0) 
					 (gera-accoes peca-i1)))

		((L) (append (gera-accoes peca-l0)  
			(gera-accoes peca-l1) 			
			(gera-accoes peca-l2) 			
			(gera-accoes peca-l3))) 		

		((J) (append (gera-accoes peca-j0)  
			 (gera-accoes peca-j1)   		
			 (gera-accoes peca-j2)  	 	
			 (gera-accoes peca-j3)))		

		((O) (gera-accoes peca-o0))			

		((S) (append (gera-accoes peca-s0)  
					 (gera-accoes peca-s1)))

		((Z) (append (gera-accoes peca-z0) 	
					 (gera-accoes peca-z1)))

		((T) (append (gera-accoes peca-t0)  
			 (gera-accoes peca-t1)   		
			 (gera-accoes peca-t2)   		
			 (gera-accoes peca-t3)))))


;@@@@ AUXILIAR @brief;
;				Funcao que recebe um estado e para esse estado remove as linhas completas e actualiza os pontos
(defun remove-linhas-e-actualiza-pontos(estado)
	(let ((numero-linhas-removidas 0))
		(dotimes (linha 18)
			(if (tabuleiro-linha-completa-p (estado-tabuleiro estado) linha)
				(progn (tabuleiro-remove-linha! (estado-tabuleiro estado) linha)
					   (setf numero-linhas-removidas (1+ numero-linhas-removidas))
					   (setf linha (1- linha)))))
	(case numero-linhas-removidas 
		((1) (setf (estado-pontos estado) (+ (estado-pontos estado) 100))) 
		((2) (setf (estado-pontos estado) (+ (estado-pontos estado) 300)))
		((3) (setf (estado-pontos estado) (+ (estado-pontos estado) 500))) 
		((4) (setf (estado-pontos estado) (+ (estado-pontos estado) 800))))))


;@brief: 
;	Recebe um estado e uma accao e devolve o estado resultante de aplicar a accao ao estado.
;	O estado devolvido nao tem a mesma referencia que o estado recebido.
(defun resultado(estado accao)
	(let ((new_estado (copia-estado estado))) 
		(push (pop (estado-pecas-por-colocar new_estado)) (estado-pecas-colocadas new_estado))
		(actualiza-tabuleiro (estado-tabuleiro new_estado) accao)

		(if (null (tabuleiro-topo-preenchido-p (estado-tabuleiro new_estado)))
			(remove-linhas-e-actualiza-pontos new_estado))

	new_estado))

;@brief:
;	Qualidade recebe um estado e retorna um valor de qualidade que corresponde ao valor negativo dos pontos ganhos ate ao momento.
(defun qualidade(estado)
	(* (estado-pontos estado) -1))

;@brief:
;	Dado um estado, devolve o custo de oportunidade de todas as accoes realizadas ate ao momento, assumindo que e 
;	sempre possivel fazer o maximo de pontos por cada peca colocada
(defun custo-oportunidade(estado)
	(let ((maximo-possivel (reduce #'+ ;vamos reduzir a lista de valores devolvida pelo mapcar a um valor final
						   (mapcar #'(lambda(peca) ;funcao lambda que da o valor maximo de cada peca
										(cond 			
											((eq peca 'I) 800)
											((or (eq peca 'L) (eq peca 'J)) 500)
											((or (eq peca 'Z) (eq peca 'S) (eq peca 'T) (eq peca 'O)) 300)
											(t 0))) 
							(estado-pecas-colocadas estado))))); aplicando a funcao lambda a esta lista criamos uma lista 
						   										;que no lugar de cada peca vai ter o valor correspondente	   
	(- maximo-possivel (estado-pontos estado))))


;;;definido ja um contructor para o problema que recebe os argumentos directamente
(defstruct problema

	(estado-inicial (make-estado))			;;estado inicial do problema
	(solucao #'solucao) 		
	(accoes	#'accoes)						;funcao linha 
	(resultado #'resultado) 				;funcao linha 
	(custo-caminho #'custo-oportunidade))	;funcao linha 

;---------------------------------------------SEGUNDA ENTREGA-------------------------------------------------------------------

;-------------------------------------------ESTRUTURAS AUXILIARES---------------------------------------------------------------

;@@@brief:
;		estrutura queue aproveitada do repositorio de codigo do livro
(defstruct q
  (key #'identity);funcao identidade so para facilitar implementacoes
  (last nil)	  ;ultimo na fila
  (elements nil)) ;elementos na fila

;@@@brief:
;		estrutura auxiliar que e usada nas procuras para construir a arvore.
(defstruct node
  (state nil)               ;Estado guardado pelo no
  (parent nil)              ;Pai do no 
  (action nil)              ;accao que leva ao estado
  (depth 0)                 ;profundidade
  (expanded? nil)           ;ja foi expandido?
)
;------------------------------------------OPERACOES BASICAS SOBRE QUEUES------------------------------------------------------

;@@@brief: cria uma queue vazia
(defun make-empty-queue () (make-q))
;@@@brief:
;		remove o elemento que esta no inicio da queue e retorna-o
(defun remove-front (q)
  (if (listp (q-elements q))
      (pop (q-elements q))
    (heap-extract-min (q-elements q) (q-key q))))

;@@@brief:
;		verifica se a queue esta vazia
(defun empty-queue? (q)
  (= (length (q-elements q)) 0))

;------------------------------------------FUNCOES PARA ENQUEUE---------------------------------------------------------------
;@@@brief:
;		coloca uma lista de item na queue tendo em conta a funcao key que devolve um valor para cada no.
(defun enqueue-by-priority (q items key)
  (setf (q-key q) key)
  (when (null (q-elements q))
    (setf (q-elements q) (make-heap)))
  (loop for item in items do
       (heap-insert (q-elements q) item key)))

;@@@brief:
;		coloca uma lista de elementos no inicio da fila.
(defun enqueue-at-front (q items)
  (setf (q-elements q) (nconc items (q-elements q))))

;-------------------------------FUNCOES PARA HEAPS (usado pelo enqueue-by-priority)-------------------------------------------

;@@@brief:
;		operacoes basicas em heaps
(defun heap-val (heap i key) (declare (fixnum i)) (funcall key (aref heap i)))
(defun heap-parent (i) (declare (fixnum i)) (floor (- i 1) 2))
(defun heap-left (i) (declare (fixnum i)) (the fixnum (+ 1 i i)))
(defun heap-right (i) (declare (fixnum i)) (the fixnum (+ 2 i i)))

;@@@brief:
;		garante que a propriedade de min-heap e mantida assumindo que os nos sao heaps e subindo da raiz ate ao topo ajusta 
;		a arvore para manter os menores mais a cima. 
(defun heapify (heap i key)
  (let ((l (heap-left i))
	(r (heap-right i))
	(N (- (length heap) 1))
	smallest)
    (setf smallest (if (and (<= l N) (<= (heap-val heap l key)
					 (heap-val heap i key)))
		       l i))
    (if (and (<= r N) (<= (heap-val heap r key) (heap-val heap smallest key)))
	(setf smallest r))
    (when (/= smallest i)
      (rotatef (aref heap i) (aref heap smallest))
      (heapify heap smallest key))))

;@@@brief:
;		coloca um item na heap tendo em conta a sua chave... MIN-HEAP
(defun heap-insert (heap item key)
  (vector-push-extend nil heap)
  (let ((i (- (length heap) 1))
	(val (funcall key item)))
    (loop while (and (> i 0) (>= (heap-val heap (heap-parent i) key) val))
      do (setf (aref heap i) (aref heap (heap-parent i))
	       i (heap-parent i)))
    (setf (aref heap i) item)))

;@@@brief:
;		retira da heap o no que tiver a chave menor, ou seja o no minimo.
(defun heap-extract-min (heap key)
  (let ((min (aref heap 0)))
    (setf (aref heap 0) (aref heap (- (length heap) 1)))
    (decf (fill-pointer heap))
    (heapify heap 0 key)
    min))

;@@@brief:
;		Cria uma min-Heap.
(defun make-heap (&optional (size 100))
  (make-array size :fill-pointer 0 :adjustable t))

;@@@brief:
;	Expand os nos de acordo com a especificacao do problema ate encontrar a solucao ou ficar sem nos para expandir.
;	A QUEUING-FN decide qual o no a olhar primeiro.
;	E uma procura generalizada que dependendo da forma como e feita a colocacao na fronteira vai tem comportamentos diferentes.
(defun general-search (problem queuing-fn) 
  (let ((nodes (make-initial-queue problem queuing-fn)) 
         node)
    (loop
      (if (empty-queue? nodes) 
          (RETURN nil))   
      (setq node (remove-front nodes))
      (if (goal-test problem node) 
        (RETURN node))
      (funcall queuing-fn nodes (expand node problem)))))

;@@@brief: 
;		inicializacao da queue com o estado inicial.
(defun make-initial-queue (problem queuing-fn)
  (let ((q (make-empty-queue)))
    (funcall queuing-fn q (list (create-start-node problem)))
    q))

;@@@brief:
;	Recebe um no e faz backtracking ate a origem e devolve as accoes que levaram ate ao no em questao.
(defun accoes-realizadas(node)
  (let ((accoes (list (node-action node)))
       (current (node-parent node)))

  (loop while (node-parent current) do
    (setf accoes (append (list (node-action current)) accoes))
    (setf current (node-parent current)))
  accoes))

;@@@brief:
; 		verificacao se e estado objectivo.
(defun goal-test(problema node)
	(funcall (problema-solucao problema) (node-state node)))

;@@brief:
;		cria o primeiro no.
(defun create-start-node(problema)
	(make-node :state (problema-estado-inicial problema)))

;@@@brief:
;		recebe um no e vai expandir esse no aplicando todas as accoes possiveis.
;		O novo no e acrescentado a lista de nos resultantes do no a ser expandido de forma a manter o criterio (LIFO)
(defun expand(node problem)
	(let ((accoes (funcall (problema-accoes problem) (node-state node)))
		  (nos-resultantes (list)) 
		  (no nil) 
		  (accao nil))

	(if (node-expanded? node) 
		(return-from expand nil))
	
	(setf (node-expanded? node) t)
	(loop while accoes do
		(setf accao (pop accoes))
    	(setf no (make-node :state (funcall (problema-resultado problem) (node-state node) accao) 
    		     			:parent node :action accao 
    		     			:depth (1+ (node-depth node))))
		(setf nos-resultantes (append (list no) nos-resultantes)))
	nos-resultantes))

;------------------------------------------------PROCURA CEGA-----------------------------------------------------------------
;@@@brief:
;   utilizando a procura generalizada com a funcao enqueue at front criamos um deepfirst-search.
;	a caracteristica de LIFO pedida no enunciado e garantida pela forma como os nos sao expandidos. 
(defun depth-first-search (problem)
  (general-search problem #'enqueue-at-front))

;@@@brief:
;	procura em profundidade primeiro pedida no enunciado. Chama a procura depth-first-search e com base no 
;	no devolvido faz backtracking ate ao estado inicial.
(defun procura-pp (problema)
	(let ((resultado (depth-first-search problema)))
	(if resultado (accoes-realizadas resultado) nil)))

;--------------------------------------------------PROCURO INFORMADA-----------------------------------------------------------
;@@@brief:
;		da o custo caminho para aquele no definido no problema.
(defun node-g-cost (node problem)
  (funcall (problema-custo-caminho problem) (node-state node)))

;@@@brief:
;		junta o valor heuristico para aquele no com o valor de caminho dado pelo problema.
(defun node-f-cost (node problem heuristic)
  (+ (node-g-cost node problem) (funcall heuristic (node-state node))))

;@@@brief:
;		procura Greedy que avalia o filhos e expande o melhor com base numa funcao de avaliacao EVAL-FN
(defun best-first-search (problem eval-fn)
  (general-search problem #'(lambda (old-q nodes) 
            (enqueue-by-priority old-q (reverse nodes) eval-fn))))

;@@@brief:
;		semelhante a procura Greedy mas usando o valor somado da heuristica com o custo caminho.
(defun tree-a*-search (problem heuristic)
  (best-first-search problem #'(lambda(node) (node-f-cost node problem heuristic))))

;@@@brief:
;		procura-A* como definida no enunciado. Devido ao criterio de desempate referido no enunciado dps de expandidos 
;		os nos sao colocado numa heap comecando no ultimo a ter sido gerado ate ao primeiro. Desta forma garantimos que 
;		em caso de empate o ultimo a ser gerado com valor mais baixo e o que aparece no topo da MIN-HEAP
(defun procura-A* (problema heuristica)
	(let ((resultado (tree-a*-search problema heuristica)))
	(if resultado (accoes-realizadas resultado) nil)))

;-------------------------------------PROCURA LOCAL (utilizada para a PROCURA-BEST)-----------------------------------------
;@@@brief:
;		Procura-best que realiza o pedido no enunciado atraves de uma procura-local com 2 niveis de profundidade,
;		ou seja para um determinado estado, expande o estado gerando os filhos, e depois expande os filhos gerando os netos,
;		com base no fitness atribuido aos netos escolhe um random entre os que tiverem melhor fitness. 
;		Assim sendo vai escolhendo as accoes considerando as peca 2 a 2 e fazendo escolhas locais que vao sendo guardadas.

;NOTA-> Caso o numero de pecas seja impar vamos realizar o processo ate a ultima peca e a ultima peca sera escolhida com 
;		base apenas nos filhos.
(defun procura-best(array pecas)
	(let ((caminho (list))
		 (node (make-node :state (make-estado :pecas-por-colocar pecas :tabuleiro (array->tabuleiro array))))
		 (resto 0))


	(if (oddp (list-length pecas))
		(setf resto 1))

	(loop for i from 0 to (1- (/ (- (list-length pecas) resto) 2)) do
		(setf node (escolhe-melhor-neto (node-state node) #'meta-heuristica))
		(setf caminho (append caminho (accoes-realizadas node))))

	(if (equalp resto 1)
		(progn (setf node (escolhe-melhor-filho (node-state node) #'meta-heuristica))
		  	   (setf caminho (append caminho (list (node-action node))))))
	caminho))

;@@@brief:
;		esta funcao gera os filhos e por sua vez os netos e devolve o par-estado-accao que leva do estado recebido como argumento
;		ao melhor neto tendo em conta a funcao-fitness.
(defun escolhe-melhor-neto(estado funcao-fitness)
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
	;Vamos buscar o primeiro neto so para servir de primeiro candidato a BEST
	(setf node-actual (first sucessores))
	(setf accoes-possiveis (accoes (node-state node-actual)))
	(setf accao-actual (first accoes-possiveis))
	(setf neto-melhor 
		(make-node :state (resultado (node-state node-actual) accao-actual) :parent node-actual :action accao-actual :depth 2))
	
	;vamos aos sucessores gerar os netos e guardar o melhor
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
			(if (< (funcall funcao-fitness neto-novo) (funcall funcao-fitness neto-melhor))
				(setf neto-melhor neto-novo))))
	
	;escolhemos um neto random dentro dos que tiverem o fitness igual ao melhor-neto
	(random-element (remove-if #'(lambda (node) (compara node neto-melhor funcao-fitness))
	  							netos))))

;@@@brief:
;		Funcao de comparacao entre dois nos com base na funcao de fitness.
;		O segundo argumento refere-se as diferencas heuristica das linhas completas a 2 niveis ou a 1.
(defun compara(node best funcao-fitness &optional (h-linhas #'linhas-completas-2niveis))
	(if (> (funcall funcao-fitness node h-linhas) (funcall funcao-fitness best h-linhas))
		t
		nil))

;@@@brief:
;		funcao chamada para fazer uma escolha greedy para o ultimo elemento da lista de pecas caso esta seja de comprimento impar
;		recebe um estado e a funcao fitness que avalia a qualidade dos filhos. Semelhante a escolhe-melhor-neto com menos 1 nivel
(defun escolhe-melhor-filho(estado funcao-fitness)
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
		(if (< (funcall funcao-fitness novo-no #'linhas-completas) 
			   (funcall funcao-fitness filho-melhor #'linhas-completas))
			(setf filho-melhor novo-no)))
	
	(random-element (remove-if #'(lambda (node) 
								 (compara node filho-melhor funcao-fitness #'linhas-completas))
	  							 sucessores))))

;----------------------------------------------FUNCOES HEURISTICAS------------------------------------------------------------
;@@@brief:
;		funcao heuristica que combina 4 heuristicas que vao avaliar o tabuleiro com base em certas caracteristicas (features).
;		essas caracteristicas sao combinadas atraves da multiplicacao por constantes empiricas calculdas atraves de um algoritmo
;		genetico.
(defun meta-heuristica(node &optional (h-linhas #'linhas-completas-2niveis))
	(+ (* 0.46069467 (heuristica-somatorio-alturas node)) (* 0.56695324 (funcall h-linhas node))
	   (* 0.4459617 (heuristica-numero-de-buracos node)) (* 0.06362687 (heuristica-monotonia node))))

;@@@brief:
;		funcao heuristica que ve a altura total das colunas do tabuleiro.
;		vamos querer minimizar
(defun heuristica-somatorio-alturas(node)
	(let ((tabuleiro (estado-tabuleiro (node-state node)))
		  (height 0))
	
	(dotimes (i 10)
		(setf height (+ height (tabuleiro-altura-coluna tabuleiro i))))
	height))

;@@@brief:
;		funcao heuristica que ve a monotonia do tabuleiro.. ou seja se nao ha grandes diferencas entre as alturas de 
;		colunas adjacentes. Faz o somatorio dessas diferencas.
;		Vamos querer minimizar.
(defun heuristica-monotonia(node)
	(let ((tabuleiro (estado-tabuleiro (node-state node)))
		   (bumpiness 0))
	
	(loop for i from 0 to 8 do
		(setf bumpiness (+ bumpiness (abs (- (tabuleiro-altura-coluna tabuleiro i) (tabuleiro-altura-coluna tabuleiro (1+ i)))))))
	bumpiness))

;@@@brief:
;		funcao heuristica que ve o numero de linhas que foram completas apos uma accao.
;		vamos querer maximizar por isso multiplicamos por -1.
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

;@@@brief:
;		se considerar-mos 2 niveis vamos querer as linhas completas pelo pai e pelo filho, ou seja linhas completas naquelas duas
;		jogadas.
(defun linhas-completas-2niveis(node)
	(+ (linhas-completas node) (linhas-completas (node-parent node))))

;@@@brief:
;		funcao heuristica que ve o numero de buracos, ou seja, posicoes no tabuleiro desocupadas e com posicoes ocupadas por cima.
(defun heuristica-numero-de-buracos(node)
	(let ((tabuleiro (estado-tabuleiro (node-state node)))
		  (altura-coluna 0) (holes 0))
	
	(dotimes (j 10)
		(setf altura-coluna (tabuleiro-altura-coluna tabuleiro j))
		(dotimes (i altura-coluna)
			(if (and (null (tabuleiro-preenchido-p tabuleiro i j)) (tabuleiro-preenchido-p tabuleiro (1+ i) j))
				(incf holes))))
	holes))

(load "utils.fas")
