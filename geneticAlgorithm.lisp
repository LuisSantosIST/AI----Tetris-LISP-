;(load "geneticBest2.lisp")

(defstruct p
	(vector (make-array (list 4)))
	(fitness 0))

(defun inicializacao-da-populacao(population)
	(dotimes (k 200) ;MUDAS AQUI NUMERO DA POPULACAO
		(setf (aref population k) (make-p)))

	(dotimes (i (first (array-dimensions population)))
		(dotimes (j 4)
			(setf (aref (p-vector (aref population i)) j) (random 1.0)))))

(defun algoritmo-genetico()
	(let ((population (make-array (list 200)))) ;MUDAR AQUI NUMERO DA POPULACAO
		
		(inicializacao-da-populacao population)
		(dotimes (i 19)  		;MUDAR AQUI NUMERO DE GERACOES
			(print "GERACAO")
			(print (1+ i))
			(setf population (simulacao-genetica population nil)))

		(setf population (simulacao-genetica population t))
		(print "FINAL")
		(print 20)
		(print (media-da-populacao population))
		population))

(defun simulacao-genetica (population geracao-final?)
	(let ((fitness-p-corrente 0)
		  accoes
		  estado-corrente
		  pecas)

	(dotimes (i 200)			;MUDAR AQUI NUMERO DA POPULACAO
		(dotimes (j 10)   ;;NUMERO DE JOGOS
			(setf pecas (random-pecas 16));NUMERO DE PECAS
			(setf estado-corrente (make-estado))
			(setf accoes (procura-best (estado-tabuleiro estado-corrente) pecas (p-vector (aref population i))))
			(loop while accoes do
				(setf estado-corrente (resultado estado-corrente (pop accoes))))
			(setf fitness-p-corrente (+ fitness-p-corrente (estado-pontos estado-corrente))))
		(setf (p-fitness (aref population i)) fitness-p-corrente)
		(setf fitness-p-corrente 0))

	(if geracao-final?
		(return-from simulacao-genetica population))

	(print population)
	(print (media-da-populacao population))
	(setf population (gera-nova-geracao population))
	population))

(defun media-da-populacao(population)
	(let ((media 0)
		  (best (aref population 0)))
	(dotimes (i 200)				;MUDAR AQUI NUMERO DA POPULACAO
		(setf media (+ media (p-fitness (aref population i))))
		(if (> (p-fitness (aref population i)) (p-fitness best))
			(setf best (aref population i))))
	(print "BEST")
	(print best)
	(/ media 200)))

(defun compara(p1 p2)
	(if (> (p-fitness p1) (p-fitness p2))
		t
		nil))

(defun compara-minimo(p1 p2)
	(if (< (p-fitness p1) (p-fitness p2))
		t
		nil))

(defun elementos-random(population)
	(let ((elementos-random (make-array '(20))) ;MUDAR AQUI NUMERO DE ELEMENTOS RANDOM
		  elemento-da-populacao)

		(dotimes (i 20)					;MUDAR NUMERO DE ELEMENTOS RANDOM
			(setf elemento-da-populacao (random 200)) ;NUMERO DA POPULACAO
			(setf (aref elementos-random i) (copy-p (aref population elemento-da-populacao))))
	elementos-random))

(defun mutacao (vector)
	(let ((ha-mutacao? (random 100))
		  (negativa? (random 2)))

	(if (< ha-mutacao? 6)
		(if (equalp negativa? 0)
			(dotimes (i 4)
				(if (>= (- (aref vector i) 0.2) 0)
				   	(setf (aref vector i) (- (aref vector i) 0.2))
				   	(setf (aref vector i) 0.000012)))
			(dotimes (i 4)
				(if (<= (+ (aref vector i) 0.2) 1)
					(setf (aref vector i) (+ (aref vector i) 0.2))
					(setf (aref vector i) 0.99999)))))
	vector))

(defun escolhe-e-cruza-2-melhores(amostra)
	(let ((amostra-ordenada (sort amostra #'compara))
		  melhor
		  segundo-melhor
		  (vector-cruzado (make-array '(4))))

	(setf melhor (aref amostra-ordenada 0))
	(setf segundo-melhor (aref amostra-ordenada 1))

	(if (null (and (equal (p-fitness melhor) 0) (equal (p-fitness segundo-melhor) 0)))
	(progn 
	(setf (aref vector-cruzado 0) (+ (* (aref (p-vector melhor) 0) ;componente a multiplicada pela constante
										(/ (p-fitness melhor) (+ (p-fitness melhor) (p-fitness segundo-melhor))))
									 (* (aref (p-vector segundo-melhor) 0)
									 	(/ (p-fitness segundo-melhor) (+ (p-fitness melhor) (p-fitness segundo-melhor))))))

	(setf (aref vector-cruzado 1) (+ (* (aref (p-vector melhor) 1) ;componente b multiplicada pela constante
										(/ (p-fitness melhor) (+ (p-fitness melhor) (p-fitness segundo-melhor))))
									 (* (aref (p-vector segundo-melhor) 1)
									 	(/ (p-fitness segundo-melhor) (+ (p-fitness melhor) (p-fitness segundo-melhor))))))
	(setf (aref vector-cruzado 2) (+ (* (aref (p-vector melhor) 2) ;componente c multiplicada pela constante
										(/ (p-fitness melhor) (+ (p-fitness melhor) (p-fitness segundo-melhor))))
									 (* (aref (p-vector segundo-melhor) 2)
									 	(/ (p-fitness segundo-melhor) (+ (p-fitness melhor) (p-fitness segundo-melhor))))))
	(setf (aref vector-cruzado 3) (+ (* (aref (p-vector melhor) 3) ;componente d multiplicada pela constante
										(/ (p-fitness melhor) (+ (p-fitness melhor) (p-fitness segundo-melhor))))
									 (* (aref (p-vector segundo-melhor) 3)
									 	(/ (p-fitness segundo-melhor) (+ (p-fitness melhor) (p-fitness segundo-melhor)))))))
	(progn
	(setf (aref vector-cruzado 0) (/ (+ (aref (p-vector melhor) 0) (aref (p-vector segundo-melhor) 0)) 2))
	(setf (aref vector-cruzado 1) (/ (+ (aref (p-vector melhor) 1) (aref (p-vector segundo-melhor) 1)) 2))
	(setf (aref vector-cruzado 2) (/ (+ (aref (p-vector melhor) 2) (aref (p-vector segundo-melhor) 2)) 2))
	(setf (aref vector-cruzado 3) (/ (+ (aref (p-vector melhor) 3) (aref (p-vector segundo-melhor) 3)) 2))))
	(setf vector-cruzado (mutacao vector-cruzado))
	vector-cruzado))

(defun gera-nova-geracao(population)
	(let ((novos-individuos (make-array '(60)))) ;NUMERO DE NOVOS ELEMENTOS
		(dotimes (i 60)							;NUMERO DE NOVOS ELEMENTOS
			(setf (aref novos-individuos i) (make-p :vector (escolhe-e-cruza-2-melhores (elementos-random population)))))

		(sort population #'compara-minimo)

		(dotimes (j 60) ;NUMERO DE NOVOS ELEMENTOS
			(setf (aref population j) (aref novos-individuos j)))

		(loop for i from 60 to 199 do ;NUMERO DE NOVOS ELEMENTOS E POPULACAO -1
			(setf (p-fitness (aref population i)) 0))

		population))

(defun meta-heuristica-p(p estado)
	(let ((vector (make-array '(4)))
		(result 0))
		(setf (aref vector 0) (heuristica-somatorio-alturas estado))
		(setf (aref vector 1) (heuristica-linhas-completas estado))
		(setf (aref vector 2) (heuristica-numero-de-buracos estado))
		(setf (aref vector 3) (heuristica-monotonia estado))

		(dotimes (i 4)
			(setf result (+ result (* (aref p i) (aref vector i)))))
		result))

