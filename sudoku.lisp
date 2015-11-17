(defparameter *solution-liste* (list (list 2 4 5 1 7 9 3 8 6) (list 1 9 6 8 2 3 5 4 7) (list 8 3 7 4 6 5 2 1 9) (list 6 8 4 5 1 7 9 2 3) (list 3 2 9 6 8 4 7 5 1) (list 5 7 1 3 9 2 4 6 8) (list 4 6 2 9 3 8 1 7 5) (list 7 1 3 2 5 6 8 9 4) (list 9 5 8 7 4 1 6 3 2)))

(defparameter *solution-liste-presque* (list (list 0 4 5 1 7 9 3 8 6) (list 0 9 6 8 2 3 5 4 7) (list 8 3 7 4 6 5 2 1 9) (list 6 8 4 5 1 7 9 2 3) (list 3 2 9 6 8 4 7 5 1) (list 5 7 1 3 9 2 4 6 8) (list 4 6 2 9 3 8 1 7 5) (list 7 1 3 2 5 6 8 9 4) (list 9 5 8 7 4 1 6 3 2)))


(defparameter *solution-avec-zero* (list (list 0 4 0 0 0 0 3 0 0) (list 0 0 6 8 2 0 0 4 7) (list 0 3 0 4 0 5 0 0 9) (list 0 8 4 0 0 7 9 0 0) (list 3 2 0 0 8 0 0 5 1) (list 0 0 1 3 0 0 4 6 0) (list 4 0 0 9 0 8 0 7 0) (list 7 1 0 0 5 6 8 0 0) (list 0 0 8 0 0 0 0 3 0) ))

; 0 est là parceque delete ne va pas supprimer le premier �l�ment dans la liste
(defparameter *valeurs-possibles* (list 0 1 2 3 4 5 6 7 8 9))

; Mod�le pour v�rifier que l'utilisateur n'�crase pas les chiffres donn�s au d�but du jeu.  
(defparameter *grille-modele* (make-array '(9 9) :initial-contents *solution-avec-zero*))

(defparameter *grille-modifiable* (make-array '(9 9) :initial-contents *solution-avec-zero*))

(defparameter *grille-vite* (make-array '(9 9) :initial-contents *solution-liste-presque*))


(defparameter *grille-solution* (make-array '(9 9) :initial-contents *solution-liste*))

;;; FONCTIONS ;;;

(defun afficher-grille (grille)
  (format t "~%  A   B   C   D   E   F   G   H   I  ")
  (format t "~% ----------------------------------- ~%")
  (loop for i below (car (array-dimensions grille)) do
        (loop for j below (cadr (array-dimensions grille)) do
          (let ((cellule (aref grille i j)))
            (format t "| ~a " cellule)))
        (format t "| ~% ----------------------------------- ~%")))

(defun demander-chiffre ()
   (defparameter *drapeau-chif* 0)    
   (loop do
      (format t "Choisissez un chiffre entre 1 et 9: ")
      (defparameter *chiffre* (parse-integer (read-line)))
      (if (and (> *chiffre* 0) (< *chiffre* 10))
         (progn (setf *drapeau-chif* 0) 
         *chiffre*)
        (progn (format t "Ce chiffre n'est pas valable. ") (setf *drapeau-chif* 1)))
     until (zerop *drapeau-chif*)))


(defun demander-colonne ()
   (defparameter *drapeau-col* 0)
   (loop do
      (format t "Choisissez une colonne (A-I): ")
      (defparameter *colonne* (char-code (char (read-line) 0)))
	 (cond ((and (< *colonne* 74) (> *colonne* 64)) (progn (setf *drapeau-col* 0) (setf *colonne* (- *colonne* 65))))  ; pour les majuscules
               ((and (< *colonne* 106) (> *colonne* 96)) (progn (setf *drapeau-col* 0) (setf *colonne* (- *colonne* 97))))  ;pour les minuscules
	       (t (progn (format t "Cette colonne n'est pas valable. ") (setf *drapeau-col* 1))))
    until (zerop *drapeau-col*)))
         

(defun demander-rang ()
   (defparameter *drapeau-rang* 0)  
   (loop do
      (format t "Choisissez un rang (1-9): ")
      (defparameter *rang* (parse-integer (read-line)))
      (if (and (> *rang* 0) (< *rang* 10))
         (progn (setf *drapeau-rang* 0) 
         (setf *rang* (- *rang* 1)))
        (progn (format t "Ce rang n'est pas valable. ") (setf *drapeau-rang* 1)))
     until (zerop *drapeau-rang*))
    (clear-input))


(defun verifier-modele (x y modele) 
  (if (zerop (aref modele x y))
     (setf *drapeau-placement* 0)    ; Modifiable 
     (setf *drapeau-placement* 1)))  ; Non-modifiable

(defun remplir-grille (y x grille chiffre)
   (defparameter *drapeau-placement* 0)
      (verifier-modele x y *grille-modele*)
      (if (zerop *drapeau-placement*)
         (setf (aref grille x y) chiffre) 
        ; (format t "Vous ne pouvez modifier cette cellule. ~%")  ; D�j� comment� car version al�atoire prend beaucoup de temps
      )) 
      
      
; fontion appel� � chaque tour pour savoir si la grille est fini ou pas      
(defun verifier-grille-rempli (grille)
   (defparameter *drapeau-rempli* 0)
    (loop for y from 0 to 8 do
     (if (/= *drapeau-rempli* 1)
      (loop for x from 0 to 8 do
         (if (/= *drapeau-rempli* 1)
            (if (zerop (aref grille x y) )
               (setf *drapeau-rempli* 1)))))
	       )
   (if (zerop *drapeau-rempli* )
	(progn(format t "La grille est entièrement remplie !~%") (verifier-solution grille *grille-solution*))
       (format t "La grille n'est pas entièrement remplie !~%"))
  )
  
      
;fonction pour savoir si ,une fois la grille rempli , elle est gagnante ou pas
(defun verifier-solution(grille solution)
   (defparameter *drapeau* 0)
   (loop for y from 0 to 8 do
     (if (/= *drapeau* 1)
      (loop for x from 0 to 8 do
         (if (/= *drapeau* 1)
            (if (/= (aref grille x y) (aref solution x y))
               (setf *drapeau* 1))))))
   (if (/= *drapeau* 1)
      (format t "Félicitations ! Vous avez gagné !~%")
      (format t "Vous n'avez pas la bonne solution! Trouvez votre erreur pour gagné !~%")
   ))


;;; M�thodes al�atoires ;;;

(defun generer-variables-aleatoires ()
  (defparameter *colonne* (random 9))
  (defparameter *rang* (random 9))
  (defparameter *chiffre* (1+ (random 9)))) ; Chiffre entre 1 et 9
  



;;; Methodes intelligence-artificielle ;;;

 
;;;;;;;;;;;;;;;;;;;;; CETTE METHODE N'EST PAS BIEN ECRITES ;;;;;;;;;;;;;;;;;;;;;;;
; Selectionner un carr� � parcourir
(defun IA-choisir-chiffre(grille x y valeurs)
  (if (and (and (>= x 0) (< x 3)) (and (>= y 0) (< y 3))) (IA-parcourir-carre grille 0 0 valeurs))
  (if (and (and (>= x 0) (< x 3)) (and (> y 2) (< y 6))) (IA-parcourir-carre grille 0 3 valeurs))
  (if (and (and (>= x 0) (< x 3)) (and (> y 5) (< y 9))) (IA-parcourir-carre grille 0 6 valeurs))
  (if (and (and (> x 2) (< x 6)) (and (>= y 0) (< y 3))) (IA-parcourir-carre grille 3 0 valeurs))
  (if (and (and (> x 2) (< x 6)) (and (> y 2) (< y 6))) (IA-parcourir-carre grille 3 3 valeurs))
  (if (and (and (> x 2) (< x 6)) (and (> y 5) (< y 9))) (IA-parcourir-carre grille 3 6 valeurs))
  (if (and (and (> x 5) (< x 9)) (and (>= y 0) (< y 3))) (IA-parcourir-carre grille 6 0 valeurs))
  (if (and (and (> x 5) (< x 9)) (and (> y 2) (< y 6))) (IA-parcourir-carre grille 6 3 valeurs))
  (if (and (and (> x 5) (< x 9)) (and (> y 5) (< y 9))) (IA-parcourir-carre grille 6 6 valeurs))
  valeurs)


; Enlever les valeurs trouv�es dans le carr�
(defun IA-parcourir-carre (grille x y valeurs) 
  (delete (aref grille x y) valeurs)
  (delete (aref grille (1+ x) y) valeurs)
  (delete (aref grille (+ x 2) y) valeurs)
  (delete (aref grille x (1+ y)) valeurs)
  (delete (aref grille (1+ x) (1+ y)) valeurs)
  (delete (aref grille (+ x 2) (1+ y)) valeurs)
  (delete (aref grille x (+ y 2)) valeurs)
  (delete (aref grille (1+ x) (+  y 2)) valeurs)
  (delete (aref grille (+ x 2) (+ y 2)) valeurs)
  valeurs)

; Enlever les valeurs trouv�es dans la colonne
(defun IA-parcourir-colonne (grille x valeurs)
  (loop for y from 0 to 8 do
    (delete (aref grille x y) valeurs)))

; Enlever les valeurs trouv�es dans le rang
(defun IA-parcourir-rang (grille y valeurs)
  (loop for x from 0 to 8 do 
    (delete (aref grille x y) valeurs)))

; Retourner une liste de valeurs possible pour un cr�neau
; Si la taille de la liste = 2 (c'est à dire 0 et un autre chiffre sont les seuls chiffres pr�sent dans la liste
; on va mettre le chiffre dans le cr�neau tout de suite. 
(defun IA-determiner-valeurs-possibles(grille x y)
  (setq *temp-vp* (copy-list *valeurs-possibles*))
  (IA-choisir-chiffre grille x y *temp-vp*)
  (IA-parcourir-rang grille y *temp-vp*)
  (IA-parcourir-colonne grille x *temp-vp*)
  (if (= 2 (list-length *temp-vp*))
    (setf (aref grille x y) (second *temp-vp*))) 
  *temp-vp*)

; Retourner une liste de forme ((#ValeursPossibles x y)(#ValeursPossibles2 x2 y2)...) 
; pour chaque cr�neau pour d�terminer quel cr�neau est � remplir d'abord.
(defun IA-determiner-solutions-possibles (grille)
  (defparameter *liste* '())
  (loop for x from 0 to 8 do 
    (loop for y from 0 to 8 do
      (if (zerop (aref grille x y))
         (setq *liste* (append *liste* (list (list (1- (length (IA-determiner-valeurs-possibles grille x y)))x y)))))))
   (reverse (sort *liste* #'> :key #'car))
   *liste*)
   
;;; LE JEU ;;;

(defun sudoku(grille)
   (format t "Welcome ! Bienvenue ! Bienvenido ! Wilkommen ! ~%")
   (defparameter *drapeau* 1)
   (loop do 
      (format t "Choisissez votre mode : ~%")
      (format t "'interactif' pour jouer tout seul, 'aleatoire' pour voir la strat�gie al�atoire, ou 'ia' pour voir une IA jouer : ")
      (defparameter *jeu* (read-line))
      (cond 
         ((string-equal *jeu* "interactif") (sudoku-interactive grille))
         ((string-equal *jeu* "aleatoire") (sudoku-aleatoire grille))
         ((string-equal *jeu* "ia") (sudoku-ia grille) )
         (t (format t "Cet option n'existe pas.  Veuillez r�essayer. ~%")))
   until (zerop *drapeau*)))

(defun sudoku-interactive(grille-interact)
   (loop do 
      (afficher-grille grille-interact)
      (demander-rang)
      (demander-colonne)
      (demander-chiffre)
      (remplir-grille *colonne* *rang* grille-interact *chiffre*)
      (verifier-grille-rempli grille-interact)
    until (zerop *drapeau*)))

(defun sudoku-aleatoire(grille-aleat)
  (afficher-grille grille-aleat)
  (format t "Veuillez patienter -- la version al�atoire peut prendre du temps � compl�ter.")
  (loop do
     (generer-variables-aleatoires)
     (remplir-grille *colonne* *rang* grille-aleat *chiffre*)
     (verifier-grille-rempli grille-aleat)
  until (zerop *drapeau*))
  (afficher-grille grille-aleat))


(defun sudoku-ia(grille-ia)
  (loop do
    (IA-determiner-solutions-possibles grille-ia)
    (verifier-grille-rempli grille-ia)
  until (zerop *drapeau*))
  (afficher-grille grille-ia))

;; IA
;; 1. Determiner quels cr�neaux ont le moins de chiffre possible (en v�rifiant les chiffres dans le carr� du cr�neau, le rang, et la colonne)
;; 2. Remplir d'abord ce cr�neaux et continue.  



;;; COMMENTAIRES GENERALES

;;faire un nouveau mode o� on sugg�re � l'utilisateur une liste de chiffre valide pour la case 